;;; etaf-layout-string.el --- Layout to string conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, rendering
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 布局树到字符串转换模块
;;
;; 本模块将布局树转换为可插入 Emacs buffer 的字符串。
;; 所有函数使用 `etaf-layout-string-' 前缀。
;;
;; 公共接口：
;; - `etaf-layout-string-render' - 将布局树转换为字符串（主入口）
;; - `etaf-layout-string-render-node' - 将单个布局节点转换为字符串
;;
;; 内部函数使用 `etaf-layout-string--' 前缀。
;;
;; 渲染方式：
;; 使用后序遍历（post-order traversal）从叶子节点开始构建字符串，
;; 通过文本拼接生成最终布局，不使用位置坐标。

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-utils)
(require 'etaf-css-face)
(require 'etaf-layout-box)

;; Forward declarations
(declare-function etaf-render-get-default-display "etaf-render")
(declare-function etaf-flex-line-breaks "etaf-flex")
(declare-function etaf-layout-get-box-model "etaf-layout")

;;; ============================================================
;;; 公共接口
;;; ============================================================

(defun etaf-layout-string-render (layout-tree)
  "将布局树转换为可插入 Emacs buffer 的字符串。
LAYOUT-TREE 是布局树根节点。

返回拼接好的布局字符串，可以直接插入到 buffer 中显示。

这种方式通过文本拼接来生成最终的布局，而不是使用精确的 x,y 坐标，
更适合在 Emacs buffer 中进行渲染。"
  (etaf-layout-string-render-node layout-tree))

(defun etaf-layout-string-render-node (layout-node)
  "将布局节点转换为可插入 buffer 的字符串。
LAYOUT-NODE 是布局节点。

返回拼接好的字符串，包含 margin、border、padding 和内容。

使用后序遍历从叶子节点开始构建字符串。
在 Emacs 中，高度使用行数（lines）而不是像素值。
CSS 文本样式会转换为 Emacs face 属性应用到文本上。"
  (let* ((box-model (or (etaf-layout-get-box-model layout-node)
                        (etaf-layout-box-create)))
         (computed-style (dom-attr layout-node 'render-style))
         (content-width (or (etaf-layout-box-content-width box-model) 0))
         (content-height-px (or (etaf-layout-box-content-height box-model) 0))
         
         ;; 获取盒模型各部分
         (padding (or (plist-get box-model :padding)
                      '(:top 0 :right 0 :bottom 0 :left 0)))
         (border (or (plist-get box-model :border)
                     '(:top-width 0 :right-width 0 :bottom-width 0 :left-width 0)))
         (margin (or (plist-get box-model :margin)
                     '(:top 0 :right 0 :bottom 0 :left 0)))
         
         ;; 提取各边的值
         (padding-top (floor (or (plist-get padding :top) 0)))
         (padding-right (or (plist-get padding :right) 0))
         (padding-bottom (floor (or (plist-get padding :bottom) 0)))
         (padding-left (or (plist-get padding :left) 0))
         
         (border-top (or (plist-get border :top-width) 0))
         (border-right (or (plist-get border :right-width) 0))
         (border-bottom (or (plist-get border :bottom-width) 0))
         (border-left (or (plist-get border :left-width) 0))
         (border-top-color (or (plist-get border :top-color) 
                               (face-attribute 'default :foreground)))
         (border-right-color (or (plist-get border :right-color) 
                                 (face-attribute 'default :foreground)))
         (border-bottom-color (or (plist-get border :bottom-color) 
                                  (face-attribute 'default :foreground)))
         (border-left-color (or (plist-get border :left-color) 
                                (face-attribute 'default :foreground)))
         
         (margin-top (floor (or (plist-get margin :top) 0)))
         (margin-right (or (plist-get margin :right) 0))
         (margin-bottom (floor (or (plist-get margin :bottom) 0)))
         (margin-left (or (plist-get margin :left) 0))
         
         ;; 处理子元素
         (children (dom-children layout-node))
         (is-flex-container (dom-attr layout-node 'layout-flex-direction))
         (child-infos
          (mapcar (lambda (child)
                    (cond
                     ((listp child)
                      (cons (etaf-layout-string-render-node child)
                            (or (dom-attr child 'render-display)
                                (etaf-render-get-default-display (dom-tag child)))))
                     ((stringp child)
                      (cons child "inline"))
                     (t (cons "" "inline"))))
                  children))
         
         ;; 合并子节点
         (children-text
          (if is-flex-container
              (etaf-layout-string--merge-flex-children
               (mapcar #'car child-infos)
               (dom-attr layout-node 'layout-flex-direction)
               (or (dom-attr layout-node 'layout-row-gap) 0)
               (or (dom-attr layout-node 'layout-column-gap) 0)
               (dom-attr layout-node 'layout-justify-content)
               content-width content-height-px
               (or (dom-attr layout-node 'layout-flex-wrap) "nowrap")
               (or (dom-attr layout-node 'layout-align-items) "stretch")
               (or (dom-attr layout-node 'layout-align-content) "stretch"))
            (etaf-layout-string--merge-by-display child-infos content-width)))
         
         (inner-content children-text)
         
         ;; 计算高度
         (natural-content-height (if (> (length inner-content) 0)
                                     (etaf-string-linum inner-content)
                                   0))
         (content-height (if (> content-height-px 0)
                             content-height-px
                           (max natural-content-height 0)))
         
         ;; 计算有效宽度
         (effective-width
          (if (and (> (length inner-content) 0) (<= content-width 0))
              (let ((lines (split-string inner-content "\n")))
                (if lines
                    (apply #'max (mapcar #'string-pixel-width lines))
                  (string-pixel-width inner-content)))
            content-width)))

    (if (and (<= effective-width 0) (<= content-height 0))
        ""
      (etaf-layout-string--build-box
       inner-content effective-width content-height content-height-px
       padding-top padding-right padding-bottom padding-left
       border-top border-right border-bottom border-left
       border-top-color border-right-color border-bottom-color border-left-color
       margin-top margin-right margin-bottom margin-left
       computed-style))))

;;; ============================================================
;;; 内部函数：盒模型构建
;;; ============================================================

(defun etaf-layout-string--build-box (inner-content effective-width content-height content-height-px
                                                     padding-top padding-right padding-bottom padding-left
                                                     border-top border-right border-bottom border-left
                                                     border-top-color border-right-color border-bottom-color border-left-color
                                                     margin-top margin-right margin-bottom margin-left
                                                     computed-style)
  "构建盒模型字符串。"
  (let* (;; 1. 调整内容宽度
         (sized-content
          (if (> (length inner-content) 0)
              (condition-case nil
                  (etaf-lines-justify inner-content effective-width)
                (error inner-content))
            (etaf-pixel-blank effective-width content-height)))
         
         ;; 1.5 应用文本样式
         (text-style (when computed-style
                       (cl-remove-if (lambda (pair)
                                       (eq (car pair) 'background-color))
                                     computed-style)))
         (styled-content (if (and text-style (> (length sized-content) 0))
                             (etaf-css-apply-face-to-string sized-content text-style)
                           sized-content))
         
         ;; 计算渲染高度
         (styled-content-height (if (> (length styled-content) 0)
                                    (etaf-string-linum styled-content)
                                  0))
         (actual-content-height (if (> content-height-px 0)
                                    content-height-px
                                  (max styled-content-height content-height)))
         
         ;; 约束高度
         (height-constrained-content
          (if (and (> content-height-px 0)
                   (> (length styled-content) 0)
                   (/= styled-content-height content-height-px))
              (etaf-lines-align styled-content content-height-px 'top)
            styled-content))
         
         (inner-height (+ actual-content-height padding-top padding-bottom))
         
         ;; 2. 添加垂直 padding
         (with-padding (if (and (> effective-width 0)
                                (or (> padding-top 0) (> padding-bottom 0)))
                           (etaf-lines-stack
                            (list (when (> padding-top 0)
                                    (etaf-pixel-blank effective-width padding-top))
                                  height-constrained-content
                                  (when (> padding-bottom 0)
                                    (etaf-pixel-blank effective-width padding-bottom))))
                         height-constrained-content))
         
         ;; 3. 添加水平 padding
         (with-h-padding (if (and (> inner-height 0)
                                  (or (> padding-left 0) (> padding-right 0)))
                             (etaf-lines-concat
                              (list (when (> padding-left 0)
                                      (etaf-pixel-blank padding-left inner-height))
                                    with-padding
                                    (when (> padding-right 0)
                                      (etaf-pixel-blank padding-right inner-height))))
                           with-padding))
         
         ;; 3.5 应用背景色
         (bgcolor (when computed-style
                    (cdr (assq 'background-color computed-style))))
         (with-bgcolor (if (and bgcolor (> (length with-h-padding) 0))
                           (let ((emacs-color (etaf-css-color-to-emacs bgcolor)))
                             (if emacs-color
                                 (let ((result (copy-sequence with-h-padding)))
                                   (add-face-text-property 0 (length result)
                                                           `(:background ,emacs-color)
                                                           t result)
                                   result)
                               with-h-padding))
                         with-h-padding))
         
         ;; 4. 添加水平 border
         (with-border (if (and (> inner-height 0)
                               (or (> border-left 0) (> border-right 0)))
                          (etaf-lines-concat
                           (list (when (> border-left 0)
                                   (etaf-pixel-border border-left inner-height
                                                      border-left-color))
                                 with-bgcolor
                                 (when (> border-right 0)
                                   (etaf-pixel-border border-right inner-height
                                                      border-right-color))))
                        with-bgcolor))
         
         (total-pixel (+ effective-width
                         padding-left padding-right
                         border-left border-right))
         
         ;; 4.5 添加垂直 border
         (with-v-border (if (or (> border-top 0) (> border-bottom 0))
                            (let ((lines (split-string with-border "\n")))
                              (when (and lines (> border-top 0))
                                (setf (car lines)
                                      (etaf-propertize-overline (car lines) border-top-color)))
                              (when (and lines (> border-bottom 0))
                                (setf (car (last lines))
                                      (etaf-propertize-underline (car (last lines))
                                                                 border-bottom-color)))
                              (string-join lines "\n"))
                          with-border))
         
         ;; 5. 添加水平 margin
         (with-h-margin (if (and (> inner-height 0)
                                 (or (> margin-left 0) (> margin-right 0)))
                            (etaf-lines-concat
                             (list (when (> margin-left 0)
                                     (etaf-pixel-blank margin-left inner-height))
                                   with-v-border
                                   (when (> margin-right 0)
                                     (etaf-pixel-blank margin-right inner-height))))
                          with-v-border))
         
         (total-width (+ total-pixel margin-left margin-right))
         
         ;; 6. 添加垂直 margin
         (final-string (if (and (> total-width 0)
                                (or (> margin-top 0) (> margin-bottom 0)))
                           (etaf-lines-stack
                            (list (when (> margin-top 0)
                                    (etaf-pixel-blank total-width margin-top))
                                  with-h-margin
                                  (when (> margin-bottom 0)
                                    (etaf-pixel-blank total-width margin-bottom))))
                         with-h-margin)))
    
    final-string))

;;; ============================================================
;;; 内部函数：子元素合并
;;; ============================================================

(defun etaf-layout-string--merge-by-display (child-infos &optional container-width)
  "根据 display 类型合并子元素字符串。
CHILD-INFOS 是 ((string . display-type) ...) 列表。
CONTAINER-WIDTH 是容器宽度（用于 inline 元素换行）。

Inline 元素水平拼接，Block 元素垂直堆叠。"
  (if (null child-infos)
      ""
    (let ((result-parts '())
          (inline-group '()))
      (dolist (info child-infos)
        (let ((str (car info))
              (display (cdr info)))
          (when (> (length str) 0)
            (if (string= display "inline")
                (push str inline-group)
              (when inline-group
                (push (etaf-layout-string--merge-inline
                       (nreverse inline-group) container-width)
                      result-parts)
                (setq inline-group nil))
              (push str result-parts)))))
      (when inline-group
        (push (etaf-layout-string--merge-inline
               (nreverse inline-group) container-width)
              result-parts))
      (string-join (reverse result-parts) "\n"))))

(defun etaf-layout-string--merge-inline (inline-strings &optional container-width)
  "合并 inline 字符串，超过容器宽度时换行。
INLINE-STRINGS 是 inline 元素字符串列表。
CONTAINER-WIDTH 是容器宽度。"
  (if (or (null inline-strings)
          (null container-width)
          (<= container-width 0))
      (if inline-strings
          (etaf-lines-concat inline-strings)
        "")
    (let* ((widths (mapcar #'string-pixel-width inline-strings))
           (total-width (apply #'+ widths)))
      (if (<= total-width container-width)
          (etaf-lines-concat inline-strings)
        (let* ((line-breaks (etaf-flex-line-breaks container-width widths 0))
               (lines '())
               (idx 0))
          (dolist (count line-breaks)
            (let ((line-strings (seq-subseq inline-strings idx (+ idx count))))
              (push (etaf-lines-concat line-strings) lines)
              (setq idx (+ idx count))))
          (if lines
              (etaf-lines-stack (nreverse lines))
            ""))))))

;;; ============================================================
;;; 内部函数：Flex 布局合并
;;; ============================================================

(defun etaf-layout-string--merge-flex-children (child-strings flex-direction
                                                              row-gap column-gap
                                                              justify-content
                                                              container-width container-height
                                                              flex-wrap align-items align-content)
  "根据 Flex 布局属性合并子元素字符串。
CHILD-STRINGS 是子元素字符串列表。
FLEX-DIRECTION 是主轴方向。
ROW-GAP/COLUMN-GAP 是行列间隙。
JUSTIFY-CONTENT 是主轴对齐。
CONTAINER-WIDTH/HEIGHT 是容器尺寸。
FLEX-WRAP 是换行模式。
ALIGN-ITEMS 是交叉轴对齐。
ALIGN-CONTENT 是多行对齐。"
  (if (null child-strings)
      ""
    (let* ((is-row (or (string= flex-direction "row")
                       (string= flex-direction "row-reverse")))
           (is-reversed (or (string= flex-direction "row-reverse")
                            (string= flex-direction "column-reverse")))
           (main-gap (if is-row column-gap row-gap))
           (cross-gap (if is-row row-gap column-gap))
           (flex-wrap (or flex-wrap "nowrap"))
           (align-items (or align-items "stretch"))
           (align-content (or align-content "stretch"))
           (valid-strings (seq-filter (lambda (s) (> (length s) 0))
                                      child-strings))
           (items-count (length valid-strings))
           (container-main-size (if is-row
                                    (or container-width 0)
                                  (or container-height 0)))
           (container-cross-size (if is-row
                                     (or container-height 0)
                                   (or container-width 0)))
           (items-units-lst (if is-row
                                (mapcar #'string-pixel-width valid-strings)
                              (mapcar #'etaf-string-linum valid-strings)))
           (items-units (apply #'+ items-units-lst))
           (gaps-units (* main-gap (max 0 (1- items-count))))
           (rest-units (if (> container-main-size 0)
                           (- container-main-size items-units gaps-units)
                         0))
           (wrap-lst
            (if (and (not (string= flex-wrap "nowrap"))
                     (> items-count 1)
                     (> container-main-size 0)
                     (< rest-units 0))
                (etaf-flex-line-breaks container-main-size
                                       items-units-lst main-gap)
              (list items-count))))
      (if (null valid-strings)
          ""
        (etaf-layout-string--render-flex-lines
         valid-strings items-units-lst wrap-lst
         is-row is-reversed main-gap cross-gap
         container-main-size container-cross-size
         justify-content flex-wrap align-items align-content)))))

(defun etaf-layout-string--render-flex-lines (valid-strings items-units-lst wrap-lst
                                                            is-row is-reversed main-gap cross-gap
                                                            container-main-size container-cross-size
                                                            justify-content flex-wrap align-items align-content)
  "渲染 Flex 行。"
  (let ((lines-strings nil)
        (cross-max-units-lst nil)
        (prev 0))
    ;; 处理每一行
    (dolist (num wrap-lst)
      (when (> num 0)
        (let* ((line-strings (seq-subseq valid-strings prev (+ prev num)))
               (line-units-lst (seq-subseq items-units-lst prev (+ prev num)))
               (line-items-units (apply #'+ line-units-lst))
               (line-gaps-units (* main-gap (max 0 (1- num))))
               (line-rest-units (if (> container-main-size 0)
                                    (max 0 (- container-main-size line-items-units line-gaps-units))
                                  0))
               (main-gaps-lst (etaf-layout-string--content-justify
                               num line-rest-units justify-content main-gap))
               (line-cross-max-units (if is-row
                                         (apply #'max (mapcar #'etaf-string-linum line-strings))
                                       (apply #'max (mapcar #'string-pixel-width line-strings))))
               (ordered-line-strings (if is-reversed
                                         (nreverse (copy-sequence line-strings))
                                       line-strings))
               (ordered-main-gaps (if is-reversed
                                      (nreverse (copy-sequence main-gaps-lst))
                                    main-gaps-lst))
               (line-string
                (if is-row
                    (etaf-layout-string--concat-with-gaps
                     ordered-line-strings ordered-main-gaps
                     line-cross-max-units align-items t)
                  (etaf-layout-string--stack-with-gaps
                   ordered-line-strings ordered-main-gaps
                   line-cross-max-units align-items))))
          (push line-cross-max-units cross-max-units-lst)
          (push line-string lines-strings)
          (setq prev (+ prev num)))))
    
    (setq lines-strings (nreverse lines-strings))
    (setq cross-max-units-lst (nreverse cross-max-units-lst))
    
    ;; wrap-reverse 处理
    (when (string= flex-wrap "wrap-reverse")
      (setq lines-strings (nreverse lines-strings))
      (setq cross-max-units-lst (nreverse cross-max-units-lst)))
    
    ;; 合并所有行
    (if (= (length lines-strings) 1)
        (car lines-strings)
      (let* ((lines-count (length lines-strings))
             (total-cross-units (apply #'+ cross-max-units-lst))
             (total-cross-gaps (* cross-gap (max 0 (1- lines-count))))
             (cross-rest-units (if (> container-cross-size 0)
                                   (max 0 (- container-cross-size total-cross-units total-cross-gaps))
                                 0))
             (cross-gaps-lst (etaf-layout-string--content-justify
                              lines-count cross-rest-units align-content cross-gap)))
        (if is-row
            (etaf-layout-string--stack-lines-with-gaps lines-strings cross-gaps-lst)
          (etaf-layout-string--concat-lines-with-gaps lines-strings cross-gaps-lst))))))

;;; ============================================================
;;; 内部函数：空间分配
;;; ============================================================

(defun etaf-layout-string--content-justify (items-num rest-units justify-content gap)
  "计算主轴间隙列表。
返回 (start-gap gap1 gap2 ... end-gap) 列表。"
  (let ((rest-units (max 0 rest-units)))
    (cond
     ((<= items-num 0)
      (list rest-units))
     ((= items-num 1)
      (pcase justify-content
        ("flex-start" (list 0 rest-units))
        ("flex-end" (list rest-units 0))
        ("center" (let ((half (/ rest-units 2)))
                    (list half (- rest-units half))))
        (_ (list 0 rest-units))))
     (t
      (pcase justify-content
        ("flex-start"
         (append (list 0)
                 (make-list (1- items-num) gap)
                 (list rest-units)))
        ("flex-end"
         (append (list rest-units)
                 (make-list (1- items-num) gap)
                 (list 0)))
        ("center"
         (let ((half (/ rest-units 2)))
           (append (list half)
                   (make-list (1- items-num) gap)
                   (list (- rest-units half)))))
        ("space-between"
         (let ((between (/ (+ rest-units (* gap (1- items-num))) (1- items-num))))
           (append (list 0)
                   (make-list (1- items-num) between)
                   (list 0))))
        ("space-around"
         (let* ((unit (/ rest-units (* 2 items-num)))
                (between (+ (* 2 unit) gap)))
           (append (list unit)
                   (make-list (1- items-num) between)
                   (list unit))))
        ("space-evenly"
         (let ((space (/ rest-units (1+ items-num))))
           (append (list space)
                   (make-list (1- items-num) (+ space gap))
                   (list space))))
        (_
         (append (list 0)
                 (make-list (1- items-num) gap)
                 (list rest-units))))))))

;;; ============================================================
;;; 内部函数：拼接与堆叠
;;; ============================================================

(defun etaf-layout-string--concat-with-gaps (strings gaps-lst cross-max-units align-items is-row)
  "使用间隙水平拼接 Flex Item。"
  (if (null strings)
      ""
    (let* ((aligned-strings
            (mapcar (lambda (str)
                      (etaf-layout-string--align-cross-axis
                       str cross-max-units align-items is-row))
                    strings))
           (gap-spacings (mapcar (lambda (gap)
                                   (if (and gap (> gap 0))
                                       (etaf-pixel-spacing gap)
                                     ""))
                                 gaps-lst)))
      (etaf-lines-concat
       (etaf-interleave gap-spacings aligned-strings)))))

(defun etaf-layout-string--stack-with-gaps (strings gaps-lst cross-max-units align-items)
  "使用间隙垂直堆叠 Flex Item。"
  (if (null strings)
      ""
    (let* ((aligned-strings
            (mapcar (lambda (str)
                      (etaf-layout-string--align-cross-axis
                       str cross-max-units align-items nil))
                    strings))
           (max-width (if aligned-strings
                          (apply #'max (mapcar #'string-pixel-width aligned-strings))
                        0))
           (gap-blanks (mapcar (lambda (gap)
                                 (if (and gap (> gap 0) (> max-width 0))
                                     (etaf-string-duplines "" gap)
                                   nil))
                               gaps-lst)))
      (etaf-lines-stack
       (etaf-interleave gap-blanks aligned-strings)))))

(defun etaf-layout-string--stack-lines-with-gaps (line-strings cross-gaps-lst)
  "使用间隙垂直堆叠多行。"
  (if (null line-strings)
      ""
    (let* ((max-width (apply #'max (mapcar #'string-pixel-width line-strings)))
           (gap-blanks (mapcar (lambda (gap)
                                 (if (and gap (> gap 0) (> max-width 0))
                                     (etaf-string-duplines "" (floor gap))
                                   nil))
                               cross-gaps-lst)))
      (etaf-lines-stack
       (etaf-interleave gap-blanks line-strings)))))

(defun etaf-layout-string--concat-lines-with-gaps (line-strings cross-gaps-lst)
  "使用间隙水平拼接多行。"
  (if (null line-strings)
      ""
    (let* ((gap-spacings (mapcar (lambda (gap)
                                   (if (and gap (> gap 0))
                                       (etaf-pixel-spacing (floor gap))
                                     ""))
                                 cross-gaps-lst)))
      (etaf-lines-concat
       (etaf-interleave gap-spacings line-strings)))))

;;; ============================================================
;;; 内部函数：交叉轴对齐
;;; ============================================================

(defun etaf-layout-string--align-cross-axis (string cross-max-units align-items is-row)
  "在交叉轴方向对齐单个 Item。
STRING 是 Item 字符串。
CROSS-MAX-UNITS 是交叉轴方向最大尺寸。
ALIGN-ITEMS 是对齐方式。
IS-ROW 表示是否为行方向（交叉轴为垂直）。"
  (if (or (null string) (= (length string) 0))
      string
    (let* ((item-cross-units (if is-row
                                 (etaf-string-linum string)
                               (string-pixel-width string)))
           (rest-units (max 0 (- cross-max-units item-cross-units))))
      (if (<= rest-units 0)
          string
        (pcase align-items
          ("flex-start"
           (if is-row
               (etaf-lines-stack
                (list string
                      (etaf-pixel-blank (string-pixel-width string) rest-units)))
             (etaf-lines-concat
              (list string
                    (etaf-pixel-blank rest-units (etaf-string-linum string))))))
          ("flex-end"
           (if is-row
               (etaf-lines-stack
                (list (etaf-pixel-blank (string-pixel-width string) rest-units)
                      string))
             (etaf-lines-concat
              (list (etaf-pixel-blank rest-units (etaf-string-linum string))
                    string))))
          ("center"
           (let ((start-units (/ rest-units 2))
                 (end-units (- rest-units (/ rest-units 2))))
             (if is-row
                 (etaf-lines-stack
                  (list (when (> start-units 0)
                          (etaf-pixel-blank (string-pixel-width string) start-units))
                        string
                        (when (> end-units 0)
                          (etaf-pixel-blank (string-pixel-width string) end-units))))
               (etaf-lines-concat
                (list (when (> start-units 0)
                        (etaf-pixel-blank start-units (etaf-string-linum string)))
                      string
                      (when (> end-units 0)
                        (etaf-pixel-blank end-units (etaf-string-linum string))))))))
          ((or "stretch" "baseline" _)
           (if is-row
               (etaf-lines-stack
                (list string
                      (when (> rest-units 0)
                        (etaf-pixel-blank (string-pixel-width string) rest-units))))
             (etaf-lines-concat
              (list string
                    (when (> rest-units 0)
                      (etaf-pixel-blank rest-units (etaf-string-linum string))))))))))))

(provide 'etaf-layout-string)
;;; etaf-layout-string.el ends here
