;;; etaf-layout.el --- Layout computation for render tree -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, box-model, css
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 实现 CSS 盒模型和布局算法
;;
;; 本模块负责从渲染树构建布局树，计算每个元素的：
;; - 盒模型（content, padding, border, margin）
;; - 尺寸（width, height）
;;
;; 布局树使用 DOM 格式表示，保留渲染树的 DOM 结构，附加的布局信息用属性表示：
;; - layout-box-model: 盒模型信息
;;
;; 布局树结构：
;; (tag ((layout-box-model . <box-model>)
;;       (render-style . ((color . "red") ...))  ;; 从渲染树继承
;;       (render-display . "block")              ;; 从渲染树继承
;;       (class . "foo"))                        ;; 原始 DOM 属性
;;   child1 child2 ...)
;;
;; 盒模型 (Box Model):
;;   (:box-sizing "content-box"|"border-box"
;;    :content (:width <number> :height <number>)
;;    :padding (:top <n> :right <n> :bottom <n> :left <n>)
;;    :border (:top-width <n> :right-width <n> :bottom-width <n> :left-width <n>
;;             :top-color <color> :right-color <color> :bottom-color <color> :left-color <color>)
;;    :margin (:top <n> :right <n> :bottom <n> :left <n>))
;;
;; 渲染方式：
;; 使用后序遍历（post-order traversal）从叶子节点开始构建字符串，
;; 通过文本拼接生成最终布局，不使用位置坐标。
;;
;; 使用示例：
;;
;;   ;; 构建布局树
;;   (setq layout-tree (etaf-layout-build-tree render-tree viewport))
;;
;;   ;; 遍历布局树 - 可以直接使用 etaf-dom-map
;;   (etaf-dom-map
;;     (lambda (node)
;;       (let ((box (etaf-layout-get-box-model node)))
;;         (message "Size: %dx%d"
;;                  (plist-get (plist-get box :content) :width)
;;                  (plist-get (plist-get box :content) :height))))
;;     layout-tree)

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-render)
(require 'etaf-utils)
(require 'etaf-css-face)

;; 像素到行数的转换系数（假设默认行高约为 20 像素）
(defconst etaf-layout-pixels-per-line 20
  "每行的像素数，用于将 px 单位转换为行数。")

;;; 辅助函数：CSS 值解析

(defun etaf-layout-parse-length (value reference-width)
  "解析 CSS 长度值。
VALUE 是 CSS 值字符串。
REFERENCE-WIDTH 是参考宽度（用于百分比计算）。
返回像素值或 'auto 或 'none。"
  (cond
   ((null value) 'auto)
   ((eq value 'auto) 'auto)
   ((eq value 'none) 'none)
   ((numberp value) value)
   ((string= value "auto") 'auto)
   ((string= value "none") 'none)
   ((string= value "0") 0)
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-width))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    ;; 简化：假设 1em = 16px
    (* (string-to-number (match-string 1 value)) 16))
   ((string-match "\\`\\([0-9.]+\\)lh\\'" value)
    ;; lh 单位：行高单位，直接返回行数
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-layout-parse-height (value reference-height)
  "解析 CSS 高度值。
VALUE 是 CSS 值字符串。
REFERENCE-HEIGHT 是参考高度（用于百分比计算）。
在 Emacs 中，高度默认使用行数（lh）作为单位。
返回行数或 'auto 或 'none。"
  (cond
   ((null value) 'auto)
   ((eq value 'auto) 'auto)
   ((eq value 'none) 'none)
   ((numberp value) value)  ; 数字直接作为行数
   ((string= value "auto") 'auto)
   ((string= value "none") 'none)
   ((string= value "0") 0)
   ;; lh 单位：行高单位，直接返回行数
   ((string-match "\\`\\([0-9.]+\\)lh\\'" value)
    (string-to-number (match-string 1 value)))
   ;; 纯数字（无单位）：作为行数
   ((string-match "\\`\\([0-9.]+\\)\\'" value)
    (string-to-number (match-string 1 value)))
   ;; 百分比：相对于参考高度
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-height))
   ;; px 单位：使用配置的像素/行转换系数
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (ceiling (/ (string-to-number (match-string 1 value))
                (float etaf-layout-pixels-per-line))))
   ;; em 单位：简化处理，假设 1em = 1 行
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-layout-get-style-value (computed-style property &optional default)
  "从计算样式中获取属性值。
COMPUTED-STYLE 是 alist 形式的计算样式。
PROPERTY 是属性名（symbol）。
DEFAULT 是默认值（可选）。"
  (or (cdr (assq property computed-style)) default))

;;; 盒模型辅助函数

(defun etaf-box-model-create ()
  "创建空的盒模型结构。"
  (list :box-sizing "content-box"
        :content (list :width 0 :height 0)
        :padding (list :top 0 :right 0 :bottom 0 :left 0)
        :border (list :top-width 0 :right-width 0 :bottom-width 0 :left-width 0
                      :top-color (face-attribute 'default :foreground)
                      :right-color (face-attribute 'default :foreground)
                      :bottom-color (face-attribute 'default :foreground)
                      :left-color (face-attribute 'default :foreground))
        :margin (list :top 0 :right 0 :bottom 0 :left 0)))

(defun etaf-box-model-content-width (box-model)
  "获取盒模型的内容宽度。"
  (plist-get (plist-get box-model :content) :width))

(defun etaf-box-model-content-height (box-model)
  "获取盒模型的内容高度。"
  (plist-get (plist-get box-model :content) :height))

(defun etaf-box-model-padding-width (box-model)
  "获取盒模型的左右内边距之和。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :left)
       (plist-get padding :right))))

(defun etaf-box-model-padding-height (box-model)
  "获取盒模型的上下内边距之和。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :top)
       (plist-get padding :bottom))))

(defun etaf-box-model-border-width (box-model)
  "获取盒模型的左右边框之和。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :left-width)
       (plist-get border :right-width))))

(defun etaf-box-model-border-height (box-model)
  "获取盒模型的上下边框之和。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :top-width)
       (plist-get border :bottom-width))))

(defun etaf-box-model-margin-width (box-model)
  "获取盒模型的左右外边距之和。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :left)
       (plist-get margin :right))))

(defun etaf-box-model-margin-height (box-model)
  "获取盒模型的上下外边距之和。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :top)
       (plist-get margin :bottom))))

(defun etaf-box-model-total-width (box-model)
  "计算盒模型的总宽度（包含 margin）。"
  (+ (etaf-box-model-content-width box-model)
     (etaf-box-model-padding-width box-model)
     (etaf-box-model-border-width box-model)
     (etaf-box-model-margin-width box-model)))

(defun etaf-box-model-total-height (box-model)
  "计算盒模型的总高度（包含 margin）。"
  (+ (etaf-box-model-content-height box-model)
     (etaf-box-model-padding-height box-model)
     (etaf-box-model-border-height box-model)
     (etaf-box-model-margin-height box-model)))

;;; 盒模型计算

;;; 布局树辅助函数

(defun etaf-layout-get-box-model (layout-node)
  "从布局节点获取盒模型。
LAYOUT-NODE 是布局节点。
返回盒模型 plist。"
  (dom-attr layout-node 'layout-box-model))

(defun etaf-layout-create-node (render-node box-model)
  "创建布局节点（使用 DOM 格式）。
RENDER-NODE 是渲染节点。
BOX-MODEL 是盒模型 plist。
返回 DOM 格式的布局节点，保留渲染节点的结构并添加布局信息。"
  (let* ((tag (dom-tag render-node))
         (render-attrs (dom-attributes render-node))
         ;; 构建布局属性
         (layout-attrs (list (cons 'layout-box-model box-model))))
    ;; 合并布局属性和渲染属性
    (list tag (append layout-attrs render-attrs))))

(defun etaf-layout-compute-box-model (render-node parent-context)
  "从渲染节点计算盒模型。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息：
  :content-width  - 可用内容宽度
  :content-height - 可用内容高度
返回盒模型 plist。"
  (let* ((style (etaf-render-get-computed-style render-node))
         (parent-width (plist-get parent-context :content-width))
         (parent-height (plist-get parent-context :content-height))
         
         ;; 提取样式值
         (box-sizing (etaf-layout-get-style-value
                      style 'box-sizing "content-box"))
         
         ;; Padding
         (padding-top (etaf-layout-parse-length 
                       (etaf-layout-get-style-value
                        style 'padding-top "0") 
                       parent-width))
         (padding-right (etaf-layout-parse-length 
                         (etaf-layout-get-style-value
                          style 'padding-right "0") 
                         parent-width))
         (padding-bottom (etaf-layout-parse-length 
                          (etaf-layout-get-style-value
                           style 'padding-bottom "0") 
                          parent-width))
         (padding-left (etaf-layout-parse-length 
                        (etaf-layout-get-style-value style 'padding-left "0") 
                        parent-width))
         
         ;; Border
         (border-top (etaf-layout-parse-length 
                      (etaf-layout-get-style-value style 'border-top-width "0") 
                      parent-width))
         (border-right (etaf-layout-parse-length 
                        (etaf-layout-get-style-value
                         style 'border-right-width "0") 
                        parent-width))
         (border-bottom (etaf-layout-parse-length 
                         (etaf-layout-get-style-value
                          style 'border-bottom-width "0") 
                         parent-width))
         (border-left (etaf-layout-parse-length 
                       (etaf-layout-get-style-value
                        style 'border-left-width "0") 
                       parent-width))
         
         (border-top-color (etaf-layout-get-style-value
                            style 'border-top-color
                            (face-attribute 'default :foreground)))
         (border-right-color (etaf-layout-get-style-value
                              style 'border-right-color
                              (face-attribute 'default :foreground)))
         (border-bottom-color (etaf-layout-get-style-value
                               style 'border-bottom-color
                               (face-attribute 'default :foreground)))
         (border-left-color (etaf-layout-get-style-value
                             style 'border-left-color
                             (face-attribute 'default :foreground)))
         
         ;; Margin
         (margin-top (etaf-layout-parse-length 
                      (etaf-layout-get-style-value style 'margin-top "0") 
                      parent-width))
         (margin-right (etaf-layout-parse-length 
                        (etaf-layout-get-style-value style 'margin-right "0") 
                        parent-width))
         (margin-bottom (etaf-layout-parse-length 
                         (etaf-layout-get-style-value style 'margin-bottom "0") 
                         parent-width))
         (margin-left (etaf-layout-parse-length 
                       (etaf-layout-get-style-value style 'margin-left "0") 
                       parent-width))
         
         ;; Width and Height
         ;; 宽度使用 etaf-layout-parse-length（像素单位）
         (width-value (etaf-layout-parse-length 
                       (etaf-layout-get-style-value style 'width "auto") 
                       parent-width))
         ;; 高度使用 etaf-layout-parse-height（行数单位）
         (height-value (etaf-layout-parse-height 
                        (etaf-layout-get-style-value style 'height "auto") 
                        parent-height))
         
         ;; min-width, max-width (像素单位)
         (min-width-value (etaf-layout-parse-length
                           (etaf-layout-get-style-value style 'min-width "0")
                           parent-width))
         (max-width-value (etaf-layout-parse-length
                           (etaf-layout-get-style-value style 'max-width "none")
                           parent-width))
         
         ;; min-height, max-height (行数单位)
         (min-height-value (etaf-layout-parse-height
                            (etaf-layout-get-style-value style 'min-height "0")
                            parent-height))
         (max-height-value (etaf-layout-parse-height
                            (etaf-layout-get-style-value style 'max-height "none")
                            parent-height))
         
         ;; 处理 auto 值
         (padding-top-val (if (eq padding-top 'auto) 0 padding-top))
         (padding-right-val (if (eq padding-right 'auto) 0 padding-right))
         (padding-bottom-val (if (eq padding-bottom 'auto) 0 padding-bottom))
         (padding-left-val (if (eq padding-left 'auto) 0 padding-left))
         
         (border-top-val (if (eq border-top 'auto) 0 border-top))
         (border-right-val (if (eq border-right 'auto) 0 border-right))
         (border-bottom-val (if (eq border-bottom 'auto) 0 border-bottom))
         (border-left-val (if (eq border-left 'auto) 0 border-left))
         
         (margin-top-val (if (eq margin-top 'auto) 0 margin-top))
         (margin-right-val (if (eq margin-right 'auto) 0 margin-right))
         (margin-bottom-val (if (eq margin-bottom 'auto) 0 margin-bottom))
         (margin-left-val (if (eq margin-left 'auto) 0 margin-left))
         
         ;; 获取 display 类型用于宽度计算
         (display (etaf-render-get-display render-node))
         (is-inline (string= display "inline"))
         ;; 检查是否在 flex 容器内
         (is-in-flex-container (plist-get parent-context :flex-container))
         
         ;; 处理 min/max 值: auto/none 应该被忽略
         (min-width-val (if (or (eq min-width-value 'auto)
                                (eq min-width-value 'none))
                            0
                          min-width-value))
         (max-width-val (if (or (eq max-width-value 'auto)
                                (eq max-width-value 'none))
                            nil  ; nil 表示无限制
                          max-width-value))
         (min-height-val (if (or (eq min-height-value 'auto)
                                 (eq min-height-value 'none))
                             0
                           min-height-value))
         (max-height-val (if (or (eq max-height-value 'auto)
                                 (eq max-height-value 'none))
                             nil  ; nil 表示无限制
                           max-height-value))
         
         ;; 计算内容宽度
         ;; 对于内联元素，width:auto 时宽度应该为 0，让后续根据实际内容计算
         ;; 对于块级元素，width:auto 时宽度应该填充父容器
         ;; 但是当位于 flex 容器内时，块级元素的宽度应该由 flex 布局算法计算
         ;; 基于 grow, shrink, basis 和 gap 等属性，而不是自动填充父容器宽度
         ;; 初始值设为 0，后续由 flex 算法调整实际宽度
         (base-content-width
          (if (eq width-value 'auto)
              (if (or is-inline is-in-flex-container)
                  0  ; 初始值：后续由内容尺寸或flex算法决定实际宽度
                (max 0 (- parent-width
                          padding-left-val padding-right-val
                          border-left-val border-right-val
                          margin-left-val margin-right-val)))
            width-value))
         
         ;; 应用 min-width, max-width 约束
         ;; 参考 etaf-box.el 中的 etaf-box-content-pixel 函数
         ;; min-width-val 经过上面处理后保证为 0 或正数，max-width-val 为 nil 或正数
         (content-width (min (or max-width-val most-positive-fixnum)
                             (max min-width-val base-content-width)))
         
         ;; 计算内容高度（如果指定）
         (base-content-height (if (eq height-value 'auto)
                                  0  ; 将在后续根据子元素计算
                                height-value))
         
         ;; 应用 min-height, max-height 约束
         ;; 参考 etaf-box.el 中的 etaf-box-content-height 函数
         ;; min-height-val 经过上面处理后保证为 0 或正数，max-height-val 为 nil 或正数
         (content-height (min (or max-height-val most-positive-fixnum)
                              (max min-height-val base-content-height))))
    
    ;; 构建盒模型
    (list :box-sizing box-sizing
          :content (list :width content-width
                         :height content-height)
          :padding (list :top padding-top-val
                         :right padding-right-val
                         :bottom padding-bottom-val
                         :left padding-left-val)
          :border (list :top-width border-top-val
                        :right-width border-right-val
                        :bottom-width border-bottom-val
                        :left-width border-left-val
                        :top-color border-top-color
                        :right-color border-right-color
                        :bottom-color border-bottom-color
                        :left-color border-left-color)
          :margin (list :top margin-top-val
                        :right margin-right-val
                        :bottom margin-bottom-val
                        :left margin-left-val))))

;;; 布局算法

(defun etaf-layout-block-formatting-context (render-node parent-context)
  "在块级格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息。
返回布局节点。"
  (let* ((box-model (etaf-layout-compute-box-model render-node parent-context))
         (content-width (etaf-box-model-content-width box-model))
         (content-height (etaf-box-model-content-height box-model))
         
         ;; 创建布局节点（不再需要位置信息）
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 递归布局子元素并保留文本节点
    ;; 注意：与之前版本不同，现在保留文本节点以便在渲染时显示文本内容
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                   :content-height content-height))
              (child-layouts '())
              (accumulated-height 0))
          
          (dolist (child children)
            (cond
             ;; 元素节点：递归布局
             ((and (consp child) (symbolp (car child)))
              (when-let ((child-layout (etaf-layout-node child child-context)))
                (push child-layout child-layouts)
                ;; 累积子元素高度
                (let ((child-total-height (etaf-box-model-total-height 
                                           (etaf-layout-get-box-model child-layout))))
                  (setq accumulated-height (+ accumulated-height child-total-height)))))
             ;; 文本节点：直接保留（不需要布局计算，仅传递给父节点）
             ((stringp child)
              (push child child-layouts))))
          
          ;; 将子节点添加到布局节点（DOM 格式）
          (setcdr (cdr layout-node) (nreverse child-layouts))
          
          ;; 如果高度为 auto，根据子元素设置高度
          (when (= content-height 0)
            (let ((box (etaf-layout-get-box-model layout-node)))
              (plist-put (plist-get box :content) :height accumulated-height))))))
    
    layout-node))

;;; Flex Layout Support

(defun etaf-layout-flex-formatting-context (render-node parent-context)
  "Layout node in flex formatting context.
RENDER-NODE is the render node to layout (display: flex).
PARENT-CONTEXT contains parent container context information.
Returns the layout node.

Supported flex container properties:
- flex-direction: row | row-reverse | column | column-reverse
- flex-wrap: nowrap | wrap | wrap-reverse
- justify-content: flex-start | flex-end | center | space-between | space-around | space-evenly
- align-items: stretch | flex-start | flex-end | center | baseline
- align-content: stretch | flex-start | flex-end | center | space-between | space-around
- gap, row-gap, column-gap

Supported flex item properties:
- order
- flex-grow
- flex-shrink
- flex-basis
- align-self"
  (let* ((box-model (etaf-layout-compute-box-model render-node parent-context))
         (computed-style (etaf-render-get-computed-style render-node))
         (content-width (etaf-box-model-content-width box-model))
         (content-height (etaf-box-model-content-height box-model))
         
         ;; Flex 容器属性
         (flex-direction (or (etaf-layout-get-style-value computed-style 'flex-direction)
                             "row"))
         (flex-wrap (or (etaf-layout-get-style-value computed-style 'flex-wrap)
                        "nowrap"))
         (justify-content (or (etaf-layout-get-style-value computed-style 'justify-content)
                              "flex-start"))
         (align-items (or (etaf-layout-get-style-value computed-style 'align-items)
                          "stretch"))
         (align-content (or (etaf-layout-get-style-value computed-style 'align-content)
                            "stretch"))
         (row-gap-str (etaf-layout-get-style-value computed-style 'row-gap "0"))
         (column-gap-str (etaf-layout-get-style-value computed-style 'column-gap "0"))
         (row-gap-parsed (etaf-layout-parse-length row-gap-str content-width))
         (column-gap-parsed (etaf-layout-parse-length column-gap-str content-width))
         (row-gap (if (eq row-gap-parsed 'auto) 0 row-gap-parsed))
         (column-gap (if (eq column-gap-parsed 'auto) 0 column-gap-parsed))
         
         ;; Main axis is horizontal
         (is-row-direction (or (string= flex-direction "row")
                               (string= flex-direction "row-reverse")))
         ;; Main axis is reversed
         (is-reversed (or (string= flex-direction "row-reverse")
                          (string= flex-direction "column-reverse")))
         ;; Whether to wrap
         (should-wrap (not (string= flex-wrap "nowrap")))
         ;; Wrap is reversed
         (wrap-reversed (string= flex-wrap "wrap-reverse"))
         
         ;; Create layout node
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 添加 flex 相关属性到布局节点
    (dom-set-attribute layout-node 'layout-flex-direction flex-direction)
    (dom-set-attribute layout-node 'layout-flex-wrap flex-wrap)
    (dom-set-attribute layout-node 'layout-justify-content justify-content)
    (dom-set-attribute layout-node 'layout-align-items align-items)
    (dom-set-attribute layout-node 'layout-align-content align-content)
    (dom-set-attribute layout-node 'layout-row-gap row-gap)
    (dom-set-attribute layout-node 'layout-column-gap column-gap)
    
    ;; 布局子元素
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                   :content-height content-height
                                   :flex-container t
                                   :flex-direction flex-direction
                                   :align-items align-items))
              (child-layouts '())
              (flex-items '()))
          
          ;; 首先递归布局所有子元素
          (dolist (child children)
            (cond
             ;; 元素节点
             ((and (consp child) (symbolp (car child)))
              (when-let ((child-layout (etaf-layout-node child child-context)))
                (push child-layout child-layouts)
                ;; 收集 flex item 信息
                (let* ((child-style (etaf-render-get-computed-style child))
                       (order (or (etaf-layout-parse-flex-number
                                   (etaf-layout-get-style-value child-style 'order))
                                  0))
                       (flex-grow
                        (or (etaf-layout-parse-flex-number
                             (etaf-layout-get-style-value child-style 'flex-grow))
                            0))
                       (flex-shrink
                        (or (etaf-layout-parse-flex-number
                             (etaf-layout-get-style-value child-style 'flex-shrink))
                            1))
                       (flex-basis (etaf-layout-get-style-value
                                    child-style 'flex-basis "auto"))
                       (align-self
                        (etaf-layout-get-style-value child-style 'align-self)))
                  ;; 添加 flex item 属性到子布局节点
                  (dom-set-attribute child-layout 'layout-order order)
                  (dom-set-attribute child-layout 'layout-flex-grow flex-grow)
                  (dom-set-attribute child-layout 'layout-flex-shrink flex-shrink)
                  (dom-set-attribute child-layout 'layout-flex-basis flex-basis)
                  (when align-self
                    (dom-set-attribute child-layout 'layout-align-self align-self))
                  (push (list :layout child-layout
                              :order order
                              :flex-grow flex-grow
                              :flex-shrink flex-shrink
                              :flex-basis flex-basis)
                        flex-items))))
             ;; 文本节点：直接保留
             ((stringp child)
              (push child child-layouts))))
          
          ;; 按 order 排序 flex items
          (setq flex-items (sort (nreverse flex-items)
                                 (lambda (a b)
                                   (< (plist-get a :order)
                                      (plist-get b :order)))))
          
          ;; 根据排序结果重新排列子节点
          (let ((sorted-children '()))
            (dolist (item flex-items)
              (push (plist-get item :layout) sorted-children))
            ;; 添加文本节点
            (dolist (child child-layouts)
              (when (stringp child)
                (push child sorted-children)))
            ;; Don't reverse here for row-reverse/column-reverse
            ;; The reversal is handled during string rendering in etaf-layout--merge-flex-children
            ;; (following the same pattern as etaf-flex.el which reverses at render time)
            ;; 将排序后的子节点添加到布局节点
            (setcdr (cdr layout-node) (nreverse sorted-children)))
          
          ;; 计算 flex 布局（主轴分配）
          (etaf-layout-flex-compute-main-axis
           layout-node flex-items content-width content-height
           flex-direction justify-content row-gap column-gap should-wrap)
          
          ;; 计算交叉轴对齐
          (etaf-layout-flex-compute-cross-axis
           layout-node flex-items content-width content-height
           flex-direction align-items align-content))))
    
    layout-node))

(defun etaf-layout-parse-flex-number (value)
  "Parse flex-grow/flex-shrink/order numeric properties.
VALUE can be a string or number.
Returns number or nil."
  (cond
   ((numberp value) value)
   ((and (stringp value) (string-match "^-?[0-9]+\\(\\.[0-9]+\\)?$" value))
    (string-to-number value))
   (t nil)))

(defun etaf-layout-flex-compute-main-axis (layout-node flex-items
                                                       container-width container-height
                                                       direction justify-content
                                                       row-gap column-gap should-wrap)
  "Compute flex layout main axis distribution.
LAYOUT-NODE is the flex container layout node.
FLEX-ITEMS is the list of flex items.
CONTAINER-WIDTH/HEIGHT are container dimensions.
DIRECTION is flex-direction.
JUSTIFY-CONTENT is main axis alignment.
ROW-GAP/COLUMN-GAP are gap values.
SHOULD-WRAP indicates whether wrapping is enabled."
  (let* ((is-row (or (string= direction "row")
                     (string= direction "row-reverse")))
         (main-size (if is-row container-width container-height))
         (main-gap (if is-row column-gap row-gap))
         (items-count (length flex-items))
         (total-flex-basis 0)
         (total-flex-grow 0)
         (total-flex-shrink 0)
         ;; Collect item sizes for grow/shrink calculation
         (item-sizes '()))
    
    ;; 计算总的 flex-basis、flex-grow 和 flex-shrink
    (dolist (item flex-items)
      (let* ((layout (plist-get item :layout))
             (box-model (etaf-layout-get-box-model layout))
             (item-main-size (if is-row
                                 (etaf-box-model-total-width box-model)
                               (etaf-box-model-total-height box-model)))
             (item-content-size (if is-row
                                    (etaf-box-model-content-width box-model)
                                  (etaf-box-model-content-height box-model)))
             ;; Calculate side size (padding + border + margin)
             (item-side-size (- item-main-size item-content-size)))
        (push (list :item item
                    :layout layout
                    :box-model box-model
                    :main-size item-main-size
                    :content-size item-content-size
                    :side-size item-side-size)
              item-sizes)
        (setq total-flex-basis (+ total-flex-basis item-main-size))
        (setq total-flex-grow (+ total-flex-grow (plist-get item :flex-grow)))
        (setq total-flex-shrink (+ total-flex-shrink (plist-get item :flex-shrink)))))
    
    ;; Reverse to maintain original order
    (setq item-sizes (nreverse item-sizes))
    
    ;; 计算总 gap
    (let* ((total-gap (* main-gap (max 0 (1- items-count))))
           (available-space (- main-size total-flex-basis total-gap))
           (free-space (max 0 available-space))
           ;; Fix: Calculate overflow-space correctly as the amount content exceeds container
           (overflow-space (max 0 (- (+ total-flex-basis total-gap) main-size))))
      
      ;; Apply flex-grow when there's free space and total-flex-grow > 0
      (when (and (> free-space 0) (> total-flex-grow 0) (> main-size 0))
        (let* ((grow-unit (/ (float free-space) total-flex-grow))
               (distributed 0))
          (dotimes (i (length flex-items))
            (let* ((item-info (nth i item-sizes))
                   (item (plist-get item-info :item))
                   (layout (plist-get item-info :layout))
                   (box-model (plist-get item-info :box-model))
                   (content-size (plist-get item-info :content-size))
                   (side-size (plist-get item-info :side-size))
                   (flex-grow (plist-get item :flex-grow)))
              (when (> flex-grow 0)
                (let* ((grow-amount (max 0  ;; Fix: Ensure grow-amount is never negative
                                         (if (= i (1- (length flex-items)))
                                             ;; Last item gets remaining space to avoid rounding errors
                                             (- free-space distributed)
                                           (floor (* grow-unit flex-grow)))))
                       (new-content-size (+ content-size grow-amount)))
                  (setq distributed (+ distributed grow-amount))
                  ;; Update box-model content width/height
                  (if is-row
                      (plist-put (plist-get box-model :content) :width new-content-size)
                    (plist-put (plist-get box-model :content) :height new-content-size))))))))
      
      ;; Apply flex-shrink when there's overflow and total-flex-shrink > 0
      (when (and (> overflow-space 0) (> total-flex-shrink 0) (> main-size 0))
        (let* ((shrink-unit (/ (float overflow-space) total-flex-shrink))
               (distributed 0))
          (dotimes (i (length flex-items))
            (let* ((item-info (nth i item-sizes))
                   (item (plist-get item-info :item))
                   (layout (plist-get item-info :layout))
                   (box-model (plist-get item-info :box-model))
                   (content-size (plist-get item-info :content-size))
                   (side-size (plist-get item-info :side-size))
                   (flex-shrink (plist-get item :flex-shrink)))
              (when (> flex-shrink 0)
                ;; Fix: Ensure shrink-amount is never negative
                (let* ((shrink-amount (max 0
                                           (if (= i (1- (length flex-items)))
                                               ;; Last item gets remaining space to avoid rounding errors
                                               (- overflow-space distributed)
                                             (floor (* shrink-unit flex-shrink)))))
                       ;; Don't shrink below 0
                       (new-content-size (max 0 (- content-size shrink-amount))))
                  (setq distributed (+ distributed shrink-amount))
                  ;; Update box-model content width/height
                  (if is-row
                      (plist-put (plist-get box-model :content) :width new-content-size)
                    (plist-put (plist-get box-model :content) :height new-content-size))))))))
      
      ;; 存储计算结果
      (dom-set-attribute layout-node 'layout-flex-free-space free-space)
      (dom-set-attribute layout-node 'layout-flex-total-grow total-flex-grow)
      
      ;; 根据 justify-content 计算主轴分布
      ;; Always compute space distribution, even when free-space is 0
      (when (> items-count 0)
        (let ((space-distribution
               (etaf-layout-flex-justify-space
                justify-content free-space items-count main-gap)))
          (dom-set-attribute layout-node 'layout-flex-space-distribution
                             space-distribution))))))

(defun etaf-layout-flex-justify-space (justify-content free-space items-count gap)
  "Calculate justify-content space distribution.
JUSTIFY-CONTENT is the alignment method.
FREE-SPACE is the remaining space.
ITEMS-COUNT is the number of items.
GAP is the gap between items.
Returns (start-space between-space end-space) list."
  (pcase justify-content
    ("flex-start"
     (list 0 gap free-space))
    ("flex-end"
     (list free-space gap 0))
    ("center"
     (let ((side-space (/ free-space 2.0)))
       (list side-space gap side-space)))
    ("space-between"
     (if (<= items-count 1)
         (list 0 0 0)
       (let ((between (/ (+ free-space (* gap (1- items-count)))
                         (1- items-count))))
         (list 0 between 0))))
    ("space-around"
     (if (<= items-count 0)
         (list 0 0 0)
       (let* ((unit-space (/ free-space (* 2.0 items-count)))
              (between (+ (* 2 unit-space) gap)))
         (list unit-space between unit-space))))
    ("space-evenly"
     (if (<= items-count 0)
         (list 0 0 0)
       (let ((space (/ free-space (1+ items-count))))
         (list space (+ space gap) space))))
    (_
     (list 0 gap free-space))))

(defun etaf-layout-flex-align-content-space (align-content free-space lines-count gap)
  "Calculate align-content space distribution for multiple lines.
ALIGN-CONTENT is the alignment method.
FREE-SPACE is the remaining space in cross axis.
LINES-COUNT is the number of lines.
GAP is the gap between lines.
Returns (start-space between-space end-space) list."
  (pcase align-content
    ("flex-start"
     (list 0 gap free-space))
    ("flex-end"
     (list free-space gap 0))
    ("center"
     (let ((side-space (/ free-space 2.0)))
       (list side-space gap side-space)))
    ("space-between"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let ((between (/ (+ free-space (* gap (1- lines-count)))
                         (1- lines-count))))
         (list 0 between 0))))
    ("space-around"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let* ((unit-space (/ free-space (* 2.0 lines-count)))
              (between (+ (* 2 unit-space) gap)))
         (list unit-space between unit-space))))
    ("space-evenly"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let ((space (/ free-space (1+ lines-count))))
         (list space (+ space gap) space))))
    ;; stretch: distribute extra space equally to lines
    ("stretch"
     (list 0 gap 0))
    (_
     (list 0 gap free-space))))

(defun etaf-layout-flex-compute-cross-axis (layout-node flex-items
                                                        container-width container-height
                                                        direction align-items align-content)
  "Compute flex layout cross axis alignment.
LAYOUT-NODE is the flex container layout node.
FLEX-ITEMS is the list of flex items.
CONTAINER-WIDTH/HEIGHT are container dimensions.
DIRECTION is flex-direction.
ALIGN-ITEMS is cross axis alignment.
ALIGN-CONTENT is multi-line alignment."
  (let* ((is-row (or (string= direction "row")
                     (string= direction "row-reverse")))
         (cross-size (if is-row container-height container-width)))
    
    ;; Store cross axis alignment info
    (dom-set-attribute layout-node 'layout-flex-align-items align-items)
    (dom-set-attribute layout-node 'layout-flex-align-content align-content)
    (dom-set-attribute layout-node 'layout-flex-cross-size cross-size)))

(defun etaf-layout-node (render-node parent-context)
  "Recursively layout render node.
RENDER-NODE is the render node.
PARENT-CONTEXT is parent container context.
Returns layout node or nil."
  (when render-node
    (let ((display (etaf-render-get-display render-node)))
      
      (cond
       ;; Flex layout
       ((string= display "flex")
        (etaf-layout-flex-formatting-context render-node parent-context))
       
       ;; 块级元素
       ((or (string= display "block")
            (null display))
        (etaf-layout-block-formatting-context render-node parent-context))
       
       ;; 内联元素（简化处理：按块级处理）
       ((string= display "inline")
        (etaf-layout-block-formatting-context render-node parent-context))
       
       ;; 其他 display 类型：按块级处理
       (t (etaf-layout-block-formatting-context render-node parent-context))))))

;;; 布局树构建

(defun etaf-layout-build-tree (render-tree viewport)
  "从渲染树构建布局树。
RENDER-TREE 是渲染树根节点。
VIEWPORT 是视口大小 (:width w :height h)。
返回布局树根节点。"
  (let ((root-context (list :content-width (plist-get viewport :width)
                            :content-height (plist-get viewport :height))))
    (etaf-layout-node render-tree root-context)))

;;; 布局树遍历和查询

(defun etaf-layout-walk (layout-tree func)
  "遍历布局树，对每个节点调用 FUNC。
LAYOUT-TREE 是布局树根节点。
FUNC 是接受一个布局节点参数的函数。
注意：可以直接使用 etaf-dom-map 实现相同功能。"
  (etaf-dom-map func layout-tree))

;;; 布局字符串生成（用于 Emacs buffer 渲染）

(defun etaf-layout--merge-children-by-display (child-infos &optional container-width)
  "Merge child element strings by display type.
CHILD-INFOS is a list of ((string . display-type) ...).
CONTAINER-WIDTH is optional container width for wrapping inline elements.
Inline elements are concatenated horizontally using etaf-lines-concat.
Block elements are stacked vertically.
Consecutive inline elements are grouped together, then combined with block elements.
When CONTAINER-WIDTH is provided and inline elements exceed it, they will wrap."
  (if (null child-infos)
      ""
    (let ((result-parts '())  ;; Final vertically combined parts
          (inline-group '())) ;; Current accumulated inline element group
      ;; Iterate through all child elements
      (dolist (info child-infos)
        (let ((str (car info))
              (display (cdr info)))
          (when (> (length str) 0)
            (if (string= display "inline")
                ;; Inline element: add to current inline group
                (push str inline-group)
              ;; Block element: first process accumulated inline group, then add block
              (when inline-group
                ;; Use etaf-lines-concat to properly join inline elements
                ;; This treats each element as a complete unit with its borders intact
                (push (etaf-layout--merge-inline-with-wrap
                       (nreverse inline-group) container-width)
                      result-parts)
                (setq inline-group nil))
              (push str result-parts)))))
      ;; Handle remaining inline group
      (when inline-group
        ;; Use etaf-lines-concat to properly join inline elements with wrapping
        (push (etaf-layout--merge-inline-with-wrap
               (nreverse inline-group) container-width)
              result-parts))
      ;; Vertically combine all parts
      ;; Use reverse instead of nreverse to avoid destructive modification
      (string-join (reverse result-parts) "\n"))))

(defun etaf-layout--merge-inline-with-wrap (inline-strings &optional container-width)
  "Merge inline strings, wrapping to new lines when container width is exceeded.
INLINE-STRINGS is a list of inline element strings.
CONTAINER-WIDTH is optional container width for wrapping."
  (if (or (null inline-strings)
          (null container-width)
          (<= container-width 0))
      ;; No wrapping needed
      (if inline-strings
          (etaf-lines-concat inline-strings)
        "")
    ;; Calculate line breaks based on container width
    (let* ((widths (mapcar #'string-pixel-width inline-strings))
           (total-width (apply #'+ widths)))
      (if (<= total-width container-width)
          ;; All fit in one line
          (etaf-lines-concat inline-strings)
        ;; Need to wrap - calculate line breaks
        (let* ((line-breaks (etaf-flex-line-breaks container-width widths 0))
               (lines '())
               (idx 0))
          (dolist (count line-breaks)
            (let ((line-strings (seq-subseq inline-strings idx (+ idx count))))
              (push (etaf-lines-concat line-strings) lines)
              (setq idx (+ idx count))))
          ;; Stack lines vertically
          (if lines
              (etaf-lines-stack (nreverse lines))
            ""))))))

(defun etaf-layout--flex-content-justify (items-num rest-units justify-content gap)
  "Calculate main-axis gaps list following the pattern in etaf-flex.el.
Returns a list of gaps: (start-gap gap1 gap2 ... end-gap) with items-num + 1 elements.
This list will be interleaved with items: gap0 item0 gap1 item1 ... gapN.
When items-num is 0 or 1, returns appropriate list maintaining the invariant."
  (let ((rest-units (max 0 rest-units)))
    (cond
     ;; Edge case: no items
     ((<= items-num 0)
      (list rest-units))
     ;; Edge case: single item - just start and end gaps
     ((= items-num 1)
      (pcase justify-content
        ("flex-start" (list 0 rest-units))
        ("flex-end" (list rest-units 0))
        ("center" (let ((half (/ rest-units 2)))
                    (list half (- rest-units half))))
        (_ (list 0 rest-units))))  ; space-between, space-around, space-evenly default to flex-start for 1 item
     ;; Normal case: multiple items
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
         ;; Default to flex-start
         (append (list 0)
                 (make-list (1- items-num) gap)
                 (list rest-units))))))))

(defun etaf-layout--merge-flex-children (child-strings flex-direction
                                                       row-gap column-gap
                                                       justify-content
                                                       container-width
                                                       &optional container-height
                                                       flex-wrap
                                                       align-items
                                                       align-content)
  "Merge child element strings according to flex layout properties.
CHILD-STRINGS is list of child element strings.
FLEX-DIRECTION is main axis direction (row/row-reverse/column/column-reverse).
ROW-GAP/COLUMN-GAP are row/column gaps.
JUSTIFY-CONTENT is main axis alignment.
CONTAINER-WIDTH is the container's content width for row layouts.
CONTAINER-HEIGHT is the container's content height for column layouts (optional).
FLEX-WRAP is wrap mode (nowrap/wrap/wrap-reverse, default nowrap).
ALIGN-ITEMS is cross-axis alignment for items within a line.
ALIGN-CONTENT is cross-axis alignment for lines when wrapping.

This function follows the same strategy as etaf-flex.el:
1. row-reverse and column-reverse reverse items AND gaps within each line
2. wrap-reverse reverses the order of lines
3. Calculate wrap-lst using etaf-flex-line-breaks when items overflow
4. Process each line with etaf-interleave pattern for gaps and items
5. Stack/concat lines in the cross-axis direction with align-content support"
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
           ;; Filter out empty strings
           (valid-strings (seq-filter (lambda (s) (> (length s) 0))
                                      child-strings))
           (items-count (length valid-strings))
           ;; Get container main axis size
           (container-main-size (if is-row
                                    (or container-width 0)
                                  (or container-height 0)))
           ;; Get container cross axis size
           (container-cross-size (if is-row
                                     (or container-height 0)
                                   (or container-width 0)))
           ;; Calculate sizes for each item (same as items-units-lst in etaf-flex.el)
           (items-units-lst (if is-row
                                (mapcar #'string-pixel-width valid-strings)
                              (mapcar #'etaf-string-linum valid-strings)))
           (items-units (apply #'+ items-units-lst))
           ;; Calculate total gap for all items
           (gaps-units (* main-gap (max 0 (1- items-count))))
           ;; Calculate rest-units (same as in etaf-flex.el line 733)
           (rest-units (if (> container-main-size 0)
                           (- container-main-size items-units gaps-units)
                         0))
           ;; Determine wrap-lst following etaf-flex.el strategy (lines 744-763)
           ;; wrap-lst records the number of items on each line
           (_ (message "rest-units:%S" rest-units))
           (_ (message "container-main-size:%S" container-main-size))
           (_ (message "flex-wrap:%S; items-count:%S" flex-wrap items-count))
           (wrap-lst
            (if (and (not (string= flex-wrap "nowrap"))
                     (> items-count 1)
                     (> container-main-size 0)
                     (< rest-units 0))  ;; items overflow
                ;; Use etaf-flex-line-breaks to calculate line breaks
                (progn
                  (message "container-main-size:%S" container-main-size)
                  (message "items-units-lst:%S" items-units-lst)
                  (message "main-gap:%S" main-gap)
                  (etaf-flex-line-breaks container-main-size
                                         items-units-lst main-gap))
              ;; No wrapping - all items on single line
              (list items-count))))
      (message "wrap-lst:%S" wrap-lst)
      (if (null valid-strings)
          ""
        ;; Process each line following etaf-flex.el strategy (lines 766-803)
        (let ((lines-strings nil)
              (cross-max-units-lst nil)
              (prev 0))
          ;; Process each line and calculate main-gaps
          (dolist (num wrap-lst)
            (when (> num 0)
              (let* ((line-strings (seq-subseq valid-strings prev (+ prev num)))
                     (line-units-lst (seq-subseq items-units-lst prev (+ prev num)))
                     (line-items-units (apply #'+ line-units-lst))
                     (line-gaps-units (* main-gap (max 0 (1- num))))
                     (line-rest-units (if (> container-main-size 0)
                                          (max 0 (- container-main-size line-items-units line-gaps-units))
                                        0))
                     ;; Calculate full gaps list like etaf-flex-content-justify
                     ;; Returns (start gap1 gap2 ... end) with num+1 elements
                     (main-gaps-lst (etaf-layout--flex-content-justify
                                     num line-rest-units justify-content main-gap))
                     ;; Calculate cross-axis max units for align-items/align-content
                     (line-cross-max-units (if is-row
                                               (apply #'max (mapcar #'etaf-string-linum line-strings))
                                             (apply #'max (mapcar #'string-pixel-width line-strings))))
                     ;; For row-reverse and column-reverse, reverse both items AND gaps
                     ;; This follows etaf-flex.el lines 641-644 and 662-665
                     (ordered-line-strings (if is-reversed
                                               (nreverse (copy-sequence line-strings))
                                             line-strings))
                     (ordered-main-gaps (if is-reversed
                                            (nreverse (copy-sequence main-gaps-lst))
                                          main-gaps-lst))
                     ;; Render this line using interleave pattern like etaf-flex.el
                     (line-string
                      (if is-row
                          (etaf-layout--flex-concat-with-gaps
                           ordered-line-strings ordered-main-gaps
                           line-cross-max-units align-items t)
                        (etaf-layout--flex-stack-with-gaps
                         ordered-line-strings ordered-main-gaps
                         line-cross-max-units align-items))))
                (push line-cross-max-units cross-max-units-lst)
                (push line-string lines-strings)
                (setq prev (+ prev num)))))
          ;; Reverse since we used push
          (setq lines-strings (nreverse lines-strings))
          (setq cross-max-units-lst (nreverse cross-max-units-lst))
          ;; Handle wrap-reverse (same as etaf-flex.el line 942-947)
          ;; Only wrap-reverse affects line order, not row-reverse/column-reverse
          (when (string= flex-wrap "wrap-reverse")
            (setq lines-strings (nreverse lines-strings))
            (setq cross-max-units-lst (nreverse cross-max-units-lst)))
          ;; Stack/concat lines in cross axis direction with align-content support
          ;; (same as etaf-flex.el lines 955-964)
          (if (= (length lines-strings) 1)
              (car lines-strings)
            (let* ((lines-count (length lines-strings))
                   (total-cross-units (apply #'+ cross-max-units-lst))
                   (total-cross-gaps (* cross-gap (max 0 (1- lines-count))))
                   (cross-rest-units (if (> container-cross-size 0)
                                         (max 0 (- container-cross-size total-cross-units total-cross-gaps))
                                       0))
                   ;; Calculate cross-axis gaps list based on align-content
                   (cross-gaps-lst (etaf-layout--flex-content-justify
                                    lines-count cross-rest-units align-content cross-gap)))
              (if is-row
                  ;; Row layout: stack lines vertically with cross-gaps
                  (etaf-layout--flex-stack-lines-with-gaps
                   lines-strings cross-gaps-lst)
                ;; Column layout: concat lines horizontally with cross-gaps
                (etaf-layout--flex-concat-lines-with-gaps
                 lines-strings cross-gaps-lst)))))))))

(defun etaf-layout--flex-stack-lines (line-strings cross-gap)
  "Stack multiple lines vertically with cross-gap between them.
LINE-STRINGS is list of line strings.
CROSS-GAP is the gap between lines (in lines for row layout)."
  (if (null line-strings)
      ""
    (let ((parts '())
          (len (length line-strings))
          (idx 0))
      (dolist (line-str line-strings)
        (push line-str parts)
        (when (and (< idx (1- len))
                   (> cross-gap 0))
          ;; Add gap lines between lines
          (let ((width (string-pixel-width line-str)))
            (when (> width 0)
              (push (etaf-pixel-blank width cross-gap) parts))))
        (setq idx (1+ idx)))
      (etaf-lines-stack (nreverse parts)))))

(defun etaf-layout--flex-concat-lines (line-strings cross-gap)
  "Concatenate multiple lines horizontally with cross-gap between them.
LINE-STRINGS is list of line strings.
CROSS-GAP is the gap between lines (in pixels for column layout)."
  (if (null line-strings)
      ""
    (let ((parts '())
          (len (length line-strings))
          (idx 0))
      (dolist (line-str line-strings)
        (push line-str parts)
        (when (and (< idx (1- len))
                   (> cross-gap 0))
          ;; Add gap spacing between lines
          (push (etaf-pixel-spacing cross-gap) parts))
        (setq idx (1+ idx)))
      (etaf-lines-concat (nreverse parts)))))

(defun etaf-layout--flex-concat-horizontal (strings start-space between-space end-space)
  "Horizontally concatenate flex child element strings.
STRINGS is list of child element strings.
START-SPACE is starting space (pixels).
BETWEEN-SPACE is space between elements (pixels).
END-SPACE is ending space (pixels)."
  (let* ((count (length strings))
         (parts '()))
    ;; 添加起始空间
    (when (and start-space (> start-space 0))
      (push (etaf-pixel-spacing start-space) parts))
    ;; 添加子元素和间隙
    (dotimes (i count)
      (push (nth i strings) parts)
      (when (< i (1- count))
        (when (> between-space 0)
          (push (etaf-pixel-spacing between-space) parts))))
    ;; 添加结束空间
    (when (and end-space (> end-space 0))
      (push (etaf-pixel-spacing end-space) parts))
    ;; 水平拼接所有部分
    (if parts
        (etaf-lines-concat (nreverse parts))
      "")))

(defun etaf-layout--flex-stack-vertical (strings start-space between-space end-space)
  "Vertically stack flex child element strings.
STRINGS is list of child element strings.
START-SPACE is starting space (lines).
BETWEEN-SPACE is space between elements (lines).
END-SPACE is ending space (lines)."
  (let* ((count (length strings))
         (parts '())
         ;; Get max width to create consistent blank lines
         (max-width (if strings
                        (apply #'max (mapcar #'string-pixel-width strings))
                      0)))
    ;; Add starting space
    (when (and start-space (> start-space 0) (> max-width 0))
      (push (etaf-pixel-blank max-width start-space) parts))
    ;; Add child elements and gaps
    (dotimes (i count)
      (push (nth i strings) parts)
      (when (and (< i (1- count)) (> between-space 0) (> max-width 0))
        (push (etaf-pixel-blank max-width between-space) parts)))
    ;; Add ending space
    (when (and end-space (> end-space 0) (> max-width 0))
      (push (etaf-pixel-blank max-width end-space) parts))
    ;; Vertically stack all parts
    (if parts
        (etaf-lines-stack (nreverse parts))
      "")))

(defun etaf-layout--flex-concat-horizontal-with-align (strings start-space between-space end-space
                                                                cross-max-units align-items)
  "Horizontally concatenate flex child element strings with cross-axis alignment.
STRINGS is list of child element strings.
START-SPACE is starting space (pixels).
BETWEEN-SPACE is space between elements (pixels).
END-SPACE is ending space (pixels).
CROSS-MAX-UNITS is the maximum height in lines for this row.
ALIGN-ITEMS is cross-axis alignment (flex-start, flex-end, center, stretch, baseline)."
  (let* ((count (length strings))
         (parts '())
         ;; Apply cross-axis alignment to each item
         (aligned-strings
          (mapcar (lambda (str)
                    (etaf-layout--align-item-cross-axis
                     str cross-max-units align-items t))
                  strings)))
    ;; Add starting space
    (when (and start-space (> start-space 0))
      (push (etaf-pixel-spacing start-space) parts))
    ;; Add child elements and gaps
    (dotimes (i count)
      (push (nth i aligned-strings) parts)
      (when (< i (1- count))
        (when (> between-space 0)
          (push (etaf-pixel-spacing between-space) parts))))
    ;; Add ending space
    (when (and end-space (> end-space 0))
      (push (etaf-pixel-spacing end-space) parts))
    ;; Horizontally concat all parts
    (if parts
        (etaf-lines-concat (nreverse parts))
      "")))

(defun etaf-layout--flex-stack-vertical-with-align (strings start-space between-space end-space
                                                             cross-max-units align-items)
  "Vertically stack flex child element strings with cross-axis alignment.
STRINGS is list of child element strings.
START-SPACE is starting space (lines).
BETWEEN-SPACE is space between elements (lines).
END-SPACE is ending space (lines).
CROSS-MAX-UNITS is the maximum width in pixels for this column.
ALIGN-ITEMS is cross-axis alignment (flex-start, flex-end, center, stretch, baseline)."
  (let* ((count (length strings))
         (parts '())
         ;; Apply cross-axis alignment to each item
         (aligned-strings
          (mapcar (lambda (str)
                    (etaf-layout--align-item-cross-axis
                     str cross-max-units align-items nil))
                  strings))
         ;; Get max width to create consistent blank lines
         (max-width (if aligned-strings
                        (apply #'max (mapcar #'string-pixel-width aligned-strings))
                      0)))
    ;; Add starting space
    (when (and start-space (> start-space 0) (> max-width 0))
      (push (etaf-pixel-blank max-width start-space) parts))
    ;; Add child elements and gaps
    (dotimes (i count)
      (push (nth i aligned-strings) parts)
      (when (and (< i (1- count)) (> between-space 0) (> max-width 0))
        (push (etaf-pixel-blank max-width between-space) parts)))
    ;; Add ending space
    (when (and end-space (> end-space 0) (> max-width 0))
      (push (etaf-pixel-blank max-width end-space) parts))
    ;; Vertically stack all parts
    (if parts
        (etaf-lines-stack (nreverse parts))
      "")))

(defun etaf-layout--align-item-cross-axis (string cross-max-units align-items is-row)
  "Align a single item in the cross-axis direction.
STRING is the item string.
CROSS-MAX-UNITS is the maximum size in the cross-axis direction.
ALIGN-ITEMS is the alignment method.
IS-ROW is t for row direction (cross-axis is vertical), nil for column."
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
           ;; Align to start of cross axis
           (if is-row
               ;; Row: add padding at bottom
               (etaf-lines-stack
                (list string
                      (etaf-pixel-blank (string-pixel-width string) rest-units)))
             ;; Column: add padding at right
             (etaf-lines-concat
              (list string
                    (etaf-pixel-blank rest-units (etaf-string-linum string))))))
          ("flex-end"
           ;; Align to end of cross axis
           (if is-row
               ;; Row: add padding at top
               (etaf-lines-stack
                (list (etaf-pixel-blank (string-pixel-width string) rest-units)
                      string))
             ;; Column: add padding at left
             (etaf-lines-concat
              (list (etaf-pixel-blank rest-units (etaf-string-linum string))
                    string))))
          ("center"
           ;; Center in cross axis
           (let ((start-units (/ rest-units 2))
                 (end-units (- rest-units (/ rest-units 2))))
             (if is-row
                 ;; Row: add padding at top and bottom
                 (etaf-lines-stack
                  (list (when (> start-units 0)
                          (etaf-pixel-blank (string-pixel-width string) start-units))
                        string
                        (when (> end-units 0)
                          (etaf-pixel-blank (string-pixel-width string) end-units))))
               ;; Column: add padding at left and right
               (etaf-lines-concat
                (list (when (> start-units 0)
                        (etaf-pixel-blank start-units (etaf-string-linum string)))
                      string
                      (when (> end-units 0)
                        (etaf-pixel-blank end-units (etaf-string-linum string))))))))
          ((or "stretch" "baseline" _)
           ;; Stretch: fill cross axis (similar to flex-start but with stretch)
           ;; Baseline: simplified to flex-start behavior
           (if is-row
               (etaf-lines-stack
                (list string
                      (when (> rest-units 0)
                        (etaf-pixel-blank (string-pixel-width string) rest-units))))
             (etaf-lines-concat
              (list string
                    (when (> rest-units 0)
                      (etaf-pixel-blank rest-units (etaf-string-linum string))))))))))))

(defun etaf-layout--flex-concat-with-gaps (strings gaps-lst cross-max-units align-items is-row)
  "Horizontally concatenate flex items with gaps using interleave pattern.
STRINGS is list of item strings.
GAPS-LST is list of gaps (start gap1 gap2 ... end) with (length strings) + 1 elements.
CROSS-MAX-UNITS is the maximum height in lines for this row.
ALIGN-ITEMS is cross-axis alignment.
IS-ROW should be t for horizontal layout.
This follows etaf-flex.el etaf-flex-items-concat-single using etaf-interleave."
  (if (null strings)
      ""
    (let* (;; Apply cross-axis alignment to each item
           (aligned-strings
            (mapcar (lambda (str)
                      (etaf-layout--align-item-cross-axis
                       str cross-max-units align-items is-row))
                    strings))
           ;; Create gap spacings
           (gap-spacings (mapcar (lambda (gap)
                                   (if (and gap (> gap 0))
                                       (etaf-pixel-spacing gap)
                                     ""))
                                 gaps-lst)))
      ;; Interleave gaps and items: gap0 item0 gap1 item1 ... gapN
      (etaf-lines-concat
       (etaf-interleave gap-spacings aligned-strings)))))

(defun etaf-layout--flex-stack-with-gaps (strings gaps-lst cross-max-units align-items)
  "Vertically stack flex items with gaps using interleave pattern.
STRINGS is list of item strings.
GAPS-LST is list of gaps (start gap1 gap2 ... end) with (length strings) + 1 elements.
CROSS-MAX-UNITS is the maximum width in pixels for this column.
ALIGN-ITEMS is cross-axis alignment.
This follows etaf-flex.el etaf-flex-items-stack-single using etaf-interleave."
  (if (null strings)
      ""
    (let* (;; Apply cross-axis alignment to each item
           (aligned-strings
            (mapcar (lambda (str)
                      (etaf-layout--align-item-cross-axis
                       str cross-max-units align-items nil))
                    strings))
           ;; Get max width for blank lines
           (max-width (if aligned-strings
                          (apply #'max (mapcar #'string-pixel-width aligned-strings))
                        0))
           ;; Create gap blank lines
           (gap-blanks (mapcar (lambda (gap)
                                 (if (and gap (> gap 0) (> max-width 0))
                                     (etaf-string-duplines "" gap)
                                   nil))
                               gaps-lst)))
      ;; Interleave gaps and items: gap0 item0 gap1 item1 ... gapN
      (etaf-lines-stack
       (etaf-interleave gap-blanks aligned-strings)))))

(defun etaf-layout--flex-stack-lines-with-gaps (line-strings cross-gaps-lst)
  "Stack multiple lines vertically with cross-gaps using interleave pattern.
LINE-STRINGS is list of line strings.
CROSS-GAPS-LST is list of cross-axis gaps (start gap1 gap2 ... end)."
  (if (null line-strings)
      ""
    (let* (;; Get max width across all lines
           (max-width (apply #'max (mapcar #'string-pixel-width line-strings)))
           ;; Create gap blank lines
           (gap-blanks (mapcar (lambda (gap)
                                 (if (and gap (> gap 0) (> max-width 0))
                                     (etaf-string-duplines "" (floor gap))
                                   nil))
                               cross-gaps-lst)))
      ;; Interleave gaps and lines
      (etaf-lines-stack
       (etaf-interleave gap-blanks line-strings)))))

(defun etaf-layout--flex-concat-lines-with-gaps (line-strings cross-gaps-lst)
  "Concatenate multiple lines horizontally with cross-gaps using interleave pattern.
LINE-STRINGS is list of line strings.
CROSS-GAPS-LST is list of cross-axis gaps (start gap1 gap2 ... end)."
  (if (null line-strings)
      ""
    (let* (;; Create gap spacings
           (gap-spacings (mapcar (lambda (gap)
                                   (if (and gap (> gap 0))
                                       (etaf-pixel-spacing (floor gap))
                                     ""))
                                 cross-gaps-lst)))
      ;; Interleave gaps and lines
      (etaf-lines-concat
       (etaf-interleave gap-spacings line-strings)))))

(defun etaf-layout-node-string (layout-node)
  "将布局节点转换为可插入 buffer 的字符串。
LAYOUT-NODE 是布局节点。
返回拼接好的字符串，包含 margin、border、padding 和内容。

这个函数使用后序遍历（post-order traversal）从叶子节点开始构建整个结构的文本。
在 Emacs 中，高度使用行数（lines）而不是像素值。
CSS 文本样式（如 color、font-weight）会转换为 Emacs face 属性应用到文本上。"
  (let* ((box-model (or (etaf-layout-get-box-model layout-node)
                        (etaf-box-model-create)))  ;; 使用默认盒模型避免 nil
         ;; 获取计算样式（用于应用 face 属性）
         (computed-style (dom-attr layout-node 'render-style))
         ;; 使用辅助函数获取内容宽高，确保一致性
         (content-width (or (etaf-box-model-content-width box-model) 0))
         (content-height-px (or (etaf-box-model-content-height box-model) 0))
         
         ;; 获取盒模型各部分
         (padding (or (plist-get box-model :padding)
                      '(:top 0 :right 0 :bottom 0 :left 0)))
         (border (or (plist-get box-model :border)
                     '(:top-width 0 :right-width 0 :bottom-width 0 :left-width 0)))
         (margin (or (plist-get box-model :margin)
                     '(:top 0 :right 0 :bottom 0 :left 0)))
         
         ;; 提取各边的值（padding 和 margin 的垂直方向使用行数）
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
         
         ;; 后序遍历：先递归处理所有子元素
         ;; 对于每个子元素，我们需要知道它的 display 类型和内容字符串
         (children (dom-children layout-node))
         ;; 检查当前节点是否是 flex 容器
         (is-flex-container (dom-attr layout-node 'layout-flex-direction))
         (child-infos
          (mapcar (lambda (child)
                    (cond
                     ;; 元素节点：递归调用，并获取 display 类型
                     ;; 如果没有 render-display 属性，根据标签类型使用默认值
                     ((listp child)
                      (cons (etaf-layout-node-string child)
                            (or (dom-attr child 'render-display)
                                (etaf-render-get-default-display (dom-tag child)))))
                     ;; 文本节点：视为 inline
                     ((stringp child)
                      (cons child "inline"))
                     (t (cons "" "inline"))))
                  children))
         ;; 根据 display 类型合并子节点
         ;; flex 容器使用 flex 特定的合并逻辑
         (children-text
          (if is-flex-container
              ;; Flex 容器：使用 flex 布局合并
              (let ((flex-direction (dom-attr layout-node 'layout-flex-direction))
                    (row-gap (or (dom-attr layout-node 'layout-row-gap) 0))
                    (column-gap (or (dom-attr layout-node 'layout-column-gap) 0))
                    (justify-content (dom-attr layout-node 'layout-justify-content))
                    (flex-wrap (or (dom-attr layout-node 'layout-flex-wrap) "nowrap"))
                    (align-items (or (dom-attr layout-node 'layout-align-items) "stretch"))
                    (align-content (or (dom-attr layout-node 'layout-align-content) "stretch"))
                    (child-strings (mapcar #'car child-infos)))
                (etaf-layout--merge-flex-children
                 child-strings flex-direction row-gap column-gap
                 justify-content content-width content-height-px flex-wrap
                 align-items align-content))
            ;; 非 flex 容器：使用原有的 display 类型合并逻辑
            ;; - inline 元素应该水平拼接
            ;; - block 元素应该垂直堆叠
            ;; 策略：将连续的 inline 元素组合在一起水平拼接，然后与 block 元素垂直堆叠
            ;; 传递 content-width 用于 inline 元素换行判断
            (etaf-layout--merge-children-by-display child-infos content-width)))
         
         ;; 内容：如果有子元素则使用子元素的字符串，否则为空
         (inner-content children-text)
         
         ;; 计算内容高度（行数）
         ;; 如果 CSS 明确设置了 height（content-height-px > 0），则使用 CSS 指定的高度
         ;; 否则根据实际内容行数计算
         (natural-content-height (if (> (length inner-content) 0)
                                     (etaf-string-linum inner-content)
                                   0))
         (content-height (if (> content-height-px 0)
                             content-height-px
                           (max natural-content-height 0)))
         
         ;; 如果有实际内容但宽度为 0，使用内容的最大行像素宽度作为默认宽度
         (effective-width (if (and (> (length inner-content) 0) (<= content-width 0))
                              (let ((lines (split-string inner-content "\n")))
                                (if lines
                                    (apply #'max (mapcar #'string-pixel-width lines))
                                  (string-pixel-width inner-content)))
                            content-width)))
    
    ;; 如果内容宽度和高度都为 0 且没有实际内容，返回空字符串
    (if (and (<= effective-width 0) (<= content-height 0))
        ""
      ;; 构建最终内容
      (let* (;; 1. 确保内容符合指定的宽度（使用 etaf-lines-justify）
             (sized-content (if (> (length inner-content) 0)
                                (condition-case nil
                                    (etaf-lines-justify inner-content effective-width)
                                  (error inner-content))
                              ;; 如果没有内容，创建空白内容
                              (etaf-pixel-blank effective-width content-height)))
             
             ;; 1.5 应用 CSS 文本样式到内容（不包括 background-color，因为背景需要覆盖 padding 区域）
             ;; 使用 cl-remove-if 过滤掉 background-color
             (text-style (when computed-style
                           (cl-remove-if (lambda (pair)
                                           (eq (car pair) 'background-color))
                                         computed-style)))
             (styled-content (if (and text-style (> (length sized-content) 0))
                                 (etaf-css-apply-face-to-string sized-content text-style)
                               sized-content))
             
             ;; 计算实际渲染高度（行数）
             ;; 如果 CSS 明确设置了 height，使用 CSS 高度来约束内容
             ;; 否则使用实际内容行数
             (styled-content-height (if (> (length styled-content) 0)
                                        (etaf-string-linum styled-content)
                                      0))
             (actual-content-height (if (> content-height-px 0)
                                        content-height-px
                                      (max styled-content-height content-height)))
             
             ;; 当 CSS 指定了高度时，使用 etaf-lines-align 来约束内容到指定高度
             (height-constrained-content
              (if (and (> content-height-px 0)
                       (> (length styled-content) 0)
                       (/= styled-content-height content-height-px))
                  (etaf-lines-align styled-content content-height-px 'top)
                styled-content))
             
             ;; 计算 border 以内的高度（行数）- 使用指定高度
             (inner-height (+ actual-content-height padding-top padding-bottom))
             
             ;; 2. 添加 padding（垂直方向）
             (with-padding (if (and (> effective-width 0)
                                    (or (> padding-top 0) (> padding-bottom 0)))
                               (etaf-lines-stack
                                (list (when (> padding-top 0)
                                        (etaf-pixel-blank effective-width padding-top))
                                      height-constrained-content
                                      (when (> padding-bottom 0)
                                        (etaf-pixel-blank effective-width padding-bottom))))
                             height-constrained-content))
             
             ;; 3. 添加 padding（水平方向）
             (with-h-padding (if (and (> inner-height 0)
                                      (or (> padding-left 0) (> padding-right 0)))
                                 (etaf-lines-concat
                                  (list (when (> padding-left 0)
                                          (etaf-pixel-blank padding-left inner-height))
                                        with-padding
                                        (when (> padding-right 0)
                                          (etaf-pixel-blank padding-right inner-height))))
                               with-padding))
             
             ;; 3.5 应用 background-color 到 content + padding 区域
             ;; 背景色应该覆盖 content 和 padding 区域，不包括 border 和 margin
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
             
             ;; 4. 添加 border（水平方向）- 左右边框
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
             
             ;; 计算添加 margin 前的总像素宽度（包含左右边框）
             (total-pixel (+ effective-width
                             padding-left padding-right
                             border-left border-right))
             
             ;; 4.5 添加 border（垂直方向）- 上下边框
             ;; 使用 overline 和 underline 属性实现上下边框（参考 etaf-box.el）
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
             
             ;; 5. 添加 margin（水平方向）
             ;; 注意：上下边框使用 overline/underline，不增加额外高度
             (with-h-margin (if (and (> inner-height 0)
                                     (or (> margin-left 0) (> margin-right 0)))
                                (etaf-lines-concat
                                 (list (when (> margin-left 0)
                                         (etaf-pixel-blank margin-left inner-height))
                                       with-v-border
                                       (when (> margin-right 0)
                                         (etaf-pixel-blank margin-right inner-height))))
                              with-v-border))
             
             ;; 更新总像素宽度（包含 margin）
             (total-width (+ total-pixel margin-left margin-right))
             
             ;; 6. 添加 margin（垂直方向，使用行数）
             (final-string (if (and (> total-width 0)
                                    (or (> margin-top 0) (> margin-bottom 0)))
                               (etaf-lines-stack
                                (list (when (> margin-top 0)
                                        (etaf-pixel-blank total-width margin-top))
                                      with-h-margin
                                      (when (> margin-bottom 0)
                                        (etaf-pixel-blank total-width margin-bottom))))
                             with-h-margin)))
        
        final-string))))

(defun etaf-layout-to-string (layout-tree)
  "将布局树转换为可插入 Emacs buffer 的字符串。
LAYOUT-TREE 是布局树根节点。
返回拼接好的布局字符串，可以直接插入到 buffer 中显示。

这种方式通过文本拼接来生成最终的布局，而不是使用精确的 x,y 坐标，
更适合在 Emacs buffer 中进行渲染。"
  (etaf-layout-node-string layout-tree))

(provide 'etaf-layout)
;;; etaf-layout.el ends here
