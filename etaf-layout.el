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
;; - 位置（x, y 坐标）
;; - 尺寸（width, height）
;;
;; 布局树使用 DOM 格式表示，保留渲染树的 DOM 结构，附加的布局信息用属性表示：
;; - layout-box-model: 盒模型信息
;; - layout-position: 位置信息
;; - layout-content-box: 内容区域信息
;;
;; 布局树结构：
;; (tag ((layout-box-model . <box-model>)
;;       (layout-position . (:x <n> :y <n>))
;;       (layout-content-box . (:x <n> :y <n> :width <n> :height <n>))
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
;; 使用示例：
;;
;;   ;; 构建布局树
;;   (setq layout-tree (etaf-layout-build-tree render-tree viewport))
;;
;;   ;; 遍历布局树 - 可以直接使用 etaf-dom-map
;;   (etaf-dom-map
;;     (lambda (node)
;;       (let ((pos (etaf-layout-get-position node))
;;             (box (etaf-layout-get-box-model node)))
;;         (message "Position: (%d,%d) Size: %dx%d"
;;                  (plist-get pos :x)
;;                  (plist-get pos :y)
;;                  (plist-get (plist-get box :content) :width)
;;                  (plist-get (plist-get box :content) :height))))
;;     layout-tree)

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-render)
(require 'etaf-utils)

;;; 辅助函数：CSS 值解析

(defun etaf-layout-parse-length (value reference-width)
  "解析 CSS 长度值。
VALUE 是 CSS 值字符串。
REFERENCE-WIDTH 是参考宽度（用于百分比计算）。
返回像素值或 'auto。"
  (cond
   ((null value) 'auto)
   ((eq value 'auto) 'auto)
   ((numberp value) value)
   ((string= value "auto") 'auto)
   ((string= value "0") 0)
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-width))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    ;; 简化：假设 1em = 16px
    (* (string-to-number (match-string 1 value)) 16))
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
                     :top-color "black" :right-color "black" 
                     :bottom-color "black" :left-color "black")
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

(defun etaf-layout-get-position (layout-node)
  "从布局节点获取位置。
LAYOUT-NODE 是布局节点。
返回位置 plist (:x <n> :y <n>)。"
  (dom-attr layout-node 'layout-position))

(defun etaf-layout-get-content-box (layout-node)
  "从布局节点获取内容区域。
LAYOUT-NODE 是布局节点。
返回内容区域 plist (:x <n> :y <n> :width <n> :height <n>)。"
  (dom-attr layout-node 'layout-content-box))

(defun etaf-layout-create-node (render-node box-model position content-box)
  "创建布局节点（使用 DOM 格式）。
RENDER-NODE 是渲染节点。
BOX-MODEL 是盒模型 plist。
POSITION 是位置 plist。
CONTENT-BOX 是内容区域 plist。
返回 DOM 格式的布局节点，保留渲染节点的结构并添加布局信息。"
  (let* ((tag (dom-tag render-node))
         (render-attrs (dom-attributes render-node))
         ;; 构建布局属性
         (layout-attrs (list (cons 'layout-box-model box-model)
                             (cons 'layout-position position)
                             (cons 'layout-content-box content-box))))
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
         (box-sizing (etaf-layout-get-style-value style 'box-sizing "content-box"))
         
         ;; Padding
         (padding-top (etaf-layout-parse-length 
                      (etaf-layout-get-style-value style 'padding-top "0") 
                      parent-width))
         (padding-right (etaf-layout-parse-length 
                        (etaf-layout-get-style-value style 'padding-right "0") 
                        parent-width))
         (padding-bottom (etaf-layout-parse-length 
                         (etaf-layout-get-style-value style 'padding-bottom "0") 
                         parent-width))
         (padding-left (etaf-layout-parse-length 
                       (etaf-layout-get-style-value style 'padding-left "0") 
                       parent-width))
         
         ;; Border
         (border-top (etaf-layout-parse-length 
                     (etaf-layout-get-style-value style 'border-top-width "0") 
                     parent-width))
         (border-right (etaf-layout-parse-length 
                       (etaf-layout-get-style-value style 'border-right-width "0") 
                       parent-width))
         (border-bottom (etaf-layout-parse-length 
                        (etaf-layout-get-style-value style 'border-bottom-width "0") 
                        parent-width))
         (border-left (etaf-layout-parse-length 
                      (etaf-layout-get-style-value style 'border-left-width "0") 
                      parent-width))
         
         (border-top-color (etaf-layout-get-style-value style 'border-top-color "black"))
         (border-right-color (etaf-layout-get-style-value style 'border-right-color "black"))
         (border-bottom-color (etaf-layout-get-style-value style 'border-bottom-color "black"))
         (border-left-color (etaf-layout-get-style-value style 'border-left-color "black"))
         
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
         (width-value (etaf-layout-parse-length 
                      (etaf-layout-get-style-value style 'width "auto") 
                      parent-width))
         (height-value (etaf-layout-parse-length 
                       (etaf-layout-get-style-value style 'height "auto") 
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
         
         ;; 计算内容宽度
         (content-width (if (eq width-value 'auto)
                           (max 0 (- parent-width
                                    padding-left-val padding-right-val
                                    border-left-val border-right-val
                                    margin-left-val margin-right-val))
                         width-value))
         
         ;; 计算内容高度（如果指定）
         (content-height (if (eq height-value 'auto)
                            0  ; 将在后续根据子元素计算
                          height-value)))
    
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
         
         ;; 计算位置
         (margin (plist-get box-model :margin))
         (border (plist-get box-model :border))
         (padding (plist-get box-model :padding))
         
         (x (+ (plist-get parent-context :x-offset)
               (plist-get margin :left)))
         (y (+ (plist-get parent-context :current-y)
               (plist-get margin :top)))
         
         ;; 计算内容区域位置
         (content-x (+ x (plist-get border :left-width) (plist-get padding :left)))
         (content-y (+ y (plist-get border :top-width) (plist-get padding :top)))
         (content-width (etaf-box-model-content-width box-model))
         (content-height (etaf-box-model-content-height box-model))
         
         ;; 创建布局节点
         (layout-node (etaf-layout-create-node 
                       render-node
                       box-model
                       (list :x x :y y)
                       (list :x content-x 
                             :y content-y
                             :width content-width
                             :height content-height))))
    
    ;; 递归布局子元素并保留文本节点
    ;; 注意：与之前版本不同，现在保留文本节点以便在渲染时显示文本内容
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                  :content-height content-height
                                  :current-y content-y
                                  :x-offset content-x))
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
                  (setq accumulated-height (+ accumulated-height child-total-height))
                  ;; 更新下一个子元素的 Y 坐标
                  (plist-put child-context :current-y 
                            (+ content-y accumulated-height)))))
             ;; 文本节点：直接保留（不需要布局计算，仅传递给父节点）
             ((stringp child)
              (push child child-layouts))))
          
          ;; 将子节点添加到布局节点（DOM 格式）
          (setcdr (cdr layout-node) (nreverse child-layouts))
          
          ;; 如果高度为 auto，根据子元素设置高度
          (when (= content-height 0)
            (let ((box (etaf-layout-get-box-model layout-node))
                  (cbox (etaf-layout-get-content-box layout-node)))
              (plist-put (plist-get box :content) :height accumulated-height)
              (plist-put cbox :height accumulated-height))))))
    
    layout-node))

(defun etaf-layout-node (render-node parent-context)
  "递归布局渲染节点。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 是父容器上下文。
返回布局节点或 nil。"
  (when render-node
    (let* ((display (etaf-render-get-display render-node))
           (style (etaf-render-get-computed-style render-node))
           (position (etaf-layout-get-style-value style 'position "static")))
      
      (cond
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
                           :content-height (plist-get viewport :height)
                           :current-y 0
                           :x-offset 0)))
    (etaf-layout-node render-tree root-context)))

;;; 布局树遍历和查询

(defun etaf-layout-walk (layout-tree func)
  "遍历布局树，对每个节点调用 FUNC。
LAYOUT-TREE 是布局树根节点。
FUNC 是接受一个布局节点参数的函数。
注意：可以直接使用 etaf-dom-map 实现相同功能。"
  (etaf-dom-map func layout-tree))

;;; 布局字符串生成（用于 Emacs buffer 渲染）

(defun etaf-layout--merge-children-by-display (child-infos)
  "根据子元素的 display 类型合并字符串。
CHILD-INFOS 是 ((string . display-type) ...) 格式的列表。
inline 元素水平拼接，block 元素垂直堆叠。
连续的 inline 元素会被组合在一起，然后与 block 元素垂直组合。"
  (if (null child-infos)
      ""
    (let ((result-parts '())  ;; 最终垂直组合的部分
          (inline-group '())) ;; 当前累积的 inline 元素组
      ;; 遍历所有子元素
      (dolist (info child-infos)
        (let ((str (car info))
              (display (cdr info)))
          (when (> (length str) 0)
            (if (string= display "inline")
                ;; inline 元素：加入当前 inline 组
                (push str inline-group)
              ;; block 元素：先处理之前累积的 inline 组，再添加当前 block
              (when inline-group
                (push (string-join (nreverse inline-group) "") result-parts)
                (setq inline-group nil))
              (push str result-parts)))))
      ;; 处理最后剩余的 inline 组
      (when inline-group
        (push (string-join (nreverse inline-group) "") result-parts))
      ;; 垂直组合所有部分
      (string-join (nreverse result-parts) "\n"))))

(defun etaf-layout-node-string (layout-node)
  "将布局节点转换为可插入 buffer 的字符串。
LAYOUT-NODE 是布局节点。
返回拼接好的字符串，包含 margin、border、padding 和内容。

这个函数使用后序遍历（post-order traversal）从叶子节点开始构建整个结构的文本。
在 Emacs 中，高度使用行数（lines）而不是像素值。"
  (let* ((box-model (etaf-layout-get-box-model layout-node))
         (content-box (etaf-layout-get-content-box layout-node))
         (content-width (or (plist-get content-box :width) 0))
         (content-height-px (or (plist-get content-box :height) 0))
         
         ;; 获取盒模型各部分
         (padding (plist-get box-model :padding))
         (border (plist-get box-model :border))
         (margin (plist-get box-model :margin))
         
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
         (child-infos (mapcar (lambda (child)
                                (cond
                                 ;; 元素节点：递归调用，并获取 display 类型
                                 ;; 如果没有 render-display 属性，默认为 "inline"
                                 ((listp child)
                                  (cons (etaf-layout-node-string child)
                                        (or (dom-attr child 'render-display) "inline")))
                                 ;; 文本节点：视为 inline
                                 ((stringp child)
                                  (cons child "inline"))
                                 (t (cons "" "inline"))))
                              children))
         ;; 根据 display 类型合并子节点：
         ;; - inline 元素应该水平拼接
         ;; - block 元素应该垂直堆叠
         ;; 策略：将连续的 inline 元素组合在一起水平拼接，然后与 block 元素垂直堆叠
         (children-text (etaf-layout--merge-children-by-display child-infos))
         
         ;; 内容：如果有子元素则使用子元素的字符串，否则为空
         (inner-content children-text)
         
         ;; 计算内容高度（行数）
         (content-height (if (> (length inner-content) 0)
                             (etaf-string-linum inner-content)
                           (if (> content-height-px 0) 1 0))))
    
    ;; 如果内容宽度或高度为 0，返回空字符串
    (if (or (<= content-width 0) (<= content-height 0))
        ""
      ;; 构建最终内容
      (let* (;; 1. 确保内容符合指定的宽度（使用 etaf-lines-justify）
             (sized-content (if (> (length inner-content) 0)
                                (condition-case nil
                                    (etaf-lines-justify inner-content content-width)
                                  (error inner-content))
                              ;; 如果没有内容，创建空白内容
                              (etaf-pixel-blank content-width content-height)))
             
             ;; 计算 border 以内的高度（行数）
             (inner-height (+ content-height padding-top padding-bottom))
             
             ;; 2. 添加 padding（垂直方向）
             (with-padding (if (and (> content-width 0)
                                    (or (> padding-top 0) (> padding-bottom 0)))
                               (etaf-lines-stack
                                (list (when (> padding-top 0)
                                        (etaf-pixel-blank content-width padding-top))
                                      sized-content
                                      (when (> padding-bottom 0)
                                        (etaf-pixel-blank content-width padding-bottom))))
                             sized-content))
             
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
             
             ;; 4. 添加 border（水平方向）- 左右边框
             (with-border (if (and (> inner-height 0)
                                   (or (> border-left 0) (> border-right 0)))
                              (etaf-lines-concat
                               (list (when (> border-left 0)
                                       (etaf-pixel-border border-left inner-height
                                                          border-left-color))
                                     with-h-padding
                                     (when (> border-right 0)
                                       (etaf-pixel-border border-right inner-height
                                                          border-right-color))))
                            with-h-padding))
             
             ;; 计算添加 margin 前的总像素宽度（包含左右边框）
             (total-pixel (+ content-width
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
