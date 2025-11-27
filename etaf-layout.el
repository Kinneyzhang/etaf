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
         
         ;; 获取 display 类型用于宽度计算
         (display (etaf-render-get-display render-node))
         (is-inline (string= display "inline"))
         
         ;; 计算内容宽度
         ;; 对于内联元素，width:auto 时宽度应该为 0，让后续根据实际内容计算
         ;; 对于块级元素，width:auto 时宽度应该填充父容器
         (content-width (if (eq width-value 'auto)
                            (if is-inline
                                0  ; 内联元素：宽度由内容决定，在渲染时计算
                              (max 0 (- parent-width
                                        padding-left-val padding-right-val
                                        border-left-val border-right-val
                                        margin-left-val margin-right-val)))
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
            (when is-reversed
              (setq sorted-children (nreverse sorted-children)))
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
         (total-flex-shrink 0))
    
    ;; 计算总的 flex-basis、flex-grow 和 flex-shrink
    (dolist (item flex-items)
      (let* ((layout (plist-get item :layout))
             (box-model (etaf-layout-get-box-model layout))
             (item-main-size (if is-row
                                 (etaf-box-model-total-width box-model)
                               (etaf-box-model-total-height box-model))))
        (setq total-flex-basis (+ total-flex-basis item-main-size))
        (setq total-flex-grow (+ total-flex-grow (plist-get item :flex-grow)))
        (setq total-flex-shrink (+ total-flex-shrink (plist-get item :flex-shrink)))))
    
    ;; 计算总 gap
    (let* ((total-gap (* main-gap (max 0 (1- items-count))))
           (available-space (- main-size total-flex-basis total-gap))
           (free-space (max 0 available-space)))
      
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

(defun etaf-layout--merge-children-by-display (child-infos)
  "Merge child element strings by display type.
CHILD-INFOS is a list of ((string . display-type) ...).
Inline elements are concatenated horizontally using etaf-lines-concat.
Block elements are stacked vertically.
Consecutive inline elements are grouped together, then combined with block elements."
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
                (push (etaf-lines-concat
                       (nreverse inline-group))
                      result-parts)
                (setq inline-group nil))
              (push str result-parts)))))
      ;; Handle remaining inline group
      (when inline-group
        ;; Use etaf-lines-concat to properly join inline elements
        (push (etaf-lines-concat (nreverse inline-group)) result-parts))
      ;; Vertically combine all parts
      ;; Use reverse instead of nreverse to avoid destructive modification
      (string-join (reverse result-parts) "\n"))))

(defun etaf-layout--merge-flex-children (child-strings flex-direction
                                                       row-gap column-gap
                                                       justify-content
                                                       container-width
                                                       &optional container-height)
  "Merge child element strings according to flex layout properties.
CHILD-STRINGS is list of child element strings.
FLEX-DIRECTION is main axis direction (row/row-reverse/column/column-reverse).
ROW-GAP/COLUMN-GAP are row/column gaps.
JUSTIFY-CONTENT is main axis alignment.
CONTAINER-WIDTH is the container's content width for row layouts.
CONTAINER-HEIGHT is the container's content height for column layouts (optional)."
  (if (null child-strings)
      ""
    (let* ((is-row (or (string= flex-direction "row")
                       (string= flex-direction "row-reverse")))
           (main-gap (if is-row column-gap row-gap))
           ;; Filter out empty strings
           (valid-strings (seq-filter (lambda (s) (> (length s) 0))
                                      child-strings))
           (items-count (length valid-strings))
           ;; Calculate actual total size from string dimensions
           ;; For row: sum of pixel widths; for column: sum of line counts
           (actual-total-size
            (if valid-strings
                (if is-row
                    (apply #'+ (mapcar #'string-pixel-width valid-strings))
                  (apply #'+ (mapcar #'etaf-string-linum valid-strings)))
              0))
           ;; Calculate total gap
           (total-gap (* main-gap (max 0 (1- items-count))))
           ;; Get container main axis size
           (container-main-size (if is-row
                                    (or container-width 0)
                                  (or container-height 0)))
           ;; Calculate free space based on actual sizes
           (free-space (if (> container-main-size 0)
                           (max 0 (- container-main-size actual-total-size total-gap))
                         0))
           ;; Recalculate space distribution based on actual sizes
           (space-distribution (when valid-strings
                                 (etaf-layout-flex-justify-space
                                  justify-content free-space items-count main-gap)))
           ;; Get space distribution
           (start-space (if space-distribution
                            (floor (nth 0 space-distribution))
                          0))
           (between-space (if space-distribution
                              (floor (nth 1 space-distribution))
                            (floor main-gap)))
           (end-space (if space-distribution
                          (floor (nth 2 space-distribution))
                        0)))
      (if (null valid-strings)
          ""
        (if is-row
            ;; Horizontal layout (row/row-reverse)
            (etaf-layout--flex-concat-horizontal
             valid-strings start-space between-space end-space)
          ;; Vertical layout (column/column-reverse)
          (etaf-layout--flex-stack-vertical
           valid-strings start-space between-space end-space))))))

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
         (_ (message "content-width:%S" content-width))
         (children-text
          (if is-flex-container
              ;; Flex 容器：使用 flex 布局合并
              (let ((flex-direction (dom-attr layout-node 'layout-flex-direction))
                    (row-gap (or (dom-attr layout-node 'layout-row-gap) 0))
                    (column-gap (or (dom-attr layout-node 'layout-column-gap) 0))
                    (justify-content (dom-attr layout-node 'layout-justify-content))
                    (child-strings (mapcar #'car child-infos)))
                (message "total-width: %S"
                         (string-pixel-width
                          (etaf-layout--merge-flex-children
                           child-strings flex-direction row-gap column-gap
                           justify-content content-width content-height-px)))
                ;; FIXME: 当多个item处于一行时，它们的总宽度应该等于前面的 content-width
                ;; 目前是每个 item 的宽度都等于 content-width 了。
                (etaf-layout--merge-flex-children
                 child-strings flex-direction row-gap column-gap
                 justify-content content-width content-height-px))
            ;; 非 flex 容器：使用原有的 display 类型合并逻辑
            ;; - inline 元素应该水平拼接
            ;; - block 元素应该垂直堆叠
            ;; 策略：将连续的 inline 元素组合在一起水平拼接，然后与 block 元素垂直堆叠
            (etaf-layout--merge-children-by-display child-infos)))
         
         ;; 内容：如果有子元素则使用子元素的字符串，否则为空
         (inner-content children-text)
         
         ;; 计算内容高度（行数）
         (content-height (if (> (length inner-content) 0)
                             (etaf-string-linum inner-content)
                           (if (> content-height-px 0) 1 0)))
         
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
             
             ;; 1.5 应用 CSS 文本样式到内容（转换为 Emacs face 属性）
             (styled-content (if (and computed-style (> (length sized-content) 0))
                                 (etaf-css-apply-face-to-string sized-content computed-style)
                               sized-content))
             
             ;; 重新计算内容高度（行数），因为 etaf-lines-justify 可能导致换行
             (actual-content-height (if (> (length styled-content) 0)
                                        (etaf-string-linum styled-content)
                                      content-height))
             
             ;; 计算 border 以内的高度（行数）- 使用实际内容高度
             (inner-height (+ actual-content-height padding-top padding-bottom))
             
             ;; 2. 添加 padding（垂直方向）
             (with-padding (if (and (> effective-width 0)
                                    (or (> padding-top 0) (> padding-bottom 0)))
                               (etaf-lines-stack
                                (list (when (> padding-top 0)
                                        (etaf-pixel-blank effective-width padding-top))
                                      styled-content
                                      (when (> padding-bottom 0)
                                        (etaf-pixel-blank effective-width padding-bottom))))
                             styled-content))
             
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
