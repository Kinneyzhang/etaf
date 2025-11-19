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
;; 数据结构说明：
;;
;; 盒模型 (Box Model):
;;   (:box-sizing "content-box"|"border-box"
;;    :content (:width <number> :height <number>)
;;    :padding (:top <n> :right <n> :bottom <n> :left <n>)
;;    :border (:top-width <n> :right-width <n> :bottom-width <n> :left-width <n>
;;             :top-color <color> :right-color <color> :bottom-color <color> :left-color <color>)
;;    :margin (:top <n> :right <n> :bottom <n> :left <n>))
;;
;; 布局节点 (Layout Node):
;;   (:render-node <render-node>
;;    :box-model <box-model>
;;    :position (:x <number> :y <number>)
;;    :bounds (:x <n> :y <n> :width <n> :height <n>)
;;    :content-box (:x <n> :y <n> :width <n> :height <n>)
;;    :children (<layout-node> ...))
;;
;; 使用示例：
;;
;;   ;; 构建布局树
;;   (setq layout-tree (etaf-layout-build-tree render-tree viewport))
;;
;;   ;; 遍历布局树
;;   (etaf-layout-walk layout-tree
;;     (lambda (node)
;;       (message "Position: (%d,%d) Size: %dx%d"
;;                (plist-get (plist-get node :position) :x)
;;                (plist-get (plist-get node :position) :y)
;;                (plist-get (plist-get node :box-model) :width)
;;                (plist-get (plist-get node :box-model) :height))))

;;; Code:

(require 'cl-lib)

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

(defun etaf-layout-compute-box-model (render-node parent-context)
  "从渲染节点计算盒模型。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息：
  :content-width  - 可用内容宽度
  :content-height - 可用内容高度
返回盒模型 plist。"
  (let* ((style (plist-get render-node :computed-style))
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
         (layout-node (list :render-node render-node
                           :box-model box-model
                           :position (list :x x :y y)
                           :content-box (list :x content-x 
                                            :y content-y
                                            :width content-width
                                            :height content-height)
                           :children '())))
    
    ;; 递归布局子元素
    (let ((children (plist-get render-node :children)))
      (when children
        (let ((child-context (list :content-width content-width
                                  :content-height content-height
                                  :current-y content-y
                                  :x-offset content-x))
              (child-layouts '())
              (accumulated-height 0))
          
          (dolist (child children)
            (when-let ((child-layout (etaf-layout-node child child-context)))
              (push child-layout child-layouts)
              ;; 累积子元素高度
              (let ((child-total-height (etaf-box-model-total-height 
                                        (plist-get child-layout :box-model))))
                (setq accumulated-height (+ accumulated-height child-total-height))
                ;; 更新下一个子元素的 Y 坐标
                (plist-put child-context :current-y 
                          (+ content-y accumulated-height)))))
          
          ;; 设置子节点（保持原始顺序）
          (plist-put layout-node :children (nreverse child-layouts))
          
          ;; 如果高度为 auto，根据子元素设置高度
          (when (= content-height 0)
            (plist-put (plist-get box-model :content) :height accumulated-height)
            (plist-put (plist-get layout-node :content-box) :height accumulated-height)))))
    
    layout-node))

(defun etaf-layout-node (render-node parent-context)
  "递归布局渲染节点。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 是父容器上下文。
返回布局节点或 nil。"
  (when render-node
    (let* ((display (plist-get render-node :display))
           (style (plist-get render-node :computed-style))
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
FUNC 是接受一个布局节点参数的函数。"
  (when layout-tree
    (funcall func layout-tree)
    (dolist (child (plist-get layout-tree :children))
      (etaf-layout-walk child func))))

(defun etaf-layout-to-string (layout-tree &optional indent)
  "将布局树转换为可读的字符串形式。
LAYOUT-TREE 是布局树根节点。
INDENT 是缩进级别（可选）。
返回格式化的字符串。"
  (setq indent (or indent 0))
  (let* ((indent-str (make-string (* indent 2) ?\s))
         (render-node (plist-get layout-tree :render-node))
         (tag (plist-get render-node :tag))
         (position (plist-get layout-tree :position))
         (box-model (plist-get layout-tree :box-model))
         (content (plist-get box-model :content))
         (children (plist-get layout-tree :children)))
    (concat indent-str
            (format "<%s> pos=(%d,%d) size=%dx%d"
                    tag
                    (plist-get position :x)
                    (plist-get position :y)
                    (plist-get content :width)
                    (plist-get content :height))
            (when children
              (concat "\n"
                      (mapconcat (lambda (child)
                                  (etaf-layout-to-string child (1+ indent)))
                                children "\n"))))))

(provide 'etaf-layout)
;;; etaf-layout.el ends here
