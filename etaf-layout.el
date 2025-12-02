;;; etaf-layout.el --- Layout computation from DOM and CSSOM -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, box-model, css
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 布局系统主模块
;;
;; 本模块负责从 DOM 和 CSSOM 直接构建布局树，集成了原来分离的渲染树构建步骤。
;; 所有函数使用 `etaf-layout-' 前缀。
;;
;; 模块结构：
;; - etaf-layout.el (本模块): 主协调器和布局树构建
;; - etaf-css-parse.el: CSS 值解析（由 CSS 模块提供）
;; - etaf-layout-box.el: 盒模型数据结构和操作
;; - etaf-layout-flex.el: Flex 布局格式化上下文
;; - etaf-layout-string.el: 布局到字符串转换
;;
;; 公共接口：
;; - `etaf-layout-build-tree' - 从 DOM 和 CSSOM 构建布局树（主入口）
;; - `etaf-layout-get-box-model' - 获取布局节点的盒模型
;; - `etaf-layout-get-style' - 获取布局节点的样式属性
;; - `etaf-layout-get-display' - 获取布局节点的 display 类型
;; - `etaf-layout-walk' - 遍历布局树
;; - `etaf-layout-to-string' - 将布局树转换为字符串
;;
;; 布局树结构：
;; (tag ((layout-box-model . <box-model>)
;;       (render-style . ((display . "block") (color . "red") ...))
;;       (id . "main"))  ;; 保留除 class 外的原始 DOM 属性
;;   child1 child2 ...)
;;
;; 使用示例：
;;   (setq layout-tree (etaf-layout-build-tree dom cssom viewport))
;;   (setq layout-string (etaf-layout-to-string layout-tree))

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-dom)
(require 'etaf-css)
(require 'etaf-utils)

;; 引入子模块
(require 'etaf-css-parse)
(require 'etaf-layout-box)

;; Forward declarations
(declare-function etaf-layout-flex-format "etaf-layout-flex")
(declare-function etaf-layout-string-render "etaf-layout-string")

;;; ============================================================
;;; 样式和显示类型辅助函数（原 etaf-render 模块功能）
;;; ============================================================

;; HTML 块级元素列表
(defconst etaf-layout-block-level-tags
  '(div p h1 h2 h3 h4 h5 h6 ul ol li dl dt dd
    article aside section nav header footer main
    blockquote pre address figure figcaption
    form fieldset table thead tbody tfoot tr th td
    hr html body)
  "HTML 块级元素标签列表。这些元素默认 display 为 block。")

(defun etaf-layout-get-default-display (tag)
  "根据元素标签返回默认的 display 值。
TAG 是元素标签名（symbol）。
块级元素返回 \"block\"，其他返回 \"inline\"。"
  (if (memq tag etaf-layout-block-level-tags)
      "block"
    "inline"))

(defun etaf-layout-get-style (layout-node property)
  "从布局节点获取指定样式属性的值。
LAYOUT-NODE 是布局节点。
PROPERTY 是样式属性名（symbol）。
返回属性值字符串或 nil。"
  (cdr (assq property (dom-attr layout-node 'render-style))))

(defun etaf-layout-get-display (layout-node)
  "从布局节点获取 display 类型。
LAYOUT-NODE 是布局节点。
返回 display 字符串。"
  (or (etaf-layout-get-style layout-node 'display)
      (etaf-layout-get-default-display (dom-tag layout-node))))

(defun etaf-layout-get-computed-style (layout-node)
  "从布局节点获取完整的计算样式 alist。
LAYOUT-NODE 是布局节点。
返回计算样式 alist。"
  (dom-attr layout-node 'render-style))

(defun etaf-layout--node-visible-p (dom-node computed-style)
  "判断 DOM 节点是否应该在布局树中显示。
不显示的元素包括：
- display: none
- <head>, <script>, <style>, <meta>, <link> 等不可见标签"
  (let ((tag (dom-tag dom-node))
        (display (cdr (assq 'display computed-style))))
    (and (not (memq tag '(head script style meta link title)))
         (not (string= display "none")))))

(defun etaf-layout--create-styled-node (dom-node computed-style &optional computed-style-dark)
  "创建带样式的节点（中间步骤，用于构建布局树）。
DOM-NODE 是 DOM 节点。
COMPUTED-STYLE 是亮色模式下的计算样式 alist。
COMPUTED-STYLE-DARK 是暗色模式下的计算样式 alist（可选）。
返回 DOM 格式的节点：(tag ((attrs...) children...)
其中 attrs 包含：
- render-style: 亮色模式计算样式（包含 display 属性）
- render-style-dark: 暗色模式计算样式（如果与亮色不同）
- 保留除 class 外的原始 DOM 属性（如 id）"
  (let* ((tag (dom-tag dom-node))
         ;; 从 computed-style 获取 display，如果没有则根据标签类型使用默认值
         (display (or (cdr (assq 'display computed-style))
                      (etaf-layout-get-default-display tag)))
         ;; 确保 computed-style 中有 display 属性
         (computed-style-with-display
          (if (assq 'display computed-style)
              computed-style
            (cons (cons 'display display) computed-style)))
         (orig-attrs (dom-attributes dom-node))
         ;; 过滤掉 class 属性，保留其他属性（如 id）
         (filtered-attrs (seq-filter (lambda (attr) (not (eq (car attr) 'class))) orig-attrs))
         ;; 构建新的属性 alist，添加样式信息
         ;; 只有当暗色样式与亮色样式不同时才添加 render-style-dark
         (render-attrs (if computed-style-dark
                           ;; 确保暗色模式样式也包含 display
                           (let* ((dark-display (or (cdr (assq 'display computed-style-dark)) display))
                                  (computed-style-dark-with-display
                                   (if (assq 'display computed-style-dark)
                                       computed-style-dark
                                     (cons (cons 'display dark-display) computed-style-dark))))
                             ;; 现在两个样式都有 display，可以正确比较
                             ;; 使用 equal 进行深度比较以检测样式差异
                             ;; 虽然对大样式对象可能较慢，但这是必要的以确保
                             ;; 只有在暗色模式真正不同时才保存 render-style-dark
                             (if (equal computed-style-with-display computed-style-dark-with-display)
                                 ;; 样式相同，只保留亮色
                                 (list (cons 'render-style computed-style-with-display))
                               ;; 样式不同，保留两者
                               (list (cons 'render-style computed-style-with-display)
                                     (cons 'render-style-dark computed-style-dark-with-display))))
                         ;; 没有暗色样式，只使用亮色
                         (list (cons 'render-style computed-style-with-display)))))
    ;; 合并样式属性和过滤后的原始属性
    (list tag (append render-attrs filtered-attrs))))

;;; ============================================================
;;; 公共接口
;;; ============================================================

(defun etaf-layout-get-box-model (layout-node)
  "从布局节点获取盒模型。
LAYOUT-NODE 是布局节点。
返回盒模型 plist。"
  (dom-attr layout-node 'layout-box-model))

(defun etaf-layout-create-node (render-node box-model)
  "创建布局节点（使用 DOM 格式）。
RENDER-NODE 是渲染节点。
BOX-MODEL 是盒模型 plist。
返回 DOM 格式的布局节点。"
  (let* ((tag (dom-tag render-node))
         (render-attrs (dom-attributes render-node))
         (layout-attrs (list (cons 'layout-box-model box-model))))
    (list tag (append layout-attrs render-attrs))))

(defun etaf-layout-build-tree (dom cssom viewport)
  "从 DOM 和 CSSOM 直接构建布局树。
DOM 是 DOM 树根节点。
CSSOM 是 CSS 对象模型。
VIEWPORT 是视口大小 (:width w :height h)。
  - width 和 height 可以为 nil，表示不限制根容器的该维度，使用内容的自然尺寸。
  - 当 width 为 nil 时，块级元素的宽度将根据内容自动计算。
  - 当 height 为 nil 时，容器的高度将根据内容自动计算。
返回布局树根节点。"
  (let ((root-context (list :content-width (plist-get viewport :width)
                            :content-height (plist-get viewport :height)
                            :is-root t
                            :cssom cssom
                            :root-dom dom)))
    (etaf-layout--build-node dom root-context)))

(defun etaf-layout-walk (layout-tree func)
  "遍历布局树，对每个节点调用 FUNC。
LAYOUT-TREE 是布局树根节点。
FUNC 是接受一个布局节点参数的函数。"
  (etaf-dom-map func layout-tree))

(defun etaf-layout-to-string (layout-tree)
  "将布局树转换为可插入 Emacs buffer 的字符串。
LAYOUT-TREE 是布局树根节点。
返回拼接好的布局字符串。"
  (etaf-layout-string-render layout-tree))

;;; ============================================================
;;; 布局树构建（从 DOM 节点）
;;; ============================================================

(defun etaf-layout--build-node (dom-node parent-context)
  "从 DOM 节点递归构建布局节点。
DOM-NODE 是当前 DOM 节点。
PARENT-CONTEXT 包含父容器上下文信息，包括 :cssom 和 :root-dom。
返回布局节点或 nil（如果节点不可见）。"
  (when-let ((tag (dom-tag dom-node)))
    (let* ((cssom (plist-get parent-context :cssom))
           (root-dom (plist-get parent-context :root-dom))
           ;; 获取双模式计算样式
           (dual-style (etaf-css-get-computed-style-dual-mode cssom dom-node root-dom))
           (computed-style (plist-get dual-style :light))
           (computed-style-dark (plist-get dual-style :dark)))
      ;; 检查节点是否可见
      (when (etaf-layout--node-visible-p dom-node computed-style)
        ;; 创建带样式的节点
        (let ((styled-node (etaf-layout--create-styled-node dom-node computed-style computed-style-dark)))
          ;; 递归处理子节点
          (let ((children '()))
            (dolist (child (dom-children dom-node))
              (cond
               ;; 元素节点：递归构建布局节点
               ((and (consp child) (symbolp (car child)))
                (when-let ((child-layout (etaf-layout--build-node child parent-context)))
                  (push child-layout children)))
               ;; 文本节点：直接保留
               ((stringp child)
                (push child children))))
            ;; 将子节点添加到styled-node
            (setcdr (cdr styled-node) (nreverse children)))
          ;; 现在有了带样式和子节点的节点，进行布局计算
          (etaf-layout-node styled-node parent-context))))))

;;; ============================================================
;;; 盒模型计算
;;; ============================================================

(defun etaf-layout-compute-box-model (render-node parent-context)
  "从渲染节点计算盒模型。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息：
  :content-width  - 可用内容宽度
  :content-height - 可用内容高度
  :is-root        - 是否为根元素（可选）
返回盒模型 plist。"
  (let* ((style (etaf-layout-get-computed-style render-node))
         (parent-width (plist-get parent-context :content-width))
         (parent-height (plist-get parent-context :content-height))
         (is-root (plist-get parent-context :is-root))
         
         ;; 提取样式值
         (box-sizing (etaf-css-parse-style-value
                      style 'box-sizing "content-box"))
         
         ;; Padding
         (padding-top (etaf-css-parse-length 
                       (etaf-css-parse-style-value
                        style 'padding-top "0") 
                       parent-width))
         (padding-right (etaf-css-parse-length 
                         (etaf-css-parse-style-value
                          style 'padding-right "0") 
                         parent-width))
         (padding-bottom (etaf-css-parse-length 
                          (etaf-css-parse-style-value
                           style 'padding-bottom "0") 
                          parent-width))
         (padding-left (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'padding-left "0") 
                        parent-width))
         
         ;; Border
         (border-top (etaf-css-parse-length 
                      (etaf-css-parse-style-value
                       style 'border-top-width "0") 
                      parent-width))
         (border-right (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'border-right-width "0") 
                        parent-width))
         (border-bottom (etaf-css-parse-length 
                         (etaf-css-parse-style-value
                          style 'border-bottom-width "0") 
                         parent-width))
         (border-left (etaf-css-parse-length 
                       (etaf-css-parse-style-value
                        style 'border-left-width "0") 
                       parent-width))
         
         (border-top-color (etaf-css-parse-style-value
                            style 'border-top-color
                            (face-attribute 'default :foreground)))
         (border-right-color (etaf-css-parse-style-value
                              style 'border-right-color
                              (face-attribute 'default :foreground)))
         (border-bottom-color (etaf-css-parse-style-value
                               style 'border-bottom-color
                               (face-attribute 'default :foreground)))
         (border-left-color (etaf-css-parse-style-value
                             style 'border-left-color
                             (face-attribute 'default :foreground)))
         
         ;; Margin
         (margin-top (etaf-css-parse-length 
                      (etaf-css-parse-style-value
                       style 'margin-top "0") 
                      parent-width))
         (margin-right (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'margin-right "0") 
                        parent-width))
         (margin-bottom
          (etaf-css-parse-length 
           (etaf-css-parse-style-value style 'margin-bottom "0") 
           parent-width))
         (margin-left (etaf-css-parse-length 
                       (etaf-css-parse-style-value style 'margin-left "0") 
                       parent-width))
         
         ;; Width and Height
         (width-value (etaf-css-parse-length 
                       (etaf-css-parse-style-value style 'width "auto") 
                       parent-width))
         (height-value (etaf-css-parse-height 
                        (etaf-css-parse-style-value style 'height "auto") 
                        parent-height))
         
         ;; min-width, max-width
         (min-width-value (etaf-css-parse-length
                           (etaf-css-parse-style-value
                            style 'min-width "0")
                           parent-width))
         (max-width-value (etaf-css-parse-length
                           (etaf-css-parse-style-value
                            style 'max-width "none")
                           parent-width))
         
         ;; min-height, max-height
         (min-height-value (etaf-css-parse-height
                            (etaf-css-parse-style-value
                             style 'min-height "0")
                            parent-height))
         (max-height-value (etaf-css-parse-height
                            (etaf-css-parse-style-value
                             style 'max-height "none")
                            parent-height))
         
         ;; 溢出处理属性
         (overflow-y (etaf-css-parse-style-value
                      style 'overflow-y "visible"))
         (v-scroll-bar-type
          (when-let ((val (etaf-css-parse-style-value
                           style 'v-scroll-bar-type nil)))
            (if (symbolp val) val (intern val))))
         (v-scroll-bar-type-plist
          (when v-scroll-bar-type
            (etaf-layout-scroll-bar-create v-scroll-bar-type)))
         (v-scroll-bar-direction
          (when-let ((val (etaf-css-parse-style-value
                           style 'v-scroll-bar-direction "right")))
            (if (symbolp val) val (intern val))))
         (scroll-thumb-color
          (when v-scroll-bar-type-plist
            (when-let ((color (plist-get
                               v-scroll-bar-type-plist :thumb-color)))
              (or color
                  (etaf-css-parse-style-value
                   style 'scroll-thumb-color
                   (face-attribute 'default :foreground))))))
         (scroll-track-color
          (when v-scroll-bar-type-plist
            (when-let ((color (plist-get
                               v-scroll-bar-type-plist :track-color)))
              (or color
                  (etaf-css-parse-style-value
                   style 'scroll-track-color
                   (face-attribute 'default :background))))))
         
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
         
         ;; display 类型
         (display (etaf-layout-get-display render-node))
         (is-inline (or (string= display "inline")
                        (string= display "inline-block")))
         (is-in-flex-container (plist-get parent-context :flex-container))
         
         ;; 处理 min/max 值
         (min-width-val (if (or (eq min-width-value 'auto)
                                (eq min-width-value 'none))
                            0
                          min-width-value))
         (max-width-val (if (or (eq max-width-value 'auto)
                                (eq max-width-value 'none))
                            nil
                          max-width-value))
         (min-height-val (if (or (eq min-height-value 'auto)
                                 (eq min-height-value 'none))
                             0
                           min-height-value))
         (max-height-val (if (or (eq max-height-value 'auto)
                                 (eq max-height-value 'none))
                             nil
                           max-height-value))
         
         ;; 计算内容宽度
         (base-content-width
          (if (eq width-value 'auto)
              (if (or is-inline is-in-flex-container (null parent-width))
                  0
                (max 0 (- parent-width
                          padding-left-val padding-right-val
                          border-left-val border-right-val
                          margin-left-val margin-right-val)))
            width-value))
         
         (content-width (min (or max-width-val most-positive-fixnum)
                             (max min-width-val base-content-width)))
         
         ;; 计算基础高度
         ;; - 如果有显式 CSS 高度，使用该值
         ;; - 如果是根元素且有视口高度约束，使用视口高度
         ;; - 否则使用 0（后续根据内容计算）
         (base-content-height (if (eq height-value 'auto)
                                  (if (and is-root parent-height)
                                      ;; 根元素使用视口高度作为约束
                                      (max 0 (- parent-height
                                                padding-top-val padding-bottom-val
                                                border-top-val border-bottom-val
                                                margin-top-val margin-bottom-val))
                                    0)
                                height-value))
         
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
                        :left margin-left-val)
          :overflow (list :overflow-y overflow-y
                          :v-scroll-bar-type v-scroll-bar-type
                          :v-scroll-bar-direction v-scroll-bar-direction
                          :scroll-thumb-color scroll-thumb-color
                          :scroll-track-color scroll-track-color))))

;;; ============================================================
;;; 布局算法
;;; ============================================================

(defun etaf-layout-block-formatting-context (render-node parent-context)
  "在块级格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息。
返回布局节点。"
  (let* ((box-model (etaf-layout-compute-box-model
                     render-node parent-context))
         (content-width (etaf-layout-box-content-width box-model))
         (content-height (etaf-layout-box-content-height box-model))
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 递归布局子元素
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                   :content-height content-height))
              (child-layouts '())
              (accumulated-height 0)
              ;; 检查是否有inline元素子节点
              (has-inline-element nil))
          
          ;; 先遍历一次检查是否有inline或inline-block元素
          (dolist (child children)
            (when (and (consp child) (symbolp (car child)))
              (let ((child-display
                     (or (etaf-layout-get-style child 'display)
                         (etaf-layout-get-default-display (car child)))))
                (when (or (string= child-display "inline")
                          (string= child-display "inline-block"))
                  (setq has-inline-element t)))))
          
          (dolist (child children)
            (cond
             ((and (consp child) (symbolp (car child)))
              (when-let ((child-layout
                          (etaf-layout-node child child-context)))
                (push child-layout child-layouts)
                ;; 只有block元素才计入高度，inline元素会在渲染时合并
                ;; 计算子元素高度时不包含border高度，因为上下border使用
                ;; :overline/:underline face实现，不占用额外行数
                (let* ((child-box (etaf-layout-get-box-model child-layout))
                       (child-display
                        (or (etaf-layout-get-style child-layout 'display) "block"))
                       (child-total-height
                        (+ (etaf-layout-box-content-height child-box)
                           (etaf-layout-box-padding-height child-box)
                           (etaf-layout-box-margin-height child-box))))
                  ;; 只有block子元素才累加高度
                  (when (string= child-display "block")
                    (setq accumulated-height
                          (+ accumulated-height child-total-height))))))
             ((stringp child)
              (push child child-layouts)
              ;; 只有在没有inline元素时，字符串才计入高度
              ;; 如果有inline元素，内容会在渲染时合并，高度由渲染结果决定
              (unless has-inline-element
                (let ((string-height (etaf-string-linum child)))
                  (setq accumulated-height
                        (+ accumulated-height string-height)))))))
          
          (setcdr (cdr layout-node) (nreverse child-layouts))
          
          ;; 高度为 auto 时，根据子元素设置
          (when (= content-height 0)
            (let ((box (etaf-layout-get-box-model layout-node)))
              (plist-put (plist-get box :content)
                         :height accumulated-height))))))
    
    layout-node))

(defun etaf-layout-node (render-node parent-context)
  "递归布局渲染节点。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 是父容器上下文。
返回布局节点或 nil。"
  (when render-node
    (let ((display (etaf-layout-get-display render-node)))
      (cond
       ((string= display "flex")
        (etaf-layout-flex-format render-node parent-context))
       ((or (string= display "block")
            (null display))
        (etaf-layout-block-formatting-context render-node parent-context))
       ((string= display "inline")
        (etaf-layout-block-formatting-context render-node parent-context))
       ((string= display "inline-block")
        (etaf-layout-block-formatting-context render-node parent-context))
       (t (etaf-layout-block-formatting-context
           render-node parent-context))))))

;;; ============================================================
;;; 延迟加载子模块
;;; ============================================================

(require 'etaf-layout-flex)
(require 'etaf-layout-string)

(provide 'etaf-layout)
;;; etaf-layout.el ends here
