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

;; 布局系统主模块
;;
;; 本模块是布局系统的主协调器，负责从渲染树构建布局树。
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
;; - `etaf-layout-build-tree' - 从渲染树构建布局树（主入口）
;; - `etaf-layout-get-box-model' - 获取布局节点的盒模型
;; - `etaf-layout-create-node' - 创建布局节点
;; - `etaf-layout-compute-box-model' - 计算盒模型
;; - `etaf-layout-node' - 递归布局节点
;; - `etaf-layout-walk' - 遍历布局树
;; - `etaf-layout-to-string' - 将布局树转换为字符串
;;
;; 布局树结构：
;; (tag ((layout-box-model . <box-model>)
;;       (computed-style . ((display . "block") (color . "red") ...)))
;;   child1 child2 ...)
;;
;; 使用示例：
;;   (setq layout-tree (etaf-layout-build-tree render-tree viewport))
;;   (setq layout-string (etaf-layout-to-string layout-tree))

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-render)
(require 'etaf-utils)

;; 引入子模块
(require 'etaf-css-parser)
(require 'etaf-layout-box)

;; Forward declarations
(declare-function etaf-layout-flex-format "etaf-layout-flex")
(declare-function etaf-layout-grid-format "etaf-layout-grid")
(declare-function etaf-layout-string-render "etaf-layout-string")

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

(defun etaf-layout-build-tree (render-tree viewport)
  "从渲染树构建布局树。
RENDER-TREE 是渲染树根节点。
VIEWPORT 是视口大小 (:width w :height h)。
  - width 和 height 可以为 nil，表示不限制根容器的该维度，使用内容的自然尺寸。
  - 当 width 为 nil 时，块级元素的宽度将根据内容自动计算。
  - 当 height 为 nil 时，容器的高度将根据内容自动计算。
返回布局树根节点。"
  (let ((root-context (list :content-width (plist-get viewport :width)
                            :content-height (plist-get viewport :height)
                            :viewport-width (plist-get viewport :width)
                            :viewport-height (plist-get viewport :height)
                            :is-root t)))
    (etaf-layout-node render-tree root-context)))

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
;;; 盒模型计算
;;; ============================================================

(defun etaf-layout-compute-box-model (render-node parent-context)
  "从渲染节点计算盒模型。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息：
  :content-width   - 可用内容宽度
  :content-height  - 可用内容高度
  :viewport-width  - 视口宽度（用于 w-screen 等）
  :viewport-height - 视口高度（用于 h-screen 等）
  :is-root         - 是否为根元素（可选）
返回盒模型 plist。"
  (let* ((style (etaf-render-get-computed-style render-node))
         (parent-width (plist-get parent-context :content-width))
         (parent-height (plist-get parent-context :content-height))
         (viewport-width (plist-get parent-context :viewport-width))
         (viewport-height (plist-get parent-context :viewport-height))
         (is-root (plist-get parent-context :is-root))
         (effective-parent-width parent-width)
         
         ;; 提取样式值
         (box-sizing (etaf-css-parse-style-value
                      style 'box-sizing "content-box"))
         
         ;; Padding
         (padding-top (etaf-css-parse-length 
                       (etaf-css-parse-style-value
                        style 'padding-top "0") 
                       effective-parent-width))
         (padding-right (etaf-css-parse-length 
                         (etaf-css-parse-style-value
                          style 'padding-right "0") 
                         effective-parent-width))
         (padding-bottom (etaf-css-parse-length 
                          (etaf-css-parse-style-value
                           style 'padding-bottom "0") 
                          effective-parent-width))
         (padding-left (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'padding-left "0") 
                        effective-parent-width))
         
         ;; Border
         (border-top (etaf-css-parse-length 
                      (etaf-css-parse-style-value
                       style 'border-top-width "0") 
                      effective-parent-width))
         (border-right (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'border-right-width "0") 
                        effective-parent-width))
         (border-bottom (etaf-css-parse-length 
                         (etaf-css-parse-style-value
                          style 'border-bottom-width "0") 
                         effective-parent-width))
         (border-left (etaf-css-parse-length 
                       (etaf-css-parse-style-value
                        style 'border-left-width "0") 
                       effective-parent-width))
         
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
                      effective-parent-width))
         (margin-right (etaf-css-parse-length 
                        (etaf-css-parse-style-value
                         style 'margin-right "0") 
                        effective-parent-width))
         (margin-bottom
          (etaf-css-parse-length 
           (etaf-css-parse-style-value style 'margin-bottom "0") 
           effective-parent-width))
         (margin-left (etaf-css-parse-length 
                       (etaf-css-parse-style-value style 'margin-left "0") 
                       effective-parent-width))
         
         ;; Width and Height
         (width-value (etaf-css-parse-length 
                       (etaf-css-parse-style-value style 'width "auto") 
                       effective-parent-width))
         (height-value (etaf-css-parse-height 
                        (etaf-css-parse-style-value style 'height "auto") 
                        parent-height))
         
         ;; min-width, max-width
         (min-width-value (etaf-css-parse-length
                           (etaf-css-parse-style-value
                            style 'min-width "0")
                           effective-parent-width))
         (max-width-value (etaf-css-parse-length
                           (etaf-css-parse-style-value
                            style 'max-width "none")
                           effective-parent-width))
         
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
         (display (etaf-render-get-display render-node))
         (is-inline (or (string= display "inline")
                        (string= display "inline-block")))
         (is-flex-container (string= display "flex"))
         (is-in-flex-container (plist-get parent-context :flex-container))
         
         ;; 处理 min/max 值和特殊关键字
         ;; 解析 screen 关键字为视口宽度（如果 viewport 为 nil，则使用窗口宽度）
         (min-width-val (cond
                         ((eq min-width-value 'screen)
                          (or viewport-width (etaf-window-content-pixel-width)))
                         ((or (eq min-width-value 'auto)
                              (eq min-width-value 'none))
                          0)
                         ((eq min-width-value 'fit-content) 0) ; fit-content 在 min 中相当于 0
                         ((eq min-width-value 'min-content) 0) ; min-content 在 min 中为 0（不限制）
                         ((eq min-width-value 'max-content) 0) ; max-content 在 min 中为 0（不限制）
                         (t min-width-value)))
         (max-width-val (cond
                         ((eq max-width-value 'screen)
                          (or viewport-width (etaf-window-content-pixel-width)))
                         ((or (eq max-width-value 'auto)
                              (eq max-width-value 'none))
                          nil)
                         ;; fit-content, min-content, max-content 在 max 中不限制
                         ((memq max-width-value '(fit-content min-content max-content))
                          nil)
                         (t max-width-value)))
         (min-height-val (cond
                          ((eq min-height-value 'screen)
                           (or viewport-height (window-body-height)))
                          ((or (eq min-height-value 'auto)
                               (eq min-height-value 'none))
                           0)
                          ((eq min-height-value 'fit-content) 0)
                          ((eq min-height-value 'min-content) 0)
                          ((eq min-height-value 'max-content) 0)
                          (t min-height-value)))
         (max-height-val (cond
                          ((eq max-height-value 'screen)
                           (or viewport-height (window-body-height)))
                          ((or (eq max-height-value 'auto)
                               (eq max-height-value 'none))
                           nil)
                          ((memq max-height-value '(fit-content min-content max-content))
                           nil)
                          (t max-height-value)))
         
         ;; 计算内容宽度
         ;; 处理特殊关键字: screen, fit-content, min-content, max-content
         ;; flex 容器自身没有指定宽度时，应该使用父容器的可用宽度，这样 justify-content 才能正常工作
         (base-content-width
          (cond
           ;; screen: 使用视口宽度（如果 viewport 为 nil，则使用窗口宽度）
           ((eq width-value 'screen)
            (let ((screen-width (or viewport-width (etaf-window-content-pixel-width))))
              (max 0 (- screen-width
                        padding-left-val padding-right-val
                        border-left-val border-right-val
                        margin-left-val margin-right-val))))
           ;; fit-content, min-content, max-content: 暂时设为 0，稍后根据内容计算
           ;; 这些值需要先遍历子元素来确定内容宽度
           ((memq width-value '(fit-content min-content max-content))
            0)
           ;; auto: 根据上下文决定
           ((eq width-value 'auto)
            (if (or is-inline is-in-flex-container (null effective-parent-width))
                0
              (max 0 (- effective-parent-width
                        padding-left-val padding-right-val
                        border-left-val border-right-val
                        margin-left-val margin-right-val))))
           ;; 具体数值
           (t width-value)))
         
         ;; 标记是否需要根据内容计算宽度
         (needs-content-width (memq width-value '(fit-content min-content max-content)))
         
         (content-width (min (or max-width-val most-positive-fixnum)
                             (max min-width-val base-content-width)))
         
         ;; 计算基础高度
         ;; 处理特殊关键字: screen, fit-content, min-content, max-content
         ;; - 如果有显式 CSS 高度，使用该值
         ;; - 如果是根元素且有视口高度约束，使用视口高度
         ;; - 否则使用 0（后续根据内容计算）
         (base-content-height
          (cond
           ;; screen: 使用视口高度（如果 viewport 为 nil，则使用窗口高度）
           ((eq height-value 'screen)
            (let ((screen-height (or viewport-height (window-body-height))))
              (max 0 (- screen-height
                        padding-top-val padding-bottom-val
                        border-top-val border-bottom-val
                        margin-top-val margin-bottom-val))))
           ;; fit-content, min-content, max-content: 暂时设为 0，稍后根据内容计算
           ((memq height-value '(fit-content min-content max-content))
            0)
           ;; auto: 根据上下文决定
           ((eq height-value 'auto)
            (if (and is-root parent-height)
                ;; 根元素使用视口高度作为约束
                (max 0 (- parent-height
                          padding-top-val padding-bottom-val
                          border-top-val border-bottom-val
                          margin-top-val margin-bottom-val))
              0))
           ;; 具体数值
           (t height-value)))
         
         ;; 标记是否需要根据内容计算高度
         (needs-content-height (memq height-value '(fit-content min-content max-content)))
         
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
                          :scroll-track-color scroll-track-color)
          ;; 添加元数据用于后续处理
          :width-keyword width-value  ; 保存原始的width关键字（fit-content等）
          :height-keyword height-value  ; 保存原始的height关键字
          :needs-content-width needs-content-width
          :needs-content-height needs-content-height
          :parent-width effective-parent-width
          :parent-height parent-height)))

;;; ============================================================
;;; 布局算法
;;; ============================================================

(defun etaf-layout-block-formatting-context (render-node parent-context)
  "在块级格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息。
返回布局节点。"
  (message "F-parent-context:%S" parent-context)
  (let* ((box-model (etaf-layout-compute-box-model
                     render-node parent-context))
         (content-width (etaf-layout-box-content-width box-model))
         (content-height (etaf-layout-box-content-height box-model))
         ;; Check if we need shrink-to-fit: original
         ;; parent-width was nil and we're at root with auto width
         (need-shrink-to-fit (and (null (plist-get parent-context :content-width))
                                  (plist-get parent-context :is-root)))
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 递归布局子元素
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context
               (list :content-width content-width
                     :content-height content-height
                     :viewport-width (plist-get parent-context :viewport-width)
                     :viewport-height (plist-get parent-context :viewport-height)))
              (child-layouts '())
              (accumulated-height 0)
              (max-child-width 0)  ; Track maximum child width for auto-sizing
              ;; 检查是否有inline元素子节点
              (has-inline-element nil))
          
          ;; 先遍历一次检查是否有inline或inline-block元素
          (dolist (child children)
            (when (and (consp child) (symbolp (car child)))
              (let ((child-display
                     (or (etaf-render-get-display child)
                         (etaf-render-get-default-display (car child)))))
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
                        (or (etaf-render-get-display child-layout) "block"))
                       (child-total-height
                        (+ (etaf-layout-box-content-height child-box)
                           (etaf-layout-box-padding-height child-box)
                           (etaf-layout-box-margin-height child-box)))
                       (child-total-width
                        (+ (etaf-layout-box-content-width child-box)
                           (etaf-layout-box-padding-width child-box)
                           (etaf-layout-box-border-width child-box)
                           (etaf-layout-box-margin-width child-box))))
                  ;; Track maximum child width for auto-sizing
                  (setq max-child-width (max max-child-width child-total-width))
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
                         :height accumulated-height)))
          
          ;; 处理特殊宽度关键字：fit-content, min-content, max-content
          (let ((box (etaf-layout-get-box-model layout-node)))
            (when (plist-get box :needs-content-width)
              (let* ((width-keyword (plist-get box :width-keyword))
                     (parent-width (plist-get box :parent-width))
                     (padding (plist-get box :padding))
                     (border (plist-get box :border))
                     (margin (plist-get box :margin))
                     ;; 计算父容器可用宽度（减去padding、border、margin）
                     (available-parent-width
                      (when parent-width
                        (max 0 (- parent-width
                                  (plist-get padding :left)
                                  (plist-get padding :right)
                                  (plist-get border :left-width)
                                  (plist-get border :right-width)
                                  (plist-get margin :left)
                                  (plist-get margin :right)))))
                     ;; 内容的自然宽度是子元素的最大宽度
                     (natural-content-width max-child-width)
                     ;; 根据关键字确定最终宽度
                     (final-width
                      (cond
                       ;; fit-content: min(parent-width, max(min-content, content-width))
                       ;; 这里简化为 min(parent-width, content-width)
                       ((eq width-keyword 'fit-content)
                        (if available-parent-width
                            (min available-parent-width natural-content-width)
                          natural-content-width))
                       ;; min-content: 内容的最小宽度
                       ;; TODO: 应该计算最窄点宽度，当前简化为使用子元素最大宽度
                       ((eq width-keyword 'min-content)
                        natural-content-width)
                       ;; max-content: 内容的最大宽度（不换行时的宽度）
                       ;; TODO: 应该计算无换行时的宽度，当前简化为使用子元素最大宽度
                       ((eq width-keyword 'max-content)
                        natural-content-width)
                       (t natural-content-width))))
                (plist-put (plist-get box :content)
                           :width final-width))))
          
          ;; 处理特殊高度关键字：fit-content, min-content, max-content
          (let ((box (etaf-layout-get-box-model layout-node)))
            (when (plist-get box :needs-content-height)
              (let* ((height-keyword (plist-get box :height-keyword))
                     (parent-height (plist-get box :parent-height))
                     (padding (plist-get box :padding))
                     (border (plist-get box :border))
                     (margin (plist-get box :margin))
                     ;; 计算父容器可用高度（减去padding、border、margin）
                     (available-parent-height
                      (when parent-height
                        (max 0 (- parent-height
                                  (plist-get padding :top)
                                  (plist-get padding :bottom)
                                  (plist-get border :top-width)
                                  (plist-get border :bottom-width)
                                  (plist-get margin :top)
                                  (plist-get margin :bottom)))))
                     ;; 内容的自然高度是累积的子元素高度
                     (natural-content-height accumulated-height)
                     ;; 根据关键字确定最终高度
                     (final-height
                      (cond
                       ;; fit-content: min(parent-height, content-height)
                       ((eq height-keyword 'fit-content)
                        (if available-parent-height
                            (min available-parent-height natural-content-height)
                          natural-content-height))
                       ;; min-content: 内容的最小高度
                       ;; TODO: 应该计算最小可能高度，当前简化为使用累积高度
                       ((eq height-keyword 'min-content)
                        natural-content-height)
                       ;; max-content: 内容的最大高度
                       ;; TODO: 应该计算最大固有高度，当前简化为使用累积高度
                       ((eq height-keyword 'max-content)
                        natural-content-height)
                       (t natural-content-height))))
                (plist-put (plist-get box :content)
                           :height final-height))))
          
          ;; 宽度shrink-to-fit：当viewport width为nil且根元素width为auto时，
          ;; 使用子元素最大宽度作为容器宽度
          (when need-shrink-to-fit
            (let ((box (etaf-layout-get-box-model layout-node)))
              (plist-put (plist-get box :content)
                         :width max-child-width))))))
    
    layout-node))

(defun etaf-layout-node (render-node parent-context)
  "递归布局渲染节点。
RENDER-NODE 是渲染节点。
PARENT-CONTEXT 是父容器上下文。
返回布局节点或 nil。"
  (when render-node
    (let ((display (etaf-render-get-display render-node)))
      (cond
       ((string= display "flex")
        (etaf-layout-flex-format render-node parent-context))
       ((string= display "grid")
        (etaf-layout-grid-format render-node parent-context))
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
(require 'etaf-layout-grid)
(require 'etaf-layout-string)

(provide 'etaf-layout)
;;; etaf-layout.el ends here
