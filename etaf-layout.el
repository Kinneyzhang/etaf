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
(eval-and-compile (require 'dom))
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
                            :is-root t)))
    (message "root-context:%S" root-context)
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
  :content-width  - 可用内容宽度
  :content-height - 可用内容高度
  :is-root        - 是否为根元素（可选）
返回盒模型 plist。"
  (let* ((style (etaf-render-get-computed-style render-node))
         (parent-width (plist-get parent-context :content-width))
         (_ (message "parent-width:%S" parent-width))
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
         (display (etaf-render-get-display render-node))
         (is-inline (or (string= display "inline")
                        (string= display "inline-block")))
         (is-flex-container (string= display "flex"))
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
         ;; flex 容器自身没有指定宽度时，应该使用父容器的可用宽度，这样 justify-content 才能正常工作
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
  ;; (message "render-node:%S" render-node)
  (when render-node
    (let ((display (etaf-render-get-display render-node)))
      (cond
       ((string= display "flex")
        (etaf-layout-flex-format render-node parent-context))
       ((string= display "grid")
        (etaf-layout-grid-format render-node parent-context))
       ((or (string= display "block")
            (null display))
        (message "parent-context:%S" parent-context)
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
