;;; etaf-render.el --- Build render tree from DOM and CSSOM -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: render, tree, layout, css
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Render Tree Construction for ETAF
;; ==================================
;;
;; This module builds the Render Tree from the DOM and CSSOM. The render tree
;; is an intermediate representation that combines document structure with
;; computed styles, ready for layout computation.
;;
;; Rendering Pipeline Position:
;; ----------------------------
;;
;;   DOM + CSSOM
;;        │
;;        ▼ etaf-render-build-tree
;;   Render Tree (this module)
;;        │
;;        ▼ etaf-layout-build-tree
;;   Layout Tree
;;        │
;;        ▼ etaf-layout-to-string
;;   Rendered String
;;
;; Render Tree vs DOM:
;; -------------------
;; The render tree differs from the DOM in several ways:
;;
;; 1. No invisible elements (display: none, <head>, <script>, etc.)
;; 2. Each node has computed styles (CSS cascade applied)
;; 3. Original DOM attributes not preserved (only computed styles)
;; 4. May have generated content (::before, ::after - future)
;;
;; Render Tree Structure:
;; ----------------------
;; Uses DOM format for compatibility with dom.el functions:
;;
;;   (tag ((computed-style . ((display . "block") (color . "red") ...))
;;         (computed-style-dark . ((color . "white") ...))  ; optional
;;         (etaf-original-attrs . ...)                       ; for interactive elements
;;         (etaf-event-handlers . ...))                      ; preserved handlers
;;     child1 child2 ...)
;;
;; Key Features:
;; -------------
;; - Dual-mode styles: Light and dark mode computed separately
;; - Event handler preservation: Interactive elements keep their handlers
;; - Display type resolution: Default display based on HTML semantics
;;
;; Public API:
;; -----------
;; Construction:
;;   - `etaf-render-build-tree' - Build render tree from DOM + CSSOM
;;
;; Accessors:
;;   - `etaf-render-get-style' - Get a specific style property
;;   - `etaf-render-get-display' - Get display type
;;   - `etaf-render-get-computed-style' - Get full computed style
;;
;; Traversal:
;;   - `etaf-render-walk' - Walk all render nodes
;;   - `etaf-render-find-by-tag' - Find nodes by tag
;;   - `etaf-render-find-by-display' - Find nodes by display type
;;
;; Utilities:
;;   - `etaf-render-to-string' - Pretty-print render tree
;;   - `etaf-render-stats' - Get tree statistics
;;
;; Usage Example:
;; --------------
;;   (let* ((dom (etaf-etml-to-dom '(div :class "box" "Hello")))
;;          (cssom (etaf-css-build-cssom dom))
;;          (render-tree (etaf-render-build-tree dom cssom)))
;;     ;; Query styles
;;     (etaf-render-get-style render-tree 'display)  ;; => "block"
;;     ;; Traverse tree
;;     (etaf-render-walk render-tree
;;       (lambda (node)
;;         (message "Node: %s" (dom-tag node)))))

;;; Code:

(require 'cl-lib)
(require 'etaf-dom)
(require 'etaf-css)
(require 'etaf-vdom)

;;; 渲染节点结构

;; HTML 块级元素列表
(defconst etaf-render-block-level-tags
  '(div p h1 h2 h3 h4 h5 h6 ul ol li dl dt dd
    article aside section nav header footer main
    blockquote pre address figure figcaption
    form fieldset
    hr html body)
  "HTML 块级元素标签列表。这些元素默认 display 为 block。")

;; Table-related element display types
(defconst etaf-render-table-display-map
  '((table . "table")
    (thead . "table-header-group")
    (tbody . "table-row-group")
    (tfoot . "table-footer-group")
    (tr . "table-row")
    (th . "table-cell")
    (td . "table-cell")
    (caption . "table-caption")
    (colgroup . "table-column-group")
    (col . "table-column"))
  "Table-related element to display type mapping.")

(defun etaf-render-get-default-display (tag)
  "根据元素标签返回默认的 display 值。
TAG 是元素标签名（symbol）。
Table elements get their proper display types.
块级元素返回 \"block\"，其他返回 \"inline\"。"
  (or (cdr (assq tag etaf-render-table-display-map))
      (if (memq tag etaf-render-block-level-tags)
          "block"
        "inline")))

(defun etaf-render-create-node (dom-node computed-style &optional computed-style-dark)
  "创建渲染节点（使用 DOM 格式）。
DOM-NODE 是 DOM 节点。
COMPUTED-STYLE 是亮色模式下的计算样式 alist。
COMPUTED-STYLE-DARK 是暗色模式下的计算样式 alist（可选）。
返回 DOM 格式的渲染节点：(tag ((attrs...) children...)
其中 attrs 包含：
- computed-style: 亮色模式计算样式（包含 display 属性）
- computed-style-dark: 暗色模式计算样式（如果与亮色不同）
- etaf-original-attrs: 原始 DOM 属性（仅保留交互元素需要的关键属性）
- etaf-event-handlers: VNode 事件处理器（如果有）
注意：不保留大多数原始 DOM 属性（class、id 等），只保留计算后的样式和交互相关属性"
  (let* ((tag (dom-tag dom-node))
         ;; 从 computed-style 获取 display，如果没有则根据标签类型使用默认值
         (display (or (cdr (assq 'display computed-style))
                      (etaf-render-get-default-display tag)))
         ;; 确保 computed-style 中有 display 属性
         (computed-style-with-display
          (if (assq 'display computed-style)
              computed-style
            (cons (cons 'display display) computed-style)))
         ;; 对于交互元素，保留某些关键的原始属性（如 href, type, value 等）
         ;; Check using VNode metadata approach - tag names that have interactive capability
         (original-attrs (when (memq tag '(a button input textarea summary))
                           (dom-attributes dom-node)))
         ;; Extract event handlers if present in DOM node
         (event-handlers (cdr (assq 'etaf-event-handlers (dom-attributes dom-node))))
         ;; 构建新的属性 alist，只添加渲染信息，不保留大多数原始 DOM 属性
         ;; 只有当暗色样式与亮色样式不同时才添加 computed-style-dark
         (render-attrs
          (if (and computed-style-dark
                   (not (equal computed-style-with-display computed-style-dark)))
              ;; 确保暗色模式样式也包含 display
              (let ((dark-display (or (cdr (assq 'display computed-style-dark)) display)))
                (list (cons 'computed-style computed-style-with-display)
                      (cons 'computed-style-dark 
                            (if (assq 'display computed-style-dark)
                                computed-style-dark
                              (cons (cons 'display dark-display) computed-style-dark)))))
            (list (cons 'computed-style computed-style-with-display)))))
    ;; 如果是交互元素，保留原始属性用于事件处理
    (when original-attrs
      (push (cons 'etaf-original-attrs original-attrs) render-attrs))
    ;; 保留事件处理器（如果有）
    (when event-handlers
      (push (cons 'etaf-event-handlers event-handlers) render-attrs))
    ;; 返回渲染节点
    (list tag render-attrs)))

(defun etaf-render-node-visible-p (dom-node computed-style)
  "判断 DOM 节点是否应该在渲染树中显示。
不显示的元素包括：
- display: none
- <head>, <script>, <style>, <meta>, <link> 等不可见标签"
  (let ((tag (dom-tag dom-node))
        (display (cdr (assq 'display computed-style))))
    (and (not (memq tag '(head script style meta link title)))
         (not (string= display "none")))))

;;; 渲染树构建

(defun etaf-render-build-tree (dom cssom)
  "从 DOM 和 CSSOM 构建渲染树。
DOM 是 DOM 树根节点。
CSSOM 是 CSS 对象模型。
返回渲染树根节点。"
  (etaf-render--build-node dom cssom dom))

(defun etaf-render--build-node (node cssom root-dom)
  "递归构建渲染节点。
NODE 是当前 DOM 节点。
CSSOM 是 CSS 对象模型。
ROOT-DOM 是 DOM 树根节点。
返回渲染节点或 nil（如果节点不可见）。"
  (when-let ((tag (dom-tag node)))
    ;; 获取双模式计算样式
    (let* ((dual-style (etaf-css-get-computed-style-dual-mode cssom node root-dom))
           (computed-style (plist-get dual-style :light))
           (computed-style-dark (plist-get dual-style :dark)))
      ;; computed-style can be nil or empty list, both are acceptable
      (when (etaf-render-node-visible-p node computed-style)
        (let ((render-node (etaf-render-create-node
                            node computed-style computed-style-dark)))
          ;; 递归处理子节点
          (let ((children '()))
            (dolist (child (dom-children node))
              (cond
               ;; 元素节点：递归构建渲染节点
               ((and (consp child) (symbolp (car child)))
                (when-let ((child-render (etaf-render--build-node child cssom root-dom)))
                  (push child-render children)))
               ;; 文本节点：直接保留
               ((stringp child)
                (push child children))))
            ;; 将子节点添加到渲染节点（DOM 格式）
            ;; render-node 是 (tag (attrs...))，需要添加子节点
            (setcdr (cdr render-node) (nreverse children)))
          render-node)))))

;;; 渲染树查询和遍历

(defun etaf-render-walk (render-tree func)
  "遍历渲染树，对每个节点调用 FUNC。
RENDER-TREE 是渲染树根节点。
FUNC 是接受一个渲染节点参数的函数。
注意：可以直接使用 etaf-dom-map 实现相同功能。"
  (etaf-dom-map func render-tree))

(defun etaf-render-get-style (render-node property)
  "从渲染节点获取指定样式属性的值。
RENDER-NODE 是渲染节点。
PROPERTY 是样式属性名（symbol）。
返回属性值字符串或 nil。"
  (cdr (assq property (dom-attr render-node 'computed-style))))

(defun etaf-render-get-display (render-node)
  "从渲染节点获取 display 类型。
RENDER-NODE 是渲染节点。
返回 display 字符串。"
  (or (etaf-render-get-style render-node 'display)
      (etaf-render-get-default-display (dom-tag render-node))))

(defun etaf-render-get-computed-style (render-node)
  "从渲染节点获取完整的计算样式 alist。
RENDER-NODE 是渲染节点。
返回计算样式 alist。"
  (dom-attr render-node 'computed-style))

(defun etaf-render-find-by-tag (render-tree tag)
  "在渲染树中查找指定标签的所有节点。
RENDER-TREE 是渲染树根节点。
TAG 是要查找的标签名（symbol）。
返回匹配的渲染节点列表。"
  (let ((result '()))
    (etaf-render-walk render-tree
      (lambda (node)
        (when (eq (dom-tag node) tag)
          (push node result))))
    (nreverse result)))

(defun etaf-render-find-by-display (render-tree display)
  "在渲染树中查找指定 display 类型的所有节点。
RENDER-TREE 是渲染树根节点。
DISPLAY 是显示类型字符串（如 \"block\", \"inline\"）。
返回匹配的渲染节点列表。"
  (let ((result '()))
    (etaf-render-walk render-tree
      (lambda (node)
        (when (string= (etaf-render-get-display node) display)
          (push node result))))
    (nreverse result)))

;;; 渲染树转换和输出

(defun etaf-render-to-string (render-tree &optional indent)
  "将渲染树转换为可读的字符串形式。
RENDER-TREE 是渲染树根节点。
INDENT 是缩进级别（可选）。
返回格式化的字符串。"
  (setq indent (or indent 0))
  (let ((indent-str (make-string (* indent 2) ?\s))
        (tag (dom-tag render-tree))
        (display (etaf-render-get-display render-tree))
        (style-count (length (etaf-render-get-computed-style render-tree)))
        (children (dom-children render-tree)))
    ;; 过滤出元素子节点（排除文本节点）
    (let ((element-children (seq-filter #'listp children)))
      (concat indent-str
              (format "<%s> [display: %s, %d styles]"
                      tag display style-count)
              (when element-children
                (concat "\n"
                        (mapconcat (lambda (child)
                                    (etaf-render-to-string child (1+ indent)))
                                  element-children "\n")))))))

(defun etaf-render-stats (render-tree)
  "计算渲染树的统计信息。
RENDER-TREE 是渲染树根节点。
返回 plist 包含：
- :node-count 节点总数
- :max-depth 最大深度
- :display-types display 类型的分布 alist"
  (let ((node-count 0)
        (max-depth 0)
        (display-types (make-hash-table :test 'equal)))
    (cl-labels ((count-nodes (node depth)
                  (when node
                    (cl-incf node-count)
                    (setq max-depth (max max-depth depth))
                    (let ((display (etaf-render-get-display node)))
                      (puthash display (1+ (gethash display display-types 0))
                              display-types))
                    ;; 遍历子节点（过滤出元素子节点）
                    (dolist (child (dom-children node))
                      (when (listp child)
                        (count-nodes child (1+ depth)))))))
      (count-nodes render-tree 0))
    ;; 转换 hash-table 为 alist
    (let ((display-alist '()))
      (maphash (lambda (k v) (push (cons k v) display-alist))
              display-types)
      (list :node-count node-count
            :max-depth max-depth
            :display-types (nreverse display-alist)))))

(provide 'etaf-render)
;;; etaf-render.el ends here
