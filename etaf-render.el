;;; etaf-render.el --- Build render tree from DOM and CSSOM -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: render, tree, layout
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 从 DOM 和 CSSOM 构建渲染树 (Render Tree)
;;
;; 渲染树是 DOM 和计算样式的结合，每个渲染节点包含：
;; - node: 对应的 DOM 节点
;; - computed-style: 计算后的样式 ((property . value) ...)
;; - children: 子渲染节点列表
;; - display: 显示类型（从样式中提取，如 "block", "inline", "none"）
;;
;; 渲染树与 DOM 的区别：
;; 1. 不包含不可见元素（display: none, <head>, <script> 等）
;; 2. 每个节点都附加了计算样式
;; 3. 某些元素可能产生多个渲染节点（如 ::before, ::after）
;;
;; 使用示例：
;;
;;   ;; 构建渲染树
;;   (setq render-tree (etaf-render-build-tree dom cssom))
;;
;;   ;; 遍历渲染树
;;   (etaf-render-walk render-tree
;;     (lambda (render-node)
;;       (message "Node: %s, Display: %s"
;;                (plist-get render-node :tag)
;;                (plist-get render-node :display))))
;;
;;   ;; 查询渲染节点的样式
;;   (etaf-render-get-style render-node 'color)

;;; Code:

(require 'cl-lib)
(require 'etaf-dom)
(require 'etaf-css)

;;; 渲染节点结构

(defun etaf-render-create-node (dom-node computed-style)
  "创建渲染节点。
DOM-NODE 是 DOM 节点。
COMPUTED-STYLE 是计算后的样式 alist。
返回渲染节点 plist。"
  (let* ((display (or (cdr (assq 'display computed-style)) "inline"))
         (tag (dom-tag dom-node)))
    (list :node dom-node
          :tag tag
          :computed-style computed-style
          :display display
          :children '())))

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
  (when-let* ((tag (dom-tag node))
              (computed-style (etaf-css-get-computed-style cssom node root-dom)))
    (when (etaf-render-node-visible-p node computed-style)
      (let ((render-node (etaf-render-create-node node computed-style)))
        ;; 递归处理子节点
        (let ((children '()))
          (dolist (child (dom-children node))
            (when (and (consp child) (symbolp (car child)))
              (when-let ((child-render (etaf-render--build-node child cssom root-dom)))
                (push child-render children))))
          ;; 设置子节点（保持原始顺序）
          (plist-put render-node :children (nreverse children)))
        render-node))))

;;; 渲染树查询和遍历

(defun etaf-render-walk (render-tree func)
  "遍历渲染树，对每个节点调用 FUNC。
RENDER-TREE 是渲染树根节点。
FUNC 是接受一个渲染节点参数的函数。"
  (when render-tree
    (funcall func render-tree)
    (dolist (child (plist-get render-tree :children))
      (etaf-render-walk child func))))

(defun etaf-render-get-style (render-node property)
  "从渲染节点获取指定样式属性的值。
RENDER-NODE 是渲染节点。
PROPERTY 是样式属性名（symbol）。
返回属性值字符串或 nil。"
  (cdr (assq property (plist-get render-node :computed-style))))

(defun etaf-render-find-by-tag (render-tree tag)
  "在渲染树中查找指定标签的所有节点。
RENDER-TREE 是渲染树根节点。
TAG 是要查找的标签名（symbol）。
返回匹配的渲染节点列表。"
  (let ((result '()))
    (etaf-render-walk render-tree
      (lambda (node)
        (when (eq (plist-get node :tag) tag)
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
        (when (string= (plist-get node :display) display)
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
        (tag (plist-get render-tree :tag))
        (display (plist-get render-tree :display))
        (style-count (length (plist-get render-tree :computed-style)))
        (children (plist-get render-tree :children)))
    (concat indent-str
            (format "<%s> [display: %s, %d styles]"
                    tag display style-count)
            (when children
              (concat "\n"
                      (mapconcat (lambda (child)
                                  (etaf-render-to-string child (1+ indent)))
                                children "\n"))))))

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
                    (let ((display (plist-get node :display)))
                      (puthash display (1+ (gethash display display-types 0))
                              display-types))
                    (dolist (child (plist-get node :children))
                      (count-nodes child (1+ depth))))))
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
