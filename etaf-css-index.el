;;; etaf-css-index.el --- CSS Rule Indexing for Performance -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, index, performance
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 规则索引系统
;;
;; 为了提高查询性能，按选择器类型索引 CSS 规则。
;; 支持按标签、类、ID 快速查找候选规则。
;;
;; 使用示例：
;;
;;   ;; 构建索引
;;   (setq index (etaf-css-index-build rules))
;;   
;;   ;; 查询候选规则
;;   (etaf-css-index-query index node)

;;; Code:

(require 'cl-lib)

(defun etaf-css-index-create ()
  "创建一个新的规则索引结构。
返回包含 by-tag, by-class, by-id 的 plist。"
  (list :by-tag (make-hash-table :test 'eq)
        :by-class (make-hash-table :test 'equal)
        :by-id (make-hash-table :test 'equal)))

(defun etaf-css-index-extract-selector-keys (selector)
  "从选择器中提取索引键（标签、类、ID）。
SELECTOR 是 CSS 选择器字符串。
返回 (:tags (...) :classes (...) :ids (...))。"
  (let ((tags '())
        (classes '())
        (ids '()))
    ;; 提取 ID
    (let ((pos 0))
      (while (string-match "#\\([a-zA-Z][a-zA-Z0-9_-]*\\)" selector pos)
        (push (match-string 1 selector) ids)
        (setq pos (match-end 0))))
    
    ;; 提取类
    (let ((pos 0))
      (while (string-match "\\.\\([a-zA-Z][a-zA-Z0-9_-]*\\)" selector pos)
        (push (match-string 1 selector) classes)
        (setq pos (match-end 0))))
    
    ;; 提取标签
    ;; 简化：查找选择器开头的标签或空格后的标签
    (let ((pos 0))
      (while (string-match "\\(?:^\\|[ >+~]\\)\\([a-z][a-z0-9]*\\)" selector pos)
        (push (intern (match-string 1 selector)) tags)
        (setq pos (match-end 0))))
    
    (list :tags (nreverse tags)
          :classes (nreverse classes)
          :ids (nreverse ids))))

(defun etaf-css-index-add-rule (index rule)
  "将规则添加到索引中。
INDEX 是规则索引结构。
RULE 是要添加的 CSS 规则。"
  (let* ((selector (plist-get rule :selector))
         (keys (etaf-css-index-extract-selector-keys selector))
         (by-tag (plist-get index :by-tag))
         (by-class (plist-get index :by-class))
         (by-id (plist-get index :by-id)))
    
    ;; 按标签索引
    (dolist (tag (plist-get keys :tags))
      (let ((existing (gethash tag by-tag)))
        (puthash tag (cons rule existing) by-tag)))
    
    ;; 按类索引
    (dolist (class (plist-get keys :classes))
      (let ((existing (gethash class by-class)))
        (puthash class (cons rule existing) by-class)))
    
    ;; 按 ID 索引
    (dolist (id (plist-get keys :ids))
      (let ((existing (gethash id by-id)))
        (puthash id (cons rule existing) by-id)))))

(defun etaf-css-index-build (rules)
  "从规则列表构建索引。
RULES 是 CSS 规则列表。
返回规则索引结构。"
  (let ((index (etaf-css-index-create)))
    (dolist (rule rules)
      (etaf-css-index-add-rule index rule))
    index))

(defun etaf-css-index-query-candidates (index node)
  "从索引中查询可能匹配节点的候选规则。
INDEX 是规则索引结构。
NODE 是 DOM 节点。
返回候选规则列表（按文档顺序排列，可能包含重复）。"
  (let ((candidates '())
        (by-tag (plist-get index :by-tag))
        (by-class (plist-get index :by-class))
        (by-id (plist-get index :by-id))
        (tag (dom-tag node))
        (classes (when-let ((class-attr (dom-attr node 'class)))
                   (split-string class-attr)))
        (id (dom-attr node 'id)))
    
    ;; 按标签查找
    (when tag
      (when-let ((tag-rules (gethash tag by-tag)))
        (setq candidates (append candidates tag-rules))))
    
    ;; 按类查找
    (dolist (class classes)
      (when-let ((class-rules (gethash class by-class)))
        (setq candidates (append candidates class-rules))))
    
    ;; 按 ID 查找
    (when id
      (when-let ((id-rules (gethash id by-id)))
        (setq candidates (append candidates id-rules))))
    
    ;; 去重 - 使用 eq 测试来按对象身份去重，而不是按内容去重
    ;; 这对于内联样式很重要，因为不同节点可能有相同内容的样式规则
    (let ((unique-candidates (cl-delete-duplicates candidates :test 'eq)))
      ;; 反转列表以恢复文档顺序（因为索引添加时使用了 cons 导致了反转）
      (nreverse unique-candidates))))

(provide 'etaf-css-index)
;;; etaf-css-index.el ends here
