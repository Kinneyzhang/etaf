;;; etaf-css-core.el --- Core CSS Systems -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, cascade, inheritance, cache, index
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Core CSS systems merged from multiple modules:
;; - Cascade algorithm and specificity calculation (from etaf-css-cascade.el)
;; - Property inheritance (from etaf-css-inheritance.el)
;; - Computed style caching (from etaf-css-cache.el)
;; - Rule indexing for performance (from etaf-css-index.el)
;;
;; This consolidates related CSS core functionality into a single module
;; for better maintainability and reduced file fragmentation.

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; CSS Cascade Algorithm and Specificity
;;; ============================================================

(defun etaf-css-calculate-specificity (selector)
  "计算 CSS 选择器的特异性。
返回 (id-count class-count type-count) 格式的列表。

SELECTOR 可以是字符串或包含 :selector 属性的 plist。

例如:
  'div'          => (0 0 1)
  '.button'      => (0 1 0)
  '#main'        => (1 0 0)
  'div.button'   => (0 1 1)
  '#main .text'  => (1 1 0)"
  (let ((id-count 0)
        (class-count 0)
        (type-count 0)
        (selector-str (if (stringp selector)
                          selector
                        (plist-get selector :selector))))
    ;; 计数 ID 选择器 (#id)
    (let ((pos 0))
      (while (string-match "#[a-zA-Z][a-zA-Z0-9_-]*" selector-str pos)
        (cl-incf id-count)
        (setq pos (match-end 0))))
    
    ;; 计数类选择器 (.class)
    (let ((pos 0))
      (while (string-match "\\.[a-zA-Z][a-zA-Z0-9_-]*" selector-str pos)
        (cl-incf class-count)
        (setq pos (match-end 0))))
    
    ;; 计数属性选择器 ([attr])
    (let ((pos 0))
      (while (string-match "\\[[^]]+\\]" selector-str pos)
        (cl-incf class-count)
        (setq pos (match-end 0))))
    
    ;; 计数伪类选择器 (:hover, :not, etc.)  
    (let ((pos 0))
      (while (string-match ":[a-zA-Z][a-zA-Z0-9_-]*" selector-str pos)
        (let ((pseudo (match-string 0 selector-str)))
          ;; :not 不计入，但其内容计入
          (unless (string= pseudo ":not")
            (cl-incf class-count))
          (setq pos (match-end 0)))))
    
    ;; 计数类型选择器（标签名）
    ;; 策略：移除所有 ID、类、属性、伪类后，剩余的字母开头的词就是标签
    (let ((cleaned selector-str))
      ;; 移除 ID
      (setq cleaned (replace-regexp-in-string "#[a-zA-Z][a-zA-Z0-9_-]*" "" cleaned))
      ;; 移除类
      (setq cleaned (replace-regexp-in-string "\\.[a-zA-Z][a-zA-Z0-9_-]*" "" cleaned))
      ;; 移除属性
      (setq cleaned (replace-regexp-in-string "\\[[^]]+\\]" "" cleaned))
      ;; 移除伪类和伪元素
      (setq cleaned (replace-regexp-in-string ":[a-zA-Z][a-zA-Z0-9_-]*" "" cleaned))
      ;; 现在计数剩余的标签（字母开头的词）
      (let ((pos 0))
        (while (string-match "\\<[a-z][a-z0-9]*\\>" cleaned pos)
          (cl-incf type-count)
          (setq pos (match-end 0)))))
    
    (list id-count class-count type-count)))

(defun etaf-css-specificity> (spec1 spec2)
  "比较两个特异性，如果 spec1 > spec2 返回 t。
特异性格式: (id-count class-count type-count)。
比较规则：首先比较 ID 数，然后类数，最后标签数。"
  (or (> (nth 0 spec1) (nth 0 spec2))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (> (nth 1 spec1) (nth 1 spec2)))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (= (nth 1 spec1) (nth 1 spec2))
           (> (nth 2 spec1) (nth 2 spec2)))))

(defun etaf-css-specificity= (spec1 spec2)
  "检查两个特异性是否相等。
特异性格式: (id-count class-count type-count)。"
  (and (= (nth 0 spec1) (nth 0 spec2))
       (= (nth 1 spec1) (nth 1 spec2))
       (= (nth 2 spec1) (nth 2 spec2))))

(defun etaf-css-cascade-compare-declarations (decl1 decl2)
  "比较两个声明的优先级。
如果 decl1 优先级高于 decl2，返回 t。

每个声明格式: (value specificity source important order)
- value: 属性值
- specificity: 选择器特异性 (id class type)
- source: 来源 (ua, style-tag, 或 inline)
- important: 是否为 !important
- order: 文档顺序（可选，用于同等情况下后定义优先）

层叠规则：
1. !important 声明优先于普通声明
2. 内联样式优先于样式表
3. 高特异性优先于低特异性
4. 同等特异性下，author 样式表优先于 UA 样式表
5. 同等情况下，后定义优先"
  (let ((value1 (nth 0 decl1))
        (spec1 (nth 1 decl1))
        (source1 (nth 2 decl1))
        (important1 (nth 3 decl1))
        (order1 (or (nth 4 decl1) 0))
        (value2 (nth 0 decl2))
        (spec2 (nth 1 decl2))
        (source2 (nth 2 decl2))
        (important2 (nth 3 decl2))
        (order2 (or (nth 4 decl2) 0)))
    
    (cond
     ;; 1. !important 优先
     ((and important1 (not important2)) t)
     ((and important2 (not important1)) nil)
     
     ;; 2. inline 样式优先（在同等 !important 级别下）
     ((and (eq source1 'inline) (not (eq source2 'inline))) t)
     ((and (eq source2 'inline) (not (eq source1 'inline))) nil)
     
     ;; 3. 比较特异性
     ((etaf-css-specificity> spec1 spec2) t)
     ((etaf-css-specificity> spec2 spec1) nil)
     
     ;; 4. 特异性相同时，author 样式优先于 UA 样式
     ((and (eq source1 'style-tag) (eq source2 'ua)) t)
     ((and (eq source2 'style-tag) (eq source1 'ua)) nil)
     
     ;; 5. 特异性和来源都相同，比较文档顺序（后定义优先）
     (t (> order1 order2)))))

(defun etaf-css-cascade-apply (declarations-by-property)
  "应用层叠算法到声明集合。
DECLARATIONS-BY-PROPERTY 是一个哈希表，键是属性名，值是声明列表。
每个声明格式: (value specificity source important order)。
返回 ((property . value) ...) 格式的最终样式。"
  (let ((result '()))
    (maphash
     (lambda (prop decl-list)
       ;; 找到优先级最高的声明
       (let ((winner (car decl-list)))
         (dolist (decl (cdr decl-list))
           (when (etaf-css-cascade-compare-declarations decl winner)
             (setq winner decl)))
         ;; 添加到结果（只保留属性和值）
         (push (cons prop (car winner)) result)))
     declarations-by-property)
    result))

(defun etaf-css-cascade-merge-rules (rules)
  "合并多个规则的声明，应用层叠算法。
RULES 是规则列表，每个规则包含 :declarations :specificity :source。
返回 ((property . value) ...) 格式的最终样式。"
  (let ((property-decls (make-hash-table :test 'eq))
        (order 0))
    ;; 收集所有声明
    (dolist (rule rules)
      (let ((declarations (plist-get rule :declarations))
            (specificity (plist-get rule :specificity))
            (source (plist-get rule :source)))
        (dolist (decl declarations)
          (let* ((prop (nth 0 decl))
                 (value (nth 1 decl))
                 (important (nth 2 decl))
                 ;; 构建完整的声明信息
                 (full-decl (list value specificity source important order)))
            (push full-decl (gethash prop property-decls))
            (cl-incf order)))))
    
    ;; 应用层叠算法
    (etaf-css-cascade-apply property-decls)))

;;; ============================================================
;;; CSS Property Inheritance
;;; ============================================================

(defconst etaf-css-inherited-properties
  '(color
    font-family
    font-size
    font-style
    font-variant
    font-weight
    font
    letter-spacing
    line-height
    list-style
    list-style-image
    list-style-position
    list-style-type
    text-align
    text-indent
    text-transform
    visibility
    white-space
    word-spacing
    cursor
    direction
    quotes)
  "会自动继承的 CSS 属性列表。")

(defun etaf-css-property-inherits-p (property)
  "检查指定的 CSS 属性是否会自动继承。
PROPERTY 是属性名（symbol）。"
  (memq property etaf-css-inherited-properties))

(defun etaf-css--remove-duplicate-properties (style-alist)
  "从样式 alist 中移除重复的属性，只保留每个属性的第一次出现。
STYLE-ALIST 是 ((property . value) ...) 格式的样式 alist。
返回一个新的 alist，其中每个属性只出现一次。

这个函数确保在继承过程中不会累积重复的属性条目。
例如：'((color . \"red\") (font-weight . \"bold\") (color . \"blue\"))
      => '((color . \"red\") (font-weight . \"bold\"))"
  (let ((seen (make-hash-table :test 'eq))
        (result '()))
    (dolist (prop style-alist)
      (let ((key (car prop)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push prop result))))
    (nreverse result)))

(defun etaf-css-apply-inheritance (computed-style parent-style)
  "将父元素的可继承属性应用到子元素。
COMPUTED-STYLE 是子元素的计算样式。
PARENT-STYLE 是父元素的计算样式。
返回合并后的样式，确保没有重复的属性条目。

修复问题：在父元素上设置的文本相关样式（如字体、加粗等）在继承时
可能会导致子元素中有多个重复的样式。此函数现在会移除所有重复的
属性条目，只保留每个属性的第一次出现。"
  (let ((result (copy-sequence computed-style)))
    ;; 对于每个可继承的属性，如果子元素没有定义，则从父元素继承
    (dolist (prop etaf-css-inherited-properties)
      (when-let ((parent-value (cdr (assq prop parent-style))))
        (unless (assq prop result)
          (push (cons prop parent-value) result))))
    ;; 移除重复的属性条目
    ;; 这确保了即使 computed-style 或继承过程中引入了重复，
    ;; 最终结果也不会包含重复的属性
    (etaf-css--remove-duplicate-properties result)))

;;; ============================================================
;;; CSS Computed Style Cache
;;; ============================================================

(defun etaf-css-cache-create ()
  "创建一个新的计算样式缓存。
返回一个哈希表，用于存储节点到样式的映射。"
  (make-hash-table :test 'eq :size 100))

(defun etaf-css-cache-get (cache node)
  "从缓存中获取节点的计算样式。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。
返回缓存的样式，如果不存在则返回 nil。"
  (gethash node cache))

(defun etaf-css-cache-set (cache node style)
  "将节点的计算样式存入缓存。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。
STYLE 是计算后的样式。"
  (puthash node style cache))

(defun etaf-css-cache-clear (cache)
  "清空缓存中的所有数据。
CACHE 是缓存哈希表。"
  (clrhash cache))

(defun etaf-css-cache-remove (cache node)
  "从缓存中移除指定节点的样式。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。"
  (remhash node cache))

(defun etaf-css-cache-size (cache)
  "获取缓存中的条目数量。
CACHE 是缓存哈希表。"
  (hash-table-count cache))

;;; ============================================================
;;; CSS Rule Indexing
;;; ============================================================

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
返回候选规则列表（可能包含重复）。"
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
    (cl-delete-duplicates candidates :test 'eq)))

(provide 'etaf-css-core)
;;; etaf-css-core.el ends here
