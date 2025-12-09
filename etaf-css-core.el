;;; etaf-css-core.el --- Core CSS Systems -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: css, cascade, inheritance, cache, index
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Core CSS Systems for ETAF
;; =========================
;;
;; This module implements the fundamental CSS algorithms that power ETAF's
;; styling system, following the CSS specification closely.
;;
;; Module Structure:
;; -----------------
;; 1. Specificity Calculation - Determine selector weight
;; 2. Cascade Algorithm - Resolve conflicting declarations
;; 3. Property Inheritance - Apply inherited properties to children
;; 4. Style Caching - Performance optimization for computed styles
;; 5. Rule Indexing - Fast rule lookup by selector components
;;
;; CSS Cascade Priority (lowest to highest):
;; -----------------------------------------
;; 1. User Agent Stylesheet (browser defaults)
;; 2. Author Stylesheet (<style> tags, external CSS)
;; 3. Inline Styles (style attribute)
;; 4. Important Author Styles (!important in author CSS)
;; 5. Important Inline Styles (!important in style attribute)
;;
;; Within each origin, specificity determines priority:
;; - ID selectors (e.g., #main) = weight 100
;; - Class/attribute/pseudo-class selectors = weight 10
;; - Type/pseudo-element selectors = weight 1
;;
;; Related Modules:
;; ----------------
;; - etaf-css-selector.el: Selector parsing and matching
;; - etaf-css-parser.el: CSS declaration and stylesheet parsing
;; - etaf-css-face.el: CSS to Emacs face conversion
;; - etaf-type.el: Type definitions (etaf-css-rule, etaf-cssom)

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Section 1: Specificity Calculation
;;; ============================================================================
;;
;; Specificity determines which CSS rule wins when multiple rules match the
;; same element. It's calculated based on the selector components:
;;
;;   Specificity = (ID-count, Class-count, Type-count)
;;
;; Example:
;;   "div"           => (0, 0, 1)
;;   ".button"       => (0, 1, 0)
;;   "#main"         => (1, 0, 0)
;;   "div.box#main"  => (1, 1, 1)
;;   "#a .b .c"      => (1, 2, 0)

(defun etaf-css-calculate-specificity (selector)
  "Calculate CSS specificity of SELECTOR.

SELECTOR can be:
  - A CSS selector string (e.g., \"div.container #main\")
  - A plist with :selector property

Returns a list (ID-COUNT CLASS-COUNT TYPE-COUNT) where:
  - ID-COUNT: Number of #id selectors
  - CLASS-COUNT: Number of .class, [attr], and :pseudo-class selectors
  - TYPE-COUNT: Number of element type selectors (div, p, etc.)

The :not() pseudo-class itself doesn't count, but its contents do.

Examples:
  (etaf-css-calculate-specificity \"div\")          => (0 0 1)
  (etaf-css-calculate-specificity \".button\")      => (0 1 0)
  (etaf-css-calculate-specificity \"#main\")        => (1 0 0)
  (etaf-css-calculate-specificity \"div.button\")   => (0 1 1)
  (etaf-css-calculate-specificity \"#main .text\")  => (1 1 0)"
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
  "Return t if SPEC1 has higher specificity than SPEC2.
Specificity format: (ID-COUNT CLASS-COUNT TYPE-COUNT).

Comparison is done left-to-right:
  1. Compare ID counts
  2. If equal, compare class counts
  3. If still equal, compare type counts

Example: (1 0 0) > (0 99 99) because ID weight beats any class/type count."
  (or (> (nth 0 spec1) (nth 0 spec2))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (> (nth 1 spec1) (nth 1 spec2)))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (= (nth 1 spec1) (nth 1 spec2))
           (> (nth 2 spec1) (nth 2 spec2)))))

(defun etaf-css-specificity= (spec1 spec2)
  "Return t if SPEC1 and SPEC2 have equal specificity."
  (and (= (nth 0 spec1) (nth 0 spec2))
       (= (nth 1 spec1) (nth 1 spec2))
       (= (nth 2 spec1) (nth 2 spec2))))

;;; ============================================================================
;;; Section 2: CSS Cascade Algorithm
;;; ============================================================================
;;
;; The cascade algorithm determines which CSS declaration wins when multiple
;; rules set the same property on an element.
;;
;; Priority order (from lowest to highest):
;;   1. Normal declarations (sorted by specificity and document order)
;;   2. Inline style normal declarations
;;   3. !important author declarations
;;   4. !important inline declarations
;;
;; Within each category, higher specificity wins.
;; When specificity is equal, later declarations win (document order).

(defun etaf-css-cascade-compare-declarations (decl1 decl2)
  "Compare two declarations and return t if DECL1 has higher priority.

Each declaration format: (VALUE SPECIFICITY SOURCE IMPORTANT ORDER)
  - VALUE: The property value string
  - SPECIFICITY: List (id class type)
  - SOURCE: Symbol 'ua, 'style-tag, or 'inline
  - IMPORTANT: Boolean for !important flag
  - ORDER: Integer document order (higher = later in document)

Comparison rules (in order):
  1. !important beats normal
  2. Inline beats stylesheet at same importance level
  3. Higher specificity beats lower
  4. Author stylesheet beats UA stylesheet at same specificity
  5. Later declaration beats earlier (document order)"
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
  "Apply cascade algorithm to grouped declarations.

DECLARATIONS-BY-PROPERTY is a hash-table where:
  - Keys: property name symbols (e.g., 'color)
  - Values: lists of declarations for that property

Each declaration format: (VALUE SPECIFICITY SOURCE IMPORTANT ORDER)

Returns alist ((PROPERTY . VALUE) ...) with winning values."
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
  "Merge declarations from RULES using the CSS cascade algorithm.

RULES is a list of CSS rule plists, each containing:
  - :declarations - List of (PROPERTY VALUE IMPORTANT) tuples
  - :specificity - Selector specificity as (id class type)
  - :source - Rule origin ('ua, 'style-tag, 'inline)

Returns alist ((PROPERTY . VALUE) ...) with computed values.

This function collects all declarations, groups by property,
then applies cascade rules to determine the final value."
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

;;; ============================================================================
;;; Section 3: CSS Property Inheritance
;;; ============================================================================
;;
;; Some CSS properties are inherited from parent to child elements.
;; For example, 'color' and 'font-family' are inherited, while
;; 'border' and 'margin' are not.
;;
;; When a child element doesn't explicitly set an inherited property,
;; it receives the computed value from its parent.

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
  "List of CSS properties that are automatically inherited.
These properties pass their values from parent to child elements
unless the child explicitly sets a different value.")

(defun etaf-css-property-inherits-p (property)
  "Return non-nil if PROPERTY is inherited from parent elements.
PROPERTY should be a symbol like 'color or 'font-size."
  (memq property etaf-css-inherited-properties))

(defun etaf-css--remove-duplicate-properties (style-alist)
  "Remove duplicate properties from STYLE-ALIST, keeping first occurrence.

STYLE-ALIST is ((PROPERTY . VALUE) ...).
Returns a new alist with unique properties.

Example:
  ((color . \"red\") (font-weight . \"bold\") (color . \"blue\"))
  => ((color . \"red\") (font-weight . \"bold\"))"
  (let ((seen (make-hash-table :test 'eq))
        (result '()))
    (dolist (prop style-alist)
      (let ((key (car prop)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push prop result))))
    (nreverse result)))

(defun etaf-css-apply-inheritance (computed-style parent-style)
  "Apply inherited properties from PARENT-STYLE to COMPUTED-STYLE.

For each property in `etaf-css-inherited-properties':
  - If the child doesn't have it, copy from parent
  - If the child already has it, keep the child's value

Returns the merged style alist with duplicates removed.

COMPUTED-STYLE is the child element's computed style alist.
PARENT-STYLE is the parent element's computed style alist."
  (let ((result (copy-sequence computed-style)))
    ;; 对于每个可继承的属性，如果子元素没有定义，则从父元素继承
    (dolist (prop etaf-css-inherited-properties)
      (when-let ((parent-value (cdr (assq prop parent-style))))
        (unless (assq prop result)
          (push (cons prop parent-value) result))))
    ;; 移除重复的属性条目
    (etaf-css--remove-duplicate-properties result)))

;;; ============================================================================
;;; Section 4: Computed Style Cache
;;; ============================================================================
;;
;; Caching computed styles significantly improves performance when
;; the same elements are queried multiple times during rendering.

(defun etaf-css-cache-create ()
  "Create a new computed style cache.
Returns a hash-table for storing node-to-style mappings.
Initial size is 100 entries."
  (make-hash-table :test 'eq :size 100))

(defun etaf-css-cache-get (cache node)
  "Get cached computed style for NODE from CACHE.
Returns the cached style alist, or nil if not cached."
  (gethash node cache))

(defun etaf-css-cache-set (cache node style)
  "Store STYLE as the computed style for NODE in CACHE."
  (puthash node style cache))

(defun etaf-css-cache-clear (cache)
  "Clear all entries from CACHE.
Call this when DOM or styles change to invalidate cached values."
  (clrhash cache))

(defun etaf-css-cache-remove (cache node)
  "Remove the cached style for NODE from CACHE."
  (remhash node cache))

(defun etaf-css-cache-size (cache)
  "Return the number of entries in CACHE."
  (hash-table-count cache))

;;; ============================================================================
;;; Section 5: CSS Rule Indexing
;;; ============================================================================
;;
;; Rule indexing dramatically improves style computation performance by
;; organizing rules for fast lookup. Instead of checking every rule against
;; every element, we can quickly find candidate rules that might match.
;;
;; Index structure:
;;   (:by-tag   <hash-table>   ; tag-symbol -> list of rules
;;    :by-class <hash-table>   ; class-string -> list of rules
;;    :by-id    <hash-table>)  ; id-string -> list of rules

(defun etaf-css-index-create ()
  "Create a new CSS rule index structure.
Returns a plist with :by-tag, :by-class, :by-id hash-tables."
  (list :by-tag (make-hash-table :test 'eq)
        :by-class (make-hash-table :test 'equal)
        :by-id (make-hash-table :test 'equal)))

(defun etaf-css-index-extract-selector-keys (selector)
  "Extract indexable keys from SELECTOR string.

Returns a plist with:
  :tags - List of tag symbols found in selector
  :classes - List of class name strings
  :ids - List of ID strings

Example:
  (etaf-css-index-extract-selector-keys \"div.container #main\")
  => (:tags (div) :classes (\"container\") :ids (\"main\"))"
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
  "Add RULE to INDEX for fast lookup.

The rule is indexed by all tags, classes, and IDs found in its
selector. Later, when computing styles for an element, we can
quickly find candidate rules by looking up the element's tag,
classes, and ID."
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
  "Build a rule index from RULES list.
Returns an index structure for fast rule lookup."
  (let ((index (etaf-css-index-create)))
    (dolist (rule rules)
      (etaf-css-index-add-rule index rule))
    index))

(defun etaf-css-index-query-candidates (index node)
  "Query INDEX for rules that might match NODE.

Returns a list of candidate rules (may contain duplicates).
The caller must still verify each rule actually matches the node
using selector matching.

This provides a significant performance improvement over checking
every rule against every node."
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
