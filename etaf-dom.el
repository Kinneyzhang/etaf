;;; etaf-dom.el ---- dom 相关操作

;; Copyright (C) 2024

;; Author: Based on postcss-selector-parser
;; Keywords: css, dom, selector, query
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这个库结合css-selector-parser.el和dom.el，实现了类似浏览器querySelector的功能。
;; 可以使用CSS选择器在DOM树中查询节点，并为匹配的节点应用CSS样式。
;;
;; 主要功能：
;; - 使用CSS选择器在DOM树中查询节点
;; - 支持标签、类、ID、属性选择器
;; - 支持后代、子元素、相邻兄弟、通用兄弟组合器
;; - 支持基本伪类选择器
;; - 为匹配的DOM节点应用CSS属性
;;
;; 使用示例：
;;
;;   ;; 查询单个节点
;;   (ecss-dom-query-selector dom "div.header")
;;
;;   ;; 查询所有匹配节点
;;   (ecss-dom-query-selector-all dom "p.text")
;;
;;   ;; 应用CSS样式
;;   (ecss-dom-apply-style dom ".button" '((color . "red") (font-size . "14px")))

;;; Code:

(require 'cl-lib)
(require 'dom)

;;; Utility functions

(defun etaf-alist-to-plist (alist)
  "Convert an alist to a plist.
Example: ((class . \"foo\") (id . \"bar\"))
         => (:class \"foo\" :id \"bar\")"
  (let ((result nil))
    (dolist (pair alist)
      (push (intern (concat ":" (symbol-name (car pair)))) result)
      (push (cdr pair) result))
    (nreverse result)))

;;; DOM to TML conversion

(defun etaf-dom-to-tml (sexp)
  "Convert S-expression from format 2 (alist) to format 1 (plist).
Format 2: (tag ((attr1 . val1) (attr2 . val2)) child1 child2 ...)
Format 1: (tag :attr1 val1 :attr2 val2 child1 child2 ...)"
  (cond
   ((atom sexp) sexp)
   (t (let* ((tag (car sexp))
             (rest (cdr sexp))
             (attr-alist (car rest))
             (children (cdr rest)))
        (let ((attr-plist (when (listp attr-alist)
                            (etaf-alist-to-plist attr-alist)))
              (processed-children
               (mapcar #'etaf-dom-to-tml children)))
          (if attr-plist
              (append (list tag) attr-plist processed-children)
            (cons tag processed-children)))))))

;;; DOM节点匹配函数

(defun  etaf-dom-tag-match-p (node tag-name)
  "检查DOM节点NODE是否匹配标签选择器TAG-NAME。"
  (when (and node (listp node))
    (let ((node-tag (symbol-name (dom-tag node))))
      (or (string= tag-name "*")  ; 通配符
          (string= tag-name node-tag)))))

(defun etaf-dom-class-match-p (node class-name)
  "检查DOM节点NODE是否匹配类选择器CLASS-NAME。"
  (when (and node (listp node))
    (when-let ((class-attr (dom-attr node 'class)))
      (let ((classes (split-string class-attr)))
        (member class-name classes)))))

(defun etaf-dom-id-match-p (node id-name)
  "检查DOM节点NODE是否匹配ID选择器ID-NAME。"
  (when (and node (listp node))
    (when-let ((id-attr (dom-attr node 'id)))
      (string= id-attr id-name))))

(defun etaf-dom-map (func dom)
  "遍历DOM树的所有节点，对每个节点调用FUNC。
DOM是要遍历的DOM节点，FUNC是对每个节点调用的函数。"
  (when (and dom (listp dom))
    (funcall func dom)
    (let ((children (dom-children dom)))
      (dolist (child children)
        (when (listp child)  ; 跳过文本节点
          (etaf-dom-map func child))))))

(defun etaf-dom-get-previous-sibling (node dom)
  "获取节点NODE的前一个兄弟元素节点（跳过文本节点）。
返回前一个兄弟节点，如果没有则返回nil。"
  ;; 首先找到包含node的父节点
  (when-let ((parent (dom-parent dom node)))
    ;; 获取node的前一个非文本兄弟节点
    (let ((children (dom-children parent))
          (prev-sibling nil)
          (found-node nil))
      (dolist (child children)
        (cond
         ((eq child node)
          (setq found-node t))
         ((and (not found-node) (listp child))
          ;; 这是node之前的一个元素节点
          (setq prev-sibling child))))
      prev-sibling)))

(defun etaf-dom-get-previous-siblings (node dom)
  "获取节点NODE之前的所有兄弟元素节点（跳过文本节点）。
返回兄弟节点列表，按文档顺序（最早的在前）。"
  ;; 首先找到包含node的父节点
  (when-let ((parent (dom-parent dom node)))
    ;; 获取node之前的所有非文本兄弟节点
    (let ((children (dom-children parent))
          (prev-siblings '())
          (found-node nil))
      (dolist (child children)
        (cond
         ((eq child node)
          (setq found-node t))
         ((and (not found-node) (listp child))
          ;; 这是node之前的一个元素节点
          (push child prev-siblings))))
      (nreverse prev-siblings))))

(defun etaf-dom-is-descendant-of (node ancestor)
  "检查 NODE 是否是 ANCESTOR 的后代。"
  (and (not (eq node ancestor))
       (not (null (dom-search
                   ancestor
                   (lambda (candidate)
                     (eq candidate node)))))))

(defun etaf-dom-get-parent (node dom)
  "获取节点NODE的父节点，返回父节点，如果没有则返回 nil。"
  (dom-parent dom node))

(defun etaf-dom-get-element-children (node)
  "获取节点NODE的所有元素子节点（跳过文本节点），返回子节点列表。"
  (when (and node (listp node))
    (dom-non-text-children node)))

(defvar-local etaf-dom--query-root nil
  "Dynamic variable holding the root DOM for the current query operation.")

(defun etaf-dom-is-first-child (node)
  "检查节点是否是其父节点的第一个子元素。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let ((element-children (etaf-dom-get-element-children parent)))
          (and element-children
               (eq node (car element-children))))))))

(defun etaf-dom-is-last-child (node)
  "检查节点是否是其父节点的最后一个子元素。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let ((element-children (etaf-dom-get-element-children parent)))
          (and element-children
               (eq node (car (last element-children)))))))))

(defun etaf-dom-get-child-index (node)
  "获取节点在其父节点中的索引位置（从1开始）。
只计算元素节点，跳过文本节点。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let ((element-children (etaf-dom-get-element-children parent))
              (index 0))
          (catch 'found
            (dolist (child element-children)
              (cl-incf index)
              (when (eq child node)
                (throw 'found index)))
            nil))))))

(defun etaf-dom-is-first-of-type (node)
  "检查节点是否是其父节点中该类型的第一个子元素。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let* ((node-tag (dom-tag node))
               (element-children (etaf-dom-get-element-children parent))
               (same-type-children
                (cl-remove-if-not
                 (lambda (child) (eq (dom-tag child) node-tag))
                 element-children)))
          (and same-type-children
               (eq node (car same-type-children))))))))

(defun etaf-dom-is-last-of-type (node)
  "检查节点是否是其父节点中该类型的最后一个子元素。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let* ((node-tag (dom-tag node))
               (element-children (etaf-dom-get-element-children parent))
               (same-type-children
                (cl-remove-if-not
                 (lambda (child) (eq (dom-tag child) node-tag))
                 element-children)))
          (and same-type-children
               (eq node (car (last same-type-children)))))))))

(defun etaf-dom-is-only-of-type (node)
  "检查节点是否是其父节点中该类型的唯一子元素。"
  (when etaf-dom--query-root
    (let ((parent (etaf-dom-get-parent node etaf-dom--query-root)))
      (when parent
        (let* ((node-tag (dom-tag node))
               (element-children (etaf-dom-get-element-children parent))
               (same-type-children
                (cl-remove-if-not
                 (lambda (child) (eq (dom-tag child) node-tag))
                 element-children)))
          (= (length same-type-children) 1))))))

(defun etaf-dom-is-empty (node)
  "检查节点是否为空（没有子节点，或只有空白文本节点）。"
  (when (and node (listp node))
    (let ((children (dom-children node)))
      (or (null children)
          (cl-every (lambda (child)
                      (and (stringp child)
                           (string-match-p "^[ \t\n\r]*$" child)))
                    children)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun ecss-dom-query-selector (dom selector-string)
;;   "在DOM树中查询第一个匹配CSS选择器的节点。
;; DOM是要查询的DOM树，SELECTOR-STRING是CSS选择器字符串。
;; 返回第一个匹配的节点，如果没有匹配则返回nil。

;; 示例：
;;   (ecss-dom-query-selector dom \"#header\")"
;;   (car (ecss-dom-query-selector-all dom selector-string)))

;;; style 样式应用

(defun ecss-dom-set-styles (node styles)
  "为DOM节点设置CSS样式。NODE是DOM节点，STYLES
是样式列表 ((property . value) ...)。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (style-attr (cdr (assq 'style attrs)))
           (style-map (ecss-dom-parse-style-string
                       (or style-attr ""))))
      ;; 合并新样式
      (dolist (style styles)
        (setq style-map (ecss-dom-set-style-property 
                         style-map (car style) (cdr style))))
      ;; 更新style属性
      (let ((new-style-string (ecss-dom-style-map-to-string style-map)))
        (if attrs
            (let ((style-assoc (assq 'style attrs)))
              (if style-assoc
                  (setcdr style-assoc new-style-string)
                ;; 如果有属性列表但没有style属性，添加到属性列表
                (setcdr attrs (cons (cons 'style new-style-string)
                                    (cdr attrs)))))
          ;; 如果没有属性，创建属性列表
          (setcar (cdr node)
                  (list (cons 'style new-style-string))))))))

(defun ecss-dom-apply-style (dom selector-string styles)
  "为DOM中匹配选择器的节点应用CSS样式。
DOM是要操作的DOM树，SELECTOR-STRING是CSS选择器字符串，
STYLES是要应用的样式列表，格式为 ((property . value) ...)。

示例：
  (ecss-dom-apply-style dom \".button\"
    '((color . \"red\") (font-size . \"14px\")))"
  (let ((nodes (ecss-dom-query-selector-all dom selector-string)))
    (dolist (node nodes)
      (ecss-dom-set-styles node styles))
    nodes))

(defun ecss-dom-parse-style-string (style-string)
  "解析CSS style属性字符串为属性映射表。
返回一个alist: ((property . value) ...)。"
  (let ((result '())
        (declarations (split-string style-string ";" t)))
    (dolist (decl declarations)
      (when (string-match "\\s-*\\([^:]+\\)\\s-*:\\s-*\\(.+\\)\\s-*" decl)
        (let ((prop (match-string 1 decl))
              (value (match-string 2 decl)))
          (push (cons (intern prop) value) result))))
    (nreverse result)))

(defun ecss-dom-set-style-property (style-map property value)
  "在样式映射表中设置或更新属性。"
  (let ((existing (assq property style-map)))
    (if existing
        (setcdr existing value)
      (setq style-map (append style-map (list (cons property value)))))
    style-map))

(defun ecss-dom-style-map-to-string (style-map)
  "将样式映射表转换为CSS style字符串。"
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             style-map "; "))

(defun ecss-dom-get-style (node property)
  "获取DOM节点的指定CSS属性值。
NODE是DOM节点，PROPERTY是CSS属性名（symbol）。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (style-attr (cdr (assq 'style attrs)))
           (style-map (ecss-dom-parse-style-string
                       (or style-attr ""))))
      (cdr (assq property style-map)))))

;;; class 属性操作

(defun ecss-dom-add-class (node class-name)
  "为DOM节点添加CSS类。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs)))
           (classes (if class-attr (split-string class-attr) '())))
      (unless (member class-name classes)
        (let ((new-class (string-join
                          (append classes (list class-name)) " ")))
          (if attrs
              (if (assq 'class attrs)
                  (setcdr (assq 'class attrs) new-class)
                (setcdr attrs (cons (cons 'class new-class)
                                    (cdr attrs))))
            (setcar (cdr node) (list (cons 'class new-class)))))))))

(defun ecss-dom-remove-class (node class-name)
  "从DOM节点移除CSS类。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs)))
           (classes (if class-attr (split-string class-attr) '())))
      (when (member class-name classes)
        (let ((new-class (string-join (delete class-name classes) " ")))
          (when (assq 'class attrs)
            (setcdr (assq 'class attrs) new-class)))))))

(defun ecss-dom-has-class (node class-name)
  "检查DOM节点是否有指定的CSS类。"
  (etaf-dom-class-match-p node class-name))

(defun ecss-dom-toggle-class (node class-name)
  "切换DOM节点的CSS类。"
  (if (ecss-dom-has-class node class-name)
      (ecss-dom-remove-class node class-name)
    (ecss-dom-add-class node class-name)))

(provide 'etaf-dom)
;;; etaf-dom.el ends here
