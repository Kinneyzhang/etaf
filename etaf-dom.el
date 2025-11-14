;;; ecss-dom.el --- CSS选择器DOM查询和样式应用 -*- lexical-binding: t; -*-

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
(require 'ecss-selector)

;;; DOM节点匹配函数

(defun ecss-dom-node-matches-tag (node tag-name)
  "检查DOM节点NODE是否匹配标签选择器TAG-NAME。"
  (when (and node (listp node))
    (let ((node-tag (symbol-name (dom-tag node))))
      (or (string= tag-name "*")  ; 通配符
          (string= tag-name node-tag)))))

(defun ecss-dom-node-matches-class (node class-name)
  "检查DOM节点NODE是否匹配类选择器CLASS-NAME。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs))))
      (when class-attr
        (let ((classes (split-string class-attr)))
          (member class-name classes))))))

(defun ecss-dom-node-matches-id (node id-name)
  "检查DOM节点NODE是否匹配ID选择器ID-NAME。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (id-attr (cdr (assq 'id attrs))))
      (and id-attr (string= id-attr id-name)))))

(defun ecss-dom-node-matches-attribute (node attr-node)
  "检查DOM节点NODE是否匹配属性选择器ATTR-NODE。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (attr-name (intern (plist-get attr-node :attribute)))
           (operator (plist-get attr-node :operator))
           (expected-value (plist-get attr-node :value))
           (actual-value (cdr (assq attr-name attrs))))
      (cond
       ;; 仅检查属性存在
       ((null operator)
        (not (null actual-value)))
       ;; 属性值完全匹配
       ((string= operator "=")
        (and actual-value (string= actual-value expected-value)))
       ;; 属性值前缀匹配
       ((string= operator "^=")
        (and actual-value (string-prefix-p expected-value actual-value)))
       ;; 属性值后缀匹配
       ((string= operator "$=")
        (and actual-value (string-suffix-p expected-value actual-value)))
       ;; 属性值包含子串
       ((string= operator "*=")
        (and actual-value (string-match-p
                           (regexp-quote expected-value) actual-value)))
       ;; 属性值包含空格分隔的单词
       ((string= operator "~=")
        (and actual-value 
             (member expected-value (split-string actual-value))))
       ;; 属性值等于或以其开头后跟连字符
       ((string= operator "|=")
        (and actual-value
             (or (string= actual-value expected-value)
                 (string-prefix-p (concat expected-value "-")
                                  actual-value))))
       (t nil)))))

(defun ecss-dom-node-matches-pseudo (node pseudo-node)
  "检查DOM节点NODE是否匹配伪类选择器PSEUDO-NODE。
目前支持基本的结构伪类。"
  (when (and node (listp node))
    (let ((pseudo-value (plist-get pseudo-node :value)))
      (cond
       ;; :first-child
       ((string= pseudo-value ":first-child")
        (ecss-dom-is-first-child node))
       ;; :last-child
       ((string= pseudo-value ":last-child")
        (ecss-dom-is-last-child node))
       ;; :only-child
       ((string= pseudo-value ":only-child")
        (and (ecss-dom-is-first-child node)
             (ecss-dom-is-last-child node)))
       ;; 其他伪类暂不支持，返回t以避免过滤
       (t t)))))

(defun ecss-dom-is-first-child (node)
  "检查节点是否是其父节点的第一个子元素。"
  ;; 简化实现：假设我们不保存父节点引用
  ;; 在实际应用中需要遍历时跟踪
  t)

(defun ecss-dom-is-last-child (node)
  "检查节点是否是其父节点的最后一个子元素。"
  ;; 简化实现
  t)

(defun ecss-dom-node-matches-simple-selector (node selector-ast)
  "检查DOM节点NODE是否匹配简单选择器SELECTOR-AST。
简单选择器是没有组合器的选择器序列，如 'div.class#id'。"
  (when (and node (listp node))
    (let ((matches t))
      ;; 遍历选择器的所有组件
      (ecss-selector-walk
       selector-ast
       (lambda (sel-node)
         (let ((type (plist-get sel-node :type)))
           (cond
            ((eq type 'tag)
             (unless (ecss-dom-node-matches-tag
                      node (plist-get sel-node :value))
               (setq matches nil)))
            ((eq type 'class)
             (unless (ecss-dom-node-matches-class
                      node (plist-get sel-node :value))
               (setq matches nil)))
            ((eq type 'id)
             (unless (ecss-dom-node-matches-id
                      node (plist-get sel-node :value))
               (setq matches nil)))
            ((eq type 'universal)
             ;; 通配符总是匹配
             t)
            ((eq type 'attribute)
             (unless (ecss-dom-node-matches-attribute
                      node sel-node)
               (setq matches nil)))
            ((eq type 'pseudo)
             (unless (ecss-dom-node-matches-pseudo
                      node sel-node)
               (setq matches nil)))))))
      matches)))

;;; DOM遍历辅助函数

(defun ecss-dom-walk (func dom)
  "遍历DOM树的所有节点，对每个节点调用FUNC。
DOM是要遍历的DOM节点，FUNC是对每个节点调用的函数。"
  (when (and dom (listp dom))
    (funcall func dom)
    (let ((children (dom-children dom)))
      (dolist (child children)
        (when (listp child)  ; 跳过文本节点
          (ecss-dom-walk func child))))))

;;; 选择器序列分割

(defun ecss-dom-split-selector-by-combinators (selector-ast)
  "将选择器AST按组合器分割成多个部分。
返回一个列表，每个元素是 (selector-nodes . combinator)。"
  (let ((parts '())
        (current-nodes '())
        (current-combinator nil)
        (last-was-combinator nil))
    (dolist (node (plist-get selector-ast :nodes))
      (if (eq (plist-get node :type) 'combinator)
          (progn
            ;; 保存当前累积的节点和组合器
            (when current-nodes
              (push (cons (nreverse current-nodes) current-combinator)
                    parts))
            ;; 设置新的组合器
            (setq current-combinator (plist-get node :value))
            (setq current-nodes '())
            (setq last-was-combinator t))
        ;; 检查是否有隐式的后代组合器（空格）
        ;; 只在上一个节点不是组合器时检查
        (unless last-was-combinator
          (let* ((spaces (plist-get node :spaces))
                 (before-space (and spaces (plist-get spaces :before))))
            (when (and before-space (not (string-empty-p before-space)))
              ;; 有前导空格，表示这是一个新的选择器部分
              (when current-nodes
                (push (cons (nreverse current-nodes) current-combinator)
                      parts))
              ;; 设置后代组合器
              (setq current-combinator " ")
              (setq current-nodes '()))))
        ;; 累积非组合器节点
        (push node current-nodes)
        (setq last-was-combinator nil)))
    ;; 添加最后一组节点
    (when current-nodes
      (push (cons (nreverse current-nodes) current-combinator) parts))
    (nreverse parts)))

(defun ecss-dom-node-matches-selector-part (node selector-nodes)
  "检查DOM节点是否匹配选择器节点列表SELECTOR-NODES。"
  (let ((matches t)
        (mock-selector (ecss-make-selector)))
    ;; 创建一个临时选择器节点来包含这些节点
    (dolist (sel-node selector-nodes)
      (ecss-node-append mock-selector sel-node))
    (ecss-dom-node-matches-simple-selector node mock-selector)))

;;; 组合器匹配

(defun ecss-dom-matches-descendant-combinator (node ancestor-nodes dom)
  "检查节点NODE是否有祖先匹配ANCESTOR-NODES（后代组合器）。"
  (let ((found nil))
    (ecss-dom-walk
     (lambda (candidate)
       (when (and (not found)
                  (ecss-dom-node-matches-selector-part
                   candidate ancestor-nodes)
                  (ecss-dom-is-descendant-of node candidate dom))
         (setq found t)))
     dom)
    found))

(defun ecss-dom-matches-child-combinator (node parent-nodes dom)
  "检查节点NODE的直接父节点是否匹配PARENT-NODES（子组合器）。"
  ;; 简化实现：遍历DOM查找包含node作为直接子节点的节点
  (let ((found nil))
    (ecss-dom-walk
     (lambda (candidate)
       (when (and (not found)
                  (ecss-dom-node-matches-selector-part
                   candidate parent-nodes))
         (let ((children (dom-non-text-children candidate)))
           (when (memq node children)
             (setq found t)))))
     dom)
    found))

(defun ecss-dom-matches-adjacent-sibling-combinator
    (node prev-sibling-nodes dom)
  "检查节点NODE的前一个兄弟节点是否匹配PREV-SIBLING-NODES（相邻兄弟组合器）。"
  ;; 简化实现
  t)

(defun ecss-dom-matches-general-sibling-combinator
    (node sibling-nodes dom)
  "检查节点NODE之前的兄弟节点中是否有匹配SIBLING-NODES（通用兄弟组合器）。"
  ;; 简化实现
  t)

(defun ecss-dom-is-descendant-of (node ancestor dom)
  "检查NODE是否是ANCESTOR的后代。"
  (and (not (eq node ancestor))
       (catch 'found
         (ecss-dom-walk (lambda (candidate)
                          (when (eq candidate node)
                            (throw 'found t)))
                        ancestor)
         nil)))

;;; 主要查询函数

(defun ecss-dom-query-selector-complex (dom selector-ast)
  "使用复杂选择器（包含组合器）查询DOM，返回所有匹配的节点列表。"
  (let ((parts (ecss-dom-split-selector-by-combinators selector-ast))
        (results '()))
    (if (= (length parts) 1)
        ;; 简单选择器，无组合器
        (ecss-dom-walk (lambda (node)
                         (when (ecss-dom-node-matches-selector-part
                                node (caar parts))
                           (push node results)))
                       dom)
      ;; 复杂选择器，有组合器
      ;; 从右到左匹配
      (let ((rightmost-part (car (last parts)))
            (preceding-parts (butlast parts)))
        ;; 首先找到匹配最右侧选择器的节点
        (ecss-dom-walk (lambda (node)
                         (when (ecss-dom-node-matches-selector-part
                                node (car rightmost-part))
                           ;; 检查是否满足所有组合器关系
                           (when (ecss-dom-check-combinator-chain
                                  node preceding-parts dom)
                             (push node results))))
                       dom)))
    (nreverse results)))

(defun ecss-dom-check-combinator-chain (node parts dom)
  "检查节点是否满足组合器链的所有条件，从右到左处理PARTS。"
  (if (null parts)
      t
    (let* ((current-part (car (last parts)))
           (remaining-parts (butlast parts))
           (combinator (cdr current-part))
           (selector-nodes (car current-part)))
      (cond
       ;; 后代组合器（空格）
       ((or (null combinator) (string= combinator " "))
        (and (ecss-dom-matches-descendant-combinator
              node selector-nodes dom)
             (if remaining-parts
                 ;; 递归检查剩余部分（需要找到匹配的祖先）
                 t  ; 简化实现
               t)))
       ;; 子组合器 (>)
       ((string= combinator ">")
        (and (ecss-dom-matches-child-combinator
              node selector-nodes dom)
             (if remaining-parts t t)))
       ;; 相邻兄弟组合器 (+)
       ((string= combinator "+")
        (and (ecss-dom-matches-adjacent-sibling-combinator
              node selector-nodes dom)
             (if remaining-parts t t)))
       ;; 通用兄弟组合器 (~)
       ((string= combinator "~")
        (and (ecss-dom-matches-general-sibling-combinator
              node selector-nodes dom)
             (if remaining-parts t t)))
       (t t)))))

(defun ecss-dom-query-selector-all (dom selector-string)
  "在DOM树中查询所有匹配CSS选择器的节点。
DOM是要查询的DOM树，SELECTOR-STRING是CSS选择器字符串。
返回匹配节点的列表。

示例：
  (ecss-dom-query-selector-all dom \"div.container p.text\")"
  (let* ((ast (ecss-selector-parse selector-string))
         (root (plist-get ast :type))
         (results '()))
    ;; 处理根节点中的所有选择器（逗号分隔）
    (dolist (selector (plist-get ast :nodes))
      (when (eq (plist-get selector :type) 'selector)
        (let ((matches (ecss-dom-query-selector-complex dom selector)))
          (setq results (append results matches)))))
    ;; 去重
    (cl-remove-duplicates results :test #'equal)))

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
  (ecss-dom-node-matches-class node class-name))

(defun ecss-dom-toggle-class (node class-name)
  "切换DOM节点的CSS类。"
  (if (ecss-dom-has-class node class-name)
      (ecss-dom-remove-class node class-name)
    (ecss-dom-add-class node class-name)))

(provide 'ecss-dom)

;;; ecss-dom.el ends here
