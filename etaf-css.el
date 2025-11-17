;;; etaf-css.el --- Parse inline and external styles to CSSOM -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, cssom, parser
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 将 DOM 中的内联/外部样式解析为 CSSOM (CSS Object Model)
;;
;; CSSOM 是一个包含 CSS 规则的数据结构，每个规则包含：
;; - selector: CSS 选择器字符串
;; - declarations: CSS 声明列表 ((property . value) ...)
;; - source: 样式来源 (inline/style-tag)
;; - specificity: 选择器特异性（用于层叠）
;;
;; 主要功能：
;; - 解析 CSS 声明字符串
;; - 从 DOM 节点的 style 属性提取内联样式
;; - 从 <style> 标签提取外部样式
;; - 构建和查询 CSSOM
;;
;; 使用示例：
;;
;;   ;; 从 DOM 构建 CSSOM
;;   (etaf-css-build-cssom dom)
;;   
;;   ;; 解析 CSS 声明
;;   (etaf-css-parse-declarations "color: red; font-size: 14px;")
;;   ;; => ((color . "red") (font-size . "14px"))
;;
;;   ;; 查询匹配节点的样式
;;   (etaf-css-get-computed-style cssom node dom)

;;; Code:

(require 'cl-lib)
(require 'etaf-dom)
(require 'etaf-css-selector)

;;; CSS 声明解析

(defun etaf-css-parse-declarations (css-string)
  "解析 CSS 声明字符串为属性列表。
CSS-STRING 是形如 \"color: red; font-size: 14px\" 的字符串。
返回 ((property . value) ...) 格式的 alist。"
  (when (and css-string (not (string-empty-p css-string)))
    (let ((result '())
          (declarations (split-string css-string ";" t)))
      (dolist (decl declarations)
        (when (string-match "^[ \t\n\r]*\\([^:]+\\)[ \t\n\r]*:[ \t\n\r]*\\(.+\\)[ \t\n\r]*$" decl)
          (let ((prop (string-trim (match-string 1 decl)))
                (value (string-trim (match-string 2 decl))))
            (when (and (not (string-empty-p prop))
                       (not (string-empty-p value)))
              (push (cons (intern prop) value) result)))))
      (nreverse result))))

;;; CSS 特异性计算

(defun etaf-css-calculate-specificity (selector)
  "计算 CSS 选择器的特异性。
返回 (id-count class-count type-count) 格式的列表。

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
    ;; 需要匹配不以 # . : [ 开头的标签名
    (let ((pos 0))
      (while (string-match "\\(?:^\\|[ >+~]\\)\\([a-z][a-z0-9]*\\)\\(?=[#.:\\[]\\|[ >+~]\\|$\\)" 
                           selector-str pos)
        (cl-incf type-count)
        (setq pos (match-end 1))))
    
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

;;; CSS 规则解析

(defun etaf-css-parse-rule (rule-string)
  "解析单个 CSS 规则字符串。
RULE-STRING 是形如 \"selector { declarations }\" 的字符串。
返回 (:selector selector :declarations declarations :specificity ...) 格式的 plist。"
  (when (string-match "^[ \t\n\r]*\\([^{]+\\)[ \t\n\r]*{[ \t\n\r]*\\([^}]*\\)[ \t\n\r]*}[ \t\n\r]*$" 
                      rule-string)
    (let* ((selector (string-trim (match-string 1 rule-string)))
           (declarations-str (string-trim (match-string 2 rule-string)))
           (declarations (etaf-css-parse-declarations declarations-str)))
      (when (and selector (not (string-empty-p selector)))
        (list :selector selector
              :declarations declarations
              :specificity (etaf-css-calculate-specificity selector)
              :source 'style-tag)))))

(defun etaf-css-parse-stylesheet (css-string)
  "解析完整的 CSS 样式表字符串。
CSS-STRING 是包含多个 CSS 规则的字符串。
返回规则列表。"
  (when (and css-string (not (string-empty-p css-string)))
    (let ((rules '())
          (start 0)
          (length (length css-string)))
      ;; 简单的规则提取：查找 { } 配对
      (while (< start length)
        (let ((open-brace (string-match "{" css-string start)))
          (if open-brace
              (let ((close-brace (string-match "}" css-string open-brace)))
                (if close-brace
                    (let* ((rule-string (substring css-string start (1+ close-brace)))
                           (rule (etaf-css-parse-rule rule-string)))
                      (when rule
                        (push rule rules))
                      (setq start (1+ close-brace)))
                  (setq start length)))
            (setq start length))))
      (nreverse rules))))

;;; 从 DOM 提取样式

(defun etaf-css-extract-inline-styles (dom)
  "从 DOM 树中提取所有内联样式（style 属性）。
返回规则列表，每个规则包含节点引用和样式声明。"
  (let ((rules '()))
    (etaf-dom-map
     (lambda (node)
       (when-let* ((style-attr (dom-attr node 'style))
                   (declarations (etaf-css-parse-declarations style-attr)))
         (when declarations
           ;; 为内联样式生成唯一标识（使用节点的标签、类和ID）
           (let* ((tag (dom-tag node))
                  (id (dom-attr node 'id))
                  (class (dom-attr node 'class))
                  (selector (concat (symbol-name tag)
                                   (when id (concat "#" id))
                                   (when class 
                                     (mapconcat (lambda (c) (concat "." c))
                                               (split-string class) "")))))
             (push (list :selector selector
                        :declarations declarations
                        :specificity '(1 0 0 0)  ; 内联样式最高特异性
                        :source 'inline
                        :node node)
                   rules)))))
     dom)
    (nreverse rules)))

(defun etaf-css-extract-style-tags (dom)
  "从 DOM 树中提取 <style> 标签内的 CSS 规则。
返回规则列表。"
  (let ((rules '())
        (style-nodes (dom-search dom (lambda (node) 
                                       (eq (dom-tag node) 'style)))))
    (dolist (style-node style-nodes)
      (let ((css-content (dom-texts style-node)))
        (when css-content
          (let ((stylesheet-rules (etaf-css-parse-stylesheet css-content)))
            (setq rules (append rules stylesheet-rules))))))
    rules))

;;; CSSOM 构建和查询

(defun etaf-css-build-cssom (dom)
  "从 DOM 树构建 CSSOM (CSS Object Model)。
返回包含所有 CSS 规则的 CSSOM 结构。"
  (let ((inline-rules (etaf-css-extract-inline-styles dom))
        (style-rules (etaf-css-extract-style-tags dom)))
    (list :inline-rules inline-rules
          :style-rules style-rules
          :all-rules (append style-rules inline-rules))))

(defun etaf-css-get-rules-for-node (cssom node dom)
  "从 CSSOM 中获取适用于指定节点的所有规则。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。
返回适用的规则列表。"
  (let ((matching-rules '())
        (all-rules (plist-get cssom :all-rules))
        (etaf-dom--query-root dom))
    (dolist (rule all-rules)
      (cond
       ;; 内联样式直接匹配节点
       ((eq (plist-get rule :source) 'inline)
        (let ((rule-node (plist-get rule :node)))
          (when (or (eq rule-node node)
                    (and (eq (dom-tag rule-node) (dom-tag node))
                         (equal (dom-attributes rule-node) (dom-attributes node))))
            (push rule matching-rules))))
       ;; 外部样式通过选择器匹配
       ((eq (plist-get rule :source) 'style-tag)
        (condition-case nil
            (let* ((selector (plist-get rule :selector))
                   (ast (etaf-css-selector-parse selector)))
              (when ast
                (let ((first-selector (car (plist-get ast :nodes))))
                  (when (and first-selector
                            (eq (plist-get first-selector :type) 'selector))
                    (when (etaf-css-selector-basic-match-p node first-selector)
                      (push rule matching-rules))))))
          (error nil)))))
    (nreverse matching-rules)))

(defun etaf-css-get-computed-style (cssom node dom)
  "计算指定节点的最终样式（层叠后的样式）。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。
返回合并后的样式声明列表 ((property . value) ...)。
使用完整的 CSS 层叠算法，考虑特异性和来源。"
  (let ((rules (etaf-css-get-rules-for-node cssom node dom))
        (property-rules (make-hash-table :test 'eq)))
    ;; 为每个属性收集所有声明及其规则信息
    (dolist (rule rules)
      (let ((declarations (plist-get rule :declarations))
            (specificity (plist-get rule :specificity))
            (source (plist-get rule :source)))
        (dolist (decl declarations)
          (let ((prop (car decl))
                (value (cdr decl)))
            ;; 存储：(值 特异性 来源 规则索引)
            (push (list value specificity source) 
                  (gethash prop property-rules))))))
    
    ;; 对每个属性应用层叠规则
    (let ((computed-style '()))
      (maphash
       (lambda (prop decl-list)
         ;; 选择优先级最高的声明
         (let ((winner (car decl-list)))
           (dolist (decl (cdr decl-list))
             (when (etaf-css--declaration-has-priority decl winner)
               (setq winner decl)))
           ;; 添加到结果
           (push (cons prop (car winner)) computed-style)))
       property-rules)
      (nreverse computed-style))))

(defun etaf-css--declaration-has-priority (decl1 decl2)
  "判断 decl1 是否比 decl2 有更高优先级。
每个声明格式: (value specificity source)。
层叠顺序：inline > 特异性 > 文档顺序。"
  (let ((spec1 (nth 1 decl1))
        (spec2 (nth 1 decl2))
        (source1 (nth 2 decl1))
        (source2 (nth 2 decl2)))
    (cond
     ;; inline 样式优先级最高
     ((and (eq source1 'inline) (not (eq source2 'inline))) t)
     ((and (eq source2 'inline) (not (eq source1 'inline))) nil)
     ;; 比较特异性
     ((etaf-css-specificity> spec1 spec2) t)
     ((etaf-css-specificity> spec2 spec1) nil)
     ;; 特异性相同，后定义的优先（但由于我们是反向遍历，先遇到的是后定义的）
     (t nil))))

;;; 辅助函数

(defun etaf-css-rule-to-string (rule)
  "将 CSS 规则转换为字符串形式。"
  (let ((selector (plist-get rule :selector))
        (declarations (plist-get rule :declarations)))
    (format "%s { %s }"
            selector
            (mapconcat (lambda (decl)
                        (format "%s: %s" (car decl) (cdr decl)))
                      declarations "; "))))

(defun etaf-css-cssom-to-string (cssom)
  "将 CSSOM 转换为可读的字符串形式。"
  (let ((all-rules (plist-get cssom :all-rules)))
    (mapconcat #'etaf-css-rule-to-string all-rules "\n\n")))

(provide 'etaf-css)
;;; etaf-css.el ends here
