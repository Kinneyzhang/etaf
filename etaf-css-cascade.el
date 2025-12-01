;;; etaf-css-cascade.el --- CSS Cascade Algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, cascade, specificity
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 层叠算法和选择器特异性计算
;;
;; 本模块整合了：
;; 1. CSS 选择器特异性计算
;; 2. 完整的 CSS 层叠规则
;;
;; 实现完整的 CSS 层叠规则，包括：
;; 1. !important 声明的处理
;; 2. 内联样式优先级
;; 3. 选择器特异性比较
;; 4. 文档顺序（后定义优先）
;;
;; 层叠顺序（从低到高）：
;; 1. 正常声明（按特异性和顺序）
;; 2. !important 声明（按特异性和顺序）
;; 3. 内联样式正常声明
;; 4. 内联样式 !important 声明
;;
;; 特异性计算：
;; 特异性格式: (id-count class-count type-count)
;;
;; 比较规则：
;; 1. 首先比较 ID 选择器数量
;; 2. 然后比较类/属性/伪类选择器数量
;; 3. 最后比较标签/伪元素选择器数量
;;
;; 示例:
;;   'div'          => (0 0 1)
;;   '.button'      => (0 1 0)
;;   '#main'        => (1 0 0)
;;   'div.button'   => (0 1 1)
;;   '#main .text'  => (1 1 0)

;;; Code:

(require 'cl-lib)

;;; CSS 选择器特异性计算

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

;;; CSS 层叠算法

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

(provide 'etaf-css-cascade)
;;; etaf-css-cascade.el ends here
