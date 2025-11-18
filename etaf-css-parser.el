;;; etaf-css-parser.el --- CSS Parser -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, parser
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 解析器，用于解析 CSS 声明、规则和样式表
;;
;; 主要功能：
;; - 解析 CSS 声明字符串（支持 !important）
;; - 解析 CSS 规则
;; - 解析完整的 CSS 样式表
;;
;; 使用示例：
;;
;;   ;; 解析 CSS 声明
;;   (etaf-css-parse-declarations "color: red; font-size: 14px;")
;;   ;; => ((color "red" nil) (font-size "14px" nil))
;;
;;   ;; 解析带 !important 的声明
;;   (etaf-css-parse-declarations "color: red !important; font-size: 14px;")
;;   ;; => ((color "red" t) (font-size "14px" nil))

;;; Code:

(require 'cl-lib)
(require 'etaf-css-specificity)

(defun etaf-css-parse-declarations (css-string)
  "解析 CSS 声明字符串为属性列表（支持 !important）。
CSS-STRING 是形如 \"color: red; font-size: 14px\" 的字符串。
返回 ((property value important) ...) 格式的列表。

每个声明元素是一个列表：(property value important)
- property: 属性名（symbol）
- value: 属性值（string）
- important: 是否标记为 !important（boolean）"
  (when (and css-string (not (string-empty-p css-string)))
    (let ((result '())
          (declarations (split-string css-string ";" t)))
      (dolist (decl declarations)
        (when (string-match "^[ \t\n\r]*\\([^:]+\\)[ \t\n\r]*:[ \t\n\r]*\\(.+\\)[ \t\n\r]*$" decl)
          (let* ((prop (string-trim (match-string 1 decl)))
                 (value-str (string-trim (match-string 2 decl)))
                 (important nil)
                 (value value-str))
            ;; 检查是否有 !important
            (when (string-match "^\\(.+?\\)[ \t\n\r]*![ \t\n\r]*important[ \t\n\r]*$" value-str)
              (setq value (string-trim (match-string 1 value-str))
                    important t))
            (when (and (not (string-empty-p prop))
                       (not (string-empty-p value)))
              (push (list (intern prop) value important) result)))))
      (nreverse result))))

(defun etaf-css-parse-declarations-compat (css-string)
  "解析 CSS 声明字符串为属性列表（向后兼容格式，不支持 !important）。
CSS-STRING 是形如 \"color: red; font-size: 14px\" 的字符串。
返回 ((property . value) ...) 格式的 alist。

这是为了向后兼容而保留的函数。新代码应该使用 etaf-css-parse-declarations。"
  (when (and css-string (not (string-empty-p css-string)))
    (let ((result '())
          (declarations (etaf-css-parse-declarations css-string)))
      (dolist (decl declarations)
        (let ((prop (nth 0 decl))
              (value (nth 1 decl)))
          (push (cons prop value) result)))
      (nreverse result))))

(defun etaf-css-parse-rule (rule-string)
  "解析单个 CSS 规则字符串。
RULE-STRING 是形如 \"selector { declarations }\" 的字符串。
返回 (:selector selector :declarations declarations :specificity ...) 格式的 plist。"
  (when (string-match "^[ \t\n\r]*\\([^{]+\\)[ \t\n\r]*{[ \t\n\r]*\\([^}]*\\)[ \t\n\r]*}[ \t\n\r]*$" 
                      rule-string)
    (when-let* ((selector (string-trim (match-string 1 rule-string)))
                (declarations-str (string-trim (match-string 2 rule-string)))
                (declarations (etaf-css-parse-declarations declarations-str))
                ((not (string-empty-p selector))))
      (list :selector selector
            :declarations declarations
            :specificity (etaf-css-calculate-specificity selector)
            :source 'style-tag))))

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
        (if-let ((open-brace (string-match "{" css-string start)))
            (if-let ((close-brace (string-match "}" css-string open-brace)))
                (let* ((rule-string (substring css-string start (1+ close-brace)))
                       (rule (etaf-css-parse-rule rule-string)))
                  (when rule
                    (push rule rules))
                  (setq start (1+ close-brace)))
              (setq start length))
          (setq start length)))
      (nreverse rules))))

(provide 'etaf-css-parser)
;;; etaf-css-parser.el ends here
