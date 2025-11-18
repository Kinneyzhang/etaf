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

(defun etaf-css-parse-stylesheet (css-string &optional media-query)
  "解析完整的 CSS 样式表字符串。
CSS-STRING 是包含多个 CSS 规则的字符串。
MEDIA-QUERY 是可选的媒体查询字符串，会附加到所有规则上。
返回规则列表。"
  (when (and css-string (not (string-empty-p css-string)))
    (let ((rules '())
          (start 0)
          (length (length css-string)))
      ;; 检查是否有 @media 规则
      (if (string-match "@media" css-string)
          ;; 有 @media 规则，需要特殊处理
          (let ((media-blocks (etaf-css-media-extract-at-media-blocks css-string))
                (regular-css (etaf-css-remove-at-media-blocks css-string)))
            ;; 首先解析非 @media 规则
            (when (not (string-empty-p (string-trim regular-css)))
              (setq rules (etaf-css-parse-stylesheet-simple regular-css media-query)))
            ;; 然后处理 @media 规则
            (dolist (block media-blocks)
              (let ((query (nth 0 block))
                    (rules-string (nth 1 block)))
                ;; 递归解析 @media 内的规则，传入媒体查询
                (let ((media-rules (etaf-css-parse-stylesheet-simple rules-string query)))
                  (setq rules (append rules media-rules))))))
        ;; 没有 @media 规则，简单解析
        (setq rules (etaf-css-parse-stylesheet-simple css-string media-query)))
      rules)))

(defun etaf-css-parse-stylesheet-simple (css-string &optional media-query)
  "简单解析 CSS 样式表（不处理 @media）。
CSS-STRING 是包含多个 CSS 规则的字符串。
MEDIA-QUERY 是可选的媒体查询字符串。
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
                    ;; 如果有媒体查询，添加到规则中
                    (when media-query
                      (setq rule (plist-put rule :media media-query)))
                    (push rule rules))
                  (setq start (1+ close-brace)))
              (setq start length))
          (setq start length)))
      (nreverse rules))))

(defun etaf-css-remove-at-media-blocks (css-string)
  "从 CSS 字符串中移除所有 @media 块。
CSS-STRING 是 CSS 文本。
返回不包含 @media 块的 CSS 文本。"
  (let ((result "")
        (start 0)
        (length (length css-string)))
    (while (string-match "@media[ \t\n\r]+[^{]+{" css-string start)
      (let* ((media-start (match-beginning 0))
             (content-start (match-end 0))
             (brace-count 1)
             (pos content-start)
             (content-end nil))
        ;; 添加 @media 之前的内容
        (setq result (concat result (substring css-string start media-start)))
        ;; 查找匹配的右花括号
        (while (and (< pos length) (> brace-count 0))
          (let ((char (aref css-string pos)))
            (cond
             ((= char ?{) (setq brace-count (1+ brace-count)))
             ((= char ?}) (setq brace-count (1- brace-count))))
            (setq pos (1+ pos))))
        (when (= brace-count 0)
          (setq start pos))))
    ;; 添加剩余内容
    (setq result (concat result (substring css-string start)))
    result))

;; 需要在文件开头声明外部函数，避免编译警告
(declare-function etaf-css-media-extract-at-media-blocks "etaf-css-media")

(provide 'etaf-css-parser)
;;; etaf-css-parser.el ends here

(provide 'etaf-css-parser)
;;; etaf-css-parser.el ends here
