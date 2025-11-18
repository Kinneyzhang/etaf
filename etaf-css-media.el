;;; etaf-css-media.el --- CSS Media Query Support -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, media-query
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 媒体查询支持
;;
;; 主要功能：
;; - 解析 @media 规则
;; - 评估媒体查询条件
;; - 支持常见媒体类型（screen, print, all）
;; - 支持基本媒体特性（width, height, min-width, max-width 等）
;;
;; 使用示例：
;;
;;   ;; 评估媒体查询
;;   (etaf-css-media-match-p "screen" nil)  ; => t
;;   (etaf-css-media-match-p "screen and (min-width: 768px)" '((width . 1024)))
;;   ;; => t
;;
;;   ;; 解析 @media 规则
;;   (etaf-css-media-parse-at-media "@media screen { .header { color: blue; } }")
;;   ;; => (:media "screen" :rules (...))

;;; Code:

(require 'cl-lib)

;;; 媒体查询评估

(defvar etaf-css-media-environment
  '((type . screen)
    (width . 1024)
    (height . 768))
  "默认的媒体查询环境。
包含媒体类型和特性值。")

(defun etaf-css-media-get-environment-value (feature &optional env)
  "获取媒体环境中的特性值。
FEATURE 是特性名（symbol），如 'width、'height。
ENV 是可选的环境 alist，默认使用 `etaf-css-media-environment'。"
  (let ((environment (or env etaf-css-media-environment)))
    (alist-get feature environment)))

(defun etaf-css-media-match-type-p (type &optional env)
  "检查媒体类型是否匹配。
TYPE 是媒体类型字符串，如 \"screen\"、\"print\"、\"all\"。
ENV 是可选的环境 alist。"
  (let* ((environment (or env etaf-css-media-environment))
         (current-type (or (alist-get 'type environment) 'screen)))
    (or (string= type "all")
        (string= type (symbol-name current-type)))))

(defun etaf-css-media-parse-feature (feature-str)
  "解析单个媒体特性表达式。
FEATURE-STR 是形如 \"min-width: 768px\" 或 \"orientation: landscape\" 的字符串。
返回 (feature operator value) 或 nil。"
  (when (string-match "^[ \t]*\\([a-z-]+\\)[ \t]*:[ \t]*\\([^)]+\\)[ \t]*$" feature-str)
    (let* ((feature-name (match-string 1 feature-str))
           (value-str (string-trim (match-string 2 feature-str)))
           (feature-sym (intern feature-name))
           (operator (cond
                     ((string-prefix-p "min-" feature-name) 'min)
                     ((string-prefix-p "max-" feature-name) 'max)
                     (t 'equal)))
           (base-feature (cond
                         ((string-prefix-p "min-" feature-name)
                          (intern (substring feature-name 4)))
                         ((string-prefix-p "max-" feature-name)
                          (intern (substring feature-name 4)))
                         (t feature-sym)))
           (value (cond
                  ;; 解析像素值
                  ((string-match "^\\([0-9]+\\)px$" value-str)
                   (string-to-number (match-string 1 value-str)))
                  ;; 解析数字
                  ((string-match "^[0-9]+$" value-str)
                   (string-to-number value-str))
                  ;; 字符串值
                  (t value-str))))
      (list base-feature operator value)))

(defun etaf-css-media-evaluate-feature (feature operator value &optional env)
  "评估单个媒体特性。
FEATURE 是特性名（symbol）。
OPERATOR 是操作符（'min, 'max, 'equal）。
VALUE 是要比较的值。
ENV 是可选的环境 alist。
返回 t 或 nil。"
  (let ((current-value (etaf-css-media-get-environment-value feature env)))
    (when current-value
      (cond
       ((eq operator 'min)
        (>= current-value value))
       ((eq operator 'max)
        (<= current-value value))
       ((eq operator 'equal)
        (if (numberp value)
            (= current-value value)
          (equal current-value value)))
       (t nil)))))

(defun etaf-css-media-match-query-p (query-str &optional env)
  "检查媒体查询是否匹配。
QUERY-STR 是完整的媒体查询字符串，如 \"screen and (min-width: 768px)\"。
ENV 是可选的环境 alist。
返回 t 或 nil。"
  (if (or (null query-str) (string-empty-p query-str))
      t  ; 空查询匹配所有
    (let* ((query-str (string-trim query-str))
           (result t))
      ;; 处理 "not" 前缀
      (when (string-prefix-p "not " query-str)
        (setq query-str (substring query-str 4)
              result (not result)))
      
      ;; 分离媒体类型和特性
      (if (string-match "^\\([a-z]+\\)\\([ \t]+and[ \t]+\\(.+\\)\\)?$" query-str)
          (let* ((media-type (match-string 1 query-str))
                 (features-str (match-string 3 query-str))
                 (type-match (etaf-css-media-match-type-p media-type env)))
            (if (not type-match)
                (if (string-prefix-p "not " (string-trim query-str))
                    (not result)
                  nil)
              ;; 媒体类型匹配，检查特性
              (if features-str
                  (let ((features-match t))
                    ;; 分离多个特性（用 "and" 连接）
                    (dolist (feature-part (split-string features-str " and " t))
                      (when (string-match "(\\([^)]+\\))" feature-part)
                        (let* ((feature-expr (match-string 1 feature-part))
                               (parsed (etaf-css-media-parse-feature feature-expr)))
                          (when parsed
                            (let ((feature (nth 0 parsed))
                                  (operator (nth 1 parsed))
                                  (value (nth 2 parsed)))
                              (unless (etaf-css-media-evaluate-feature feature operator value env)
                                (setq features-match nil)))))))
                    (if (string-prefix-p "not " (string-trim query-str))
                        (not features-match)
                      features-match))
                ;; 只有媒体类型，没有特性
                result))
        ;; 只有特性，没有媒体类型（假设为 "all"）
        (when (string-match "(\\([^)]+\\))" query-str)
          (let* ((feature-expr (match-string 1 query-str))
                 (parsed (etaf-css-media-parse-feature feature-expr)))
            (when parsed
              (let ((feature (nth 0 parsed))
                    (operator (nth 1 parsed))
                    (value (nth 2 parsed)))
                (etaf-css-media-evaluate-feature feature operator value env))))))))

(defun etaf-css-media-match-p (query-str &optional env)
  "检查媒体查询是否匹配（简化接口）。
QUERY-STR 是完整的媒体查询字符串。
ENV 是可选的环境 alist。
返回 t 或 nil。"
  (etaf-css-media-match-query-p query-str env))

;;; 解析 @media 规则

(defun etaf-css-media-extract-at-media-blocks (css-string)
  "从 CSS 字符串中提取所有 @media 块。
CSS-STRING 是包含 @media 规则的 CSS 文本。
返回 @media 块列表，每个元素为 (query-string rules-string)。"
  (let ((blocks '())
        (start 0)
        (length (length css-string)))
    (while (string-match "@media[ \t\n\r]+\\([^{]+\\)[ \t\n\r]*{" css-string start)
      (let* ((media-start (match-beginning 0))
             (query (string-trim (match-string 1 css-string)))
             (content-start (match-end 0))
             (brace-count 1)
             (pos content-start)
             (content-end nil))
        ;; 查找匹配的右花括号
        (while (and (< pos length) (> brace-count 0))
          (let ((char (aref css-string pos)))
            (cond
             ((= char ?{) (setq brace-count (1+ brace-count)))
             ((= char ?}) (setq brace-count (1- brace-count))))
            (setq pos (1+ pos))))
        (when (= brace-count 0)
          (setq content-end (1- pos))
          (let ((rules-string (substring css-string content-start content-end)))
            (push (list query rules-string) blocks))
          (setq start pos))))
    (nreverse blocks)))

(defun etaf-css-media-parse-at-media (css-string &optional env)
  "解析 @media 规则。
CSS-STRING 是包含 @media 规则的 CSS 文本。
ENV 是可选的环境 alist。
返回匹配的规则列表。如果媒体查询不匹配，返回 nil。"
  (let ((media-blocks (etaf-css-media-extract-at-media-blocks css-string))
        (all-rules '()))
    (dolist (block media-blocks)
      (let ((query (nth 0 block))
            (rules-string (nth 1 block)))
        (when (etaf-css-media-match-p query env)
          ;; 媒体查询匹配，解析内部规则
          ;; 注意：这里需要使用 etaf-css-parse-stylesheet
          ;; 但为了避免循环依赖，我们返回原始字符串
          (push (list :media query :rules-string rules-string) all-rules))))
    (nreverse all-rules)))

(provide 'etaf-css-media)
;;; etaf-css-media.el ends here
