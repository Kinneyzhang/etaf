;;; etaf-css-specificity.el --- CSS Specificity Calculation -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, specificity
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 选择器特异性计算
;;
;; 特异性是用于确定哪个 CSS 规则应用于元素的重要因素。
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

(provide 'etaf-css-specificity)
;;; etaf-css-specificity.el ends here
