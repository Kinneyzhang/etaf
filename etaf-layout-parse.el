;;; etaf-layout-parse.el --- CSS value parsing for layout -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, css, parsing
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 值解析模块
;;
;; 本模块提供用于布局计算的 CSS 值解析功能。
;; 所有函数使用 `etaf-layout-parse-' 前缀。
;;
;; 公共接口：
;; - `etaf-layout-parse-length' - 解析 CSS 长度值（px, %, em, lh）
;; - `etaf-layout-parse-height' - 解析 CSS 高度值（行数单位）
;; - `etaf-layout-parse-style-value' - 从计算样式中获取属性值
;; - `etaf-layout-parse-flex-number' - 解析 flex 数值属性
;;
;; 常量：
;; - `etaf-layout-parse-pixels-per-line' - 像素到行数的转换系数

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; 常量
;;; ============================================================

(defconst etaf-layout-parse-pixels-per-line 20
  "每行的像素数，用于将 px 单位转换为行数。
默认假设行高约为 20 像素。")

;;; ============================================================
;;; 公共接口
;;; ============================================================

(defun etaf-layout-parse-length (value reference-width)
  "解析 CSS 长度值。
VALUE 是 CSS 值字符串或数字。
REFERENCE-WIDTH 是参考宽度（用于百分比计算）。

支持的单位：
- px: 像素值
- %: 百分比（相对于 REFERENCE-WIDTH）
- em: 相对单位（1em = 16px）
- lh: 行高单位

返回值：
- 数字: 解析后的像素值
- \\='auto: 自动计算
- \\='none: 无值"
  (cond
   ((null value) 'auto)
   ((eq value 'auto) 'auto)
   ((eq value 'none) 'none)
   ((numberp value) value)
   ((string= value "auto") 'auto)
   ((string= value "none") 'none)
   ((string= value "0") 0)
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-width))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    (* (string-to-number (match-string 1 value)) 16))
   ((string-match "\\`\\([0-9.]+\\)lh\\'" value)
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-layout-parse-height (value reference-height)
  "解析 CSS 高度值。
VALUE 是 CSS 值字符串或数字。
REFERENCE-HEIGHT 是参考高度（用于百分比计算）。

在 Emacs 中，高度使用行数（lh）作为基本单位。

支持的单位：
- lh: 行高单位（直接作为行数）
- 纯数字: 作为行数
- %: 百分比（相对于 REFERENCE-HEIGHT）
- px: 像素值（转换为行数）
- em: 相对单位（1em = 1 行）

返回值：
- 数字: 解析后的行数
- \\='auto: 自动计算
- \\='none: 无值"
  (cond
   ((null value) 'auto)
   ((eq value 'auto) 'auto)
   ((eq value 'none) 'none)
   ((numberp value) value)
   ((string= value "auto") 'auto)
   ((string= value "none") 'none)
   ((string= value "0") 0)
   ((string-match "\\`\\([0-9.]+\\)lh\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\`\\([0-9.]+\\)\\'" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-height))
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (ceiling (/ (string-to-number (match-string 1 value))
                (float etaf-layout-parse-pixels-per-line))))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-layout-parse-style-value (computed-style property &optional default)
  "从计算样式中获取属性值。
COMPUTED-STYLE 是 alist 形式的计算样式。
PROPERTY 是属性名（symbol）。
DEFAULT 是默认值（可选）。

返回属性值或 DEFAULT。"
  (or (cdr (assq property computed-style)) default))

(defun etaf-layout-parse-flex-number (value)
  "解析 flex 数值属性。
VALUE 可以是字符串或数字。

用于解析 flex-grow、flex-shrink、order 等属性。

返回值：
- 数字: 解析后的数值
- nil: 无法解析"
  (cond
   ((numberp value) value)
   ((and (stringp value) (string-match "^-?[0-9]+\\(\\.[0-9]+\\)?$" value))
    (string-to-number value))
   (t nil)))

(provide 'etaf-layout-parse)
;;; etaf-layout-parse.el ends here
