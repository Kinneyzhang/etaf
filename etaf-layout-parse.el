;;; etaf-layout-parse.el --- CSS value parsing for layout (compatibility layer) -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, css, parsing
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 向后兼容层 - CSS 值解析功能已迁移至 etaf-css-parse.el
;;
;; 该模块为旧代码提供向后兼容性，所有函数现在委托给 etaf-css-parse 模块。
;; 新代码应直接使用 `etaf-css-parse-*' 函数。
;;
;; 迁移说明：
;; - `etaf-layout-parse-length' -> `etaf-css-parse-length'
;; - `etaf-layout-parse-height' -> `etaf-css-parse-height'
;; - `etaf-layout-parse-style-value' -> `etaf-css-parse-style-value'
;; - `etaf-layout-parse-flex-number' -> `etaf-css-parse-flex-number'
;; - `etaf-layout-parse-pixels-per-line' -> `etaf-css-parse-pixels-per-line'

;;; Code:

(require 'etaf-css-parse)

;;; ============================================================
;;; 向后兼容别名
;;; ============================================================

(defconst etaf-layout-parse-pixels-per-line etaf-css-parse-pixels-per-line
  "每行的像素数，用于将 px 单位转换为行数。
已弃用，请使用 `etaf-css-parse-pixels-per-line'。")

(defalias 'etaf-layout-parse-length 'etaf-css-parse-length
  "解析 CSS 长度值。已弃用，请使用 `etaf-css-parse-length'。")

(defalias 'etaf-layout-parse-height 'etaf-css-parse-height
  "解析 CSS 高度值。已弃用，请使用 `etaf-css-parse-height'。")

(defalias 'etaf-layout-parse-style-value 'etaf-css-parse-style-value
  "从计算样式中获取属性值。已弃用，请使用 `etaf-css-parse-style-value'。")

(defalias 'etaf-layout-parse-flex-number 'etaf-css-parse-flex-number
  "解析 flex 数值属性。已弃用，请使用 `etaf-css-parse-flex-number'。")

(provide 'etaf-layout-parse)
;;; etaf-layout-parse.el ends here
