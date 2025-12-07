;;; etaf-css-parser.el --- Complete CSS Parsing Module -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, parser, media, shorthand, values
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Complete CSS parsing module consolidating:
;; - CSS parser (declarations, rules, stylesheets) - from etaf-css-parser.el
;; - Value parsing (px, %, em, lh, cw) - from etaf-css-parse.el
;; - Media query parsing and evaluation - from etaf-css-media.el
;; - Shorthand property expansion - from etaf-css-shorthand.el
;;
;; This consolidates all CSS parsing functionality into a single module
;; for better maintainability and reduced file fragmentation.

;;; Code:

(require 'cl-lib)
(require 'etaf-css-core)

;;; ============================================================
;;; CSS Value Parsing (from etaf-css-parse.el)
;;; ============================================================

(require 'cl-lib)

;;; ============================================================
;;; 常量
;;; ============================================================

(defconst etaf-css-parse-pixels-per-line 20
  "每行的像素数，用于将 px 单位转换为行数。
默认假设行高约为 20 像素。")

;;; ============================================================
;;; 公共接口
;;; ============================================================

(defun etaf-css-parse-length (value reference-width)
  "解析 CSS 长度值。
VALUE 是 CSS 值字符串或数字。
REFERENCE-WIDTH 是参考宽度（用于百分比计算），可以为 nil。

支持的单位：
- px: 像素值
- cw: 字符宽度单位（1cw = 1个字符宽度，使用 frame-char-width）
- %: 百分比（相对于 REFERENCE-WIDTH）
- em: 相对单位（1em = 16px）
- lh: 行高单位

返回值：
- 数字: 解析后的像素值
- \\='auto: 自动计算（包括 REFERENCE-WIDTH 为 nil 时的百分比值）
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
   ((string-match "\\`\\([0-9.]+\\)cw\\'" value)
    (* (string-to-number (match-string 1 value)) (frame-char-width)))
   ((string-match "\\`\\([0-9.]+\\)%\\'" value)
    (if reference-width
        (* (/ (string-to-number (match-string 1 value)) 100.0)
           reference-width)
      'auto))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    (* (string-to-number (match-string 1 value)) 16))
   ((string-match "\\`\\([0-9.]+\\)lh\\'" value)
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-css-parse-height (value reference-height)
  "解析 CSS 高度值。
VALUE 是 CSS 值字符串或数字。
REFERENCE-HEIGHT 是参考高度（用于百分比计算），可以为 nil。

在 Emacs 中，高度使用行数（lh）作为基本单位。

支持的单位：
- lh: 行高单位（直接作为行数）
- 纯数字: 作为行数
- %: 百分比（相对于 REFERENCE-HEIGHT）
- px: 像素值（转换为行数）
- em: 相对单位（1em = 1 行）

返回值：
- 数字: 解析后的行数
- \\='auto: 自动计算（包括 REFERENCE-HEIGHT 为 nil 时的百分比值）
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
    (if reference-height
        (* (/ (string-to-number (match-string 1 value)) 100.0)
           reference-height)
      'auto))
   ((string-match "\\`\\([0-9.]+\\)px\\'" value)
    (ceiling (/ (string-to-number (match-string 1 value))
                (float etaf-css-parse-pixels-per-line))))
   ((string-match "\\`\\([0-9.]+\\)em\\'" value)
    (string-to-number (match-string 1 value)))
   (t 'auto)))

(defun etaf-css-parse-style-value (computed-style property &optional default)
  "从计算样式中获取属性值。
COMPUTED-STYLE 是 alist 形式的计算样式。
PROPERTY 是属性名（symbol）。
DEFAULT 是默认值（可选）。

返回属性值或 DEFAULT。"
  (or (cdr (assq property computed-style)) default))

(defun etaf-css-parse-flex-number (value)
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

;;; ============================================================
;;; Media Query Support (from etaf-css-media.el)
;;; ============================================================

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
                   ;; 解析字符宽度值（支持小数）
                   ((string-match "^\\([0-9.]+\\)cw$" value-str)
                    (* (string-to-number (match-string 1 value-str))
                       (frame-char-width)))
                   ;; 解析数字
                   ((string-match "^[0-9]+$" value-str)
                    (string-to-number value-str))
                   ;; 字符串值
                   (t value-str))))
      (list base-feature operator value))))

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
                              (unless (etaf-css-media-evaluate-feature
                                       feature operator value env)
                                (setq features-match nil)))))))
                    (if (string-prefix-p "not " (string-trim query-str))
                        (not features-match)
                      features-match))
                ;; 只有媒体类型，没有特性
                result)))
        ;; 只有特性，没有媒体类型（假设为 "all"）
        (when (string-match "(\\([^)]+\\))" query-str)
          (let* ((feature-expr (match-string 1 query-str))
                 (parsed (etaf-css-media-parse-feature feature-expr)))
            (when parsed
              (let ((feature (nth 0 parsed))
                    (operator (nth 1 parsed))
                    (value (nth 2 parsed)))
                (etaf-css-media-evaluate-feature feature operator value env)))))))))

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

;;; ============================================================
;;; Shorthand Property Expansion (from etaf-css-shorthand.el)
;;; ============================================================

(require 'cl-lib)

;;; CSS 颜色关键字
(defconst etaf-css-color-keywords
  '("transparent" "currentcolor" "inherit" "initial" "unset"
    ;; Basic colors
    "black" "silver" "gray" "white" "maroon" "red" "purple" "fuchsia"
    "green" "lime" "olive" "yellow" "navy" "blue" "teal" "aqua"
    ;; Extended colors
    "aliceblue" "antiquewhite" "aquamarine" "azure" "beige" "bisque"
    "blanchedalmond" "blueviolet" "brown" "burlywood" "cadetblue"
    "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk"
    "crimson" "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray"
    "darkgreen" "darkgrey" "darkkhaki" "darkmagenta" "darkolivegreen"
    "darkorange" "darkorchid" "darkred" "darksalmon" "darkseagreen"
    "darkslateblue" "darkslategray" "darkslategrey" "darkturquoise"
    "darkviolet" "deeppink" "deepskyblue" "dimgray" "dimgrey"
    "dodgerblue" "firebrick" "floralwhite" "forestgreen" "gainsboro"
    "ghostwhite" "gold" "goldenrod" "greenyellow" "grey" "honeydew"
    "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender"
    "lavenderblush" "lawngreen" "lemonchiffon" "lightblue" "lightcoral"
    "lightcyan" "lightgoldenrodyellow" "lightgray" "lightgreen"
    "lightgrey" "lightpink" "lightsalmon" "lightseagreen" "lightskyblue"
    "lightslategray" "lightslategrey" "lightsteelblue" "lightyellow"
    "limegreen" "linen" "magenta" "mediumaquamarine" "mediumblue"
    "mediumorchid" "mediumpurple" "mediumseagreen" "mediumslateblue"
    "mediumspringgreen" "mediumturquoise" "mediumvioletred" "midnightblue"
    "mintcream" "mistyrose" "moccasin" "navajowhite" "oldlace"
    "olivedrab" "orange" "orangered" "orchid" "palegoldenrod" "palegreen"
    "paleturquoise" "palevioletred" "papayawhip" "peachpuff" "peru"
    "pink" "plum" "powderblue" "rosybrown" "royalblue" "saddlebrown"
    "salmon" "sandybrown" "seagreen" "seashell" "sienna" "skyblue"
    "slateblue" "slategray" "slategrey" "snow" "springgreen" "steelblue"
    "tan" "thistle" "tomato" "turquoise" "violet" "wheat" "whitesmoke"
    "yellowgreen")
  "CSS 颜色关键字列表。")

;;; CSS 边框样式关键字
(defconst etaf-css-border-style-keywords
  '("none" "hidden" "dotted" "dashed" "solid" "double" "groove" "ridge" "inset" "outset")
  "CSS 边框样式关键字列表。")

;;; 辅助函数

(defun etaf-css--is-length-p (value)
  "检查 VALUE 是否是 CSS 长度值。"
  (and (stringp value)
       (or ;; Number with optional decimal and optional unit
        (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?\\(px\\|lh\\|cw\\|%\\)?$" value)
        (string= value "0")
        (string= value "auto")
        (string= value "thin")
        (string= value "medium")
        (string= value "thick"))))

(defun etaf-css--is-flex-basis-p (value)
  "检查 VALUE 是否是有效的 flex-basis 值（带单位的长度或关键字）。
无单位的数字不应被视为 flex-basis。"
  (and (stringp value)
       (or ;; 带单位的长度值
        (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?\\(px\\|lh\\|cw\\|%\\)$" value)
        ;; 0 可以是 flex-basis
        (string= value "0")
        ;; 关键字
        (string= value "auto")
        (string= value "content")
        (string= value "max-content")
        (string= value "min-content")
        (string= value "fit-content"))))

(defun etaf-css--is-color-p (value)
  "检查 VALUE 是否是 CSS 颜色值。"
  (and (stringp value)
       (or (member (downcase value) etaf-css-color-keywords)
           ;; Hex colors: #RGB, #RGBA, #RRGGBB, #RRGGBBAA
           (string-match-p "^#\\([0-9a-fA-F]\\{3\\}\\|[0-9a-fA-F]\\{4\\}\\|[0-9a-fA-F]\\{6\\}\\|[0-9a-fA-F]\\{8\\}\\)$" value)
           ;; CSS color functions
           (string-match-p "^rgb([^)]+)$" value)
           (string-match-p "^rgba([^)]+)$" value)
           (string-match-p "^hsl([^)]+)$" value)
           (string-match-p "^hsla([^)]+)$" value))))

(defun etaf-css--is-border-style-p (value)
  "检查 VALUE 是否是 CSS 边框样式。"
  (and (stringp value)
       (member (downcase value) etaf-css-border-style-keywords)))

(defun etaf-css--parse-border-value (value)
  "解析 border 复合值为 (width style color)。
VALUE 是形如 \"1px solid red\" 的字符串。
返回 (width style color) 列表，未指定的属性为 nil。"
  (let ((parts (split-string value "[ \t]+" t))
        (width nil)
        (style nil)
        (color nil))
    (dolist (part parts)
      (cond
       ((and (not width) (etaf-css--is-length-p part))
        (setq width part))
       ((and (not style) (etaf-css--is-border-style-p part))
        (setq style part))
       ((and (not color) (etaf-css--is-color-p part))
        (setq color part))))
    (list width style color)))

(defun etaf-css--parse-four-values (value)
  "解析四值语法（用于 margin/padding/border-width 等）。
VALUE 是形如 \"1px\" 或 \"1px 2px\" 或 \"1px 2px 3px\" 或 \"1px 2px 3px 4px\" 的字符串。
返回 (top right bottom left) 列表。"
  (let ((parts (split-string value "[ \t]+" t)))
    (pcase (length parts)
      (1 (list (nth 0 parts) (nth 0 parts) (nth 0 parts) (nth 0 parts)))
      (2 (list (nth 0 parts) (nth 1 parts) (nth 0 parts) (nth 1 parts)))
      (3 (list (nth 0 parts) (nth 1 parts) (nth 2 parts) (nth 1 parts)))
      (_ (list (nth 0 parts) (nth 1 parts) (nth 2 parts) (nth 3 parts))))))

;;; 展开函数

(defun etaf-css--expand-border (value important)
  "展开 border 属性。
VALUE 是形如 \"1px solid red\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((parsed (etaf-css--parse-border-value value))
         (width (nth 0 parsed))
         (style (nth 1 parsed))
         (color (nth 2 parsed))
         (result '()))
    (dolist (side '(top right bottom left))
      (when width
        (push (list (intern (format "border-%s-width" side))
                    width important)
              result))
      (when style
        (push (list (intern (format "border-%s-style" side))
                    style important)
              result))
      (when color
        (push (list (intern (format "border-%s-color" side))
                    color important)
              result)))
    (nreverse result)))

(defun etaf-css--expand-border-side (side value important)
  "展开 border-SIDE 属性（如 border-top, border-left 等）。
SIDE 是边的字符串名称，如 \"top\", \"right\", \"bottom\", 或 \"left\"。
VALUE 是形如 \"1px solid red\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((parsed (etaf-css--parse-border-value value))
         (width (nth 0 parsed))
         (style (nth 1 parsed))
         (color (nth 2 parsed))
         (result '()))
    (when width
      (push (list (intern (format "border-%s-width" side))
                  width important)
            result))
    (when style
      (push (list (intern (format "border-%s-style" side))
                  style important)
            result))
    (when color
      (push (list (intern (format "border-%s-color" side))
                  color important)
            result))
    (nreverse result)))

(defun etaf-css--expand-border-width (value important)
  "展开 border-width 属性。
VALUE 是形如 \"1px\" 或 \"1px 2px 3px 4px\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((values (etaf-css--parse-four-values value))
         (top (nth 0 values))
         (right (nth 1 values))
         (bottom (nth 2 values))
         (left (nth 3 values)))
    (list (list 'border-top-width top important)
          (list 'border-right-width right important)
          (list 'border-bottom-width bottom important)
          (list 'border-left-width left important))))

(defun etaf-css--expand-border-color (value important)
  "展开 border-color 属性。
VALUE 是形如 \"red\" 或 \"red green blue yellow\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((values (etaf-css--parse-four-values value))
         (top (nth 0 values))
         (right (nth 1 values))
         (bottom (nth 2 values))
         (left (nth 3 values)))
    (list (list 'border-top-color top important)
          (list 'border-right-color right important)
          (list 'border-bottom-color bottom important)
          (list 'border-left-color left important))))

(defun etaf-css--expand-border-style (value important)
  "展开 border-style 属性。
VALUE 是形如 \"solid\" 或 \"solid dashed dotted double\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((values (etaf-css--parse-four-values value))
         (top (nth 0 values))
         (right (nth 1 values))
         (bottom (nth 2 values))
         (left (nth 3 values)))
    (list (list 'border-top-style top important)
          (list 'border-right-style right important)
          (list 'border-bottom-style bottom important)
          (list 'border-left-style left important))))

(defun etaf-css--expand-margin (value important)
  "展开 margin 属性。
VALUE 是形如 \"10px\" 或 \"10px 20px 30px 40px\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((values (etaf-css--parse-four-values value))
         (top (nth 0 values))
         (right (nth 1 values))
         (bottom (nth 2 values))
         (left (nth 3 values)))
    (list (list 'margin-top top important)
          (list 'margin-right right important)
          (list 'margin-bottom bottom important)
          (list 'margin-left left important))))

(defun etaf-css--expand-margin-inline (value important)
  "展开 margin-inline 属性。IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (list (list 'margin-left value important)
        (list 'margin-right value important)))

(defun etaf-css--expand-margin-block (value important)
  "展开 margin-inline 属性。IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (list (list 'margin-top value important)
        (list 'margin-bottom value important)))

(defun etaf-css--expand-padding (value important)
  "展开 padding 属性。
VALUE 是形如 \"10px\" 或 \"10px 20px 30px 40px\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((values (etaf-css--parse-four-values value))
         (top (nth 0 values))
         (right (nth 1 values))
         (bottom (nth 2 values))
         (left (nth 3 values)))
    (list (list 'padding-top top important)
          (list 'padding-right right important)
          (list 'padding-bottom bottom important)
          (list 'padding-left left important))))

(defun etaf-css--expand-padding-inline (value important)
  "展开 padding-inline 属性。IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (list (list 'padding-left value important)
        (list 'padding-right value important)))

(defun etaf-css--expand-padding-block (value important)
  "展开 padding-inline 属性。IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (list (list 'padding-top value important)
        (list 'padding-bottom value important)))

;;; flex 相关的复合属性

(defconst etaf-css-flex-direction-keywords
  '("row" "row-reverse" "column" "column-reverse")
  "CSS flex-direction 关键字列表。")

(defconst etaf-css-flex-wrap-keywords
  '("nowrap" "wrap" "wrap-reverse")
  "CSS flex-wrap 关键字列表。")

(defun etaf-css--is-flex-direction-p (value)
  "检查 VALUE 是否是 flex-direction 值。"
  (and (stringp value)
       (member (downcase value) etaf-css-flex-direction-keywords)))

(defun etaf-css--is-flex-wrap-p (value)
  "检查 VALUE 是否是 flex-wrap 值。"
  (and (stringp value)
       (member (downcase value) etaf-css-flex-wrap-keywords)))

(defun etaf-css--expand-flex (value important)
  "展开 flex 属性。
VALUE 是形如 \"1\" 或 \"1 1\" 或 \"1 1 auto\" 或 \"none\" 或 \"auto\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：flex-grow, flex-shrink, flex-basis。"
  (let* ((parts (split-string value "[ \t]+" t))
         (grow "0")
         (shrink "1")
         (basis "auto"))
    (pcase (length parts)
      ;; flex: none => 0 0 auto
      ;; flex: auto => 1 1 auto
      ;; flex: <number> => <number> 1 0
      ;; flex: <basis> => 1 1 <basis>
      (1
       (let ((v (car parts)))
         (cond
          ((string= v "none")
           (setq grow "0" shrink "0" basis "auto"))
          ((string= v "auto")
           (setq grow "1" shrink "1" basis "auto"))
          ((string= v "initial")
           (setq grow "0" shrink "1" basis "auto"))
          ;; 使用更严格的 flex-basis 检查，排除无单位数字
          ((etaf-css--is-flex-basis-p v)
           ;; flex: <basis> (带单位或关键字)
           (setq grow "1" shrink "1" basis v))
          (t
           ;; flex: <grow> (无单位数字)
           (setq grow v shrink "1" basis "0")))))
      ;; flex: <grow> <shrink> or flex: <grow> <basis>
      (2
       (let ((v1 (nth 0 parts))
             (v2 (nth 1 parts)))
         (setq grow v1)
         (if (etaf-css--is-flex-basis-p v2)
             (setq shrink "1" basis v2)
           (setq shrink v2 basis "0"))))
      ;; flex: <grow> <shrink> <basis>
      (_
       (setq grow (nth 0 parts)
             shrink (nth 1 parts)
             basis (nth 2 parts))))
    (list (list 'flex-grow grow important)
          (list 'flex-shrink shrink important)
          (list 'flex-basis basis important))))

(defun etaf-css--expand-flex-flow (value important)
  "展开 flex-flow 属性。
VALUE 是形如 \"row\" 或 \"wrap\" 或 \"row wrap\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：flex-direction, flex-wrap。"
  (let* ((parts (split-string value "[ \t]+" t))
         (direction nil)
         (wrap nil))
    (dolist (part parts)
      (cond
       ((and (not direction) (etaf-css--is-flex-direction-p part))
        (setq direction part))
       ((and (not wrap) (etaf-css--is-flex-wrap-p part))
        (setq wrap part))))
    (let ((result '()))
      (when direction
        (push (list 'flex-direction direction important) result))
      (when wrap
        (push (list 'flex-wrap wrap important) result))
      (nreverse result))))

(defun etaf-css--expand-gap (value important)
  "展开 gap 属性。
VALUE 是形如 \"10px\" 或 \"10px 20px\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：row-gap, column-gap。"
  (let* ((parts (split-string value "[ \t]+" t))
         (row-gap nil)
         (column-gap nil))
    (pcase (length parts)
      (1
       (setq row-gap (nth 0 parts)
             column-gap (nth 0 parts)))
      (_
       (setq row-gap (nth 0 parts)
             column-gap (nth 1 parts))))
    (list (list 'row-gap row-gap important)
          (list 'column-gap column-gap important))))

(defun etaf-css--expand-place-property (value important align-prop justify-prop)
  "Helper function to expand place-* shorthand properties.
VALUE is a string like \"center\" or \"center space-between\".
IMPORTANT is whether this is !important.
ALIGN-PROP is the align property symbol (e.g., 'align-content).
JUSTIFY-PROP is the justify property symbol (e.g., 'justify-content).
Returns expanded declaration list.

辅助函数用于展开 place-* 复合属性。"
  (let* ((parts (split-string value "[ \t]+" t))
         (align-value (nth 0 parts))
         (justify-value (if (> (length parts) 1)
                            (nth 1 parts)
                          (nth 0 parts))))
    (list (list align-prop align-value important)
          (list justify-prop justify-value important))))

(defun etaf-css--expand-place-content (value important)
  "Expand place-content shorthand property.
VALUE is a string like \"center\" or \"center space-between\".
IMPORTANT is whether this is !important.
Returns expanded declaration list: align-content, justify-content.

展开 place-content 属性。
VALUE 是形如 \"center\" 或 \"center space-between\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：align-content, justify-content。"
  (etaf-css--expand-place-property value important 'align-content 'justify-content))

(defun etaf-css--expand-place-items (value important)
  "Expand place-items shorthand property.
VALUE is a string like \"center\" or \"center start\".
IMPORTANT is whether this is !important.
Returns expanded declaration list: align-items, justify-items.

展开 place-items 属性。
VALUE 是形如 \"center\" 或 \"center start\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：align-items, justify-items。"
  (etaf-css--expand-place-property value important 'align-items 'justify-items))

(defun etaf-css--expand-place-self (value important)
  "Expand place-self shorthand property.
VALUE is a string like \"center\" or \"center start\".
IMPORTANT is whether this is !important.
Returns expanded declaration list: align-self, justify-self.

展开 place-self 属性。
VALUE 是形如 \"center\" 或 \"center start\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：align-self, justify-self。"
  (etaf-css--expand-place-property value important 'align-self 'justify-self))

;;; Grid shorthand properties

(defun etaf-css--expand-grid-column (value important)
  "展开 grid-column 属性。
VALUE 是形如 \"1\" 或 \"1 / 3\" 或 \"span 2\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：grid-column-start, grid-column-end。"
  (let* ((parts (split-string value "/" t))
         (start (string-trim (nth 0 parts)))
         (end (if (> (length parts) 1)
                  (string-trim (nth 1 parts))
                "auto")))
    (list (list 'grid-column-start start important)
          (list 'grid-column-end end important))))

(defun etaf-css--expand-grid-row (value important)
  "展开 grid-row 属性。
VALUE 是形如 \"1\" 或 \"1 / 3\" 或 \"span 2\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表：grid-row-start, grid-row-end。"
  (let* ((parts (split-string value "/" t))
         (start (string-trim (nth 0 parts)))
         (end (if (> (length parts) 1)
                  (string-trim (nth 1 parts))
                "auto")))
    (list (list 'grid-row-start start important)
          (list 'grid-row-end end important))))

(defun etaf-css--expand-grid-area (value important)
  "展开 grid-area 属性。
VALUE 可以是命名区域（如 \"header\"）或位置（如 \"1 / 1 / 3 / 3\"）。
IMPORTANT 是否为 !important。

如果是命名区域，返回单个 grid-area 属性。
如果是位置，按 row-start / column-start / row-end / column-end 顺序解析，
返回展开后的声明列表：grid-row-start, grid-column-start, grid-row-end, grid-column-end。"
  (let ((parts (split-string value "/" t)))
    (if (= (length parts) 1)
        ;; 命名区域，不展开
        (let ((area-name (string-trim value)))
          (list (list 'grid-area area-name important)))
      ;; 位置值：row-start / column-start / row-end / column-end
      (let ((row-start (string-trim (nth 0 parts)))
            (column-start (string-trim (nth 1 parts)))
            (row-end (if (> (length parts) 2) (string-trim (nth 2 parts)) "auto"))
            (column-end (if (> (length parts) 3) (string-trim (nth 3 parts)) "auto")))
        (list (list 'grid-row-start row-start important)
              (list 'grid-column-start column-start important)
              (list 'grid-row-end row-end important)
              (list 'grid-column-end column-end important))))))

;;; 主展开函数

(defun etaf-css-expand-shorthand (prop value important)
  "Expand CSS shorthand properties.
PROP is the property name (symbol).
VALUE is the property value (string).
IMPORTANT is whether this is !important.
If it's a shorthand property, returns expanded declaration list ((prop value important) ...).
If not a shorthand property, returns nil.

展开 CSS 复合属性。
PROP 是属性名（symbol）。
VALUE 是属性值（string）。
IMPORTANT 是否为 !important。
如果是复合属性，返回展开后的声明列表 ((prop value important) ...)。
如果不是复合属性，返回 nil。"
  (pcase prop
    ;; border 复合属性
    ('border (etaf-css--expand-border value important))
    ('border-top (etaf-css--expand-border-side "top" value important))
    ('border-right (etaf-css--expand-border-side "right" value important))
    ('border-bottom (etaf-css--expand-border-side "bottom" value important))
    ('border-left (etaf-css--expand-border-side "left" value important))
    ('border-width (etaf-css--expand-border-width value important))
    ('border-color (etaf-css--expand-border-color value important))
    ('border-style (etaf-css--expand-border-style value important))
    ;; margin/padding 复合属性
    ('margin (etaf-css--expand-margin value important))
    ('margin-inline (etaf-css--expand-margin-inline value important))
    ('margin-block (etaf-css--expand-margin-block value important))
    ('padding (etaf-css--expand-padding value important))
    ('padding-inline (etaf-css--expand-padding-inline value important))
    ('padding-block (etaf-css--expand-padding-block value important))
    ;; flex 复合属性
    ('flex (etaf-css--expand-flex value important))
    ('flex-flow (etaf-css--expand-flex-flow value important))
    ('gap (etaf-css--expand-gap value important))
    ;; grid 复合属性
    ('grid-column (etaf-css--expand-grid-column value important))
    ('grid-row (etaf-css--expand-grid-row value important))
    ('grid-area (etaf-css--expand-grid-area value important))
    ;; place-* 复合属性
    ('place-content (etaf-css--expand-place-content value important))
    ('place-items (etaf-css--expand-place-items value important))
    ('place-self (etaf-css--expand-place-self value important))
    ;; 非复合属性
    (_ nil)))

(defun etaf-css-expand-declarations (declarations)
  "展开声明列表中的所有复合属性。
DECLARATIONS 是 ((prop value important) ...) 格式的列表。
返回展开后的声明列表。"
  (let ((result '()))
    (dolist (decl declarations)
      (let* ((prop (nth 0 decl))
             (value (nth 1 decl))
             (important (nth 2 decl))
             (expanded (etaf-css-expand-shorthand prop value important)))
        (if expanded
            ;; 复合属性，添加展开后的声明
            (dolist (exp-decl expanded)
              (push exp-decl result))
          ;; 非复合属性，直接添加
          (push decl result))))
    (nreverse result)))

;;; ============================================================
;;; CSS Parser (from etaf-css-parser.el)
;;; ============================================================

(require 'cl-lib)
;;(require 'etaf-css-cascade) ; now in etaf-css-core
;;(require 'etaf-css-shorthand) ; now in same file



(defun etaf-css-parse-declarations (css-string)
  "解析 CSS 声明字符串为属性列表（支持 !important 和复合属性展开）。
CSS-STRING 是形如 \"color: red; font-size: 14px\" 的字符串。
返回 ((property value important) ...) 格式的列表。

每个声明元素是一个列表：(property value important)
- property: 属性名（symbol）
- value: 属性值（string）
- important: 是否标记为 !important（boolean）

复合属性（如 border, margin, padding）会自动展开为具体属性。"
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
              (let* ((prop-sym (intern prop))
                     (expanded (etaf-css-expand-shorthand prop-sym value important)))
                (if expanded
                    ;; 复合属性，添加展开后的声明
                    (dolist (exp-decl expanded)
                      (push exp-decl result))
                  ;; 非复合属性，直接添加
                  (push (list prop-sym value important) result)))))))
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

(provide 'etaf-css-parser)
;;; etaf-css-parser.el ends here
