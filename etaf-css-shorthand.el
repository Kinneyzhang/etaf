;;; etaf-css-shorthand.el --- CSS Shorthand Property Expansion -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, shorthand, properties
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 复合属性（shorthand）展开
;;
;; 支持的复合属性：
;; - border: 展开为 border-top/right/bottom/left-width/style/color
;; - border-width: 展开为 border-top/right/bottom/left-width
;; - border-color: 展开为 border-top/right/bottom/left-color
;; - border-style: 展开为 border-top/right/bottom/left-style
;; - border-top/right/bottom/left: 展开为对应的 width/style/color
;; - margin: 展开为 margin-top/right/bottom/left
;; - padding: 展开为 padding-top/right/bottom/left
;;
;; 使用示例：
;;   (etaf-css-expand-shorthand 'border "1px solid red")
;;   ;; => ((border-top-width "1px") (border-top-style "solid") (border-top-color "red")
;;   ;;     (border-right-width "1px") ...)

;;; Code:

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
返回展开后的声明列表：grid-row-start, grid-column-start, grid-row-end, grid-column-end。

如果是命名区域，则所有四个值都设为该名称。
如果是位置，则按 row-start / column-start / row-end / column-end 顺序解析。"
  (let ((parts (split-string value "/" t)))
    (if (= (length parts) 1)
        ;; 命名区域，所有值设为同一个名称
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

(provide 'etaf-css-shorthand)
;;; etaf-css-shorthand.el ends here
