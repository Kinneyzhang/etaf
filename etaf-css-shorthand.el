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
       (or (string-match-p "^[0-9.]+\\(px\\|em\\|rem\\|%\\|pt\\|cm\\|mm\\|in\\|vh\\|vw\\|vmin\\|vmax\\|ex\\|ch\\)?$" value)
           (string= value "0")
           (string= value "auto")
           (string= value "thin")
           (string= value "medium")
           (string= value "thick"))))

(defun etaf-css--is-color-p (value)
  "检查 VALUE 是否是 CSS 颜色值。"
  (and (stringp value)
       (or (member (downcase value) etaf-css-color-keywords)
           (string-match-p "^#[0-9a-fA-F]\\{3,8\\}$" value)
           (string-match-p "^rgb(" value)
           (string-match-p "^rgba(" value)
           (string-match-p "^hsl(" value)
           (string-match-p "^hsla(" value))))

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
        (push (list (intern (format "border-%s-width" side)) width important) result))
      (when style
        (push (list (intern (format "border-%s-style" side)) style important) result))
      (when color
        (push (list (intern (format "border-%s-color" side)) color important) result)))
    (nreverse result)))

(defun etaf-css--expand-border-side (side value important)
  "展开 border-SIDE 属性（如 border-top, border-left 等）。
SIDE 是 'top, 'right, 'bottom, 或 'left。
VALUE 是形如 \"1px solid red\" 的字符串。
IMPORTANT 是否为 !important。
返回展开后的声明列表。"
  (let* ((parsed (etaf-css--parse-border-value value))
         (width (nth 0 parsed))
         (style (nth 1 parsed))
         (color (nth 2 parsed))
         (result '()))
    (when width
      (push (list (intern (format "border-%s-width" side)) width important) result))
    (when style
      (push (list (intern (format "border-%s-style" side)) style important) result))
    (when color
      (push (list (intern (format "border-%s-color" side)) color important) result))
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

;;; 主展开函数

(defun etaf-css-expand-shorthand (prop value important)
  "展开 CSS 复合属性。
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
    ('padding (etaf-css--expand-padding value important))
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
