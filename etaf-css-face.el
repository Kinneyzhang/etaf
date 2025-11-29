;;; etaf-css-face.el --- CSS to Emacs Face Property Mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, face, text-properties
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 样式到 Emacs face 属性的映射
;;
;; 这个模块实现 CSS 属性到 Emacs text-properties face 属性的转换。
;; 
;; 支持的 CSS 属性映射：
;; - color → :foreground
;; - background-color → :background
;; - font-weight: bold → :weight bold
;; - font-style: italic → :slant italic
;; - text-decoration: underline → :underline
;; - text-decoration: line-through → :strike-through
;; - font-size → :height (相对值)
;; - font-family → :family
;;
;; 使用示例：
;;   (etaf-css-style-to-face '((color . "red") (font-weight . "bold")))
;;   ;; => (:foreground "red" :weight bold)
;;
;;   (etaf-css-apply-face-to-string "hello" style)
;;   ;; => #("hello" 0 5 (face (:foreground "red" :weight bold)))

;;; Code:

(require 'cl-lib)

;;; CSS 字体大小常量

(defconst etaf-css-baseline-font-size 16.0
  "CSS 基准字体大小（像素）。
用于将像素值转换为 Emacs :height 相对值。
16px 是 CSS 默认的 medium 字体大小。")

;;; CSS 颜色处理

(defun etaf-css-color-to-emacs (css-color)
  "将 CSS 颜色值转换为 Emacs 可用的颜色。
CSS-COLOR 是 CSS 颜色字符串，如 \"red\", \"#ff0000\", \"rgb(255,0,0)\"。
返回 Emacs 颜色字符串，如果无法解析则返回 nil。"
  (when (and css-color (stringp css-color))
    (let ((color (string-trim css-color)))
      (cond
       ;; 十六进制颜色
       ((string-match-p "^#[0-9a-fA-F]\\{3,8\\}$" color)
        color)
       ;; rgb() 函数
       ((string-match "^rgb(\\s*\\([0-9]+\\)\\s*,\\s*\\([0-9]+\\)\\s*,\\s*\\([0-9]+\\)\\s*)$" color)
        (format "#%02x%02x%02x"
                (string-to-number (match-string 1 color))
                (string-to-number (match-string 2 color))
                (string-to-number (match-string 3 color))))
       ;; rgba() 函数（忽略 alpha）
       ((string-match "^rgba(\\s*\\([0-9]+\\)\\s*,\\s*\\([0-9]+\\)\\s*,\\s*\\([0-9]+\\)\\s*,.*)" color)
        (format "#%02x%02x%02x"
                (string-to-number (match-string 1 color))
                (string-to-number (match-string 2 color))
                (string-to-number (match-string 3 color))))
       ;; 命名颜色（直接使用，Emacs 支持大部分 CSS 颜色名）
       ((member (downcase color) '("transparent" "inherit" "initial" "unset"))
        nil)  ; 特殊值不处理
       (t color)))))  ; 其他情况直接返回，让 Emacs 尝试解析

;;; CSS 字体属性处理

(defun etaf-css-font-weight-to-emacs (css-weight)
  "将 CSS font-weight 转换为 Emacs :weight 值。"
  (when (and css-weight (stringp css-weight))
    (let ((weight (string-trim (downcase css-weight))))
      (cond
       ((or (string= weight "bold") (string= weight "700") (string= weight "800") (string= weight "900"))
        'bold)
       ((or (string= weight "normal") (string= weight "400"))
        'normal)
       ((or (string= weight "lighter") (string= weight "100") (string= weight "200") (string= weight "300"))
        'light)
       ((or (string= weight "500") (string= weight "600"))
        'semi-bold)
       (t nil)))))

(defun etaf-css-font-style-to-emacs (css-style)
  "将 CSS font-style 转换为 Emacs :slant 值。"
  (when (and css-style (stringp css-style))
    (let ((style (string-trim (downcase css-style))))
      (cond
       ((string= style "italic") 'italic)
       ((string= style "oblique") 'oblique)
       ((string= style "normal") 'normal)
       (t nil)))))

(defun etaf-css-text-decoration-to-emacs (css-decoration)
  "将 CSS text-decoration 转换为 Emacs face 属性。
返回 plist，可能包含 :underline、:strike-through、:overline。"
  (when (and css-decoration (stringp css-decoration))
    (let ((decoration (string-trim (downcase css-decoration)))
          (result nil))
      (when (string-match-p "underline" decoration)
        (setq result (plist-put result :underline t)))
      (when (string-match-p "line-through" decoration)
        (setq result (plist-put result :strike-through t)))
      (when (string-match-p "overline" decoration)
        (setq result (plist-put result :overline t)))
      result)))

(defun etaf-css-font-size-to-emacs (css-size)
  "将 CSS font-size 转换为 Emacs :height 值。
返回相对高度浮点数（如 1.2），与 Emacs face 的 :height 属性一致。
Emacs 的 :height 浮点数表示相对于默认字体的缩放比例。"
  (cond
   ;; 数值类型：直接作为相对高度（支持 etaf-tag.el 中的数值定义）
   ((numberp css-size) (float css-size))
   ;; 字符串类型
   ((stringp css-size)
    (let ((size (string-trim css-size)))
      (cond
       ;; 像素值：转换为相对高度
       ((string-match "^\\([0-9.]+\\)px$" size)
        (let ((px (string-to-number (match-string 1 size))))
          (/ px etaf-css-baseline-font-size)))
       ;; em 值：直接作为相对高度
       ((string-match "^\\([0-9.]+\\)em$" size)
        (float (string-to-number (match-string 1 size))))
       ;; rem 值：作为相对高度
       ((string-match "^\\([0-9.]+\\)rem$" size)
        (float (string-to-number (match-string 1 size))))
       ;; 百分比
       ((string-match "^\\([0-9.]+\\)%$" size)
        (/ (string-to-number (match-string 1 size)) 100.0))
       ;; CSS 绝对关键字：基于 16px medium，相邻比例约为 1.2
       ;; https://www.w3.org/TR/css-fonts-3/#absolute-size-value
       ((string= size "xx-small") 0.5625)   ; 9px / 16px
       ((string= size "x-small") 0.625)     ; 10px / 16px
       ((string= size "small") 0.8125)      ; 13px / 16px
       ((string= size "medium") 1.0)        ; 16px / 16px (基准)
       ((string= size "large") 1.125)       ; 18px / 16px
       ((string= size "x-large") 1.5)       ; 24px / 16px
       ((string= size "xx-large") 2.0)      ; 32px / 16px
       ;; CSS 相对关键字：根据当前字体大小调整
       ;; smaller/larger 使用常见的缩放因子
       ((string= size "smaller") 0.833)     ; 约 1/1.2，缩小一级
       ((string= size "larger") 1.2)        ; 放大一级
       (t nil))))
   (t nil)))

;;; 主转换函数

(defun etaf-css-style-to-face (computed-style)
  "将 CSS 计算样式转换为 Emacs face plist。
COMPUTED-STYLE 是 CSS 计算样式 alist，如 ((color . \"red\") (font-weight . \"bold\"))。
返回 Emacs face plist，如 (:foreground \"red\" :weight bold)。"
  (let ((face nil))
    ;; 处理 color
    (when-let ((color (cdr (assq 'color computed-style))))
      (when-let ((emacs-color (etaf-css-color-to-emacs color)))
        (setq face (plist-put face :foreground emacs-color))))
    
    ;; 处理 background-color
    (when-let ((bgcolor (cdr (assq 'background-color computed-style))))
      (when-let ((emacs-color (etaf-css-color-to-emacs bgcolor)))
        (setq face (plist-put face :background emacs-color))))
    
    ;; 处理 font-weight
    (when-let ((weight (cdr (assq 'font-weight computed-style))))
      (when-let ((emacs-weight (etaf-css-font-weight-to-emacs weight)))
        (setq face (plist-put face :weight emacs-weight))))
    
    ;; 处理 font-style
    (when-let ((style (cdr (assq 'font-style computed-style))))
      (when-let ((emacs-slant (etaf-css-font-style-to-emacs style)))
        (setq face (plist-put face :slant emacs-slant))))
    
    ;; 处理 text-decoration
    (when-let ((decoration (cdr (assq 'text-decoration computed-style))))
      (let ((decoration-face (etaf-css-text-decoration-to-emacs decoration)))
        (dolist (prop '(:underline :strike-through :overline))
          (when (plist-get decoration-face prop)
            (setq face (plist-put face prop (plist-get decoration-face prop)))))))
    
    ;; 处理 font-size
    (when-let ((size (cdr (assq 'font-size computed-style))))
      (when-let ((emacs-height (etaf-css-font-size-to-emacs size)))
        (setq face (plist-put face :height emacs-height))))
    
    ;; 处理 font-family
    (when-let ((family (cdr (assq 'font-family computed-style))))
      ;; 提取第一个字体族名称
      (when (string-match "^\\([^,]+\\)" family)
        (let ((font-name (string-trim (match-string 1 family))))
          ;; 移除引号
          (setq font-name (replace-regexp-in-string "['\"]" "" font-name))
          (setq face (plist-put face :family font-name)))))
    
    face))

(defun etaf-css-apply-face-to-string (string computed-style)
  "将 CSS 计算样式应用到字符串上。
STRING 是要添加 face 属性的字符串。
COMPUTED-STYLE 是 CSS 计算样式 alist。
返回带有 face 文本属性的新字符串。"
  (let ((face (etaf-css-style-to-face computed-style)))
    (if (and face (> (length string) 0))
        (let ((result (copy-sequence string)))
          (add-face-text-property 0 (length result) face t result)
          result)
      string)))

(provide 'etaf-css-face)
;;; etaf-css-face.el ends here
