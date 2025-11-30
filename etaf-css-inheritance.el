;;; etaf-css-inheritance.el --- CSS Property Inheritance -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, inheritance
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 属性继承系统
;;
;; 实现 CSS 属性从父元素到子元素的继承机制。
;; 某些属性（如 color, font-family）会自动继承，
;; 而其他属性（如 border, margin）则不会。

;;; Code:

(require 'cl-lib)

(defconst etaf-css-inherited-properties
  '(color
    font-family
    font-size
    font-style
    font-variant
    font-weight
    font
    letter-spacing
    line-height
    list-style
    list-style-image
    list-style-position
    list-style-type
    text-align
    text-indent
    text-transform
    visibility
    white-space
    word-spacing
    cursor
    direction
    quotes)
  "会自动继承的 CSS 属性列表。")

(defun etaf-css-property-inherits-p (property)
  "检查指定的 CSS 属性是否会自动继承。
PROPERTY 是属性名（symbol）。"
  (memq property etaf-css-inherited-properties))

(defun etaf-css-apply-inheritance (computed-style parent-style)
  "将父元素的可继承属性应用到子元素。
COMPUTED-STYLE 是子元素的计算样式。
PARENT-STYLE 是父元素的计算样式。
返回合并后的样式。"
  (let ((result (copy-sequence computed-style)))
    ;; 对于每个可继承的属性，如果子元素没有定义，则从父元素继承
    (dolist (prop etaf-css-inherited-properties)
      (when-let ((parent-value (cdr (assq prop parent-style))))
        (unless (assq prop result)
          (push (cons prop parent-value) result))))
    result))

(defun etaf-css-get-parent-style (node dom cssom)
  "获取节点的父元素的计算样式。
NODE 是当前节点。
DOM 是根 DOM 节点。
CSSOM 是 CSS 对象模型。
返回父元素的计算样式，如果没有父元素则返回 nil。"
  (when-let ((parent (dom-parent dom node)))
    ;; 注意：这里需要递归计算父元素的样式
    ;; 在实际实现中，这应该通过缓存来优化
    nil))  ; 将在 etaf-css-cascade.el 中实现完整逻辑

(provide 'etaf-css-inheritance)
;;; etaf-css-inheritance.el ends here
