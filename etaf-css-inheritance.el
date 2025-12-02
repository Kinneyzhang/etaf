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

(defun etaf-css--remove-duplicate-properties (style-alist)
  "从样式 alist 中移除重复的属性，只保留每个属性的第一次出现。
STYLE-ALIST 是 ((property . value) ...) 格式的样式 alist。
返回一个新的 alist，其中每个属性只出现一次。

这个函数确保在继承过程中不会累积重复的属性条目。
例如：'((color . \"red\") (font-weight . \"bold\") (color . \"blue\"))
      => '((color . \"red\") (font-weight . \"bold\"))"
  (let ((seen (make-hash-table :test 'eq))
        (result '()))
    (dolist (prop style-alist)
      (let ((key (car prop)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push prop result))))
    (nreverse result)))

(defun etaf-css-apply-inheritance (computed-style parent-style)
  "将父元素的可继承属性应用到子元素。
COMPUTED-STYLE 是子元素的计算样式。
PARENT-STYLE 是父元素的计算样式。
返回合并后的样式，确保没有重复的属性条目。

修复问题：在父元素上设置的文本相关样式（如字体、加粗等）在继承时
可能会导致子元素中有多个重复的样式。此函数现在会移除所有重复的
属性条目，只保留每个属性的第一次出现。"
  (let ((result (copy-sequence computed-style)))
    ;; 对于每个可继承的属性，如果子元素没有定义，则从父元素继承
    (dolist (prop etaf-css-inherited-properties)
      (when-let ((parent-value (cdr (assq prop parent-style))))
        (unless (assq prop result)
          (push (cons prop parent-value) result))))
    ;; 移除重复的属性条目
    ;; 这确保了即使 computed-style 或继承过程中引入了重复，
    ;; 最终结果也不会包含重复的属性
    (etaf-css--remove-duplicate-properties result)))

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
