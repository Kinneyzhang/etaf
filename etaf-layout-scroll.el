;;; etaf-layout-scroll.el --- Scroll bar for layout rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, scroll-bar
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 布局系统滚动条模块
;;
;; 本模块提供布局系统中滚动条的创建和渲染功能。
;; 所有函数使用 `etaf-layout-scroll-' 前缀。
;;
;; 滚动条数据结构:
;;   (:track-height <number>           ; 轨道高度（行数）
;;    :track-color <color>             ; 轨道颜色
;;    :track-padding-left-pixel <n>    ; 轨道左 padding（像素）
;;    :track-padding-right-pixel <n>   ; 轨道右 padding（像素）
;;    :track-padding-top-height <n>    ; 轨道上 padding（行数）
;;    :track-padding-bottom-height <n> ; 轨道下 padding（行数）
;;    :thumb-pixel <number>            ; 滑块宽度（像素）
;;    :thumb-height <number>           ; 滑块高度（行数）
;;    :thumb-offset <number>           ; 滑块偏移量
;;    :thumb-color <color>             ; 滑块颜色
;;    :thumb-border-p <boolean>        ; 是否有边框
;;    :thumb-border-color <color>)     ; 边框颜色
;;
;; 公共接口：
;; - `etaf-layout-scroll-bar-create' - 创建滚动条配置
;; - `etaf-layout-scroll-bar-pixel' - 获取滚动条总宽度（像素）
;; - `etaf-layout-scroll-bar-render' - 渲染滚动条为字符串
;;
;; 滚动条风格定义：
;; - `etaf-layout-scroll-bar-alist' - 存储不同风格滚动条的配置
;; - `etaf-layout-scroll-bar-define' - 定义新的滚动条风格

;;; Code:

(require 'cl-lib)
(require 'etaf-utils)

;;; ============================================================
;;; 滚动条风格定义
;;; ============================================================

(defvar etaf-layout-scroll-bar-alist nil
  "存储不同风格滚动条的定义的 alist。
car 是滚动条的名称（符号），cdr 是滚动条的 plist 配置。")

(defmacro etaf-layout-scroll-bar-define (name &rest kvs)
  "定义不同风格滚动条。
NAME 是滚动条风格名称（符号）。
KVS 是滚动条的配置键值对，值会在定义时被求值。"
  (declare (indent defun))
  `(etaf-alist-set etaf-layout-scroll-bar-alist ',name (list ,@kvs)))

;; 预定义的滚动条风格
(etaf-layout-scroll-bar-define simple
  :thumb-pixel 2
  :track-padding-left-pixel 0
  :track-padding-right-pixel 3)

(etaf-layout-scroll-bar-define s2
  :thumb-pixel 2
  :track-border-left-pixel 1
  :track-padding-left-pixel 12
  :track-padding-right-pixel 12
  :track-border-right-pixel 1)

;;; ============================================================
;;; 滚动条创建
;;; ============================================================

(defun etaf-layout-scroll-bar-create (&optional type)
  "创建滚动条配置 plist。
TYPE 是可选的滚动条风格类型（符号），用于引用 `etaf-layout-scroll-bar-alist'。"
  (let ((scroll-bar
         (list :track-height 1
               :track-color (face-attribute 'default :background)
               :track-margin-left-pixel 0
               :track-margin-right-pixel 0
               :track-padding-left-pixel 0
               :track-padding-right-pixel 0
               :track-padding-top-height 0
               :track-padding-bottom-height 0
               :track-border-left-pixel 0
               :track-border-left-color nil
               :track-border-right-pixel 0
               :track-border-right-color nil
               :track-border-top-p nil
               :track-border-top-color nil
               :track-border-bottom-p nil
               :track-border-bottom-color nil
               :thumb-offset 0
               :thumb-height 1
               :thumb-pixel 2
               :thumb-border-p nil
               :thumb-border-color (face-attribute 'default :foreground)
               :thumb-color (face-attribute 'default :foreground))))
    ;; 如果有定义的风格，应用风格设置
    (when-let* ((type type)
                (kvs (copy-sequence (alist-get type etaf-layout-scroll-bar-alist))))
      (while kvs
        (let ((key (pop kvs))
              (val (pop kvs)))
          (setq scroll-bar (plist-put scroll-bar key val)))))
    scroll-bar))

;;; ============================================================
;;; 滚动条属性访问器
;;; ============================================================

(defun etaf-layout-scroll-bar-pixel (scroll-bar &optional border-box-p)
  "获取滚动栏所占的总像素宽度。
SCROLL-BAR 是滚动条配置 plist。
BORDER-BOX-P 为 t 时表示不包含 margin。"
  (let ((border-box-pixel
         (+ (or (plist-get scroll-bar :track-padding-left-pixel) 0)
            (or (plist-get scroll-bar :track-padding-right-pixel) 0)
            (or (plist-get scroll-bar :track-border-left-pixel) 0)
            (or (plist-get scroll-bar :track-border-right-pixel) 0)
            (or (plist-get scroll-bar :thumb-pixel) 2))))
    (if border-box-p
        border-box-pixel
      (+ border-box-pixel
         (or (plist-get scroll-bar :track-margin-left-pixel) 0)
         (or (plist-get scroll-bar :track-margin-right-pixel) 0)))))

;;; ============================================================
;;; 滚动条渲染
;;; ============================================================

(defun etaf-layout-scroll--track-face (scroll-bar)
  "获取滚动条轨道部分的 face。
SCROLL-BAR 是滚动条配置 plist。"
  (let ((color (plist-get scroll-bar :track-color)))
    `(:background ,color
      ,@(when (plist-get scroll-bar :thumb-border-p)
          `(:box (:line-width (1 . 0) :color ,color))))))

(defun etaf-layout-scroll--thumb-face (scroll-bar idx)
  "获取滚动条滑块的 face。
SCROLL-BAR 是滚动条配置 plist。
IDX 是相对于滚动条首行的偏移量。"
  (let ((border-color (plist-get scroll-bar :thumb-border-color))
        (thumb-height (plist-get scroll-bar :thumb-height))
        (thumb-color (plist-get scroll-bar :thumb-color)))
    `(:background ,thumb-color
      ,@(when (plist-get scroll-bar :thumb-border-p)
          (if (= 1 thumb-height)
              ;; 滚动条高度为1时有边框，直接使用 :box 属性
              `(:box (:color ,border-color))
            ;; 其余情况，分别使用 :overline 和 :underline 表示上下边框
            ;; 0 表示首行, (1- thumb-height) 表示尾行，中间的表示中间行
            `(:box (:line-width (1 . 0) :color ,border-color)
              ,@(cond
                 ((= idx 0) `(:overline ,border-color))
                 ((= idx (1- thumb-height))
                  `(:underline (:position t :color ,border-color)))
                 ((< 0 idx (1- thumb-height)) nil)
                 (t nil))))))))

(defun etaf-layout-scroll--render-track-with-thumb (scroll-bar)
  "渲染滚动条轨道和滑块（不含 padding）。
SCROLL-BAR 是滚动条配置 plist。
返回渲染后的字符串。"
  (let* ((track-height (or (plist-get scroll-bar :track-height) 1))
         (thumb-offset (or (plist-get scroll-bar :thumb-offset) 0))
         (thumb-height (or (plist-get scroll-bar :thumb-height) 1))
         (thumb-pixel (or (plist-get scroll-bar :thumb-pixel) 2))
         (below-height (- track-height (+ thumb-offset thumb-height))))
    ;; track-height should >= thumb-height + thumb-offset
    (when (< below-height 0)
      (error "track-height should >= thumb-height + thumb-offset"))
    (let* (;; 渲染全高的轨道
           (basic-track-str
            (propertize
             (etaf-pixel-blank thumb-pixel track-height)
             'face (etaf-layout-scroll--track-face scroll-bar)))
           ;; 在轨道中标记出滑块部分
           (track-thumb-str
            (with-temp-buffer
              (insert basic-track-str)
              (goto-char (point-min))
              (forward-line thumb-offset)
              ;; 依次设置滑块每一行的 face
              (dotimes (idx thumb-height)
                (add-text-properties
                 (line-beginning-position) (line-end-position)
                 `(face ,(etaf-layout-scroll--thumb-face scroll-bar idx)))
                (forward-line 1))
              (buffer-string))))
      track-thumb-str)))

(defun etaf-layout-scroll-bar-render (scroll-bar &optional _box-uuid _scroll-steps)
  "渲染滚动条为字符串。
SCROLL-BAR 是滚动条配置 plist。
_BOX-UUID 和 _SCROLL-STEPS 是兼容参数，当前未使用。

返回渲染后的滚动条字符串。"
  (let* ((track-color (or (plist-get scroll-bar :track-color)
                          (face-attribute 'default :background)))
         (track-height (or (plist-get scroll-bar :track-height) 1))
         (padding-left (or (plist-get scroll-bar :track-padding-left-pixel) 0))
         (padding-right (or (plist-get scroll-bar :track-padding-right-pixel) 0))
         (padding-top (or (plist-get scroll-bar :track-padding-top-height) 0))
         (padding-bottom (or (plist-get scroll-bar :track-padding-bottom-height) 0))
         (inner-height (+ track-height padding-top padding-bottom))
         ;; 渲染轨道和滑块
         (track-thumb-str (etaf-layout-scroll--render-track-with-thumb scroll-bar))
         ;; 添加垂直 padding
         (with-v-padding
          (if (or (> padding-top 0) (> padding-bottom 0))
              (let* ((thumb-pixel (or (plist-get scroll-bar :thumb-pixel) 2)))
                (etaf-lines-stack
                 (list (when (> padding-top 0)
                         (etaf-pixel-blank thumb-pixel padding-top))
                       track-thumb-str
                       (when (> padding-bottom 0)
                         (etaf-pixel-blank thumb-pixel padding-bottom)))))
            track-thumb-str))
         ;; 添加水平 padding
         (with-h-padding
          (if (or (> padding-left 0) (> padding-right 0))
              (etaf-lines-concat
               (list (when (> padding-left 0)
                       (etaf-pixel-blank padding-left inner-height))
                     with-v-padding
                     (when (> padding-right 0)
                       (etaf-pixel-blank padding-right inner-height))))
            with-v-padding))
         ;; 应用背景色到整个滚动条区域
         (with-bgcolor
          (if track-color
              (etaf-layout-scroll--apply-bgcolor with-h-padding track-color)
            with-h-padding)))
    with-bgcolor))

(defun etaf-layout-scroll--apply-bgcolor (string bgcolor)
  "为字符串的每一行应用背景色（不包括换行符）。
STRING 是要处理的字符串。
BGCOLOR 是背景色。"
  (let ((lines (split-string string "\n")))
    (mapconcat
     (lambda (line)
       (let ((result (copy-sequence line)))
         (when (> (length result) 0)
           (add-face-text-property 0 (length result)
                                   `(:background ,bgcolor)
                                   t result))
         result))
     lines
     "\n")))

(provide 'etaf-layout-scroll)
;;; etaf-layout-scroll.el ends here
