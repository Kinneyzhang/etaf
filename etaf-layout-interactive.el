;;; etaf-layout-interactive.el --- Interactive commands for ETAF layout -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, interactive, scrolling
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 本模块提供布局系统的交互命令，包括:
;;
;; 1. 滚动命令 - 参考 etaf-box.el 实现在 buffer 中的实际滚动
;;    - `etaf-layout-scroll-up' - 向上滚动内容
;;    - `etaf-layout-scroll-down' - 向下滚动内容
;;    - 支持鼠标滚轮和键盘操作
;;
;; 2. 增量更新 API - 用于在不完全重渲染的情况下更新 buffer 内容
;;    - `etaf-buffer-update-text' - 更新特定区域的文本
;;    - `etaf-buffer-update-style' - 更新特定区域的样式
;;    - `etaf-buffer-update-region' - 更新指定 uuid 区域的内容
;;
;; 使用方式:
;;
;; 1. 滚动功能在渲染时自动启用，当鼠标悬停在可滚动区域时:
;;    - 使用 "n" 或鼠标滚轮向下 滚动内容向下
;;    - 使用 "p" 或鼠标滚轮向上 滚动内容向上
;;
;; 2. 增量更新示例:
;;    ```elisp
;;    ;; 更新特定 uuid 区域的文本内容
;;    (etaf-buffer-update-text buffer uuid "新文本内容")
;;
;;    ;; 更新特定 uuid 区域的样式
;;    (etaf-buffer-update-style buffer uuid '((color . "red")))
;;    ```
;;
;; 数据结构:
;;
;; 每个可滚动区域在渲染时会通过文本属性存储以下信息:
;; - `etaf-layout-scroll-area' - 标识可滚动区域的 uuid
;; - `etaf-layout-content-line' - 标识内容行的 uuid
;; - `etaf-layout-scroll-thumb' - 标识滚动条滑块
;;
;; 缓存数据存储在 buffer-local 变量 `etaf-layout-caches' 中:
;; - :content-lines - 原始内容行列表
;; - :content-linum - 原始内容行数
;; - :content-height - 显示高度
;; - :v-scroll-offset - 当前滚动偏移量
;; - :v-scroll-steps - 滚动步长列表

;;; Code:

(require 'cl-lib)
(require 'etaf-utils)
(require 'etaf-layout-scroll)

;;; ============================================================
;;; 变量定义
;;; ============================================================

(defvar etaf-layout-caches nil
  "Buffer-local hash table 用于缓存布局信息。
键是 uuid，值是 plist 包含渲染和滚动所需的数据。")

(defvar etaf-layout-scroll-down-keys '("n" [wheel-down])
  "向下滚动的键绑定列表。")

(defvar etaf-layout-scroll-up-keys '("p" [wheel-up])
  "向上滚动的键绑定列表。")

;;; ============================================================
;;; 缓存管理
;;; ============================================================

(defun etaf-layout-caches-init (&optional buffer-or-name)
  "初始化 BUFFER-OR-NAME 中的 `etaf-layout-caches'。
如果不提供参数，则在当前 buffer 中初始化。"
  (with-current-buffer (or (and buffer-or-name
                                (get-buffer-create buffer-or-name))
                           (current-buffer))
    (unless etaf-layout-caches
      (setq-local
       etaf-layout-caches
       (make-hash-table
        :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))))

(defun etaf-layout-cache-get (uuid &optional key)
  "获取 UUID 对应的缓存数据。
如果提供 KEY，返回缓存 plist 中对应的值；否则返回整个 plist。"
  (when etaf-layout-caches
    (let ((cache (gethash uuid etaf-layout-caches)))
      (if key
          (plist-get cache key)
        cache))))

(defun etaf-layout-cache-set (uuid key value)
  "设置 UUID 对应缓存中 KEY 的值为 VALUE。"
  (when etaf-layout-caches
    (let ((cache (or (gethash uuid etaf-layout-caches) '())))
      (puthash uuid (plist-put cache key value) etaf-layout-caches))))

(defun etaf-layout-cache-put (uuid cache-plist)
  "将 CACHE-PLIST 存储为 UUID 的缓存数据。"
  (when etaf-layout-caches
    (puthash uuid cache-plist etaf-layout-caches)))

;;; ============================================================
;;; Keymap
;;; ============================================================

(defun etaf-layout-scroll-map ()
  "创建并返回滚动区域的 keymap。"
  (let ((map (make-sparse-keymap)))
    (dolist (key etaf-layout-scroll-down-keys)
      (define-key map key #'etaf-layout-scroll-down))
    (dolist (key etaf-layout-scroll-up-keys)
      (define-key map key #'etaf-layout-scroll-up))
    map))

;;; ============================================================
;;; 滚动条信息计算
;;; ============================================================

(defun etaf-layout-v-scroll-steps (content-linum content-height)
  "计算滚动步长列表。
CONTENT-LINUM 是内容总行数，CONTENT-HEIGHT 是显示高度。
返回每次滚动对应的行数列表。"
  (let ((overflow-linum (- content-linum content-height)))
    (if (> overflow-linum 0)
        (if (> content-height overflow-linum)
            ;; 盒子内容高度 > 溢出行数，每次滚动一行
            (make-list overflow-linum 1)
          ;; 内容高度 <= 溢出高度，平摊滚动
          (when (> content-height 1)
            (etaf-split-size overflow-linum (1- content-height) nil nil nil t)))
      nil)))

(defun etaf-layout-v-scroll-thumb-offset (scroll-steps scroll-offset)
  "根据 SCROLL-STEPS 和 SCROLL-OFFSET 计算滑块偏移量。
用于实时滚动时确定滑块位置。"
  (let ((prefixs (let ((prefix 0))
                   (mapcar (lambda (n)
                             (setq prefix (+ prefix n)))
                           scroll-steps))))
    (or (etaf--num-in-prefixs scroll-offset prefixs)
        0)))

(defun etaf-layout-v-scroll-thumb-height (content-linum content-height)
  "计算滚动条滑块高度。
CONTENT-LINUM 是内容总行数，CONTENT-HEIGHT 是显示高度。"
  (let ((overflow-linum (- content-linum content-height)))
    (if (> overflow-linum 0)
        (if (> content-height overflow-linum)
            (- content-height overflow-linum)
          1)
      content-height)))

;;; ============================================================
;;; 滚动命令实现
;;; ============================================================

(defun etaf-layout-scroll (direction &optional n)
  "滚动当前位置的可滚动区域。
DIRECTION 是滚动方向，'up 或 'down。
N 是可选的滚动行数，默认为 1。"
  (interactive)
  (when-let* ((props (text-properties-at (point)))
              (uuid (plist-get props 'etaf-layout-content-line))
              (cache (gethash uuid etaf-layout-caches))
              (content-lines (plist-get cache :content-lines))
              (content-linum (plist-get cache :content-linum))
              (content-height (plist-get cache :content-height))
              (overflow-linum (- content-linum content-height))
              ((> overflow-linum 0))  ;; 只有溢出时才能滚动
              (v-scroll-offset
               (max 0 (min (or (plist-get cache :v-scroll-offset) 0)
                           overflow-linum)))
              (v-scroll-steps (or (plist-get cache :v-scroll-steps)
                                  (etaf-layout-v-scroll-steps
                                   content-linum content-height))))
    (let* ((thumb-offset (etaf-layout-v-scroll-thumb-offset
                          v-scroll-steps v-scroll-offset))
           (new-v-scroll-offset
            (cond ((eq 'down direction)
                   (min overflow-linum (+ v-scroll-offset (or n 1))))
                  ((eq 'up direction)
                   (max 0 (- v-scroll-offset (or n 1))))))
           (new-thumb-offset (etaf-layout-v-scroll-thumb-offset
                              v-scroll-steps new-v-scroll-offset))
           (box-content-lines
            (seq-subseq content-lines
                        new-v-scroll-offset
                        (min (length content-lines)
                             (+ new-v-scroll-offset content-height)))))
      ;; 更新缓存中的滚动偏移量
      (plist-put cache :v-scroll-offset new-v-scroll-offset)
      
      ;; 更新内容和滚动条
      (let* ((padding-top-height (or (plist-get cache :padding-top-height) 0))
             (padding-bottom-height (or (plist-get cache :padding-bottom-height) 0))
             (border-top-p (plist-get cache :border-top-p))
             (border-top-color (plist-get cache :border-top-color))
             (border-bottom-p (plist-get cache :border-bottom-p))
             (border-bottom-color (plist-get cache :border-bottom-color))
             (thumb-height (etaf-layout-v-scroll-thumb-height
                            content-linum content-height))
             (inhibit-read-only t))
        
        ;; 更新滚动条位置（如果滑块需要移动）
        (when (/= new-thumb-offset thumb-offset)
          (save-excursion
            (goto-char (point-min))
            (let ((offset (abs (- new-thumb-offset thumb-offset))))
              (dotimes (_ offset)
                (etaf-layout-scroll-thumb-shift
                 direction uuid thumb-height
                 new-thumb-offset overflow-linum
                 border-top-p border-top-color
                 border-bottom-p border-bottom-color
                 padding-top-height padding-bottom-height)))))
        
        ;; 更新内容区域
        (etaf-property-map-regions
         (lambda (start end idx)
           (when (< idx (length box-content-lines))
             (let ((line (propertize (nth idx box-content-lines)
                                     'etaf-layout-content-line uuid
                                     'keymap (etaf-layout-scroll-map))))
               ;; 处理边框
               (cond ((and (= idx 0)
                           (< padding-top-height 1)
                           border-top-p)
                      (setq line (etaf-propertize-overline
                                  line border-top-color)))
                     ((and (= idx (1- content-height))
                           (< padding-bottom-height 1)
                           border-bottom-p)
                      (setq line (etaf-propertize-underline
                                  line border-bottom-color))))
               (etaf-region-replace line start end))))
         'etaf-layout-content-line uuid t)))))

(defun etaf-layout-scroll-thumb-shift (direction uuid thumb-height
                                                 new-thumb-offset overflow-linum
                                                 border-top-p border-top-color
                                                 border-bottom-p border-bottom-color
                                                 padding-top-height padding-bottom-height)
  "移动滚动条滑块一格。
DIRECTION 是方向 'up 或 'down。
UUID 是滚动区域的标识符。
THUMB-HEIGHT 是滑块高度。
其余参数用于处理边框和 padding。"
  (let* ((cons (cond
                ((= thumb-height 1)
                 (etaf-layout-scroll-1-height-thumb-shift direction uuid))
                ((= thumb-height 2)
                 (etaf-layout-scroll-2-height-thumb-shift direction uuid))
                ((> thumb-height 2)
                 (etaf-layout-scroll-n-height-thumb-shift direction uuid))))
         (old-region (car cons))
         (new-region (cdr cons)))
    (when (and old-region new-region)
      ;; 处理边框恢复
      (when (and (= new-thumb-offset 1)
                 (eq direction 'down)
                 (or border-top-p)
                 (< padding-top-height 1))
        (add-face-text-property
         (car old-region) (cdr old-region)
         `(:overline ,border-top-color)))
      
      (when (and (eq direction 'up)
                 (= new-thumb-offset (1- overflow-linum))
                 (or border-bottom-p)
                 (< padding-bottom-height 1))
        (add-face-text-property
         (car old-region) (cdr old-region)
         `(:underline (:position t :color ,border-bottom-color)))))))

(defun etaf-layout-scroll-1-height-thumb-shift (direction uuid)
  "滑块高度为 1 时的移动逻辑。
返回 (old-region . new-region)。"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-region (etaf-property-forward-region
                          'etaf-layout-scroll-thumb uuid t))
           (thumb-start (car thumb-region))
           (thumb-end (cdr thumb-region)))
      (when thumb-region
        (cond
         ((eq 'down direction)
          (let ((next-region
                 (progn (goto-char thumb-end)
                        (etaf-property-forward-region
                         'etaf-layout-scroll-area uuid t))))
            (when next-region
              (etaf-region-swap thumb-region next-region)
              (cons thumb-region next-region))))
         ((eq 'up direction)
          (let ((prev-region
                 (progn (goto-char thumb-start)
                        (etaf-property-backward-region
                         'etaf-layout-scroll-area uuid t))))
            (when prev-region
              (etaf-region-swap prev-region thumb-region)
              (cons thumb-region prev-region)))))))))

(defun etaf-layout-scroll-2-height-thumb-shift (direction uuid)
  "滑块高度为 2 时的移动逻辑。
返回 (old-region . new-region)。"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-head-region (etaf-property-forward-region
                               'etaf-layout-scroll-thumb-head uuid t))
           (thumb-head-start (car thumb-head-region))
           (thumb-head-end (cdr thumb-head-region))
           (thumb-tail-region (etaf-property-forward-region
                               'etaf-layout-scroll-thumb-tail uuid t))
           (thumb-tail-start (car thumb-tail-region))
           (thumb-tail-end (cdr thumb-tail-region)))
      (when (and thumb-head-region thumb-tail-region)
        (cond
         ((eq 'down direction)
          (let ((tail-next-region
                 (progn (goto-char thumb-tail-end)
                        (etaf-property-forward-region
                         'etaf-layout-scroll-area uuid t))))
            (when tail-next-region
              (etaf-region-swap thumb-tail-region tail-next-region)
              (etaf-region-swap thumb-head-region thumb-tail-region)
              (cons thumb-head-region thumb-tail-region))))
         ((eq 'up direction)
          (let ((head-prev-region
                 (progn (goto-char thumb-head-start)
                        (etaf-property-backward-region
                         'etaf-layout-scroll-area uuid t))))
            (when head-prev-region
              (etaf-region-swap head-prev-region thumb-head-region)
              (etaf-region-swap thumb-head-region thumb-tail-region)
              (cons thumb-tail-region thumb-head-region)))))))))

(defun etaf-layout-scroll-n-height-thumb-shift (direction uuid)
  "滑块高度 > 2 时的移动逻辑。
返回 (old-region . new-region)。"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-head-region (etaf-property-forward-region
                               'etaf-layout-scroll-thumb-head uuid t))
           (thumb-head-start (car thumb-head-region))
           (thumb-head-end (cdr thumb-head-region))
           (thumb-tail-region (etaf-property-forward-region
                               'etaf-layout-scroll-thumb-tail uuid t))
           (thumb-tail-start (car thumb-tail-region))
           (thumb-tail-end (cdr thumb-tail-region)))
      (when (and thumb-head-region thumb-tail-region)
        (cond
         ((eq 'down direction)
          (let* ((head-next-region
                  (progn (goto-char thumb-head-end)
                         (etaf-property-forward-region
                          'etaf-layout-scroll-area uuid t)))
                 (tail-next-region
                  (progn (goto-char thumb-tail-end)
                         (etaf-property-forward-region
                          'etaf-layout-scroll-area uuid t))))
            (when (and head-next-region tail-next-region)
              (etaf-region-swap thumb-head-region head-next-region)
              (etaf-region-swap thumb-tail-region tail-next-region)
              (etaf-region-swap thumb-head-region thumb-tail-region)
              (cons thumb-head-region head-next-region))))
         ((eq 'up direction)
          (let* ((head-prev-region
                  (progn (goto-char thumb-head-start)
                         (etaf-property-backward-region
                          'etaf-layout-scroll-area uuid t)))
                 (tail-prev-region
                  (progn (goto-char thumb-tail-start)
                         (etaf-property-backward-region
                          'etaf-layout-scroll-area uuid t))))
            (when (and head-prev-region tail-prev-region)
              (etaf-region-swap head-prev-region thumb-head-region)
              (etaf-region-swap tail-prev-region thumb-tail-region)
              (etaf-region-swap thumb-head-region thumb-tail-region)
              (cons thumb-tail-region tail-prev-region)))))))))

;;;###autoload
(defun etaf-layout-scroll-up (&optional n)
  "向上滚动当前位置的可滚动区域。
N 是可选的滚动行数，默认为 1。"
  (interactive "p")
  (etaf-layout-scroll 'up (or n 1)))

;;;###autoload
(defun etaf-layout-scroll-down (&optional n)
  "向下滚动当前位置的可滚动区域。
N 是可选的滚动行数，默认为 1。"
  (interactive "p")
  (etaf-layout-scroll 'down (or n 1)))

;;; ============================================================
;;; 增量更新 API
;;; ============================================================

(defun etaf-buffer-find-region-by-uuid (uuid &optional property)
  "在当前 buffer 中查找具有指定 UUID 的区域。
PROPERTY 是用于查找的属性名，默认为 `etaf-layout-content-line'。
返回 (start . end) 或 nil。"
  (let ((prop (or property 'etaf-layout-content-line))
        (regions nil))
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward prop uuid t))
                  (start (prop-match-beginning match))
                  (end (prop-match-end match)))
        (push (cons start end) regions)))
    (when regions
      (cons (caar (last regions))
            (cdar regions)))))

(defun etaf-buffer-update-text (buffer uuid new-text)
  "在 BUFFER 中更新 UUID 标识区域的文本内容。
NEW-TEXT 是新的文本内容。
保留原有的文本属性。

这是增量更新的核心函数，避免了完全重渲染的开销。

返回 t 如果更新成功，nil 如果找不到区域。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region))
                (end (cdr region)))
      (let ((inhibit-read-only t)
            (props (text-properties-at start)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert (apply #'propertize new-text props)))
        t))))

(defun etaf-buffer-update-style (buffer uuid style-alist)
  "在 BUFFER 中更新 UUID 标识区域的样式。
STYLE-ALIST 是 CSS 样式 alist，如 ((color . \"red\") (background . \"blue\"))。

此函数将 CSS 样式转换为 Emacs face 属性并应用到文本上。

返回 t 如果更新成功，nil 如果找不到区域。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region))
                (end (cdr region)))
      (let ((inhibit-read-only t)
            (face-spec (etaf-buffer--style-to-face style-alist)))
        (add-face-text-property start end face-spec)
        t))))

(defun etaf-buffer--style-to-face (style-alist)
  "将 CSS 样式 alist 转换为 Emacs face 属性列表。
STYLE-ALIST 是 ((property . value) ...) 格式的列表。"
  (let ((face-spec '()))
    (dolist (pair style-alist)
      (let ((prop (car pair))
            (val (cdr pair)))
        (pcase prop
          ('color
           (setq face-spec (plist-put face-spec :foreground val)))
          ('background-color
           (setq face-spec (plist-put face-spec :background val)))
          ('background
           (setq face-spec (plist-put face-spec :background val)))
          ('font-weight
           (when (or (string= val "bold") (eq val 'bold))
             (setq face-spec (plist-put face-spec :weight 'bold))))
          ('font-style
           (when (or (string= val "italic") (eq val 'italic))
             (setq face-spec (plist-put face-spec :slant 'italic))))
          ('text-decoration
           (cond
            ((or (string= val "underline") (eq val 'underline))
             (setq face-spec (plist-put face-spec :underline t)))
            ((or (string= val "overline") (eq val 'overline))
             (setq face-spec (plist-put face-spec :overline t)))
            ((or (string= val "line-through") (eq val 'line-through))
             (setq face-spec (plist-put face-spec :strike-through t))))))))
    face-spec))

(defun etaf-buffer-update-region (buffer uuid new-content &optional preserve-props)
  "在 BUFFER 中完全替换 UUID 标识区域的内容。
NEW-CONTENT 是新的字符串内容（可能已经带有属性）。
如果 PRESERVE-PROPS 为 t，保留原有区域的文本属性。

此函数用于需要完全替换内容的场景。

返回 t 如果更新成功，nil 如果找不到区域。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region))
                (end (cdr region)))
      (let ((inhibit-read-only t)
            (old-props (when preserve-props
                         (text-properties-at start))))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert (if (and preserve-props old-props)
                      (apply #'propertize new-content old-props)
                    new-content)))
        t))))

(defun etaf-buffer-update-attribute (buffer uuid attribute value)
  "在 BUFFER 中更新 UUID 标识区域的单个文本属性。
ATTRIBUTE 是属性名称（symbol）。
VALUE 是新的属性值。

这是最细粒度的增量更新函数。

返回 t 如果更新成功，nil 如果找不到区域。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region))
                (end (cdr region)))
      (let ((inhibit-read-only t))
        (put-text-property start end attribute value)
        t))))

(defun etaf-buffer-get-region-text (buffer uuid)
  "获取 BUFFER 中 UUID 标识区域的文本内容。
返回文本字符串或 nil。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region))
                (end (cdr region)))
      (buffer-substring-no-properties start end))))

(defun etaf-buffer-get-region-properties (buffer uuid)
  "获取 BUFFER 中 UUID 标识区域的文本属性。
返回属性 plist 或 nil。"
  (with-current-buffer buffer
    (when-let* ((region (etaf-buffer-find-region-by-uuid uuid))
                (start (car region)))
      (text-properties-at start))))

;;; ============================================================
;;; 渲染辅助函数
;;; ============================================================

(defun etaf-layout-setup-scroll-region (string uuid content-lines
                                               content-height
                                               &optional padding-top padding-bottom
                                               border-top-p border-top-color
                                               border-bottom-p border-bottom-color)
  "设置字符串的滚动区域属性并缓存数据。
STRING 是要设置属性的字符串。
UUID 是区域标识符。
CONTENT-LINES 是内容行列表。
CONTENT-HEIGHT 是显示高度。
其余参数用于边框和 padding 信息。

返回设置了属性的新字符串。"
  ;; 初始化缓存
  (etaf-layout-caches-init)
  
  ;; 存储缓存数据
  (let ((content-linum (length content-lines))
        (v-scroll-steps (etaf-layout-v-scroll-steps
                         (length content-lines) content-height)))
    (etaf-layout-cache-put
     uuid
     (list :content-lines content-lines
           :content-linum content-linum
           :content-height content-height
           :v-scroll-offset 0
           :v-scroll-steps v-scroll-steps
           :padding-top-height (or padding-top 0)
           :padding-bottom-height (or padding-bottom 0)
           :border-top-p border-top-p
           :border-top-color border-top-color
           :border-bottom-p border-bottom-p
           :border-bottom-color border-bottom-color)))
  
  ;; 给每一行设置属性
  (mapconcat (lambda (line)
               (propertize line
                           'etaf-layout-content-line uuid
                           'keymap (etaf-layout-scroll-map)))
             (split-string string "\n" t) "\n"))

(provide 'etaf-layout-interactive)
;;; etaf-layout-interactive.el ends here
