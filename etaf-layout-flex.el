;;; etaf-layout-flex.el --- Flex layout formatting context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, flexbox, css
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Flex 布局格式化上下文模块
;;
;; 本模块实现 CSS Flexbox 布局算法。
;; 所有函数使用 `etaf-layout-flex-' 前缀。
;;
;; 公共接口：
;; - `etaf-layout-flex-format' - Flex 格式化上下文主入口
;; - `etaf-layout-flex-compute-main-axis' - 计算主轴分配
;; - `etaf-layout-flex-compute-cross-axis' - 计算交叉轴对齐
;; - `etaf-layout-flex-justify-space' - 计算 justify-content 空间分配
;; - `etaf-layout-flex-align-content-space' - 计算 align-content 空间分配
;;
;; 支持的 Flex 容器属性：
;; - flex-direction: row | row-reverse | column | column-reverse
;; - flex-wrap: nowrap | wrap | wrap-reverse
;; - justify-content: flex-start | flex-end | center | space-between | space-around | space-evenly
;; - align-items: stretch | flex-start | flex-end | center | baseline
;; - align-content: stretch | flex-start | flex-end | center | space-between | space-around
;; - gap, row-gap, column-gap
;;
;; 支持的 Flex Item 属性：
;; - order
;; - flex-grow
;; - flex-shrink
;; - flex-basis
;; - align-self

;;; Code:

(require 'cl-lib)
(eval-and-compile (require 'dom))
(require 'etaf-css-parser)
(require 'etaf-layout-box)

;; Forward declarations
(declare-function etaf-layout-get-box-model "etaf-layout")
(declare-function etaf-layout-create-node "etaf-layout")
(declare-function etaf-layout-compute-box-model "etaf-layout")
(declare-function etaf-layout-node "etaf-layout")
(declare-function etaf-render-get-computed-style "etaf-render")
(declare-function etaf-render-get-display "etaf-render")

;;; ============================================================
;;; 公共接口
;;; ============================================================

(defun etaf-layout-flex-format (render-node parent-context)
  "在 Flex 格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点（display: flex）。
PARENT-CONTEXT 包含父容器的上下文信息。

返回布局节点。

该函数是 Flex 布局的主入口，负责：
1. 计算盒模型
2. 提取 Flex 容器属性
3. 递归布局子元素
4. 计算主轴分配（flex-grow/shrink）
5. 计算交叉轴对齐（align-items）"
  (let* ((box-model (etaf-layout-compute-box-model render-node parent-context))
         (computed-style (etaf-render-get-computed-style render-node))
         (content-width (etaf-layout-box-content-width box-model))
         (content-height (etaf-layout-box-content-height box-model))
         
         ;; Flex 容器属性
         (flex-direction (or (etaf-css-parse-style-value computed-style 'flex-direction)
                             "row"))
         (flex-wrap (or (etaf-css-parse-style-value computed-style 'flex-wrap)
                        "nowrap"))
         (justify-content (or (etaf-css-parse-style-value computed-style 'justify-content)
                              "flex-start"))
         (align-items (or (etaf-css-parse-style-value computed-style 'align-items)
                          "stretch"))
         (align-content (or (etaf-css-parse-style-value computed-style 'align-content)
                            "stretch"))
         (row-gap-str (etaf-css-parse-style-value computed-style 'row-gap "0"))
         (column-gap-str (etaf-css-parse-style-value computed-style 'column-gap "0"))
         (row-gap-parsed (etaf-css-parse-length row-gap-str content-width))
         (column-gap-parsed (etaf-css-parse-length column-gap-str content-width))
         (row-gap (if (eq row-gap-parsed 'auto) 0 row-gap-parsed))
         (column-gap (if (eq column-gap-parsed 'auto) 0 column-gap-parsed))
         
         ;; 判断方向
         (is-row-direction (or (string= flex-direction "row")
                               (string= flex-direction "row-reverse")))
         (is-reversed (or (string= flex-direction "row-reverse")
                          (string= flex-direction "column-reverse")))
         (should-wrap (not (string= flex-wrap "nowrap")))
         (wrap-reversed (string= flex-wrap "wrap-reverse"))
         
         ;; 创建布局节点
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 添加 flex 相关属性到布局节点
    (dom-set-attribute layout-node 'layout-flex-direction flex-direction)
    (dom-set-attribute layout-node 'layout-flex-wrap flex-wrap)
    (dom-set-attribute layout-node 'layout-justify-content justify-content)
    (dom-set-attribute layout-node 'layout-align-items align-items)
    (dom-set-attribute layout-node 'layout-align-content align-content)
    (dom-set-attribute layout-node 'layout-row-gap row-gap)
    (dom-set-attribute layout-node 'layout-column-gap column-gap)
    
    ;; 布局子元素
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                   :content-height content-height
                                   :flex-container t
                                   :flex-direction flex-direction
                                   :align-items align-items))
              (child-layouts '())
              (flex-items '()))
          
          ;; 递归布局所有子元素
          (dolist (child children)
            (cond
             ;; 元素节点
             ((and (consp child) (symbolp (car child)))
              (when-let ((child-layout (etaf-layout-node child child-context)))
                (push child-layout child-layouts)
                ;; 收集 flex item 信息
                (let* ((child-style (etaf-render-get-computed-style child))
                       (order (or (etaf-css-parse-flex-number
                                   (etaf-css-parse-style-value child-style 'order))
                                  0))
                       (flex-grow
                        (or (etaf-css-parse-flex-number
                             (etaf-css-parse-style-value child-style 'flex-grow))
                            0))
                       (flex-shrink
                        (or (etaf-css-parse-flex-number
                             (etaf-css-parse-style-value child-style 'flex-shrink))
                            1))
                       (flex-basis (etaf-css-parse-style-value
                                    child-style 'flex-basis "auto"))
                       (align-self
                        (etaf-css-parse-style-value child-style 'align-self)))
                  ;; 添加 flex item 属性到子布局节点
                  (dom-set-attribute child-layout 'layout-order order)
                  (dom-set-attribute child-layout 'layout-flex-grow flex-grow)
                  (dom-set-attribute child-layout 'layout-flex-shrink flex-shrink)
                  (dom-set-attribute child-layout 'layout-flex-basis flex-basis)
                  (when align-self
                    (dom-set-attribute child-layout 'layout-align-self align-self))
                  (push (list :layout child-layout
                              :order order
                              :flex-grow flex-grow
                              :flex-shrink flex-shrink
                              :flex-basis flex-basis)
                        flex-items))))
             ;; 文本节点：直接保留
             ((stringp child)
              (push child child-layouts))))
          
          ;; 按 order 排序 flex items
          (setq flex-items (sort (nreverse flex-items)
                                 (lambda (a b)
                                   (< (plist-get a :order)
                                      (plist-get b :order)))))
          
          ;; 根据排序结果重新排列子节点
          (let ((sorted-children '()))
            (dolist (item flex-items)
              (push (plist-get item :layout) sorted-children))
            (dolist (child child-layouts)
              (when (stringp child)
                (push child sorted-children)))
            (setcdr (cdr layout-node) (nreverse sorted-children)))
          
          ;; 计算主轴分配
          (message "content-width:%S" content-width)
          (etaf-layout-flex-compute-main-axis
           layout-node flex-items content-width content-height
           flex-direction justify-content row-gap column-gap should-wrap)
          
          ;; 计算交叉轴对齐
          (etaf-layout-flex-compute-cross-axis
           layout-node flex-items content-width content-height
           flex-direction align-items align-content))))
    
    layout-node))

;;; ============================================================
;;; 主轴计算
;;; ============================================================

(defun etaf-layout-flex-compute-main-axis (layout-node flex-items
                                                       container-width container-height
                                                       direction justify-content
                                                       row-gap column-gap should-wrap)
  "计算 Flex 布局主轴分配。
LAYOUT-NODE 是 Flex 容器布局节点。
FLEX-ITEMS 是 Flex Item 列表。
CONTAINER-WIDTH/HEIGHT 是容器尺寸。
DIRECTION 是 flex-direction。
JUSTIFY-CONTENT 是主轴对齐方式。
ROW-GAP/COLUMN-GAP 是间隙值。
SHOULD-WRAP 表示是否启用换行。"
  (let* ((is-row (or (string= direction "row")
                     (string= direction "row-reverse")))
         (main-size (if is-row container-width container-height))
         (main-gap (if is-row column-gap row-gap))
         (items-count (length flex-items))
         (total-flex-basis 0)
         (total-flex-grow 0)
         (total-flex-shrink 0)
         (item-sizes '()))
    
    ;; 计算总的 flex-basis、flex-grow 和 flex-shrink
    (dolist (item flex-items)
      (let* ((layout (plist-get item :layout))
             (box-model (etaf-layout-get-box-model layout))
             (item-main-size (if is-row
                                 (etaf-layout-box-total-width box-model)
                               (etaf-layout-box-total-height box-model)))
             (item-content-size (if is-row
                                    (etaf-layout-box-content-width box-model)
                                  (etaf-layout-box-content-height box-model)))
             (item-side-size (- item-main-size item-content-size)))
        (push (list :item item
                    :layout layout
                    :box-model box-model
                    :main-size item-main-size
                    :content-size item-content-size
                    :side-size item-side-size)
              item-sizes)
        (setq total-flex-basis (+ total-flex-basis item-main-size))
        (setq total-flex-grow (+ total-flex-grow (plist-get item :flex-grow)))
        (setq total-flex-shrink (+ total-flex-shrink (plist-get item :flex-shrink)))))
    
    (setq item-sizes (nreverse item-sizes))
    
    ;; 计算空间分配
    (let* ((total-gap (* main-gap (max 0 (1- items-count))))
           (available-space (- main-size total-flex-basis total-gap))
           (free-space (max 0 available-space))
           (overflow-space (max 0 (- (+ total-flex-basis total-gap) main-size))))
      
      ;; 应用 flex-grow
      (when (and (> free-space 0) (> total-flex-grow 0) (> main-size 0))
        (etaf-layout-flex--apply-grow flex-items item-sizes free-space total-flex-grow is-row))
      
      ;; 应用 flex-shrink
      (when (and (> overflow-space 0) (> total-flex-shrink 0) (> main-size 0))
        (etaf-layout-flex--apply-shrink flex-items item-sizes overflow-space total-flex-shrink is-row))
      
      ;; 存储计算结果
      (dom-set-attribute layout-node 'layout-flex-free-space free-space)
      (dom-set-attribute layout-node 'layout-flex-total-grow total-flex-grow)
      
      ;; 计算 justify-content 分布
      (when (> items-count 0)
        (let ((space-distribution
               (etaf-layout-flex-justify-space
                justify-content free-space items-count main-gap)))
          (dom-set-attribute layout-node 'layout-flex-space-distribution
                             space-distribution))))))

(defun etaf-layout-flex--apply-grow (flex-items item-sizes free-space total-flex-grow is-row)
  "应用 flex-grow 分配剩余空间。
FLEX-ITEMS 是 Flex Item 列表。
ITEM-SIZES 是 Item 尺寸信息列表。
FREE-SPACE 是剩余空间。
TOTAL-FLEX-GROW 是 flex-grow 总和。
IS-ROW 表示是否为行方向。"
  (let* ((grow-unit (/ (float free-space) total-flex-grow))
         (distributed 0))
    (dotimes (i (length flex-items))
      (let* ((item-info (nth i item-sizes))
             (item (plist-get item-info :item))
             (box-model (plist-get item-info :box-model))
             (content-size (plist-get item-info :content-size))
             (flex-grow (plist-get item :flex-grow)))
        (when (> flex-grow 0)
          (let* ((grow-amount (max 0
                                   (if (= i (1- (length flex-items)))
                                       (- free-space distributed)
                                     (floor (* grow-unit flex-grow)))))
                 (new-content-size (+ content-size grow-amount)))
            (setq distributed (+ distributed grow-amount))
            (if is-row
                (plist-put (plist-get box-model :content) :width new-content-size)
              (plist-put (plist-get box-model :content) :height new-content-size))))))))

(defun etaf-layout-flex--apply-shrink (flex-items item-sizes overflow-space total-flex-shrink is-row)
  "应用 flex-shrink 缩减溢出空间。
FLEX-ITEMS 是 Flex Item 列表。
ITEM-SIZES 是 Item 尺寸信息列表。
OVERFLOW-SPACE 是溢出空间。
TOTAL-FLEX-SHRINK 是 flex-shrink 总和。
IS-ROW 表示是否为行方向。"
  (let* ((shrink-unit (/ (float overflow-space) total-flex-shrink))
         (distributed 0))
    (dotimes (i (length flex-items))
      (let* ((item-info (nth i item-sizes))
             (item (plist-get item-info :item))
             (box-model (plist-get item-info :box-model))
             (content-size (plist-get item-info :content-size))
             (flex-shrink (plist-get item :flex-shrink)))
        (when (> flex-shrink 0)
          (let* ((shrink-amount (max 0
                                     (if (= i (1- (length flex-items)))
                                         (- overflow-space distributed)
                                       (floor (* shrink-unit flex-shrink)))))
                 (new-content-size (max 0 (- content-size shrink-amount))))
            (setq distributed (+ distributed shrink-amount))
            (if is-row
                (plist-put (plist-get box-model :content) :width new-content-size)
              (plist-put (plist-get box-model :content) :height new-content-size))))))))

;;; ============================================================
;;; 空间分配计算
;;; ============================================================

(defun etaf-layout-flex-justify-space (justify-content free-space items-count gap)
  "计算 justify-content 空间分配。
JUSTIFY-CONTENT 是对齐方式。
FREE-SPACE 是剩余空间。
ITEMS-COUNT 是 Item 数量。
GAP 是 Item 之间的间隙。

返回 (start-space between-space end-space) 列表。"
  (pcase justify-content
    ("flex-start"
     (list 0 gap free-space))
    ("flex-end"
     (list free-space gap 0))
    ("center"
     (let ((side-space (/ free-space 2.0)))
       (list side-space gap side-space)))
    ("space-between"
     (if (<= items-count 1)
         (list 0 0 0)
       (let ((between (/ (+ free-space (* gap (1- items-count)))
                         (1- items-count))))
         (list 0 between 0))))
    ("space-around"
     (if (<= items-count 0)
         (list 0 0 0)
       (let* ((unit-space (/ free-space (* 2.0 items-count)))
              (between (+ (* 2 unit-space) gap)))
         (list unit-space between unit-space))))
    ("space-evenly"
     (if (<= items-count 0)
         (list 0 0 0)
       (let ((space (/ free-space (1+ items-count))))
         (list space (+ space gap) space))))
    (_
     (list 0 gap free-space))))

(defun etaf-layout-flex-align-content-space (align-content free-space lines-count gap)
  "计算 align-content 空间分配（多行情况）。
ALIGN-CONTENT 是对齐方式。
FREE-SPACE 是交叉轴剩余空间。
LINES-COUNT 是行数。
GAP 是行之间的间隙。

返回 (start-space between-space end-space) 列表。"
  (pcase align-content
    ("flex-start"
     (list 0 gap free-space))
    ("flex-end"
     (list free-space gap 0))
    ("center"
     (let ((side-space (/ free-space 2.0)))
       (list side-space gap side-space)))
    ("space-between"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let ((between (/ (+ free-space (* gap (1- lines-count)))
                         (1- lines-count))))
         (list 0 between 0))))
    ("space-around"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let* ((unit-space (/ free-space (* 2.0 lines-count)))
              (between (+ (* 2 unit-space) gap)))
         (list unit-space between unit-space))))
    ("space-evenly"
     (if (<= lines-count 1)
         (list 0 0 0)
       (let ((space (/ free-space (1+ lines-count))))
         (list space (+ space gap) space))))
    ("stretch"
     (list 0 gap 0))
    (_
     (list 0 gap free-space))))

;;; ============================================================
;;; 交叉轴计算
;;; ============================================================

(defun etaf-layout-flex-compute-cross-axis (layout-node flex-items
                                                        container-width container-height
                                                        direction align-items align-content)
  "计算 Flex 布局交叉轴对齐。
LAYOUT-NODE 是 Flex 容器布局节点。
FLEX-ITEMS 是 Flex Item 列表。
CONTAINER-WIDTH/HEIGHT 是容器尺寸。
DIRECTION 是 flex-direction。
ALIGN-ITEMS 是 Item 对齐方式。
ALIGN-CONTENT 是多行对齐方式。"
  (let* ((is-row (or (string= direction "row")
                     (string= direction "row-reverse")))
         (cross-size (if is-row container-height container-width)))
    
    ;; 存储交叉轴对齐信息
    (dom-set-attribute layout-node 'layout-flex-align-items align-items)
    (dom-set-attribute layout-node 'layout-flex-align-content align-content)
    (dom-set-attribute layout-node 'layout-flex-cross-size cross-size)))

(provide 'etaf-layout-flex)
;;; etaf-layout-flex.el ends here
