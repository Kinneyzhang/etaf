;;; etaf-layout-grid.el --- Grid layout formatting context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, grid, css
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Grid 布局格式化上下文模块
;;
;; 本模块实现 CSS Grid 布局算法。
;; 所有函数使用 `etaf-layout-grid-' 前缀。
;;
;; 公共接口：
;; - `etaf-layout-grid-format' - Grid 格式化上下文主入口
;; - `etaf-layout-grid-compute-tracks' - 计算网格轨道尺寸
;; - `etaf-layout-grid-place-items' - 放置网格项目
;; - `etaf-layout-grid-align-items' - 对齐网格项目
;;
;; 支持的 Grid 容器属性：
;; - grid-template-columns: 列轨道定义 (支持 px, fr, auto, repeat())
;; - grid-template-rows: 行轨道定义 (支持 px, fr, auto, repeat())
;; - grid-template-areas: 命名网格区域
;; - gap, row-gap, column-gap: 网格间隙
;; - justify-items: 水平对齐 (start | end | center | stretch)
;; - align-items: 垂直对齐 (start | end | center | stretch)
;; - justify-content: 网格整体水平对齐 (start | end | center | stretch | space-between | space-around | space-evenly)
;; - align-content: 网格整体垂直对齐 (start | end | center | stretch | space-between | space-around | space-evenly)
;;
;; 支持的 Grid Item 属性：
;; - grid-column-start, grid-column-end: 列起止位置
;; - grid-row-start, grid-row-end: 行起止位置
;; - grid-column: 列位置简写
;; - grid-row: 行位置简写
;; - grid-area: 区域名称或位置简写
;; - justify-self: 单个项目水平对齐
;; - align-self: 单个项目垂直对齐

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-css-parser)
(require 'etaf-layout-box)

;;; Constants

(defconst etaf-layout-grid-default-row-height 100
  "Default height for auto-generated grid rows.")

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

(defun etaf-layout-grid-format (render-node parent-context)
  "在 Grid 格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点（display: grid）。
PARENT-CONTEXT 包含父容器的上下文信息。

返回布局节点。

该函数是 Grid 布局的主入口，负责：
1. 计算盒模型
2. 提取 Grid 容器属性
3. 递归布局子元素
4. 计算网格轨道（行和列）
5. 放置网格项目
6. 对齐网格项目"
  (let* ((box-model (etaf-layout-compute-box-model render-node parent-context))
         (computed-style (etaf-render-get-computed-style render-node))
         (content-width (etaf-layout-box-content-width box-model))
         (content-height (etaf-layout-box-content-height box-model))
         
         ;; Grid 容器属性
         (grid-template-columns (or (etaf-css-parse-style-value 
                                     computed-style 'grid-template-columns)
                                    "auto"))
         (grid-template-rows (or (etaf-css-parse-style-value 
                                  computed-style 'grid-template-rows)
                                 "auto"))
         (grid-template-areas (etaf-css-parse-style-value 
                               computed-style 'grid-template-areas))
         (row-gap-str (etaf-css-parse-style-value computed-style 'row-gap "0"))
         (column-gap-str (etaf-css-parse-style-value computed-style 'column-gap "0"))
         (row-gap-parsed (etaf-css-parse-length row-gap-str content-width))
         (column-gap-parsed (etaf-css-parse-length column-gap-str content-width))
         (row-gap (if (eq row-gap-parsed 'auto) 0 row-gap-parsed))
         (column-gap (if (eq column-gap-parsed 'auto) 0 column-gap-parsed))
         
         (justify-items (or (etaf-css-parse-style-value 
                            computed-style 'justify-items)
                           "stretch"))
         (align-items (or (etaf-css-parse-style-value 
                          computed-style 'align-items)
                         "stretch"))
         (justify-content (or (etaf-css-parse-style-value 
                              computed-style 'justify-content)
                             "start"))
         (align-content (or (etaf-css-parse-style-value 
                            computed-style 'align-content)
                           "start"))
         
         ;; 创建布局节点
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; 添加 grid 相关属性到布局节点
    (dom-set-attribute layout-node 'layout-grid-template-columns grid-template-columns)
    (dom-set-attribute layout-node 'layout-grid-template-rows grid-template-rows)
    (when grid-template-areas
      (dom-set-attribute layout-node 'layout-grid-template-areas grid-template-areas))
    (dom-set-attribute layout-node 'layout-row-gap row-gap)
    (dom-set-attribute layout-node 'layout-column-gap column-gap)
    (dom-set-attribute layout-node 'layout-justify-items justify-items)
    (dom-set-attribute layout-node 'layout-align-items align-items)
    (dom-set-attribute layout-node 'layout-justify-content justify-content)
    (dom-set-attribute layout-node 'layout-align-content align-content)
    
    ;; 布局子元素
    (let ((children (dom-children render-node)))
      (when children
        (let ((child-context (list :content-width content-width
                                   :content-height content-height
                                   :viewport-width (plist-get parent-context :viewport-width)
                                   :viewport-height (plist-get parent-context :viewport-height)
                                   :grid-container t))
              (child-layouts '())
              (grid-items '()))
          
          ;; 递归布局所有子元素
          (dolist (child children)
            (cond
             ;; 元素节点
             ((and (consp child) (symbolp (car child)))
              (when-let ((child-layout (etaf-layout-node child child-context)))
                (push child-layout child-layouts)
                ;; 收集 grid item 信息
                (let* ((child-style (etaf-render-get-computed-style child))
                       (grid-column-start (etaf-css-parse-style-value 
                                          child-style 'grid-column-start "auto"))
                       (grid-column-end (etaf-css-parse-style-value 
                                        child-style 'grid-column-end "auto"))
                       (grid-row-start (etaf-css-parse-style-value 
                                       child-style 'grid-row-start "auto"))
                       (grid-row-end (etaf-css-parse-style-value 
                                     child-style 'grid-row-end "auto"))
                       (grid-area (etaf-css-parse-style-value 
                                  child-style 'grid-area))
                       (justify-self (etaf-css-parse-style-value 
                                     child-style 'justify-self))
                       (align-self (etaf-css-parse-style-value 
                                   child-style 'align-self)))
                  ;; 添加 grid item 属性到子布局节点
                  (dom-set-attribute child-layout 'layout-grid-column-start grid-column-start)
                  (dom-set-attribute child-layout 'layout-grid-column-end grid-column-end)
                  (dom-set-attribute child-layout 'layout-grid-row-start grid-row-start)
                  (dom-set-attribute child-layout 'layout-grid-row-end grid-row-end)
                  (when grid-area
                    (dom-set-attribute child-layout 'layout-grid-area grid-area))
                  (when justify-self
                    (dom-set-attribute child-layout 'layout-justify-self justify-self))
                  (when align-self
                    (dom-set-attribute child-layout 'layout-align-self align-self))
                  (push (list :layout child-layout
                              :column-start grid-column-start
                              :column-end grid-column-end
                              :row-start grid-row-start
                              :row-end grid-row-end
                              :area grid-area)
                        grid-items))))
             ;; 文本节点：直接保留
             ((stringp child)
              (push child child-layouts))))
          
          (setq grid-items (nreverse grid-items))
          (setcdr (cdr layout-node) (nreverse child-layouts))
          
          ;; 计算网格轨道
          (let ((tracks (etaf-layout-grid-compute-tracks
                        layout-node grid-items
                        grid-template-columns grid-template-rows
                        content-width content-height
                        row-gap column-gap)))
            (dom-set-attribute layout-node 'layout-grid-tracks tracks)
            
            ;; 放置网格项目
            (etaf-layout-grid-place-items
             layout-node grid-items tracks
             grid-template-areas row-gap column-gap)
            
            ;; 对齐网格项目
            (etaf-layout-grid-align-items
             layout-node grid-items tracks
             justify-items align-items
             justify-content align-content)))))
    
    layout-node))

;;; ============================================================
;;; 网格轨道计算
;;; ============================================================

(defun etaf-layout-grid-compute-tracks (_layout-node _grid-items
                                                    template-columns template-rows
                                                    container-width container-height
                                                    row-gap column-gap)
  "计算 Grid 布局的轨道尺寸。
LAYOUT-NODE 是 Grid 容器布局节点。
GRID-ITEMS 是 Grid Item 列表。
TEMPLATE-COLUMNS/ROWS 是轨道定义字符串。
CONTAINER-WIDTH/HEIGHT 是容器尺寸。
ROW-GAP/COLUMN-GAP 是间隙值。

返回轨道信息 plist: (:columns (...) :rows (...))"
  (let* ((column-tracks (etaf-layout-grid-parse-tracks 
                        template-columns container-width))
         (row-tracks (etaf-layout-grid-parse-tracks 
                     template-rows container-height))
         (num-columns (length column-tracks))
         (num-rows (length row-tracks)))
    
    ;; 计算列宽
    (let* ((total-column-gap (* column-gap (max 0 (1- num-columns))))
           (available-width (- container-width total-column-gap))
           (column-widths (etaf-layout-grid-resolve-tracks 
                          column-tracks available-width)))
      
      ;; 计算行高
      (let* ((total-row-gap (* row-gap (max 0 (1- num-rows))))
             (available-height (- container-height total-row-gap))
             (row-heights (etaf-layout-grid-resolve-tracks 
                          row-tracks available-height)))
        
        (list :columns column-widths
              :rows row-heights
              :num-columns num-columns
              :num-rows num-rows)))))

(defun etaf-layout-grid-parse-tracks (track-def available-size)
  "解析轨道定义字符串。
TRACK-DEF 是轨道定义（如 \"100px 1fr 2fr\"）。
AVAILABLE-SIZE 是可用空间。

返回轨道列表，每个轨道是 (:type <type> :value <value>)。
类型可以是: px, fr, auto, %"
  (if (or (null track-def) (string= track-def "auto") (string= track-def "none"))
      (list (list :type 'auto :value 0))
    (let ((parts (split-string track-def))
          (tracks '()))
      (dolist (part parts)
        (cond
         ;; fr 单位
         ((string-match "^\\([0-9.]+\\)fr$" part)
          (push (list :type 'fr 
                     :value (string-to-number (match-string 1 part)))
                tracks))
         ;; 像素单位
         ((string-match "^\\([0-9.]+\\)px$" part)
          (push (list :type 'px 
                     :value (string-to-number (match-string 1 part)))
                tracks))
         ;; 百分比
         ((string-match "^\\([0-9.]+\\)%$" part)
          (push (list :type '% 
                     :value (floor (* (string-to-number (match-string 1 part))
                                     available-size)
                                  100))
                tracks))
         ;; auto
         ((string= part "auto")
          (push (list :type 'auto :value 0) tracks))
         ;; 数字（当作像素）
         ((string-match "^[0-9.]+$" part)
          (push (list :type 'px 
                     :value (string-to-number part))
                tracks))
         ;; repeat() - 简化处理
         ((string-match "^repeat(\\([0-9]+\\),\\(.+\\))$" part)
          (let ((count (string-to-number (match-string 1 part)))
                (pattern (match-string 2 part)))
            (dotimes (_ count)
              (let ((pattern-tracks (etaf-layout-grid-parse-tracks 
                                    pattern available-size)))
                (dolist (track pattern-tracks)
                  (push track tracks))))))
         (t
          (push (list :type 'auto :value 0) tracks))))
      (nreverse tracks))))

(defun etaf-layout-grid-resolve-tracks (tracks available-size)
  "解析轨道尺寸，返回实际尺寸列表。
TRACKS 是轨道定义列表。
AVAILABLE-SIZE 是可用空间。

返回实际尺寸列表。"
  (let ((fixed-size 0)
        (fr-count 0)
        (auto-count 0)
        (sizes '()))
    
    ;; 第一遍：计算固定尺寸和 fr 总数
    (dolist (track tracks)
      (let ((type (plist-get track :type))
            (value (plist-get track :value)))
        (cond
         ((eq type 'px)
          (setq fixed-size (+ fixed-size value))
          (push value sizes))
         ((eq type '%)
          (setq fixed-size (+ fixed-size value))
          (push value sizes))
         ((eq type 'fr)
          (setq fr-count (+ fr-count value))
          (push nil sizes))
         ((eq type 'auto)
          (setq auto-count (+ auto-count 1))
          (push nil sizes)))))
    
    (setq sizes (nreverse sizes))
    
    ;; 第二遍：分配 fr 和 auto
    (let* ((remaining-size (max 0 (- available-size fixed-size)))
           (fr-unit (if (zerop fr-count)
                       0
                     (/ (float remaining-size) fr-count)))
           (auto-size (if (zerop auto-count)
                         0
                       (/ (float remaining-size) auto-count)))
           (result '())
           (index 0))
      
      (dolist (track tracks)
        (let ((type (plist-get track :type))
              (value (plist-get track :value))
              (size (nth index sizes)))
          (setq index (1+ index))
          (cond
           ((eq type 'fr)
            (push (floor (* value fr-unit)) result))
           ((eq type 'auto)
            (push (floor auto-size) result))
           (t
            (push (floor size) result)))))
      
      (nreverse result))))

;;; ============================================================
;;; 网格项目放置
;;; ============================================================

(defun etaf-layout-grid-place-items (_layout-node grid-items tracks
                                                 template-areas row-gap column-gap)
  "放置网格项目到网格单元格中。
LAYOUT-NODE 是 Grid 容器布局节点。
GRID-ITEMS 是 Grid Item 列表。
TRACKS 是轨道信息。
TEMPLATE-AREAS 是命名区域定义。
ROW-GAP/COLUMN-GAP 是间隙值。"
  (let* ((num-columns (plist-get tracks :num-columns))
         (num-rows (max (plist-get tracks :num-rows)
                       ;; Ensure we have enough rows for auto-placement
                       (ceiling (/ (float (length grid-items)) num-columns))))
         (column-widths (plist-get tracks :columns))
         (row-heights (plist-get tracks :rows))
         ;; Expand row heights if needed
         (expanded-row-heights 
          (if (< (length row-heights) num-rows)
              (append row-heights 
                     (make-list (- num-rows (length row-heights))
                               (if (> (length row-heights) 0)
                                   (car (last row-heights))
                                 etaf-layout-grid-default-row-height)))
            row-heights))
         (placement-grid (make-vector (* num-columns num-rows) nil))
         (auto-row 0)
         (auto-col 0))
    
    ;; 解析命名区域（如果有）
    (let ((areas (when template-areas
                   (etaf-layout-grid-parse-areas template-areas))))
      
      ;; 放置每个项目
      (dolist (item grid-items)
        (let* ((layout (plist-get item :layout))
               (area-name (plist-get item :area))
               (col-start (plist-get item :column-start))
               (col-end (plist-get item :column-end))
               (row-start (plist-get item :row-start))
               (row-end (plist-get item :row-end)))
          
          ;; 如果使用命名区域
          (when (and area-name areas)
            (when-let ((area (cdr (assoc area-name areas))))
              (setq col-start (plist-get area :col-start))
              (setq col-end (plist-get area :col-end))
              (setq row-start (plist-get area :row-start))
              (setq row-end (plist-get area :row-end))))
          
          ;; 解析位置
          (let ((start-col (etaf-layout-grid-parse-line col-start 1 num-columns))
                (end-col (etaf-layout-grid-parse-line col-end 2 num-columns))
                (start-row (etaf-layout-grid-parse-line row-start 1 num-rows))
                (end-row (etaf-layout-grid-parse-line row-end 2 num-rows)))
            
            ;; 自动放置
            (when (or (eq start-col 'auto) (eq start-row 'auto))
              (let ((pos (etaf-layout-grid-find-auto-position 
                         placement-grid num-columns num-rows
                         auto-row auto-col)))
                (when (eq start-row 'auto)
                  (setq start-row (car pos))
                  (setq auto-row (car pos)))
                (when (eq start-col 'auto)
                  (setq start-col (cdr pos))
                  (setq auto-col (cdr pos)))))
            
            (when (eq end-col 'auto)
              (setq end-col (1+ start-col)))
            (when (eq end-row 'auto)
              (setq end-row (1+ start-row)))
            
            ;; 标记占用的单元格
            (etaf-layout-grid-mark-cells placement-grid num-columns
                                        start-row end-row start-col end-col
                                        item)
            
            ;; 保存位置信息到布局节点
            (dom-set-attribute layout 'layout-grid-column start-col)
            (dom-set-attribute layout 'layout-grid-column-span (- end-col start-col))
            (dom-set-attribute layout 'layout-grid-row start-row)
            (dom-set-attribute layout 'layout-grid-row-span (- end-row start-row))
            
            ;; 计算实际位置和尺寸
            (let ((x 0)
                  (y 0)
                  (width 0)
                  (height 0))
              
              ;; 计算 x 和 width
              (dotimes (i start-col)
                (setq x (+ x (nth i column-widths)))
                (when (> i 0)
                  (setq x (+ x column-gap))))
              
              (dotimes (i (- end-col start-col))
                (setq width (+ width (nth (+ start-col i) column-widths)))
                (when (> i 0)
                  (setq width (+ width column-gap))))
              
              ;; 计算 y 和 height
              (dotimes (i start-row)
                (when (< i (length expanded-row-heights))
                  (setq y (+ y (nth i expanded-row-heights)))
                  (when (> i 0)
                    (setq y (+ y row-gap)))))
              
              (dotimes (i (- end-row start-row))
                (let ((row-idx (+ start-row i)))
                  (when (< row-idx (length expanded-row-heights))
                    (setq height (+ height (nth row-idx expanded-row-heights)))
                    (when (> i 0)
                      (setq height (+ height row-gap))))))
              
              ;; 更新项目的盒模型尺寸
              (let ((box-model (etaf-layout-get-box-model layout)))
                (plist-put (plist-get box-model :content) :width width)
                (plist-put (plist-get box-model :content) :height height))
              
              ;; 保存位置
              (dom-set-attribute layout 'layout-grid-x x)
              (dom-set-attribute layout 'layout-grid-y y))))))))

(defun etaf-layout-grid-parse-line (line-spec default max-lines)
  "解析网格线位置。
LINE-SPEC 是线位置规范（数字字符串或 \"auto\"）。
DEFAULT 是默认值。
MAX-LINES 是最大线数。

返回线位置（从 0 开始）或符号 auto。"
  (cond
   ((or (null line-spec) (string= line-spec "auto"))
    'auto)
   ((string-match "^span +\\([0-9]+\\)$" line-spec)
    ;; span 语法暂不完全支持，返回 auto
    'auto)
   ((string-match "^\\([0-9]+\\)$" line-spec)
    (let ((line (string-to-number (match-string 1 line-spec))))
      ;; 网格线从 1 开始，转换为从 0 开始的索引
      (max 0 (min (1- line) max-lines))))
   (t default)))

(defun etaf-layout-grid-find-auto-position (grid num-cols num-rows 
                                                start-row start-col)
  "在网格中查找下一个可用位置。
GRID 是放置网格。
NUM-COLS/NUM-ROWS 是网格尺寸。
START-ROW/START-COL 是起始搜索位置。

返回 (row . col)。"
  (let ((row start-row)
        (col start-col)
        (found nil)
        (grid-size (length grid)))
    (while (and (not found) (< row num-rows))
      (while (and (not found) (< col num-cols))
        (let ((idx (+ (* row num-cols) col)))
          (when (and (< idx grid-size) (null (aref grid idx)))
            (setq found t)))
        (unless found
          (setq col (1+ col))))
      (unless found
        (setq row (1+ row))
        (setq col 0)))
    (cons row col)))

(defun etaf-layout-grid-mark-cells (grid num-cols start-row end-row 
                                        start-col end-col item)
  "标记网格单元格为已占用。
GRID 是放置网格。
NUM-COLS 是列数。
START-ROW/END-ROW 是行范围。
START-COL/END-COL 是列范围。
ITEM 是占用单元格的项目。"
  (let ((grid-size (length grid)))
    (cl-loop for row from start-row below end-row do
             (cl-loop for col from start-col below end-col do
                      (let ((idx (+ (* row num-cols) col)))
                        (when (< idx grid-size)
                          (aset grid idx item)))))))

(defun etaf-layout-grid-parse-areas (_template-areas)
  "解析 grid-template-areas 定义。
TEMPLATE-AREAS 是区域定义字符串。

返回关联列表 ((area-name . (:col-start N :col-end M :row-start R :row-end S)))。"
  ;; 简化实现：暂不支持复杂的命名区域解析
  nil)

;;; ============================================================
;;; 网格项目对齐
;;; ============================================================

(defun etaf-layout-grid-align-items (layout-node grid-items _tracks
                                                 justify-items align-items
                                                 justify-content align-content)
  "对齐网格项目。
LAYOUT-NODE 是 Grid 容器布局节点。
GRID-ITEMS 是 Grid Item 列表。
TRACKS 是轨道信息。
JUSTIFY-ITEMS/ALIGN-ITEMS 是项目对齐方式。
JUSTIFY-CONTENT/ALIGN-CONTENT 是内容对齐方式。"
  ;; 存储对齐信息到布局节点
  (dom-set-attribute layout-node 'layout-grid-justify-items justify-items)
  (dom-set-attribute layout-node 'layout-grid-align-items align-items)
  (dom-set-attribute layout-node 'layout-grid-justify-content justify-content)
  (dom-set-attribute layout-node 'layout-grid-align-content align-content)
  
  ;; 对齐每个项目
  (dolist (item grid-items)
    (let* ((layout (plist-get item :layout))
           (justify-self (dom-attr layout 'layout-justify-self))
           (align-self (dom-attr layout 'layout-align-self))
           (final-justify (or justify-self justify-items))
           (final-align (or align-self align-items)))
      
      ;; 保存最终对齐值
      (dom-set-attribute layout 'layout-grid-final-justify final-justify)
      (dom-set-attribute layout 'layout-grid-final-align final-align))))

(provide 'etaf-layout-grid)
;;; etaf-layout-grid.el ends here
