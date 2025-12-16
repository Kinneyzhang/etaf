;;; etaf-layout-table.el --- Table layout formatting context -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, table, css
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Table layout formatting context module
;;
;; This module implements CSS Table layout algorithm.
;; All functions use `etaf-layout-table-' prefix.
;;
;; Public API:
;; - `etaf-layout-table-format' - Table formatting context entry point
;;
;; Supported display types:
;; - table
;; - table-row-group (thead, tbody, tfoot)
;; - table-header-group
;; - table-footer-group
;; - table-row
;; - table-cell
;; - table-caption
;;
;; Table Layout Algorithm:
;; 1. Collect all rows and cells
;; 2. Calculate column widths (fixed or auto)
;; 3. Distribute available width among columns
;; 4. Layout each cell with calculated width
;; 5. Calculate row heights based on cell content
;; 6. Apply borders and spacing

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'etaf-css-parser)
(require 'etaf-layout-box)
(require 'etaf-utils)

;; Forward declarations
(declare-function etaf-layout-get-box-model "etaf-layout")
(declare-function etaf-layout-create-node "etaf-layout")
(declare-function etaf-layout-compute-box-model "etaf-layout")
(declare-function etaf-layout-node "etaf-layout")
(declare-function etaf-layout-block-formatting-context "etaf-layout")
(declare-function etaf-render-get-computed-style "etaf-render")
(declare-function etaf-render-get-display "etaf-render")
(declare-function etaf-render-get-default-display "etaf-render")

;;; ============================================================
;;; Public API
;;; ============================================================

(defun etaf-layout-table-format (render-node parent-context)
  "Layout a table element in table formatting context.
RENDER-NODE is the table element to layout (display: table).
PARENT-CONTEXT contains parent container context information.

Returns a layout node.

This function is the main entry point for table layout:
1. Compute box model for the table
2. Extract table properties (border-collapse, border-spacing)
3. Collect all rows and cells
4. Calculate column widths
5. Layout cells with calculated widths
6. Calculate row heights"
  (let* ((box-model (etaf-layout-compute-box-model render-node parent-context))
         (computed-style (etaf-render-get-computed-style render-node))
         ;; Get content width from box model, fallback to parent context
         (box-content-width (etaf-layout-box-content-width box-model))
         (parent-content-width (plist-get parent-context :content-width))
         (content-width (if (and box-content-width (> box-content-width 0))
                            box-content-width
                          (or parent-content-width 0)))
         (content-height (etaf-layout-box-content-height box-model))
         
         ;; Table properties
         (border-collapse (or (etaf-css-parse-style-value
                               computed-style 'border-collapse)
                              "separate"))
         (border-spacing-val (etaf-css-parse-style-value
                               computed-style 'border-spacing "0"))
         (border-spacing (if (stringp border-spacing-val)
                             (etaf-css-parse-length border-spacing-val content-width)
                           0))
         (border-spacing (if (eq border-spacing 'auto) 0 border-spacing))
         
         ;; Create layout node
         (layout-node (etaf-layout-create-node render-node box-model)))
    
    ;; Add table-related attributes to layout node
    (dom-set-attribute layout-node 'layout-table t)
    (dom-set-attribute layout-node 'layout-border-collapse border-collapse)
    (dom-set-attribute layout-node 'layout-border-spacing border-spacing)
    
    ;; Collect table structure: rows, row-groups, caption
    (let* ((children (dom-children render-node))
           (table-structure (etaf-layout-table--collect-structure children))
           (caption (plist-get table-structure :caption))
           (row-groups (plist-get table-structure :row-groups))
           (direct-rows (plist-get table-structure :direct-rows))
           (all-rows (plist-get table-structure :all-rows))
           
           ;; Calculate column count and widths
           (column-count (etaf-layout-table--count-columns all-rows))
           (column-widths (etaf-layout-table--calculate-column-widths
                           all-rows column-count content-width border-spacing))
           
           ;; Child context for layouting cells
           (child-context (list :content-width content-width
                                :content-height content-height
                                :viewport-width (plist-get parent-context :viewport-width)
                                :viewport-height (plist-get parent-context :viewport-height)
                                :table-column-widths column-widths
                                :table-border-spacing border-spacing))
           (child-layouts '())
           (accumulated-height 0))
      
      ;; Store column info in layout node
      (dom-set-attribute layout-node 'layout-table-column-count column-count)
      (dom-set-attribute layout-node 'layout-table-column-widths column-widths)
      
      ;; Layout caption if present (before or after table)
      (when caption
        (let ((caption-layout (etaf-layout-block-formatting-context
                               caption child-context)))
          (when caption-layout
            (push caption-layout child-layouts)
            (let ((caption-box (etaf-layout-get-box-model caption-layout)))
              (setq accumulated-height
                    (+ accumulated-height
                       (etaf-layout-box-content-height caption-box)
                       (etaf-layout-box-padding-height caption-box)
                       (etaf-layout-box-margin-height caption-box)))))))
      
      ;; Layout row groups (thead, tbody, tfoot)
      (dolist (group row-groups)
        (let ((group-layout (etaf-layout-table--layout-row-group
                             group child-context column-widths border-spacing)))
          (when group-layout
            (push group-layout child-layouts)
            (let ((group-box (etaf-layout-get-box-model group-layout)))
              (setq accumulated-height
                    (+ accumulated-height
                       (etaf-layout-box-content-height group-box)
                       (etaf-layout-box-padding-height group-box)))))))
      
      ;; Layout direct rows (rows without thead/tbody/tfoot wrapper)
      (dolist (row direct-rows)
        (let ((row-layout (etaf-layout-table--layout-row
                           row child-context column-widths border-spacing)))
          (when row-layout
            (push row-layout child-layouts)
            (let ((row-box (etaf-layout-get-box-model row-layout)))
              (setq accumulated-height
                    (+ accumulated-height
                       (etaf-layout-box-content-height row-box)
                       (etaf-layout-box-padding-height row-box)))))))
      
      ;; Set children
      (setcdr (cdr layout-node) (nreverse child-layouts))
      
      ;; Update height if auto
      (when (= content-height 0)
        (let ((box (etaf-layout-get-box-model layout-node)))
          (plist-put (plist-get box :content) :height accumulated-height))))
    
    layout-node))

;;; ============================================================
;;; Table Structure Collection
;;; ============================================================

(defun etaf-layout-table--collect-structure (children)
  "Collect table structure from CHILDREN.
Returns a plist with:
  :caption - the table caption element (if any)
  :row-groups - list of row group elements (thead, tbody, tfoot)
  :direct-rows - rows directly inside table (without thead/tbody/tfoot wrapper)
  :all-rows - ALL rows for column width calculation (includes both
              rows inside row-groups AND direct-rows)"
  (let ((caption nil)
        (row-groups '())
        (direct-rows '())
        (all-rows '()))
    
    (dolist (child children)
      (when (and (consp child) (symbolp (car child)))
        (let ((display (or (etaf-render-get-display child)
                           (etaf-render-get-default-display (car child)))))
          (cond
           ;; Caption
           ((string= display "table-caption")
            (setq caption child))
           
           ;; Row groups (thead, tbody, tfoot)
           ((or (string= display "table-header-group")
                (string= display "table-row-group")
                (string= display "table-footer-group"))
            (push child row-groups)
            ;; Collect rows from this group into all-rows
            (dolist (row-child (dom-children child))
              (when (and (consp row-child) (symbolp (car row-child)))
                (let ((row-display (or (etaf-render-get-display row-child)
                                       (etaf-render-get-default-display (car row-child)))))
                  (when (string= row-display "table-row")
                    (push row-child all-rows))))))
           
           ;; Direct rows (without wrapper) - added to both direct-rows and all-rows
           ((string= display "table-row")
            (push child direct-rows)
            (push child all-rows))))))
    
    (list :caption caption
          :row-groups (nreverse row-groups)
          :direct-rows (nreverse direct-rows)
          :all-rows (nreverse all-rows))))

;;; ============================================================
;;; Column Width Calculation
;;; ============================================================

(defun etaf-layout-table--count-columns (rows)
  "Count the maximum number of columns across all ROWS."
  (let ((max-cols 0))
    (dolist (row rows)
      (let ((cell-count 0))
        (dolist (child (dom-children row))
          (when (and (consp child) (symbolp (car child)))
            (let ((display (or (etaf-render-get-display child)
                               (etaf-render-get-default-display (car child)))))
              (when (string= display "table-cell")
                (cl-incf cell-count)))))
        (setq max-cols (max max-cols cell-count))))
    max-cols))

(defun etaf-layout-table--calculate-column-widths (rows column-count container-width border-spacing)
  "Calculate column widths for ROWS with COLUMN-COUNT columns.
CONTAINER-WIDTH is the available width.
BORDER-SPACING is the spacing between cells.

Returns a list of column widths in pixels.

Algorithm:
1. If column has explicit width, use it
2. Otherwise, distribute remaining space equally among auto columns

For border-spacing calculation:
- We use (n + 1) spacing elements for n columns: |spacing|cell1|spacing|cell2|...|cellN|spacing|
- This provides outer margins and gaps between all cells."
  (if (= column-count 0)
      '()
    ;; Calculate available width (minus spacing)
    ;; Total spacing = border-spacing * (column-count + 1) for outer + inner gaps
    ;; At this point, column-count > 0
    (let* ((total-spacing (* border-spacing (1+ column-count)))
           (available-width (max 0 (- container-width total-spacing)))
           ;; For now, distribute equally (basic implementation)
           ;; Ensure each column has at least 1 pixel width
           (equal-width (max 1 (/ available-width column-count))))
      (make-list column-count equal-width))))

;;; ============================================================
;;; Row Group Layout
;;; ============================================================

(defun etaf-layout-table--layout-row-group (row-group child-context column-widths border-spacing)
  "Layout a table row group (thead, tbody, tfoot).
ROW-GROUP is the row group node.
CHILD-CONTEXT is the layout context.
COLUMN-WIDTHS is the list of column widths.
BORDER-SPACING is the spacing between cells.

Returns a layout node."
  (let* ((box-model (etaf-layout-compute-box-model row-group child-context))
         (layout-node (etaf-layout-create-node row-group box-model))
         (child-layouts '())
         (accumulated-height 0))
    
    ;; Layout each row in the group
    (dolist (child (dom-children row-group))
      (cond
       ((and (consp child) (symbolp (car child)))
        (let ((display (or (etaf-render-get-display child)
                           (etaf-render-get-default-display (car child)))))
          (when (string= display "table-row")
            (let ((row-layout (etaf-layout-table--layout-row
                               child child-context column-widths border-spacing)))
              (when row-layout
                (push row-layout child-layouts)
                (let ((row-box (etaf-layout-get-box-model row-layout)))
                  (setq accumulated-height
                        (+ accumulated-height
                           (etaf-layout-box-content-height row-box)
                           (etaf-layout-box-padding-height row-box)))))))))
       ((stringp child)
        (push child child-layouts))))
    
    ;; Set children
    (setcdr (cdr layout-node) (nreverse child-layouts))
    
    ;; Update height
    (let ((box (etaf-layout-get-box-model layout-node)))
      (plist-put (plist-get box :content) :height accumulated-height))
    
    layout-node))

;;; ============================================================
;;; Row Layout
;;; ============================================================

(defun etaf-layout-table--layout-row (row child-context column-widths border-spacing)
  "Layout a table row.
ROW is the row node.
CHILD-CONTEXT is the layout context.
COLUMN-WIDTHS is the list of column widths.
BORDER-SPACING is the spacing between cells.

Returns a layout node."
  (let* ((box-model (etaf-layout-compute-box-model row child-context))
         (layout-node (etaf-layout-create-node row box-model))
         (child-layouts '())
         (max-cell-height 0)
         (col-index 0))
    
    ;; Mark as table row for string rendering
    (dom-set-attribute layout-node 'layout-table-row t)
    (dom-set-attribute layout-node 'layout-border-spacing border-spacing)
    
    ;; Layout each cell
    (dolist (child (dom-children row))
      (cond
       ((and (consp child) (symbolp (car child)))
        (let ((display (or (etaf-render-get-display child)
                           (etaf-render-get-default-display (car child)))))
          (when (string= display "table-cell")
            (let* ((cell-width (or (nth col-index column-widths) 0))
                   (cell-context (plist-put (copy-sequence child-context)
                                            :content-width cell-width))
                   (cell-layout (etaf-layout-table--layout-cell
                                 child cell-context cell-width)))
              (when cell-layout
                (push cell-layout child-layouts)
                (let ((cell-box (etaf-layout-get-box-model cell-layout)))
                  (setq max-cell-height
                        (max max-cell-height
                             (+ (etaf-layout-box-content-height cell-box)
                                (etaf-layout-box-padding-height cell-box)))))))
            (cl-incf col-index))))
       ((stringp child)
        (push child child-layouts))))
    
    ;; Set children
    (setcdr (cdr layout-node) (nreverse child-layouts))
    
    ;; Update row height to match tallest cell
    (let ((box (etaf-layout-get-box-model layout-node)))
      (plist-put (plist-get box :content) :height max-cell-height)
      ;; Also update total width to sum of column widths + spacing
      (let* ((total-width (+ (apply #'+ column-widths)
                             (* border-spacing (1+ (length column-widths))))))
        (plist-put (plist-get box :content) :width total-width)))
    
    layout-node))

;;; ============================================================
;;; Cell Layout
;;; ============================================================

(defun etaf-layout-table--layout-cell (cell child-context cell-width)
  "Layout a table cell.
CELL is the cell node.
CHILD-CONTEXT is the layout context.
CELL-WIDTH is the calculated width for this cell.

Returns a layout node."
  (let* ((box-model (etaf-layout-compute-box-model cell child-context))
         (layout-node (etaf-layout-create-node cell box-model))
         (child-layouts '())
         (accumulated-height 0))
    
    ;; Mark as table cell for string rendering
    (dom-set-attribute layout-node 'layout-table-cell t)
    
    ;; Update cell width to match column width
    (let ((box (etaf-layout-get-box-model layout-node)))
      (plist-put (plist-get box :content) :width cell-width))
    
    ;; Layout cell contents
    (let ((cell-child-context (plist-put (copy-sequence child-context)
                                         :content-width cell-width)))
      (dolist (child (dom-children cell))
        (cond
         ((and (consp child) (symbolp (car child)))
          (when-let ((child-layout (etaf-layout-node child cell-child-context)))
            (push child-layout child-layouts)
            (let ((child-box (etaf-layout-get-box-model child-layout)))
              (setq accumulated-height
                    (+ accumulated-height
                       (etaf-layout-box-content-height child-box)
                       (etaf-layout-box-padding-height child-box)
                       (etaf-layout-box-margin-height child-box))))))
         ((stringp child)
          (push child child-layouts)
          ;; Count string height
          (setq accumulated-height
                (+ accumulated-height
                   (etaf-string-linum child)))))))
    
    ;; Set children
    (setcdr (cdr layout-node) (nreverse child-layouts))
    
    ;; Update cell height based on content
    (let ((box (etaf-layout-get-box-model layout-node)))
      (when (= (etaf-layout-box-content-height box) 0)
        (plist-put (plist-get box :content) :height accumulated-height)))
    
    layout-node))

(provide 'etaf-layout-table)
;;; etaf-layout-table.el ends here
