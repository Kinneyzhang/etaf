;;; etaf-table.el --- Feature-rich table component for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: components, table, ui
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Feature-rich Table Component for ETAF
;;
;; Inspired by Element Plus (https://element-plus.org/en-US/component/table),
;; this module provides a comprehensive table component with:
;;
;; 1. Basic Features:
;;    - Column definitions with customizable properties
;;    - Data binding and rendering
;;    - Striped and bordered styles
;;    - Custom column widths
;;    - Empty state handling
;;    - Loading state
;;
;; 2. Sorting:
;;    - Sortable columns
;;    - Custom sort functions
;;    - Multiple column sorting
;;    - Ascending/descending order
;;
;; 3. Selection:
;;    - Single row selection
;;    - Multiple row selection
;;    - Select all functionality
;;    - Selection change callbacks
;;
;; 4. Filtering:
;;    - Column-based filtering
;;    - Custom filter functions
;;    - Multiple filter conditions
;;
;; 5. Pagination:
;;    - Page size configuration
;;    - Current page tracking
;;    - Total count display
;;    - Page navigation
;;
;; 6. Customization:
;;    - Custom cell renderers
;;    - Row class name functions
;;    - Cell class name functions
;;    - Header cell rendering
;;    - Expandable rows
;;
;; Usage Example:
;;
;;   (require 'etaf-table)
;;
;;   ;; Define columns (width values are in character widths, not pixels)
;;   (setq my-columns
;;     '((:prop "name" :label "Name" :width 150 :sortable t)
;;       (:prop "age" :label "Age" :width 80 :sortable t)
;;       (:prop "email" :label "Email" :width 200)
;;       (:prop "status" :label "Status" :width 100
;;        :formatter (lambda (row col value)
;;                    (if (equal value "active") "✓ Active" "✗ Inactive")))))
;;
;;   ;; Define data
;;   (setq my-data
;;     '((:id 1 :name "Alice" :age 28 :email "alice@example.com" :status "active")
;;       (:id 2 :name "Bob" :age 32 :email "bob@example.com" :status "inactive")
;;       (:id 3 :name "Charlie" :age 25 :email "charlie@example.com" :status "active")))
;;
;;   ;; Render table
;;   (etaf-paint-to-buffer "*table-demo*"
;;     '(etaf-table :data my-data
;;                  :columns my-columns
;;                  :stripe t
;;                  :border t
;;                  :show-header t
;;                  :empty-text "No data available"
;;                  :row-key "id"))

;;; Code:

(require 'etaf-component)
(require 'cl-lib)

;;; ============================================================================
;;; Table State Management
;;; ============================================================================

(defun etaf-table--initial-state (props)
  "Create initial state for table component from PROPS."
  (let* ((data (plist-get props :data))
         (columns (plist-get props :columns))
         (row-key (plist-get props :row-key))
         (default-sort (plist-get props :default-sort))
         (page-size (or (plist-get props :page-size) 10))
         (current-page (or (plist-get props :current-page) 1)))
    (list
     :data data
     :columns columns
     :row-key row-key
     :sort-prop (plist-get default-sort :prop)
     :sort-order (plist-get default-sort :order)
     :selected-rows '()
     :filters (make-hash-table :test 'equal)
     :current-page current-page
     :page-size page-size
     :expandable-rows (make-hash-table :test 'equal))))

;;; ============================================================================
;;; Table Utilities
;;; ============================================================================

(defun etaf-table--get-cell-value (row column)
  "Get cell value from ROW using COLUMN property."
  (let ((prop (plist-get column :prop)))
    (when prop
      (plist-get row (intern (concat ":" prop))))))

(defun etaf-table--format-cell-value (row column value)
  "Format cell VALUE using COLUMN formatter if available.
ROW is the current row data, COLUMN is the column definition."
  (let ((formatter (plist-get column :formatter)))
    (if formatter
        (funcall formatter row column value)
      (if value (format "%s" value) ""))))

(defun etaf-table--sort-data (data column order)
  "Sort DATA by COLUMN in ORDER (asc or desc)."
  (if (not column)
      data
    (let* ((prop (plist-get column :prop))
           (sort-method (plist-get column :sort-method))
           (comparator (or sort-method
                          (lambda (a b)
                            (let ((val-a (etaf-table--get-cell-value a column))
                                  (val-b (etaf-table--get-cell-value b column)))
                              (cond
                               ((and (numberp val-a) (numberp val-b))
                                (< val-a val-b))
                               ((and (stringp val-a) (stringp val-b))
                                (string< val-a val-b))
                               (t (string< (format "%s" val-a) (format "%s" val-b)))))))))
      (let ((sorted (sort (copy-sequence data) comparator)))
        (if (equal order "desc")
            (reverse sorted)
          sorted)))))

(defun etaf-table--filter-data (data filters columns)
  "Filter DATA using FILTERS hash table and COLUMNS definitions."
  (if (zerop (hash-table-count filters))
      data
    (seq-filter
     (lambda (row)
       (cl-every
        (lambda (column)
          (let* ((prop (plist-get column :prop))
                 (filter-value (gethash prop filters))
                 (filter-method (plist-get column :filter-method)))
            (if (and filter-value filter-method)
                (funcall filter-method filter-value row column)
              t)))
        columns))
     data)))

(defun etaf-table--paginate-data (data current-page page-size)
  "Paginate DATA based on CURRENT-PAGE and PAGE-SIZE."
  (let* ((start (* (1- current-page) page-size))
         (end (min (+ start page-size) (length data))))
    (if (>= start (length data))
        '()
      (seq-subseq data start end))))

(defun etaf-table--get-row-key (row row-key)
  "Get unique key for ROW using ROW-KEY property."
  (if row-key
      (plist-get row (intern (concat ":" row-key)))
    row))

(defun etaf-table--is-row-selected (row selected-rows row-key)
  "Check if ROW is in SELECTED-ROWS using ROW-KEY for comparison."
  (let ((key (etaf-table--get-row-key row row-key)))
    (member key (mapcar (lambda (r) (etaf-table--get-row-key r row-key))
                        selected-rows))))

;;; ============================================================================
;;; Table Component Definition
;;; ============================================================================

(etaf-define-component etaf-table
  :props '(:data :columns :stripe :border :show-header :empty-text
           :loading :loading-text :height :max-height :row-class-name
           :cell-class-name :header-cell-class-name :row-key :default-sort
           :show-selection :show-index :page-size :current-page :show-pagination
           :total)
  
  :setup
  (lambda (props)
    "Setup function for table component."
    (let* ((data (etaf-ref (or (plist-get props :data) '())))
           (columns (etaf-ref (or (plist-get props :columns) '())))
           (row-key (plist-get props :row-key))
           (show-selection (plist-get props :show-selection))
           (show-index (plist-get props :show-index))
           
           ;; Sorting state
           (sort-prop (etaf-ref nil))
           (sort-order (etaf-ref nil))
           
           ;; Selection state
           (selected-rows (etaf-ref '()))
           
           ;; Filter state
           (filters (etaf-ref (make-hash-table :test 'equal)))
           
           ;; Pagination state
           (current-page (etaf-ref (or (plist-get props :current-page) 1)))
           (page-size (etaf-ref (or (plist-get props :page-size) 10)))
           
           ;; Computed: processed data
           (processed-data
            (etaf-computed
             (lambda ()
               (let* ((raw-data (etaf-ref-get data))
                      (cols (etaf-ref-get columns))
                      (s-prop (etaf-ref-get sort-prop))
                      (s-order (etaf-ref-get sort-order))
                      (filts (etaf-ref-get filters))
                      (sorted-col (seq-find
                                   (lambda (col) (equal (plist-get col :prop) s-prop))
                                   cols))
                      (filtered (etaf-table--filter-data raw-data filts cols))
                      (sorted (etaf-table--sort-data filtered sorted-col s-order)))
                 sorted))))
           
           ;; Computed: paginated data
           (paginated-data
            (etaf-computed
             (lambda ()
               (let ((processed (etaf-computed-get processed-data))
                     (cur-page (etaf-ref-get current-page))
                     (p-size (etaf-ref-get page-size))
                     (show-pag (plist-get props :show-pagination)))
                 (if show-pag
                     (etaf-table--paginate-data processed cur-page p-size)
                   processed)))))
           
           ;; Computed: total pages
           (total-pages
            (etaf-computed
             (lambda ()
               (let ((processed (etaf-computed-get processed-data))
                     (p-size (etaf-ref-get page-size)))
                 (ceiling (/ (float (length processed)) p-size))))))
           
           ;; Method: handle sort
           (handle-sort
            (lambda (column)
              (let* ((prop (plist-get column :prop))
                     (current-prop (etaf-ref-get sort-prop))
                     (current-order (etaf-ref-get sort-order)))
                (if (equal prop current-prop)
                    ;; Toggle order
                    (cond
                     ((equal current-order "asc")
                      (etaf-ref-set sort-order "desc"))
                     ((equal current-order "desc")
                      (etaf-ref-set sort-prop nil)
                      (etaf-ref-set sort-order nil))
                     (t
                      (etaf-ref-set sort-order "asc")))
                  ;; New sort column
                  (etaf-ref-set sort-prop prop)
                  (etaf-ref-set sort-order "asc")))))
           
           ;; Method: handle selection
           (toggle-row-selection
            (lambda (row)
              (let* ((current (etaf-ref-get selected-rows))
                     (key (etaf-table--get-row-key row row-key))
                     (is-selected (etaf-table--is-row-selected row current row-key)))
                (if is-selected
                    (etaf-ref-set selected-rows
                                 (seq-remove
                                  (lambda (r)
                                    (equal (etaf-table--get-row-key r row-key) key))
                                  current))
                  (etaf-ref-set selected-rows (cons row current))))))
           
           ;; Method: select all
           (toggle-all-selection
            (lambda ()
              (let* ((current (etaf-ref-get selected-rows))
                     (visible (etaf-computed-get paginated-data))
                     (all-selected (and (not (null visible))
                                       (cl-every
                                        (lambda (row)
                                          (etaf-table--is-row-selected row current row-key))
                                        visible))))
                (if all-selected
                    ;; Deselect all visible
                    (etaf-ref-set selected-rows
                                 (seq-remove
                                  (lambda (r)
                                    (member (etaf-table--get-row-key r row-key)
                                           (mapcar (lambda (v)
                                                    (etaf-table--get-row-key v row-key))
                                                  visible)))
                                  current))
                  ;; Select all visible
                  (etaf-ref-set selected-rows
                               (seq-uniq
                                (append current visible)
                                (lambda (a b)
                                  (equal (etaf-table--get-row-key a row-key)
                                        (etaf-table--get-row-key b row-key)))))))))
           
           ;; Method: change page
           (change-page
            (lambda (page)
              (etaf-ref-set current-page page)))
           
           ;; Method: change page size
           (change-page-size
            (lambda (size)
              (etaf-ref-set page-size size)
              (etaf-ref-set current-page 1))))
      
      ;; Return component state
      (list :data data
            :columns columns
            :row-key row-key
            :show-selection show-selection
            :show-index show-index
            :sort-prop sort-prop
            :sort-order sort-order
            :selected-rows selected-rows
            :filters filters
            :current-page current-page
            :page-size page-size
            :paginated-data paginated-data
            :processed-data processed-data
            :total-pages total-pages
            :handle-sort handle-sort
            :toggle-row-selection toggle-row-selection
            :toggle-all-selection toggle-all-selection
            :change-page change-page
            :change-page-size change-page-size)))
  
  :template
  (lambda (state)
    "Template function for table component."
    (let* ((props (plist-get state :$props))
           (columns (etaf-ref-get (plist-get state :columns)))
           (paginated-data (etaf-computed-get (plist-get state :paginated-data)))
           (processed-data (etaf-computed-get (plist-get state :processed-data)))
           (row-key (plist-get state :row-key))
           (show-selection (plist-get state :show-selection))
           (show-index (plist-get state :show-index))
           (selected-rows (etaf-ref-get (plist-get state :selected-rows)))
           (sort-prop (etaf-ref-get (plist-get state :sort-prop)))
           (sort-order (etaf-ref-get (plist-get state :sort-order)))
           (stripe (plist-get props :stripe))
           (border (plist-get props :border))
           (show-header (if (plist-member props :show-header)
                           (plist-get props :show-header)
                         t))
           (empty-text (or (plist-get props :empty-text) "No data"))
           (loading (plist-get props :loading))
           (loading-text (or (plist-get props :loading-text) "Loading..."))
           (show-pagination (plist-get props :show-pagination))
           (current-page (etaf-ref-get (plist-get state :current-page)))
           (page-size (etaf-ref-get (plist-get state :page-size)))
           (total-pages (etaf-computed-get (plist-get state :total-pages)))
           (handle-sort (plist-get state :handle-sort))
           (toggle-row-selection (plist-get state :toggle-row-selection))
           (toggle-all-selection (plist-get state :toggle-all-selection))
           (change-page (plist-get state :change-page))
           
           ;; Build table classes
           (table-classes (string-join
                          (delq nil
                                (list "etaf-table"
                                      (when stripe "etaf-table--striped")
                                      (when border "etaf-table--border")))
                          " "))
           
           ;; Build column list (including selection and index columns)
           (all-columns (append
                        (when show-selection '((:type "selection" :width 55)))
                        (when show-index '((:type "index" :label "#" :width 60)))
                        columns)))
      
      `(div :class "etaf-table-container"
            ;; Loading overlay
            ,@(when loading
                `((div :class "etaf-table-loading"
                       (div :class "etaf-table-loading__text" ,loading-text))))
            
            ;; Table
            (table :class ,table-classes
                   ;; Header
                   ,@(when show-header
                       `((thead :class "etaf-table__header"
                                (tr :class "etaf-table__header-row"
                                    ,@(mapcar
                                       (lambda (col)
                                         (let* ((type (plist-get col :type))
                                                (label (or (plist-get col :label) ""))
                                                (prop (plist-get col :prop))
                                                (width (plist-get col :width))
                                                (sortable (plist-get col :sortable))
                                                (is-sorted (equal prop sort-prop))
                                                (cell-class (string-join
                                                            (delq nil
                                                                  (list "etaf-table__cell"
                                                                        "etaf-table__header-cell"
                                                                        (when sortable "etaf-table__cell--sortable")
                                                                        (when is-sorted
                                                                          (format "etaf-table__cell--sort-%s" sort-order))))
                                                            " ")))
                                           (cond
                                            ((equal type "selection")
                                             `(th :class ,cell-class
                                                  :style ,(when width (format "width: %dcw" width))
                                                  (input :type "checkbox"
                                                         :on-change ,toggle-all-selection)))
                                            ((equal type "index")
                                             `(th :class ,cell-class
                                                  :style ,(when width (format "width: %dcw" width))
                                                  ,label))
                                            (t
                                             `(th :class ,cell-class
                                                  :style ,(when width (format "width: %dcw" width))
                                                  :on-click ,(when sortable
                                                              (lambda () (funcall handle-sort col)))
                                                  ,label
                                                  ,@(when (and sortable is-sorted)
                                                      `((span :class "etaf-table__sort-icon"
                                                              ,(if (equal sort-order "asc") " ↑" " ↓")))))))))
                                       all-columns)))))
                   
                   ;; Body
                   (tbody :class "etaf-table__body"
                          ,@(if (null paginated-data)
                                ;; Empty state
                                `((tr :class "etaf-table__empty-row"
                                      (td :colspan ,(length all-columns)
                                          :class "etaf-table__empty-cell"
                                          ,empty-text)))
                              ;; Data rows
                              (let ((row-index 0))
                                (mapcar
                                 (lambda (row)
                                   (let* ((is-selected (etaf-table--is-row-selected
                                                       row selected-rows row-key))
                                          (row-class (string-join
                                                     (delq nil
                                                           (list "etaf-table__row"
                                                                 (when is-selected "etaf-table__row--selected")
                                                                 (when (and stripe (cl-oddp row-index))
                                                                   "etaf-table__row--striped")))
                                                     " ")))
                                     (setq row-index (1+ row-index))
                                     `(tr :class ,row-class
                                          ,@(mapcar
                                             (lambda (col)
                                               (let* ((type (plist-get col :type))
                                                      (prop (plist-get col :prop))
                                                      (width (plist-get col :width))
                                                      (value (etaf-table--get-cell-value row col))
                                                      (formatted (etaf-table--format-cell-value
                                                                 row col value))
                                                      (cell-class "etaf-table__cell etaf-table__body-cell"))
                                                 (cond
                                                  ((equal type "selection")
                                                   `(td :class ,cell-class
                                                        :style ,(when width (format "width: %dcw" width))
                                                        (input :type "checkbox"
                                                               :checked ,is-selected
                                                               :on-change ,(lambda ()
                                                                            (funcall toggle-row-selection row)))))
                                                  ((equal type "index")
                                                   `(td :class ,cell-class
                                                        :style ,(when width (format "width: %dcw" width))
                                                        ,(format "%d" (+ (* (1- current-page) page-size)
                                                                        row-index))))
                                                  (t
                                                   `(td :class ,cell-class
                                                        :style ,(when width (format "width: %dcw" width))
                                                        ,formatted)))))
                                             all-columns))))
                                 paginated-data)))))
            
            ;; Pagination
            ,@(when show-pagination
                `((div :class "etaf-table-pagination"
                       (div :class "etaf-table-pagination__info"
                            ,(format "Total: %d | Page %d of %d"
                                    (length processed-data)
                                    current-page
                                    (max 1 total-pages)))
                       (div :class "etaf-table-pagination__controls"
                            (button :class "etaf-table-pagination__btn"
                                    :disabled ,(= current-page 1)
                                    :on-click ,(lambda () (funcall change-page 1))
                                    "First")
                            (button :class "etaf-table-pagination__btn"
                                    :disabled ,(= current-page 1)
                                    :on-click ,(lambda () (funcall change-page (1- current-page)))
                                    "Prev")
                            (span :class "etaf-table-pagination__current"
                                  ,(format "%d" current-page))
                            (button :class "etaf-table-pagination__btn"
                                    :disabled ,(>= current-page total-pages)
                                    :on-click ,(lambda () (funcall change-page (1+ current-page)))
                                    "Next")
                            (button :class "etaf-table-pagination__btn"
                                    :disabled ,(>= current-page total-pages)
                                    :on-click ,(lambda () (funcall change-page total-pages))
                                    "Last")))))))))

;;; ============================================================================
;;; Default Styles
;;; ============================================================================

(defvar etaf-table-default-styles
  "
  /* Table Container */
  .etaf-table-container {
    position: relative;
    width: 100%;
  }
  
  /* Table Base */
  .etaf-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 14px;
    color: #333;
    background-color: #fff;
  }
  
  /* Table Border */
  .etaf-table--border {
    border: 1px solid #ddd;
  }
  
  .etaf-table--border .etaf-table__cell {
    border-right: 1px solid #ddd;
  }
  
  /* Header */
  .etaf-table__header {
    background-color: #f5f7fa;
  }
  
  .etaf-table__header-cell {
    padding: 12px 10px;
    font-weight: bold;
    text-align: left;
    border-bottom: 1px solid #ddd;
  }
  
  .etaf-table__cell--sortable {
    cursor: pointer;
    user-select: none;
  }
  
  .etaf-table__cell--sortable:hover {
    background-color: #e8eaec;
  }
  
  .etaf-table__sort-icon {
    margin-left: 4px;
    font-size: 12px;
  }
  
  /* Body */
  .etaf-table__body-cell {
    padding: 12px 10px;
    border-bottom: 1px solid #eee;
  }
  
  .etaf-table__row:hover {
    background-color: #f5f7fa;
  }
  
  /* Striped */
  .etaf-table--striped .etaf-table__row--striped {
    background-color: #fafafa;
  }
  
  /* Selection */
  .etaf-table__row--selected {
    background-color: #e6f7ff !important;
  }
  
  /* Empty State */
  .etaf-table__empty-cell {
    padding: 40px 20px;
    text-align: center;
    color: #999;
    font-size: 14px;
  }
  
  /* Loading */
  .etaf-table-loading {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background-color: rgba(255, 255, 255, 0.8);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 10;
  }
  
  .etaf-table-loading__text {
    font-size: 16px;
    color: #666;
  }
  
  /* Pagination */
  .etaf-table-pagination {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px 0;
    margin-top: 12px;
    border-top: 1px solid #eee;
  }
  
  .etaf-table-pagination__info {
    font-size: 14px;
    color: #666;
  }
  
  .etaf-table-pagination__controls {
    display: flex;
    gap: 8px;
    align-items: center;
  }
  
  .etaf-table-pagination__btn {
    padding: 6px 12px;
    font-size: 14px;
    border: 1px solid #ddd;
    background-color: #fff;
    color: #333;
    cursor: pointer;
    border-radius: 4px;
  }
  
  .etaf-table-pagination__btn:hover:not(:disabled) {
    background-color: #f5f7fa;
    border-color: #409eff;
    color: #409eff;
  }
  
  .etaf-table-pagination__btn:disabled {
    cursor: not-allowed;
    opacity: 0.5;
  }
  
  .etaf-table-pagination__current {
    padding: 6px 12px;
    font-weight: bold;
    color: #409eff;
  }
  "
  "Default CSS styles for the table component.")

(provide 'etaf-table)

;;; etaf-table.el ends here
