;;; etaf-table-example.el --- Table component examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: components, table, examples
;; Version: 1.0.0

;;; Commentary:

;; This file provides comprehensive examples of ETAF's table component,
;; demonstrating various features including:
;; - Basic table with data binding
;; - Sortable columns
;; - Selectable rows
;; - Custom formatters
;; - Pagination
;; - Striped and bordered styles
;; - Loading states
;; - Empty states
;; - Custom cell rendering
;;
;; Usage:
;;   (load-file "examples/etaf-table-example.el")
;;   M-x etaf-table-demo

;;; Code:

(require 'etaf)
(require 'etaf-table)

;;; ============================================================================
;;; Sample Data
;;; ============================================================================

(defvar etaf-table-example-users
  '((:id 1 :name "Alice Johnson" :age 28 :email "alice@example.com" 
     :department "Engineering" :status "active" :salary 85000)
    (:id 2 :name "Bob Smith" :age 32 :email "bob@example.com"
     :department "Marketing" :status "active" :salary 72000)
    (:id 3 :name "Charlie Brown" :age 25 :email "charlie@example.com"
     :department "Engineering" :status "inactive" :salary 68000)
    (:id 4 :name "Diana Prince" :age 35 :email "diana@example.com"
     :department "Sales" :status "active" :salary 95000)
    (:id 5 :name "Eve Davis" :age 29 :email "eve@example.com"
     :department "Engineering" :status "active" :salary 78000)
    (:id 6 :name "Frank Miller" :age 41 :email "frank@example.com"
     :department "Management" :status "active" :salary 120000)
    (:id 7 :name "Grace Lee" :age 27 :email "grace@example.com"
     :department "Design" :status "active" :salary 71000)
    (:id 8 :name "Henry Wilson" :age 38 :email "henry@example.com"
     :department "Engineering" :status "inactive" :salary 88000)
    (:id 9 :name "Iris Chen" :age 31 :email "iris@example.com"
     :department "Marketing" :status "active" :salary 74000)
    (:id 10 :name "Jack Robinson" :age 26 :email "jack@example.com"
     :department "Sales" :status "active" :salary 69000)
    (:id 11 :name "Kate Martinez" :age 33 :email "kate@example.com"
     :department "Engineering" :status "active" :salary 91000)
    (:id 12 :name "Leo Anderson" :age 29 :email "leo@example.com"
     :department "Design" :status "inactive" :salary 70000))
  "Sample user data for table examples.")

(defvar etaf-table-example-products
  '((:id 1 :name "Laptop" :category "Electronics" :price 1299.99 :stock 45 :rating 4.5)
    (:id 2 :name "Mouse" :category "Electronics" :price 29.99 :stock 120 :rating 4.2)
    (:id 3 :name "Keyboard" :category "Electronics" :price 79.99 :stock 80 :rating 4.7)
    (:id 4 :name "Monitor" :category "Electronics" :price 399.99 :stock 30 :rating 4.6)
    (:id 5 :name "Desk Chair" :category "Furniture" :price 249.99 :stock 25 :rating 4.3)
    (:id 6 :name "Desk Lamp" :category "Furniture" :price 49.99 :stock 60 :rating 4.1)
    (:id 7 :name "Notebook" :category "Stationery" :price 5.99 :stock 200 :rating 4.0)
    (:id 8 :name "Pen Set" :category "Stationery" :price 12.99 :stock 150 :rating 4.4))
  "Sample product data for table examples.")

;;; ============================================================================
;;; Example 1: Basic Table
;;; ============================================================================

(defun etaf-table-example-1-basic ()
  "Basic table example with simple data display."
  (interactive)
  (let ((columns '((:prop "name" :label "Name" :width 150)
                   (:prop "email" :label "Email" :width 200)
                   (:prop "department" :label "Department" :width 120)
                   (:prop "age" :label "Age" :width 80)))
        (data (seq-take etaf-table-example-users 5)))
    (etaf-paint-to-buffer "*etaf-table-basic*"
      `(div
        (h1 "Example 1: Basic Table")
        (p "Simple table with column definitions and data binding")
        (etaf-table :data ,data
                    :columns ,columns
                    :show-header t
                    :row-key "id")))))

;;; ============================================================================
;;; Example 2: Striped and Bordered Table
;;; ============================================================================

(defun etaf-table-example-2-striped-border ()
  "Table with striped rows and borders."
  (interactive)
  (let ((columns '((:prop "id" :label "ID" :width 60)
                   (:prop "name" :label "Product" :width 150)
                   (:prop "category" :label "Category" :width 120)
                   (:prop "price" :label "Price" :width 100
                    :formatter (lambda (row col value)
                                (format "$%.2f" value)))
                   (:prop "stock" :label "Stock" :width 80)))
        (data etaf-table-example-products))
    (etaf-paint-to-buffer "*etaf-table-striped*"
      `(div
        (h1 "Example 2: Striped & Bordered Table")
        (p "Table with striped rows and borders for better readability")
        (etaf-table :data ,data
                    :columns ,columns
                    :stripe t
                    :border t
                    :show-header t
                    :row-key "id")))))

;;; ============================================================================
;;; Example 3: Sortable Columns
;;; ============================================================================

(defun etaf-table-example-3-sortable ()
  "Table with sortable columns."
  (interactive)
  (let ((columns '((:prop "name" :label "Name" :width 150 :sortable t)
                   (:prop "age" :label "Age" :width 80 :sortable t)
                   (:prop "department" :label "Department" :width 120 :sortable t)
                   (:prop "salary" :label "Salary" :width 120 :sortable t
                    :formatter (lambda (row col value)
                                (format "$%d" value)))
                   (:prop "status" :label "Status" :width 100)))
        (data etaf-table-example-users))
    (etaf-paint-to-buffer "*etaf-table-sortable*"
      `(div
        (h1 "Example 3: Sortable Columns")
        (p "Click on column headers to sort the data. Click again to reverse order.")
        (etaf-table :data ,data
                    :columns ,columns
                    :stripe t
                    :show-header t
                    :row-key "id"
                    :default-sort (:prop "name" :order "asc"))))))

;;; ============================================================================
;;; Example 4: Selection
;;; ============================================================================

(defun etaf-table-example-4-selection ()
  "Table with row selection."
  (interactive)
  (let ((columns '((:prop "id" :label "ID" :width 60)
                   (:prop "name" :label "Name" :width 150)
                   (:prop "department" :label "Department" :width 120)
                   (:prop "status" :label "Status" :width 100
                    :formatter (lambda (row col value)
                                (if (equal value "active")
                                    "✓ Active"
                                  "✗ Inactive")))))
        (data (seq-take etaf-table-example-users 8)))
    (etaf-paint-to-buffer "*etaf-table-selection*"
      `(div
        (h1 "Example 4: Row Selection")
        (p "Select individual rows or use the header checkbox to select all")
        (etaf-table :data ,data
                    :columns ,columns
                    :show-selection t
                    :stripe t
                    :border t
                    :show-header t
                    :row-key "id")))))

;;; ============================================================================
;;; Example 5: Index Column
;;; ============================================================================

(defun etaf-table-example-5-index ()
  "Table with index column."
  (interactive)
  (let ((columns '((:prop "name" :label "Product" :width 150)
                   (:prop "category" :label "Category" :width 120)
                   (:prop "price" :label "Price" :width 100
                    :formatter (lambda (row col value)
                                (format "$%.2f" value)))
                   (:prop "rating" :label "Rating" :width 80
                    :formatter (lambda (row col value)
                                (format "%.1f ★" value)))))
        (data etaf-table-example-products))
    (etaf-paint-to-buffer "*etaf-table-index*"
      `(div
        (h1 "Example 5: Index Column")
        (p "Table with auto-incrementing index column")
        (etaf-table :data ,data
                    :columns ,columns
                    :show-index t
                    :stripe t
                    :show-header t
                    :row-key "id")))))

;;; ============================================================================
;;; Example 6: Custom Formatters
;;; ============================================================================

(defun etaf-table-example-6-formatters ()
  "Table with custom cell formatters."
  (interactive)
  (let ((columns '((:prop "name" :label "Name" :width 150)
                   (:prop "age" :label "Age" :width 80
                    :formatter (lambda (row col value)
                                (format "%d years" value)))
                   (:prop "salary" :label "Salary" :width 120
                    :sortable t
                    :formatter (lambda (row col value)
                                (let ((formatted (format "$%,d" value)))
                                  ;; Simple thousands separator
                                  (replace-regexp-in-string
                                   "\\([0-9]\\)\\([0-9]\\{3\\}\\)$"
                                   "\\1,\\2" formatted))))
                   (:prop "status" :label "Status" :width 120
                    :formatter (lambda (row col value)
                                (if (equal value "active")
                                    "✓ Active"
                                  "✗ Inactive")))
                   (:prop "department" :label "Dept" :width 100)))
        (data (seq-take etaf-table-example-users 6)))
    (etaf-paint-to-buffer "*etaf-table-formatters*"
      `(div
        (h1 "Example 6: Custom Formatters")
        (p "Customize how data is displayed using formatter functions")
        (etaf-table :data ,data
                    :columns ,columns
                    :stripe t
                    :border t
                    :show-header t
                    :row-key "id")))))

;;; ============================================================================
;;; Example 7: Pagination
;;; ============================================================================

(defun etaf-table-example-7-pagination ()
  "Table with pagination."
  (interactive)
  (let ((columns '((:prop "id" :label "ID" :width 60)
                   (:prop "name" :label "Name" :width 150 :sortable t)
                   (:prop "email" :label "Email" :width 200)
                   (:prop "department" :label "Department" :width 120)
                   (:prop "status" :label "Status" :width 100)))
        (data etaf-table-example-users))
    (etaf-paint-to-buffer "*etaf-table-pagination*"
      `(div
        (h1 "Example 7: Pagination")
        (p "Navigate through large datasets with pagination controls")
        (etaf-table :data ,data
                    :columns ,columns
                    :stripe t
                    :show-header t
                    :row-key "id"
                    :show-pagination t
                    :page-size 5
                    :current-page 1)))))

;;; ============================================================================
;;; Example 8: Empty State
;;; ============================================================================

(defun etaf-table-example-8-empty ()
  "Table with empty state."
  (interactive)
  (let ((columns '((:prop "name" :label "Name" :width 150)
                   (:prop "email" :label "Email" :width 200)
                   (:prop "department" :label "Department" :width 120)))
        (data '()))
    (etaf-paint-to-buffer "*etaf-table-empty*"
      `(div
        (h1 "Example 8: Empty State")
        (p "Table displays a message when no data is available")
        (etaf-table :data ,data
                    :columns ,columns
                    :border t
                    :show-header t
                    :empty-text "No employees found")))))

;;; ============================================================================
;;; Example 9: Loading State
;;; ============================================================================

(defun etaf-table-example-9-loading ()
  "Table with loading state."
  (interactive)
  (let ((columns '((:prop "name" :label "Name" :width 150)
                   (:prop "email" :label "Email" :width 200)
                   (:prop "department" :label "Department" :width 120)))
        (data (seq-take etaf-table-example-users 5)))
    (etaf-paint-to-buffer "*etaf-table-loading*"
      `(div
        (h1 "Example 9: Loading State")
        (p "Table displays a loading overlay when data is being fetched")
        (etaf-table :data ,data
                    :columns ,columns
                    :stripe t
                    :show-header t
                    :row-key "id"
                    :loading t
                    :loading-text "Loading data...")))))

;;; ============================================================================
;;; Example 10: Complete Feature Demo
;;; ============================================================================

(defun etaf-table-example-10-complete ()
  "Comprehensive table with all features."
  (interactive)
  (let ((columns '((:prop "name" :label "Employee" :width 150 :sortable t)
                   (:prop "age" :label "Age" :width 70 :sortable t)
                   (:prop "department" :label "Department" :width 120 :sortable t)
                   (:prop "salary" :label "Salary" :width 120 :sortable t
                    :formatter (lambda (row col value)
                                (format "$%,d" value)))
                   (:prop "status" :label "Status" :width 100
                    :formatter (lambda (row col value)
                                (if (equal value "active")
                                    "✓ Active"
                                  "✗ Inactive")))))
        (data etaf-table-example-users))
    (etaf-paint-to-buffer "*etaf-table-complete*"
      `(div
        (h1 "Example 10: Complete Feature Demo")
        (p "All table features combined:")
        (ul
         (li "Sortable columns - Click headers to sort")
         (li "Row selection - Select individual or all rows")
         (li "Index column - Auto-incrementing row numbers")
         (li "Custom formatters - Formatted salary and status")
         (li "Pagination - Navigate through pages")
         (li "Striped & bordered - Enhanced visual appearance"))
        (etaf-table :data ,data
                    :columns ,columns
                    :show-selection t
                    :show-index t
                    :stripe t
                    :border t
                    :show-header t
                    :row-key "id"
                    :show-pagination t
                    :page-size 5
                    :default-sort (:prop "name" :order "asc"))))))

;;; ============================================================================
;;; Interactive Demo Function
;;; ============================================================================

(defun etaf-table-demo ()
  "Run interactive table component demonstrations."
  (interactive)
  (let ((choice (completing-read
                 "Select table example: "
                 '("1. Basic Table"
                   "2. Striped & Bordered"
                   "3. Sortable Columns"
                   "4. Row Selection"
                   "5. Index Column"
                   "6. Custom Formatters"
                   "7. Pagination"
                   "8. Empty State"
                   "9. Loading State"
                   "10. Complete Feature Demo"
                   "All Examples")
                 nil t)))
    (cond
     ((string-prefix-p "1" choice) (etaf-table-example-1-basic))
     ((string-prefix-p "2" choice) (etaf-table-example-2-striped-border))
     ((string-prefix-p "3" choice) (etaf-table-example-3-sortable))
     ((string-prefix-p "4" choice) (etaf-table-example-4-selection))
     ((string-prefix-p "5" choice) (etaf-table-example-5-index))
     ((string-prefix-p "6" choice) (etaf-table-example-6-formatters))
     ((string-prefix-p "7" choice) (etaf-table-example-7-pagination))
     ((string-prefix-p "8" choice) (etaf-table-example-8-empty))
     ((string-prefix-p "9" choice) (etaf-table-example-9-loading))
     ((string-prefix-p "10" choice) (etaf-table-example-10-complete))
     ((string-prefix-p "All" choice)
      (etaf-table-example-1-basic)
      (etaf-table-example-2-striped-border)
      (etaf-table-example-3-sortable)
      (etaf-table-example-4-selection)
      (etaf-table-example-5-index)
      (etaf-table-example-6-formatters)
      (etaf-table-example-7-pagination)
      (etaf-table-example-8-empty)
      (etaf-table-example-9-loading)
      (etaf-table-example-10-complete)
      (message "All examples created. Check buffers: *etaf-table-*")))))

;;; ============================================================================
;;; Quick Test Functions
;;; ============================================================================

(defun etaf-table-quick-test ()
  "Quick test of the table component."
  (interactive)
  (etaf-table-example-10-complete))

(provide 'etaf-table-example)

;;; etaf-table-example.el ends here
