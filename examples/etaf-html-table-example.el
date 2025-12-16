;;; etaf-html-table-example.el --- Basic HTML5 table examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: html5, table, examples
;; Version: 1.0.0

;;; Commentary:

;; This file provides examples of basic HTML5 table tags in ETAF.
;; These are the native table elements (table, tr, td, th, etc.),
;; not the high-level etaf-table component.
;;
;; Supported HTML5 table tags:
;; - table - Table container
;; - thead - Table header group
;; - tbody - Table body group
;; - tfoot - Table footer group
;; - tr - Table row
;; - th - Table header cell
;; - td - Table data cell
;; - caption - Table caption
;; - colgroup - Column group (for styling)
;; - col - Column definition (for styling)
;;
;; Usage:
;;   (load-file "examples/etaf-html-table-example.el")
;;   M-x etaf-html-table-demo

;;; Code:

(require 'etaf)

;;; ============================================================================
;;; Example 1: Basic Table
;;; ============================================================================

(defun etaf-html-table-example-1-basic ()
  "Basic table with simple rows and cells."
  (interactive)
  (etaf-paint-to-buffer "*html-table-basic*"
    '(div :style "width: 400px"
       (h1 "Example 1: Basic HTML Table")
       (p "A simple table with two columns and three rows")
       (table
        (tr (td "Row 1, Cell 1") (td "Row 1, Cell 2"))
        (tr (td "Row 2, Cell 1") (td "Row 2, Cell 2"))
        (tr (td "Row 3, Cell 1") (td "Row 3, Cell 2"))))))

;;; ============================================================================
;;; Example 2: Table with Header
;;; ============================================================================

(defun etaf-html-table-example-2-header ()
  "Table with thead and th elements."
  (interactive)
  (etaf-paint-to-buffer "*html-table-header*"
    '(div :style "width: 500px"
       (h1 "Example 2: Table with Header")
       (p "Using thead and th for header cells")
       (table
        (thead
         (tr (th "Name") (th "Age") (th "City")))
        (tbody
         (tr (td "Alice") (td "25") (td "New York"))
         (tr (td "Bob") (td "30") (td "Los Angeles"))
         (tr (td "Charlie") (td "35") (td "Chicago")))))))

;;; ============================================================================
;;; Example 3: Table with Footer
;;; ============================================================================

(defun etaf-html-table-example-3-footer ()
  "Table with thead, tbody, and tfoot."
  (interactive)
  (etaf-paint-to-buffer "*html-table-footer*"
    '(div :style "width: 400px"
       (h1 "Example 3: Table with Footer")
       (p "Complete table structure with header, body, and footer")
       (table
        (thead
         (tr (th "Product") (th "Price")))
        (tbody
         (tr (td "Apple") (td "$1.00"))
         (tr (td "Banana") (td "$0.50"))
         (tr (td "Orange") (td "$0.75")))
        (tfoot
         (tr (td "Total") (td "$2.25")))))))

;;; ============================================================================
;;; Example 4: Table with Caption
;;; ============================================================================

(defun etaf-html-table-example-4-caption ()
  "Table with a caption element."
  (interactive)
  (etaf-paint-to-buffer "*html-table-caption*"
    '(div :style "width: 500px"
       (h1 "Example 4: Table with Caption")
       (p "Using caption to describe the table")
       (table
        (caption "Monthly Sales Report")
        (thead
         (tr (th "Month") (th "Sales")))
        (tbody
         (tr (td "January") (td "$10,000"))
         (tr (td "February") (td "$12,500"))
         (tr (td "March") (td "$15,000")))))))

;;; ============================================================================
;;; Example 5: Styled Table
;;; ============================================================================

(defun etaf-html-table-example-5-styled ()
  "Table with inline styles."
  (interactive)
  (etaf-paint-to-buffer "*html-table-styled*"
    '(div :style "width: 600px"
       (h1 "Example 5: Styled Table")
       (p "Table with custom styles using inline CSS")
       (table :style "border-collapse: collapse"
        (thead
         (tr :style "background-color: #4a90d9"
          (th :style "padding: 10px; color: white" "ID")
          (th :style "padding: 10px; color: white" "Name")
          (th :style "padding: 10px; color: white" "Status")))
        (tbody
         (tr (td :style "padding: 8px" "001") 
             (td :style "padding: 8px" "Widget A")
             (td :style "padding: 8px; color: green" "Active"))
         (tr :style "background-color: #f5f5f5"
             (td :style "padding: 8px" "002")
             (td :style "padding: 8px" "Widget B")
             (td :style "padding: 8px; color: red" "Inactive"))
         (tr (td :style "padding: 8px" "003")
             (td :style "padding: 8px" "Widget C")
             (td :style "padding: 8px; color: green" "Active")))))))

;;; ============================================================================
;;; Demo Command
;;; ============================================================================

(defun etaf-html-table-demo ()
  "Run HTML5 table examples."
  (interactive)
  (let ((choice (completing-read
                 "Select table example: "
                 '("1. Basic Table"
                   "2. Table with Header"
                   "3. Table with Footer"
                   "4. Table with Caption"
                   "5. Styled Table"
                   "All Examples")
                 nil t)))
    (cond
     ((string-prefix-p "1" choice) (etaf-html-table-example-1-basic))
     ((string-prefix-p "2" choice) (etaf-html-table-example-2-header))
     ((string-prefix-p "3" choice) (etaf-html-table-example-3-footer))
     ((string-prefix-p "4" choice) (etaf-html-table-example-4-caption))
     ((string-prefix-p "5" choice) (etaf-html-table-example-5-styled))
     ((string-prefix-p "All" choice)
      (etaf-html-table-example-1-basic)
      (etaf-html-table-example-2-header)
      (etaf-html-table-example-3-footer)
      (etaf-html-table-example-4-caption)
      (etaf-html-table-example-5-styled)
      (message "All HTML table examples created. Check buffers: *html-table-*")))))

(provide 'etaf-html-table-example)
;;; etaf-html-table-example.el ends here
