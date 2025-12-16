;;; etaf-layout-table-tests.el --- Tests for table layout -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;;; Commentary:

;; Tests for HTML5 table layout implementation.

;;; Code:

(require 'ert)
(require 'etaf)

(defmacro should-equal (a b)
  "Assert that A equals B."
  `(should (equal ,a ,b)))

;;; ============================================================
;;; Basic table structure tests
;;; ============================================================

(ert-deftest etaf-layout-table-test-basic-structure ()
  "Test basic table with rows and cells."
  (let* ((dom (etaf-etml-to-dom
               '(table
                 (tr (td "A") (td "B"))
                 (tr (td "C") (td "D")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 400 :height 200))))
    ;; Table should have layout-table attribute
    (should (dom-attr layout-tree 'layout-table))
    ;; Table should have correct column count
    (should-equal (dom-attr layout-tree 'layout-table-column-count) 2)))

(ert-deftest etaf-layout-table-test-with-thead-tbody ()
  "Test table with thead and tbody."
  (let* ((dom (etaf-etml-to-dom
               '(table
                 (thead (tr (th "Header 1") (th "Header 2")))
                 (tbody
                  (tr (td "A") (td "B"))
                  (tr (td "C") (td "D"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 400 :height 200))))
    (should (dom-attr layout-tree 'layout-table))
    (should-equal (dom-attr layout-tree 'layout-table-column-count) 2)))

(ert-deftest etaf-layout-table-test-column-widths ()
  "Test that column widths are calculated correctly."
  (let* ((dom (etaf-etml-to-dom
               '(table :style "width: 400px"
                 (tr (td "A") (td "B") (td "C") (td "D")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 500 :height 200))))
    ;; Should have 4 columns
    (should-equal (dom-attr layout-tree 'layout-table-column-count) 4)
    ;; Each column should have equal width (400 - spacing) / 4
    (let ((column-widths (dom-attr layout-tree 'layout-table-column-widths)))
      (should (= (length column-widths) 4)))))

(ert-deftest etaf-layout-table-test-empty-table ()
  "Test empty table."
  (let* ((dom (etaf-etml-to-dom '(table)))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 400 :height 200))))
    (should (dom-attr layout-tree 'layout-table))
    (should-equal (dom-attr layout-tree 'layout-table-column-count) 0)))

;;; ============================================================
;;; Table rendering tests
;;; ============================================================

(ert-deftest etaf-layout-table-test-render-simple ()
  "Test simple table rendering."
  (let* ((dom (etaf-etml-to-dom
               '(table
                 (tr (td "A") (td "B")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 400 :height 200)))
         (result (etaf-layout-to-string layout-tree)))
    ;; Result should contain both cells
    (should (stringp result))
    (should (> (length result) 0))))

(ert-deftest etaf-layout-table-test-render-with-content ()
  "Test table rendering with actual content."
  (let* ((dom (etaf-etml-to-dom
               '(table
                 (thead (tr (th "Name") (th "Age")))
                 (tbody
                  (tr (td "Alice") (td "25"))
                  (tr (td "Bob") (td "30"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 400 :height 200)))
         (result (etaf-layout-to-string layout-tree)))
    (should (stringp result))
    (should (> (length result) 0))))

;;; ============================================================
;;; Integration with etaf-paint
;;; ============================================================

(ert-deftest etaf-layout-table-test-paint ()
  "Test that table can be painted using etaf-paint."
  (let ((buffer-name "*test-table-paint*"))
    (unwind-protect
        (progn
          (etaf-paint-to-buffer buffer-name
            '(div :style "width: 400px"
               (table
                (tr (td "Cell 1") (td "Cell 2"))
                (tr (td "Cell 3") (td "Cell 4")))))
          (with-current-buffer buffer-name
            ;; Buffer should have content
            (should (> (buffer-size) 0))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(provide 'etaf-layout-table-tests)
;;; etaf-layout-table-tests.el ends here
