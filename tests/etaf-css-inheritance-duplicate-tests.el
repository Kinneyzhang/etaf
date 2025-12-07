;;; etaf-css-inheritance-duplicate-tests.el --- Tests for inheritance duplication issue -*- lexical-binding: t; -*-

;; This test file attempts to reproduce the issue described in the problem statement:
;; "etml中在父元素上面设置的文本相关的样式，比如字体，加粗等，在继承时会导致子元素中有多个重复的样式"
;; (In etml, text-related styles set on parent elements, such as fonts, bold, etc., 
;;  cause multiple duplicate styles in child elements during inheritance.)

(require 'etaf-css)
(require 'etaf-css-core)
(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; Test 1: Check for duplicate entries in computed style

(message "\n=== Test 1: Single inherited property ===")
(let* ((test-dom (etaf-etml-to-dom
                  '(div :style "font-weight: bold; color: red;"
                        (p "Child text"))))
       (cssom (etaf-css-build-cssom test-dom))
       (p-node (dom-by-tag test-dom 'p))
       (computed (etaf-css-get-computed-style cssom p-node test-dom)))
  (message "Computed style: %S" computed)
  (let ((font-weight-entries (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) computed))
        (color-entries (seq-filter (lambda (prop) (eq (car prop) 'color)) computed)))
    (message "font-weight entries: %d" (length font-weight-entries))
    (message "color entries: %d" (length color-entries))
    ;; Check for duplicates
    (should (= (length font-weight-entries) 1))
    (should (= (length color-entries) 1))))

;;; Test 2: Nested inheritance (grandparent -> parent -> child)

(message "\n=== Test 2: Nested inheritance ===")
(let* ((test-dom (etaf-etml-to-dom
                  '(div :style "font-weight: bold; font-size: 16px;"
                        (p :style "color: blue;"
                           (span "Deep child")))))
       (cssom (etaf-css-build-cssom test-dom))
       (span-node (dom-by-tag test-dom 'span))
       (computed (etaf-css-get-computed-style cssom span-node test-dom)))
  (message "Computed style: %S" computed)
  (let ((font-weight-entries (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) computed))
        (font-size-entries (seq-filter (lambda (prop) (eq (car prop) 'font-size)) computed))
        (color-entries (seq-filter (lambda (prop) (eq (car prop) 'color)) computed)))
    (message "font-weight entries: %d" (length font-weight-entries))
    (message "font-size entries: %d" (length font-size-entries))
    (message "color entries: %d" (length color-entries))
    ;; Check for duplicates
    (should (= (length font-weight-entries) 1))
    (should (= (length font-size-entries) 1))
    (should (= (length color-entries) 1))))

;;; Test 3: Multiple levels with same inherited property

(message "\n=== Test 3: Multiple levels ===")
(let* ((test-dom (etaf-etml-to-dom
                  '(div :style "font-weight: bold;"
                        (section
                          (p
                            (span "Four levels deep"))))))
       (cssom (etaf-css-build-cssom test-dom))
       (span-node (dom-by-tag test-dom 'span))
       (computed (etaf-css-get-computed-style cssom span-node test-dom)))
  (message "Computed style: %S" computed)
  (let ((font-weight-entries (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) computed)))
    (message "font-weight entries: %d" (length font-weight-entries))
    ;; Should only have ONE entry for font-weight
    (should (= (length font-weight-entries) 1))))

;;; Test 4: Check that etaf-css-apply-inheritance doesn't create duplicates

(message "\n=== Test 4: Direct call to etaf-css-apply-inheritance ===")
(let* ((parent-style '((font-weight . "bold") (color . "red") (font-size . "16px")))
       ;; Child already has font-weight from a rule
       (child-style '((font-weight . "normal") (background-color . "white")))
       (result (etaf-css-apply-inheritance child-style parent-style)))
  (message "Parent style: %S" parent-style)
  (message "Child style: %S" child-style)
  (message "Result: %S" result)
  (let ((font-weight-entries (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) result)))
    (message "font-weight entries in result: %d" (length font-weight-entries))
    ;; Should only have ONE entry - the child's own value should not be overridden
    (should (= (length font-weight-entries) 1))
    ;; And it should be the child's value, not the parent's
    (should (string= (cdr (assq 'font-weight result)) "normal"))))

;;; Test 5: Check inherited properties across multiple children

(message "\n=== Test 5: Multiple siblings ===")
(let* ((test-dom (etaf-etml-to-dom
                  '(div :style "font-weight: bold; color: red;"
                        (p "First child")
                        (span "Second child")
                        (div "Third child"))))
       (cssom (etaf-css-build-cssom test-dom))
       (p-node (dom-by-tag test-dom 'p))
       (span-node (dom-by-tag test-dom 'span))
       (div-node (car (last (dom-by-tag test-dom 'div)))))
  (let ((p-computed (etaf-css-get-computed-style cssom p-node test-dom))
        (span-computed (etaf-css-get-computed-style cssom span-node test-dom))
        (div-computed (etaf-css-get-computed-style cssom div-node test-dom)))
    (message "P computed: %S" p-computed)
    (message "SPAN computed: %S" span-computed)
    (message "DIV computed: %S" div-computed)
    ;; All should have exactly one font-weight
    (should (= 1 (length (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) p-computed))))
    (should (= 1 (length (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) span-computed))))
    (should (= 1 (length (seq-filter (lambda (prop) (eq (car prop) 'font-weight)) div-computed))))))

(provide 'etaf-css-inheritance-duplicate-tests)
;;; etaf-css-inheritance-duplicate-tests.el ends here
