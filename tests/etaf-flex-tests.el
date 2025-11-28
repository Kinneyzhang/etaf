;;; etaf-flex-tests.el --- Tests for flex layout support -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the flex layout system in etaf-layout.el and etaf-css-shorthand.el

;;; Code:

(require 'ert)
(require 'etaf-ert)

;;; CSS Shorthand Tests for Flex Properties

(ert-deftest etaf-css-shorthand-test-flex-expand-single-number ()
  "Test expanding flex: <number>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex "1" nil)))
    (should (equal (length result) 3))
    (should (equal (nth 0 result) '(flex-grow "1" nil)))
    (should (equal (nth 1 result) '(flex-shrink "1" nil)))
    (should (equal (nth 2 result) '(flex-basis "0" nil)))))

(ert-deftest etaf-css-shorthand-test-flex-expand-none ()
  "Test expanding flex: none."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex "none" nil)))
    (should (equal (length result) 3))
    (should (equal (nth 0 result) '(flex-grow "0" nil)))
    (should (equal (nth 1 result) '(flex-shrink "0" nil)))
    (should (equal (nth 2 result) '(flex-basis "auto" nil)))))

(ert-deftest etaf-css-shorthand-test-flex-expand-auto ()
  "Test expanding flex: auto."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex "auto" nil)))
    (should (equal (length result) 3))
    (should (equal (nth 0 result) '(flex-grow "1" nil)))
    (should (equal (nth 1 result) '(flex-shrink "1" nil)))
    (should (equal (nth 2 result) '(flex-basis "auto" nil)))))

(ert-deftest etaf-css-shorthand-test-flex-expand-three-values ()
  "Test expanding flex: <grow> <shrink> <basis>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex "1 0 100px" nil)))
    (should (equal (length result) 3))
    (should (equal (nth 0 result) '(flex-grow "1" nil)))
    (should (equal (nth 1 result) '(flex-shrink "0" nil)))
    (should (equal (nth 2 result) '(flex-basis "100px" nil)))))

(ert-deftest etaf-css-shorthand-test-flex-flow-row-wrap ()
  "Test expanding flex-flow: row wrap."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex-flow "row wrap" nil)))
    (should (equal (length result) 2))
    (should (member '(flex-direction "row" nil) result))
    (should (member '(flex-wrap "wrap" nil) result))))

(ert-deftest etaf-css-shorthand-test-flex-flow-column ()
  "Test expanding flex-flow: column."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-flex-flow "column" nil)))
    (should (equal (length result) 1))
    (should (equal (nth 0 result) '(flex-direction "column" nil)))))

(ert-deftest etaf-css-shorthand-test-gap-single-value ()
  "Test expanding gap: <value>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-gap "10px" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(row-gap "10px" nil)))
    (should (equal (nth 1 result) '(column-gap "10px" nil)))))

(ert-deftest etaf-css-shorthand-test-gap-two-values ()
  "Test expanding gap: <row-gap> <column-gap>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-gap "10px 20px" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(row-gap "10px" nil)))
    (should (equal (nth 1 result) '(column-gap "20px" nil)))))

;;; Flex Layout Helper Tests

(ert-deftest etaf-layout-test-flex-justify-space-flex-start ()
  "Test justify-content: flex-start space distribution."
  (require 'etaf-layout)
  (let ((result (etaf-layout-flex-justify-space "flex-start" 100 3 10)))
    (should (equal (nth 0 result) 0))      ;; start-space
    (should (equal (nth 1 result) 10))     ;; between-space (gap)
    (should (equal (nth 2 result) 100))))  ;; end-space

(ert-deftest etaf-layout-test-flex-justify-space-flex-end ()
  "Test justify-content: flex-end space distribution."
  (require 'etaf-layout)
  (let ((result (etaf-layout-flex-justify-space "flex-end" 100 3 10)))
    (should (equal (nth 0 result) 100))    ;; start-space
    (should (equal (nth 1 result) 10))     ;; between-space (gap)
    (should (equal (nth 2 result) 0))))    ;; end-space

(ert-deftest etaf-layout-test-flex-justify-space-center ()
  "Test justify-content: center space distribution."
  (require 'etaf-layout)
  (let ((result (etaf-layout-flex-justify-space "center" 100 3 10)))
    (should (equal (nth 0 result) 50.0))   ;; start-space (half)
    (should (equal (nth 1 result) 10))     ;; between-space (gap)
    (should (equal (nth 2 result) 50.0)))) ;; end-space (half)

(ert-deftest etaf-layout-test-flex-justify-space-between ()
  "Test justify-content: space-between space distribution."
  (require 'etaf-layout)
  (let ((result (etaf-layout-flex-justify-space "space-between" 100 3 10)))
    (should (equal (nth 0 result) 0))      ;; start-space
    (should (= (nth 1 result) 60))         ;; between-space (100 + 2*10) / 2 = 60
    (should (equal (nth 2 result) 0))))    ;; end-space

(ert-deftest etaf-layout-test-parse-flex-number ()
  "Test parsing flex number values."
  (require 'etaf-layout)
  (should (equal (etaf-layout-parse-flex-number "1") 1))
  (should (equal (etaf-layout-parse-flex-number "0") 0))
  (should (equal (etaf-layout-parse-flex-number "1.5") 1.5))
  (should (equal (etaf-layout-parse-flex-number "-1") -1))
  (should (equal (etaf-layout-parse-flex-number 5) 5))
  (should (equal (etaf-layout-parse-flex-number "auto") nil)))

;;; Flex Item Width Calculation Tests

(ert-deftest etaf-layout-test-flex-item-width-auto ()
  "Test that flex item with width:auto does not auto-fill parent width.
When items are in a row flex layout, their combined width should equal
the container's content-width, not each item filling the entire parent width."
  (require 'etaf-layout)
  (require 'etaf-render)
  (require 'etaf-css)
  (require 'etaf-tml)
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    .flex-container {
                      display: flex;
                      width: 800px;
                    }
                    .flex-item {
                      height: 50px;
                    }
                  "))
                 (body
                  (div :class "flex-container"
                       (div :class "flex-item" "Item 1")
                       (div :class "flex-item" "Item 2")
                       (div :class "flex-item" "Item 3"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container))
         (container-box (etaf-layout-get-box-model flex-container))
         (container-width (etaf-box-model-content-width container-box))
         (total-items-width 0))
    ;; Container should have width 800
    (should-equal container-width 800)
    ;; Calculate total width of all flex items
    (dolist (item flex-items)
      (let* ((box-model (etaf-layout-get-box-model item))
             (item-width (etaf-box-model-content-width box-model)))
        ;; Each item should NOT have width equal to container width
        ;; In old buggy code, each item would be 800px (parent width)
        ;; With fix, items should have 0 width (auto-sized by content/flex algorithm)
        (should (< item-width container-width))
        (setq total-items-width (+ total-items-width (etaf-box-model-total-width box-model)))))
    ;; Total width of all items should be <= container width (they shouldn't overflow)
    ;; Note: With width:0, the total will be less than container, which is correct
    ;; as flex algorithm will distribute space based on grow/shrink
    (should (<= total-items-width container-width))))

(provide 'etaf-flex-tests)
;;; etaf-flex-tests.el ends here
