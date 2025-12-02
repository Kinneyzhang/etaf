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
    (require 'etaf-css)
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
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
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container))
         (container-box (etaf-layout-get-box-model flex-container))
         (container-width (etaf-layout-box-content-width container-box))
         (total-items-width 0))
    ;; Container should have width 800
    (should (equal container-width 800))
    ;; Calculate total width of all flex items
    (dolist (item flex-items)
      (let* ((box-model (etaf-layout-get-box-model item))
             (item-width (etaf-layout-box-content-width box-model)))
        ;; Each item should NOT have width equal to container width
        ;; In old buggy code, each item would be 800px (parent width)
        ;; With fix, items should have 0 width (auto-sized by content/flex algorithm)
        (should (< item-width container-width))
        (setq total-items-width (+ total-items-width (etaf-layout-box-total-width box-model)))))
    ;; Total width of all items should be <= container width (they shouldn't overflow)
    ;; Note: With width:0, the total will be less than container, which is correct
    ;; as flex algorithm will distribute space based on grow/shrink
    (should (<= total-items-width container-width))))

;;; Flex-grow and Flex-shrink Tests

(ert-deftest etaf-layout-test-flex-grow-stretches-items ()
  "Test that flex-grow > 0 properly stretches items to fill container.
When container width is greater than the total width of child elements,
children with flex-grow > 0 should be stretched proportionally."
  (require 'etaf-layout)
    (require 'etaf-css)
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .flex-container {
                      display: flex;
                      width: 800px;
                    }
                    .flex-item {
                      width: 100px;
                      height: 50px;
                      flex-grow: 1;
                    }
                  "))
                 (body
                  (div :class "flex-container"
                       (div :class "flex-item" "A")
                       (div :class "flex-item" "B"))))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container))
         (container-box (etaf-layout-get-box-model flex-container))
         (container-width (etaf-layout-box-content-width container-box)))
    ;; Container should have width 800
    (should (equal container-width 800))
    ;; Each item should be stretched from 100px to about 400px (800/2 items)
    ;; because both items have equal flex-grow: 1
    (let* ((item1-box (etaf-layout-get-box-model (nth 0 flex-items)))
           (item2-box (etaf-layout-get-box-model (nth 1 flex-items)))
           (item1-width (etaf-layout-box-content-width item1-box))
           (item2-width (etaf-layout-box-content-width item2-box)))
      ;; Each item should be stretched beyond original 100px width
      ;; With equal flex-grow, they should each get 400px (half of 800px)
      (should (> item1-width 100))
      (should (> item2-width 100))
      ;; Total width should equal container width
      (should (= (+ (etaf-layout-box-total-width item1-box)
                    (etaf-layout-box-total-width item2-box))
                 container-width)))))

(ert-deftest etaf-layout-test-flex-grow-proportional ()
  "Test that flex-grow distributes space proportionally.
An item with flex-grow: 2 should grow twice as much as one with flex-grow: 1."
  (require 'etaf-layout)
    (require 'etaf-css)
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .flex-container {
                      display: flex;
                      width: 900px;
                    }
                    .item1 {
                      width: 100px;
                      flex-grow: 1;
                    }
                    .item2 {
                      width: 100px;
                      flex-grow: 2;
                    }
                  "))
                 (body
                  (div :class "flex-container"
                       (div :class "item1" "A")
                       (div :class "item2" "B"))))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container)))
    ;; Total initial width: 200px, free space: 700px
    ;; item1 gets 700/3 ≈ 233.33px extra, item2 gets 2*700/3 ≈ 466.67px extra
    ;; Final: item1 ≈ 333.33px, item2 ≈ 566.67px
    (let* ((item1-box (etaf-layout-get-box-model (nth 0 flex-items)))
           (item2-box (etaf-layout-get-box-model (nth 1 flex-items)))
           (item1-width (etaf-layout-box-content-width item1-box))
           (item2-width (etaf-layout-box-content-width item2-box)))
      ;; Both should be stretched beyond original 100px
      (should (> item1-width 100))
      (should (> item2-width 100))
      ;; item2 should have gotten more grow space than item1 (roughly 2x)
      ;; Growth amount: item1 grew by (item1-width - 100), item2 grew by (item2-width - 100)
      (let ((item1-growth (- item1-width 100))
            (item2-growth (- item2-width 100)))
        ;; item2 growth should be approximately 2x item1 growth
        ;; Allow some tolerance for rounding
        (should (> item2-growth (* 1.5 item1-growth)))))))

(ert-deftest etaf-layout-test-flex-shrink-reduces-items ()
  "Test that flex-shrink > 0 properly shrinks items when overflow occurs.
When container width is less than the total width of child elements,
children with flex-shrink > 0 should be reduced proportionally."
  (require 'etaf-layout)
    (require 'etaf-css)
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .flex-container {
                      display: flex;
                      width: 300px;
                    }
                    .flex-item {
                      width: 200px;
                      height: 50px;
                      flex-shrink: 1;
                    }
                  "))
                 (body
                  (div :class "flex-container"
                       (div :class "flex-item" "A")
                       (div :class "flex-item" "B"))))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container)))
    ;; Total initial width: 400px, container: 300px, overflow: 100px
    ;; Each item should shrink by 50px to fit (200 - 50 = 150px each)
    (let* ((item1-box (etaf-layout-get-box-model (nth 0 flex-items)))
           (item2-box (etaf-layout-get-box-model (nth 1 flex-items)))
           (item1-width (etaf-layout-box-content-width item1-box))
           (item2-width (etaf-layout-box-content-width item2-box)))
      ;; Each item should be shrunk below original 200px width
      (should (< item1-width 200))
      (should (< item2-width 200))
      ;; Total width should equal container width
      (should (= (+ (etaf-layout-box-total-width item1-box)
                    (etaf-layout-box-total-width item2-box))
                 300)))))

(ert-deftest etaf-layout-test-flex-grow-zero-no-stretch ()
  "Test that flex-grow: 0 items don't stretch."
  (require 'etaf-layout)
    (require 'etaf-css)
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .flex-container {
                      display: flex;
                      width: 800px;
                    }
                    .no-grow {
                      width: 100px;
                      flex-grow: 0;
                    }
                    .grow {
                      width: 100px;
                      flex-grow: 1;
                    }
                  "))
                 (body
                  (div :class "flex-container"
                       (div :class "no-grow" "A")
                       (div :class "grow" "B"))))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (flex-container (car (dom-non-text-children body-node)))
         (flex-items (dom-non-text-children flex-container)))
    (let* ((no-grow-box (etaf-layout-get-box-model (nth 0 flex-items)))
           (grow-box (etaf-layout-get-box-model (nth 1 flex-items)))
           (no-grow-width (etaf-layout-box-content-width no-grow-box))
           (grow-width (etaf-layout-box-content-width grow-box)))
      ;; no-grow item should stay at 100px
      (should (= no-grow-width 100))
      ;; grow item should expand to fill remaining space (700px)
      (should (= grow-width 700)))))

;;; Place-* shorthand property tests

(ert-deftest etaf-css-shorthand-test-place-content-single ()
  "Test expanding place-content with a single value."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-place-content "center" nil)))
    (should (equal (length result) 2))
    (should (member '(align-content "center" nil) result))
    (should (member '(justify-content "center" nil) result))))

(ert-deftest etaf-css-shorthand-test-place-content-two-values ()
  "Test expanding place-content with two values."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-place-content "center space-between" nil)))
    (should (equal (length result) 2))
    (should (member '(align-content "center" nil) result))
    (should (member '(justify-content "space-between" nil) result))))

(ert-deftest etaf-css-shorthand-test-place-items-single ()
  "Test expanding place-items with a single value."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-place-items "stretch" nil)))
    (should (equal (length result) 2))
    (should (member '(align-items "stretch" nil) result))
    (should (member '(justify-items "stretch" nil) result))))

(ert-deftest etaf-css-shorthand-test-place-self-two-values ()
  "Test expanding place-self with two values."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-place-self "start end" nil)))
    (should (equal (length result) 2))
    (should (member '(align-self "start" nil) result))
    (should (member '(justify-self "end" nil) result))))

;;; Inline Element Wrapping Tests

(ert-deftest etaf-layout-test-inline-elements-wrap ()
  "Test that inline elements wrap when they exceed container width."
  (require 'etaf-layout)
    (require 'etaf-css)
  (require 'etaf-etml)
  ;; Test the helper function directly
  (let* ((str1 (make-string 50 ?A))  ;; A string of 50 'A' characters
         (str2 (make-string 50 ?B))  ;; A string of 50 'B' characters
         (str3 (make-string 50 ?C))  ;; A string of 50 'C' characters
         (inline-strings (list str1 str2 str3))
         (total-width (apply #'+ (mapcar #'string-pixel-width inline-strings)))
         ;; Set container width to be less than total width but more than 2 items
         (container-width (* (string-pixel-width str1) 2)))
    ;; Without wrapping (container-width = nil)
    (let ((result-no-wrap (etaf-layout--merge-inline-with-wrap inline-strings nil)))
      ;; Should be on one line (no newlines from wrapping, just from etaf-lines-concat)
      (should (stringp result-no-wrap)))
    ;; With wrapping (container-width set)
    (let ((result-wrap (etaf-layout--merge-inline-with-wrap inline-strings container-width)))
      ;; Should contain newlines due to wrapping
      (should (stringp result-wrap))
      ;; When items exceed container, result should have multiple lines
      (when (> total-width container-width)
        (should (> (length (split-string result-wrap "\n")) 1))))))

(provide 'etaf-flex-tests)
;;; etaf-flex-tests.el ends here
