;;; test-flex-item-width.el --- Test for flex item width bug fix

;; This file tests the fix for the issue where nested flex containers
;; were incorrectly expanding to full parent width when used as flex items.

(require 'ert)
(require 'etaf)

(ert-deftest etaf-test-nested-flex-container-as-flex-item ()
  "Test that nested flex containers shrink-wrap when used as flex items.
This tests the bug fix where a flex container used as a flex item
should shrink-wrap its content, not expand to full parent width."
  (let* ((etml '(div :class "flex"
                     (span "A")
                     (div :class "flex"
                          (span "B")
                          (span "C"))))
         (dom (etaf-etml-to-dom etml nil))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 800 :height nil)))
         (children (dom-children layout-tree))
         (nested-flex (nth 1 children))
         (nested-box (etaf-layout-get-box-model nested-flex)))
    
    ;; The nested flex div should shrink-wrap (width=0) not expand to 800
    (should (equal (plist-get (plist-get nested-box :content) :width) 0))
    
    ;; Test that the layout renders correctly
    (let ((result (etaf-paint-string etml nil nil 800)))
      ;; Should contain both "A" and "BC"
      (should (string-match-p "A" result))
      (should (string-match-p "BC" result)))))

(ert-deftest etaf-test-flex-justify-between-with-nested-flex ()
  "Test justify-between works correctly with nested flex containers.
This tests the original problem: span and nested flex div should be
properly spaced with justify-between."
  (let* ((etml '(div :class "flex justify-between"
                     (span "LEFT")
                     (div :class "flex gap-2"
                          (span "R1")
                          (span "R2"))))
         (result (etaf-paint-string etml nil nil 800)))
    
    ;; Result should contain all text
    (should (string-match-p "LEFT" result))
    (should (string-match-p "R1" result))
    (should (string-match-p "R2" result))
    
    ;; There should be spacing between items (represented as text properties)
    ;; The result length should be greater than just the visible text
    (should (> (length result) (length "LEFTR1R2")))))

(ert-deftest etaf-test-flex-vs-block-level ()
  "Test that flex containers behave as block-level when not in flex container.
When a flex container is in a normal (non-flex) parent, it should
behave as a block-level element, appearing on its own line."
  (let* ((etml '(div
                     (span "A")
                     (div :class "flex"
                          (span "B")
                          (span "C"))))
         (result (etaf-paint-string etml nil nil 800)))
    
    ;; Should contain both elements
    (should (string-match-p "A" result))
    (should (string-match-p "BC" result))
    ;; Should have newline separating them (may have space before newline)
    (should (string-match-p " *\n" result))))

(provide 'test-flex-item-width)
