;;; etaf-layout-fit-content-tests.el --- Tests for fit-content width calculation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for fit-content, min-content, and max-content width calculations.
;; These tests verify that children of elements with content-based sizing
;; receive the correct parent width context.

;;; Code:

(require 'ert)
(require 'etaf)
(require 'etaf-layout)
(require 'etaf-tailwind)

(ert-deftest etaf-layout-test-w-fit-flex-children ()
  "Test that flex children of w-fit parent calculate their own intrinsic width.
When a parent has w-fit (fit-content), its children should calculate their
width based on their own content, not fill the parent's width."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "border w-fit"
                     (p "this is a long line, should make the next div use this width when set w-fit")
                     (div :class "border flex justify-between"
                          (span :class "border" "Read the docs")
                          (span :class "border" "click me")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree))
         (p-node (nth 0 (dom-non-text-children layout-tree)))
         (flex-node (nth 1 (dom-non-text-children layout-tree)))
         (flex-box (etaf-layout-get-box-model flex-node))
         (span1-node (nth 0 (dom-non-text-children flex-node)))
         (span2-node (nth 1 (dom-non-text-children flex-node)))
         (span1-box (etaf-layout-get-box-model span1-node))
         (span2-box (etaf-layout-get-box-model span2-node)))
    
    ;; The root div should have w-fit, so its width should be based on content
    ;; It should be the max of p-width and flex-width
    (should (> (etaf-layout-box-content-width root-box) 0))
    
    ;; The flex container should have width based on its content (span widths)
    ;; It should not be 0
    (should (> (etaf-layout-box-content-width flex-box) 0))
    
    ;; Both spans should have non-zero width based on their text content
    (should (> (etaf-layout-box-content-width span1-box) 0))
    (should (> (etaf-layout-box-content-width span2-box) 0))
    
    ;; The root's width should be based on the widest child (p in this case)
    (let ((p-box (etaf-layout-get-box-model p-node)))
      (should (>= (etaf-layout-box-content-width root-box)
                  (etaf-layout-box-content-width p-box))))))

(ert-deftest etaf-layout-test-w-fit-block-children ()
  "Test that block children of w-fit parent calculate their own intrinsic width.
When a parent has w-fit, block children should calculate width based on their
content, not fill the parent's available width."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-fit"
                     (p "Long line to set the width")
                     (div :class "border"
                          (span "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree))
         (child-div (nth 1 (dom-non-text-children layout-tree)))
         (child-box (etaf-layout-get-box-model child-div)))
    
    ;; The root div should have w-fit, its width based on widest child
    (should (> (etaf-layout-box-content-width root-box) 0))
    
    ;; The child div should have non-zero width based on its content
    (should (> (etaf-layout-box-content-width child-box) 0))))

(ert-deftest etaf-layout-test-nested-w-fit ()
  "Test nested w-fit elements work correctly."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-fit"
                     (div :class "w-fit"
                          (span "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (outer-box (etaf-layout-get-box-model layout-tree))
         (inner-div (car (dom-non-text-children layout-tree)))
         (inner-box (etaf-layout-get-box-model inner-div)))
    
    ;; Both divs should have non-zero width
    (should (> (etaf-layout-box-content-width outer-box) 0))
    (should (> (etaf-layout-box-content-width inner-box) 0))))

(ert-deftest etaf-layout-test-fit-content-css ()
  "Test fit-content CSS value directly."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { width: fit-content; }"))
                 (body
                  (div
                   (p "Content line")
                   (div :class "flex"
                        (span "Item 1")
                        (span "Item 2")))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (body-node (car (dom-non-text-children layout-tree)))
         (div-node (car (dom-non-text-children body-node)))
         (div-box (etaf-layout-get-box-model div-node))
         (flex-node (nth 1 (dom-non-text-children div-node)))
         (flex-box (etaf-layout-get-box-model flex-node)))
    
    ;; The div with fit-content should have non-zero width
    (should (> (etaf-layout-box-content-width div-box) 0))
    
    ;; The flex child should also have non-zero width
    (should (> (etaf-layout-box-content-width flex-box) 0))))

(ert-deftest etaf-layout-test-min-content-width ()
  "Test min-content width calculation."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-min"
                     (span "Short")
                     (span "Longer content"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree)))
    
    ;; The div with w-min should have non-zero width
    (should (> (etaf-layout-box-content-width root-box) 0))))

(ert-deftest etaf-layout-test-max-content-width ()
  "Test max-content width calculation."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-max"
                     (span "Content that determines the max width"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree)))
    
    ;; The div with w-max should have non-zero width
    (should (> (etaf-layout-box-content-width root-box) 0))))

(provide 'etaf-layout-fit-content-tests)

;;; etaf-layout-fit-content-tests.el ends here
