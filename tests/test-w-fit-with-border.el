;;; test-w-fit-with-border.el --- Test w-fit with border class -*- lexical-binding: t; -*-

;;; Commentary:
;; This test validates that w-fit works correctly with border class.

;;; Code:

(require 'ert)
(require 'etaf)
(require 'etaf-layout)

(ert-deftest etaf-layout-test-w-fit-with-border ()
  "Test that w-fit works correctly when element has border.
This is the exact case from the bug report."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-fit mt-1 border"
                     (div :class "flex justify-between"
                          (span :class "border" "Read the docs")
                          (span :class "border" "click me")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree))
         (flex-node (car (dom-non-text-children layout-tree)))
         (flex-box (etaf-layout-get-box-model flex-node)))
    
    ;; The root div should have non-zero width
    (should (> (etaf-layout-box-content-width root-box) 0))
    
    ;; The flex container should have non-zero width
    ;; This was failing before the fix when parent had border
    (should (> (etaf-layout-box-content-width flex-box) 0))
    
    ;; The flex container's width should be less than or equal to root's width
    ;; (accounting for border)
    (let ((border-width (* 2 (plist-get (plist-get root-box :border) :left-width))))
      (should (<= (etaf-layout-box-content-width flex-box)
                  (etaf-layout-box-content-width root-box))))))

(ert-deftest etaf-layout-test-w-fit-without-border ()
  "Test that w-fit still works correctly without border.
This is the control case that was already working."
  (let* ((dom (etaf-etml-to-dom
               '(div :class "w-fit mt-1"
                     (div :class "flex justify-between"
                          (span :class "border" "Read the docs")
                          (span :class "border" "click me")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (root-box (etaf-layout-get-box-model layout-tree))
         (flex-node (car (dom-non-text-children layout-tree)))
         (flex-box (etaf-layout-get-box-model flex-node)))
    
    ;; Both should have non-zero widths
    (should (> (etaf-layout-box-content-width root-box) 0))
    (should (> (etaf-layout-box-content-width flex-box) 0))))

(provide 'test-w-fit-with-border)

;;; test-w-fit-with-border.el ends here
