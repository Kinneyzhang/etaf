;;; etaf-dom-format-tests.el --- Tests for DOM format conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; This file is part of ETAF.

;;; Commentary:

;; Tests for converting CSSOM, render tree, and layout tree to DOM format.
;;
;; This test suite verifies that:
;; 1. CSSOM can be converted to/from DOM format
;; 2. Render tree can be converted to/from DOM format
;; 3. Layout tree can be converted to/from DOM format
;; 4. Round-trip conversion preserves data
;; 5. DOM format structures can be manipulated like DOM trees

;;; Code:

(require 'ert)
(require 'etaf-tml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; CSSOM DOM format tests

(ert-deftest etaf-test-cssom-to-dom-basic ()
  "Test basic CSSOM to DOM conversion."
  (let* ((dom (etaf-tml-to-dom '(div :class "test" "content")))
         (cssom (etaf-css-build-cssom dom))
         (cssom-dom (etaf-css-cssom-to-dom cssom)))
    ;; Check that result is in DOM format
    (should (listp cssom-dom))
    (should (eq (car cssom-dom) 'cssom))
    (should (listp (cadr cssom-dom)))
    ;; Check that attributes are accessible
    (let ((attrs (cadr cssom-dom)))
      (should (assq 'inline-rules attrs))
      (should (assq 'style-rules attrs))
      (should (assq 'all-rules attrs))
      (should (assq 'rule-index attrs))
      (should (assq 'cache attrs))
      (should (assq 'media-env attrs)))))

(ert-deftest etaf-test-cssom-round-trip ()
  "Test CSSOM round-trip conversion (plist -> DOM -> plist)."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "div { color: red; }"))
                 (body
                  (div :style "font-size: 14px;" "Text")))))
         (cssom-original (etaf-css-build-cssom dom))
         (cssom-dom (etaf-css-cssom-to-dom cssom-original))
         (cssom-restored (etaf-css-cssom-from-dom cssom-dom)))
    ;; Check that structure is preserved
    (should (equal (plist-get cssom-original :inline-rules)
                   (plist-get cssom-restored :inline-rules)))
    (should (equal (plist-get cssom-original :style-rules)
                   (plist-get cssom-restored :style-rules)))
    (should (equal (plist-get cssom-original :all-rules)
                   (plist-get cssom-restored :all-rules)))
    ;; Media environment should be preserved
    (should (equal (plist-get cssom-original :media-env)
                   (plist-get cssom-restored :media-env)))))

(ert-deftest etaf-test-cssom-dom-attributes ()
  "Test accessing CSSOM attributes in DOM format."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style ".button { color: blue; }"))
                 (body
                  (div :class "button" "Click me")))))
         (cssom (etaf-css-build-cssom dom))
         (cssom-dom (etaf-css-cssom-to-dom cssom))
         (attrs (cadr cssom-dom)))
    ;; Check style-rules
    (let ((style-rules (cdr (assq 'style-rules attrs))))
      (should (listp style-rules))
      (should (> (length style-rules) 0)))
    ;; Check all-rules contains the merged rules
    (let ((all-rules (cdr (assq 'all-rules attrs))))
      (should (listp all-rules))
      (should (> (length all-rules) 0)))))

;;; Render tree DOM format tests

(ert-deftest etaf-test-render-to-dom-basic ()
  "Test basic render tree to DOM conversion."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (body
                  (div :class "test" "content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-tree)))
    ;; Check that result is in DOM format
    (should (listp render-dom))
    (should (eq (car render-dom) 'render-node))
    (should (listp (cadr render-dom)))
    ;; Check that attributes are accessible
    (let ((attrs (cadr render-dom)))
      (should (assq 'tag attrs))
      (should (assq 'display attrs))
      (should (assq 'computed-style attrs))
      (should (assq 'node attrs)))))

(ert-deftest etaf-test-render-round-trip ()
  "Test render tree round-trip conversion (plist -> DOM -> plist)."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "div { display: block; color: red; }"))
                 (body
                  (div "Parent"
                   (div "Child"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-original (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-original))
         (render-restored (etaf-render-from-dom render-dom)))
    ;; Check that tag is preserved
    (should (eq (plist-get render-original :tag)
                (plist-get render-restored :tag)))
    ;; Check that display is preserved
    (should (equal (plist-get render-original :display)
                   (plist-get render-restored :display)))
    ;; Check that computed-style is preserved
    (should (equal (plist-get render-original :computed-style)
                   (plist-get render-restored :computed-style)))
    ;; Check that children count is preserved
    (should (= (length (plist-get render-original :children))
               (length (plist-get render-restored :children))))))

(ert-deftest etaf-test-render-dom-hierarchy ()
  "Test render tree DOM hierarchy with multiple levels."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (body
                  (div
                   (span "Text 1")
                   (span "Text 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-tree)))
    ;; Root should be render-node
    (should (eq (car render-dom) 'render-node))
    ;; Should have children (body)
    (let ((children (cddr render-dom)))
      (should (> (length children) 0))
      ;; First child should also be render-node
      (let ((first-child (car children)))
        (should (eq (car first-child) 'render-node))
        ;; Body should have div child
        (let ((body-children (cddr first-child)))
          (should (> (length body-children) 0)))))))

;;; Layout tree DOM format tests

(ert-deftest etaf-test-layout-to-dom-basic ()
  "Test basic layout tree to DOM conversion."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "div { width: 200px; height: 100px; }"))
                 (body
                  (div "content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-dom (etaf-layout-to-dom layout-tree)))
    ;; Check that result is in DOM format
    (should (listp layout-dom))
    (should (eq (car layout-dom) 'layout-node))
    (should (listp (cadr layout-dom)))
    ;; Check that attributes are accessible
    (let ((attrs (cadr layout-dom)))
      (should (assq 'render-node attrs))
      (should (assq 'box-model attrs))
      (should (assq 'position attrs)))))

(ert-deftest etaf-test-layout-round-trip ()
  "Test layout tree round-trip conversion (plist -> DOM -> plist)."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "div { width: 300px; height: 150px; padding-left: 10px; margin-top: 5px; }"))
                 (body
                  (div "Box")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-original (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-dom (etaf-layout-to-dom layout-original))
         (layout-restored (etaf-layout-from-dom layout-dom)))
    ;; Check that box-model is preserved
    (should (equal (plist-get layout-original :box-model)
                   (plist-get layout-restored :box-model)))
    ;; Check that position is preserved
    (should (equal (plist-get layout-original :position)
                   (plist-get layout-restored :position)))
    ;; Check that children count is preserved
    (should (= (length (plist-get layout-original :children))
               (length (plist-get layout-restored :children))))))

(ert-deftest etaf-test-layout-dom-box-model ()
  "Test layout tree DOM format preserves box model information."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "div { 
                    width: 200px; 
                    height: 100px; 
                    padding-left: 10px; 
                    padding-right: 10px;
                    margin-left: 5px;
                    border-top-width: 2px;
                  }"))
                 (body
                  (div "Box")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-dom (etaf-layout-to-dom layout-tree))
         (attrs (cadr layout-dom))
         (box-model (cdr (assq 'box-model attrs))))
    ;; Check that box-model structure exists
    (should (plistp box-model))
    (should (plist-get box-model :content))
    (should (plist-get box-model :padding))
    (should (plist-get box-model :border))
    (should (plist-get box-model :margin))))

;;; Integration tests

(ert-deftest etaf-test-full-pipeline-dom-format ()
  "Test complete pipeline: TML -> DOM -> CSSOM -> Render -> Layout, all in DOM format."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "
                    .container { width: 800px; padding-left: 20px; }
                    .box { width: 200px; height: 100px; margin-left: 10px; }"))
                 (body
                  (div :class "container"
                   (div :class "box" "Box 1")
                   (div :class "box" "Box 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (cssom-dom (etaf-css-cssom-to-dom cssom))
         (render-tree (etaf-render-build-tree dom cssom))
         (render-dom (etaf-render-to-dom render-tree))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-dom (etaf-layout-to-dom layout-tree)))
    ;; Verify all are in DOM format
    (should (eq (car cssom-dom) 'cssom))
    (should (eq (car render-dom) 'render-node))
    (should (eq (car layout-dom) 'layout-node))
    ;; Verify we can access attributes from all structures
    (should (assq 'all-rules (cadr cssom-dom)))
    (should (assq 'tag (cadr render-dom)))
    (should (assq 'box-model (cadr layout-dom)))))

(ert-deftest etaf-test-dom-format-with-media-queries ()
  "Test DOM format conversion with media queries."
  (let* ((dom (etaf-tml-to-dom 
               '(html
                 (head
                  (style "
                    .box { width: 100%; }
                    @media (min-width: 768px) {
                      .box { width: 50%; }
                    }"))
                 (body
                  (div :class "box" "Responsive")))))
         ;; Test with different viewport sizes
         (cssom-mobile (etaf-css-build-cssom dom '((type . screen) (width . 375))))
         (cssom-mobile-dom (etaf-css-cssom-to-dom cssom-mobile))
         (cssom-desktop (etaf-css-build-cssom dom '((type . screen) (width . 1024))))
         (cssom-desktop-dom (etaf-css-cssom-to-dom cssom-desktop)))
    ;; Both should be in DOM format
    (should (eq (car cssom-mobile-dom) 'cssom))
    (should (eq (car cssom-desktop-dom) 'cssom))
    ;; Media environments should be preserved
    (let ((mobile-env (cdr (assq 'media-env (cadr cssom-mobile-dom))))
          (desktop-env (cdr (assq 'media-env (cadr cssom-desktop-dom)))))
      (should (= (cdr (assq 'width mobile-env)) 375))
      (should (= (cdr (assq 'width desktop-env)) 1024)))))

(provide 'etaf-dom-format-tests)
;;; etaf-dom-format-tests.el ends here
