;;; etaf-vdom-textcontent-tests.el --- Tests for VNode textContent rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Keywords: test, vdom, rendering

;;; Commentary:

;; Tests for fixing the textContent rendering issue where style elements
;; with :textContent property were not rendering their CSS content to DOM.

;;; Code:

(require 'etaf-vdom)
(require 'etaf-etml)
(require 'etaf-ecss)
(require 'etaf-ert)

;;; VNode textContent Rendering Tests

(ert-deftest etaf-vdom-test-textcontent-rendering ()
  "Test that VNodes with :textContent prop are rendered correctly to DOM.
This tests the fix for the bug where style elements created with :textContent
were not rendering their CSS content."
  (let* ((css-content "p { color: red; }")
         (vnode (etaf-vdom-element 'style
                                   (list :textContent css-content)
                                   nil))
         (dom (etaf-vdom-render vnode)))
    ;; DOM should be (style nil "p { color: red; }")
    (should (eq (car dom) 'style))
    (should (equal (nth 1 dom) nil)) ; no attributes
    (should (equal (nth 2 dom) css-content)))) ; CSS content as child

(ert-deftest etaf-vdom-test-textcontent-with-attributes ()
  "Test that textContent works with other attributes."
  (let* ((css-content ".box { padding: 10px; }")
         (vnode (etaf-vdom-element 'style
                                   (list :id "my-style" :textContent css-content)
                                   nil))
         (dom (etaf-vdom-render vnode)))
    ;; Check tag
    (should (eq (car dom) 'style))
    ;; Check attributes include id but not textContent
    (let ((attrs (nth 1 dom)))
      (should (equal (cdr (assq 'id attrs)) "my-style"))
      (should-not (assq 'textContent attrs)))
    ;; Check CSS content as child
    (should (equal (nth 2 dom) css-content))))

(ert-deftest etaf-vdom-test-textcontent-precedence ()
  "Test that :textContent takes precedence over children when both exist."
  (let* ((text-content "From textContent")
         (children-content "From children")
         (vnode (etaf-vdom-element 'div
                                   (list :textContent text-content)
                                   (list (etaf-vdom-text children-content))))
         (dom (etaf-vdom-render vnode)))
    ;; textContent should take precedence
    (should (equal (nth 2 dom) text-content))
    ;; Should not have the children content
    (should-not (equal (nth 2 dom) children-content))))

;;; ECSS Integration Tests

(ert-deftest etaf-vdom-test-ecss-style-rendering ()
  "Test that ecss tags generate proper style elements with CSS content in DOM."
  (let* ((etml '(div :id "parent"
                     (ecss "p{text-red-200}")
                     (p "Test paragraph")))
         ;; Compile and render through the full pipeline
         (render-fn (etaf-compile etml))
         (vnode (funcall render-fn nil))
         (dom (etaf-vdom-render vnode)))
    ;; Check that DOM has a style element
    (let ((style-elem (dom-by-tag dom 'style)))
      (should style-elem)
      ;; Style element should have CSS content as a child (not in attributes)
      (let ((css-content (nth 2 style-elem)))
        (should (stringp css-content))
        ;; Should contain color property from text-red-200
        (should (string-match-p "color:" css-content))))))

(ert-deftest etaf-vdom-test-ecss-multiple-rules ()
  "Test that multiple ecss rules are properly rendered to DOM."
  (let* ((etml '(div
                  (ecss "p{text-red-200}" "a{text-blue-400}")
                  (p "Paragraph")
                  (a :href "#" "Link")))
         (render-fn (etaf-compile etml))
         (vnode (funcall render-fn nil))
         (dom (etaf-vdom-render vnode)))
    (let ((style-elem (dom-by-tag dom 'style)))
      (should style-elem)
      (let ((css-content (nth 2 style-elem)))
        (should (stringp css-content))
        ;; Should contain rules for both p and a
        (should (string-match-p "p.*color:" css-content))
        (should (string-match-p "a.*color:" css-content))))))

(ert-deftest etaf-vdom-test-ecss-scoped-css ()
  "Test that scoped ecss generates CSS with scope class prefix."
  (let* ((etml '(div :id "container"
                     (ecss "p{text-green-400}")
                     (p "Scoped paragraph")))
         (render-fn (etaf-compile etml))
         (vnode (funcall render-fn nil))
         (dom (etaf-vdom-render vnode)))
    ;; Get the parent div
    (let* ((parent-div (dom-by-id dom "container"))
           (class-attr (dom-attr parent-div 'class))
           (style-elem (dom-by-tag dom 'style))
           (css-content (nth 2 style-elem)))
      ;; Parent should have scope class
      (should (string-match-p "etaf-scope-" class-attr))
      ;; CSS should have scope class prefix
      (should (string-match-p "\\.etaf-scope-[0-9]+ p" css-content))
      ;; CSS should have color property
      (should (string-match-p "color:" css-content)))))

(ert-deftest etaf-vdom-test-ecss-color-classes ()
  "Test that various Tailwind color classes are rendered correctly."
  (let* ((etml '(div
                  (ecss "p{text-red-200}"
                        "ul>li:nth-child(odd)>p{text-green-400}"
                        "ul li>p code{text-orange-400}"
                        "a{text-blue-400}")
                  (p "Red text")
                  (ul (li (p (code "Code") " Orange")))
                  (a :href "#" "Blue link")))
         (render-fn (etaf-compile etml))
         (vnode (funcall render-fn nil))
         (dom (etaf-vdom-render vnode)))
    (let ((style-elem (dom-by-tag dom 'style)))
      (should style-elem)
      (let ((css-content (nth 2 style-elem)))
        (should (stringp css-content))
        ;; Verify all color rules are present
        (should (string-match-p "p.*{.*color:" css-content))
        (should (string-match-p "ul>li:nth-child(odd)>p.*{.*color:" css-content))
        (should (string-match-p "ul li>p code.*{.*color:" css-content))
        (should (string-match-p "a.*{.*color:" css-content))))))

(provide 'etaf-vdom-textcontent-tests)
;;; etaf-vdom-textcontent-tests.el ends here
