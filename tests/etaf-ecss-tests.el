;;; etaf-ecss-tests.el --- Tests for ECSS (Emacs CSS Expressions) -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Keywords: test, css, emacs

;;; Commentary:

;; Tests for etaf-ecss.el - Emacs-style CSS expressions

;;; Code:

(require 'etaf-ecss)
(require 'etaf-ert)

;;; Selector Tests

(ert-deftest etaf-ecss-test-selector-tag ()
  "Test tag selector."
  (should (equal (etaf-ecss-selector 'div) "div"))
  (should (equal (etaf-ecss-selector "span") "span")))

(ert-deftest etaf-ecss-test-selector-class ()
  "Test class selector."
  (should (equal (etaf-ecss-selector '(class "container")) ".container"))
  (should (equal (etaf-ecss-selector '(class "my-class")) ".my-class")))

(ert-deftest etaf-ecss-test-selector-id ()
  "Test ID selector."
  (should (equal (etaf-ecss-selector '(id "main")) "#main"))
  (should (equal (etaf-ecss-selector '(id "header")) "#header")))

(ert-deftest etaf-ecss-test-selector-combined ()
  "Test combined selector (AND)."
  (should (equal (etaf-ecss-selector '(and (tag "div") (class "box"))) "div.box"))
  (should (equal (etaf-ecss-selector '(and (tag "a") (class "link") (pseudo "hover")))
                 "a.link:hover")))

(ert-deftest etaf-ecss-test-selector-descendant ()
  "Test descendant combinator."
  (should (equal (etaf-ecss-selector '(descendant "nav" "a")) "nav a"))
  (should (equal (etaf-ecss-selector '(descendant ".container" (class "item")))
                 ".container .item")))

(ert-deftest etaf-ecss-test-selector-child ()
  "Test child combinator."
  (should (equal (etaf-ecss-selector '(child "ul" "li")) "ul > li"))
  (should (equal (etaf-ecss-selector '(child ".parent" (class "child")))
                 ".parent > .child")))

(ert-deftest etaf-ecss-test-selector-group ()
  "Test grouped selectors (OR)."
  (should (equal (etaf-ecss-selector '(or ".a" ".b" ".c")) ".a, .b, .c"))
  (should (equal (etaf-ecss-selector '(or "h1" "h2" "h3")) "h1, h2, h3")))

(ert-deftest etaf-ecss-test-selector-pseudo ()
  "Test pseudo-class selector."
  (should (equal (etaf-ecss-selector '(pseudo "hover")) ":hover"))
  (should (equal (etaf-ecss-selector '(pseudo "focus")) ":focus")))

(ert-deftest etaf-ecss-test-selector-attr ()
  "Test attribute selector."
  (should (equal (etaf-ecss-selector '(attr "disabled")) "[disabled]"))
  (should (equal (etaf-ecss-selector '(attr "type" "text")) "[type=\"text\"]")))

;;; Property Tests

(ert-deftest etaf-ecss-test-property-simple ()
  "Test simple property."
  (should (equal (etaf-ecss-property 'background "red") "background: red"))
  (should (equal (etaf-ecss-property 'color "#333") "color: #333")))

(ert-deftest etaf-ecss-test-property-with-unit ()
  "Test property with automatic unit."
  (should (equal (etaf-ecss-property 'padding 10) "padding: 10px"))
  (should (equal (etaf-ecss-property 'width 800) "width: 800px")))

(ert-deftest etaf-ecss-test-property-multiple-values ()
  "Test property with multiple values."
  (should (equal (etaf-ecss-property 'padding 10 20) "padding: 10px 20px"))
  (should (equal (etaf-ecss-property 'margin 0 'auto) "margin: 0 auto")))

(ert-deftest etaf-ecss-test-property-vertical-unit ()
  "Test vertical properties use lh unit."
  (should (equal (etaf-ecss-property 'height 10) "height: 10lh"))
  (should (equal (etaf-ecss-property 'padding-top 5) "padding-top: 5lh"))
  (should (equal (etaf-ecss-property 'margin-bottom 3) "margin-bottom: 3lh")))

;;; Rule Tests

(ert-deftest etaf-ecss-test-rule-basic ()
  "Test basic CSS rule."
  (let ((result (etaf-ecss ".box" '(background "red"))))
    (should (string-match-p "^\\.box" result))
    (should (string-match-p "background: red" result))))

(ert-deftest etaf-ecss-test-rule-multiple-declarations ()
  "Test rule with multiple declarations."
  (let ((result (etaf-ecss ".container"
                      '(width 800)
                      '(margin 0 auto)
                      '(padding 20))))
    (should (string-match-p "width: 800px" result))
    (should (string-match-p "margin: 0 auto" result))
    (should (string-match-p "padding: 20px" result))))

;;; Stylesheet Tests

(ert-deftest etaf-ecss-test-stylesheet ()
  "Test stylesheet generation."
  (let ((result (etaf-ecss
                 '(".a" (color "red"))
                 '(".b" (color "blue")))))
    (should (string-match-p "\\.a" result))
    (should (string-match-p "\\.b" result))
    (should (string-match-p "color: red" result))
    (should (string-match-p "color: blue" result))))

;;; TML Integration Tests

(ert-deftest etaf-ecss-test-etaf-ecss-props ()
  "Test etaf-ecss-props for TML :style attribute (list format)."
  (let ((result (etaf-ecss-props '(background "red") '(padding 10))))
    (should (equal (cdr (assq 'background result)) "red"))
    (should (equal (cdr (assq 'padding result)) "10px"))))

(ert-deftest etaf-ecss-test-etaf-ecss-style ()
  "Test etaf-ecss-style for TML :style attribute."
  (let ((result (etaf-ecss-style '(color "red") '(padding 10))))
    (should (string-match-p "color: red" result))
    (should (string-match-p "padding: 10px" result))))

;;; Tailwind CSS Support Tests

(ert-deftest etaf-ecss-test-tailwind-class-detection ()
  "Test that Tailwind CSS utility classes are correctly detected."
  (should (etaf-ecss--tailwind-class-p '(flex)))
  (should (etaf-ecss--tailwind-class-p '(bg-red-500)))
  (should (etaf-ecss--tailwind-class-p '(p-4)))
  (should (etaf-ecss--tailwind-class-p '(items-center)))
  ;; Not a Tailwind class (has multiple elements)
  (should-not (etaf-ecss--tailwind-class-p '(background "red")))
  ;; Not a Tailwind class (unknown utility)
  (should-not (etaf-ecss--tailwind-class-p '(xyz-unknown-class))))

(ert-deftest etaf-ecss-test-tailwind-in-declaration-block ()
  "Test Tailwind CSS utilities in etaf-ecss-declaration-block."
  (let ((result (etaf-ecss-declaration-block '(flex) '(items-center))))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))))

(ert-deftest etaf-ecss-test-tailwind-mixed-with-standard ()
  "Test mixing Tailwind CSS utilities with standard declarations."
  (let ((result (etaf-ecss-declaration-block
                 '(flex)
                 '(bg-red-500)
                 '(padding 10)
                 '(color "blue"))))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "background-color: #ef4444" result))
    (should (string-match-p "padding: 10px" result))
    (should (string-match-p "color: blue" result))))

(ert-deftest etaf-ecss-test-tailwind-in-ecss-rule ()
  "Test Tailwind CSS utilities in a complete ecss rule."
  (let ((result (etaf-ecss ".card"
                  '(flex)
                  '(items-center)
                  '(justify-center)
                  '(bg-blue-500)
                  '(p-4))))
    (should (string-match-p "^\\.card" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "justify-content: center" result))
    (should (string-match-p "background-color: #3b82f6" result))))

(ert-deftest etaf-ecss-test-tailwind-in-props ()
  "Test Tailwind CSS utilities in etaf-ecss-props."
  (let ((result (etaf-ecss-props '(flex) '(bg-red-500) '(padding 10))))
    (should (equal (cdr (assq 'display result)) "flex"))
    (should (equal (cdr (assq 'background-color result)) "#ef4444"))
    (should (equal (cdr (assq 'padding result)) "10px"))))

(ert-deftest etaf-ecss-test-tailwind-in-style ()
  "Test Tailwind CSS utilities in etaf-ecss-style."
  (let ((result (etaf-ecss-style '(flex) '(items-center) '(bg-blue-500))))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))))

(ert-deftest etaf-ecss-test-tailwind-multi-property-utility ()
  "Test Tailwind utilities that generate multiple CSS properties."
  ;; p-4 generates multiple padding properties
  ;; Note: Horizontal (left/right) uses cw (character width) units for Emacs compatibility
  ;; Vertical (top/bottom) uses lh (line height) units
  (let ((result (etaf-ecss-declaration-block '(p-4))))
    (should (string-match-p "padding-top: 4lh" result))
    (should (string-match-p "padding-right: 4cw" result))
    (should (string-match-p "padding-bottom: 4lh" result))
    (should (string-match-p "padding-left: 4cw" result))))

(ert-deftest etaf-ecss-test-tailwind-typography ()
  "Test Tailwind typography utilities in ecss."
  (let ((result (etaf-ecss-declaration-block
                 '(text-lg)
                 '(font-bold)
                 '(italic))))
    (should (string-match-p "font-weight: 700" result))
    (should (string-match-p "font-style: italic" result))))

(ert-deftest etaf-ecss-test-tailwind-stylesheet ()
  "Test Tailwind CSS utilities in etaf-ecss."
  (let ((result (etaf-ecss
                 '(".header" (flex) (items-center) (bg-blue-500))
                 '(".content" (p-4) (text-gray-700)))))
    (should (string-match-p "\\.header" result))
    (should (string-match-p "\\.content" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))))

;;; ECSS in Style Tags Tests

(ert-deftest etaf-ecss-test-ecss-css-macro ()
  "Test etaf-ecss-css macro for style tags."
  (let ((result (etaf-ecss-css
                 (".box" (background "red") (padding 10))
                 (".title" (color "blue") (font-bold)))))
    (should (string-match-p "\\.box" result))
    (should (string-match-p "\\.title" result))
    (should (string-match-p "background: red" result))
    (should (string-match-p "padding: 10px" result))
    (should (string-match-p "color: blue" result))
    (should (string-match-p "font-weight: 700" result))))

(ert-deftest etaf-ecss-test-ecss-css-with-tailwind ()
  "Test etaf-ecss-css macro with Tailwind CSS utilities."
  (let ((result (etaf-ecss-css
                 (".container" (flex) (items-center) (p-4))
                 (".button" (bg-blue-500) (text-white) (rounded-lg)))))
    (should (string-match-p "\\.container" result))
    (should (string-match-p "\\.button" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))
    (should (string-match-p "border-radius: 0.5rem" result))))

(ert-deftest etaf-ecss-test-ecss-css-mixed ()
  "Test etaf-ecss-css macro with mixed Tailwind and standard CSS."
  (let ((result (etaf-ecss-css
                 (".card" (flex) (items-center) (background "white") (padding 20)))))
    (should (string-match-p "\\.card" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background: white" result))
    (should (string-match-p "padding: 20px" result))))

;;; String format Tailwind CSS Tests (simpler syntax)

(ert-deftest etaf-ecss-test-tailwind-string-basic ()
  "Test Tailwind CSS utilities as space-separated string."
  (let ((result (etaf-ecss ".card" "flex items-center justify-center")))
    (should (string-match-p "\\.card" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "justify-content: center" result))))

(ert-deftest etaf-ecss-test-tailwind-string-colors ()
  "Test Tailwind color utilities as string."
  (let ((result (etaf-ecss ".box" "bg-red-500 text-white")))
    (should (string-match-p "background-color: #ef4444" result))
    (should (string-match-p "color: #ffffff" result))))

(ert-deftest etaf-ecss-test-tailwind-string-mixed-with-props ()
  "Test mixing Tailwind string with property expressions."
  (let ((result (etaf-ecss ".container"
                  "flex items-center bg-blue-500"
                  '(padding 20)
                  '(margin 0 auto))))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))
    (should (string-match-p "padding: 20px" result))
    (should (string-match-p "margin: 0 auto" result))))

(ert-deftest etaf-ecss-test-tailwind-string-in-style ()
  "Test etaf-ecss-style with Tailwind string."
  (let ((result (etaf-ecss-style "flex items-center bg-red-500")))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #ef4444" result))))

(ert-deftest etaf-ecss-test-tailwind-string-in-props ()
  "Test etaf-ecss-props with Tailwind string."
  (let ((result (etaf-ecss-props "flex items-center")))
    (should (equal (cdr (assq 'display result)) "flex"))
    (should (equal (cdr (assq 'align-items result)) "center"))))

(ert-deftest etaf-ecss-test-tailwind-string-stylesheet ()
  "Test etaf-ecss with Tailwind string."
  (let ((result (etaf-ecss
                 '(".header" "flex items-center bg-blue-500")
                 '(".content" "p-4" (color "gray")))))
    (should (string-match-p "\\.header" result))
    (should (string-match-p "\\.content" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "background-color: #3b82f6" result))))

(ert-deftest etaf-ecss-test-ecss-css-with-string ()
  "Test etaf-ecss-css macro with string format."
  (let ((result (etaf-ecss-css
                 (".container" "flex items-center" (width 800))
                 (".box" "bg-red-500 p-4"))))
    (should (string-match-p "\\.container" result))
    (should (string-match-p "\\.box" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "width: 800px" result))
    (should (string-match-p "background-color: #ef4444" result))))

;;; ETML Style Tag with (ecss ...) Tests

(ert-deftest etaf-ecss-test-etml-style-tag-ecss ()
  "Test (ecss ...) forms in style tags are converted to CSS."
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style
                   (ecss ".test{flex items-center}")
                   (ecss "#main{bg-red-500}")))
                 (body
                  (div :class "test" "Hello")))))
         (style-node (car (dom-by-tag dom 'style)))
         (style-content (car (dom-children style-node))))
    (should (stringp style-content))
    (should (string-match-p "\\.test" style-content))
    (should (string-match-p "display: flex" style-content))
    (should (string-match-p "#main" style-content))
    (should (string-match-p "background-color: #ef4444" style-content))))

(ert-deftest etaf-ecss-test-etml-style-tag-ecss-multiple ()
  "Test (ecss ...) with multiple unified format strings in style tags."
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style
                   (ecss ".card{flex items-center}" ".button{p-4 bg-blue-500}")))
                 (body))))
         (style-node (car (dom-by-tag dom 'style)))
         (style-content (car (dom-children style-node))))
    (should (stringp style-content))
    (should (string-match-p "\\.card" style-content))
    (should (string-match-p "display: flex" style-content))
    (should (string-match-p "\\.button" style-content))
    (should (string-match-p "background-color: #3b82f6" style-content))))

;;; Unified Format Tests

(ert-deftest etaf-ecss-test-unified-format-basic ()
  "Test the basic unified ECSS format: selector{tailwind-classes}."
  (let ((result (etaf-ecss ".card{flex items-center bg-blue-500}")))
    (should (string-match-p "^\\.card" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))))

(ert-deftest etaf-ecss-test-unified-format-complex-selector ()
  "Test unified format with complex CSS selectors."
  (let ((result (etaf-ecss "div>p:nth-child(odd){pl-6px pr-2 py-1}")))
    (should (string-match-p "^div>p:nth-child(odd)" result))
    (should (string-match-p "padding-left: 6px" result))
    (should (string-match-p "padding-right: 2cw" result))
    (should (string-match-p "padding-top: 1lh" result))))

(ert-deftest etaf-ecss-test-unified-format-border ()
  "Test unified format with border utilities."
  (let ((result (etaf-ecss ".box{border border-gray-500}")))
    (should (string-match-p "border-width: 1px" result))
    (should (string-match-p "border-color: #6b7280" result))))

(ert-deftest etaf-ecss-test-unified-format-parse ()
  "Test etaf-ecss-parse function."
  (let ((parsed (etaf-ecss-parse ".container{flex items-center}")))
    (should (equal (plist-get parsed :selector) ".container"))
    (should (string-match-p "display: flex" (plist-get parsed :css-string)))
    (should (string-match-p "align-items: center" (plist-get parsed :css-string)))))

(ert-deftest etaf-ecss-test-unified-format-predicate ()
  "Test etaf-ecss-unified-p predicate."
  (should (etaf-ecss-unified-p ".card{flex}"))
  (should (etaf-ecss-unified-p "div>p:nth-child(odd){pl-6px}"))
  (should (etaf-ecss-unified-p "#main{bg-red-500}"))
  (should-not (etaf-ecss-unified-p ".card"))
  (should-not (etaf-ecss-unified-p "flex items-center"))
  ;; Empty selector or classes should fail
  (should-not (etaf-ecss-unified-p "{flex}"))
  (should-not (etaf-ecss-unified-p ".card{}"))
  (should-not (etaf-ecss-unified-p "  {flex}"))
  (should-not (etaf-ecss-unified-p ".card{  }")))

(ert-deftest etaf-ecss-test-unified-stylesheet ()
  "Test etaf-ecss with unified format strings."
  (let ((result (etaf-ecss
                 ".header{flex items-center bg-blue-500}"
                 ".content{p-4}"
                 "nav>a{text-white}")))
    (should (string-match-p "\\.header" result))
    (should (string-match-p "\\.content" result))
    (should (string-match-p "nav>a" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "color: #ffffff" result))))

(ert-deftest etaf-ecss-test-unified-stylesheet-mixed ()
  "Test etaf-ecss with mixed unified and legacy formats."
  (let ((result (etaf-ecss
                 ".header{flex items-center}"
                 '(".footer" (background "white") (padding 10)))))
    (should (string-match-p "\\.header" result))
    (should (string-match-p "\\.footer" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "background: white" result))))

(ert-deftest etaf-ecss-test-legacy-format-still-works ()
  "Test that legacy format still works after unified format addition."
  ;; Legacy with property lists
  (let ((result (etaf-ecss ".box" '(background "red") '(padding 10))))
    (should (string-match-p "background: red" result))
    (should (string-match-p "padding: 10px" result)))
  ;; Legacy with Tailwind string
  (let ((result (etaf-ecss ".card" "flex items-center")))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))))

(ert-deftest etaf-ecss-test-multiple-unified-strings ()
  "Test etaf-ecss with multiple unified format strings."
  (let ((result (etaf-ecss
                 ".header{flex items-center bg-blue-500}"
                 ".content{p-4}"
                 "nav>a{text-white}")))
    (should (string-match-p "\\.header" result))
    (should (string-match-p "\\.content" result))
    (should (string-match-p "nav>a" result))
    (should (string-match-p "display: flex" result))
    (should (string-match-p "align-items: center" result))
    (should (string-match-p "background-color: #3b82f6" result))
    (should (string-match-p "color: #ffffff" result))))

(ert-deftest etaf-ecss-test-numeric-font-size-in-unified ()
  "Test numeric font-size values in unified ECSS format."
  (let ((result (etaf-ecss "h1{text-1.6 font-bold}")))
    (should (string-match-p "^h1" result))
    (should (string-match-p "font-size: 1.6lh" result))
    (should (string-match-p "font-weight: 700" result))))

(ert-deftest etaf-ecss-test-pseudo-classes-in-unified ()
  "Test pseudo-class selectors in unified ECSS format."
  (let ((result (etaf-ecss
                 "a:hover{text-blue-700}"
                 "button:active{bg-gray-300}"
                 "input:focus{border-blue-500}"
                 "button:disabled{bg-gray-100 text-gray-500}")))
    (should (string-match-p "a:hover" result))
    (should (string-match-p "button:active" result))
    (should (string-match-p "input:focus" result))
    (should (string-match-p "button:disabled" result))
    (should (string-match-p "color: #1d4ed8" result))
    (should (string-match-p "background-color: #d1d5db" result))
    (should (string-match-p "border-color: #3b82f6" result))))

;;; Scoped ecss Tag Tests (ecss at any position with local scope)

(ert-deftest etaf-ecss-test-scoped-ecss-basic ()
  "Test ecss tag at element level creates scoped CSS."
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(div
                 (ecss ".box{bg-red-500}")
                 (div :class "box" "Hello")))))
    ;; Parent div should have scope class
    (let ((class-attr (dom-attr dom 'class)))
      (should (string-match-p "etaf-scope-[0-9]+" class-attr)))
    ;; Should have a style element as first child
    (let ((first-child (car (dom-children dom))))
      (should (eq (dom-tag first-child) 'style))
      ;; Style content should have scoped selector
      (let ((style-content (car (dom-children first-child))))
        (should (stringp style-content))
        ;; Should have scope class prefix
        (should (string-match-p "etaf-scope-[0-9]+ \\.box" style-content))
        (should (string-match-p "background-color: #ef4444" style-content))))
    ;; Sibling div should NOT need scope class (parent has it)
    (let ((second-child (cadr (dom-children dom))))
      (should (eq (dom-tag second-child) 'div))
      (let ((class-attr (dom-attr second-child 'class)))
        ;; Should have original class but NOT scope class
        (should (string-match-p "box" class-attr))
        ;; Scope class is on parent, not siblings
        (should-not (string-match-p "etaf-scope-[0-9]+" class-attr))))))

(ert-deftest etaf-ecss-test-scoped-ecss-multiple-siblings ()
  "Test ecss scopes CSS to multiple sibling elements via parent scope."
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(div
                 (ecss ".item{p-4}")
                 (span :class "item" "First")
                 (span :class "item" "Second")
                 (span :class "item" "Third")))))
    ;; Parent div should have scope class
    (let ((class-attr (dom-attr dom 'class)))
      (should (string-match-p "etaf-scope-[0-9]+" class-attr)))
    ;; Children should be processed
    (let ((children (dom-children dom)))
      ;; First child is style
      (should (eq (dom-tag (car children)) 'style))
      ;; Other children are spans, scope class is on parent not children
      (dolist (child (cdr children))
        (when (eq (dom-tag child) 'span)
          (let ((class-attr (dom-attr child 'class)))
            (should (string-match-p "item" class-attr))
            ;; Scope class is on parent, not siblings
            (should-not (string-match-p "etaf-scope-[0-9]+" class-attr))))))))

(ert-deftest etaf-ecss-test-scoped-ecss-multiple-rules ()
  "Test multiple ecss tags in same scope."
  (require 'etaf-etml)
  (let* ((dom (etaf-etml-to-dom
               '(div
                 (ecss ".header{bg-blue-500}")
                 (ecss ".content{p-4}")
                 (div :class "header" "Header")
                 (div :class "content" "Content")))))
    (let ((style-child (car (dom-children dom))))
      (should (eq (dom-tag style-child) 'style))
      (let ((style-content (car (dom-children style-child))))
        ;; Both rules should be in the style
        (should (string-match-p "\\.header" style-content))
        (should (string-match-p "\\.content" style-content))
        ;; Both should have same scope prefix
        (should (string-match-p "background-color: #3b82f6" style-content))))))

(ert-deftest etaf-ecss-test-scoped-ecss-vs-global-style ()
  "Test that ecss in style tag remains global while ecss outside is scoped."
  (require 'etaf-etml)
  ;; Global ecss in style tag
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style
                   (ecss ".global{bg-red-500}")))
                 (body
                  (div :class "global" "Global"))))))
    (let* ((style-node (car (dom-by-tag dom 'style)))
           (style-content (car (dom-children style-node))))
      ;; Should NOT have scope prefix
      (should (string-match-p "^\\.global" style-content))
      (should-not (string-match-p "etaf-scope" style-content))))
  
  ;; Scoped ecss outside style tag
  (let* ((dom (etaf-etml-to-dom
               '(div
                 (ecss ".scoped{bg-blue-500}")
                 (span :class "scoped" "Scoped")))))
    (let* ((style-node (car (dom-children dom)))
           (style-content (car (dom-children style-node))))
      ;; SHOULD have scope prefix
      (should (string-match-p "etaf-scope-[0-9]+ \\.scoped" style-content)))))

(ert-deftest etaf-ecss-test-ecss-tag-p ()
  "Test etaf-etml--ecss-tag-p predicate."
  (require 'etaf-etml)
  (should (etaf-etml--ecss-tag-p '(ecss ".box{bg-red-500}")))
  (should (etaf-etml--ecss-tag-p '(ecss ".card{flex}")))
  (should-not (etaf-etml--ecss-tag-p '(div :class "box")))
  (should-not (etaf-etml--ecss-tag-p "not a list"))
  (should-not (etaf-etml--ecss-tag-p '(style (ecss ".x{p-4}")))))

(ert-deftest etaf-ecss-test-ecss-tag-rejects-legacy-format ()
  "Test that ecss tag rejects legacy format and only accepts unified format."
  (require 'etaf-etml)
  ;; Should reject legacy format with separate selector and declarations
  (should-error (etaf-etml-to-dom
                 '(div
                   (ecss ".box" "bg-red-500")
                   (div :class "box" "Hello"))))
  ;; Should reject legacy format with property lists
  (should-error (etaf-etml-to-dom
                 '(div
                   (ecss ".box" (padding 10))
                   (div :class "box" "Hello"))))
  ;; Should accept unified format
  (should (etaf-etml-to-dom
           '(div
             (ecss ".box{bg-red-500}")
             (div :class "box" "Hello")))))

(provide 'etaf-ecss-tests)
;;; etaf-ecss-tests.el ends here
