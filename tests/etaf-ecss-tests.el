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
  (let ((result (etaf-ecss-stylesheet
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
  (let ((result (etaf-ecss-declaration-block '(p-4))))
    (should (string-match-p "padding-top: 4lh" result))
    (should (string-match-p "padding-right: 4px" result))
    (should (string-match-p "padding-bottom: 4lh" result))
    (should (string-match-p "padding-left: 4px" result))))

(ert-deftest etaf-ecss-test-tailwind-typography ()
  "Test Tailwind typography utilities in ecss."
  (let ((result (etaf-ecss-declaration-block
                 '(text-lg)
                 '(font-bold)
                 '(italic))))
    (should (string-match-p "font-weight: 700" result))
    (should (string-match-p "font-style: italic" result))))

(ert-deftest etaf-ecss-test-tailwind-stylesheet ()
  "Test Tailwind CSS utilities in etaf-ecss-stylesheet."
  (let ((result (etaf-ecss-stylesheet
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

(provide 'etaf-ecss-tests)
;;; etaf-ecss-tests.el ends here
