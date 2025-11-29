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
  (should (equal (ecss-selector 'div) "div"))
  (should (equal (ecss-selector "span") "span")))

(ert-deftest etaf-ecss-test-selector-class ()
  "Test class selector."
  (should (equal (ecss-selector '(class "container")) ".container"))
  (should (equal (ecss-selector '(class "my-class")) ".my-class")))

(ert-deftest etaf-ecss-test-selector-id ()
  "Test ID selector."
  (should (equal (ecss-selector '(id "main")) "#main"))
  (should (equal (ecss-selector '(id "header")) "#header")))

(ert-deftest etaf-ecss-test-selector-combined ()
  "Test combined selector (AND)."
  (should (equal (ecss-selector '(and (tag "div") (class "box"))) "div.box"))
  (should (equal (ecss-selector '(and (tag "a") (class "link") (pseudo "hover")))
                 "a.link:hover")))

(ert-deftest etaf-ecss-test-selector-descendant ()
  "Test descendant combinator."
  (should (equal (ecss-selector '(descendant "nav" "a")) "nav a"))
  (should (equal (ecss-selector '(descendant ".container" (class "item")))
                 ".container .item")))

(ert-deftest etaf-ecss-test-selector-child ()
  "Test child combinator."
  (should (equal (ecss-selector '(child "ul" "li")) "ul > li"))
  (should (equal (ecss-selector '(child ".parent" (class "child")))
                 ".parent > .child")))

(ert-deftest etaf-ecss-test-selector-group ()
  "Test grouped selectors (OR)."
  (should (equal (ecss-selector '(or ".a" ".b" ".c")) ".a, .b, .c"))
  (should (equal (ecss-selector '(or "h1" "h2" "h3")) "h1, h2, h3")))

(ert-deftest etaf-ecss-test-selector-pseudo ()
  "Test pseudo-class selector."
  (should (equal (ecss-selector '(pseudo "hover")) ":hover"))
  (should (equal (ecss-selector '(pseudo "focus")) ":focus")))

(ert-deftest etaf-ecss-test-selector-attr ()
  "Test attribute selector."
  (should (equal (ecss-selector '(attr "disabled")) "[disabled]"))
  (should (equal (ecss-selector '(attr "type" "text")) "[type=\"text\"]")))

;;; Property Tests

(ert-deftest etaf-ecss-test-property-simple ()
  "Test simple property."
  (should (equal (ecss-property 'background "red") "background: red"))
  (should (equal (ecss-property 'color "#333") "color: #333")))

(ert-deftest etaf-ecss-test-property-with-unit ()
  "Test property with automatic unit."
  (should (equal (ecss-property 'padding 10) "padding: 10px"))
  (should (equal (ecss-property 'width 800) "width: 800px")))

(ert-deftest etaf-ecss-test-property-multiple-values ()
  "Test property with multiple values."
  (should (equal (ecss-property 'padding 10 20) "padding: 10px 20px"))
  (should (equal (ecss-property 'margin 0 'auto) "margin: 0 auto")))

(ert-deftest etaf-ecss-test-property-vertical-unit ()
  "Test vertical properties use lh unit."
  (should (equal (ecss-property 'height 10) "height: 10lh"))
  (should (equal (ecss-property 'padding-top 5) "padding-top: 5lh"))
  (should (equal (ecss-property 'margin-bottom 3) "margin-bottom: 3lh")))

;;; Rule Tests

(ert-deftest etaf-ecss-test-rule-basic ()
  "Test basic CSS rule."
  (let ((result (ecss ".box" '(background "red"))))
    (should (string-match-p "^\\.box" result))
    (should (string-match-p "background: red" result))))

(ert-deftest etaf-ecss-test-rule-multiple-declarations ()
  "Test rule with multiple declarations."
  (let ((result (ecss ".container"
                      '(width 800)
                      '(margin 0 auto)
                      '(padding 20))))
    (should (string-match-p "width: 800px" result))
    (should (string-match-p "margin: 0 auto" result))
    (should (string-match-p "padding: 20px" result))))

;;; Stylesheet Tests

(ert-deftest etaf-ecss-test-stylesheet ()
  "Test stylesheet generation."
  (let ((result (ecss-stylesheet
                 '(".a" (color "red"))
                 '(".b" (color "blue")))))
    (should (string-match-p "\\.a" result))
    (should (string-match-p "\\.b" result))
    (should (string-match-p "color: red" result))
    (should (string-match-p "color: blue" result))))

;;; TML Integration Tests

(ert-deftest etaf-ecss-test-ecss-props ()
  "Test ecss-props for TML :css attribute."
  (let ((result (ecss-props '(background "red") '(padding 10))))
    (should (equal (cdr (assq 'background result)) "red"))
    (should (equal (cdr (assq 'padding result)) "10px"))))

(ert-deftest etaf-ecss-test-ecss-style ()
  "Test ecss-style for TML :style attribute."
  (let ((result (ecss-style '(color "red") '(padding 10))))
    (should (string-match-p "color: red" result))
    (should (string-match-p "padding: 10px" result))))

(provide 'etaf-ecss-tests)
;;; etaf-ecss-tests.el ends here
