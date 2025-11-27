;;; etaf-css-shorthand-tests.el --- Tests for CSS shorthand expansion -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for CSS shorthand property expansion

;;; Code:

(require 'ert)
(require 'etaf-css-shorthand)
(require 'etaf-css-parser)

;;; Tests for internal functions

(ert-deftest etaf-css-shorthand-test-is-length ()
  "Test length value detection."
  (should (etaf-css--is-length-p "10px"))
  (should (etaf-css--is-length-p "2em"))
  (should (etaf-css--is-length-p "50%"))
  (should (etaf-css--is-length-p "0"))
  (should (etaf-css--is-length-p "auto"))
  (should (etaf-css--is-length-p "thin"))
  (should-not (etaf-css--is-length-p "red"))
  (should-not (etaf-css--is-length-p "solid")))

(ert-deftest etaf-css-shorthand-test-is-color ()
  "Test color value detection."
  (should (etaf-css--is-color-p "red"))
  (should (etaf-css--is-color-p "blue"))
  (should (etaf-css--is-color-p "#fff"))
  (should (etaf-css--is-color-p "#ffffff"))
  (should (etaf-css--is-color-p "rgb(255, 0, 0)"))
  (should (etaf-css--is-color-p "transparent"))
  (should-not (etaf-css--is-color-p "10px"))
  (should-not (etaf-css--is-color-p "solid")))

(ert-deftest etaf-css-shorthand-test-is-border-style ()
  "Test border style value detection."
  (should (etaf-css--is-border-style-p "solid"))
  (should (etaf-css--is-border-style-p "dashed"))
  (should (etaf-css--is-border-style-p "none"))
  (should-not (etaf-css--is-border-style-p "red"))
  (should-not (etaf-css--is-border-style-p "10px")))

(ert-deftest etaf-css-shorthand-test-parse-border-value ()
  "Test parsing border shorthand values."
  (should (equal (etaf-css--parse-border-value "1px solid red")
                 '("1px" "solid" "red")))
  (should (equal (etaf-css--parse-border-value "2px dashed")
                 '("2px" "dashed" nil)))
  (should (equal (etaf-css--parse-border-value "solid blue")
                 '(nil "solid" "blue")))
  (should (equal (etaf-css--parse-border-value "3px")
                 '("3px" nil nil))))

(ert-deftest etaf-css-shorthand-test-parse-four-values ()
  "Test parsing four-value syntax."
  (should (equal (etaf-css--parse-four-values "10px")
                 '("10px" "10px" "10px" "10px")))
  (should (equal (etaf-css--parse-four-values "10px 20px")
                 '("10px" "20px" "10px" "20px")))
  (should (equal (etaf-css--parse-four-values "10px 20px 30px")
                 '("10px" "20px" "30px" "20px")))
  (should (equal (etaf-css--parse-four-values "10px 20px 30px 40px")
                 '("10px" "20px" "30px" "40px"))))

;;; Tests for expansion functions

(ert-deftest etaf-css-shorthand-test-expand-border ()
  "Test border property expansion."
  (let ((result (etaf-css--expand-border "1px solid red" nil)))
    (should (= (length result) 12))  ; 4 sides * 3 properties
    ;; Check that all properties are present
    (should (assq 'border-top-width result))
    (should (assq 'border-right-width result))
    (should (assq 'border-bottom-width result))
    (should (assq 'border-left-width result))
    (should (assq 'border-top-style result))
    (should (assq 'border-top-color result))))

(ert-deftest etaf-css-shorthand-test-expand-border-width ()
  "Test border-width property expansion."
  (let ((result (etaf-css--expand-border-width "1px 2px 3px 4px" nil)))
    (should (= (length result) 4))
    (should (equal (car (cdr (assq 'border-top-width result))) "1px"))
    (should (equal (car (cdr (assq 'border-right-width result))) "2px"))
    (should (equal (car (cdr (assq 'border-bottom-width result))) "3px"))
    (should (equal (car (cdr (assq 'border-left-width result))) "4px"))))

(ert-deftest etaf-css-shorthand-test-expand-margin ()
  "Test margin property expansion."
  (let ((result (etaf-css--expand-margin "10px 20px" nil)))
    (should (= (length result) 4))
    (should (equal (car (cdr (assq 'margin-top result))) "10px"))
    (should (equal (car (cdr (assq 'margin-right result))) "20px"))
    (should (equal (car (cdr (assq 'margin-bottom result))) "10px"))
    (should (equal (car (cdr (assq 'margin-left result))) "20px"))))

(ert-deftest etaf-css-shorthand-test-expand-padding ()
  "Test padding property expansion."
  (let ((result (etaf-css--expand-padding "5px" nil)))
    (should (= (length result) 4))
    (should (equal (car (cdr (assq 'padding-top result))) "5px"))
    (should (equal (car (cdr (assq 'padding-right result))) "5px"))
    (should (equal (car (cdr (assq 'padding-bottom result))) "5px"))
    (should (equal (car (cdr (assq 'padding-left result))) "5px"))))

;;; Tests for integration with parser

(ert-deftest etaf-css-shorthand-test-parse-with-border ()
  "Test CSS parsing with border shorthand."
  (let ((result (etaf-css-parse-declarations "border: 2px solid blue")))
    (should result)
    ;; Should have expanded to individual properties
    (should (assq 'border-top-width result))
    (should (assq 'border-top-style result))
    (should (assq 'border-top-color result))))

(ert-deftest etaf-css-shorthand-test-parse-with-margin ()
  "Test CSS parsing with margin shorthand."
  (let ((result (etaf-css-parse-declarations "margin: 10px 20px")))
    (should result)
    (should (assq 'margin-top result))
    (should (assq 'margin-right result))
    (should (assq 'margin-bottom result))
    (should (assq 'margin-left result))))

(ert-deftest etaf-css-shorthand-test-parse-mixed ()
  "Test CSS parsing with mixed shorthand and regular properties."
  (let ((result (etaf-css-parse-declarations "color: red; border: 1px solid green; padding-left: 5px")))
    (should result)
    (should (assq 'color result))
    (should (assq 'border-top-width result))
    (should (assq 'padding-left result))))

(provide 'etaf-css-shorthand-tests)
;;; etaf-css-shorthand-tests.el ends here
