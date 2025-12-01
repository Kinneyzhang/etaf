;;; etaf-css-parse-tests.el --- Tests for etaf-css-parse.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for CSS value parsing functions in etaf-css-parse.el

;;; Code:

(require 'ert)
(require 'etaf-css-parse)

;;; Tests for etaf-css-parse-length

(ert-deftest etaf-css-parse-test-length-none ()
  "Test parsing 'none' value for length."
  (should (eq (etaf-css-parse-length "none" 100) 'none))
  (should (eq (etaf-css-parse-length 'none 100) 'none)))

(ert-deftest etaf-css-parse-test-length-auto ()
  "Test parsing 'auto' value for length."
  (should (eq (etaf-css-parse-length "auto" 100) 'auto))
  (should (eq (etaf-css-parse-length 'auto 100) 'auto))
  (should (eq (etaf-css-parse-length nil 100) 'auto)))

(ert-deftest etaf-css-parse-test-length-px ()
  "Test parsing pixel values for length."
  (should (= (etaf-css-parse-length "100px" 1000) 100))
  (should (= (etaf-css-parse-length "50px" 1000) 50))
  (should (= (etaf-css-parse-length "10px" 1000) 10))
  (should (= (etaf-css-parse-length "0" 1000) 0)))

(ert-deftest etaf-css-parse-test-length-cw ()
  "Test parsing cw (character-width) values for length.
The cw unit uses `frame-char-width' as the base value."
  (let ((char-width (frame-char-width)))
    ;; 1cw should equal frame-char-width
    (should (= (etaf-css-parse-length "1cw" 1000) char-width))
    ;; 10cw should equal 10 * frame-char-width
    (should (= (etaf-css-parse-length "10cw" 1000) (* 10 char-width)))
    ;; 2.5cw should equal 2.5 * frame-char-width
    (should (= (etaf-css-parse-length "2.5cw" 1000) (* 2.5 char-width)))))

(ert-deftest etaf-css-parse-test-length-percent ()
  "Test parsing percentage values for length."
  (should (= (etaf-css-parse-length "50%" 1000) 500.0))
  (should (= (etaf-css-parse-length "25%" 800) 200.0))
  (should (= (etaf-css-parse-length "100%" 500) 500.0)))

(ert-deftest etaf-css-parse-test-length-percent-nil-reference ()
  "Test parsing percentage values with nil reference width.
When reference-width is nil, percentage values should return auto"
  (should (eq (etaf-css-parse-length "50%" nil) 'auto))
  (should (eq (etaf-css-parse-length "100%" nil) 'auto)))

(ert-deftest etaf-css-parse-test-length-em ()
  "Test parsing em values for length."
  (should (= (etaf-css-parse-length "1em" 1000) 16))
  (should (= (etaf-css-parse-length "2em" 1000) 32)))

(ert-deftest etaf-css-parse-test-length-lh ()
  "Test parsing lh values for length."
  (should (= (etaf-css-parse-length "1lh" 1000) 1))
  (should (= (etaf-css-parse-length "5lh" 1000) 5)))

(ert-deftest etaf-css-parse-test-length-numeric ()
  "Test parsing numeric values for length."
  (should (= (etaf-css-parse-length 100 1000) 100))
  (should (= (etaf-css-parse-length 50 1000) 50)))

;;; Tests for etaf-css-parse-height

(ert-deftest etaf-css-parse-test-height-none ()
  "Test parsing 'none' value for height."
  (should (eq (etaf-css-parse-height "none" 100) 'none))
  (should (eq (etaf-css-parse-height 'none 100) 'none)))

(ert-deftest etaf-css-parse-test-height-auto ()
  "Test parsing 'auto' value for height."
  (should (eq (etaf-css-parse-height "auto" 100) 'auto))
  (should (eq (etaf-css-parse-height 'auto 100) 'auto))
  (should (eq (etaf-css-parse-height nil 100) 'auto)))

(ert-deftest etaf-css-parse-test-height-lh ()
  "Test parsing lh values for height."
  (should (= (etaf-css-parse-height "5lh" 100) 5))
  (should (= (etaf-css-parse-height "10lh" 100) 10)))

(ert-deftest etaf-css-parse-test-height-number ()
  "Test parsing numeric values for height."
  (should (= (etaf-css-parse-height 5 100) 5))
  (should (= (etaf-css-parse-height "5" 100) 5)))

(ert-deftest etaf-css-parse-test-height-percent ()
  "Test parsing percentage values for height."
  (should (= (etaf-css-parse-height "50%" 100) 50.0))
  (should (= (etaf-css-parse-height "100%" 100) 100.0)))

(ert-deftest etaf-css-parse-test-height-percent-nil-reference ()
  "Test parsing percentage values with nil reference height.
When reference-height is nil, percentage values should return auto"
  (should (eq (etaf-css-parse-height "50%" nil) 'auto))
  (should (eq (etaf-css-parse-height "100%" nil) 'auto)))

(ert-deftest etaf-css-parse-test-height-px ()
  "Test parsing px values for height (converted to line numbers)."
  ;; px values are converted to line numbers using pixels-per-line (default 20)
  (should (= (etaf-css-parse-height "20px" 100) 1))
  (should (= (etaf-css-parse-height "40px" 100) 2))
  (should (= (etaf-css-parse-height "50px" 100)
             (ceiling (/ 50.0 etaf-css-parse-pixels-per-line)))))

(ert-deftest etaf-css-parse-test-height-em ()
  "Test parsing em values for height."
  (should (= (etaf-css-parse-height "1em" 100) 1))
  (should (= (etaf-css-parse-height "2em" 100) 2)))

;;; Tests for etaf-css-parse-style-value

(ert-deftest etaf-css-parse-test-style-value ()
  "Test extracting values from computed style alist."
  (let ((style '((color . "red")
                 (font-size . "14px")
                 (display . "flex"))))
    (should (equal (etaf-css-parse-style-value style 'color) "red"))
    (should (equal (etaf-css-parse-style-value style 'font-size) "14px"))
    (should (equal (etaf-css-parse-style-value style 'display) "flex"))
    (should (eq (etaf-css-parse-style-value style 'unknown) nil))
    (should (equal (etaf-css-parse-style-value style 'unknown "default") "default"))))

;;; Tests for etaf-css-parse-flex-number

(ert-deftest etaf-css-parse-test-flex-number ()
  "Test parsing flex number values."
  (should (equal (etaf-css-parse-flex-number "1") 1))
  (should (equal (etaf-css-parse-flex-number "0") 0))
  (should (equal (etaf-css-parse-flex-number "1.5") 1.5))
  (should (equal (etaf-css-parse-flex-number "-1") -1))
  (should (equal (etaf-css-parse-flex-number 5) 5))
  (should (equal (etaf-css-parse-flex-number "auto") nil))
  (should (equal (etaf-css-parse-flex-number "invalid") nil)))

;;; Tests for etaf-css-parse-pixels-per-line constant

(ert-deftest etaf-css-parse-test-pixels-per-line-constant ()
  "Test that pixels-per-line constant is defined."
  (should (boundp 'etaf-css-parse-pixels-per-line))
  (should (numberp etaf-css-parse-pixels-per-line))
  (should (= etaf-css-parse-pixels-per-line 20)))

;;; Backward compatibility tests

(ert-deftest etaf-css-parse-test-backward-compatibility ()
  "Test that etaf-layout-parse aliases work correctly."
  (require 'etaf-layout-parse)
  ;; Test that aliases are defined
  (should (fboundp 'etaf-layout-parse-length))
  (should (fboundp 'etaf-layout-parse-height))
  (should (fboundp 'etaf-layout-parse-style-value))
  (should (fboundp 'etaf-layout-parse-flex-number))
  ;; Test that aliases work the same as new functions
  (should (equal (etaf-layout-parse-length "100px" 1000)
                 (etaf-css-parse-length "100px" 1000)))
  (should (equal (etaf-layout-parse-height "5lh" 100)
                 (etaf-css-parse-height "5lh" 100)))
  (should (equal (etaf-layout-parse-flex-number "1")
                 (etaf-css-parse-flex-number "1"))))

(provide 'etaf-css-parse-tests)
;;; etaf-css-parse-tests.el ends here
