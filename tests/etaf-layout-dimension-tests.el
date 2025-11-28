;;; etaf-layout-dimension-tests.el --- Tests for CSS dimension properties -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for CSS dimension properties: min-width, max-width, height, min-height, max-height

;;; Code:

(require 'ert)
(require 'etaf-layout)

;;; Tests for etaf-layout-parse-length

(ert-deftest etaf-layout-test-parse-length-none ()
  "Test parsing 'none' value for length."
  (should (eq (etaf-layout-parse-length "none" 100) 'none))
  (should (eq (etaf-layout-parse-length 'none 100) 'none)))

(ert-deftest etaf-layout-test-parse-length-auto ()
  "Test parsing 'auto' value for length."
  (should (eq (etaf-layout-parse-length "auto" 100) 'auto))
  (should (eq (etaf-layout-parse-length 'auto 100) 'auto))
  (should (eq (etaf-layout-parse-length nil 100) 'auto)))

(ert-deftest etaf-layout-test-parse-length-px ()
  "Test parsing pixel values for length."
  (should (= (etaf-layout-parse-length "100px" 1000) 100))
  (should (= (etaf-layout-parse-length "50px" 1000) 50))
  (should (= (etaf-layout-parse-length "0" 1000) 0)))

(ert-deftest etaf-layout-test-parse-length-percent ()
  "Test parsing percentage values for length."
  (should (= (etaf-layout-parse-length "50%" 1000) 500.0))
  (should (= (etaf-layout-parse-length "100%" 800) 800.0)))

;;; Tests for etaf-layout-parse-height

(ert-deftest etaf-layout-test-parse-height-none ()
  "Test parsing 'none' value for height."
  (should (eq (etaf-layout-parse-height "none" 100) 'none))
  (should (eq (etaf-layout-parse-height 'none 100) 'none)))

(ert-deftest etaf-layout-test-parse-height-auto ()
  "Test parsing 'auto' value for height."
  (should (eq (etaf-layout-parse-height "auto" 100) 'auto))
  (should (eq (etaf-layout-parse-height 'auto 100) 'auto))
  (should (eq (etaf-layout-parse-height nil 100) 'auto)))

(ert-deftest etaf-layout-test-parse-height-lh ()
  "Test parsing lh values for height."
  (should (= (etaf-layout-parse-height "5lh" 100) 5))
  (should (= (etaf-layout-parse-height "10lh" 100) 10)))

(ert-deftest etaf-layout-test-parse-height-number ()
  "Test parsing numeric values for height."
  (should (= (etaf-layout-parse-height 5 100) 5))
  (should (= (etaf-layout-parse-height "5" 100) 5)))

(provide 'etaf-layout-dimension-tests)
;;; etaf-layout-dimension-tests.el ends here
