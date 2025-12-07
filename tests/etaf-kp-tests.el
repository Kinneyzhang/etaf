;;; etaf-kp-tests.el --- Tests for etaf-kp integration -*- lexical-binding: t; -*-

;; Test basic etaf-kp integration with ETAF
;; Note: Full etaf-kp functionality requires interactive Emacs with proper font/window context
;; These tests verify that the library loads and basic functions are available

(require 'etaf-kp)
(require 'etaf-kp-utils)
(require 'etaf-kp-hyphen)
(require 'etaf-pixel)
(require 'ert)

(ert-deftest etaf-kp-library-loaded ()
  "Test that etaf-kp library loads correctly."
  (should (fboundp 'etaf-kp-pixel-justify))
  (should (fboundp 'etaf-kp-pixel-range-justify)))

(ert-deftest etaf-kp-parameters-settable ()
  "Test that etaf-kp parameters can be set."
  (should (fboundp 'etaf-kp-param-set))
  (etaf-kp-param-set 7 3 2 5 2 1 0 2 0)
  (should (numberp etaf-kp-lws-ideal-pixel))
  (should (= etaf-kp-lws-ideal-pixel 7)))

(ert-deftest etaf-pixel-functions-available ()
  "Test that etaf-pixel functions are available."
  (should (fboundp 'etaf-pixel-wrap))
  (should (fboundp 'etaf-pixel-spacing))
  (should (fboundp 'etaf-pixel-align)))

(ert-deftest etaf-kp-hyphen-available ()
  "Test that hyphen functions are available."
  (should (fboundp 'etaf-kp-hyphen-load-languages)))

(ert-deftest etaf-kp-dictionaries-exist ()
  "Test that dictionaries directory exists."
  (let ((dict-dir (expand-file-name "./dictionaries" (etaf-kp-root-dir))))
    (should (file-directory-p dict-dir))))

(ert-deftest etaf-kp-latin-lang-set ()
  "Verify etaf-kp-latin-lang is set."
  (should (stringp etaf-kp-latin-lang))
  (should (string= etaf-kp-latin-lang "en_US")))

(provide 'etaf-kp-tests)
;;; etaf-kp-tests.el ends here
