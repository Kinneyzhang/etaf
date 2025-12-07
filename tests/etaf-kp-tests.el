;;; etaf-kp-tests.el --- Tests for etaf-kp integration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, typesetting, knuth-plass
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Tests for etaf-kp integration with ETAF framework.
;; The etaf-kp module implements the Knuth-Plass typesetting algorithm
;; for hybrid CJK and Latin text justification.
;;
;; Note: Full etaf-kp functionality requires interactive Emacs with proper
;; font/window context. These tests verify that the library loads correctly
;; and basic functions are available.

;;; Code:

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
