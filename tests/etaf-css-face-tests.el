;;; etaf-css-face-tests.el --- Tests for CSS to face mapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for CSS to Emacs face property mapping

;;; Code:

(require 'ert)
(require 'etaf-css-face)

;;; Tests for color conversion

(ert-deftest etaf-css-face-test-color-hex ()
  "Test hex color conversion."
  (should (equal (etaf-css-color-to-emacs "#ff0000") "#ff0000"))
  (should (equal (etaf-css-color-to-emacs "#fff") "#fff"))
  (should (equal (etaf-css-color-to-emacs "#aabbcc") "#aabbcc")))

(ert-deftest etaf-css-face-test-color-rgb ()
  "Test rgb() color conversion."
  (should (equal (etaf-css-color-to-emacs "rgb(255, 0, 0)") "#ff0000"))
  (should (equal (etaf-css-color-to-emacs "rgb(0, 255, 0)") "#00ff00"))
  (should (equal (etaf-css-color-to-emacs "rgb(0, 0, 255)") "#0000ff")))

(ert-deftest etaf-css-face-test-color-named ()
  "Test named color conversion."
  (should (equal (etaf-css-color-to-emacs "red") "red"))
  (should (equal (etaf-css-color-to-emacs "blue") "blue"))
  (should (equal (etaf-css-color-to-emacs "green") "green")))

;;; Tests for font-weight conversion

(ert-deftest etaf-css-face-test-font-weight ()
  "Test font-weight conversion."
  (should (eq (etaf-css-font-weight-to-emacs "bold") 'bold))
  (should (eq (etaf-css-font-weight-to-emacs "700") 'bold))
  (should (eq (etaf-css-font-weight-to-emacs "normal") 'normal))
  (should (eq (etaf-css-font-weight-to-emacs "lighter") 'light)))

;;; Tests for font-style conversion

(ert-deftest etaf-css-face-test-font-style ()
  "Test font-style conversion."
  (should (eq (etaf-css-font-style-to-emacs "italic") 'italic))
  (should (eq (etaf-css-font-style-to-emacs "oblique") 'oblique))
  (should (eq (etaf-css-font-style-to-emacs "normal") 'normal)))

;;; Tests for font-size conversion

(ert-deftest etaf-css-face-test-font-size ()
  "Test font-size conversion to :height."
  ;; Numeric values
  (should (= (etaf-css-font-size-to-emacs 1.4) 1.4))
  (should (= (etaf-css-font-size-to-emacs 1.0) 1.0))
  ;; Numeric strings (unitless) - important for etaf-etml-tag.el compatibility
  (should (= (etaf-css-font-size-to-emacs "1.4") 1.4))
  (should (= (etaf-css-font-size-to-emacs "1.6") 1.6))
  (should (= (etaf-css-font-size-to-emacs "1") 1.0))
  (should (= (etaf-css-font-size-to-emacs "2.0") 2.0))
  ;; px values
  (should (= (etaf-css-font-size-to-emacs "16px") 1.0))
  (should (= (etaf-css-font-size-to-emacs "32px") 2.0))
  ;; em values
  (should (= (etaf-css-font-size-to-emacs "1.5em") 1.5))
  ;; CSS keywords
  (should (= (etaf-css-font-size-to-emacs "medium") 1.0))
  (should (= (etaf-css-font-size-to-emacs "larger") 1.2)))

(ert-deftest etaf-css-face-test-font-size-in-style-to-face ()
  "Test font-size is correctly converted to :height in style-to-face."
  ;; Numeric font-size
  (let ((face (etaf-css-style-to-face '((font-size . 1.4)))))
    (should (= (plist-get face :height) 1.4)))
  ;; String numeric font-size (unitless)
  (let ((face (etaf-css-style-to-face '((font-size . "1.6")))))
    (should (= (plist-get face :height) 1.6)))
  ;; px font-size (20px / 16px baseline = 1.25)
  (let ((face (etaf-css-style-to-face '((font-size . "20px")))))
    (should (= (plist-get face :height) 1.25))))

;;; Tests for text-decoration conversion

(ert-deftest etaf-css-face-test-text-decoration ()
  "Test text-decoration conversion."
  (should (plist-get (etaf-css-text-decoration-to-emacs "underline") :underline))
  (should (plist-get (etaf-css-text-decoration-to-emacs "line-through") :strike-through))
  (should (plist-get (etaf-css-text-decoration-to-emacs "overline") :overline)))

;;; Tests for style-to-face conversion

(ert-deftest etaf-css-face-test-style-to-face ()
  "Test complete style-to-face conversion."
  (let ((face (etaf-css-style-to-face '((color . "red") (font-weight . "bold")))))
    (should (equal (plist-get face :foreground) "red"))
    (should (eq (plist-get face :weight) 'bold))))

(ert-deftest etaf-css-face-test-apply-face ()
  "Test applying face to string."
  (let ((result (etaf-css-apply-face-to-string "hello" '((color . "red")))))
    (should (stringp result))
    (should (equal (length result) 5))
    ;; Check that face property is set
    (should (get-text-property 0 'face result))))

(provide 'etaf-css-face-tests)
;;; etaf-css-face-tests.el ends here
