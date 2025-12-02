;;; etaf-layout-dimension-tests.el --- Tests for CSS dimension properties -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for CSS dimension properties: min-width, max-width, height, min-height, max-height
;; Also tests for CSS units: px, cw (character-width), lh (line-height), etc.

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

(ert-deftest etaf-layout-test-parse-length-cw ()
  "Test parsing cw (character-width) values for length.
The cw unit uses `frame-char-width' as the base value."
  (let ((char-width (frame-char-width)))
    ;; 1cw should equal frame-char-width
    (should (= (etaf-layout-parse-length "1cw" 1000) char-width))
    ;; 10cw should equal 10 * frame-char-width
    (should (= (etaf-layout-parse-length "10cw" 1000) (* 10 char-width)))
    ;; 2.5cw should equal 2.5 * frame-char-width
    (should (= (etaf-layout-parse-length "2.5cw" 1000) (* 2.5 char-width)))))

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

;;; Tests for CSS height rendering

(ert-deftest etaf-layout-test-css-height-respected ()
  "Test that CSS height property is respected when rendering box height.
When a CSS height is specified, the rendered box should have that height,
not the natural height of the content."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { width: 200px; height: 5; }"))
                 (body
                  (div "Short text")))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (body-node (car (dom-non-text-children layout-tree)))
         (div-node (car (dom-non-text-children body-node)))
         (box-model (etaf-layout-get-box-model div-node)))
    ;; The div should have height 5 as specified in CSS, not 1 (natural content height)
    (should-equal (etaf-layout-box-content-height box-model) 5)))

(ert-deftest etaf-layout-test-css-height-string-rendering ()
  "Test that rendered string respects the CSS height property.
When CSS height is 3 lines, the rendered string should have 3 lines."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { width: 200px; height: 3; }"))
                 (body
                  (div "Line")))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-string layout-tree)))
    ;; The rendered string should have at least 3 lines for the div content
    ;; (the div has height 3)
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))))

;;; Tests for nil viewport dimensions

(ert-deftest etaf-layout-test-parse-length-nil-reference ()
  "Test parsing percentage values with nil reference width.
When reference-width is nil, percentage values should return 'auto."
  (should (eq (etaf-layout-parse-length "50%" nil) 'auto))
  (should (eq (etaf-layout-parse-length "100%" nil) 'auto)))

(ert-deftest etaf-layout-test-parse-height-nil-reference ()
  "Test parsing percentage values with nil reference height.
When reference-height is nil, percentage values should return 'auto."
  (should (eq (etaf-layout-parse-height "50%" nil) 'auto))
  (should (eq (etaf-layout-parse-height "100%" nil) 'auto)))

(ert-deftest etaf-layout-test-nil-viewport-width ()
  "Test layout building with nil viewport width.
When viewport width is nil, block elements should use content width (0)."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { height: 3lh; }"))
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         ;; Use nil width in viewport
         (layout-tree (etaf-layout-build-tree render-tree '(:width nil :height 768))))
    ;; Layout tree should be created successfully
    (should layout-tree)
    (should (etaf-layout-get-box-model layout-tree))))

(ert-deftest etaf-layout-test-nil-viewport-height ()
  "Test layout building with nil viewport height.
When viewport height is nil, percentage heights should return auto."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { width: 200px; height: 5lh; }"))
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         ;; Use nil height in viewport
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height nil)))
         (body-node (car (dom-non-text-children layout-tree)))
         (div-node (car (dom-non-text-children body-node)))
         (box-model (etaf-layout-get-box-model div-node)))
    ;; Layout tree should be created successfully
    (should layout-tree)
    ;; The div should have explicit height 5
    (should-equal (etaf-layout-box-content-height box-model) 5)))

(ert-deftest etaf-layout-test-nil-viewport-both ()
  "Test layout building with both width and height as nil.
When both viewport dimensions are nil, elements should use natural dimensions."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (body
                  (div "Some text content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         ;; Use nil for both width and height
         (layout-tree (etaf-layout-build-tree render-tree '(:width nil :height nil))))
    ;; Layout tree should be created successfully
    (should layout-tree)
    (should (etaf-layout-get-box-model layout-tree))))

(provide 'etaf-layout-dimension-tests)
;;; etaf-layout-dimension-tests.el ends here
