;;; etaf-inline-block-tests.el --- Tests for inline-block display fix -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests to verify that inline-block elements display horizontally like inline elements
;; but can have width, height, and other block-level properties.

;;; Code:

(require 'ert)
(require 'etaf-layout)
(require 'etaf-ecss)
(require 'etaf-render)
(require 'etaf-css)
(require 'etaf-etml)

;;; Helper Functions

(defmacro should-equal (actual expected)
  "Assert that ACTUAL equals EXPECTED."
  `(should (equal ,actual ,expected)))

;;; Inline-Block Display Tests

(ert-deftest etaf-inline-block-test-basic-layout ()
  "Test that inline-block elements are laid out horizontally."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "button { display: inline-block; }"))
                 (body
                  (div
                   (button "Button 1")
                   (button "Button 2")
                   (button "Button 3"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    
    ;; Check that buttons have inline-block display
    (let ((body-node (car (dom-by-tag layout-tree 'body))))
      (should body-node)
      (let ((div-node (car (dom-children body-node))))
        (should div-node)
        (let ((buttons (seq-filter (lambda (child)
                                     (and (consp child)
                                          (eq (dom-tag child) 'button)))
                                   (dom-children div-node))))
          (should (= (length buttons) 3))
          (dolist (button buttons)
            (should-equal (etaf-render-get-display button) "inline-block")))))))

(ert-deftest etaf-inline-block-test-ua-stylesheet ()
  "Test that buttons have inline-block display by default from UA stylesheet."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (body
                  (button "Default Button")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom)))
    
    (let ((body-node (car (dom-by-tag render-tree 'body))))
      (should body-node)
      (let ((button-node (car (dom-children body-node))))
        (should button-node)
        (should-equal (dom-tag button-node) 'button)
        (should-equal (etaf-render-get-display button-node) "inline-block")))))

(ert-deftest etaf-inline-block-test-with-dimensions ()
  "Test that inline-block elements can have width and height."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .btn { 
                      display: inline-block; 
                      width: 100px; 
                      height: 2em; 
                      padding: 5px; 
                      margin: 3px; 
                    }"))
                 (body
                  (div
                   (button :class "btn" "Button 1")
                   (button :class "btn" "Button 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    
    ;; Check that buttons have the specified dimensions
    (let ((body-node (car (dom-by-tag layout-tree 'body))))
      (should body-node)
      (let ((div-node (car (dom-children body-node))))
        (should div-node)
        (let ((button (car (seq-filter (lambda (child)
                                        (and (consp child)
                                             (eq (dom-tag child) 'button)))
                                      (dom-children div-node)))))
          (should button)
          (let ((box-model (etaf-layout-get-box-model button)))
            (should box-model)
            (should-equal (etaf-layout-box-content-width box-model) 100)
            ;; Height should be set (2em = 32px in default font size)
            (should (> (etaf-layout-box-content-height box-model) 0))
            (should-equal (plist-get (plist-get box-model :padding) :top) 5)
            (should-equal (plist-get (plist-get box-model :padding) :left) 5)
            (should-equal (plist-get (plist-get box-model :margin) :top) 3)))))))

(ert-deftest etaf-inline-block-test-mixed-with-inline ()
  "Test that inline-block elements can be mixed with inline elements."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    button { display: inline-block; }
                    span { display: inline; }"))
                 (body
                  (div
                   (span "Text before ")
                   (button "Button")
                   (span " text after"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    
    ;; All elements should be in the same div
    (let ((body-node (car (dom-by-tag layout-tree 'body))))
      (should body-node)
      (let ((div-node (car (dom-children body-node))))
        (should div-node)
        (let ((children (dom-children div-node)))
          ;; Should have 3 children: span, button, span
          (should (= (length children) 3)))))))

(ert-deftest etaf-inline-block-test-vs-block ()
  "Test that inline-block elements don't break to new lines like block elements."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "
                    .inline-block-btn { display: inline-block; }
                    .block-btn { display: block; }"))
                 (body
                  (div :class "container-inline"
                   (button :class "inline-block-btn" "IB1")
                   (button :class "inline-block-btn" "IB2"))
                  (div :class "container-block"
                   (button :class "block-btn" "B1")
                   (button :class "block-btn" "B2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    
    (let* ((body-node (car (dom-by-tag layout-tree 'body)))
           (divs (seq-filter (lambda (child)
                              (and (consp child)
                                   (eq (dom-tag child) 'div)))
                            (dom-children body-node)))
           (inline-container (car divs))
           (block-container (cadr divs)))
      
      (should inline-container)
      (should block-container)
      
      ;; Verify display types
      (let ((inline-buttons (seq-filter (lambda (child)
                                         (and (consp child)
                                              (eq (dom-tag child) 'button)))
                                       (dom-children inline-container)))
            (block-buttons (seq-filter (lambda (child)
                                        (and (consp child)
                                             (eq (dom-tag child) 'button)))
                                      (dom-children block-container))))
        (should (= (length inline-buttons) 2))
        (should (= (length block-buttons) 2))
        
        ;; Check display types
        (dolist (btn inline-buttons)
          (should-equal (etaf-render-get-display btn) "inline-block"))
        (dolist (btn block-buttons)
          (should-equal (etaf-render-get-display btn) "block"))))))

(ert-deftest etaf-inline-block-test-custom-tag ()
  "Test that custom tags with inline-block display work correctly."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "badge { display: inline-block; padding: 2px; }"))
                 (body
                  (div
                   (badge "Badge 1")
                   (badge "Badge 2")
                   (badge "Badge 3"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    
    ;; Check that badges have inline-block display
    (let ((body-node (car (dom-by-tag layout-tree 'body))))
      (should body-node)
      (let ((div-node (car (dom-children body-node))))
        (should div-node)
        (let ((badges (seq-filter (lambda (child)
                                   (and (consp child)
                                        (eq (dom-tag child) 'badge)))
                                 (dom-children div-node))))
          (should (= (length badges) 3))
          (dolist (badge badges)
            (should-equal (etaf-render-get-display badge) "inline-block")))))))

(provide 'etaf-inline-block-tests)
;;; etaf-inline-block-tests.el ends here
