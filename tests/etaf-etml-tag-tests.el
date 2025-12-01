(require 'etaf-ert)
(require 'etaf-etml-tag)
(require 'etaf-ua-stylesheet)
(require 'etaf-css)
(require 'etaf-dom)

(setq-local lisp-indent-offset 2)

;;; Test tag registration

;; Test that built-in tags are defined
(should-equal (etaf-etml-tag-defined-p 'div) t)
(should-equal (etaf-etml-tag-defined-p 'span) t)
(should-equal (etaf-etml-tag-defined-p 'button) t)
(should-equal (etaf-etml-tag-defined-p 'input) t)
(should-equal (etaf-etml-tag-defined-p 'a) t)
(should-equal (etaf-etml-tag-defined-p 'p) t)
(should-equal (etaf-etml-tag-defined-p 'h1) t)

;; Test undefined tag
(should-equal (etaf-etml-tag-defined-p 'my-undefined-tag) nil)

;;; Test tag definitions

;; Test div tag definition
(let ((div-def (etaf-etml-tag-get-definition 'div)))
  (should-equal (plist-get div-def :name) 'div)
  (should-equal (plist-get div-def :display) 'block)
  (should-equal (plist-get div-def :children-allowed) t))

;; Test button tag definition
(let ((button-def (etaf-etml-tag-get-definition 'button)))
  (should-equal (plist-get button-def :name) 'button)
  (should-equal (plist-get button-def :display) 'inline-block)
  ;; Default styles are now in UA stylesheet, not tag definition
  (should-equal (plist-get button-def :default-style) nil))

;; Test self-closing tags
(let ((input-def (etaf-etml-tag-get-definition 'input)))
  (should-equal (plist-get input-def :self-closing) t)
  (should-equal (plist-get input-def :children-allowed) nil))

(let ((br-def (etaf-etml-tag-get-definition 'br)))
  (should-equal (plist-get br-def :self-closing) t)
  (should-equal (plist-get br-def :children-allowed) nil))

(let ((hr-def (etaf-etml-tag-get-definition 'hr)))
  (should-equal (plist-get hr-def :self-closing) t)
  (should-equal (plist-get hr-def :children-allowed) nil))

;;; Test custom tag definition
;; Custom tags can still use :default-style if needed for custom behavior
;; In this test, custom-button inherits from button and adds custom hover behavior
;; Default styles (like padding, border) now come from UA stylesheet
(define-etaf-etml-tag custom-button
  :display 'inline-block
  :inherit 'button
  :hover-style '((background-color . "darkblue")))

(should-equal (etaf-etml-tag-defined-p 'custom-button) t)

(let ((custom-def (etaf-etml-tag-get-definition 'custom-button)))
  (should-equal (plist-get custom-def :display) 'inline-block)
  (should-equal (plist-get custom-def :inherit) 'button))

;;; Test style merging

;; Test that merged style contains all expected properties (order-independent)
(let ((merged (etaf-etml-tag--merge-styles '((color . "red") (font-size . "12px"))
                                      '((color . "blue") (padding . "10px")))))
  ;; color should be overridden to blue
  (should-equal (cdr (assq 'color merged)) "blue")
  ;; font-size should be preserved from base
  (should-equal (cdr (assq 'font-size merged)) "12px")
  ;; padding should be added from override
  (should-equal (cdr (assq 'padding merged)) "10px")
  ;; Should have exactly 3 properties
  (should-equal (length merged) 3))

(should-equal
 (cdr (assq 'color (etaf-etml-tag--merge-styles nil '((color . "blue")))))
 "blue")

(should-equal
 (cdr (assq 'color (etaf-etml-tag--merge-styles '((color . "red")) nil)))
 "red")

;;; Test style string parsing

(should-equal
 (etaf-etml-tag--parse-style-string "color: red; font-size: 12px")
 '((color . "red") (font-size . "12px")))

(should-equal
 (etaf-etml-tag--parse-style-string "  padding : 10px 20px  ;  margin: 5px  ")
 '((padding . "10px 20px") (margin . "5px")))

;;; Test style alist to string conversion

(should-equal
 (etaf-etml-tag--style-alist-to-string '((color . "red") (font-size . "12px")))
 "color: red; font-size: 12px")

;;; Test tag instance creation

(let ((instance (etaf-etml-tag-create-instance 'div '(:class "container") '("Hello"))))
  (should-equal (plist-get instance :tag-name) 'div)
  (should-equal (plist-get instance :attrs) '(:class "container"))
  (should-equal (plist-get instance :children) '("Hello")))

;;; Test tag parsing

(let ((parsed (etaf-etml-tag-parse '(div :class "container" "Hello"))))
  (should-equal (plist-get parsed :tag-name) 'div)
  (should-equal (plist-get parsed :attrs) '(:class "container"))
  (should-equal (plist-get parsed :children) '("Hello")))

;; Test nested tag parsing
(let* ((parsed (etaf-etml-tag-parse '(div :id "outer"
                                     (span :class "inner" "Text"))))
       (children (plist-get parsed :children))
       (child (car children)))
  (should-equal (plist-get parsed :tag-name) 'div)
  (should-equal (plist-get child :tag-name) 'span)
  (should-equal (plist-get child :attrs) '(:class "inner")))

;;; Test tag rendering to DOM

(let* ((instance (etaf-etml-tag-create-instance 'div '(:class "container") '("Hello")))
       (dom (etaf-etml-tag-render-to-dom instance)))
  (should-equal (car dom) 'div)
  (should-equal (assq 'class (cadr dom)) '(class . "container")))

;; Test rendering button tag
;; Note: Default styles now come from UA stylesheet via CSS system
;; When using etaf-etml-tag-render-to-dom directly, no default styles are added
;; Styles are applied when the DOM goes through the CSS system
(let* ((instance (etaf-etml-tag-create-instance 'button nil '("Click")))
       (dom (etaf-etml-tag-render-to-dom instance)))
  (should-equal (car dom) 'button))

;; Test rendering p tag (which has no default style)
(let* ((instance (etaf-etml-tag-create-instance 'p nil '("Paragraph")))
       (dom (etaf-etml-tag-render-to-dom instance)))
  (should-equal (car dom) 'p))

;;; Test etaf-etml-tag-to-dom

(should-equal
 (car (etaf-etml-tag-to-dom '(div "Hello")))
 'div)

(should-equal
 (car (etaf-etml-tag-to-dom '(button :type "submit" "Click")))
 'button)

;;; Test tag list

(let ((all-tags (etaf-etml-tag-list-all)))
  (should (member 'div all-tags))
  (should (member 'span all-tags))
  (should (member 'button all-tags))
  (should (member 'custom-button all-tags)))

;;; Test event creation

(let ((event (etaf-etml-tag--make-event 'click '(:tag-name test) '(:x 100 :y 200))))
  (should-equal (plist-get event :type) 'click)
  (should-equal (plist-get event :target) '(:tag-name test))
  (should-equal (plist-get event :x) 100)
  (should-equal (plist-get event :y) 200)
  (should (plist-get event :timestamp)))

;; Test event text property from button children
(let* ((instance (etaf-etml-tag-create-instance 'button nil '("Click Me")))
       (event (etaf-etml-tag--make-event 'click instance nil)))
  (should-equal (plist-get event :text) "Click Me"))

;; Test event text property with multiple children
(let* ((instance (etaf-etml-tag-create-instance 'button nil '("Hello" " " "World")))
       (event (etaf-etml-tag--make-event 'click instance nil)))
  (should-equal (plist-get event :text) "Hello World"))

;;; Test display types for different tags

;; Block elements
(should-equal (plist-get (etaf-etml-tag-get-definition 'div) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'p) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'h1) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'ul) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'ol) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'blockquote) :display) 'block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'pre) :display) 'block)

;; Inline elements
(should-equal (plist-get (etaf-etml-tag-get-definition 'span) :display) 'inline)
(should-equal (plist-get (etaf-etml-tag-get-definition 'a) :display) 'inline)
(should-equal (plist-get (etaf-etml-tag-get-definition 'em) :display) 'inline)
(should-equal (plist-get (etaf-etml-tag-get-definition 'strong) :display) 'inline)
(should-equal (plist-get (etaf-etml-tag-get-definition 'code) :display) 'inline)

;; Inline-block elements
(should-equal (plist-get (etaf-etml-tag-get-definition 'button) :display) 'inline-block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'input) :display) 'inline-block)
(should-equal (plist-get (etaf-etml-tag-get-definition 'textarea) :display) 'inline-block)

;; Table elements
(should-equal (plist-get (etaf-etml-tag-get-definition 'table) :display) 'table)
(should-equal (plist-get (etaf-etml-tag-get-definition 'tr) :display) 'table-row)
(should-equal (plist-get (etaf-etml-tag-get-definition 'td) :display) 'table-cell)
(should-equal (plist-get (etaf-etml-tag-get-definition 'th) :display) 'table-cell)

;;; Test interactive tags have click handlers

(let ((a-def (etaf-etml-tag-get-definition 'a)))
  (should (functionp (plist-get a-def :on-click))))

(let ((button-def (etaf-etml-tag-get-definition 'button)))
  (should (functionp (plist-get button-def :on-click))))

(let ((summary-def (etaf-etml-tag-get-definition 'summary)))
  (should (functionp (plist-get summary-def :on-click))))

;;; Test state-based styles

(let ((button-def (etaf-etml-tag-get-definition 'button)))
  (should (plist-get button-def :hover-style))
  (should (plist-get button-def :active-style))
  (should (plist-get button-def :disabled-style)))

(let ((input-def (etaf-etml-tag-get-definition 'input)))
  (should (plist-get input-def :focus-style))
  (should (plist-get input-def :disabled-style)))

(let ((a-def (etaf-etml-tag-get-definition 'a)))
  (should (plist-get a-def :hover-style)))

;;; Test computed style with state
;; Note: Default styles are now provided by UA stylesheet via CSS pipeline
;; etaf-etml-tag-get-computed-style only handles inline and state-based styles

(let* ((instance (etaf-etml-tag-create-instance 'button nil '("Click")))
       (base-style (etaf-etml-tag-get-computed-style instance)))
  ;; Simulate hover state - should add hover-style
  (plist-put (plist-get instance :state) :hovered t)
  (let ((hover-style (etaf-etml-tag-get-computed-style instance)))
    ;; Should have hover background color
    (should (assq 'background-color hover-style))))

;;; Test UA stylesheet integration
;; These tests verify that default styles are now in the UA stylesheet
;; and not in tag definitions

;; Test that tag definitions no longer contain default-style
(let ((ul-def (etaf-etml-tag-get-definition 'ul)))
  (should-equal (plist-get ul-def :default-style) nil))

(let ((blockquote-def (etaf-etml-tag-get-definition 'blockquote)))
  (should-equal (plist-get blockquote-def :default-style) nil))

(let ((button-def (etaf-etml-tag-get-definition 'button)))
  (should-equal (plist-get button-def :default-style) nil))

;;; Test event handling keymap setup

;; Test keymap creation for clickable elements
(let* ((instance (etaf-etml-tag-create-instance 'button nil '("Click")))
       (keymap (etaf-etml-tag-setup-keymap instance)))
  (should (keymapp keymap))
  ;; Should have RET binding
  (should (lookup-key keymap (kbd "RET")))
  ;; Should have SPC binding
  (should (lookup-key keymap (kbd "SPC")))
  ;; Should have mouse-1 binding
  (should (lookup-key keymap [mouse-1]))
  ;; Should have double-mouse-1 binding
  (should (lookup-key keymap [double-mouse-1])))

;; Test keymap for non-clickable elements
(let* ((instance (etaf-etml-tag-create-instance 'div nil '("Text")))
       (keymap (etaf-etml-tag-setup-keymap instance)))
  (should (keymapp keymap))
  ;; Should NOT have RET binding (no on-click)
  (should-not (lookup-key keymap (kbd "RET")))
  ;; Should NOT have mouse-1 binding
  (should-not (lookup-key keymap [mouse-1])))

;;; Test etaf-etml-tag-make-interactive adds proper text properties

(with-temp-buffer
  (insert "Click Me")
  (let ((instance (etaf-etml-tag-create-instance 'button nil '("Click Me"))))
    (etaf-etml-tag-make-interactive 1 9 instance)
    (goto-char 1)
    ;; Check text properties were added
    (should (get-text-property 1 'etaf-tag-instance))
    (should (get-text-property 1 'keymap))
    (should (get-text-property 1 'mouse-face))
    (should (get-text-property 1 'pointer))
    (should (get-text-property 1 'help-echo))
    ;; Verify keymap has bindings
    (let ((keymap (get-text-property 1 'keymap)))
      (should (lookup-key keymap (kbd "RET"))))))

;;; Test hover tracking variables are defined in etaf-etml-tag.el

;; The variable should be defined after loading etaf-etml-tag.el (buffer-local)
(should (boundp 'etaf-etml-tag--current-hover-instance))
;; Initial value should be nil in a fresh buffer
(with-temp-buffer
  (should-equal etaf-etml-tag--current-hover-instance nil))

;;; Test event dispatch

(let ((clicked nil))
  (define-etaf-etml-tag test-click-tag
    :display 'inline-block
    :on-click (lambda (event)
                (setq clicked t)))
  (let ((instance (etaf-etml-tag-create-instance 'test-click-tag nil '("Test"))))
    ;; Dispatch click event
    (etaf-etml-tag--dispatch-event instance 'click)
    (should clicked)))

;;; Test UA Stylesheet Integration

;; Test that UA stylesheet can be retrieved
(should (stringp (etaf-ua-stylesheet-get-css)))

;; Test that UA stylesheet contains expected rules
(let ((css (etaf-ua-stylesheet-get-css)))
  (should (string-match-p "button" css))
  (should (string-match-p "padding" css))
  (should (string-match-p "border" css)))

;; Test that UA rules are properly parsed
(let ((rules (etaf-ua-stylesheet-get-rules)))
  (should (listp rules))
  (should (> (length rules) 0))
  ;; All rules should be marked as 'ua source
  (dolist (rule rules)
    (should-equal (plist-get rule :source) 'ua)))

;; Test CSS priority order: UA < Author < Inline
;; Create a simple DOM with button and build CSSOM
(let* ((dom '(html nil
               (body nil
                 (button ((id . "test-btn")) "Click"))))
       (cssom (etaf-css-build-cssom dom))
       (button-node (dom-by-id dom "test-btn"))
       (rules (etaf-css-get-rules-for-node cssom button-node dom)))
  ;; Should have UA rules for button
  (should (> (length rules) 0))
  ;; At least one rule should be from UA
  (should (cl-some (lambda (rule)
                     (eq (plist-get rule :source) 'ua))
                   rules)))

;; Test that UA styles are applied correctly through CSS system
(let* ((dom '(html nil
               (body nil
                 (h1 nil "Heading")
                 (button nil "Click")
                 (code nil "code"))))
       (cssom (etaf-css-build-cssom dom))
       (h1-node (car (dom-search dom (lambda (node) (eq (dom-tag node) 'h1)))))
       (button-node (car (dom-search dom (lambda (node) (eq (dom-tag node) 'button)))))
       (code-node (car (dom-search dom (lambda (node) (eq (dom-tag node) 'code))))))
  ;; h1 should have font-size from UA stylesheet
  (when h1-node
    (let ((h1-style (etaf-css-get-computed-style cssom h1-node dom)))
      (should (assq 'font-size h1-style))))
  ;; button should have padding and border from UA stylesheet
  (when button-node
    (let ((button-style (etaf-css-get-computed-style cssom button-node dom)))
      (should (or (assq 'padding-block button-style)
                  (assq 'padding button-style)))
      (should (assq 'border button-style))))
  ;; code should have font-family from UA stylesheet
  (when code-node
    (let ((code-style (etaf-css-get-computed-style cssom code-node dom)))
      (should (assq 'font-family code-style)))))

;; Test CSS cascade: inline style should override UA style
(let* ((dom '(html nil
               (body nil
                 (button ((style . "padding: 20px;")) "Click"))))
       (cssom (etaf-css-build-cssom dom))
       (button-node (car (dom-search dom (lambda (node) (eq (dom-tag node) 'button)))))
       (computed-style (etaf-css-get-computed-style cssom button-node dom)))
  ;; Inline padding should override UA padding
  (should (assq 'padding computed-style)))

;; Test CSS cascade: author style should override UA style
(let* ((dom '(html nil
               (head nil
                 (style nil "button { padding: 30px; }"))
               (body nil
                 (button nil "Click"))))
       (cssom (etaf-css-build-cssom dom))
       (button-node (car (dom-search dom (lambda (node) (eq (dom-tag node) 'button)))))
       (computed-style (etaf-css-get-computed-style cssom button-node dom)))
  ;; Author padding should override UA padding
  (should (assq 'padding computed-style)))

(provide 'etaf-etml-tag-tests)
