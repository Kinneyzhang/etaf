(require 'etaf-ert)
(require 'etaf-tag)

(setq-local lisp-indent-offset 2)

;;; Test tag registration

;; Test that built-in tags are defined
(should-equal (etaf-tag-defined-p 'div) t)
(should-equal (etaf-tag-defined-p 'span) t)
(should-equal (etaf-tag-defined-p 'button) t)
(should-equal (etaf-tag-defined-p 'input) t)
(should-equal (etaf-tag-defined-p 'a) t)
(should-equal (etaf-tag-defined-p 'p) t)
(should-equal (etaf-tag-defined-p 'h1) t)

;; Test undefined tag
(should-equal (etaf-tag-defined-p 'my-undefined-tag) nil)

;;; Test tag definitions

;; Test div tag definition
(let ((div-def (etaf-tag-get-definition 'div)))
  (should-equal (plist-get div-def :name) 'div)
  (should-equal (plist-get div-def :display) 'block)
  (should-equal (plist-get div-def :children-allowed) t))

;; Test button tag definition
(let ((button-def (etaf-tag-get-definition 'button)))
  (should-equal (plist-get button-def :name) 'button)
  (should-equal (plist-get button-def :display) 'inline-block)
  (should-equal (assq 'cursor (plist-get button-def :default-style))
                '(cursor . "pointer")))

;; Test self-closing tags
(let ((input-def (etaf-tag-get-definition 'input)))
  (should-equal (plist-get input-def :self-closing) t)
  (should-equal (plist-get input-def :children-allowed) nil))

(let ((br-def (etaf-tag-get-definition 'br)))
  (should-equal (plist-get br-def :self-closing) t)
  (should-equal (plist-get br-def :children-allowed) nil))

(let ((hr-def (etaf-tag-get-definition 'hr)))
  (should-equal (plist-get hr-def :self-closing) t)
  (should-equal (plist-get hr-def :children-allowed) nil))

;;; Test custom tag definition

(define-etaf-tag custom-button
  :display 'inline-block
  :inherit 'button
  :default-style '((background-color . "blue")
                   (color . "white"))
  :hover-style '((background-color . "darkblue")))

(should-equal (etaf-tag-defined-p 'custom-button) t)

(let ((custom-def (etaf-tag-get-definition 'custom-button)))
  (should-equal (plist-get custom-def :display) 'inline-block)
  (should-equal (plist-get custom-def :inherit) 'button)
  (should-equal (assq 'background-color (plist-get custom-def :default-style))
                '(background-color . "blue")))

;;; Test style merging

;; Test that merged style contains all expected properties (order-independent)
(let ((merged (etaf-tag--merge-styles '((color . "red") (font-size . "12px"))
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
 (cdr (assq 'color (etaf-tag--merge-styles nil '((color . "blue")))))
 "blue")

(should-equal
 (cdr (assq 'color (etaf-tag--merge-styles '((color . "red")) nil)))
 "red")

;;; Test style string parsing

(should-equal
 (etaf-tag--parse-style-string "color: red; font-size: 12px")
 '((color . "red") (font-size . "12px")))

(should-equal
 (etaf-tag--parse-style-string "  padding : 10px 20px  ;  margin: 5px  ")
 '((padding . "10px 20px") (margin . "5px")))

;;; Test style alist to string conversion

(should-equal
 (etaf-tag--style-alist-to-string '((color . "red") (font-size . "12px")))
 "color: red; font-size: 12px")

;;; Test tag instance creation

(let ((instance (etaf-tag-create-instance 'div '(:class "container") '("Hello"))))
  (should-equal (plist-get instance :tag-name) 'div)
  (should-equal (plist-get instance :attrs) '(:class "container"))
  (should-equal (plist-get instance :children) '("Hello")))

;;; Test tag parsing

(let ((parsed (etaf-tag-parse '(div :class "container" "Hello"))))
  (should-equal (plist-get parsed :tag-name) 'div)
  (should-equal (plist-get parsed :attrs) '(:class "container"))
  (should-equal (plist-get parsed :children) '("Hello")))

;; Test nested tag parsing
(let* ((parsed (etaf-tag-parse '(div :id "outer"
                                     (span :class "inner" "Text"))))
       (children (plist-get parsed :children))
       (child (car children)))
  (should-equal (plist-get parsed :tag-name) 'div)
  (should-equal (plist-get child :tag-name) 'span)
  (should-equal (plist-get child :attrs) '(:class "inner")))

;;; Test tag rendering to DOM

(let* ((instance (etaf-tag-create-instance 'div '(:class "container") '("Hello")))
       (dom (etaf-tag-render-to-dom instance)))
  (should-equal (car dom) 'div)
  (should-equal (assq 'class (cadr dom)) '(class . "container")))

;; Test rendering with style (using button which has default styles)
(let* ((instance (etaf-tag-create-instance 'button nil '("Click")))
       (dom (etaf-tag-render-to-dom instance)))
  (should-equal (car dom) 'button)
  ;; button has default style
  (should (assq 'style (cadr dom))))

;; Test rendering p tag (which has no default style)
(let* ((instance (etaf-tag-create-instance 'p nil '("Paragraph")))
       (dom (etaf-tag-render-to-dom instance)))
  (should-equal (car dom) 'p))

;;; Test etaf-tag-to-dom

(should-equal
 (car (etaf-tag-to-dom '(div "Hello")))
 'div)

(should-equal
 (car (etaf-tag-to-dom '(button :type "submit" "Click")))
 'button)

;;; Test tag list

(let ((all-tags (etaf-tag-list-all)))
  (should (member 'div all-tags))
  (should (member 'span all-tags))
  (should (member 'button all-tags))
  (should (member 'custom-button all-tags)))

;;; Test event creation

(let ((event (etaf-tag--make-event 'click '(:tag-name test) '(:x 100 :y 200))))
  (should-equal (plist-get event :type) 'click)
  (should-equal (plist-get event :target) '(:tag-name test))
  (should-equal (plist-get event :x) 100)
  (should-equal (plist-get event :y) 200)
  (should (plist-get event :timestamp)))

;;; Test display types for different tags

;; Block elements
(should-equal (plist-get (etaf-tag-get-definition 'div) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'p) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'h1) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'ul) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'ol) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'blockquote) :display) 'block)
(should-equal (plist-get (etaf-tag-get-definition 'pre) :display) 'block)

;; Inline elements
(should-equal (plist-get (etaf-tag-get-definition 'span) :display) 'inline)
(should-equal (plist-get (etaf-tag-get-definition 'a) :display) 'inline)
(should-equal (plist-get (etaf-tag-get-definition 'em) :display) 'inline)
(should-equal (plist-get (etaf-tag-get-definition 'strong) :display) 'inline)
(should-equal (plist-get (etaf-tag-get-definition 'code) :display) 'inline)

;; Inline-block elements
(should-equal (plist-get (etaf-tag-get-definition 'button) :display) 'inline-block)
(should-equal (plist-get (etaf-tag-get-definition 'input) :display) 'inline-block)
(should-equal (plist-get (etaf-tag-get-definition 'textarea) :display) 'inline-block)

;; Table elements
(should-equal (plist-get (etaf-tag-get-definition 'table) :display) 'table)
(should-equal (plist-get (etaf-tag-get-definition 'tr) :display) 'table-row)
(should-equal (plist-get (etaf-tag-get-definition 'td) :display) 'table-cell)
(should-equal (plist-get (etaf-tag-get-definition 'th) :display) 'table-cell)

;;; Test interactive tags have click handlers

(let ((a-def (etaf-tag-get-definition 'a)))
  (should (functionp (plist-get a-def :on-click))))

(let ((button-def (etaf-tag-get-definition 'button)))
  (should (functionp (plist-get button-def :on-click))))

(let ((summary-def (etaf-tag-get-definition 'summary)))
  (should (functionp (plist-get summary-def :on-click))))

;;; Test state-based styles

(let ((button-def (etaf-tag-get-definition 'button)))
  (should (plist-get button-def :hover-style))
  (should (plist-get button-def :active-style))
  (should (plist-get button-def :disabled-style)))

(let ((input-def (etaf-tag-get-definition 'input)))
  (should (plist-get input-def :focus-style))
  (should (plist-get input-def :disabled-style)))

(let ((a-def (etaf-tag-get-definition 'a)))
  (should (plist-get a-def :hover-style)))

;;; Test computed style with state

(let* ((instance (etaf-tag-create-instance 'button nil '("Click")))
       (base-style (etaf-tag-get-computed-style instance)))
  ;; Button should have padding-left/right in default style
  (should (assq 'padding-left base-style))
  (should (assq 'padding-right base-style))
  
  ;; Simulate hover state
  (plist-put (plist-get instance :state) :hovered t)
  (let ((hover-style (etaf-tag-get-computed-style instance)))
    ;; Should have hover background color
    (should (assq 'background-color hover-style))))

;;; Test unit consistency - vertical uses lh, horizontal uses px

;; Test ul tag uses lh for vertical margins and px for horizontal padding
(let ((ul-def (etaf-tag-get-definition 'ul)))
  (let ((style (plist-get ul-def :default-style)))
    (should (string-match "lh" (cdr (assq 'margin-top style))))
    (should (string-match "lh" (cdr (assq 'margin-bottom style))))
    (should (string-match "px" (cdr (assq 'padding-left style))))))

;; Test blockquote uses lh for vertical margins
(let ((bq-def (etaf-tag-get-definition 'blockquote)))
  (let ((style (plist-get bq-def :default-style)))
    (should (string-match "lh" (cdr (assq 'margin-top style))))
    (should (string-match "lh" (cdr (assq 'margin-bottom style))))))

;; Test button tag uses lh units for vertical padding and px units for horizontal padding
;; Note: vertical padding values are "0lh" which still uses the lh unit
(let ((button-def (etaf-tag-get-definition 'button)))
  (let ((style (plist-get button-def :default-style)))
    (should (string-match "lh" (cdr (assq 'padding-top style))))
    (should (string-match "lh" (cdr (assq 'padding-bottom style))))
    (should (string-match "px" (cdr (assq 'padding-left style))))
    (should (string-match "px" (cdr (assq 'padding-right style))))))

;;; Test event handling keymap setup

;; Test keymap creation for clickable elements
(let* ((instance (etaf-tag-create-instance 'button nil '("Click")))
       (keymap (etaf-tag-setup-keymap instance)))
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
(let* ((instance (etaf-tag-create-instance 'div nil '("Text")))
       (keymap (etaf-tag-setup-keymap instance)))
  (should (keymapp keymap))
  ;; Should NOT have RET binding (no on-click)
  (should-not (lookup-key keymap (kbd "RET")))
  ;; Should NOT have mouse-1 binding
  (should-not (lookup-key keymap [mouse-1])))

;;; Test etaf-tag-make-interactive adds proper text properties

(with-temp-buffer
  (insert "Click Me")
  (let ((instance (etaf-tag-create-instance 'button nil '("Click Me"))))
    (etaf-tag-make-interactive 1 9 instance)
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

;;; Test hover tracking variables are defined in etaf-tag.el

;; The variable should be defined after loading etaf-tag.el (buffer-local)
(should (boundp 'etaf-tag--current-hover-instance))
;; Initial value should be nil in a fresh buffer
(with-temp-buffer
  (should-equal etaf-tag--current-hover-instance nil))

;;; Test event dispatch

(let ((clicked nil))
  (define-etaf-tag test-click-tag
    :display 'inline-block
    :on-click (lambda (event)
                (setq clicked t)))
  (let ((instance (etaf-tag-create-instance 'test-click-tag nil '("Test"))))
    ;; Dispatch click event
    (etaf-tag--dispatch-event instance 'click)
    (should clicked)))

(provide 'etaf-tag-tests)
