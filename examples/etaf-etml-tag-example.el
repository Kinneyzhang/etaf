;;; etaf-etml-tag-example.el --- ETML Tag Definition System Examples -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates the usage of the ETML tag definition system.
;; It shows how to:
;; - Define custom tags with styles and interactions
;; - Use built-in tags
;; - Create interactive components
;; - Handle events

;;; Code:

(require 'etaf-etml-tag)

;;; Example 1: Basic Tag Usage

(defun etaf-etml-tag-example-basic ()
  "Demonstrate basic tag usage."
  (interactive)
  (message "=== Basic Tag Usage ===\n")
  
  ;; Check if tags are defined
  (message "div defined: %s" (etaf-etml-tag-defined-p 'div))
  (message "button defined: %s" (etaf-etml-tag-defined-p 'button))
  
  ;; Parse a simple tag
  (let* ((sexp '(div :class "container" "Hello World"))
         (parsed (etaf-etml-tag-parse sexp)))
    (message "\nParsed tag: %S" parsed)
    (message "Tag name: %s" (plist-get parsed :tag-name))
    (message "Attrs: %S" (plist-get parsed :attrs))
    (message "Children: %S" (plist-get parsed :children))))

;;; Example 2: Tag Definitions

(defun etaf-etml-tag-example-definitions ()
  "Demonstrate tag definitions."
  (interactive)
  (message "=== Tag Definitions ===\n")
  
  ;; Get definition for a block element
  (let ((div-def (etaf-etml-tag-get-definition 'div)))
    (message "div display: %s" (plist-get div-def :display))
    (message "div allows children: %s" (plist-get div-def :children-allowed)))
  
  ;; Get definition for an inline element
  (let ((span-def (etaf-etml-tag-get-definition 'span)))
    (message "\nspan display: %s" (plist-get span-def :display)))
  
  ;; Get definition for a form element
  (let ((button-def (etaf-etml-tag-get-definition 'button)))
    (message "\nbutton display: %s" (plist-get button-def :display))
    (message "button has click handler: %s"
             (functionp (plist-get button-def :on-click)))
    ;; Note: Default styles now come from UA stylesheet, not tag definition
    (message "button default style (in tag def): %S" (plist-get button-def :default-style))
    (message "button hover style: %S" (plist-get button-def :hover-style))))

;;; Example 3: Custom Tag Definition

;; Note: For custom tags, you can still use :default-style if you want
;; tag-specific styles that don't belong in the global UA stylesheet.
;; However, HTML-like built-in tags (button, input, etc.) now get their
;; default styles from the User Agent Stylesheet.

;; Define a custom primary button tag
(define-etaf-etml-tag primary-button
  :display 'inline-block
  :inherit 'button
  :hover-style '((background-color . "#0056b3"))
  :active-style '((background-color . "#004494"))
  :on-click (lambda (event)
              (message "Primary button clicked!")
              (let* ((target (plist-get event :target))
                     (attrs (plist-get target :attrs)))
                (when-let ((custom-action (plist-get attrs :action)))
                  (message "Executing action: %s" custom-action)))))

;; Define a custom danger button tag
(define-etaf-etml-tag danger-button
  :display 'inline-block
  :inherit 'button
  :hover-style '((background-color . "#c82333"))
  :on-click (lambda (event)
              (message "Danger button clicked! Be careful!")))

;; Define a custom card component
;; Custom components can use :default-style for their unique styling
(define-etaf-etml-tag card
  :display 'block)

;; Define a custom badge/chip component
(define-etaf-etml-tag badge
  :display 'inline-block)

(defun etaf-etml-tag-example-custom ()
  "Demonstrate custom tag definitions."
  (interactive)
  (message "=== Custom Tag Definitions ===\n")
  
  ;; Check custom tags are defined
  (message "primary-button defined: %s" (etaf-etml-tag-defined-p 'primary-button))
  (message "danger-button defined: %s" (etaf-etml-tag-defined-p 'danger-button))
  (message "card defined: %s" (etaf-etml-tag-defined-p 'card))
  (message "badge defined: %s" (etaf-etml-tag-defined-p 'badge))
  
  ;; Get custom tag definition
  (let ((primary-def (etaf-etml-tag-get-definition 'primary-button)))
    (message "\nprimary-button inherits from: %s" (plist-get primary-def :inherit))))

;;; Example 4: Rendering Tags to DOM

(defun etaf-etml-tag-example-render ()
  "Demonstrate rendering tags to DOM format."
  (interactive)
  (message "=== Rendering Tags to DOM ===\n")
  
  ;; Simple tag
  (let ((dom (etaf-etml-tag-to-dom '(div :class "container" "Hello"))))
    (message "Simple div DOM: %S" dom))
  
  ;; Nested tags
  (let ((dom (etaf-etml-tag-to-dom
              '(div :class "app"
                    (h1 "Title")
                    (p :class "content" "This is a paragraph.")
                    (button :type "submit" "Click Me")))))
    (message "\nNested DOM: %S" dom))
  
  ;; Using custom tags
  (let ((dom (etaf-etml-tag-to-dom
              '(card
                (h2 "Card Title")
                (p "Card content goes here.")
                (primary-button :action "save" "Save")
                (danger-button "Delete")))))
    (message "\nCustom component DOM: %S" dom)))

;;; Example 5: Interactive Components

(defun etaf-etml-tag-example-interactive ()
  "Demonstrate interactive tag features."
  (interactive)
  (message "=== Interactive Tags ===\n")
  
  ;; Create a button instance
  (let* ((instance (etaf-etml-tag-create-instance
                    'button
                    '(:class "my-btn" :type "button")
                    '("Click Me")))
         (definition (plist-get instance :definition)))
    (message "Button instance created")
    (message "Has click handler: %s" (functionp (plist-get definition :on-click)))
    (message "Has hover style: %s" (not (null (plist-get definition :hover-style))))
    
    ;; Simulate state changes
    (message "\n--- Simulating states ---")
    
    ;; Default state
    (let ((style (etaf-etml-tag-get-computed-style instance)))
      (message "Default background: %s" (cdr (assq 'background-color style))))
    
    ;; Hover state
    (plist-put (plist-get instance :state) :hovered t)
    (let ((style (etaf-etml-tag-get-computed-style instance)))
      (message "Hovered background: %s" (cdr (assq 'background-color style))))
    
    ;; Active state
    (plist-put (plist-get instance :state) :active t)
    (let ((style (etaf-etml-tag-get-computed-style instance)))
      (message "Active background: %s" (cdr (assq 'background-color style))))
    
    ;; Disabled state
    (plist-put (plist-get instance :state) :hovered nil)
    (plist-put (plist-get instance :state) :active nil)
    (plist-put (plist-get instance :state) :disabled t)
    (let ((style (etaf-etml-tag-get-computed-style instance)))
      (message "Disabled background: %s" (cdr (assq 'background-color style))))))

;;; Example 6: Event Handling

(defun etaf-etml-tag-example-events ()
  "Demonstrate event handling."
  (interactive)
  (message "=== Event Handling ===\n")
  
  ;; Create an event
  (let ((event (etaf-etml-tag--make-event 'click
                                     '(:tag-name button)
                                     '(:x 100 :y 200 :button 1))))
    (message "Event type: %s" (plist-get event :type))
    (message "Event target: %S" (plist-get event :target))
    (message "Event timestamp: %s" (plist-get event :timestamp))
    (message "Event x: %s" (plist-get event :x))
    (message "Event y: %s" (plist-get event :y)))
  
  ;; Dispatch event on an instance
  (message "\n--- Dispatching events ---")
  (let ((instance (etaf-etml-tag-create-instance
                   'primary-button
                   '(:action "test-action")
                   '("Test Button"))))
    (message "Dispatching click event...")
    (etaf-etml-tag--dispatch-event instance 'click)))

;;; Example 7: List All Tags

(defun etaf-etml-tag-example-list-tags ()
  "List all defined tags by category."
  (interactive)
  (message "=== All Defined Tags ===\n")
  
  (let* ((all-tags (etaf-etml-tag-list-all))
         (block-tags '())
         (inline-tags '())
         (inline-block-tags '())
         (other-tags '()))
    ;; Categorize tags
    (dolist (tag all-tags)
      (let* ((def (etaf-etml-tag-get-definition tag))
             (display (plist-get def :display)))
        (pcase display
          ('block (push tag block-tags))
          ('inline (push tag inline-tags))
          ('inline-block (push tag inline-block-tags))
          (_ (push tag other-tags)))))
    
    (message "Block tags (%d):" (length block-tags))
    (message "  %S" (reverse block-tags))
    
    (message "\nInline tags (%d):" (length inline-tags))
    (message "  %S" (reverse inline-tags))
    
    (message "\nInline-block tags (%d):" (length inline-block-tags))
    (message "  %S" (reverse inline-block-tags))
    
    (message "\nOther tags (%d):" (length other-tags))
    (message "  %S" (reverse other-tags))
    
    (message "\nTotal tags: %d" (length all-tags))))

;;; Run All Examples

(defun etaf-tag-run-all-examples ()
  "Run all tag examples."
  (interactive)
  (etaf-etml-tag-example-basic)
  (message "\n")
  (etaf-etml-tag-example-definitions)
  (message "\n")
  (etaf-etml-tag-example-custom)
  (message "\n")
  (etaf-etml-tag-example-render)
  (message "\n")
  (etaf-etml-tag-example-interactive)
  (message "\n")
  (etaf-etml-tag-example-events)
  (message "\n")
  (etaf-etml-tag-example-list-tags)
  (message "\n=== All Examples Complete ==="))

(provide 'etaf-etml-tag-example)
;;; etaf-etml-tag-example.el ends here
