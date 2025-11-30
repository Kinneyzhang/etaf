;;; etaf-etml-tag.el --- ETML Tag Definition System -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: template, markup, tags
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; ETML Tag Definition System - Define ETML tags similar to HTML tags
;;
;; This module provides a system for defining ETML tags with:
;; - Content: The inner content of the tag
;; - Style: CSS-like styling for the tag
;; - Interaction: Event handlers and interactive behaviors
;;
;; Key Features:
;; - `define-etaf-etml-tag': Macro for defining custom tags
;; - Built-in tags: div, span, button, input, a, p, h1-h6, etc.
;; - Event handlers: on-click, on-hover, on-focus, on-change, etc.
;; - Style inheritance: Tags can inherit styles from parent tags
;;
;; Event Support Status (using Emacs native capabilities):
;;
;; FULLY SUPPORTED:
;; - :on-click - Triggered by mouse-1 click, RET, or SPC key
;;               Uses Emacs keymap property on text
;; - :on-hover-enter - Triggered when mouse enters the text region
;;                     Uses help-echo function for tracking
;; - :on-hover-leave - Triggered when mouse leaves the text region
;;                     Uses help-echo function for tracking
;; - :on-keydown - Triggered on key press when point is in region
;;                 Uses keymap with remapped self-insert-command
;;
;; PARTIALLY SUPPORTED:
;; - :hover-style - Visual hover effect using mouse-face property
;;
;; NOT SUPPORTED (Emacs limitations):
;; - :on-keyup - Emacs has no key release events
;; - :on-focus - Text regions don't have focus concept in Emacs
;; - :on-blur - Text regions don't have focus concept in Emacs
;; - :on-change - For form input; use widgets (widget.el) instead
;; - :on-input - For form input; use widgets (widget.el) instead
;;
;; Usage Example:
;;
;;   ;; Define a custom button tag
;;   (define-etaf-etml-tag my-button
;;     :display 'inline-block
;;     :default-style '((background-color . "blue")
;;                      (color . "white")
;;                      (padding . "10px 20px"))
;;     :hover-style '((background-color . "darkblue"))
;;     :on-click (lambda (event) (message "Button clicked!")))
;;
;;   ;; Use the tag in ETML
;;   (my-button :class "primary" "Click Me")

;;; Code:

(require 'cl-lib)

;;; Tag Registry

(defvar etaf-etml-tag-definitions (make-hash-table :test 'eq)
  "Hash table storing all defined ETML tag definitions.
Keys are tag symbols, values are tag definition plists.")

(defvar etaf-etml-tag-instances nil
  "List of active tag instances in the current buffer.")

;;; Tag Definition Structure

(defun etaf-etml-tag-create-definition (name &rest props)
  "Create a tag definition with NAME and PROPS.
PROPS is a plist with the following keys:
- :display - Display type: `block', `inline', `inline-block', `flex', `none'
- :default-style - Default CSS-like style alist
- :hover-style - Style applied on hover
- :active-style - Style applied when active/pressed
- :focus-style - Style applied when focused
- :disabled-style - Style applied when disabled
- :on-click - Click event handler function
- :on-hover-enter - Mouse enter event handler
- :on-hover-leave - Mouse leave event handler
- :on-focus - Focus event handler
- :on-blur - Blur event handler
- :on-change - Change event handler (for form elements)
- :on-input - Input event handler
- :on-keydown - Keydown event handler
- :on-keyup - Keyup event handler
- :children-allowed - Whether the tag can have children (default t)
- :self-closing - Whether the tag is self-closing (default nil)
- :inherit - Parent tag to inherit from
- :render - Custom render function"
  (let ((definition (list :name name
                          :display (or (plist-get props :display) 'inline)
                          :default-style (plist-get props :default-style)
                          :hover-style (plist-get props :hover-style)
                          :active-style (plist-get props :active-style)
                          :focus-style (plist-get props :focus-style)
                          :disabled-style (plist-get props :disabled-style)
                          :on-click (plist-get props :on-click)
                          :on-hover-enter (plist-get props :on-hover-enter)
                          :on-hover-leave (plist-get props :on-hover-leave)
                          :on-focus (plist-get props :on-focus)
                          :on-blur (plist-get props :on-blur)
                          :on-change (plist-get props :on-change)
                          :on-input (plist-get props :on-input)
                          :on-keydown (plist-get props :on-keydown)
                          :on-keyup (plist-get props :on-keyup)
                          :children-allowed (if (plist-member props :children-allowed)
                                                (plist-get props :children-allowed)
                                              t)
                          :self-closing (plist-get props :self-closing)
                          :inherit (plist-get props :inherit)
                          :render (plist-get props :render))))
    definition))

(defun etaf-etml-tag-get-definition (name)
  "Get the tag definition for NAME."
  (gethash name etaf-etml-tag-definitions))

(defun etaf-etml-tag-register (name definition)
  "Register a tag DEFINITION with NAME."
  (puthash name definition etaf-etml-tag-definitions))

(defun etaf-etml-tag-unregister (name)
  "Unregister the tag with NAME."
  (remhash name etaf-etml-tag-definitions))

(defun etaf-etml-tag-defined-p (name)
  "Check if a tag with NAME is defined."
  (not (null (gethash name etaf-etml-tag-definitions))))

(defun etaf-etml-tag-list-all ()
  "Return a list of all defined tag names."
  (let ((tags nil))
    (maphash (lambda (key _value) (push key tags))
             etaf-etml-tag-definitions)
    (nreverse tags)))

;;; Style Merging

(defun etaf-etml-tag--merge-styles (base-style override-style)
  "Merge OVERRIDE-STYLE into BASE-STYLE.
Returns a new alist with all properties from both, with override taking precedence."
  (let ((result (copy-alist (or base-style '()))))
    (dolist (pair (or override-style '()))
      (let ((existing (assq (car pair) result)))
        (if existing
            (setcdr existing (cdr pair))
          (push pair result))))
    result))

(defun etaf-etml-tag--resolve-inherited-style (tag-name)
  "Resolve the complete style for TAG-NAME including inherited styles."
  (let* ((definition (etaf-etml-tag-get-definition tag-name))
         (inherit (plist-get definition :inherit))
         (own-style (plist-get definition :default-style)))
    (if (and inherit (etaf-etml-tag-defined-p inherit))
        (etaf-etml-tag--merge-styles
         (etaf-etml-tag--resolve-inherited-style inherit)
         own-style)
      own-style)))

;;; Event Handling

(defun etaf-etml-tag--get-text-content (tag-instance)
  "Extract the text content from TAG-INSTANCE's children.
TAG-INSTANCE should be a plist with a :children property.
Children can be strings or nested tag instances (plists with :children).
Returns a string with all text content concatenated."
  (let ((children (plist-get tag-instance :children)))
    (when children
      (mapconcat (lambda (child)
                   (cond
                    ((stringp child) child)
                    ;; Handle nested tag instances (plists with :children)
                    ((and (listp child)
                          (plist-member child :children))
                     (etaf-etml-tag--get-text-content child))
                    ;; Handle DOM nodes (lists like (tag ((attrs)) children...))
                    ((and (listp child)
                          (symbolp (car child))
                          (listp (cadr child)))
                     (let ((dom-children (cddr child)))
                       (mapconcat (lambda (c)
                                    (if (stringp c) c ""))
                                  dom-children "")))
                    (t "")))
                 children ""))))

(defun etaf-etml-tag--make-event (type target &optional extra-data)
  "Create an event object with TYPE and TARGET.
EXTRA-DATA is an optional plist of additional event properties.
TARGET should be a tag-instance (plist with :tag-name, :children, etc.).
The event includes a :text property with the target's text content."
  (let ((text (when (and (listp target)
                         (plist-member target :children))
                (etaf-etml-tag--get-text-content target))))
    (append (list :type type
                  :target target
                  :text text
                  :timestamp (current-time))
            extra-data)))

(defun etaf-etml-tag--dispatch-event (tag-instance event-type &optional extra-data)
  "Dispatch an event of EVENT-TYPE for TAG-INSTANCE."
  (let* ((definition (plist-get tag-instance :definition))
         (handler-key (intern (concat ":on-" (symbol-name event-type))))
         (handler (plist-get definition handler-key)))
    (when (functionp handler)
      (let ((event (etaf-etml-tag--make-event event-type tag-instance extra-data)))
        (funcall handler event)))))

;;; Interactive Behaviors

;; Hover tracking state - buffer-local to avoid conflicts between buffers
(defvar-local etaf-etml-tag--current-hover-instance nil
  "The tag instance currently being hovered over in this buffer.
This is buffer-local to support multiple buffers with etaf-tag content.")

(defvar-local etaf-etml-tag--hover-overlay nil
  "Overlay used for hover state visual feedback in this buffer.")

(defun etaf-etml-tag-setup-keymap (tag-instance)
  "Set up keybindings for TAG-INSTANCE.
Creates a keymap with the following bindings:
- RET and SPC: trigger on-click event
- mouse-1: trigger on-click event
- Other keys: trigger on-keydown event if handler is defined

Returns the configured keymap."
  (let* ((keymap (make-sparse-keymap))
         (definition (plist-get tag-instance :definition))
         (on-click (plist-get definition :on-click))
         (on-keydown (plist-get definition :on-keydown))
         (on-keyup (plist-get definition :on-keyup)))
    ;; Click handler via RET and SPC keys
    (when on-click
      (define-key keymap (kbd "RET")
        (lambda ()
          (interactive)
          (etaf-etml-tag--dispatch-event tag-instance 'click)))
      (define-key keymap (kbd "SPC")
        (lambda ()
          (interactive)
          (etaf-etml-tag--dispatch-event tag-instance 'click)))
      ;; Mouse click handler - mouse-1
      (define-key keymap [mouse-1]
        (lambda (event)
          (interactive "e")
          (let* ((posn (event-start event))
                 (pos (posn-point posn)))
            (etaf-etml-tag--dispatch-event tag-instance 'click
                                      (list :mouse-event event
                                            :position pos)))))
      ;; Double click
      (define-key keymap [double-mouse-1]
        (lambda (event)
          (interactive "e")
          (let* ((posn (event-start event))
                 (pos (posn-point posn)))
            (etaf-etml-tag--dispatch-event tag-instance 'dblclick
                                      (list :mouse-event event
                                            :position pos))))))
    ;; Keydown handler - capture any key when focused
    (when on-keydown
      (define-key keymap [remap self-insert-command]
        (lambda ()
          (interactive)
          (let ((key (this-command-keys-vector)))
            (etaf-etml-tag--dispatch-event tag-instance 'keydown
                                      (list :key key
                                            :key-char (when (> (length key) 0)
                                                        (aref key (1- (length key))))))))))
    ;; Note: on-keyup cannot be reliably implemented in Emacs
    ;; as there's no native key release event. It is marked as unsupported.
    keymap))

(defun etaf-etml-tag--help-echo-handler (window _obj pos)
  "Help-echo function to track mouse hover and dispatch hover events.
WINDOW is the window where the mouse is.
POS is the buffer position under the mouse."
  (when (and window pos)
    (let* ((instance (get-text-property pos 'etaf-tag-instance))
           (definition (and instance (plist-get instance :definition))))
      ;; Handle hover-leave for previous instance
      (when (and etaf-etml-tag--current-hover-instance
                 (not (eq etaf-etml-tag--current-hover-instance instance)))
        (let ((old-def (plist-get etaf-etml-tag--current-hover-instance :definition)))
          (when (plist-get old-def :on-hover-leave)
            (etaf-etml-tag--dispatch-event etaf-etml-tag--current-hover-instance 'hover-leave))
          ;; Update hover state
          (plist-put (plist-get etaf-etml-tag--current-hover-instance :state) :hovered nil)))
      ;; Handle hover-enter for new instance
      (when (and instance (not (eq etaf-etml-tag--current-hover-instance instance)))
        (when (plist-get definition :on-hover-enter)
          (etaf-etml-tag--dispatch-event instance 'hover-enter))
        ;; Update hover state
        (plist-put (plist-get instance :state) :hovered t))
      ;; Update current hover instance
      (setq etaf-etml-tag--current-hover-instance instance)
      ;; Return help-echo string
      (when definition
        (let ((tag-name (or (plist-get definition :name) "element")))
          (format "Click or press RET/SPC to activate %s" tag-name))))))

(defun etaf-etml-tag-make-interactive (start end tag-instance)
  "Make the region from START to END interactive based on TAG-INSTANCE.
This function adds text properties to enable:
- Click events via keyboard (RET, SPC) and mouse (mouse-1)
- Hover events via help-echo tracking
- Visual hover feedback via mouse-face
- Pointer cursor change

Note: The following events have limitations in Emacs:
- on-focus/on-blur: Not supported for text regions (no focus concept)
- on-change/on-input: Not supported for static text (use widgets instead)
- on-keyup: Not reliably supported (no key release event in Emacs)"
  (let* ((definition (plist-get tag-instance :definition))
         (keymap (etaf-etml-tag-setup-keymap tag-instance))
         (has-click (plist-get definition :on-click))
         (has-hover (or (plist-get definition :on-hover-enter)
                        (plist-get definition :on-hover-leave)
                        (plist-get definition :hover-style))))
    ;; Add text properties for interactivity
    (add-text-properties
     start end
     `(etaf-tag-instance ,tag-instance
       keymap ,keymap
       ;; Visual feedback for hover
       ,@(when (or has-click has-hover)
           '(mouse-face highlight))
       ;; Pointer cursor for clickable elements
       ,@(when has-click
           '(pointer hand))
       ;; Help-echo with function for hover tracking
       help-echo ,#'etaf-etml-tag--help-echo-handler))))

;;; Tag Instance Creation

(defun etaf-etml-tag-create-instance (tag-name attrs children)
  "Create a tag instance with TAG-NAME, ATTRS, and CHILDREN."
  (let* ((definition (etaf-etml-tag-get-definition tag-name))
         (instance (list :tag-name tag-name
                         :definition definition
                         :attrs attrs
                         :children children
                         :state (list :hovered nil
                                      :focused nil
                                      :active nil
                                      :disabled (plist-get attrs :disabled)))))
    ;; Register instance
    (push instance etaf-etml-tag-instances)
    instance))

;;; Tag Definition Macro

;;;###autoload
(defmacro define-etaf-etml-tag (name &rest props)
  "Define an ETML tag with NAME and PROPS.

NAME is the tag symbol (e.g., button, div, my-component).

PROPS is a plist that can include:
- :display - Display type (`block', `inline', `inline-block', `flex', `none')
- :default-style - Default style alist ((property . value) ...)
- :hover-style - Style when hovered (visual feedback via mouse-face)
- :active-style - Style when active/pressed
- :focus-style - Style when focused (not supported for text regions)
- :disabled-style - Style when disabled

Event handlers (see Commentary for support status):
- :on-click - Click handler (SUPPORTED: mouse-1, RET, SPC)
- :on-hover-enter - Mouse enter handler (SUPPORTED: via help-echo tracking)
- :on-hover-leave - Mouse leave handler (SUPPORTED: via help-echo tracking)
- :on-keydown - Keydown handler (SUPPORTED: via keymap)
- :on-keyup - Keyup handler (NOT SUPPORTED: no key release event in Emacs)
- :on-focus - Focus handler (NOT SUPPORTED: text regions don't have focus)
- :on-blur - Blur handler (NOT SUPPORTED: text regions don't have focus)
- :on-change - Change handler (NOT SUPPORTED: use widget.el for forms)
- :on-input - Input handler (NOT SUPPORTED: use widget.el for forms)

Other options:
- :children-allowed - Whether children are allowed (default t)
- :self-closing - Whether self-closing (default nil)
- :inherit - Parent tag to inherit from
- :render - Custom render function

Example:
  (define-etaf-etml-tag button
    :display \\='inline-block
    :default-style \\='((padding . \"5px 10px\")
                      (cursor . \"pointer\"))
    :hover-style \\='((background-color . \"#e0e0e0\"))
    :on-click (lambda (event)
                (message \"Button clicked!\")))"
  (declare (indent defun))
  (let ((definition-var (intern (format "etaf-etml-tag--%s-definition" name))))
    `(progn
       ;; Create the definition
       (defvar ,definition-var
         (etaf-etml-tag-create-definition ',name ,@props)
         ,(format "Tag definition for %s." name))
       ;; Register the tag
       (etaf-etml-tag-register ',name ,definition-var)
       ;; Return the tag name
       ',name)))

;;; Tag Rendering

(defun etaf-etml-tag-get-computed-style (tag-instance)
  "Get the computed style for TAG-INSTANCE based on its current state."
  (let* ((definition (plist-get tag-instance :definition))
         (state (plist-get tag-instance :state))
         (attrs (plist-get tag-instance :attrs))
         ;; Start with inherited + default style
         (base-style (etaf-etml-tag--resolve-inherited-style
                      (plist-get tag-instance :tag-name)))
         ;; Apply inline styles from attrs
         (inline-style (plist-get attrs :style))
         (style (etaf-etml-tag--merge-styles
                 base-style (when inline-style
                              (if (stringp inline-style)
                                  (etaf-etml-tag--parse-style-string inline-style)
                                inline-style)))))
    ;; Apply state-based styles
    (when (plist-get state :hovered)
      (setq style (etaf-etml-tag--merge-styles
                   style (plist-get definition :hover-style))))
    (when (plist-get state :focused)
      (setq style (etaf-etml-tag--merge-styles
                   style (plist-get definition :focus-style))))
    (when (plist-get state :active)
      (setq style (etaf-etml-tag--merge-styles
                   style (plist-get definition :active-style))))
    (when (plist-get state :disabled)
      (setq style (etaf-etml-tag--merge-styles
                   style (plist-get definition :disabled-style))))
    style))

(defun etaf-etml-tag--parse-style-string (style-string)
  "Parse a CSS style string into an alist.
Example: \"color: red; padding: 10px\" => ((color . \"red\") (padding . \"10px\"))"
  (let ((result '())
        (declarations (split-string style-string ";" t)))
    (dolist (decl declarations)
      (when (string-match "\\s-*\\([^:]+\\)\\s-*:\\s-*\\(.+\\)\\s-*" decl)
        (let ((prop (intern (string-trim (match-string 1 decl))))
              (value (string-trim (match-string 2 decl))))
          (push (cons prop value) result))))
    (nreverse result)))

(defun etaf-etml-tag-render-to-dom (tag-instance)
  "Render TAG-INSTANCE to DOM format.
Returns (tag-name ((attrs...)) children...)."
  (let* ((tag-name (plist-get tag-instance :tag-name))
         (definition (plist-get tag-instance :definition))
         (attrs (plist-get tag-instance :attrs))
         (children (plist-get tag-instance :children))
         (custom-render (plist-get definition :render))
         (computed-style (etaf-etml-tag-get-computed-style tag-instance)))
    ;; Use custom render if provided
    (if custom-render
        (funcall custom-render tag-instance)
      ;; Default rendering
      (let ((dom-attrs '()))
        ;; Convert computed style to style attribute
        (when computed-style
          (push (cons 'style (etaf-etml-tag--style-alist-to-string computed-style))
                dom-attrs))
        ;; Copy other attributes (except :style which we computed)
        (let ((attr-rest attrs))
          (while attr-rest
            (let ((key (car attr-rest))
                  (value (cadr attr-rest)))
              (unless (eq key :style)
                (when (keywordp key)
                  (let ((attr-name (intern (substring (symbol-name key) 1))))
                    (push (cons attr-name value) dom-attrs))))
              (setq attr-rest (cddr attr-rest)))))
        ;; Build DOM node
        (cons tag-name
              (cons (nreverse dom-attrs)
                    (mapcar (lambda (child)
                              (if (and (listp child)
                                       (plist-get child :tag-name))
                                  (etaf-etml-tag-render-to-dom child)
                                child))
                            children)))))))

(defun etaf-etml-tag--style-alist-to-string (style-alist)
  "Convert STYLE-ALIST to CSS style string."
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             style-alist "; "))

;;; Built-in Tags

;; Block-level tags
(define-etaf-etml-tag div
  :display 'block
  :default-style nil)

(define-etaf-etml-tag p
  :display 'block
  :default-style nil)

(define-etaf-etml-tag h1
  :display 'block
  :default-style '((font-size . 1.6)
                   (font-weight . "bold")))

(define-etaf-etml-tag h2
  :display 'block
  :default-style '((font-size . 1.4)
                   (font-weight . "bold")))

(define-etaf-etml-tag h3
  :display 'block
  :default-style '((font-size . 1.3)
                   (font-weight . "bold")))

(define-etaf-etml-tag h4
  :display 'block
  :default-style '((font-size . 1.2)
                   (font-weight . "bold")))

(define-etaf-etml-tag h5
  :display 'block
  :default-style '((font-size . 1.1)
                   (font-weight . "bold")))

(define-etaf-etml-tag h6
  :display 'block
  :default-style '((font-size . 1.0)
                   (font-weight . "bold")))

(define-etaf-etml-tag header
  :display 'block)

(define-etaf-etml-tag footer
  :display 'block)

(define-etaf-etml-tag section
  :display 'block)

(define-etaf-etml-tag article
  :display 'block)

(define-etaf-etml-tag aside
  :display 'block)

(define-etaf-etml-tag nav
  :display 'block)

(define-etaf-etml-tag main
  :display 'block)

(define-etaf-etml-tag ul
  :display 'block
  :default-style '((list-style-type . "disc")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (padding-left . "10px")))

(define-etaf-etml-tag ol
  :display 'block
  :default-style '((list-style-type . "decimal")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (padding-left . "10px")))

(define-etaf-etml-tag li
  :display 'list-item)

(define-etaf-etml-tag blockquote
  :display 'block
  :default-style '((margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (margin-left . "40px")
                   (margin-right . "40px")))

(define-etaf-etml-tag pre
  :display 'block
  :default-style '((font-family . "monospace")
                   (white-space . "pre")))

(define-etaf-etml-tag hr
  :display 'block
  :self-closing t
  :children-allowed nil
  :default-style '((border-top . "1px solid")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")))

;; Inline tags
(define-etaf-etml-tag span
  :display 'inline)

(define-etaf-etml-tag a
  :display 'inline
  :default-style '((color . "blue")
                   (text-decoration . "underline")
                   (cursor . "pointer"))
  :hover-style '((color . "darkblue"))
  :on-click (lambda (event)
              (let* ((target (plist-get event :target))
                     (attrs (plist-get target :attrs))
                     (href (plist-get attrs :href)))
                (when href
                  (browse-url href)))))

(define-etaf-etml-tag em
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-etml-tag strong
  :display 'inline
  :default-style '((font-weight . "bold")))

(define-etaf-etml-tag b
  :display 'inline
  :default-style '((font-weight . "bold")))

(define-etaf-etml-tag i
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-etml-tag u
  :display 'inline
  :default-style '((text-decoration . "underline")))

(define-etaf-etml-tag s
  :display 'inline
  :default-style '((text-decoration . "line-through")))

(define-etaf-etml-tag del
  :display 'inline
  :default-style '((text-decoration . "line-through")))

(define-etaf-etml-tag ins
  :display 'inline
  :default-style '((text-decoration . "underline")))

(define-etaf-etml-tag mark
  :display 'inline
  :default-style '((background-color . "yellow")))

(define-etaf-etml-tag small
  :display 'inline
  :default-style '((font-size . "smaller")))

(define-etaf-etml-tag sub
  :display 'inline
  :default-style '((vertical-align . "sub")
                   (font-size . "smaller")))

(define-etaf-etml-tag sup
  :display 'inline
  :default-style '((vertical-align . "super")
                   (font-size . "smaller")))

(define-etaf-etml-tag code
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-etml-tag kbd
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-etml-tag samp
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-etml-tag var
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-etml-tag abbr
  :display 'inline
  :default-style '((text-decoration . "dotted underline")))

(define-etaf-etml-tag cite
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-etml-tag q
  :display 'inline)

(define-etaf-etml-tag br
  :display 'inline
  :self-closing t
  :children-allowed nil)

;; Form elements
(define-etaf-etml-tag button
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "10px")
                   (padding-right . "10px")
                   (border . "1px solid #ccc")
                   (cursor . "pointer"))
  :hover-style '((background-color . "#e0e0e0"))
  :active-style '((background-color . "#d0d0d0"))
  :disabled-style '((background-color . "#f5f5f5")
                    (color . "#999")
                    (cursor . "not-allowed"))
  :on-click (lambda (event)
              (let* ((target (plist-get event :target))
                     (state (plist-get target :state)))
                (unless (plist-get state :disabled)
                  ;; Allow custom click handler via attrs
                  (let* ((attrs (plist-get target :attrs))
                         (custom-handler (plist-get attrs :on-click)))
                    (when (functionp custom-handler)
                      (funcall custom-handler event)))))))

(define-etaf-etml-tag input
  :display 'inline-block
  :self-closing t
  :children-allowed nil
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")
                   (border . "1px solid #ccc"))
  :focus-style '((border-color . "blue")
                 (outline . "none"))
  :disabled-style '((background-color . "#f5f5f5")
                    (color . "#999")))

(define-etaf-etml-tag textarea
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")
                   (border . "1px solid #ccc")
                   (font-family . "inherit"))
  :focus-style '((border-color . "blue")
                 (outline . "none")))

(define-etaf-etml-tag select
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")
                   (border . "1px solid #ccc")))

(define-etaf-etml-tag option
  :display 'block)

(define-etaf-etml-tag label
  :display 'inline
  :default-style '((cursor . "pointer")))

(define-etaf-etml-tag form
  :display 'block)

(define-etaf-etml-tag fieldset
  :display 'block
  :default-style '((border . "1px solid #ccc")
                   (padding-top . "1lh")
                   (padding-bottom . "1lh")
                   (padding-left . "10px")
                   (padding-right . "10px")))

(define-etaf-etml-tag legend
  :display 'block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

;; Table elements
(define-etaf-etml-tag table
  :display 'table
  :default-style '((border-collapse . "collapse")))

(define-etaf-etml-tag thead
  :display 'table-header-group)

(define-etaf-etml-tag tbody
  :display 'table-row-group)

(define-etaf-etml-tag tfoot
  :display 'table-footer-group)

(define-etaf-etml-tag tr
  :display 'table-row)

(define-etaf-etml-tag th
  :display 'table-cell
  :default-style '((font-weight . "bold")
                   (text-align . "center")
                   (padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

(define-etaf-etml-tag td
  :display 'table-cell
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

(define-etaf-etml-tag caption
  :display 'table-caption)

;; Media elements
(define-etaf-etml-tag img
  :display 'inline-block
  :self-closing t
  :children-allowed nil)

(define-etaf-etml-tag video
  :display 'inline-block)

(define-etaf-etml-tag audio
  :display 'inline)

(define-etaf-etml-tag canvas
  :display 'inline-block)

(define-etaf-etml-tag svg
  :display 'inline-block)

;; Semantic elements
(define-etaf-etml-tag figure
  :display 'block
  :default-style '((margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (margin-left . "40px")
                   (margin-right . "40px")))

(define-etaf-etml-tag figcaption
  :display 'block)

(define-etaf-etml-tag details
  :display 'block)

(define-etaf-etml-tag summary
  :display 'list-item
  :default-style '((cursor . "pointer"))
  :on-click (lambda (event)
              (let* ((target (plist-get event :target))
                     (state (plist-get target :state)))
                ;; Toggle expanded state
                (plist-put state :expanded
                           (not (plist-get state :expanded))))))

(define-etaf-etml-tag dialog
  :display 'block
  :default-style '((position . "absolute")
                   (border . "1px solid #ccc")
                   (padding-top . "1lh")
                   (padding-bottom . "1lh")
                   (padding-left . "10px")
                   (padding-right . "10px")
                   (background-color . "white")))

(define-etaf-etml-tag progress
  :display 'inline-block
  :children-allowed nil)

(define-etaf-etml-tag meter
  :display 'inline-block
  :children-allowed nil)

;;; High-level API

(defun etaf-etml-tag-parse (sexp)
  "Parse SEXP into a tag instance.
SEXP format: (tag-name :attr1 val1 :attr2 val2 children...)
Returns a tag instance or the original sexp if not a valid tag."
  (when (and (listp sexp) (symbolp (car sexp)))
    (let* ((tag-name (car sexp))
           (rest (cdr sexp))
           (attrs nil)
           (children nil))
      ;; Check if this is a defined tag
      (if (etaf-etml-tag-defined-p tag-name)
          (progn
            ;; Parse attributes (keywords and their values)
            (while (and rest (keywordp (car rest)))
              (push (car rest) attrs)
              (setq rest (cdr rest))
              (when rest
                (push (car rest) attrs)
                (setq rest (cdr rest))))
            (setq attrs (nreverse attrs))
            ;; Rest are children
            (setq children
                  (mapcar (lambda (child)
                            (if (listp child)
                                (etaf-etml-tag-parse child)
                              child))
                          rest))
            ;; Create tag instance
            (etaf-etml-tag-create-instance tag-name attrs children))
        ;; Not a defined tag, return as-is
        sexp))))

(defun etaf-etml-tag-to-dom (sexp)
  "Convert SEXP with ETML tags to DOM format.
This parses the SEXP, resolves tag definitions, and returns standard DOM."
  (let ((parsed (etaf-etml-tag-parse sexp)))
    (if (and (listp parsed) (plist-get parsed :tag-name))
        (etaf-etml-tag-render-to-dom parsed)
      ;; Pass through if not a valid tag instance
      parsed)))

(provide 'etaf-etml-tag)
;;; etaf-etml-tag.el ends here
