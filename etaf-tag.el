;;; etaf-tag.el --- ETML Tag Definition System -*- lexical-binding: t; -*-

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
;; - `define-etaf-tag': Macro for defining custom tags
;; - Built-in tags: div, span, button, input, a, p, h1-h6, etc.
;; - Event handlers: on-click, on-hover, on-focus, on-change, etc.
;; - Style inheritance: Tags can inherit styles from parent tags
;;
;; Usage Example:
;;
;;   ;; Define a custom button tag
;;   (define-etaf-tag my-button
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

(defvar etaf-tag-definitions (make-hash-table :test 'eq)
  "Hash table storing all defined ETML tag definitions.
Keys are tag symbols, values are tag definition plists.")

(defvar etaf-tag-instances nil
  "List of active tag instances in the current buffer.")

;;; Tag Definition Structure

(defun etaf-tag-create-definition (name &rest props)
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

(defun etaf-tag-get-definition (name)
  "Get the tag definition for NAME."
  (gethash name etaf-tag-definitions))

(defun etaf-tag-register (name definition)
  "Register a tag DEFINITION with NAME."
  (puthash name definition etaf-tag-definitions))

(defun etaf-tag-unregister (name)
  "Unregister the tag with NAME."
  (remhash name etaf-tag-definitions))

(defun etaf-tag-defined-p (name)
  "Check if a tag with NAME is defined."
  (not (null (gethash name etaf-tag-definitions))))

(defun etaf-tag-list-all ()
  "Return a list of all defined tag names."
  (let ((tags nil))
    (maphash (lambda (key _value) (push key tags))
             etaf-tag-definitions)
    (nreverse tags)))

;;; Style Merging

(defun etaf-tag--merge-styles (base-style override-style)
  "Merge OVERRIDE-STYLE into BASE-STYLE.
Returns a new alist with all properties from both, with override taking precedence."
  (let ((result (copy-alist (or base-style '()))))
    (dolist (pair (or override-style '()))
      (let ((existing (assq (car pair) result)))
        (if existing
            (setcdr existing (cdr pair))
          (push pair result))))
    result))

(defun etaf-tag--resolve-inherited-style (tag-name)
  "Resolve the complete style for TAG-NAME including inherited styles."
  (let* ((definition (etaf-tag-get-definition tag-name))
         (inherit (plist-get definition :inherit))
         (own-style (plist-get definition :default-style)))
    (if (and inherit (etaf-tag-defined-p inherit))
        (etaf-tag--merge-styles
         (etaf-tag--resolve-inherited-style inherit)
         own-style)
      own-style)))

;;; Event Handling

(defun etaf-tag--make-event (type target &optional extra-data)
  "Create an event object with TYPE and TARGET.
EXTRA-DATA is an optional alist of additional event properties."
  (append (list :type type
                :target target
                :timestamp (current-time))
          extra-data))

(defun etaf-tag--dispatch-event (tag-instance event-type &optional extra-data)
  "Dispatch an event of EVENT-TYPE for TAG-INSTANCE."
  (let* ((definition (plist-get tag-instance :definition))
         (handler-key (intern (concat ":on-" (symbol-name event-type))))
         (handler (plist-get definition handler-key)))
    (when (functionp handler)
      (let ((event (etaf-tag--make-event event-type tag-instance extra-data)))
        (funcall handler event)))))

;;; Interactive Behaviors

(defun etaf-tag-setup-keymap (tag-instance)
  "Set up keybindings for TAG-INSTANCE."
  (let* ((keymap (make-sparse-keymap))
         (definition (plist-get tag-instance :definition)))
    ;; Click handler via RET
    (when (plist-get definition :on-click)
      (define-key keymap (kbd "RET")
        (lambda ()
          (interactive)
          (etaf-tag--dispatch-event tag-instance 'click))))
    ;; Keydown handlers
    (when (plist-get definition :on-keydown)
      (define-key keymap [t]
        (lambda ()
          (interactive)
          (let ((key (this-command-keys-vector)))
            (etaf-tag--dispatch-event tag-instance 'keydown
                                      (list :key key))))))
    keymap))

(defun etaf-tag-make-interactive (start end tag-instance)
  "Make the region from START to END interactive based on TAG-INSTANCE."
  (let* ((definition (plist-get tag-instance :definition))
         (keymap (etaf-tag-setup-keymap tag-instance)))
    ;; Add text properties for interactivity
    (add-text-properties start end
                         (list 'etaf-tag-instance tag-instance
                               'keymap keymap
                               'mouse-face 'highlight
                               'help-echo (format "Click or press RET to activate %s"
                                                  (plist-get definition :name))))
    ;; Set up mouse click handler
    (when (plist-get definition :on-click)
      (add-text-properties start end
                           (list 'local-map
                                 (let ((map (make-sparse-keymap)))
                                   (define-key map [mouse-1]
                                     (lambda (_event)
                                       (interactive "e")
                                       (etaf-tag--dispatch-event tag-instance 'click)))
                                   map))))))

;;; Tag Instance Creation

(defun etaf-tag-create-instance (tag-name attrs children)
  "Create a tag instance with TAG-NAME, ATTRS, and CHILDREN."
  (let* ((definition (etaf-tag-get-definition tag-name))
         (instance (list :tag-name tag-name
                         :definition definition
                         :attrs attrs
                         :children children
                         :state (list :hovered nil
                                      :focused nil
                                      :active nil
                                      :disabled (plist-get attrs :disabled)))))
    ;; Register instance
    (push instance etaf-tag-instances)
    instance))

;;; Tag Definition Macro

;;;###autoload
(defmacro define-etaf-tag (name &rest props)
  "Define an ETML tag with NAME and PROPS.

NAME is the tag symbol (e.g., button, div, my-component).

PROPS is a plist that can include:
- :display - Display type (`block', `inline', `inline-block', `flex', `none')
- :default-style - Default style alist ((property . value) ...)
- :hover-style - Style when hovered
- :active-style - Style when active/pressed
- :focus-style - Style when focused
- :disabled-style - Style when disabled
- :on-click - Click handler function (lambda (event) ...)
- :on-hover-enter - Mouse enter handler
- :on-hover-leave - Mouse leave handler
- :on-focus - Focus handler
- :on-blur - Blur handler
- :on-change - Change handler (for inputs)
- :on-input - Input handler
- :on-keydown - Keydown handler
- :on-keyup - Keyup handler
- :children-allowed - Whether children are allowed (default t)
- :self-closing - Whether self-closing (default nil)
- :inherit - Parent tag to inherit from
- :render - Custom render function

Example:
  (define-etaf-tag button
    :display \\='inline-block
    :default-style \\='((padding . \"5px 10px\")
                      (cursor . \"pointer\"))
    :hover-style \\='((background-color . \"#e0e0e0\"))
    :on-click (lambda (event)
                (message \"Button clicked!\")))"
  (declare (indent defun))
  (let ((definition-var (intern (format "etaf-tag--%s-definition" name))))
    `(progn
       ;; Create the definition
       (defvar ,definition-var
         (etaf-tag-create-definition ',name ,@props)
         ,(format "Tag definition for %s." name))
       ;; Register the tag
       (etaf-tag-register ',name ,definition-var)
       ;; Return the tag name
       ',name)))

;;; Tag Rendering

(defun etaf-tag-get-computed-style (tag-instance)
  "Get the computed style for TAG-INSTANCE based on its current state."
  (let* ((definition (plist-get tag-instance :definition))
         (state (plist-get tag-instance :state))
         (attrs (plist-get tag-instance :attrs))
         ;; Start with inherited + default style
         (base-style (etaf-tag--resolve-inherited-style
                      (plist-get tag-instance :tag-name)))
         ;; Apply inline styles from attrs
         (inline-style (plist-get attrs :style))
         (style (etaf-tag--merge-styles base-style
                                        (when inline-style
                                          (if (stringp inline-style)
                                              (etaf-tag--parse-style-string inline-style)
                                            inline-style)))))
    ;; Apply state-based styles
    (when (plist-get state :hovered)
      (setq style (etaf-tag--merge-styles style
                                          (plist-get definition :hover-style))))
    (when (plist-get state :focused)
      (setq style (etaf-tag--merge-styles style
                                          (plist-get definition :focus-style))))
    (when (plist-get state :active)
      (setq style (etaf-tag--merge-styles style
                                          (plist-get definition :active-style))))
    (when (plist-get state :disabled)
      (setq style (etaf-tag--merge-styles style
                                          (plist-get definition :disabled-style))))
    style))

(defun etaf-tag--parse-style-string (style-string)
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

(defun etaf-tag-render-to-dom (tag-instance)
  "Render TAG-INSTANCE to DOM format.
Returns (tag-name ((attrs...)) children...)."
  (let* ((tag-name (plist-get tag-instance :tag-name))
         (definition (plist-get tag-instance :definition))
         (attrs (plist-get tag-instance :attrs))
         (children (plist-get tag-instance :children))
         (custom-render (plist-get definition :render))
         (computed-style (etaf-tag-get-computed-style tag-instance)))
    ;; Use custom render if provided
    (if custom-render
        (funcall custom-render tag-instance)
      ;; Default rendering
      (let ((dom-attrs '()))
        ;; Convert computed style to style attribute
        (when computed-style
          (push (cons 'style (etaf-tag--style-alist-to-string computed-style))
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
        (cons tag-name (cons (nreverse dom-attrs)
                             (mapcar (lambda (child)
                                       (if (and (listp child)
                                                (plist-get child :tag-name))
                                           (etaf-tag-render-to-dom child)
                                         child))
                                     children)))))))

(defun etaf-tag--style-alist-to-string (style-alist)
  "Convert STYLE-ALIST to CSS style string."
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             style-alist "; "))

;;; Built-in Tags

;; Block-level tags
(define-etaf-tag div
  :display 'block
  :default-style nil)

(define-etaf-tag p
  :display 'block
  :default-style '((margin-left . "5px")))

(define-etaf-tag h1
  :display 'block
  :default-style '((font-size . 1.6)
                   (font-weight . "bold")))

(define-etaf-tag h2
  :display 'block
  :default-style '((font-size . 1.4)
                   (font-weight . "bold")))

(define-etaf-tag h3
  :display 'block
  :default-style '((font-size . 1.3)
                   (font-weight . "bold")))

(define-etaf-tag h4
  :display 'block
  :default-style '((font-size . 1.2)
                   (font-weight . "bold")))

(define-etaf-tag h5
  :display 'block
  :default-style '((font-size . 1.1)
                   (font-weight . "bold")))

(define-etaf-tag h6
  :display 'block
  :default-style '((font-size . 1.0)
                   (font-weight . "bold")))

(define-etaf-tag header
  :display 'block)

(define-etaf-tag footer
  :display 'block)

(define-etaf-tag section
  :display 'block)

(define-etaf-tag article
  :display 'block)

(define-etaf-tag aside
  :display 'block)

(define-etaf-tag nav
  :display 'block)

(define-etaf-tag main
  :display 'block)

(define-etaf-tag ul
  :display 'block
  :default-style '((list-style-type . "disc")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (padding-left . "40px")))

(define-etaf-tag ol
  :display 'block
  :default-style '((list-style-type . "decimal")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (padding-left . "40px")))

(define-etaf-tag li
  :display 'list-item)

(define-etaf-tag blockquote
  :display 'block
  :default-style '((margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (margin-left . "40px")
                   (margin-right . "40px")))

(define-etaf-tag pre
  :display 'block
  :default-style '((font-family . "monospace")
                   (white-space . "pre")))

(define-etaf-tag hr
  :display 'block
  :self-closing t
  :children-allowed nil
  :default-style '((border-top . "1px solid")
                   (margin-top . "1lh")
                   (margin-bottom . "1lh")))

;; Inline tags
(define-etaf-tag span
  :display 'inline)

(define-etaf-tag a
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

(define-etaf-tag em
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-tag strong
  :display 'inline
  :default-style '((font-weight . "bold")))

(define-etaf-tag b
  :display 'inline
  :default-style '((font-weight . "bold")))

(define-etaf-tag i
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-tag u
  :display 'inline
  :default-style '((text-decoration . "underline")))

(define-etaf-tag s
  :display 'inline
  :default-style '((text-decoration . "line-through")))

(define-etaf-tag del
  :display 'inline
  :default-style '((text-decoration . "line-through")))

(define-etaf-tag ins
  :display 'inline
  :default-style '((text-decoration . "underline")))

(define-etaf-tag mark
  :display 'inline
  :default-style '((background-color . "yellow")))

(define-etaf-tag small
  :display 'inline
  :default-style '((font-size . "smaller")))

(define-etaf-tag sub
  :display 'inline
  :default-style '((vertical-align . "sub")
                   (font-size . "smaller")))

(define-etaf-tag sup
  :display 'inline
  :default-style '((vertical-align . "super")
                   (font-size . "smaller")))

(define-etaf-tag code
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-tag kbd
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-tag samp
  :display 'inline
  :default-style '((font-family . "monospace")))

(define-etaf-tag var
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-tag abbr
  :display 'inline
  :default-style '((text-decoration . "dotted underline")))

(define-etaf-tag cite
  :display 'inline
  :default-style '((font-style . "italic")))

(define-etaf-tag q
  :display 'inline)

(define-etaf-tag br
  :display 'inline
  :self-closing t
  :children-allowed nil)

;; Form elements
(define-etaf-tag button
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "10px")
                   (padding-right . "10px")
                   (border . "1px solid #ccc")
                   (background-color . "#f0f0f0")
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

(define-etaf-tag input
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

(define-etaf-tag textarea
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")
                   (border . "1px solid #ccc")
                   (font-family . "inherit"))
  :focus-style '((border-color . "blue")
                 (outline . "none")))

(define-etaf-tag select
  :display 'inline-block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")
                   (border . "1px solid #ccc")))

(define-etaf-tag option
  :display 'block)

(define-etaf-tag label
  :display 'inline
  :default-style '((cursor . "pointer")))

(define-etaf-tag form
  :display 'block)

(define-etaf-tag fieldset
  :display 'block
  :default-style '((border . "1px solid #ccc")
                   (padding-top . "1lh")
                   (padding-bottom . "1lh")
                   (padding-left . "10px")
                   (padding-right . "10px")))

(define-etaf-tag legend
  :display 'block
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

;; Table elements
(define-etaf-tag table
  :display 'table
  :default-style '((border-collapse . "collapse")))

(define-etaf-tag thead
  :display 'table-header-group)

(define-etaf-tag tbody
  :display 'table-row-group)

(define-etaf-tag tfoot
  :display 'table-footer-group)

(define-etaf-tag tr
  :display 'table-row)

(define-etaf-tag th
  :display 'table-cell
  :default-style '((font-weight . "bold")
                   (text-align . "center")
                   (padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

(define-etaf-tag td
  :display 'table-cell
  :default-style '((padding-top . "0lh")
                   (padding-bottom . "0lh")
                   (padding-left . "5px")
                   (padding-right . "5px")))

(define-etaf-tag caption
  :display 'table-caption)

;; Media elements
(define-etaf-tag img
  :display 'inline-block
  :self-closing t
  :children-allowed nil)

(define-etaf-tag video
  :display 'inline-block)

(define-etaf-tag audio
  :display 'inline)

(define-etaf-tag canvas
  :display 'inline-block)

(define-etaf-tag svg
  :display 'inline-block)

;; Semantic elements
(define-etaf-tag figure
  :display 'block
  :default-style '((margin-top . "1lh")
                   (margin-bottom . "1lh")
                   (margin-left . "40px")
                   (margin-right . "40px")))

(define-etaf-tag figcaption
  :display 'block)

(define-etaf-tag details
  :display 'block)

(define-etaf-tag summary
  :display 'list-item
  :default-style '((cursor . "pointer"))
  :on-click (lambda (event)
              (let* ((target (plist-get event :target))
                     (state (plist-get target :state)))
                ;; Toggle expanded state
                (plist-put state :expanded
                           (not (plist-get state :expanded))))))

(define-etaf-tag dialog
  :display 'block
  :default-style '((position . "absolute")
                   (border . "1px solid #ccc")
                   (padding-top . "1lh")
                   (padding-bottom . "1lh")
                   (padding-left . "10px")
                   (padding-right . "10px")
                   (background-color . "white")))

(define-etaf-tag progress
  :display 'inline-block
  :children-allowed nil)

(define-etaf-tag meter
  :display 'inline-block
  :children-allowed nil)

;;; High-level API

(defun etaf-tag-parse (sexp)
  "Parse SEXP into a tag instance.
SEXP format: (tag-name :attr1 val1 :attr2 val2 children...)
Returns a tag instance or the original sexp if not a valid tag."
  (when (and (listp sexp) (symbolp (car sexp)))
    (let* ((tag-name (car sexp))
           (rest (cdr sexp))
           (attrs nil)
           (children nil))
      ;; Check if this is a defined tag
      (if (etaf-tag-defined-p tag-name)
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
                                (etaf-tag-parse child)
                              child))
                          rest))
            ;; Create tag instance
            (etaf-tag-create-instance tag-name attrs children))
        ;; Not a defined tag, return as-is
        sexp))))

(defun etaf-tag-to-dom (sexp)
  "Convert SEXP with ETML tags to DOM format.
This parses the SEXP, resolves tag definitions, and returns standard DOM."
  (let ((parsed (etaf-tag-parse sexp)))
    (if (and (listp parsed) (plist-get parsed :tag-name))
        (etaf-tag-render-to-dom parsed)
      ;; Pass through if not a valid tag instance
      parsed)))

(provide 'etaf-tag)
;;; etaf-tag.el ends here
