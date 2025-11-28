;;; etaf-template.el --- Vue.js-like template syntax for TML -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: template, tml, vue, reactive
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; This library implements Vue.js-like template syntax for TML (Template Markup Language).
;; It provides reactive data binding and template directives similar to Vue.js.
;;
;; Core Features:
;; - Text interpolation: {{ expression }}
;; - Conditional rendering: v-if, v-else-if, v-else
;; - List rendering: v-for
;; - Attribute binding: :attr or v-bind:attr
;; - Text content: v-text
;; - Show/Hide: v-show
;;
;; Usage:
;;
;;   ;; Define reactive data
;;   (setq my-data '(:name "John" :count 5 :items ("a" "b" "c") :visible t))
;;
;;   ;; Create template with Vue-like syntax
;;   (setq my-template
;;     '(div
;;       (h1 "Hello, {{ name }}!")
;;       (p :v-if "visible" "This is visible")
;;       (p :v-else "This is hidden")
;;       (ul
;;         (li :v-for "item in items" "{{ item }}"))))
;;
;;   ;; Render template with data
;;   (etaf-template-render my-template my-data)
;;
;; The template will be converted to standard TML format that can be
;; processed by etaf-tml-to-dom.

;;; Code:

(require 'cl-lib)

;;; Text Interpolation (Mustache syntax)

(defun etaf-template--interpolate-string (str data)
  "Replace {{ expression }} patterns in STR with values from DATA.
DATA is a plist containing the template data."
  (if (not (stringp str))
      str
    (let ((result "")
          (pos 0))
      (while (string-match "{{\\s-*\\([^}]+?\\)\\s-*}}" str pos)
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (expr (match-string 1 str))
               (value (etaf-template--eval-expr expr data))
               (value-str (etaf-template--to-string value)))
          (setq result (concat result (substring str pos match-start) value-str))
          (setq pos match-end)))
      (concat result (substring str pos)))))

(defun etaf-template--to-string (value)
  "Convert VALUE to string for template output."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun etaf-template--eval-expr (expr data)
  "Evaluate expression EXPR using DATA.
EXPR can be:
- A simple variable name: \"name\"
- A property access: \"user.name\"
- A negation: \"!visible\"
- A number: \"42\"
- A string literal: \"'hello'\""
  (let ((expr (string-trim expr)))
    (cond
     ;; Empty expression
     ((string-empty-p expr) nil)
     
     ;; Boolean negation
     ((string-prefix-p "!" expr)
      (not (etaf-template--eval-expr (substring expr 1) data)))
     
     ;; Number literal
     ((string-match-p "^-?[0-9]+\\(\\.[0-9]+\\)?$" expr)
      (string-to-number expr))
     
     ;; String literal with single quotes
     ((string-match "^'\\(.*\\)'$" expr)
      (match-string 1 expr))
     
     ;; String literal with double quotes
     ((string-match "^\"\\(.*\\)\"$" expr)
      (match-string 1 expr))
     
     ;; Boolean literals
     ((string= expr "true") t)
     ((string= expr "false") nil)
     ((string= expr "nil") nil)
     
     ;; Property access (e.g., "user.name")
     ((string-match "\\." expr)
      (let* ((parts (split-string expr "\\."))
             (value data))
        (dolist (part parts value)
          (when value
            (let ((key (intern (concat ":" part))))
              (setq value (plist-get value key)))))))
     
     ;; Simple variable lookup
     (t
      (let ((key (intern (concat ":" expr))))
        (plist-get data key))))))

;;; Directive Parsing

(defun etaf-template--parse-v-for (expr)
  "Parse v-for expression EXPR.
Returns (ITEM-VAR . COLLECTION-EXPR) or (ITEM-VAR INDEX-VAR . COLLECTION-EXPR).
Supports:
- \"item in items\"
- \"(item, index) in items\""
  (cond
   ;; Format: (item, index) in collection
   ((string-match "(\\s-*\\([^,]+\\)\\s-*,\\s-*\\([^)]+\\)\\s-*)\\s-+in\\s-+\\(.+\\)" expr)
    (list (string-trim (match-string 1 expr))
          (string-trim (match-string 2 expr))
          (string-trim (match-string 3 expr))))
   
   ;; Format: item in collection
   ((string-match "\\([^[:space:]]+\\)\\s-+in\\s-+\\(.+\\)" expr)
    (list (string-trim (match-string 1 expr))
          nil
          (string-trim (match-string 2 expr))))
   
   (t (error "Invalid v-for expression: %s" expr))))

(defun etaf-template--truthy-p (value)
  "Check if VALUE is truthy (non-nil and non-empty)."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   ((numberp value) (not (zerop value)))
   (t t)))

;;; Template Rendering

(defun etaf-template--get-attr (attrs key)
  "Get attribute KEY from ATTRS (plist format)."
  (plist-get attrs key))

(defun etaf-template--remove-attr (attrs key)
  "Remove attribute KEY from ATTRS (plist format)."
  (let ((result nil)
        (rest attrs))
    (while rest
      (let ((k (car rest))
            (v (cadr rest)))
        (unless (eq k key)
          (setq result (append result (list k v)))))
      (setq rest (cddr rest)))
    result))

(defun etaf-template--split-attrs-and-children (rest)
  "Split REST into (ATTRS . CHILDREN) where ATTRS is a plist.
Handles boolean directives like :v-else that don't take a value."
  (let ((attrs nil)
        (children nil)
        (boolean-directives '(:v-else)))  ; Directives that don't take a value
    (while (and rest (keywordp (car rest)))
      (let ((key (car rest)))
        (push key attrs)
        (setq rest (cdr rest))
        (cond
         ;; Boolean directive - use t as value
         ((member key boolean-directives)
          (push t attrs))
         ;; Regular attribute - consume the value
         (rest
          (push (car rest) attrs)
          (setq rest (cdr rest))))))
    (setq attrs (nreverse attrs))
    (setq children rest)
    (cons attrs children)))

(defun etaf-template--process-bindings (attrs data)
  "Process attribute bindings in ATTRS using DATA.
Converts :attr to attr with evaluated value.
Returns processed attrs plist."
  (let ((result nil)
        (rest attrs))
    (while rest
      (let* ((key (car rest))
             (val (cadr rest))
             (key-name (symbol-name key)))
        (cond
         ;; v-bind:attr="expr" or :attr="expr" binding
         ((or (string-prefix-p ":v-bind:" key-name)
              (and (string-prefix-p ":" key-name)
                   (not (string-prefix-p ":v-" key-name))
                   (> (length key-name) 1)
                   ;; Check if it looks like a binding (has double colons)
                   (string-match-p "^::" key-name)))
          (let* ((attr-name (if (string-prefix-p ":v-bind:" key-name)
                                (substring key-name 8)
                              (substring key-name 2)))
                 (new-key (intern (concat ":" attr-name)))
                 (new-val (etaf-template--to-string
                          (etaf-template--eval-expr val data))))
            (setq result (append result (list new-key new-val)))))
         
         ;; Regular attribute
         (t
          (setq result (append result (list key val))))))
      (setq rest (cddr rest)))
    result))

(defun etaf-template--find-else-sibling (siblings)
  "Find v-else or v-else-if sibling from SIBLINGS list.
Returns (MATCHING-SIBLING . REMAINING-SIBLINGS) or nil."
  (when siblings
    (let ((first (car siblings)))
      (when (and (listp first) (not (stringp first)))
        (let ((parsed (etaf-template--split-attrs-and-children (cdr first))))
          (when (or (etaf-template--get-attr (car parsed) :v-else)
                    (etaf-template--get-attr (car parsed) :v-else-if))
            (cons first (cdr siblings))))))))

(defun etaf-template-render (template data)
  "Render TEMPLATE with DATA.
TEMPLATE is TML with Vue-like directives.
DATA is a plist with template variables.
Returns standard TML format."
  (let ((result (etaf-template--render-node template data nil)))
    ;; Result is (nodes . skip-count), we return the first node
    (car (car result))))

(defun etaf-template--render-node (node data siblings)
  "Render NODE with DATA, considering SIBLINGS for v-else directives.
Returns (RENDERED-NODES . SKIP-COUNT) where SKIP-COUNT is siblings to skip."
  (cond
   ;; String - interpolate
   ((stringp node)
    (cons (list (etaf-template--interpolate-string node data)) 0))
   
   ;; Atom - return as is
   ((atom node)
    (cons (list node) 0))
   
   ;; List (element)
   (t
    (let* ((tag (car node))
           (rest (cdr node))
           (parsed (etaf-template--split-attrs-and-children rest))
           (attrs (car parsed))
           (children (cdr parsed)))
      
      ;; Check for v-for (list rendering)
      (if-let ((v-for (etaf-template--get-attr attrs :v-for)))
          (let* ((for-parsed (etaf-template--parse-v-for v-for))
                 (item-var (nth 0 for-parsed))
                 (index-var (nth 1 for-parsed))
                 (collection-expr (nth 2 for-parsed))
                 (collection (etaf-template--eval-expr collection-expr data))
                 (new-attrs (etaf-template--remove-attr attrs :v-for))
                 (results nil)
                 (index 0))
            (dolist (item (if (listp collection) collection (list collection)))
              (let* ((item-key (intern (concat ":" item-var)))
                     (new-data (copy-sequence data)))
                (setq new-data (plist-put new-data item-key item))
                (when index-var
                  (let ((idx-key (intern (concat ":" index-var))))
                    (setq new-data (plist-put new-data idx-key index))))
                (let* ((new-node (cons tag (append new-attrs children)))
                       (rendered (etaf-template--render-node new-node new-data nil)))
                  (setq results (append results (car rendered)))))
              (cl-incf index))
            (cons results 0))
        
        ;; Check for v-if (conditional rendering)
        (if-let ((v-if (etaf-template--get-attr attrs :v-if)))
            (let ((condition (etaf-template--eval-expr v-if data)))
              (if (etaf-template--truthy-p condition)
                  ;; Condition true - render this node, skip else siblings
                  (let* ((new-attrs (etaf-template--remove-attr attrs :v-if))
                         (skip-count 0))
                    ;; Count consecutive v-else-if and v-else siblings to skip
                    (let ((remaining siblings))
                      (while (etaf-template--find-else-sibling remaining)
                        (cl-incf skip-count)
                        (setq remaining (cdr remaining))))
                    (let ((rendered (etaf-template--render-element
                                   tag new-attrs children data)))
                      (cons rendered skip-count)))
                ;; Condition false - check for v-else-if or v-else
                (let ((else-info (etaf-template--find-else-sibling siblings)))
                  (if else-info
                      (let* ((else-node (car else-info))
                             (else-tag (car else-node))
                             (else-parsed (etaf-template--split-attrs-and-children
                                          (cdr else-node)))
                             (else-attrs (car else-parsed))
                             (else-children (cdr else-parsed))
                             (remaining-siblings (cdr else-info)))
                        (if-let ((else-if-expr (etaf-template--get-attr
                                               else-attrs :v-else-if)))
                            ;; Process v-else-if
                            (let* ((new-else-attrs
                                   (etaf-template--remove-attr else-attrs :v-else-if))
                                   (new-else-attrs
                                   (append (list :v-if else-if-expr) new-else-attrs))
                                   (new-else-node
                                   (cons else-tag
                                         (append new-else-attrs else-children)))
                                   ;; Render the v-else-if with its remaining siblings
                                   (result (etaf-template--render-node
                                           new-else-node data remaining-siblings)))
                              ;; Skip count is 1 (for this v-else-if) plus what it skips
                              (cons (car result) (+ 1 (cdr result))))
                          ;; Process v-else - render it directly (strip v-else attr)
                          (let* ((new-else-attrs (etaf-template--remove-attr
                                                 else-attrs :v-else))
                                 (rendered (etaf-template--render-element
                                           else-tag new-else-attrs else-children data)))
                            (cons rendered 1))))
                    ;; No else sibling - render nothing
                    (cons nil 0)))))
          
          ;; Check for v-else-if without preceding v-if (standalone)
          (if-let ((v-else-if (etaf-template--get-attr attrs :v-else-if)))
              ;; This shouldn't normally happen, treat as v-if
              (let* ((new-attrs (etaf-template--remove-attr attrs :v-else-if))
                     (new-attrs (append (list :v-if v-else-if) new-attrs))
                     (new-node (cons tag (append new-attrs children))))
                (etaf-template--render-node new-node data siblings))
            
            ;; Check for v-else without preceding v-if (standalone)
            (if (etaf-template--get-attr attrs :v-else)
                ;; Render as-is (remove v-else attr)
                (let* ((new-attrs (etaf-template--remove-attr attrs :v-else))
                       (rendered (etaf-template--render-element
                                 tag new-attrs children data)))
                  (cons rendered 0))
              
              ;; Check for v-show (visibility)
              (if-let ((v-show (etaf-template--get-attr attrs :v-show)))
                  (let ((visible (etaf-template--eval-expr v-show data))
                        (new-attrs (etaf-template--remove-attr attrs :v-show)))
                    (if (etaf-template--truthy-p visible)
                        (let ((rendered (etaf-template--render-element
                                        tag new-attrs children data)))
                          (cons rendered 0))
                      ;; Add display: none style
                      (let* ((style (etaf-template--get-attr new-attrs :style))
                             (new-style (if style
                                           (concat style "; display: none")
                                         "display: none"))
                             (new-attrs (etaf-template--remove-attr new-attrs :style))
                             (new-attrs (append new-attrs (list :style new-style)))
                             (rendered (etaf-template--render-element
                                       tag new-attrs children data)))
                        (cons rendered 0))))
                
                ;; Check for v-text
                (if-let ((v-text (etaf-template--get-attr attrs :v-text)))
                    (let* ((text-value (etaf-template--to-string
                                       (etaf-template--eval-expr v-text data)))
                           (new-attrs (etaf-template--remove-attr attrs :v-text))
                           (rendered (etaf-template--render-element
                                     tag new-attrs (list text-value) data)))
                      (cons rendered 0))
                  
                  ;; No special directives - render normally
                  (let ((rendered (etaf-template--render-element
                                  tag attrs children data)))
                    (cons rendered 0))))))))))))

(defun etaf-template--render-element (tag attrs children data)
  "Render element with TAG, ATTRS, CHILDREN using DATA.
Returns list of rendered nodes (usually just one)."
  ;; Process attribute bindings
  (let* ((processed-attrs (etaf-template--process-bindings attrs data))
         ;; Interpolate string attribute values
         (final-attrs nil))
    (let ((rest processed-attrs))
      (while rest
        (let ((key (car rest))
              (val (cadr rest)))
          (setq final-attrs
                (append final-attrs
                        (list key
                              (if (stringp val)
                                  (etaf-template--interpolate-string val data)
                                val)))))
        (setq rest (cddr rest))))
    
    ;; Render children
    (let ((rendered-children nil)
          (child-list children)
          (skip-count 0))
      (while child-list
        (if (> skip-count 0)
            (progn
              (cl-decf skip-count)
              (setq child-list (cdr child-list)))
          (let* ((child (car child-list))
                 (remaining (cdr child-list))
                 (result (etaf-template--render-node child data remaining)))
            (setq rendered-children (append rendered-children (car result)))
            (setq skip-count (cdr result))
            (setq child-list (cdr child-list)))))
      
      ;; Build final element
      (list (if final-attrs
                (append (list tag) final-attrs rendered-children)
              (cons tag rendered-children))))))

;;; Convenience Functions

(defun etaf-template-to-dom (template data)
  "Render TEMPLATE with DATA and convert to DOM.
This is a convenience function combining template rendering and TML-to-DOM."
  (require 'etaf-tml)
  (etaf-tml-to-dom (etaf-template-render template data)))

;;; Reactive Data System

(defvar etaf-template--watchers (make-hash-table :test 'eq)
  "Hash table mapping data objects to their watchers.")

(defun etaf-template-create-reactive (data)
  "Create a reactive data wrapper around DATA plist.
Returns a reactive data object that can trigger re-renders."
  (let ((reactive (list :data data
                       :version 0
                       :watchers nil)))
    reactive))

(defun etaf-template-get (reactive key)
  "Get value for KEY from REACTIVE data object."
  (plist-get (plist-get reactive :data) key))

(defun etaf-template-set (reactive key value)
  "Set KEY to VALUE in REACTIVE data object and trigger watchers."
  (let* ((data (plist-get reactive :data))
         (new-data (plist-put data key value)))
    (plist-put reactive :data new-data)
    (plist-put reactive :version (1+ (plist-get reactive :version)))
    ;; Trigger watchers
    (dolist (watcher (plist-get reactive :watchers))
      (funcall watcher reactive key value))
    value))

(defun etaf-template-watch (reactive callback)
  "Add CALLBACK as watcher to REACTIVE data.
CALLBACK receives (reactive key value) when data changes."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (cons callback watchers))))

(defun etaf-template-unwatch (reactive callback)
  "Remove CALLBACK from REACTIVE data watchers."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (delete callback watchers))))

(provide 'etaf-template)
;;; etaf-template.el ends here
