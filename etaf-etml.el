;;; etaf-etml.el --- TML (Template Markup Language) to DOM conversion -*- lexical-binding: t; -*-

;;; Commentary:

;; TML is an S-expression based markup language for Emacs.
;; This module converts TML to DOM format.
;;
;; Key features:
;; - Converts plist-based TML to alist-based DOM
;; - Supports :style attribute for inline CSS rules (Emacs-specific)
;;
;; Example:
;;   ;; String format (standard CSS)
;;   (div :class "box" :style "background: red; padding: 10px"
;;     "Hello")
;;
;;   ;; List format (alist of CSS properties)
;;   (div :class "box" :style ((background . "red") (padding . "10px"))
;;     "Hello")
;;
;; The :style attribute accepts:
;; - A string of CSS properties: "property1: value1; property2: value2"
;; - An alist of CSS properties: ((property . value) ...)
;; - Both are converted to a style attribute string in the DOM

;;; Utility functions

(require 'cl-lib)
(require 'etaf-tag)

(defun etaf-plist-to-alist (plist)
  "Convert a plist to an alist.
Example: (:class \"foo\" :id \"bar\")
         => ((class . \"foo\") (id . \"bar\"))"
  (when plist
    (let ((result nil))
      (while plist
        (let ((key (pop plist))
              (value (pop plist)))
          (push (cons (intern (substring (symbol-name key) 1)) value)
                result)))
      (nreverse result))))

(defun etaf-css-alist-to-string (css-alist)
  "Convert CSS alist to style string.
CSS-ALIST is ((property . value) ...).
Returns a string like \"property1: value1; property2: value2\"."
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             css-alist "; "))

(defun etaf-etml--merge-tag-styles (tag attr-alist)
  "Merge etaf-tag default styles into ATTR-ALIST for TAG.
If TAG is defined in etaf-tag, its default style is used as the base,
and any inline :style in ATTR-ALIST overrides the default.
Returns the updated ATTR-ALIST with merged style attribute."
  (when (etaf-tag-defined-p tag)
    (let* ((tag-def (etaf-tag-get-definition tag))
           (default-style (plist-get tag-def :default-style)))
      (when default-style
        (let* ((style-attr (assq 'style attr-alist))
               (inline-style (when style-attr
                               (if (stringp (cdr style-attr))
                                   (etaf-tag--parse-style-string (cdr style-attr))
                                 (cdr style-attr))))
               ;; Merge: default-style as base, inline-style overrides
               (merged-style (etaf-tag--merge-styles default-style inline-style)))
          (if merged-style
              (let ((merged-string (etaf-css-alist-to-string merged-style)))
                (if style-attr
                    (setcdr style-attr merged-string)
                  ;; Add style attribute if not present
                  (setq attr-alist (cons (cons 'style merged-string) attr-alist))))
            attr-alist)))))
  attr-alist)

(defun etaf-etml--to-dom (sexp)
  "Convert S-expression from format 1 (plist) to format 2 (alist).
Format 1: (tag :attr1 val1 :attr2 val2 child1 child2 ...)
Format 2: (tag ((attr1 . val1) (attr2 . val2)) child1 child2 ...)

Supports :style attribute for inline CSS rules in two formats:
  String format:
    (div :style \"background: red; padding: 10px\" ...)
  List format:
    (div :style ((background . \"red\") (padding . \"10px\")) ...)
Both are converted to:
  (div ((style . \"background: red; padding: 10px\")) ...)

If the tag is defined in etaf-tag, its default styles are merged with
inline styles, where inline styles take precedence."
  (cond
   ;; If it's an atom (string, number, etc.), return as is
   ((atom sexp) sexp)
   ;; If it's a list, process it
   (t (let ((tag (car sexp))
            (rest (cdr sexp))
            (attrs nil))
        (while (and rest (keywordp (car rest)))
          (push (car rest) attrs)
          (setq rest (cdr rest))
          (when rest
            (push (car rest) attrs)
            (setq rest (cdr rest))))
        (setq attrs (nreverse attrs))
        ;; Process :style attribute - handle both string and list formats
        (let* ((attr-alist (etaf-plist-to-alist attrs))
               (style-attr (assq 'style attr-alist)))
          ;; If :style is present and is a list (alist), convert it to string
          (when (and style-attr (listp (cdr style-attr)))
            (let ((style-string (etaf-css-alist-to-string (cdr style-attr))))
              (setcdr style-attr style-string)))
          ;; Merge etaf-tag default styles if tag is defined
          (setq attr-alist (etaf-etml--merge-tag-styles tag attr-alist))
          (let ((children (mapcar #'etaf-etml-to-dom rest)))
            (cons tag (cons attr-alist children))))))))

;;; Text Interpolation (Mustache syntax)

(defun etaf-etml--interpolate-string (str data)
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
               (value (etaf-etml--eval-expr expr data))
               (value-str (etaf-etml--to-string value)))
          (setq result (concat result (substring str pos match-start) value-str))
          (setq pos match-end)))
      (concat result (substring str pos)))))

(defun etaf-etml--to-string (value)
  "Convert VALUE to string for template output."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun etaf-etml--eval-expr (expr data)
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
      (not (etaf-etml--eval-expr (substring expr 1) data)))
     
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

(defun etaf-etml--parse-v-for (expr)
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

(defun etaf-etml--truthy-p (value)
  "Check if VALUE is truthy (non-nil and non-empty)."
  (cond
   ((null value) nil)
   ((eq value t) t)
   ((stringp value) (not (string-empty-p value)))
   ((listp value) (not (null value)))
   ((numberp value) (not (zerop value)))
   (t t)))

;;; Template Rendering

(defun etaf-etml--get-attr (attrs key)
  "Get attribute KEY from ATTRS (plist format)."
  (plist-get attrs key))

(defun etaf-etml--get-directive (attrs e-key v-key)
  "Get directive from ATTRS checking both E-KEY (preferred) and V-KEY (backward compat).
Returns the value of the first matching key found."
  (or (plist-get attrs e-key)
      (plist-get attrs v-key)))

(defun etaf-etml--has-directive-p (attrs e-key v-key)
  "Check if ATTRS has either E-KEY or V-KEY directive."
  (or (plist-member attrs e-key)
      (plist-member attrs v-key)))

(defun etaf-etml--remove-directive (attrs e-key v-key)
  "Remove both E-KEY and V-KEY directives from ATTRS."
  (let ((result (etaf-etml--remove-attr attrs e-key)))
    (etaf-etml--remove-attr result v-key)))

(defun etaf-etml--remove-attr (attrs key)
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

(defun etaf-etml--split-attrs-and-children (rest)
  "Split REST into (ATTRS . CHILDREN) where ATTRS is a plist.
Handles boolean directives like :e-else/:v-else that don't take a value.
Supports both :e-* (preferred) and :v-* (backward compatible) prefixes."
  (let ((attrs nil)
        (children nil)
        ;; Boolean directives that don't take a value (both e- and v- prefixes)
        (boolean-directives '(:e-else :v-else)))
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

;; Directive prefix length constants
(defconst etaf-etml--e-bind-prefix ":e-bind:"
  "The e-bind directive prefix.")
(defconst etaf-etml--v-bind-prefix ":v-bind:"
  "The v-bind directive prefix (backward compatible).")
(defconst etaf-etml--bind-prefix-length 8
  "Length of bind directive prefixes (:e-bind: or :v-bind:).")

(defun etaf-etml--process-bindings (attrs data)
  "Process attribute bindings in ATTRS using DATA.
Returns processed attrs plist.
Note: e-bind/v-bind is processed via :e-bind:attr or :v-bind:attr syntax."
  (let ((result nil)
        (rest attrs))
    (while rest
      (let* ((key (car rest))
             (val (cadr rest))
             (key-name (symbol-name key)))
        (cond
         ;; e-bind:attr="expr" binding - evaluate expression for value (preferred)
         ((string-prefix-p etaf-etml--e-bind-prefix key-name)
          (let* ((attr-name (substring key-name etaf-etml--bind-prefix-length))
                 (new-key (intern (concat ":" attr-name)))
                 (new-val (etaf-etml--to-string
                           (etaf-etml--eval-expr val data))))
            (setq result (append result (list new-key new-val)))))
         
         ;; v-bind:attr="expr" binding - evaluate expression for value (backward compat)
         ((string-prefix-p etaf-etml--v-bind-prefix key-name)
          (let* ((attr-name (substring key-name etaf-etml--bind-prefix-length))
                 (new-key (intern (concat ":" attr-name)))
                 (new-val (etaf-etml--to-string
                           (etaf-etml--eval-expr val data))))
            (setq result (append result (list new-key new-val)))))
         
         ;; Regular attribute - keep as-is
         (t
          (setq result (append result (list key val))))))
      (setq rest (cddr rest)))
    result))

(defun etaf-etml--find-else-sibling (siblings)
  "Find e-else/v-else or e-else-if/v-else-if sibling from SIBLINGS list.
Returns (MATCHING-SIBLING . REMAINING-SIBLINGS) or nil.
Supports both :e-* (preferred) and :v-* (backward compatible) prefixes."
  (when siblings
    (let ((first (car siblings)))
      (when (and (listp first) (not (stringp first)))
        (let ((parsed (etaf-etml--split-attrs-and-children (cdr first))))
          (when (or (etaf-etml--get-attr (car parsed) :e-else)
                    (etaf-etml--get-attr (car parsed) :v-else)
                    (etaf-etml--get-attr (car parsed) :e-else-if)
                    (etaf-etml--get-attr (car parsed) :v-else-if))
            (cons first (cdr siblings))))))))

(defun etaf-etml-render (template data)
  "Render TEMPLATE with DATA.
TEMPLATE is TML with e-* or v-* directives.
DATA is a plist with template variables.
Returns standard TML format."
  (let ((result (etaf-etml--render-node template data nil)))
    ;; Result is (nodes . skip-count), we return the first node
    (car (car result))))

(defun etaf-etml--render-node (node data siblings)
  "Render NODE with DATA, considering SIBLINGS for e-else/v-else directives.
Returns (RENDERED-NODES . SKIP-COUNT) where SKIP-COUNT is siblings to skip."
  (cond
   ;; String - interpolate
   ((stringp node)
    (cons (list (etaf-etml--interpolate-string node data)) 0))
   
   ;; Atom - return as is
   ((atom node)
    (cons (list node) 0))
   
   ;; List (element)
   (t
    (let* ((tag (car node))
           (rest (cdr node))
           (parsed (etaf-etml--split-attrs-and-children rest))
           (attrs (car parsed))
           (children (cdr parsed)))
      
      ;; Check for e-for/v-for (list rendering)
      (if-let ((for-expr (etaf-etml--get-directive attrs :e-for :v-for)))
          (let* ((for-parsed (etaf-etml--parse-v-for for-expr))
                 (item-var (nth 0 for-parsed))
                 (index-var (nth 1 for-parsed))
                 (collection-expr (nth 2 for-parsed))
                 (collection (etaf-etml--eval-expr collection-expr data))
                 (new-attrs (etaf-etml--remove-directive attrs :e-for :v-for))
                 (results '())
                 (index 0)
                 ;; Ensure collection is a list; nil becomes empty list
                 (items (cond
                         ((null collection) nil)
                         ((listp collection) collection)
                         (t (error "e-for/v-for requires a list, got: %S" collection)))))
            (dolist (item items)
              (let* ((item-key (intern (concat ":" item-var)))
                     (new-data (copy-sequence data)))
                (setq new-data (plist-put new-data item-key item))
                (when index-var
                  (let ((idx-key (intern (concat ":" index-var))))
                    (setq new-data (plist-put new-data idx-key index))))
                (let* ((new-node (cons tag (append new-attrs children)))
                       (rendered (etaf-etml--render-node new-node new-data nil)))
                  (push (car rendered) results)))
              (cl-incf index))
            (cons (apply #'append (nreverse results)) 0))
        
        ;; Check for e-if/v-if (conditional rendering)
        (if-let ((if-expr (etaf-etml--get-directive attrs :e-if :v-if)))
            (let ((condition (etaf-etml--eval-expr if-expr data)))
              (if (etaf-etml--truthy-p condition)
                  ;; Condition true - render this node, skip else siblings
                  (let* ((new-attrs (etaf-etml--remove-directive attrs :e-if :v-if))
                         (skip-count 0))
                    ;; Count consecutive e-else-if/v-else-if and e-else/v-else siblings to skip
                    (let ((remaining siblings))
                      (while (etaf-etml--find-else-sibling remaining)
                        (cl-incf skip-count)
                        (setq remaining (cdr remaining))))
                    (let ((rendered (etaf-etml--render-element
                                     tag new-attrs children data)))
                      (cons rendered skip-count)))
                ;; Condition false - check for e-else-if/v-else-if or e-else/v-else
                (let ((else-info (etaf-etml--find-else-sibling siblings)))
                  (if else-info
                      (let* ((else-node (car else-info))
                             (else-tag (car else-node))
                             (else-parsed (etaf-etml--split-attrs-and-children
                                           (cdr else-node)))
                             (else-attrs (car else-parsed))
                             (else-children (cdr else-parsed))
                             (remaining-siblings (cdr else-info)))
                        (if-let ((else-if-expr (etaf-etml--get-directive
                                                else-attrs :e-else-if :v-else-if)))
                            ;; Process e-else-if/v-else-if
                            (let* ((stripped-else-attrs
                                    (etaf-etml--remove-directive else-attrs :e-else-if :v-else-if))
                                   (else-if-attrs
                                    (append (list :e-if else-if-expr) stripped-else-attrs))
                                   (new-else-node
                                    (cons else-tag
                                          (append else-if-attrs else-children)))
                                   ;; Render the e-else-if/v-else-if with its remaining siblings
                                   (result (etaf-etml--render-node
                                            new-else-node data remaining-siblings)))
                              ;; Skip count is 1 (for this e-else-if/v-else-if) plus what it skips
                              (cons (car result) (+ 1 (cdr result))))
                          ;; Process e-else/v-else - render it directly (strip e-else/v-else attr)
                          (let* ((stripped-else-attrs (etaf-etml--remove-directive
                                                       else-attrs :e-else :v-else))
                                 (rendered (etaf-etml--render-element
                                            else-tag stripped-else-attrs else-children data)))
                            (cons rendered 1))))
                    ;; No else sibling - render nothing
                    (cons nil 0)))))
          
          ;; Check for e-else-if/v-else-if without preceding e-if/v-if (standalone)
          (if-let ((else-if-expr (etaf-etml--get-directive attrs :e-else-if :v-else-if)))
              ;; This shouldn't normally happen, treat as e-if/v-if
              (let* ((new-attrs (etaf-etml--remove-directive attrs :e-else-if :v-else-if))
                     (new-attrs (append (list :e-if else-if-expr) new-attrs))
                     (new-node (cons tag (append new-attrs children))))
                (etaf-etml--render-node new-node data siblings))
            
            ;; Check for e-else/v-else without preceding e-if/v-if (standalone)
            (if (etaf-etml--has-directive-p attrs :e-else :v-else)
                ;; Render as-is (remove e-else/v-else attr)
                (let* ((new-attrs (etaf-etml--remove-directive attrs :e-else :v-else))
                       (rendered (etaf-etml--render-element
                                  tag new-attrs children data)))
                  (cons rendered 0))
              
              ;; Check for e-show/v-show (visibility)
              (if-let ((show-expr (etaf-etml--get-directive attrs :e-show :v-show)))
                  (let ((visible (etaf-etml--eval-expr show-expr data))
                        (new-attrs (etaf-etml--remove-directive attrs :e-show :v-show)))
                    (if (etaf-etml--truthy-p visible)
                        (let ((rendered (etaf-etml--render-element
                                         tag new-attrs children data)))
                          (cons rendered 0))
                      ;; Add display: none style
                      (let* ((style (etaf-etml--get-attr new-attrs :style))
                             (new-style (if style
                                            (concat style "; display: none")
                                          "display: none"))
                             (new-attrs (etaf-etml--remove-attr new-attrs :style))
                             (new-attrs (append new-attrs (list :style new-style)))
                             (rendered (etaf-etml--render-element
                                        tag new-attrs children data)))
                        (cons rendered 0))))
                
                ;; Check for e-text/v-text
                (if-let ((text-expr (etaf-etml--get-directive attrs :e-text :v-text)))
                    (let* ((text-value (etaf-etml--to-string
                                        (etaf-etml--eval-expr text-expr data)))
                           (new-attrs (etaf-etml--remove-directive attrs :e-text :v-text))
                           (rendered (etaf-etml--render-element
                                      tag new-attrs (list text-value) data)))
                      (cons rendered 0))
                  
                  ;; No special directives - render normally
                  (let ((rendered (etaf-etml--render-element
                                   tag attrs children data)))
                    (cons rendered 0))))))))))))

(defun etaf-etml--render-element (tag attrs children data)
  "Render element with TAG, ATTRS, CHILDREN using DATA.
Returns list of rendered nodes (usually just one)."
  ;; Process attribute bindings
  (let* ((processed-attrs (etaf-etml--process-bindings attrs data))
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
                                  (etaf-etml--interpolate-string val data)
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
                 (result (etaf-etml--render-node child data remaining)))
            (setq rendered-children (append rendered-children (car result)))
            (setq skip-count (cdr result))
            (setq child-list (cdr child-list)))))
      
      ;; Build final element
      (list (if final-attrs
                (append (list tag) final-attrs rendered-children)
              (cons tag rendered-children))))))

;;; Convenience Functions

(defun etaf-etml-to-dom (template &optional data)
  "Render TEMPLATE with DATA and convert to DOM.
This is a convenience function combining template rendering and TML-to-DOM."
  (require 'etaf-etml)
  (etaf-etml--to-dom (etaf-etml-render template data)))

;;; Reactive Data System

(defvar etaf-etml--watchers (make-hash-table :test 'eq)
  "Hash table mapping data objects to their watchers.")

(defun etaf-etml-create-reactive (data)
  "Create a reactive data wrapper around DATA plist.
Returns a reactive data object that can trigger re-renders."
  (let ((reactive (list :data data
                        :version 0
                        :watchers nil)))
    reactive))

(defun etaf-etml-get (reactive key)
  "Get value for KEY from REACTIVE data object."
  (plist-get (plist-get reactive :data) key))

(defun etaf-etml-set (reactive key value)
  "Set KEY to VALUE in REACTIVE data object and trigger watchers."
  (let* ((data (plist-get reactive :data))
         (new-data (plist-put data key value)))
    (plist-put reactive :data new-data)
    (plist-put reactive :version (1+ (plist-get reactive :version)))
    ;; Trigger watchers
    (dolist (watcher (plist-get reactive :watchers))
      (funcall watcher reactive key value))
    value))

(defun etaf-etml-watch (reactive callback)
  "Add CALLBACK as watcher to REACTIVE data.
CALLBACK receives (reactive key value) when data changes."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (cons callback watchers))))

(defun etaf-etml-unwatch (reactive callback)
  "Remove CALLBACK from REACTIVE data watchers."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (delete callback watchers))))

(provide 'etaf-etml)
;;; etaf-etml.el ends here
