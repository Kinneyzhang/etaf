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
(require 'etaf-etml-tag)
(require 'etaf-vdom)

;; Declare etaf-ecss functions to avoid warnings
(declare-function etaf-ecss "etaf-ecss")

;;; Scoped CSS (ecss tag) support
;; ecss tags can be used at any position in ETML to define locally scoped CSS.
;; The scope includes the parent element and all its descendants.
;;
;; How scoping works:
;; - When an ecss tag is found among children, a unique scope ID is generated
;; - The scope class is added to the parent element (the element containing ecss)
;; - The CSS selectors are prefixed with the scope class (e.g., ".scope-1 .box")
;; - This CSS selector matches any element matching the selector within the scope
;;
;; Example:
;;   (div :id "parent"
;;     (ecss "p{text-red-200}")
;;     (ecss "ul p{italic}")
;;     (p "Direct child p")
;;     (ul (li (p "Nested p"))))
;;
;; Becomes:
;;   (div :id "parent" :class "etaf-scope-1"
;;     (style ".etaf-scope-1 p { color: #fecaca; }\n.etaf-scope-1 ul p { ... }")
;;     (p "Direct child p")
;;     (ul (li (p "Nested p"))))
;;
;; Both p elements will be styled because they are descendants of .etaf-scope-1

(defvar etaf-etml--scope-counter 0
  "Counter for generating unique scope IDs.")

(defun etaf-etml--generate-scope-id ()
  "Generate a unique scope ID for scoped CSS."
  (format "etaf-scope-%d" (cl-incf etaf-etml--scope-counter)))


(defun etaf-etml--ecss-tag-p (item)
  "Check if ITEM is an ecss tag (not inside style tag).
An ecss tag is (ecss ...) used at the element level."
  (and (listp item)
       (eq (car item) 'ecss)))

(defun etaf-etml--process-ecss-content (ecss-form scope-id)
  "Process ECSS-FORM and return CSS string with selectors scoped by SCOPE-ID.
ECSS-FORM is (ecss selector declarations...) or (ecss \"unified-format\").
Returns CSS string with selectors prefixed by scope class."
  (require 'etaf-ecss)
  (let* ((args (cdr ecss-form))
         ;; Get the raw CSS string first
         (raw-css (apply #'etaf-ecss args)))
    ;; Add scope prefix to the selector
    ;; CSS format is: "selector { declarations }"
    ;; We need to transform it to ".scope-id selector { declarations }"
    (if (string-match "^\\([^{]+\\)\\({.*\\)$" raw-css)
        (let ((selector (string-trim (match-string 1 raw-css)))
              (rest (match-string 2 raw-css)))
          (format ".%s %s %s" scope-id selector rest))
      raw-css)))

(defun etaf-etml--add-scope-class (node scope-id)
  "Add SCOPE-ID class to NODE's class attribute.
NODE is in DOM format: (tag ((attrs...) children...)).
Note: This function is kept for utility purposes but is no longer used
in the main ecss scoping flow. Scope classes are now added to parent
elements directly in `etaf-etml--to-dom'.
Returns modified node."
  (when (and (listp node) (symbolp (car node)))
    (let* ((attrs (cadr node))
           (class-attr (assq 'class attrs)))
      (if class-attr
          ;; Append scope ID to existing class
          (setcdr class-attr (concat (cdr class-attr) " " scope-id))
        ;; Add new class attribute
        (setcar (cdr node) (cons (cons 'class scope-id) attrs)))))
  node)

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

(defun etaf-etml--ecss-item-p (item)
  "Check if ITEM is an (ecss ...) form.
Returns t if ITEM is a list starting with symbol `ecss'."
  (and (listp item)
       (eq (car item) 'ecss)))

(defun etaf-etml--process-style-children (children)
  "Process style tag children, converting (ecss ...) forms to CSS string.
CHILDREN can be:
- A single CSS string
- A list containing (ecss selector declarations...) forms
Returns a CSS string."
  (let ((css-parts '()))
    (dolist (child children)
      (cond
       ;; String - add as-is
       ((stringp child)
        (push child css-parts))
       ;; (ecss selector declarations...) form
       ((etaf-etml--ecss-item-p child)
        (require 'etaf-ecss)
        (let ((css (apply #'etaf-ecss (cdr child))))
          (push css css-parts)))
       ;; Other - ignore or convert to string
       (t nil)))
    (mapconcat #'identity (nreverse css-parts) "\n")))

(defun etaf-etml--merge-tag-styles (tag attr-alist)
  "Merge etaf-etml-tag default styles into ATTR-ALIST for TAG.
If TAG is defined in etaf-etml-tag, its default style is used as the base,
and any inline :style in ATTR-ALIST overrides the default.
Returns the updated ATTR-ALIST with merged style attribute."
  (when (etaf-etml-tag-defined-p tag)
    (let* ((tag-def (etaf-etml-tag-get-definition tag))
           (default-style (plist-get tag-def :default-style)))
      (when default-style
        (let* ((style-attr (assq 'style attr-alist))
               (inline-style (when style-attr
                               (if (stringp (cdr style-attr))
                                   (etaf-etml-tag--parse-style-string (cdr style-attr))
                                 (cdr style-attr))))
               ;; Merge: default-style as base, inline-style overrides
               (merged-style (etaf-etml-tag--merge-styles default-style inline-style)))
          (if merged-style
              (let ((merged-string (etaf-css-alist-to-string merged-style)))
                (if style-attr
                    (setcdr style-attr merged-string)
                  ;; Add style attribute if not present
                  (setq attr-alist (cons (cons 'style merged-string) attr-alist))))
            attr-alist)))))
  attr-alist)

(defun etaf-etml--process-children-with-ecss (children scope-id)
  "Process CHILDREN list, handling ecss tags for local scope CSS.
When ecss tags are found, they are converted to style elements with scoped CSS.
The SCOPE-ID is required and should be added to the parent element by the caller.
Returns processed children list (style element followed by other children)."
  (let ((ecss-tags nil)
        (other-children nil))
    ;; First pass: separate ecss tags from other children
    (dolist (child children)
      (if (etaf-etml--ecss-tag-p child)
          (push child ecss-tags)
        (push child other-children)))
    (setq ecss-tags (nreverse ecss-tags))
    (setq other-children (nreverse other-children))
    
    (if (null ecss-tags)
        ;; No ecss tags, process children normally
        (mapcar #'etaf-etml-to-dom children)
      ;; Has ecss tags: create scoped CSS using provided scope-id
      (let* (;; Generate scoped CSS from all ecss tags
             (css-parts (mapcar (lambda (ecss-form)
                                  (etaf-etml--process-ecss-content ecss-form scope-id))
                                ecss-tags))
             (css-string (mapconcat #'identity css-parts "\n"))
             ;; Create style element for scoped CSS
             (style-element (list 'style nil css-string))
             ;; Process other children (no need to add scope class - parent has it)
             (processed-children (mapcar #'etaf-etml-to-dom other-children)))
        ;; Return style element followed by processed children
        (cons style-element processed-children)))))

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

Special handling for ecss tags:
1. Inside <style> tags (global scope):
   (style
     (ecss \"#id\" \"flex bg-red-500\")
     (ecss \".class\" \"p-4\" (color \"blue\")))
   Becomes:
   (style nil \"#id { display: flex; ... }\\n.class { ... }\")

2. At any position (local scope):
   (div
     (ecss \".box\" \"bg-red-500\")
     (div :class \"box\" \"Hello\"))
   The ecss tag creates scoped CSS that only applies to sibling elements
   and their descendants. A unique scope class is added to siblings.

If the tag is defined in etaf-etml-tag, its default styles are merged with
inline styles, where inline styles take precedence.

If the tag has event handlers (on-click, on-hover-enter, etc.), a tag-instance
is created and stored in the DOM attributes for later use when generating
the final string with keymap properties."
  (cond
   ;; If it's an atom (string, number, etc.), return as is
   ((atom sexp) sexp)
   ;; If it's an ecss tag at top level, convert to style element
   ((etaf-etml--ecss-tag-p sexp)
    ;; ecss at top level becomes a global style (no scope)
    (require 'etaf-ecss)
    (let ((css (apply #'etaf-ecss (cdr sexp))))
      (list 'style nil css)))
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
          ;; Merge etaf-etml-tag default styles if tag is defined
          (setq attr-alist (etaf-etml--merge-tag-styles tag attr-alist))
          ;; NOTE: Tag instances are no longer embedded in DOM attributes.
          ;; They are stored in the virtual DOM layer (see etaf-vdom.el).
          ;; To access tag instances with event handlers, use:
          ;;   (etaf-etml-to-dom-with-vdom template data)
          ;; This returns an etaf-vdom-result with:
          ;;   - :dom - Clean DOM without tag-instances
          ;;   - :vtree - Virtual DOM tree with tag-instances and event handlers
          ;; Process children based on tag type and content
          ;; Check for ecss children first to handle scoping properly
          (let* ((has-ecss-children (and rest 
                                         (not (eq tag 'style))
                                         (cl-some #'etaf-etml--ecss-tag-p rest)))
                 ;; Generate scope-id if needed
                 (scope-id (when has-ecss-children
                             (etaf-etml--generate-scope-id))))
            ;; Add scope class to parent element's attributes if needed
            (when scope-id
              (let ((class-attr (assq 'class attr-alist)))
                (if class-attr
                    ;; Append scope ID to existing class
                    (setcdr class-attr (concat (cdr class-attr) " " scope-id))
                  ;; Add new class attribute with scope ID
                  (setq attr-alist (cons (cons 'class scope-id) attr-alist)))))
            ;; Process children
            (let ((children
                   (cond
                    ;; Style tag with (ecss ...) forms - global scope CSS
                    ((and (eq tag 'style)
                          rest
                          (cl-some #'etaf-etml--ecss-item-p rest))
                     (list (etaf-etml--process-style-children rest)))
                    ;; Other tags with ecss children - local scope CSS (scope-id added to parent)
                    (has-ecss-children
                     (etaf-etml--process-children-with-ecss rest scope-id))
                    ;; Normal processing
                    (t (mapcar #'etaf-etml-to-dom rest)))))
              (cons tag (cons attr-alist children)))))))))

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

(defun etaf-etml--parse-e-for (expr)
  "Parse e-for expression EXPR.
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
   
   (t (error "Invalid e-for expression: %s" expr))))

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

(defun etaf-etml--get-directive (attrs e-key)
  "Get directive E-KEY from ATTRS.
Returns the value of the key if found."
  (plist-get attrs e-key))

(defun etaf-etml--has-directive-p (attrs e-key)
  "Check if ATTRS has E-KEY directive."
  (plist-member attrs e-key))

(defun etaf-etml--remove-directive (attrs e-key)
  "Remove E-KEY directive from ATTRS."
  (etaf-etml--remove-attr attrs e-key))

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
Handles boolean directives like :e-else that don't take a value."
  (let ((attrs nil)
        (children nil)
        ;; Boolean directives that don't take a value
        (boolean-directives '(:e-else)))
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
(defconst etaf-etml--bind-prefix-length 8
  "Length of bind directive prefix (:e-bind:).")

(defun etaf-etml--process-bindings (attrs data)
  "Process attribute bindings in ATTRS using DATA.
Returns processed attrs plist.
Note: e-bind is processed via :e-bind:attr syntax."
  (let ((result nil)
        (rest attrs))
    (while rest
      (let* ((key (car rest))
             (val (cadr rest))
             (key-name (symbol-name key)))
        (cond
         ;; e-bind:attr="expr" binding - evaluate expression for value
         ((string-prefix-p etaf-etml--e-bind-prefix key-name)
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
  "Find e-else or e-else-if sibling from SIBLINGS list.
Returns (MATCHING-SIBLING . REMAINING-SIBLINGS) or nil."
  (when siblings
    (let ((first (car siblings)))
      (when (and (listp first) (not (stringp first)))
        (let ((parsed (etaf-etml--split-attrs-and-children (cdr first))))
          (when (or (etaf-etml--get-attr (car parsed) :e-else)
                    (etaf-etml--get-attr (car parsed) :e-else-if))
            (cons first (cdr siblings))))))))

(defun etaf-etml-render (template data)
  "Render TEMPLATE with DATA.
TEMPLATE is TML with e-* directives.
DATA is a plist with template variables.
Returns standard TML format."
  (let ((result (etaf-etml--render-node template data nil)))
    ;; Result is (nodes . skip-count), we return the first node
    (car (car result))))

(defun etaf-etml--render-node (node data siblings)
  "Render NODE with DATA, considering SIBLINGS for e-else directives.
Returns (RENDERED-NODES . SKIP-COUNT) where SKIP-COUNT is siblings to skip."
  (cond
   ;; String - interpolate
   ((stringp node)
    (cons (list (etaf-etml--interpolate-string node data)) 0))
   
   ;; Atom - return as is
   ((atom node)
    (cons (list node) 0))
   
   ;; List (element or component)
   (t
    (let* ((tag (car node))
           (rest (cdr node))
           (parsed (etaf-etml--split-attrs-and-children rest))
           (attrs (car parsed))
           (children (cdr parsed)))
      
      ;; Check if this is a component first
      (if (etaf-etml--is-component-p tag)
          (let* ((component (etaf-etml-component-get tag))
                 (rendered (etaf-etml--render-component
                            component attrs children data)))
            (cons (list rendered) 0))
        
        ;; Check for e-for (list rendering)
      (if-let ((for-expr (etaf-etml--get-directive attrs :e-for)))
          (let* ((for-parsed (etaf-etml--parse-e-for for-expr))
                 (item-var (nth 0 for-parsed))
                 (index-var (nth 1 for-parsed))
                 (collection-expr (nth 2 for-parsed))
                 (collection (etaf-etml--eval-expr collection-expr data))
                 (new-attrs (etaf-etml--remove-directive attrs :e-for))
                 (results '())
                 (index 0)
                 ;; Ensure collection is a list; nil becomes empty list
                 (items (cond
                         ((null collection) nil)
                         ((listp collection) collection)
                         (t (error "e-for requires a list, got: %S" collection)))))
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
        
        ;; Check for e-if (conditional rendering)
        (if-let ((if-expr (etaf-etml--get-directive attrs :e-if)))
            (let ((condition (etaf-etml--eval-expr if-expr data)))
              (if (etaf-etml--truthy-p condition)
                  ;; Condition true - render this node, skip else siblings
                  (let* ((new-attrs (etaf-etml--remove-directive attrs :e-if))
                         (skip-count 0))
                    ;; Count consecutive e-else-if and e-else siblings to skip
                    (let ((remaining siblings))
                      (while (etaf-etml--find-else-sibling remaining)
                        (cl-incf skip-count)
                        (setq remaining (cdr remaining))))
                    (let ((rendered (etaf-etml--render-element
                                     tag new-attrs children data)))
                      (cons rendered skip-count)))
                ;; Condition false - check for e-else-if or e-else
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
                                                else-attrs :e-else-if)))
                            ;; Process e-else-if
                            (let* ((stripped-else-attrs
                                    (etaf-etml--remove-directive else-attrs :e-else-if))
                                   (else-if-attrs
                                    (append (list :e-if else-if-expr) stripped-else-attrs))
                                   (new-else-node
                                    (cons else-tag
                                          (append else-if-attrs else-children)))
                                   ;; Render the e-else-if with its remaining siblings
                                   (result (etaf-etml--render-node
                                            new-else-node data remaining-siblings)))
                              ;; Skip count is 1 (for this e-else-if) plus what it skips
                              (cons (car result) (+ 1 (cdr result))))
                          ;; Process e-else - render it directly (strip e-else attr)
                          (let* ((stripped-else-attrs (etaf-etml--remove-directive
                                                       else-attrs :e-else))
                                 (rendered (etaf-etml--render-element
                                            else-tag stripped-else-attrs else-children data)))
                            (cons rendered 1))))
                    ;; No else sibling - render nothing
                    (cons nil 0)))))
          
          ;; Check for e-else-if without preceding e-if (standalone)
          (if-let ((else-if-expr (etaf-etml--get-directive attrs :e-else-if)))
              ;; This shouldn't normally happen, treat as e-if
              (let* ((new-attrs (etaf-etml--remove-directive attrs :e-else-if))
                     (new-attrs (append (list :e-if else-if-expr) new-attrs))
                     (new-node (cons tag (append new-attrs children))))
                (etaf-etml--render-node new-node data siblings))
            
            ;; Check for e-else without preceding e-if (standalone)
            (if (etaf-etml--has-directive-p attrs :e-else)
                ;; Render as-is (remove e-else attr)
                (let* ((new-attrs (etaf-etml--remove-directive attrs :e-else))
                       (rendered (etaf-etml--render-element
                                  tag new-attrs children data)))
                  (cons rendered 0))
              
              ;; Check for e-show (visibility)
              (if-let ((show-expr (etaf-etml--get-directive attrs :e-show)))
                  (let ((visible (etaf-etml--eval-expr show-expr data))
                        (new-attrs (etaf-etml--remove-directive attrs :e-show)))
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
                
                ;; Check for e-text
                (if-let ((text-expr (etaf-etml--get-directive attrs :e-text)))
                    (let* ((text-value (etaf-etml--to-string
                                        (etaf-etml--eval-expr text-expr data)))
                           (new-attrs (etaf-etml--remove-directive attrs :e-text))
                           (rendered (etaf-etml--render-element
                                      tag new-attrs (list text-value) data)))
                      (cons rendered 0))
                  
                  ;; No special directives - render normally
                  (let ((rendered (etaf-etml--render-element
                                   tag attrs children data)))
                    (cons rendered 0)))))))))))))

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

;;; ============================================================================
;;; Component System (Vue3-like)
;;; ============================================================================

(defvar etaf-etml-components (make-hash-table :test 'eq)
  "Hash table storing all registered ETML components.
Keys are component name symbols, values are component definitions.")

(defun etaf-etml-component-create (name &rest options)
  "Create a component definition with NAME and OPTIONS.
OPTIONS is a plist that can include:
- :props - List of prop names or plist with prop definitions
- :setup - Setup function that returns data and methods
- :template - Component template function or ETML expression
- :render - Custom render function
- :emits - List of events the component can emit"
  (let ((component (list :name name
                         :props (plist-get options :props)
                         :setup (plist-get options :setup)
                         :template (plist-get options :template)
                         :render (plist-get options :render)
                         :emits (plist-get options :emits))))
    component))

(defun etaf-etml-component-register (name component)
  "Register COMPONENT with NAME in the global component registry."
  (puthash name component etaf-etml-components))

(defun etaf-etml-component-unregister (name)
  "Unregister the component with NAME."
  (remhash name etaf-etml-components))

(defun etaf-etml-component-get (name)
  "Get the component definition for NAME."
  (gethash name etaf-etml-components))

(defun etaf-etml-component-defined-p (name)
  "Check if a component with NAME is defined."
  (not (null (gethash name etaf-etml-components))))

(defun etaf-etml-component-list-all ()
  "Return a list of all registered component names."
  (let ((names nil))
    (maphash (lambda (key _value) (push key names))
             etaf-etml-components)
    (nreverse names)))

;;;###autoload
(defmacro etaf-etml-define-component (name &rest options)
  "Define an ETML component with NAME and OPTIONS.

NAME is the component symbol (e.g., my-button, user-card).

OPTIONS is a plist that can include:
- :props - List of prop names the component accepts
- :setup - Setup function (props) -> plist of reactive data and methods
- :template - Template function (data) -> ETML sexp, or ETML sexp directly
- :emits - List of events this component can emit

Example:
  (etaf-etml-define-component my-counter
    :props \\='(:initial-count)
    :setup (lambda (props)
             (let* ((count (etaf-etml-ref
                            (or (plist-get props :initial-count) 0)))
                    (increment (lambda ()
                                 (etaf-etml-ref-set
                                  count (1+ (etaf-etml-ref-get count))))))
               (list :count count :increment increment)))
    :template (lambda (data)
                `(div :class \"counter\"
                      (span ,(format \"Count: %s\" (etaf-etml-ref-get
                                                    (plist-get data :count))))
                      (button :on-click ,(plist-get data :increment)
                              \"Increment\"))))"
  (declare (indent defun))
  (let ((component-var (intern (format "etaf-etml-component--%s" name))))
    `(progn
       (defvar ,component-var
         (etaf-etml-component-create ',name ,@options)
         ,(format "Component definition for %s." name))
       (etaf-etml-component-register ',name ,component-var)
       ',name)))

(defun etaf-etml--extract-props (attrs prop-names)
  "Extract props from ATTRS based on PROP-NAMES.
Returns a plist of prop values."
  (let ((props nil))
    (dolist (prop-name prop-names)
      (let* ((key (if (keywordp prop-name) prop-name
                    (intern (concat ":" (symbol-name prop-name)))))
             (value (plist-get attrs key)))
        (when value
          (setq props (plist-put props key value)))))
    props))

(defun etaf-etml--render-component (component attrs children data)
  "Render COMPONENT with ATTRS and CHILDREN using DATA context.
Returns the rendered ETML."
  (let* ((prop-names (plist-get component :props))
         (props (etaf-etml--extract-props attrs prop-names))
         (setup-fn (plist-get component :setup))
         (template-fn (plist-get component :template))
         (render-fn (plist-get component :render))
         ;; Add special $slots prop for children
         (props-with-slots (plist-put props :$slots children)))
    ;; Run setup function if provided
    (let ((component-data (if setup-fn
                              (funcall setup-fn props-with-slots)
                            props-with-slots)))
      ;; Merge component data with parent data
      (let ((merged-data (append component-data data)))
        (cond
         ;; Custom render function
         (render-fn
          (funcall render-fn component-data))
         ;; Template function
         ((functionp template-fn)
          (let ((template (funcall template-fn component-data)))
            (etaf-etml-render template merged-data)))
         ;; Template as ETML expression
         (template-fn
          (etaf-etml-render template-fn merged-data))
         ;; No template - render children with component data
         (t
          (if children
              (if (= (length children) 1)
                  (etaf-etml-render (car children) merged-data)
                `(div ,@(mapcar (lambda (c)
                                  (etaf-etml-render c merged-data))
                                children)))
            nil)))))))

(defun etaf-etml--is-component-p (tag)
  "Check if TAG is a registered component."
  (etaf-etml-component-defined-p tag))

;;; ============================================================================
;;; Reactive System (Vue3-like using add-variable-watcher)
;;; ============================================================================

(defvar etaf-etml--current-effect nil
  "The currently running effect, used for dependency tracking.")

(defvar etaf-etml--effect-stack nil
  "Stack of currently running effects for nested effect tracking.")

(defvar etaf-etml--effect-deps-tracker nil
  "Tracks dependencies collected during effect execution for cleanup.")

;;; --- Ref: Basic Reactive Reference ---

(defun etaf-etml-ref (value)
  "Create a reactive reference with initial VALUE.
Returns a ref object that can be accessed with `etaf-etml-ref-get'
and modified with `etaf-etml-ref-set'.

Usage:
  (let ((count (etaf-etml-ref 0)))
    (etaf-etml-ref-get count)     ; => 0
    (etaf-etml-ref-set count 1)   ; sets to 1 and triggers watchers
    (etaf-etml-ref-get count))    ; => 1"
  (let* ((id (cl-gensym "etaf-ref-"))
         (ref (list :type 'ref
                    :id id
                    :value value
                    :deps nil)))    ; dependencies (effects that read this ref)
    ref))

(defun etaf-etml-ref-p (obj)
  "Check if OBJ is a ref object."
  (and (listp obj)
       (eq (plist-get obj :type) 'ref)))

(defun etaf-etml-ref-get (ref)
  "Get the current value of REF.
Also tracks the current effect as a dependency if inside an effect."
  (when (etaf-etml-ref-p ref)
    ;; Track dependency if inside an effect
    (when etaf-etml--current-effect
      (let ((deps (plist-get ref :deps)))
        (unless (member etaf-etml--current-effect deps)
          (plist-put ref :deps (cons etaf-etml--current-effect deps))))
      ;; Also track for effect cleanup
      (when etaf-etml--effect-deps-tracker
        (let ((tracked (plist-get etaf-etml--effect-deps-tracker :deps)))
          (unless (member ref tracked)
            (plist-put etaf-etml--effect-deps-tracker :deps (cons ref tracked))))))
    (plist-get ref :value)))

(defun etaf-etml-ref-set (ref value)
  "Set REF to VALUE and trigger all dependent effects."
  (when (etaf-etml-ref-p ref)
    (let ((old-value (plist-get ref :value)))
      (unless (equal old-value value)
        (plist-put ref :value value)
        ;; Trigger all dependent effects
        (dolist (effect (plist-get ref :deps))
          (when (functionp effect)
            (funcall effect)))))))

(defun etaf-etml-ref-update (ref fn)
  "Update REF by applying FN to its current value.
FN receives the current value and should return the new value."
  (when (etaf-etml-ref-p ref)
    (let* ((current (etaf-etml-ref-get ref))
           (new-value (funcall fn current)))
      (etaf-etml-ref-set ref new-value)
      new-value)))

;;; --- Computed: Derived Reactive Values ---

(defun etaf-etml-computed (getter)
  "Create a computed value from GETTER function.
The computed value is lazily evaluated and cached.
It automatically tracks dependencies and recomputes when they change.

Usage:
  (let* ((count (etaf-etml-ref 0))
         (doubled (etaf-etml-computed
                   (lambda () (* 2 (etaf-etml-ref-get count))))))
    (etaf-etml-computed-get doubled)    ; => 0
    (etaf-etml-ref-set count 5)
    (etaf-etml-computed-get doubled))   ; => 10"
  (let* ((id (cl-gensym "etaf-computed-"))
         (computed (list :type 'computed
                         :id id
                         :getter getter
                         :value nil
                         :dirty t       ; needs recomputation
                         :deps nil)))   ; effects that depend on this computed
    ;; Create an effect to recompute when dependencies change
    (let ((recompute-effect
           (lambda ()
             (plist-put computed :dirty t)
             ;; Trigger dependent effects
             (dolist (effect (plist-get computed :deps))
               (when (functionp effect)
                 (funcall effect))))))
      (plist-put computed :recompute-effect recompute-effect))
    computed))

(defun etaf-etml-computed-p (obj)
  "Check if OBJ is a computed object."
  (and (listp obj)
       (eq (plist-get obj :type) 'computed)))

(defun etaf-etml-computed-get (computed)
  "Get the current value of COMPUTED.
Recomputes if dirty (dependencies have changed)."
  (when (etaf-etml-computed-p computed)
    ;; Track as dependency if inside an effect
    (when etaf-etml--current-effect
      (let ((deps (plist-get computed :deps)))
        (unless (member etaf-etml--current-effect deps)
          (plist-put computed :deps (cons etaf-etml--current-effect deps))))
      ;; Also track for effect cleanup
      (when etaf-etml--effect-deps-tracker
        (let ((tracked (plist-get etaf-etml--effect-deps-tracker :deps)))
          (unless (member computed tracked)
            (plist-put etaf-etml--effect-deps-tracker :deps (cons computed tracked))))))
    ;; Recompute if dirty
    (when (plist-get computed :dirty)
      (let* ((getter (plist-get computed :getter))
             (recompute-effect (plist-get computed :recompute-effect))
             ;; Track dependencies during computation
             (etaf-etml--current-effect recompute-effect)
             (value (funcall getter)))
        (plist-put computed :value value)
        (plist-put computed :dirty nil)))
    (plist-get computed :value)))

;;; --- Watch: Explicit Dependency Watching ---

(defun etaf-etml-watch-source (source callback &optional options)
  "Watch SOURCE (ref or computed) and call CALLBACK when it changes.
CALLBACK receives (new-value old-value).
OPTIONS can include:
- :immediate - If t, run callback immediately with current value
- :deep - If t, deep watch objects (not implemented yet)

Returns a stop function that removes the watcher.

Usage:
  (let* ((count (etaf-etml-ref 0))
         (stop (etaf-etml-watch-source
                count
                (lambda (new old)
                  (message \"Count changed: %s -> %s\" old new)))))
    (etaf-etml-ref-set count 1)  ; triggers callback
    (funcall stop)               ; stop watching
    (etaf-etml-ref-set count 2)) ; no callback"
  (let* ((immediate (plist-get options :immediate))
         (old-value (cond
                     ((etaf-etml-ref-p source)
                      (etaf-etml-ref-get source))
                     ((etaf-etml-computed-p source)
                      (etaf-etml-computed-get source))))
         (watcher (lambda ()
                    (let ((new-value (cond
                                      ((etaf-etml-ref-p source)
                                       (etaf-etml-ref-get source))
                                      ((etaf-etml-computed-p source)
                                       (etaf-etml-computed-get source)))))
                      (unless (equal new-value old-value)
                        (funcall callback new-value old-value)
                        (setq old-value new-value))))))
    ;; Add watcher to source's deps
    (let ((deps (plist-get source :deps)))
      (plist-put source :deps (cons watcher deps)))
    ;; Run immediately if requested
    (when immediate
      (funcall callback old-value nil))
    ;; Return stop function
    (lambda ()
      (let ((deps (plist-get source :deps)))
        (plist-put source :deps (delete watcher deps))))))

;;; --- WatchEffect: Automatic Dependency Tracking ---

(defun etaf-etml-watch-effect (effect-fn)
  "Run EFFECT-FN immediately and re-run when dependencies change.
Dependencies are automatically tracked when refs/computed values are accessed.

Returns a stop function that removes the effect.

Usage:
  (let* ((count (etaf-etml-ref 0))
         (stop (etaf-etml-watch-effect
                (lambda ()
                  (message \"Count is: %s\" (etaf-etml-ref-get count))))))
    (etaf-etml-ref-set count 1)  ; re-runs effect, logs \"Count is: 1\"
    (funcall stop)               ; stop watching
    (etaf-etml-ref-set count 2)) ; no effect"
  (let* ((tracked-deps nil)  ; refs/computed that this effect depends on
         (runner nil)
         (active t))         ; whether the effect is still active
    ;; Create the runner that will execute the effect
    (setq runner
          (lambda ()
            (when active
              ;; Clear old dependencies before re-running
              (dolist (dep tracked-deps)
                (let ((old-deps (plist-get dep :deps)))
                  (plist-put dep :deps (delete runner old-deps))))
              (setq tracked-deps nil)
              ;; Run effect with dependency tracking
              ;; When refs are accessed, they add runner to their deps
              (let ((etaf-etml--current-effect runner)
                    (etaf-etml--effect-deps-tracker (list :deps nil)))
                (push runner etaf-etml--effect-stack)
                (unwind-protect
                    (funcall effect-fn)
                  (pop etaf-etml--effect-stack)
                  ;; Capture tracked deps for cleanup later
                  (setq tracked-deps (plist-get etaf-etml--effect-deps-tracker :deps)))))))
    ;; Run immediately
    (funcall runner)
    ;; Return stop function
    (lambda ()
      (setq active nil)
      (dolist (dep tracked-deps)
        (let ((old-deps (plist-get dep :deps)))
          (plist-put dep :deps (delete runner old-deps)))))))

;;; --- Reactive Object (for plist-like data) ---

(defun etaf-etml-reactive (data)
  "Create a reactive wrapper around DATA plist.
Each key becomes a ref that can be accessed and modified reactively.

Usage:
  (let ((state (etaf-etml-reactive \\='(:name \"Alice\" :age 30))))
    (etaf-etml-reactive-get state :name)      ; => \"Alice\"
    (etaf-etml-reactive-set state :name \"Bob\")
    (etaf-etml-reactive-get state :name))     ; => \"Bob\""
  (let ((refs (make-hash-table :test 'eq)))
    ;; Create refs for each key in data
    (let ((rest data))
      (while rest
        (let ((key (car rest))
              (value (cadr rest)))
          (puthash key (etaf-etml-ref value) refs))
        (setq rest (cddr rest))))
    (list :type 'reactive
          :refs refs
          :data data)))

(defun etaf-etml-reactive-p (obj)
  "Check if OBJ is a reactive object."
  (and (listp obj)
       (eq (plist-get obj :type) 'reactive)))

(defun etaf-etml-reactive-get (reactive key)
  "Get value for KEY from REACTIVE object."
  (when (etaf-etml-reactive-p reactive)
    (let* ((refs (plist-get reactive :refs))
           (ref (gethash key refs)))
      (if ref
          (etaf-etml-ref-get ref)
        nil))))

(defun etaf-etml-reactive-set (reactive key value)
  "Set KEY to VALUE in REACTIVE object."
  (when (etaf-etml-reactive-p reactive)
    (let* ((refs (plist-get reactive :refs))
           (ref (gethash key refs)))
      (if ref
          (etaf-etml-ref-set ref value)
        ;; Create new ref for new key
        (puthash key (etaf-etml-ref value) refs)
        value))))

(defun etaf-etml-reactive-to-plist (reactive)
  "Convert REACTIVE object to a plain plist."
  (when (etaf-etml-reactive-p reactive)
    (let ((result nil)
          (refs (plist-get reactive :refs)))
      (maphash (lambda (key ref)
                 (setq result (plist-put result key (etaf-etml-ref-get ref))))
               refs)
      result)))

;;; --- Backward Compatibility ---
;;; Keep the old simple reactive system for backward compatibility

(defvar etaf-etml--watchers (make-hash-table :test 'eq)
  "Hash table mapping data objects to their watchers.")

(defun etaf-etml-create-reactive (data)
  "Create a reactive data wrapper around DATA plist.
Returns a reactive data object that can trigger re-renders.
\(Legacy function - consider using `etaf-etml-reactive' for new code.)"
  (let ((reactive (list :data data
                        :version 0
                        :watchers nil)))
    reactive))

(defun etaf-etml-get (reactive key)
  "Get value for KEY from REACTIVE data object.
\(Legacy function - works with `etaf-etml-create-reactive' objects.)"
  (plist-get (plist-get reactive :data) key))

(defun etaf-etml-set (reactive key value)
  "Set KEY to VALUE in REACTIVE data object and trigger watchers.
\(Legacy function - works with `etaf-etml-create-reactive' objects.)"
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
CALLBACK receives (reactive key value) when data changes.
\(Legacy function - works with `etaf-etml-create-reactive' objects.)"
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (cons callback watchers))))

(defun etaf-etml-unwatch (reactive callback)
  "Remove CALLBACK from REACTIVE data watchers.
\(Legacy function - works with `etaf-etml-create-reactive' objects.)"
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (delete callback watchers))))

;;; ============================================================================
;;; Virtual DOM Integration
;;; ============================================================================

;; The following functions provide integration with the virtual DOM layer
;; (etaf-vdom.el). The virtual DOM allows storing tag instances and event
;; handlers separately from the clean DOM structure.

(defun etaf-etml--to-vdom-node (sexp)
  "Convert SEXP to both VNode and clean DOM, recursively.
Returns a VNode with :dom set to clean DOM and :tag-instance if applicable.
SEXP can be:
- An atom (string, number, etc.) - returned as-is
- An ecss tag - converted to style element
- An element (tag attrs... children...)"
  (cond
   ;; Atom (string, number, etc.) - create text VNode
   ((atom sexp)
    (etaf-vdom-text sexp))
   
   ;; ecss tag at top level - convert to style element
   ((etaf-etml--ecss-tag-p sexp)
    (require 'etaf-ecss)
    (let* ((css (apply #'etaf-ecss (cdr sexp)))
           (dom (list 'style nil css))
           (vnode (etaf-vdom-element 'style
                                     :props nil
                                     :dom dom)))
      vnode))
   
   ;; Element
   (t
    (let ((tag (car sexp))
          (rest (cdr sexp))
          (attrs nil))
      ;; Parse attributes from plist
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
        
        ;; Merge etaf-etml-tag default styles if tag is defined
        (setq attr-alist (etaf-etml--merge-tag-styles tag attr-alist))
        
        ;; Create tag-instance for tags with event handlers (for VNode only, not DOM)
        (let ((tag-instance nil))
          (when (etaf-etml-tag-defined-p tag)
            (let* ((tag-def (etaf-etml-tag-get-definition tag))
                   (has-events (or (plist-get tag-def :on-click)
                                   (plist-get tag-def :on-hover-enter)
                                   (plist-get tag-def :on-hover-leave)
                                   (plist-get tag-def :on-keydown)
                                   (plist-get tag-def :hover-style))))
              (when has-events
                (setq tag-instance (etaf-etml-tag-create-instance tag attrs rest)))))
          
          ;; Process children based on tag type and content
          (let* ((has-ecss-children (and rest 
                                         (not (eq tag 'style))
                                         (cl-some #'etaf-etml--ecss-tag-p rest)))
                 (scope-id (when has-ecss-children
                             (etaf-etml--generate-scope-id))))
            ;; Add scope class to parent element's attributes if needed
            (when scope-id
              (let ((class-attr (assq 'class attr-alist)))
                (if class-attr
                    (setcdr class-attr (concat (cdr class-attr) " " scope-id))
                  (setq attr-alist (cons (cons 'class scope-id) attr-alist)))))
            
            ;; Process children recursively
            (let* ((child-vnodes
                    (cond
                     ;; Style tag with (ecss ...) forms - global scope CSS
                     ((and (eq tag 'style)
                           rest
                           (cl-some #'etaf-etml--ecss-item-p rest))
                      (list (etaf-vdom-text (etaf-etml--process-style-children rest))))
                     ;; Other tags with ecss children - local scope CSS
                     (has-ecss-children
                      (etaf-etml--process-children-with-ecss-vdom rest scope-id))
                     ;; Normal processing
                     (t (mapcar #'etaf-etml--to-vdom-node rest))))
                   ;; Build clean DOM (without tag-instance)
                   (child-doms (mapcar #'etaf-vdom-get-dom child-vnodes))
                   (dom (cons tag (cons attr-alist child-doms)))
                   ;; Create VNode
                   (vnode (etaf-vdom-element tag
                                             :props attrs
                                             :dom dom)))
              ;; Set tag-instance in VNode if present
              (when tag-instance
                (etaf-vdom-set-tag-instance vnode tag-instance))
              ;; Set children
              (etaf-vdom-set-children vnode child-vnodes)
              vnode))))))))

(defun etaf-etml--process-children-with-ecss-vdom (children scope-id)
  "Process CHILDREN list with ecss tags, returning VNodes.
Similar to `etaf-etml--process-children-with-ecss' but returns VNodes."
  (let ((ecss-tags nil)
        (other-children nil))
    ;; First pass: separate ecss tags from other children
    (dolist (child children)
      (if (etaf-etml--ecss-tag-p child)
          (push child ecss-tags)
        (push child other-children)))
    (setq ecss-tags (nreverse ecss-tags))
    (setq other-children (nreverse other-children))
    
    (if (null ecss-tags)
        ;; No ecss tags, process children normally
        (mapcar #'etaf-etml--to-vdom-node children)
      ;; Has ecss tags: create scoped CSS
      (let* ((css-parts (mapcar (lambda (ecss-form)
                                  (etaf-etml--process-ecss-content ecss-form scope-id))
                                ecss-tags))
             (css-string (mapconcat #'identity css-parts "\n"))
             (style-dom (list 'style nil css-string))
             (style-vnode (etaf-vdom-element 'style
                                             :props nil
                                             :dom style-dom
                                             :children (list (etaf-vdom-text css-string))))
             (processed-children (mapcar #'etaf-etml--to-vdom-node other-children)))
        (cons style-vnode processed-children)))))

(defun etaf-etml-to-dom-with-vdom (template &optional data)
  "Render TEMPLATE with DATA and convert to both clean DOM and VTree.
Returns an `etaf-vdom-result' structure containing:
- :vtree - The root VNode of the virtual DOM tree
- :dom - The clean DOM (without tag-instances in attributes)

This function should be used when you need access to tag instances
and event handlers through the virtual DOM layer.

Example:
  (let* ((result (etaf-etml-to-dom-with-vdom \\='(a :href \"/test\" \"Link\")))
         (dom (etaf-vdom-result-get-dom result))
         (vtree (etaf-vdom-result-get-vtree result)))
    ;; dom is clean: (a ((href . \"/test\")) \"Link\")
    ;; vtree contains the tag-instance for event handling
    )"
  (let* ((rendered (etaf-etml-render template data))
         (vnode (etaf-etml--to-vdom-node rendered))
         (dom (etaf-vdom-get-dom vnode)))
    (etaf-vdom-make-result :vtree vnode :dom dom)))

(provide 'etaf-etml)
;;; etaf-etml.el ends here
