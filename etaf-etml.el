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
(require 'etaf-vdom)
(require 'etaf-component)

;; Declare etaf-ecss functions to avoid warnings
(declare-function etaf-ecss "etaf-ecss")

;;; Scoped CSS (ecss tag) support
;; ecss tags can be used at any position in ETML to define locally scoped styling.
;; The scope includes sibling elements and their descendants.
;;
;; How scoping works (pre-compilation approach):
;; - When an ecss tag is found among children, its Tailwind classes are extracted
;; - The CSS selector is used to match elements within the sibling element tree
;; - Matched elements have the Tailwind classes added to their class attribute
;; - These classes are processed by the dual-mode CSS engine for light/dark support
;;
;; Example:
;;   (div :id "parent"
;;     (ecss "p{bg-green-700 dark:bg-gray-600}")
;;     (p "Direct child p")
;;     (ul (li (p "Nested p"))))
;;
;; Becomes:
;;   (div :id "parent"
;;     (p :class "bg-green-700 dark:bg-gray-600" "Direct child p")
;;     (ul (li (p :class "bg-green-700 dark:bg-gray-600" "Nested p"))))
;;
;; Both p elements get the classes applied because they match the "p" selector
;; within the scope (sibling tree). The classes are then processed for dual-mode.

(defun etaf-etml--ecss-tag-p (item)
  "Check if ITEM is an ecss tag (not inside style tag).
An ecss tag is (ecss ...) used at the element level."
  (and (listp item)
       (eq (car item) 'ecss)))

(defun etaf-etml--process-ecss-content (ecss-form scope-id)
  "Process ECSS-FORM and return CSS string with selectors scoped by SCOPE-ID.
ECSS-FORM must be in unified format: (ecss \"selector{tailwind-classes}\" ...).
Multiple unified format strings can be provided.
Returns CSS string with selectors prefixed by scope class."
  (require 'etaf-ecss)
  (let* ((args (cdr ecss-form)))
    ;; Validate that all arguments are in unified format
    (dolist (arg args)
      (unless (and (stringp arg)
                   (etaf-ecss-unified-p arg))
        (error "ecss tag only supports unified format: \"selector{tailwind-classes}\". Got: %S" arg)))
    ;; Get the raw CSS string
    (let ((raw-css (apply #'etaf-ecss args)))
      ;; Handle multiple rules (separated by newlines)
      (mapconcat
       (lambda (rule)
         (when (not (string-empty-p (string-trim rule)))
           ;; Add scope prefix to the selector
           ;; CSS format is: "selector { declarations }"
           ;; We need to transform it to ".scope-id selector { declarations }"
           (if (string-match "^\\([^{]+\\)\\({.*\\)$" rule)
               (let ((selector (string-trim (match-string 1 rule)))
                     (rest (match-string 2 rule)))
                 (format ".%s %s %s" scope-id selector rest))
             rule)))
       (split-string raw-css "\n" t)
       "\n"))))

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

(defun etaf-alist-to-plist (alist)
  "Convert an alist to a plist.
Example: ((class . \"foo\") (id . \"bar\"))
         => (:class \"foo\" :id \"bar\")"
  (when alist
    (let ((result nil))
      (dolist (pair alist)
        (push (intern (concat ":" (symbol-name (car pair)))) result)
        (push (cdr pair) result))
      (nreverse result))))

(defun etaf-css-alist-to-string (css-alist)
  "Convert CSS alist to style string.
CSS-ALIST is ((property . value) ...).
Returns a string like \"property1: value1; property2: value2\"."
  (mapconcat (lambda (pair)
               (format "%s: %s" (car pair) (cdr pair)))
             css-alist "; "))

(defun etaf-class-list-to-string (class-list)
  "Convert CLASS-LIST to a class string.
CLASS-LIST is a list of strings: (\"class1\" \"class2\" ...).
Returns a string like \"class1 class2 ...\".
Empty strings and nil values are filtered out."
  (mapconcat #'identity
             (delq nil (mapcar (lambda (s)
                                 (and (stringp s)
                                      (not (string-empty-p s))
                                      s))
                               class-list))
             " "))

(defun etaf-etml--ecss-item-p (item)
  "Check if ITEM is an (ecss ...) form.
Returns t if ITEM is a list starting with symbol `ecss'."
  (and (listp item)
       (eq (car item) 'ecss)))

(defun etaf-etml--process-style-children (children)
  "Process style tag children, converting (ecss ...) forms to CSS string.
CHILDREN can be:
- A single CSS string
- A list containing (ecss \"selector{tailwind-classes}\" ...) forms
Only unified format is supported for ecss forms.
Returns a CSS string."
  (let ((css-parts '()))
    (dolist (child children)
      (cond
       ;; String - add as-is
       ((stringp child)
        (push child css-parts))
       ;; (ecss ...) form - validate unified format
       ((etaf-etml--ecss-item-p child)
        (require 'etaf-ecss)
        (let ((args (cdr child)))
          ;; Validate that all arguments are in unified format
          (dolist (arg args)
            (unless (and (stringp arg)
                         (etaf-ecss-unified-p arg))
              (error "ecss tag only supports unified format: \"selector{tailwind-classes}\". Got: %S" arg)))
          (let ((css (apply #'etaf-ecss args)))
            (push css css-parts))))
       ;; Other - ignore or convert to string
       (t nil)))
    (mapconcat #'identity (nreverse css-parts) "\n")))

(defun etaf-etml--merge-tag-styles (tag attr-alist)
  "Merge tag default styles into ATTR-ALIST for TAG.
Since tag default styles are now in the UA stylesheet, this function
only handles custom inline styles. Returns the ATTR-ALIST unchanged."
  ;; Tag default styles are now in UA stylesheet, not in tag definitions
  ;; This function is kept for backward compatibility but doesn't merge anything
  attr-alist)

(defun etaf-etml--apply-ecss-classes-to-node (node dom ecss-rules)
  "Apply Tailwind classes from ECSS-RULES to NODE if it matches any selector.
NODE is a DOM node: (tag ((attrs...)) children...).
DOM is the root DOM node (for selector matching context).
ECSS-RULES is a list of (:selector selector :classes classes :ast ast) plists.
Returns the modified node.

Priority order: :style > :class > ecss
To achieve this, ecss classes are prepended to the class attribute, so original
:class classes come later and have higher priority in CSS cascade."
  (require 'etaf-css-selector)
  (when (and (listp node) (symbolp (car node)))
    (let ((attrs (cadr node))
          (children (cddr node))
          (ecss-classes-to-add nil))
      ;; Collect all ecss classes that match this node
      (dolist (rule ecss-rules)
        (let* ((selector (plist-get rule :selector))
               (classes (plist-get rule :classes))
               (ast (plist-get rule :ast)))
          
          ;; Use the CSS selector matching engine to check if node matches
          (when (and ast (etaf-css-selector-node-matches-p node dom ast))
            ;; Node matches! Collect the classes
            (setq ecss-classes-to-add 
                  (if ecss-classes-to-add
                      (concat ecss-classes-to-add " " classes)
                    classes)))))
      
      ;; Apply collected ecss classes if any
      (when ecss-classes-to-add
        (let ((existing-class (cdr (assq 'class attrs))))
          (if existing-class
              ;; Prepend ecss classes to existing class (so original class has higher priority)
              (setcdr (assq 'class attrs) 
                     (concat ecss-classes-to-add " " existing-class))
            ;; Add new class attribute with ecss classes only
            (setcar (cdr node) 
                   (cons (cons 'class ecss-classes-to-add) attrs)))))
      
      ;; Recursively process all children
      (when children
        (let ((current children))
          (while current
            (let ((child (car current)))
              (when (and (listp child) (symbolp (car child)))
                (etaf-etml--apply-ecss-classes-to-node child dom ecss-rules)))
            (setq current (cdr current)))))))
  node)

(defun etaf-etml--process-children-with-ecss (children)
  "Process CHILDREN list, handling ecss tags by applying classes to matching elements.
When ecss tags are found, their Tailwind classes are applied directly to matching
child elements' class attributes, enabling dual-mode (light/dark) support.
Scoping is handled by matching within the sibling element tree only.
Returns processed children list."
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
      ;; Has ecss tags: parse them and apply classes to matching elements
      (require 'etaf-ecss)
      (require 'etaf-css-selector)
      (let ((ecss-rules nil))
        ;; Parse all ecss tags to extract selectors, classes, and AST
        (dolist (ecss-form ecss-tags)
          (let ((args (cdr ecss-form)))
            (dolist (arg args)
              (when (etaf-ecss-unified-p arg)
                (when-let* ((parsed (etaf-ecss-parse-to-classes arg))
                           (selector (plist-get parsed :selector))
                           (classes (plist-get parsed :classes))
                           ;; Parse selector to AST for matching
                           (ast (condition-case err
                                    (etaf-css-selector-parse selector)
                                  (error 
                                   (message "Warning: Failed to parse ecss selector '%s': %s" 
                                           selector (error-message-string err))
                                   nil))))
                  ;; Store selector, classes, and AST
                  (when ast
                    (push (list :selector selector 
                               :classes classes 
                               :ast ast)
                          ecss-rules)))))))
        (setq ecss-rules (nreverse ecss-rules))
        
        ;; Process children to DOM first
        (let ((processed-children (mapcar #'etaf-etml-to-dom other-children)))
          ;; Build a temporary DOM tree for selector matching context
          ;; We need a root node to provide context for descendant selectors
          (let ((temp-root (cons 'div (cons nil processed-children))))
            
            ;; Apply ecss classes to all matching elements in the tree
            (dolist (child processed-children)
              (when (and (listp child) (symbolp (car child)))
                (etaf-etml--apply-ecss-classes-to-node child temp-root ecss-rules)))
            
            ;; Return the processed children (without the temp root)
            processed-children)))))

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

Supports :class attribute in two formats:
  String format:
    (div :class \"w-20 h-4\" ...)
  List format:
    (div :class (\"w-20 h-4\" \"border border-red-200\" \"bg-green-200\") ...)
Both are converted to:
  (div ((class . \"w-20 h-4 border border-red-200 bg-green-200\")) ...)

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

If the tag has event handlers (on-click, on-hover-enter, etc.), a tag-metadata
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
        ;; Process :style and :class attributes - handle both string and list formats
        (let* ((attr-alist (etaf-plist-to-alist attrs))
               (style-attr (assq 'style attr-alist))
               (class-attr (assq 'class attr-alist)))
          ;; If :style is present and is a list (alist), convert it to string
          (when (and style-attr (listp (cdr style-attr)))
            (let ((style-string (etaf-css-alist-to-string (cdr style-attr))))
              (setcdr style-attr style-string)))
          ;; If :class is present and is a list, convert it to string
          (when (and class-attr (listp (cdr class-attr)))
            (let ((class-string (etaf-class-list-to-string (cdr class-attr))))
              (setcdr class-attr class-string)))
          ;; Merge etaf-etml-tag default styles if tag is defined
          (setq attr-alist (etaf-etml--merge-tag-styles tag attr-alist))
          
          ;; Process children based on tag type and content
          ;; Check for ecss children first
          (let* ((has-ecss-children (and rest 
                                         (not (eq tag 'style))
                                         (cl-some #'etaf-etml--ecss-tag-p rest))))
            ;; Process children
            (let ((children
                   (cond
                    ;; Style tag with (ecss ...) forms - global scope CSS
                    ((and (eq tag 'style)
                          rest
                          (cl-some #'etaf-etml--ecss-item-p rest))
                     (list (etaf-etml--process-style-children rest)))
                    ;; Other tags with ecss children - local scope
                    (has-ecss-children
                     (etaf-etml--process-children-with-ecss rest))
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
          (let* ((component (etaf-component-get tag))
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
This is a convenience function combining template rendering and TML-to-DOM.
When DATA is nil, rendering is skipped for efficiency."
  (require 'etaf-etml)
  (etaf-etml--to-dom 
   (if data
       ;; Has data - render template with data substitution
       (etaf-etml-render template data)
     ;; No data - skip rendering step (no substitutions needed)
     template)))

;;; ============================================================================
;;; Vue 3 Style Compilation Pipeline
;;; ============================================================================

;; Pipeline: ETML → Compiler → Render Function → VNode Tree

;;; Step 1: Compiler - Compile ETML template to render function

(defun etaf-compile (template)
  "Compile ETML TEMPLATE into a render function.
This is the COMPILER step in the Vue 3 pipeline.

TEMPLATE - ETML template S-expression
Returns - A render function that produces VNode tree when called

The render function signature: (lambda (data) ... vnode)

Example:
  (setq render-fn (etaf-compile '(div :class \"box\" \"Hello {{ name }}\")))
  (setq vnode (funcall render-fn '(:name \"World\")))"
  (lambda (data)
    "Render function - produces VNode tree.
This is the RENDER FUNCTION step in the Vue 3 pipeline.
DATA - Context data for template rendering"
    ;; First, render template with data (handle {{ }} interpolations, directives)
    (let ((rendered-template (etaf-etml-render template data)))
      ;; Then, create VNode tree from rendered template
      (etaf-vdom-create-from-etml rendered-template))))

;;; Step 2: VNode Creation - Convert rendered ETML to VNode tree

(defun etaf-vdom-create-from-etml (sexp)
  "Create VNode tree from rendered ETML S-expression.
This produces the VIRTUAL DOM (VNode tree) in the Vue 3 pipeline.

SEXP - Rendered ETML (S-expression after template rendering)
Returns - VNode tree (pure data structure, no DOM)

The VNode follows Vue 3 design:
- :type - Node type (element, text, fragment, etc.)
- :tag - Tag name for elements
- :props - Properties including attributes and event handlers
- :children - Child VNodes
- :key - Key for diff optimization

Example ETML:
  (div :class \"box\" :id \"main\" (span \"Hello\"))

Produces VNode:
  (:type element
   :tag div
   :props (:class \"box\" :id \"main\")
   :children ((:type element :tag span :props (:textContent \"Hello\"))))"
  (cond
   ;; Text node - atom (string, number, etc.)
   ((atom sexp)
    (etaf-vdom-text sexp))
   
   ;; Special: ecss tag - becomes style element
   ((and (listp sexp) (eq (car sexp) 'ecss))
    (require 'etaf-ecss)
    (let ((css (apply #'etaf-ecss (cdr sexp))))
      (etaf-vdom-element 'style
                         (list :textContent css)
                         nil)))
   
   ;; Element node
   ((listp sexp)
    (let* ((tag (car sexp))
           (rest (cdr sexp))
           (props nil)
           (children nil))
      
      ;; Parse attributes (keywords)
      (while (and rest (keywordp (car rest)))
        (push (car rest) props)
        (setq rest (cdr rest))
        (when rest
          (push (car rest) props)
          (setq rest (cdr rest))))
      (setq props (nreverse props))
      
      ;; Process props (convert :style and :class if needed)
      (setq props (etaf-vdom--normalize-props props tag))
      
      ;; Add built-in event handlers for interactive tags
      (setq props (etaf-vdom--add-builtin-handlers tag props))
      
      ;; Remaining items are children - recursively create VNodes
      (setq children (mapcar #'etaf-vdom-create-from-etml rest))
      
      ;; Handle ecss in children (scoped CSS)
      (when (cl-some (lambda (c) (and (listp c) (eq (car c) 'ecss))) rest)
        (let ((scope-id (etaf-etml--generate-scope-id)))
          ;; Add scope class to props
          (setq props (etaf-vdom--add-scope-class props scope-id))
          ;; Process ecss children
          (setq children (etaf-vdom--process-ecss-children rest scope-id))))
      
      ;; Create element VNode
      (etaf-vdom-element tag props children)))
   
   ;; Fallback
   (t nil)))

(defun etaf-vdom--normalize-props (props tag)
  "Normalize PROPS for TAG.
Convert :style list to string, :class list to string, etc."
  (let ((result (copy-sequence props)))
    ;; Process :style
    (let ((style (plist-get result :style)))
      (when (and style (listp style))
        (setq result (plist-put result :style
                                (etaf-css-alist-to-string style)))))
    ;; Process :class
    (let ((class (plist-get result :class)))
      (when (and class (listp class))
        (setq result (plist-put result :class
                                (etaf-class-list-to-string class)))))
    result))

(defun etaf-vdom--add-builtin-handlers (tag props)
  "Add built-in event handlers for TAG to PROPS.
For interactive tags like 'a', 'button', etc., add default handlers."
  (let ((result (copy-sequence props)))
    (pcase tag
      ('a
       ;; Add default click handler for links
       (unless (plist-get result :on-click)
         (setq result (plist-put result :on-click
                                 (lambda ()
                                   (interactive)
                                   (when-let ((href (plist-get props :href)))
                                     (browse-url href)))))))
      ('button
       ;; Button handlers will be added by layout rendering
       result)
      ;; Add more tags as needed
      (_ result))
    result))

(defun etaf-vdom--add-scope-class (props scope-id)
  "Add SCOPE-ID to class in PROPS."
  (let* ((class (plist-get props :class))
         (new-class (if class
                        (concat class " " scope-id)
                      scope-id)))
    (plist-put (copy-sequence props) :class new-class)))

(defun etaf-vdom--process-ecss-children (children scope-id)
  "Process CHILDREN with ecss tags, applying SCOPE-ID.
Returns list of VNodes with scoped CSS."
  (let ((ecss-nodes nil)
        (other-nodes nil))
    ;; Separate ecss from other children
    (dolist (child children)
      (if (and (listp child) (eq (car child) 'ecss))
          (push child ecss-nodes)
        (push child other-nodes)))
    
    (if (null ecss-nodes)
        ;; No ecss, process normally
        (mapcar #'etaf-vdom-create-from-etml children)
      ;; Has ecss - create scoped style
      (require 'etaf-ecss)
      (let* ((css-parts (mapcar (lambda (ecss-form)
                                  (etaf-etml--process-ecss-content ecss-form scope-id))
                                (nreverse ecss-nodes)))
             (css-string (mapconcat #'identity css-parts "\n"))
             (style-vnode (etaf-vdom-element 'style
                                             (list :textContent css-string)
                                             nil))
             (other-vnodes (mapcar #'etaf-vdom-create-from-etml (nreverse other-nodes))))
        (cons style-vnode other-vnodes)))))

;;; ============================================================================
;;; Template Analysis - Detect Dynamic Content
;;; ============================================================================

(defconst etaf-etml--interpolation-regex "{{\\s-*[^}]+?\\s-*}}"
  "Regular expression pattern for matching template interpolation {{ }}.")

(defconst etaf-etml--dynamic-directives 
  '(:e-if :e-for :e-show :e-text :e-else :e-else-if)
  "List of directive keywords that indicate dynamic template content.")

(defconst etaf-etml--bind-directive-prefix "e-bind:"
  "Prefix for bind directives (without leading colon).")

(defconst etaf-etml--bind-directive-regex "^:e-bind:"
  "Regular expression pattern for matching bind directives.")

(defun etaf-etml-has-dynamic-content-p (template)
  "Check if TEMPLATE has dynamic content.
A template is considered dynamic if it contains:
- Text interpolation: {{ }}
- Directives: :e-if, :e-for, :e-show, :e-bind:*, :e-text, :e-else, :e-else-if
- Component usage (registered components)

This function recursively analyzes the template tree, examining:
- All string values for {{ }} interpolation patterns
- All attributes for dynamic directives
- All child elements for dynamic content

Returns t if the template has dynamic content, nil otherwise."
  (cond
   ;; String - check for {{ }} interpolation
   ((stringp template)
    (string-match-p etaf-etml--interpolation-regex template))
   
   ;; Atom - no dynamic content
   ((atom template) nil)
   
   ;; List - check tag and attributes
   ((listp template)
    (let ((tag (car template))
          (rest (cdr template)))
      (or
       ;; Check if tag is a registered component
       (etaf-etml--is-component-p tag)
       
       ;; Check attributes for directives
       (catch 'has-directive
         (while (and rest (keywordp (car rest)))
           (let* ((key (car rest))
                  (val (cadr rest)))
             ;; Check for dynamic directives
             (when (memq key etaf-etml--dynamic-directives)
               (throw 'has-directive t))
             ;; Check for :e-bind:* directives using regex
             (when (string-match-p etaf-etml--bind-directive-regex
                                   (symbol-name key))
               (throw 'has-directive t))
             ;; Check attribute value for {{ }} interpolation
             (when (and (stringp val) 
                        (string-match-p etaf-etml--interpolation-regex val))
               (throw 'has-directive t))
             ;; Move to next attribute
             (setq rest (cddr rest))))
         nil) ; No directive found
       
       ;; Recursively check children
       (catch 'has-dynamic-child
         ;; Skip attributes (already processed above)
         (while (and rest (keywordp (car rest)))
           (setq rest (cddr rest)))
         ;; Check remaining children
         (while rest
           (when (etaf-etml-has-dynamic-content-p (car rest))
             (throw 'has-dynamic-child t))
           (setq rest (cdr rest)))
         nil)))) ; No dynamic child found
   
   ;; Unknown type
   (t nil)))

(defun etaf-etml--render-component (component attrs children data)
  "Render COMPONENT with ATTRS and CHILDREN using DATA context.
Returns the rendered ETML."
  (let* ((rendered (etaf-component--render component attrs children data)))
    ;; If the component rendering returned a template,
    ;; we need to render it with the data
    (if (and rendered (listp rendered))
        (etaf-etml-render rendered data)
      rendered)))

(defun etaf-etml--is-component-p (tag)
  "Check if TAG is a registered component."
  (etaf-component-is-component-p tag))

(provide 'etaf-etml)
;;; etaf-etml.el ends here
