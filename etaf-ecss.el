;;; etaf-ecss.el --- Emacs-style CSS Expressions (like rx for regex) -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, emacs, expressions
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; This module provides an Emacs-specific way to express CSS using list
;; structures, similar to how Emacs's `rx' macro converts list expressions
;; to regular expression strings.
;;
;; The goal is to provide a more Lisp-friendly way to write CSS that:
;; 1. Uses symbols instead of strings for property names
;; 2. Supports nested structures for related properties
;; 3. Provides shorthand notations for common patterns
;; 4. Converts to standard CSS strings for use with CSSOM
;; 5. Supports Tailwind CSS utility classes as style declarations
;;
;; Basic Usage:
;;
;;   ;; Convert a single rule to CSS string
;;   (etaf-ecss ".box" 
;;     '(background "red")
;;     '(padding 10)
;;     '(border 1 'solid "black"))
;;   ;; => ".box { background: red; padding: 10px; border: 1px solid black; }"
;;
;;   ;; Build a stylesheet from multiple rules
;;   (etaf-ecss-stylesheet
;;     '(".container" (width 800) (margin 0 'auto))
;;     '(".box" (background "blue") (padding 10)))
;;   ;; => ".container { width: 800px; margin: 0 auto; }\n.box { ... }"
;;
;; Tailwind CSS Support:
;;
;;   ;; Simple string format (recommended):
;;   (etaf-ecss ".card" "flex items-center justify-center bg-red-500 p-4")
;;   ;; => ".card { display: flex; align-items: center; ... }"
;;
;;   ;; Mix Tailwind string with standard properties:
;;   (etaf-ecss ".card"
;;     "flex items-center bg-red-500"
;;     '(padding 10)
;;     '(margin 0 auto))
;;
;;   ;; In :style attribute:
;;   (div :style (etaf-ecss-style "flex items-center bg-blue-500")
;;     "Hello")
;;
;; Selector Expressions:
;;
;;   (etaf-ecss-selector tag)         ; "tag"
;;   (etaf-ecss-selector (class "box")) ; ".box"
;;   (etaf-ecss-selector (id "main"))   ; "#main"
;;   (etaf-ecss-selector (tag "div"))   ; "div"
;;   (etaf-ecss-selector (child "ul" "li")) ; "ul > li"
;;   (etaf-ecss-selector (descendant "nav" "a")) ; "nav a"
;;   (etaf-ecss-selector (and (tag "div") (class "box"))) ; "div.box"
;;
;; Property Expressions:
;;
;;   (background "red")          ; background: red
;;   (background 'red)           ; background: red (symbol)
;;   (color "#333")              ; color: #333
;;   (padding 10)                ; padding: 10px (auto-units)
;;   (padding 10 20)             ; padding: 10px 20px
;;   (margin 0 'auto)            ; margin: 0 auto
;;   (border 1 'solid "black")   ; border: 1px solid black
;;   (font 16 'bold "Arial")     ; font: 16px bold Arial

;;; Code:

(require 'cl-lib)
(require 'etaf-tailwind)

;;; Tailwind CSS Support

(defun etaf-ecss--tailwind-class-p (decl)
  "Check if DECL is a Tailwind CSS utility class declaration.
A Tailwind declaration is a single-element list containing a symbol
that is recognized as a valid Tailwind class.

Examples of Tailwind declarations:
  (flex)           ; standalone utility
  (bg-red-500)     ; color utility
  (p-4)            ; spacing utility
  (hover:bg-blue)  ; variant utility (as symbol)"
  (and (listp decl)
       (= (length decl) 1)
       (symbolp (car decl))
       (let ((class-name (symbol-name (car decl))))
         (etaf-tailwind-class-p class-name))))

(defun etaf-ecss--convert-tailwind-to-css-string (class-name)
  "Convert Tailwind CLASS-NAME to CSS declaration string(s).
Returns a list of CSS declaration strings like (\"display: flex\" ...).
Returns nil if the class cannot be converted."
  (when-let ((css-props (etaf-tailwind-to-css class-name)))
    (mapcar (lambda (prop)
              (format "%s: %s" (car prop) (cdr prop)))
            css-props)))

(defun etaf-ecss--process-tailwind-decl (decl)
  "Process a Tailwind CSS declaration DECL and return CSS string.
DECL should be a single-element list like (flex) or (bg-red-500).
Returns a CSS declaration string or nil if conversion fails.
When conversion fails, returns nil and the caller falls back to
treating it as a standard property expression."
  (let* ((class-name (symbol-name (car decl)))
         (css-strings (etaf-ecss--convert-tailwind-to-css-string class-name)))
    (if css-strings
        (mapconcat #'identity css-strings "; ")
      ;; Log warning for Tailwind class that couldn't be converted
      (message "Warning: Tailwind class '%s' could not be converted to CSS"
               class-name)
      nil)))

;;; CSS Selector Expressions

(defun etaf-ecss-selector (expr)
  "Convert selector expression EXPR to CSS selector string.

EXPR can be:
- A string (used as-is)
- A symbol (converted to tag name)
- A list expression:
  - (class \"name\")      => \".name\"
  - (id \"name\")         => \"#name\"
  - (tag \"name\")        => \"name\"
  - (attr \"name\")       => \"[name]\"
  - (attr \"name\" \"value\") => \"[name=\\\"value\\\"]\"
  - (child sel1 sel2 ...) => \"sel1 > sel2 > ...\"
  - (descendant sel1 sel2) => \"sel1 sel2\"
  - (sibling sel1 sel2)   => \"sel1 + sel2\"
  - (general-sibling sel1 sel2) => \"sel1 ~ sel2\"
  - (and sel1 sel2 ...)   => \"sel1sel2...\" (combine)
  - (or sel1 sel2 ...)    => \"sel1, sel2, ...\" (group)
  - (pseudo \"hover\")    => \":hover\"
  - (pseudo-element \"before\") => \"::before\"
  - (not sel)            => \":not(sel)\"

Examples:
  (etaf-ecss-selector 'div)
  ;; => \"div\"

  (etaf-ecss-selector '(class \"container\"))
  ;; => \".container\"

  (etaf-ecss-selector '(and (tag \"div\") (class \"box\")))
  ;; => \"div.box\"

  (etaf-ecss-selector '(descendant \"nav\" (class \"link\")))
  ;; => \"nav .link\""
  (cond
   ;; String - use as-is
   ((stringp expr) expr)
   
   ;; Symbol - convert to tag name
   ((symbolp expr) (symbol-name expr))
   
   ;; List expression
   ((listp expr)
    (let ((type (car expr))
          (args (cdr expr)))
      (pcase type
        ;; Class selector
        ('class (concat "." (etaf-ecss--to-string (car args))))
        
        ;; ID selector
        ('id (concat "#" (etaf-ecss--to-string (car args))))
        
        ;; Tag selector
        ('tag (etaf-ecss--to-string (car args)))
        
        ;; Universal selector
        ('any "*")
        
        ;; Attribute selector
        ('attr
         (if (cdr args)
             (format "[%s=\"%s\"]" (car args) (cadr args))
           (format "[%s]" (car args))))
        
        ;; Child combinator
        ('child
         (mapconcat #'etaf-ecss-selector args " > "))
        
        ;; Descendant combinator
        ('descendant
         (mapconcat #'etaf-ecss-selector args " "))
        
        ;; Adjacent sibling
        ('sibling
         (mapconcat #'etaf-ecss-selector args " + "))
        
        ;; General sibling
        ('general-sibling
         (mapconcat #'etaf-ecss-selector args " ~ "))
        
        ;; Combine selectors (AND)
        ('and
         (mapconcat #'etaf-ecss-selector args ""))
        
        ;; Group selectors (OR)
        ('or
         (mapconcat #'etaf-ecss-selector args ", "))
        
        ;; Pseudo-class
        ('pseudo
         (concat ":" (etaf-ecss--to-string (car args))))
        
        ;; Pseudo-element
        ('pseudo-element
         (concat "::" (etaf-ecss--to-string (car args))))
        
        ;; Negation
        ('not
         (format ":not(%s)" (etaf-ecss-selector (car args))))
        
        ;; First/last child shortcuts
        ('first-child ":first-child")
        ('last-child ":last-child")
        ('nth-child
         (format ":nth-child(%s)" (car args)))
        
        ;; Unknown - treat first element as tag
        (_
         (etaf-ecss-selector (symbol-name type))))))
   
   ;; Unknown type
   (t (format "%s" expr))))

;;; CSS Property Expressions

(defun etaf-ecss--to-string (val)
  "Convert VAL to string for CSS."
  (cond
   ((stringp val) val)
   ((numberp val) (number-to-string val))
   ((symbolp val) (symbol-name val))
   (t (format "%s" val))))

;; Regex pattern for simple numeric values (integer or decimal)
;; Note: Does not handle scientific notation (e.g., '1e5') as CSS rarely uses it
(defconst etaf-ecss--numeric-pattern "^-?[0-9]+\\(\\.[0-9]+\\)?$"
  "Regex pattern for simple CSS numeric values.
This matches integers and decimals but not scientific notation,
which is acceptable as CSS properties rarely use scientific notation.")

(defun etaf-ecss--value-with-unit (val &optional default-unit)
  "Convert VAL to CSS value, adding DEFAULT-UNIT if numeric.
DEFAULT-UNIT defaults to \"px\".

Zero values are special-cased to return \"0\" without units,
as CSS accepts unitless zero for length values. This follows
the common CSS minification practice."
  (let ((unit (or default-unit "px")))
    (cond
     ;; Number - add unit (zero is special: no unit needed)
     ((numberp val)
      (if (zerop val)
          "0"
        (concat (number-to-string val) unit)))
     ;; Symbol - no unit (keywords like 'auto, 'inherit)
     ((symbolp val) (symbol-name val))
     ;; String - check if it needs unit
     ((stringp val)
      (if (string-match-p etaf-ecss--numeric-pattern val)
          (if (string= val "0")
              "0"
            (concat val unit))
        val))
     ;; Default
     (t (format "%s" val)))))

(defun etaf-ecss-property (prop &rest values)
  "Convert property expression to \"property: value\" string.

PROP is the property name (symbol or string).
VALUES are the property values.

Special handling:
- Numeric values get 'px' unit by default
- Multiple values are joined with spaces
- Certain properties have special value handling

Examples:
  (etaf-ecss-property 'background \"red\")
  ;; => \"background: red\"

  (etaf-ecss-property 'padding 10 20)
  ;; => \"padding: 10px 20px\"

  (etaf-ecss-property 'margin 0 'auto)
  ;; => \"margin: 0 auto\""
  (let* ((prop-name (etaf-ecss--to-string prop))
         (prop-symbol (if (symbolp prop) prop (intern prop-name)))
         ;; Determine the default unit based on property type
         (default-unit (etaf-ecss--property-default-unit prop-symbol))
         (value-str (mapconcat
                     (lambda (v)
                       (etaf-ecss--value-with-unit v default-unit))
                     values " ")))
    (format "%s: %s" prop-name value-str)))

(defun etaf-ecss--property-default-unit (prop)
  "Get default unit for property PROP.
Returns \"px\" for most properties, \"lh\" for vertical spacing in Emacs."
  ;; For Emacs compatibility, we use:
  ;; - px for horizontal properties (width, padding-left, etc.)
  ;; - lh for vertical properties (height, padding-top, etc.)
  (cond
   ;; Vertical properties - use lh (line-height units)
   ((memq prop '(height min-height max-height
                 padding-top padding-bottom
                 margin-top margin-bottom
                 top bottom
                 line-height))
    "lh")
   ;; Unitless properties
   ((memq prop '(opacity z-index flex-grow flex-shrink
                 font-weight line-height-ratio))
    "")
   ;; Default to px
   (t "px")))

(defun etaf-ecss--process-tailwind-string (class-string)
  "Process a space-separated string of Tailwind CSS classes.
CLASS-STRING is a string like \"flex items-center bg-red-500\".
Returns a list of CSS declaration strings.
Unrecognized classes are skipped with a warning message."
  (let ((classes (split-string class-string))
        (css-strings '()))
    (dolist (class classes)
      (when (and class (not (string-empty-p class)))
        (let ((css-props (etaf-tailwind-to-css class)))
          (if css-props
              (dolist (prop css-props)
                (push (format "%s: %s" (car prop) (cdr prop)) css-strings))
            ;; Log warning and skip unrecognized class
            (message "Warning: Tailwind class '%s' could not be converted to CSS, skipping" class)))))
    (nreverse css-strings)))

(defun etaf-ecss-declaration-block (&rest declarations)
  "Convert list of DECLARATIONS to CSS declaration block string.

Each declaration can be:
- A string of space-separated Tailwind classes: \"flex items-center bg-red-500\"
- A list like (property value ...) for standard CSS properties
- A cons cell like (property . value)
- A single-element list like (tailwind-class) for Tailwind CSS utilities

Returns string like \"{ property1: value1; property2: value2; }\"

Example:
  (etaf-ecss-declaration-block
    '(background \"red\")
    '(padding 10 20)
    '(margin 0 auto))
  ;; => \"{ background: red; padding: 10px 20px; margin: 0 auto; }\"

  ;; With Tailwind CSS utilities as string (recommended):
  (etaf-ecss-declaration-block \"flex items-center bg-red-500\" '(padding 10))
  ;; => \"{ display: flex; align-items: center; background-color: #ef4444; padding: 10px; }\"

  ;; With Tailwind CSS utilities as lists (verbose):
  (etaf-ecss-declaration-block
    '(flex)
    '(items-center)
    '(bg-red-500)
    '(padding 10))
  ;; => \"{ display: flex; align-items: center; background-color: #ef4444; padding: 10px; }\""
  (let ((decl-strings '())
        (string-decls '()))
    ;; First pass: collect all string declarations' CSS
    ;; Second pass: process other declarations
    (dolist (decl declarations)
      (cond
       ;; String: space-separated Tailwind classes - collect separately
       ((stringp decl)
        (let ((css-strs (etaf-ecss--process-tailwind-string decl)))
          (dolist (css-str css-strs)
            (push css-str string-decls))))
       ;; Check if it's a Tailwind CSS utility class (single-element list)
       ((etaf-ecss--tailwind-class-p decl)
        (let ((css-str (or (etaf-ecss--process-tailwind-decl decl)
                           ;; Fall back to treating it as a property if not recognized
                           (apply #'etaf-ecss-property decl))))
          (push css-str decl-strings)))
       ;; Standard cons/list declarations
       ((consp decl)
        (let ((css-str (if (listp (cdr decl))
                           ;; List: (prop val1 val2 ...)
                           (apply #'etaf-ecss-property decl)
                         ;; Cons: (prop . val)
                         (etaf-ecss-property (car decl) (cdr decl)))))
          (push css-str decl-strings)))
       ;; Other types - just convert to string
       (t (push (format "%s" decl) decl-strings))))
    ;; Combine: string declarations first (in order), then other declarations (reversed)
    (let ((all-decls (nconc (nreverse string-decls) (nreverse decl-strings))))
      (format "{ %s; }" (mapconcat #'identity all-decls "; ")))))

;;; CSS Rule and Stylesheet

(defun etaf-ecss (selector &rest declarations)
  "Create a CSS rule with SELECTOR and DECLARATIONS.

SELECTOR can be a string or selector expression.
DECLARATIONS are property expressions.

Example:
  (etaf-ecss \".box\"
    '(background \"red\")
    '(padding 10)
    '(border 1 solid \"black\"))
  ;; => \".box { background: red; padding: 10px; border: 1px solid black; }\"

  (etaf-ecss '(and (tag \"div\") (class \"container\"))
    '(width 800)
    '(margin 0 auto))
  ;; => \"div.container { width: 800px; margin: 0 auto; }\""
  (let ((sel-str (etaf-ecss-selector selector))
        (decl-block (apply #'etaf-ecss-declaration-block declarations)))
    (format "%s %s" sel-str decl-block)))

(defun etaf-ecss-stylesheet (&rest rules)
  "Create a stylesheet from multiple RULES.

Each rule is a list: (selector declarations ...)

Example:
  (etaf-ecss-stylesheet
    '(\".container\" (width 800) (margin 0 auto))
    '(\".box\" (background \"blue\") (padding 10))
    '((descendant \"nav\" \"a\") (color \"white\")))
  ;; => \".container { width: 800px; margin: 0 auto; }
  ;;     .box { background: blue; padding: 10px; }
  ;;     nav a { color: white; }\""
  (mapconcat
   (lambda (rule)
     (let ((selector (car rule))
           (declarations (cdr rule)))
       (apply #'etaf-ecss selector declarations)))
   rules "\n"))

;;; Macros for more convenient usage

(defmacro etaf-ecss-rule (selector &rest body)
  "Create CSS rule with SELECTOR and property declarations in BODY.

This macro allows a cleaner syntax without explicit quoting:

  (etaf-ecss-rule \".box\"
    (background \"red\")
    (padding 10 20))

Expands to:
  (etaf-ecss \".box\" '(background \"red\") '(padding 10 20))"
  `(etaf-ecss ,selector
         ,@(mapcar (lambda (decl) `',decl) body)))

(defmacro etaf-ecss-rules (&rest rules)
  "Create multiple CSS rules.

  (etaf-ecss-rules
    (\".container\"
      (width 800)
      (margin 0 auto))
    (\".box\"
      (background \"blue\")))

Returns the combined CSS string."
  `(etaf-ecss-stylesheet
    ,@(mapcar (lambda (rule)
                `'(,(car rule) ,@(cdr rule)))
              rules)))

;;; Convenience property functions

(defun etaf-ecss-props (&rest props)
  "Convert property expressions to CSS declarations alist.
PROPS can be:
- A string of space-separated Tailwind classes: \"flex items-center bg-red-500\"
- Property expressions: (property value ...)
- Single-element Tailwind classes: (flex), (bg-red-500)

Useful for :style attribute (list format) in TML:
  (div :style (etaf-ecss-props '(background \"red\") '(padding 10))
    \"content\")

With Tailwind CSS utilities as string (recommended):
  (div :style (etaf-ecss-props \"flex items-center bg-red-500\")
    \"content\")

Returns: ((background . \"red\") (padding . \"10px\"))"
  (let ((result '()))
    (dolist (prop props)
      (cond
       ;; String: space-separated Tailwind classes
       ((stringp prop)
        (let ((classes (split-string prop)))
          (dolist (class classes)
            (when (and class (not (string-empty-p class)))
              (let ((css-props (etaf-tailwind-to-css class)))
                (when css-props
                  (dolist (css-prop css-props)
                    (push css-prop result))))))))
       ;; Tailwind CSS utility class (single-element list)
       ((etaf-ecss--tailwind-class-p prop)
        (let* ((class-name (symbol-name (car prop)))
               (css-props (etaf-tailwind-to-css class-name)))
          (when css-props
            (dolist (css-prop css-props)
              (push css-prop result)))))
       ;; Standard property expression
       (t
        (let* ((name (car prop))
               (values (cdr prop))
               (name-str (etaf-ecss--to-string name))
               (default-unit (etaf-ecss--property-default-unit
                             (if (symbolp name) name (intern name-str))))
               (value-str (mapconcat
                          (lambda (v) (etaf-ecss--value-with-unit v default-unit))
                          values " ")))
          (push (cons (intern name-str) value-str) result)))))
    (nreverse result)))

;;; Integration with TML

(defun etaf-ecss-style (&rest declarations)
  "Create style string from DECLARATIONS.
Convenient for :style attribute in TML.

Each declaration can be:
- A string of space-separated Tailwind classes: \"flex items-center bg-red-500\"
- A standard property expression: (color \"red\")
- A Tailwind CSS utility: (flex), (bg-red-500), (p-4)

Example:
  (div :style (etaf-ecss-style '(color \"red\") '(padding 10))
    \"Hello\")
  ;; => \"color: red; padding: 10px\"

  ;; With Tailwind CSS utilities as string (recommended):
  (div :style (etaf-ecss-style \"flex items-center bg-red-500\")
    \"Hello\")
  ;; => \"display: flex; align-items: center; background-color: #ef4444\"

  ;; Mixed:
  (div :style (etaf-ecss-style \"flex items-center\" '(padding 10))
    \"Hello\")
  ;; => \"display: flex; align-items: center; padding: 10px\"

Returns: \"property1: value1; property2: value2\""
  (let ((decl-strings '())
        (string-decls '()))
    (dolist (decl declarations)
      (cond
       ;; String: space-separated Tailwind classes
       ((stringp decl)
        (let ((css-strs (etaf-ecss--process-tailwind-string decl)))
          (dolist (css-str css-strs)
            (push css-str string-decls))))
       ;; Tailwind CSS utility class (single-element list)
       ((etaf-ecss--tailwind-class-p decl)
        (let ((css-str (or (etaf-ecss--process-tailwind-decl decl)
                           (apply #'etaf-ecss-property decl))))
          (push css-str decl-strings)))
       ;; Standard property expression
       (t (push (apply #'etaf-ecss-property decl) decl-strings))))
    ;; Combine: string declarations first (in order), then other declarations (reversed)
    (let ((all-decls (nconc (nreverse string-decls) (nreverse decl-strings))))
      (mapconcat #'identity all-decls "; "))))

;;; Support for ECSS in style tags

(defmacro etaf-ecss-css (&rest rules)
  "Convert ECSS rules to CSS stylesheet string for use in style tags.

Each rule is an unquoted list: (selector declarations ...)
Declarations can be:
- A string of space-separated Tailwind classes (recommended)
- Property expressions like (width 800)

This macro is designed to be used within ETML style tags to write
CSS using ECSS syntax.

Example:
  ;; In ETML (using string format for Tailwind - recommended):
  (html
    (head
      (style (etaf-ecss-css
              (\".container\" \"flex items-center\" (width 800) (margin 0 auto))
              (\".box\" \"flex items-center bg-red-500 p-4\")
              (\".title\" \"text-lg font-bold\" (color \"#333\")))))
    (body
      (div :class \"container\"
        (div :class \"box\"
          (h1 :class \"title\" \"Hello ETAF!\")))))

  ;; The style tag content becomes:
  ;; .container { display: flex; align-items: center; width: 800px; margin: 0 auto; }
  ;; .box { display: flex; align-items: center; background-color: #ef4444; ... }
  ;; .title { font-size: 1.125rem; line-height: 1.75rem; font-weight: 700; color: #333; }"
  `(etaf-ecss-stylesheet
    ,@(mapcar (lambda (rule)
                `'(,(car rule) ,@(cdr rule)))
              rules)))

(provide 'etaf-ecss)
;;; etaf-ecss.el ends here
