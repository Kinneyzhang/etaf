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
;;
;; Basic Usage:
;;
;;   ;; Convert a single rule to CSS string
;;   (ecss ".box" 
;;     (background "red")
;;     (padding 10)
;;     (border 1 'solid "black"))
;;   ;; => ".box { background: red; padding: 10px; border: 1px solid black; }"
;;
;;   ;; Build a stylesheet from multiple rules
;;   (ecss-stylesheet
;;     (".container" (width 800) (margin 0 'auto))
;;     (".box" (background "blue") (padding 10)))
;;   ;; => ".container { width: 800px; margin: 0 auto; }\n.box { ... }"
;;
;; Selector Expressions:
;;
;;   (ecss-selector tag)         ; "tag"
;;   (ecss-selector (class "box")) ; ".box"
;;   (ecss-selector (id "main"))   ; "#main"
;;   (ecss-selector (tag "div"))   ; "div"
;;   (ecss-selector (child "ul" "li")) ; "ul > li"
;;   (ecss-selector (descendant "nav" "a")) ; "nav a"
;;   (ecss-selector (and (tag "div") (class "box"))) ; "div.box"
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

;;; CSS Selector Expressions

(defun ecss-selector (expr)
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
  (ecss-selector 'div)
  ;; => \"div\"

  (ecss-selector '(class \"container\"))
  ;; => \".container\"

  (ecss-selector '(and (tag \"div\") (class \"box\")))
  ;; => \"div.box\"

  (ecss-selector '(descendant \"nav\" (class \"link\")))
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
        ('class (concat "." (ecss--to-string (car args))))
        
        ;; ID selector
        ('id (concat "#" (ecss--to-string (car args))))
        
        ;; Tag selector
        ('tag (ecss--to-string (car args)))
        
        ;; Universal selector
        ('any "*")
        
        ;; Attribute selector
        ('attr
         (if (cdr args)
             (format "[%s=\"%s\"]" (car args) (cadr args))
           (format "[%s]" (car args))))
        
        ;; Child combinator
        ('child
         (mapconcat #'ecss-selector args " > "))
        
        ;; Descendant combinator
        ('descendant
         (mapconcat #'ecss-selector args " "))
        
        ;; Adjacent sibling
        ('sibling
         (mapconcat #'ecss-selector args " + "))
        
        ;; General sibling
        ('general-sibling
         (mapconcat #'ecss-selector args " ~ "))
        
        ;; Combine selectors (AND)
        ('and
         (mapconcat #'ecss-selector args ""))
        
        ;; Group selectors (OR)
        ('or
         (mapconcat #'ecss-selector args ", "))
        
        ;; Pseudo-class
        ('pseudo
         (concat ":" (ecss--to-string (car args))))
        
        ;; Pseudo-element
        ('pseudo-element
         (concat "::" (ecss--to-string (car args))))
        
        ;; Negation
        ('not
         (format ":not(%s)" (ecss-selector (car args))))
        
        ;; First/last child shortcuts
        ('first-child ":first-child")
        ('last-child ":last-child")
        ('nth-child
         (format ":nth-child(%s)" (car args)))
        
        ;; Unknown - treat first element as tag
        (_
         (ecss-selector (symbol-name type))))))
   
   ;; Unknown type
   (t (format "%s" expr))))

;;; CSS Property Expressions

(defun ecss--to-string (val)
  "Convert VAL to string for CSS."
  (cond
   ((stringp val) val)
   ((numberp val) (number-to-string val))
   ((symbolp val) (symbol-name val))
   (t (format "%s" val))))

;; Regex pattern for simple numeric values (integer or decimal)
;; Note: Does not handle scientific notation (e.g., '1e5') as CSS rarely uses it
(defconst ecss--numeric-pattern "^-?[0-9]+\\(\\.[0-9]+\\)?$"
  "Regex pattern for simple CSS numeric values.
This matches integers and decimals but not scientific notation,
which is acceptable as CSS properties rarely use scientific notation.")

(defun ecss--value-with-unit (val &optional default-unit)
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
      (if (string-match-p ecss--numeric-pattern val)
          (if (string= val "0")
              "0"
            (concat val unit))
        val))
     ;; Default
     (t (format "%s" val)))))

(defun ecss-property (prop &rest values)
  "Convert property expression to \"property: value\" string.

PROP is the property name (symbol or string).
VALUES are the property values.

Special handling:
- Numeric values get 'px' unit by default
- Multiple values are joined with spaces
- Certain properties have special value handling

Examples:
  (ecss-property 'background \"red\")
  ;; => \"background: red\"

  (ecss-property 'padding 10 20)
  ;; => \"padding: 10px 20px\"

  (ecss-property 'margin 0 'auto)
  ;; => \"margin: 0 auto\""
  (let* ((prop-name (ecss--to-string prop))
         (prop-symbol (if (symbolp prop) prop (intern prop-name)))
         ;; Determine the default unit based on property type
         (default-unit (ecss--property-default-unit prop-symbol))
         (value-str (mapconcat
                     (lambda (v)
                       (ecss--value-with-unit v default-unit))
                     values " ")))
    (format "%s: %s" prop-name value-str)))

(defun ecss--property-default-unit (prop)
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

(defun ecss-declaration-block (&rest declarations)
  "Convert list of DECLARATIONS to CSS declaration block string.

Each declaration is either:
- A list like (property value ...)
- A cons cell like (property . value)

Returns string like \"{ property1: value1; property2: value2; }\"

Example:
  (ecss-declaration-block
    '(background \"red\")
    '(padding 10 20)
    '(margin 0 auto))
  ;; => \"{ background: red; padding: 10px 20px; margin: 0 auto; }\""
  (let ((decl-strings
         (mapcar (lambda (decl)
                   (if (consp decl)
                       (if (listp (cdr decl))
                           ;; List: (prop val1 val2 ...)
                           (apply #'ecss-property decl)
                         ;; Cons: (prop . val)
                         (ecss-property (car decl) (cdr decl)))
                     decl))
                 declarations)))
    (format "{ %s; }" (mapconcat #'identity decl-strings "; "))))

;;; CSS Rule and Stylesheet

(defun ecss (selector &rest declarations)
  "Create a CSS rule with SELECTOR and DECLARATIONS.

SELECTOR can be a string or selector expression.
DECLARATIONS are property expressions.

Example:
  (ecss \".box\"
    '(background \"red\")
    '(padding 10)
    '(border 1 solid \"black\"))
  ;; => \".box { background: red; padding: 10px; border: 1px solid black; }\"

  (ecss '(and (tag \"div\") (class \"container\"))
    '(width 800)
    '(margin 0 auto))
  ;; => \"div.container { width: 800px; margin: 0 auto; }\""
  (let ((sel-str (ecss-selector selector))
        (decl-block (apply #'ecss-declaration-block declarations)))
    (format "%s %s" sel-str decl-block)))

(defun ecss-stylesheet (&rest rules)
  "Create a stylesheet from multiple RULES.

Each rule is a list: (selector declarations ...)

Example:
  (ecss-stylesheet
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
       (apply #'ecss selector declarations)))
   rules "\n"))

;;; Macros for more convenient usage

(defmacro ecss-rule (selector &rest body)
  "Create CSS rule with SELECTOR and property declarations in BODY.

This macro allows a cleaner syntax without explicit quoting:

  (ecss-rule \".box\"
    (background \"red\")
    (padding 10 20))

Expands to:
  (ecss \".box\" '(background \"red\") '(padding 10 20))"
  `(ecss ,selector
         ,@(mapcar (lambda (decl) `',decl) body)))

(defmacro ecss-rules (&rest rules)
  "Create multiple CSS rules.

  (ecss-rules
    (\".container\"
      (width 800)
      (margin 0 auto))
    (\".box\"
      (background \"blue\")))

Returns the combined CSS string."
  `(ecss-stylesheet
    ,@(mapcar (lambda (rule)
                `'(,(car rule) ,@(cdr rule)))
              rules)))

;;; Convenience property functions

(defun ecss-props (&rest props)
  "Convert property expressions to CSS declarations alist.
PROPS are pairs of (property value ...).

Useful for :css attribute in TML:
  (div :css (ecss-props (background \"red\") (padding 10))
    \"content\")

Returns: ((background . \"red\") (padding . \"10px\"))"
  (let ((result '()))
    (dolist (prop props)
      (let* ((name (car prop))
             (values (cdr prop))
             (name-str (ecss--to-string name))
             (default-unit (ecss--property-default-unit
                           (if (symbolp name) name (intern name-str))))
             (value-str (mapconcat
                        (lambda (v) (ecss--value-with-unit v default-unit))
                        values " ")))
        (push (cons (intern name-str) value-str) result)))
    (nreverse result)))

;;; Integration with TML

(defun ecss-style (&rest declarations)
  "Create style string from DECLARATIONS.
Convenient for :style attribute in TML.

Example:
  (div :style (ecss-style (color \"red\") (padding 10))
    \"Hello\")
  
Returns: \"color: red; padding: 10px\""
  (mapconcat
   (lambda (decl)
     (apply #'ecss-property decl))
   declarations "; "))

(provide 'etaf-ecss)
;;; etaf-ecss.el ends here
