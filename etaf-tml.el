;;; etaf-tml.el --- TML (Template Markup Language) to DOM conversion -*- lexical-binding: t; -*-

;;; Commentary:

;; TML is an S-expression based markup language for Emacs.
;; This module converts TML to DOM format.
;;
;; Key features:
;; - Converts plist-based TML to alist-based DOM
;; - Supports :css attribute for inline CSS rules (Emacs-specific)
;;
;; Example:
;;   (div :class "box" :css ((background . "red") (padding . "10px"))
;;     "Hello")
;;
;; The :css attribute accepts:
;; - An alist of CSS properties: ((property . value) ...)
;; - This is converted to a style attribute in the DOM

;;; Utility functions

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

;;; TML to DOM conversion

(defun etaf-tml-to-dom (sexp)
  "Convert S-expression from format 1 (plist) to format 2 (alist).
Format 1: (tag :attr1 val1 :attr2 val2 child1 child2 ...)
Format 2: (tag ((attr1 . val1) (attr2 . val2)) child1 child2 ...)

Supports :css attribute for inline CSS rules:
  (div :css ((background . \"red\") (padding . \"10px\")) ...)
This is converted to:
  (div ((style . \"background: red; padding: 10px\")) ...)"
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
        ;; Process :css attribute - convert to style
        (let* ((attr-alist (etaf-plist-to-alist attrs))
               (css-attr (assq 'css attr-alist))
               (style-attr (assq 'style attr-alist)))
          ;; If :css is present, merge it into style
          (when css-attr
            (let* ((css-rules (cdr css-attr))
                   (css-string (etaf-css-alist-to-string css-rules))
                   (existing-style (cdr style-attr)))
              ;; Remove the css attribute from alist
              (setq attr-alist (delq css-attr attr-alist))
              ;; Update or add style attribute
              (if style-attr
                  ;; Append to existing style
                  (setcdr style-attr 
                          (concat existing-style "; " css-string))
                ;; Add new style attribute
                (push (cons 'style css-string) attr-alist))))
          (let ((children (mapcar #'etaf-tml-to-dom rest)))
            (cons tag (cons attr-alist children))))))))

(provide 'etaf-tml)
;;; etaf-tml.el ends here
