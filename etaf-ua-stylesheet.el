;;; etaf-ua-stylesheet.el --- User Agent Stylesheet for ETML Tags -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, stylesheet, user-agent
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; User Agent Stylesheet for ETAF
;;
;; This module provides the default styles for all ETML tags, similar to
;; how browsers implement User Agent Stylesheets. These styles have the
;; lowest priority in the CSS cascade:
;;
;; Priority order (low to high):
;; 1. User Agent Stylesheet (this file)
;; 2. Author Stylesheets (from <style> tags or external CSS)
;; 3. Inline Styles (from style attribute)
;;
;; The User Agent Stylesheet is defined using the etaf-ecss module,
;; which provides a convenient syntax for writing CSS rules.
;;
;; Usage:
;;
;;   ;; Get the UA stylesheet as a CSS string
;;   (etaf-ua-stylesheet-get-css)
;;
;;   ;; Get the UA stylesheet as parsed rules
;;   (etaf-ua-stylesheet-get-rules)

;;; Code:

(require 'etaf-ecss)
(require 'etaf-css-parser)

;;; User Agent Stylesheet Definition

(defconst etaf-ua-stylesheet-css
  (etaf-ecss-stylesheet
   ;; Display property for block-level elements
   '("div, p, h1, h2, h3, h4, h5, h6" (display block))
   '("header, footer, section, article, aside, nav, main" (display block))
   '("ul, ol, blockquote, pre, hr, form, fieldset" (display block))
   '("figure, figcaption, details, dialog" (display block))
   '("table" (display table))
   '("thead" (display table-header-group))
   '("tbody" (display table-row-group))
   '("tfoot" (display table-footer-group))
   '("tr" (display table-row))
   '("th, td" (display table-cell))
   '("caption" (display table-caption))
   '("li" (display list-item))

   '("button" (display inline))
   
   ;; Display property for inline-block elements
   '("input, textarea, select, img, video, canvas, svg" (display inline-block))
   
   ;; Inline elements have display: inline by default (browser default)
   ;; No need to explicitly set for: span, a, em, strong, b, i, u, s, del, ins, mark, small, sub, sup, code, kbd, samp, var, abbr, cite, q, label, audio, option, summary, progress, meter
   ;; Note: br is also inline but is defined as self-closing tag in tag definitions
   
   '("h1" (font-size "1.6em") (font-weight bold))
   '("h2" (font-size "1.4lh") (font-weight bold))
   '("h3" (font-size "1.3lh") (font-weight bold))
   '("h4" (font-size "1.2lh") (font-weight bold))
   '("h5" (font-size "1.1lh") (font-weight bold))
   '("h6" (font-size "1.0lh") (font-weight bold))
   
   ;; Lists
   '("ul" (list-style-type disc) (margin-top 1) (margin-bottom 1) (padding-left 10))
   '("ol" (list-style-type decimal) (margin-top 1) (margin-bottom 1) (padding-left 10))
   
   ;; Block quotes
   '("blockquote" (margin-top 1) (margin-bottom 1) (margin-left 40) (margin-right 40))
   
   ;; Preformatted text
   '("pre" (font-family monospace) (white-space pre))
   
   ;; Horizontal rule
   '("hr" (border-top "1px solid") (margin-top 1) (margin-bottom 1))
   
   ;; Links
   '("a" (color blue) (text-decoration underline) (cursor pointer))
   
   ;; Text formatting
   '("em" (font-style italic))
   '("strong" (font-weight bold))
   '("b" (font-weight bold))
   '("i" (font-style italic))
   '("u" (text-decoration underline))
   '("s" (text-decoration line-through))
   '("del" (text-decoration line-through))
   '("ins" (text-decoration underline))
   '("mark" (background-color yellow))
   '("small" (font-size smaller))
   '("sub" (vertical-align sub) (font-size smaller))
   '("sup" (vertical-align super) (font-size smaller))
   
   ;; Code
   '("code" (font-family monospace))
   '("kbd" (font-family monospace))
   '("samp" (font-family monospace))
   '("var" (font-style italic))
   
   ;; Abbreviations and citations
   '("abbr" (text-decoration "dotted underline"))
   '("cite" (font-style italic))
   
   ;; Form elements
   ;; '("button" (padding-block 0) (padding-inline 10) (border "1px solid #ccc") (cursor pointer))
   '("input" (padding-block 0) (padding-inline 5) (border "1px solid #ccc"))
   '("textarea" (padding-block 0) (padding-inline 5) (border "1px solid #ccc") (font-family inherit))
   '("select" (padding-block 0) (padding-inline 5) (border "1px solid #ccc"))
   '("label" (cursor pointer))
   '("fieldset" (border "1px solid #ccc") (padding-block 1) (padding-inline 10))
   '("legend" (padding-block 0) (padding-inline 5))
   
   ;; Tables
   '("table" (border-collapse collapse))
   '("th" (font-weight bold) (text-align center) (padding-block 0) (padding-inline 5))
   '("td" (padding-block 0) (padding-inline 5))
   
   ;; Figures
   '("figure" (margin-top 1) (margin-bottom 1) (margin-left 40) (margin-right 40))
   
   ;; Dialog
   '("dialog" (position absolute) (border "1px solid #ccc") (padding-block 1) (padding-inline 10) (background-color white))
   
   ;; Summary
   '("summary" (cursor pointer)))
  "User Agent Stylesheet for ETML tags.
This stylesheet contains the default styles for all HTML-like tags in ETAF.
These styles have the lowest priority in the CSS cascade.")

;;; Parsed Rules Cache

(defvar etaf-ua-stylesheet--parsed-rules nil
  "Cached parsed rules from the User Agent Stylesheet.
Initialized lazily on first access to avoid parsing overhead during load.")

(defun etaf-ua-stylesheet-get-rules ()
  "Get the parsed rules from the User Agent Stylesheet.
Returns a list of CSS rules with specificity and source information.
Results are cached to avoid repeated parsing."
  (unless etaf-ua-stylesheet--parsed-rules
    (setq etaf-ua-stylesheet--parsed-rules
          (let ((rules (etaf-css-parse-stylesheet etaf-ua-stylesheet-css)))
            ;; Mark rules as coming from UA stylesheet for priority handling
            ;; UA stylesheet has the lowest priority (source 'ua)
            (mapcar (lambda (rule)
                      (plist-put rule :source 'ua)
                      rule)
                    rules))))
  etaf-ua-stylesheet--parsed-rules)

(defun etaf-ua-stylesheet-get-css ()
  "Get the User Agent Stylesheet as a CSS string.
This is the complete UA stylesheet that can be used in debugging or
for external consumption."
  etaf-ua-stylesheet-css)

(provide 'etaf-ua-stylesheet)
;;; etaf-ua-stylesheet.el ends here
