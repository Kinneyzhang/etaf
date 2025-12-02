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
  (etaf-ecss
   ;; Display property for block-level elements
   "div, p, h1, h2, h3, h4, h5, h6{block}"
   "header, footer, section, article, aside, nav, main{block}"
   "ul, ol, blockquote, pre, hr, form, fieldset{block}"
   "figure, figcaption, details, dialog{block}"
   "table{table}"
   "thead{table-header-group}"
   "tbody{table-row-group}"
   "tfoot{table-footer-group}"
   "tr{table-row}"
   "th, td{table-cell}"
   "caption{table-caption}"
   "li{list-item}"

   "button{inline}"
   "button:hover{bg-gray-200}"
   "button:active{bg-gray-300}"
   "button:disabled{bg-gray-100 text-gray-500 cursor-not-allowed}"
   
   ;; Display property for inline-block elements
   "input, textarea, select, img, video, canvas, svg{inline-block}"
   
   ;; Inline elements have display: inline by default (browser default)
   ;; No need to explicitly set for: span, a, em, strong, b, i, u, s, del, ins, mark, small, sub, sup, code, kbd, samp, var, abbr, cite, q, label, audio, option, summary, progress, meter
   ;; Note: br is also inline but is defined as self-closing tag in tag definitions
   
   ;; Headings with numeric font-size (using new text-N.N format for lh units)
   "h1{text-1.6 font-bold}"
   "h2{text-1.4 font-bold}"
   "h3{text-1.3 font-bold}"
   "h4{text-1.2 font-bold}"
   "h5{text-1.1 font-bold}"
   "h6{text-1.0 font-bold}"
   
   ;; Lists
   "ul{mt-1 mb-1 pl-10}"
   "ol{mt-1 mb-1 pl-10}"
   
   ;; Block quotes
   "blockquote{mt-1 mb-1 ml-4 mr-4}"
   
   ;; Preformatted text
   "pre{font-mono whitespace-pre}"
   
   ;; Horizontal rule
   "hr{mt-1 mb-1}"
   
   ;; Links
   "a{text-blue underline cursor-pointer}"
   "a:hover{text-blue-700}"
   
   ;; Text formatting
   "em{italic}"
   "strong{font-bold}"
   "b{font-bold}"
   "i{italic}"
   "u{underline}"
   "s{line-through}"
   "del{line-through}"
   "ins{underline}"
   "mark{bg-yellow}"
   ;; Note: small, sub, sup would use font-size which requires custom values
   "sub{align-sub}"
   "sup{align-super}"
   
   ;; Code
   "code{font-mono}"
   "kbd{font-mono}"
   "samp{font-mono}"
   "var{italic}"
   
   ;; Abbreviations and citations
   ;; Note: abbr would use text-decoration: dotted underline (no Tailwind equivalent)
   "cite{italic}"
   
   ;; Form elements
   "input{border border-gray-300}"
   "input:focus{border-blue-500}"
   "input:disabled{bg-gray-100 text-gray-500}"
   "textarea{border border-gray-300 font-mono}"
   "textarea:focus{border-blue-500}"
   "select{border border-gray-300}"
   "label{cursor-pointer}"
   "fieldset{border border-gray-300}"
   ;; Note: legend would use padding which could be added if needed
   
   ;; Tables
   "table{border-collapse}"
   "th{font-bold text-center}"
   ;; Note: td would use padding which could be added if needed
   
   ;; Figures
   "figure{mt-1 mb-1 ml-4 mr-4}"
   
   ;; Dialog
   "dialog{absolute border border-gray-300 bg-white}"
   
   ;; Summary
   "summary{cursor-pointer}")
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
