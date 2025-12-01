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
   ;; Headings - font-size as ratio for em-based sizing
   "h1 { font-size: 1.6em; font-weight: bold; }"
   "h2 { font-size: 1.4em; font-weight: bold; }"
   "h3 { font-size: 1.3em; font-weight: bold; }"
   "h4 { font-size: 1.2em; font-weight: bold; }"
   "h5 { font-size: 1.1em; font-weight: bold; }"
   "h6 { font-size: 1.0em; font-weight: bold; }"
   
   ;; Lists
   "ul { list-style-type: disc; margin-top: 1lh; margin-bottom: 1lh; padding-left: 10px; }"
   "ol { list-style-type: decimal; margin-top: 1lh; margin-bottom: 1lh; padding-left: 10px; }"
   
   ;; Block quotes
   "blockquote { margin-top: 1lh; margin-bottom: 1lh; margin-left: 40px; margin-right: 40px; }"
   
   ;; Preformatted text
   "pre { font-family: monospace; white-space: pre; }"
   
   ;; Horizontal rule
   "hr { border-top: 1px solid; margin-top: 1lh; margin-bottom: 1lh; }"
   
   ;; Links
   "a { color: blue; text-decoration: underline; cursor: pointer; }"
   
   ;; Text formatting
   "em { font-style: italic; }"
   "strong { font-weight: bold; }"
   "b { font-weight: bold; }"
   "i { font-style: italic; }"
   "u { text-decoration: underline; }"
   "s { text-decoration: line-through; }"
   "del { text-decoration: line-through; }"
   "ins { text-decoration: underline; }"
   "mark { background-color: yellow; }"
   "small { font-size: smaller; }"
   "sub { vertical-align: sub; font-size: smaller; }"
   "sup { vertical-align: super; font-size: smaller; }"
   
   ;; Code
   "code { font-family: monospace; }"
   "kbd { font-family: monospace; }"
   "samp { font-family: monospace; }"
   "var { font-style: italic; }"
   
   ;; Abbreviations and citations
   "abbr { text-decoration: dotted underline; }"
   "cite { font-style: italic; }"
   
   ;; Form elements
   "button { padding-block: 0lh; padding-inline: 10px; border: 1px solid #ccc; cursor: pointer; }"
   "input { padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; border: 1px solid #ccc; }"
   "textarea { padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; border: 1px solid #ccc; font-family: inherit; }"
   "select { padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; border: 1px solid #ccc; }"
   "label { cursor: pointer; }"
   "fieldset { border: 1px solid #ccc; padding-top: 1lh; padding-bottom: 1lh; padding-left: 10px; padding-right: 10px; }"
   "legend { padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; }"
   
   ;; Tables
   "table { border-collapse: collapse; }"
   "th { font-weight: bold; text-align: center; padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; }"
   "td { padding-top: 0lh; padding-bottom: 0lh; padding-left: 5px; padding-right: 5px; }"
   
   ;; Figures
   "figure { margin-top: 1lh; margin-bottom: 1lh; margin-left: 40px; margin-right: 40px; }"
   
   ;; Dialog
   "dialog { position: absolute; border: 1px solid #ccc; padding-top: 1lh; padding-bottom: 1lh; padding-left: 10px; padding-right: 10px; background-color: white; }"
   
   ;; Summary
   "summary { cursor: pointer; }")
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
