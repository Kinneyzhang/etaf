;;; etaf-type.el --- Core Type Definitions for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: types, structs, data-structures
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; ETAF Core Type Definitions
;; ==========================
;;
;; This module defines the fundamental data structures used throughout ETAF
;; using Emacs Lisp's `cl-defstruct` for type-safe, performant data access.
;;
;; Benefits of using cl-defstruct over plists:
;; 1. Type checking with predicates (e.g., `etaf-box-model-p`)
;; 2. Faster access via direct slot accessors (vs plist-get)
;; 3. Clearer API with named constructors
;; 4. Memory efficiency with vector-based storage
;; 5. Better documentation and code navigation
;;
;; Module Structure:
;; -----------------
;; 1. Type Predicates - Basic type checking utilities
;; 2. Box Model Types - CSS box model structures
;; 3. VNode Types - Virtual DOM node structures
;; 4. CSS Types - CSS rule and style structures
;; 5. Reactive Types - Reactivity system structures
;; 6. Component Types - Component definition structures
;;
;; Usage Example:
;;
;;   ;; Create a box model
;;   (setq box (make-etaf-box-model :content-width 100 :content-height 50))
;;
;;   ;; Access fields
;;   (etaf-box-model-content-width box)  ;; => 100
;;
;;   ;; Type checking
;;   (etaf-box-model-p box)  ;; => t

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Section 1: Type Predicates
;;; ============================================================================
;;
;; Basic type checking utilities for common data types.

(defun etaf-type-plistp (value)
  "Check if VALUE is a property list.
A plist is a list of alternating keyword-value pairs."
  (plistp value))

(defun etaf-type-alistp (value)
  "Check if VALUE is an association list.
An alist is a list of cons cells (key . value)."
  (and (listp value)
       (cl-every #'consp value)))

;;; ============================================================================
;;; Section 2: Box Model Types
;;; ============================================================================
;;
;; CSS Box Model structures for layout computation.
;;
;; The box model represents the visual layout of an element:
;;
;;   ┌─────────────────────────────────────┐
;;   │           margin (外边距)            │
;;   │  ┌───────────────────────────────┐  │
;;   │  │      border (边框)            │  │
;;   │  │  ┌─────────────────────────┐  │  │
;;   │  │  │   padding (内边距)      │  │  │
;;   │  │  │  ┌───────────────────┐  │  │  │
;;   │  │  │  │  content (内容)   │  │  │  │
;;   │  │  │  │                   │  │  │  │
;;   │  │  │  └───────────────────┘  │  │  │
;;   │  │  └─────────────────────────┘  │  │
;;   │  └───────────────────────────────┘  │
;;   └─────────────────────────────────────┘

(cl-defstruct (etaf-sides
               (:constructor etaf-sides-create)
               (:copier nil))
  "Four-sided measurement structure for padding, margin, etc.
Used to represent values that apply to all four sides of a box."
  (top 0 :type number :documentation "Top side value in pixels.")
  (right 0 :type number :documentation "Right side value in pixels.")
  (bottom 0 :type number :documentation "Bottom side value in pixels.")
  (left 0 :type number :documentation "Left side value in pixels."))

(cl-defstruct (etaf-border
               (:constructor etaf-border-create)
               (:copier nil))
  "Border structure with widths and colors for all four sides."
  ;; Widths
  (top-width 0 :type number :documentation "Top border width in pixels.")
  (right-width 0 :type number :documentation "Right border width in pixels.")
  (bottom-width 0 :type number :documentation "Bottom border width in pixels.")
  (left-width 0 :type number :documentation "Left border width in pixels.")
  ;; Colors
  (top-color nil :type (or string null) :documentation "Top border color.")
  (right-color nil :type (or string null) :documentation "Right border color.")
  (bottom-color nil :type (or string null) :documentation "Bottom border color.")
  (left-color nil :type (or string null) :documentation "Left border color."))

(cl-defstruct (etaf-overflow
               (:constructor etaf-overflow-create)
               (:copier nil))
  "Overflow handling configuration for an element."
  (overflow-y "visible"
   :type string
   :documentation "Vertical overflow: visible, hidden, auto, scroll.")
  (v-scroll-bar-type nil
   :type (or symbol null)
   :documentation "Vertical scrollbar style type symbol.")
  (v-scroll-bar-direction 'right
   :type symbol
   :documentation "Scrollbar position: left or right.")
  (scroll-thumb-color nil
   :type (or string null)
   :documentation "Scrollbar thumb color.")
  (scroll-track-color nil
   :type (or string null)
   :documentation "Scrollbar track color."))

(cl-defstruct (etaf-box-model
               (:constructor etaf-box-model-create)
               (:copier nil))
  "CSS Box Model structure.
Represents the complete box model for layout computation including
content dimensions, padding, border, margin, and overflow settings."
  ;; Box sizing mode
  (box-sizing "content-box"
   :type string
   :documentation "Box sizing mode: content-box or border-box.")
  ;; Content dimensions
  (content-width 0
   :type number
   :documentation "Content area width in pixels.")
  (content-height 0
   :type number
   :documentation "Content area height in pixels/lines.")
  ;; Padding (inner spacing)
  (padding nil
   :type (or etaf-sides null)
   :documentation "Padding around content.")
  ;; Border
  (border nil
   :type (or etaf-border null)
   :documentation "Border around padding.")
  ;; Margin (outer spacing)
  (margin nil
   :type (or etaf-sides null)
   :documentation "Margin around border.")
  ;; Overflow handling
  (overflow nil
   :type (or etaf-overflow null)
   :documentation "Overflow configuration.")
  ;; Layout metadata
  (width-keyword nil
   :type (or symbol null)
   :documentation "Original width keyword (auto, fit-content, etc.).")
  (height-keyword nil
   :type (or symbol null)
   :documentation "Original height keyword (auto, fit-content, etc.).")
  (needs-content-width nil
   :type boolean
   :documentation "Whether width needs content-based calculation.")
  (needs-content-height nil
   :type boolean
   :documentation "Whether height needs content-based calculation.")
  (parent-width nil
   :type (or number null)
   :documentation "Parent container's available width.")
  (parent-height nil
   :type (or number null)
   :documentation "Parent container's available height."))

;;; Box Model Helper Functions

(defun etaf-box-model-padding-width (box)
  "Get total horizontal padding (left + right) of BOX."
  (if-let ((padding (etaf-box-model-padding box)))
      (+ (etaf-sides-left padding) (etaf-sides-right padding))
    0))

(defun etaf-box-model-padding-height (box)
  "Get total vertical padding (top + bottom) of BOX."
  (if-let ((padding (etaf-box-model-padding box)))
      (+ (etaf-sides-top padding) (etaf-sides-bottom padding))
    0))

(defun etaf-box-model-border-width (box)
  "Get total horizontal border width (left + right) of BOX."
  (if-let ((border (etaf-box-model-border box)))
      (+ (etaf-border-left-width border) (etaf-border-right-width border))
    0))

(defun etaf-box-model-border-height (box)
  "Get total vertical border width (top + bottom) of BOX."
  (if-let ((border (etaf-box-model-border box)))
      (+ (etaf-border-top-width border) (etaf-border-bottom-width border))
    0))

(defun etaf-box-model-margin-width (box)
  "Get total horizontal margin (left + right) of BOX."
  (if-let ((margin (etaf-box-model-margin box)))
      (+ (etaf-sides-left margin) (etaf-sides-right margin))
    0))

(defun etaf-box-model-margin-height (box)
  "Get total vertical margin (top + bottom) of BOX."
  (if-let ((margin (etaf-box-model-margin box)))
      (+ (etaf-sides-top margin) (etaf-sides-bottom margin))
    0))

(defun etaf-box-model-total-width (box)
  "Calculate total width of BOX (content + padding + border + margin)."
  (+ (etaf-box-model-content-width box)
     (etaf-box-model-padding-width box)
     (etaf-box-model-border-width box)
     (etaf-box-model-margin-width box)))

(defun etaf-box-model-total-height (box)
  "Calculate total height of BOX (content + padding + border + margin)."
  (+ (etaf-box-model-content-height box)
     (etaf-box-model-padding-height box)
     (etaf-box-model-border-height box)
     (etaf-box-model-margin-height box)))

;;; ============================================================================
;;; Section 3: VNode Types
;;; ============================================================================
;;
;; Virtual DOM node structures following Vue 3's design.
;;
;; The VNode system provides:
;; - Efficient diffing and patching
;; - Component abstraction
;; - Event handling coordination
;; - Static node optimization

;; VNode special type symbols
(defconst etaf-vnode-type-fragment 'v-fgt
  "Fragment VNode type - allows multiple root nodes without wrapper.")

(defconst etaf-vnode-type-text 'v-txt
  "Text VNode type - represents text content.")

(defconst etaf-vnode-type-comment 'v-cmt
  "Comment VNode type - represents HTML comments.")

(defconst etaf-vnode-type-static 'v-stc
  "Static VNode type - fully static, never needs update.")

;; PatchFlag constants for optimization
(defconst etaf-vnode-patch-flag-text 1
  "Dynamic text content flag.")

(defconst etaf-vnode-patch-flag-class 2
  "Dynamic class binding flag.")

(defconst etaf-vnode-patch-flag-style 4
  "Dynamic style binding flag.")

(defconst etaf-vnode-patch-flag-props 8
  "Dynamic props (except class/style) flag.")

(defconst etaf-vnode-patch-flag-hoisted -1
  "Static node (hoisted, never needs update).")

;; ShapeFlag constants for node type identification
(defconst etaf-vnode-shape-element 1
  "Element VNode shape flag.")

(defconst etaf-vnode-shape-text-children 8
  "VNode has text children flag.")

(defconst etaf-vnode-shape-array-children 16
  "VNode has array children flag.")

(cl-defstruct (etaf-vnode
               (:constructor etaf-vnode-create)
               (:copier nil))
  "Virtual DOM Node structure (Vue 3 compatible).
VNode is a pure data structure describing UI, not storing actual DOM."
  ;; Core properties
  (type nil
   :type (or symbol function)
   :documentation "Node type: element symbol, component, or special type.")
  (tag nil
   :type (or symbol null)
   :documentation "Tag name for element nodes (div, span, etc.).")
  (props nil
   :type list
   :documentation "Properties plist including attributes and event handlers.")
  (children nil
   :type (or list string null)
   :documentation "Child nodes (list of vnodes) or text content.")
  (key nil
   :documentation "Unique key for diff optimization.")
  ;; Optimization flags
  (patch-flag 0
   :type integer
   :documentation "Optimization hints for patching.")
  (shape-flag 0
   :type integer
   :documentation "Node shape for type checking.")
  (dynamic-props nil
   :type list
   :documentation "List of dynamic property names.")
  ;; Runtime state
  (el nil
   :documentation "Reference to actual DOM element (set by renderer).")
  (parent nil
   :type (or etaf-vnode null)
   :documentation "Parent VNode reference.")
  (scope-id nil
   :type (or string null)
   :documentation "Scope ID for scoped styles.")
  ;; Component state
  (component nil
   :documentation "Component instance (for component vnodes).")
  (ctx nil
   :documentation "Current rendering context."))

;;; VNode Helper Functions

(defun etaf-vnode-element-p (vnode)
  "Check if VNODE is an element node."
  (and (etaf-vnode-p vnode)
       (let ((shape (etaf-vnode-shape-flag vnode)))
         (not (zerop (logand shape etaf-vnode-shape-element))))))

(defun etaf-vnode-text-p (vnode)
  "Check if VNODE is a text node."
  (and (etaf-vnode-p vnode)
       (eq (etaf-vnode-type vnode) etaf-vnode-type-text)))

(defun etaf-vnode-fragment-p (vnode)
  "Check if VNODE is a fragment node."
  (and (etaf-vnode-p vnode)
       (eq (etaf-vnode-type vnode) etaf-vnode-type-fragment)))

(defun etaf-vnode-static-p (vnode)
  "Check if VNODE is fully static (no dynamic content)."
  (and (etaf-vnode-p vnode)
       (let ((flag (etaf-vnode-patch-flag vnode)))
         (or (null flag)
             (zerop flag)
             (= flag etaf-vnode-patch-flag-hoisted)))))

(defun etaf-vnode-same-type-p (vnode1 vnode2)
  "Check if VNODE1 and VNODE2 are the same type for diffing.
Same type means same type property and same key (or both nil keys)."
  (and (eq (etaf-vnode-type vnode1) (etaf-vnode-type vnode2))
       (let ((key1 (etaf-vnode-key vnode1))
             (key2 (etaf-vnode-key vnode2)))
         (or (and (null key1) (null key2))
             (equal key1 key2)))))

;;; ============================================================================
;;; Section 4: CSS Types
;;; ============================================================================
;;
;; CSS rule and declaration structures for the CSS Object Model (CSSOM).

(cl-defstruct (etaf-css-declaration
               (:constructor etaf-css-declaration-create)
               (:copier nil))
  "CSS property declaration with value and importance."
  (property nil
   :type symbol
   :documentation "CSS property name as symbol (e.g., 'color, 'font-size).")
  (value nil
   :type string
   :documentation "CSS property value as string.")
  (important nil
   :type boolean
   :documentation "Whether this declaration has !important flag."))

(cl-defstruct (etaf-css-rule
               (:constructor etaf-css-rule-create)
               (:copier nil))
  "CSS rule structure containing selector and declarations."
  (selector nil
   :type string
   :documentation "CSS selector string (e.g., \"div.container #main\").")
  (declarations nil
   :type list
   :documentation "List of etaf-css-declaration structs.")
  (specificity nil
   :type list
   :documentation "Specificity as (id-count class-count type-count).")
  (source nil
   :type symbol
   :documentation "Rule source: 'ua, 'style-tag, or 'inline.")
  (order 0
   :type integer
   :documentation "Document order for cascade precedence.")
  (media nil
   :type (or string null)
   :documentation "Media query string (if within @media rule).")
  (node nil
   :documentation "DOM node reference (for inline styles only)."))

(cl-defstruct (etaf-cssom
               (:constructor etaf-cssom-create)
               (:copier nil))
  "CSS Object Model structure containing all parsed CSS rules.
The CSSOM is the central structure for CSS processing, containing
all rules organized by source and optimized for querying."
  (ua-rules nil
   :type list
   :documentation "User Agent stylesheet rules (lowest priority).")
  (style-rules nil
   :type list
   :documentation "Author stylesheet rules (<style> tags).")
  (inline-rules nil
   :type list
   :documentation "Inline style rules (highest priority).")
  (all-rules nil
   :type list
   :documentation "All rules in cascade order.")
  (rule-index nil
   :documentation "Rule index hash-table for quick lookup.")
  (cache nil
   :documentation "Computed style cache hash-table.")
  (media-env nil
   :type list
   :documentation "Media query environment alist."))

;;; ============================================================================
;;; Section 5: Reactive Types
;;; ============================================================================
;;
;; Reactivity system structures following Vue 3's Composition API design.
;;
;; The reactive system provides:
;; - ref: Basic reactive references
;; - computed: Derived reactive values with caching
;; - watch/watchEffect: Side effect tracking

(cl-defstruct (etaf-ref
               (:constructor etaf-ref--create)
               (:copier nil))
  "Reactive reference structure.
A ref is a reactive container that holds a single value.
When the value changes, all dependent effects are notified."
  (id nil
   :documentation "Unique identifier for this ref.")
  (value nil
   :documentation "The current value held by this ref.")
  (watchers nil
   :type list
   :documentation "List of watcher callbacks."))

(defun etaf-ref-make (value)
  "Create a new reactive ref with initial VALUE."
  (etaf-ref--create :id (cl-gensym "etaf-ref-")
                    :value value
                    :watchers nil))

(cl-defstruct (etaf-computed
               (:constructor etaf-computed--create)
               (:copier nil))
  "Computed reactive value structure.
A computed value is derived from other reactive values and is
lazily evaluated and cached."
  (id nil
   :documentation "Unique identifier for this computed.")
  (getter nil
   :type function
   :documentation "Function to compute the value.")
  (value nil
   :documentation "Cached computed value.")
  (dirty t
   :type boolean
   :documentation "Whether the value needs recomputation.")
  (effect nil
   :type function
   :documentation "Effect runner for dependency tracking."))

(defun etaf-computed-make (getter)
  "Create a new computed value with GETTER function."
  (etaf-computed--create :id (cl-gensym "etaf-computed-")
                         :getter getter
                         :value nil
                         :dirty t
                         :effect nil))

(cl-defstruct (etaf-effect
               (:constructor etaf-effect-create)
               (:copier nil))
  "Reactive effect structure.
Effects are functions that automatically re-run when their
reactive dependencies change."
  (fn nil
   :type function
   :documentation "The effect function to run.")
  (scheduler nil
   :type (or function null)
   :documentation "Custom scheduler for effect execution.")
  (deps nil
   :type list
   :documentation "List of (target . key) dependency pairs.")
  (active t
   :type boolean
   :documentation "Whether this effect is currently active.")
  (lazy nil
   :type boolean
   :documentation "Whether to skip initial execution.")
  (on-stop nil
   :type (or function null)
   :documentation "Callback when effect is stopped."))

;;; ============================================================================
;;; Section 6: Component Types
;;; ============================================================================
;;
;; Component definition structures for the component system.

(cl-defstruct (etaf-component-def
               (:constructor etaf-component-def-create)
               (:copier nil))
  "Component definition structure.
Supports both Vue 2 Options API and Vue 3 Composition API styles."
  (name nil
   :type symbol
   :documentation "Component name symbol.")
  ;; Composition API
  (props nil
   :type list
   :documentation "List of prop names or prop definitions.")
  (setup nil
   :type (or function null)
   :documentation "Setup function (props) -> reactive state.")
  ;; Options API
  (data nil
   :type (or function null)
   :documentation "Data function returning initial state.")
  (methods nil
   :type list
   :documentation "Plist of method functions.")
  (computed nil
   :type list
   :documentation "Plist of computed property getters.")
  (watch nil
   :type list
   :documentation "Plist of watcher functions.")
  ;; Lifecycle hooks
  (mounted nil
   :type (or function null)
   :documentation "Called when component is mounted.")
  (updated nil
   :type (or function null)
   :documentation "Called when component updates.")
  (unmounted nil
   :type (or function null)
   :documentation "Called when component is unmounted.")
  ;; Template
  (template nil
   :documentation "Component template (ETML or function).")
  (render nil
   :type (or function null)
   :documentation "Custom render function.")
  (emits nil
   :type list
   :documentation "List of events this component can emit."))

;;; ============================================================================
;;; Section 7: Conversion Utilities
;;; ============================================================================
;;
;; Functions to convert between legacy plist format and new struct format.
;; These ensure backward compatibility during the transition.

(defun etaf-box-model-from-plist (plist)
  "Create an etaf-box-model struct from legacy PLIST format.
This provides backward compatibility with existing code."
  (let* ((content (plist-get plist :content))
         (padding-plist (plist-get plist :padding))
         (border-plist (plist-get plist :border))
         (margin-plist (plist-get plist :margin))
         (overflow-plist (plist-get plist :overflow)))
    (etaf-box-model-create
     :box-sizing (or (plist-get plist :box-sizing) "content-box")
     :content-width (or (plist-get content :width) 0)
     :content-height (or (plist-get content :height) 0)
     :padding (when padding-plist
                (etaf-sides-create
                 :top (or (plist-get padding-plist :top) 0)
                 :right (or (plist-get padding-plist :right) 0)
                 :bottom (or (plist-get padding-plist :bottom) 0)
                 :left (or (plist-get padding-plist :left) 0)))
     :border (when border-plist
               (etaf-border-create
                :top-width (or (plist-get border-plist :top-width) 0)
                :right-width (or (plist-get border-plist :right-width) 0)
                :bottom-width (or (plist-get border-plist :bottom-width) 0)
                :left-width (or (plist-get border-plist :left-width) 0)
                :top-color (plist-get border-plist :top-color)
                :right-color (plist-get border-plist :right-color)
                :bottom-color (plist-get border-plist :bottom-color)
                :left-color (plist-get border-plist :left-color)))
     :margin (when margin-plist
               (etaf-sides-create
                :top (or (plist-get margin-plist :top) 0)
                :right (or (plist-get margin-plist :right) 0)
                :bottom (or (plist-get margin-plist :bottom) 0)
                :left (or (plist-get margin-plist :left) 0)))
     :overflow (when overflow-plist
                 (etaf-overflow-create
                  :overflow-y (or (plist-get overflow-plist :overflow-y) "visible")
                  :v-scroll-bar-type (plist-get overflow-plist :v-scroll-bar-type)
                  :v-scroll-bar-direction (or (plist-get overflow-plist :v-scroll-bar-direction) 'right)
                  :scroll-thumb-color (plist-get overflow-plist :scroll-thumb-color)
                  :scroll-track-color (plist-get overflow-plist :scroll-track-color)))
     :width-keyword (plist-get plist :width-keyword)
     :height-keyword (plist-get plist :height-keyword)
     :needs-content-width (plist-get plist :needs-content-width)
     :needs-content-height (plist-get plist :needs-content-height)
     :parent-width (plist-get plist :parent-width)
     :parent-height (plist-get plist :parent-height))))

(defun etaf-box-model-to-plist (box)
  "Convert an etaf-box-model struct BOX to legacy plist format.
This provides backward compatibility with existing code."
  (let ((padding (etaf-box-model-padding box))
        (border (etaf-box-model-border box))
        (margin (etaf-box-model-margin box))
        (overflow (etaf-box-model-overflow box)))
    (list
     :box-sizing (etaf-box-model-box-sizing box)
     :content (list :width (etaf-box-model-content-width box)
                    :height (etaf-box-model-content-height box))
     :padding (when padding
                (list :top (etaf-sides-top padding)
                      :right (etaf-sides-right padding)
                      :bottom (etaf-sides-bottom padding)
                      :left (etaf-sides-left padding)))
     :border (when border
               (list :top-width (etaf-border-top-width border)
                     :right-width (etaf-border-right-width border)
                     :bottom-width (etaf-border-bottom-width border)
                     :left-width (etaf-border-left-width border)
                     :top-color (etaf-border-top-color border)
                     :right-color (etaf-border-right-color border)
                     :bottom-color (etaf-border-bottom-color border)
                     :left-color (etaf-border-left-color border)))
     :margin (when margin
               (list :top (etaf-sides-top margin)
                     :right (etaf-sides-right margin)
                     :bottom (etaf-sides-bottom margin)
                     :left (etaf-sides-left margin)))
     :overflow (when overflow
                 (list :overflow-y (etaf-overflow-overflow-y overflow)
                       :v-scroll-bar-type (etaf-overflow-v-scroll-bar-type overflow)
                       :v-scroll-bar-direction (etaf-overflow-v-scroll-bar-direction overflow)
                       :scroll-thumb-color (etaf-overflow-scroll-thumb-color overflow)
                       :scroll-track-color (etaf-overflow-scroll-track-color overflow)))
     :width-keyword (etaf-box-model-width-keyword box)
     :height-keyword (etaf-box-model-height-keyword box)
     :needs-content-width (etaf-box-model-needs-content-width box)
     :needs-content-height (etaf-box-model-needs-content-height box)
     :parent-width (etaf-box-model-parent-width box)
     :parent-height (etaf-box-model-parent-height box))))

(provide 'etaf-type)
;;; etaf-type.el ends here
