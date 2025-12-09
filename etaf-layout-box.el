;;; etaf-layout-box.el --- Box model for layout computation -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, box-model, css
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS Box Model Module
;; ====================
;;
;; This module provides the CSS box model data structure and accessor functions
;; for layout computation. It follows the CSS specification for the box model:
;;
;;   ┌─────────────────────────────────────────────────────────┐
;;   │                      margin (外边距)                     │
;;   │  ┌─────────────────────────────────────────────────┐   │
;;   │  │                  border (边框)                   │   │
;;   │  │  ┌─────────────────────────────────────────┐   │   │
;;   │  │  │            padding (内边距)              │   │   │
;;   │  │  │  ┌─────────────────────────────────┐   │   │   │
;;   │  │  │  │        content (内容区域)        │   │   │   │
;;   │  │  │  │                                  │   │   │   │
;;   │  │  │  └─────────────────────────────────┘   │   │   │
;;   │  │  └─────────────────────────────────────────┘   │   │
;;   │  └─────────────────────────────────────────────────┘   │
;;   └─────────────────────────────────────────────────────────┘
;;
;; Module Prefix: `etaf-layout-box-'
;;
;; Data Structure:
;; ---------------
;; The box model is represented as a plist with the following keys:
;;
;;   (:box-sizing "content-box"|"border-box"
;;    :content (:width <number> :height <number>)
;;    :padding (:top <n> :right <n> :bottom <n> :left <n>)
;;    :border (:top-width <n> :right-width <n> :bottom-width <n> :left-width <n>
;;             :top-color <color> :right-color <color> :bottom-color <color> :left-color <color>)
;;    :margin (:top <n> :right <n> :bottom <n> :left <n>)
;;    :overflow (:overflow-y "visible"|"hidden"|"auto"|"scroll"
;;               :v-scroll-bar-type <symbol>
;;               :v-scroll-bar-direction left|right
;;               :scroll-thumb-color <color>
;;               :scroll-track-color <color>))
;;
;; Public API:
;; -----------
;; Creation:
;;   - `etaf-layout-box-create' - Create an empty box model structure
;;
;; Content Accessors:
;;   - `etaf-layout-box-content-width' - Get content area width
;;   - `etaf-layout-box-content-height' - Get content area height
;;
;; Spacing Accessors (horizontal/vertical totals):
;;   - `etaf-layout-box-padding-width' - Left + right padding
;;   - `etaf-layout-box-padding-height' - Top + bottom padding
;;   - `etaf-layout-box-border-width' - Left + right border
;;   - `etaf-layout-box-border-height' - Top + bottom border
;;   - `etaf-layout-box-margin-width' - Left + right margin
;;   - `etaf-layout-box-margin-height' - Top + bottom margin
;;
;; Total Dimensions:
;;   - `etaf-layout-box-total-width' - Full width including all spacing
;;   - `etaf-layout-box-total-height' - Full height including all spacing
;;
;; Overflow Accessors:
;;   - `etaf-layout-box-overflow-y' - Vertical overflow mode
;;   - `etaf-layout-box-v-scroll-bar-type' - Scrollbar style type
;;   - `etaf-layout-box-v-scroll-bar-direction' - Scrollbar position
;;   - `etaf-layout-box-scroll-thumb-color' - Scrollbar thumb color
;;   - `etaf-layout-box-scroll-track-color' - Scrollbar track color
;;
;; Integration:
;; ------------
;; This module integrates with the type system in `etaf-type.el' which provides
;; `cl-defstruct' based types. Use `etaf-box-model-from-plist' and
;; `etaf-box-model-to-plist' for conversion between formats.

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Box Model Creation
;;; ============================================================================

(defun etaf-layout-box-create ()
  "Create an empty box model structure with default values.

Returns a plist with the following structure:
  (:box-sizing \"content-box\"
   :content (:width 0 :height 0)
   :padding (:top 0 :right 0 :bottom 0 :left 0)
   :border (:top-width 0 ... :top-color <default-fg> ...)
   :margin (:top 0 :right 0 :bottom 0 :left 0)
   :overflow (:overflow-y \"visible\" ...))

The default box-sizing is \"content-box\", meaning width/height refer to
the content area only. Use \"border-box\" to include padding and border
in the specified width/height.

Border colors default to the current frame's foreground color.
Track colors for scrollbars default to the frame's background color."
  (list :box-sizing "content-box"
        :content (list :width 0 :height 0)
        :padding (list :top 0 :right 0 :bottom 0 :left 0)
        :border (list :top-width 0 :right-width 0
                      :bottom-width 0 :left-width 0
                      :top-color (face-attribute 'default :foreground)
                      :right-color (face-attribute 'default :foreground)
                      :bottom-color (face-attribute 'default :foreground)
                      :left-color (face-attribute 'default :foreground))
        :margin (list :top 0 :right 0 :bottom 0 :left 0)
        :overflow
        (list :overflow-y "visible"
              :v-scroll-bar-type nil
              :v-scroll-bar-direction nil
              :scroll-thumb-color (face-attribute 'default :foreground)
              :scroll-track-color (face-attribute 'default :background))))

;;; ============================================================================
;;; Content Area Accessors
;;; ============================================================================
;;
;; The content area is the innermost part of the box model where the actual
;; element content (text, images, child elements) is rendered.

(defun etaf-layout-box-content-width (box-model)
  "Get the content area width of BOX-MODEL in pixels.
This is the width available for content, not including padding,
border, or margin."
  (plist-get (plist-get box-model :content) :width))

(defun etaf-layout-box-content-height (box-model)
  "Get the content area height of BOX-MODEL.
This is measured in pixels for pixel-based layouts, or in lines
for text-based layouts."
  (plist-get (plist-get box-model :content) :height))

;;; ============================================================================
;;; Padding Accessors
;;; ============================================================================
;;
;; Padding is the space between the content area and the border.
;; It inherits the element's background color.

(defun etaf-layout-box-padding-width (box-model)
  "Get total horizontal padding (left + right) of BOX-MODEL in pixels."
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :left)
       (plist-get padding :right))))

(defun etaf-layout-box-padding-height (box-model)
  "Get total vertical padding (top + bottom) of BOX-MODEL."
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :top)
       (plist-get padding :bottom))))

;;; ============================================================================
;;; Border Accessors
;;; ============================================================================
;;
;; The border surrounds the padding. Each side can have its own width and color.
;; In text-based rendering, borders are typically rendered using underline/overline
;; face properties for horizontal borders, and inverse video for vertical borders.

(defun etaf-layout-box-border-width (box-model)
  "Get total horizontal border width (left + right) of BOX-MODEL in pixels."
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :left-width)
       (plist-get border :right-width))))

(defun etaf-layout-box-border-height (box-model)
  "Get total vertical border width (top + bottom) of BOX-MODEL.
Note: Vertical borders may be rendered using overline/underline face
properties and may not occupy actual buffer lines."
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :top-width)
       (plist-get border :bottom-width))))

;;; ============================================================================
;;; Margin Accessors
;;; ============================================================================
;;
;; Margin is the outermost spacing of the box model. It creates space between
;; adjacent elements. Margins are always transparent (they don't inherit the
;; element's background color).

(defun etaf-layout-box-margin-width (box-model)
  "Get total horizontal margin (left + right) of BOX-MODEL in pixels."
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :left)
       (plist-get margin :right))))

(defun etaf-layout-box-margin-height (box-model)
  "Get total vertical margin (top + bottom) of BOX-MODEL."
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :top)
       (plist-get margin :bottom))))

;;; ============================================================================
;;; Total Dimension Calculations
;;; ============================================================================
;;
;; These functions calculate the complete outer dimensions of a box,
;; including all spacing layers (content + padding + border + margin).

(defun etaf-layout-box-total-width (box-model)
  "Calculate total width of BOX-MODEL including all spacing.
Total width = content-width + padding-width + border-width + margin-width"
  (+ (etaf-layout-box-content-width box-model)
     (etaf-layout-box-padding-width box-model)
     (etaf-layout-box-border-width box-model)
     (etaf-layout-box-margin-width box-model)))

(defun etaf-layout-box-total-height (box-model)
  "Calculate total height of BOX-MODEL including all spacing.
Total height = content-height + padding-height + border-height + margin-height"
  (+ (etaf-layout-box-content-height box-model)
     (etaf-layout-box-padding-height box-model)
     (etaf-layout-box-border-height box-model)
     (etaf-layout-box-margin-height box-model)))

;;; ============================================================================
;;; Overflow Property Accessors
;;; ============================================================================
;;
;; Overflow properties control how content that exceeds the box dimensions
;; is handled. ETAF supports vertical scrolling for content overflow.

(defun etaf-layout-box-overflow-y (box-model)
  "Get vertical overflow handling mode of BOX-MODEL.
Returns one of:
  \"visible\" - Content is not clipped (default)
  \"hidden\"  - Content is clipped, no scrollbar
  \"auto\"    - Scrollbar shown only when content overflows
  \"scroll\"  - Scrollbar always shown"
  (let ((overflow (plist-get box-model :overflow)))
    (plist-get overflow :overflow-y)))

(defun etaf-layout-box-v-scroll-bar-type (box-model)
  "Get vertical scrollbar style type of BOX-MODEL.
Returns a symbol that references a scrollbar style definition in
`etaf-layout-scroll-bar-alist', or nil if no custom style is set."
  (let ((overflow (plist-get box-model :overflow)))
    (plist-get overflow :v-scroll-bar-type)))

(defun etaf-layout-box-v-scroll-bar-direction (box-model)
  "Get vertical scrollbar position of BOX-MODEL.
Returns 'left or 'right. Defaults to 'right if not explicitly set."
  (let ((overflow (plist-get box-model :overflow)))
    (or (plist-get overflow :v-scroll-bar-direction) 'right)))

(defun etaf-layout-box-scroll-thumb-color (box-model)
  "Get scrollbar thumb (handle) color of BOX-MODEL.
The thumb is the draggable element that represents the current
viewport position within the scrollable content."
  (let ((overflow (plist-get box-model :overflow)))
    (plist-get overflow :scroll-thumb-color)))

(defun etaf-layout-box-scroll-track-color (box-model)
  "Get scrollbar track (background) color of BOX-MODEL.
The track is the background area along which the thumb moves."
  (let ((overflow (plist-get box-model :overflow)))
    (plist-get overflow :scroll-track-color)))

(provide 'etaf-layout-box)
;;; etaf-layout-box.el ends here
