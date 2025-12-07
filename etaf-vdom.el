;;; etaf-vdom.el --- Virtual DOM for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: dom, virtual dom, vdom
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Virtual DOM (VNode) System for ETAF - Following Vue 3 Standards
;;
;; This module implements a Vue 3 compatible virtual DOM layer.
;; VNode is a pure data structure describing the UI, NOT storing actual DOM.
;;
;; VNode Structure (Vue 3 compatible):
;;   (:type element                    ; Node type
;;    :tag div                         ; Tag name
;;    :props (:class "box" :id "main") ; Properties (attrs + events)
;;    :children (child-vnode1 ...)     ; Child VNodes
;;    :key "unique-key"                ; Key for diff optimization
;;    :ref nil                         ; Reference
;;    :patchFlag 3                     ; Optimization flags
;;    :dynamicProps ("class" "style")) ; Dynamic prop names
;;
;; PatchFlags indicate which parts of the VNode are dynamic:
;; - Enables compiler optimizations
;; - Skips static content during patch
;; - Targets only changed properties
;;
;; Based on Vue 3's design:
;; - https://github.com/vuejs/core/blob/main/packages/runtime-core/src/vnode.ts
;; - https://github.com/vuejs/core/blob/main/packages/shared/src/patchFlags.ts

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; PatchFlags (Vue 3 compatible)
;;; ============================================================================

(defconst etaf-patch-flag-text 1
  "Dynamic text content.
Indicates the node has dynamic text children that need to be updated.")

(defconst etaf-patch-flag-class 2
  "Dynamic class binding.
The node's class attribute contains dynamic expressions.")

(defconst etaf-patch-flag-style 4
  "Dynamic style binding.
The node's style attribute contains dynamic expressions.")

(defconst etaf-patch-flag-props 8
  "Dynamic props (except class/style).
The node has dynamic non-class/style attributes.")

(defconst etaf-patch-flag-full-props 16
  "Props with dynamic keys.
The node has props with dynamic keys, need full props diff.")

(defconst etaf-patch-flag-hydrate-events 32
  "Has event listeners.
The node has event handlers attached (on-click, etc.).")

(defconst etaf-patch-flag-stable-fragment 64
  "Stable fragment.
Fragment with stable children (no key needed).")

(defconst etaf-patch-flag-keyed-fragment 128
  "Keyed fragment.
Fragment with keyed children (needs key-based diff).")

(defconst etaf-patch-flag-unkeyed-fragment 256
  "Unkeyed fragment.
Fragment with unkeyed children.")

(defconst etaf-patch-flag-need-patch 512
  "Force patch.
Always perform patch on this node.")

(defconst etaf-patch-flag-dynamic-slots 1024
  "Dynamic slots.
Component has dynamic slot content.")

(defconst etaf-patch-flag-hoisted -1
  "Static node (hoisted).
Content is fully static and hoisted, never needs update.")

(defconst etaf-patch-flag-bail -2
  "Bail optimization.
Diff algorithm should exit optimization mode.")

(defun etaf-vdom-has-patch-flag-p (vnode flag)
  "Check if VNODE has the specified patch FLAG.
FLAG should be one of the etaf-patch-flag-* constants."
  (let ((patch-flag (plist-get vnode :patchFlag)))
    (and patch-flag
         (> patch-flag 0)
         (not (zerop (logand patch-flag flag))))))

(defun etaf-vdom-add-patch-flag (vnode flag)
  "Add patch FLAG to VNODE.
Uses bitwise OR to combine with existing flags."
  (let ((current-flag (or (plist-get vnode :patchFlag) 0)))
    (plist-put vnode :patchFlag (logior current-flag flag))))

(defun etaf-vdom-is-static-p (vnode)
  "Check if VNODE is fully static (no dynamic content).
Returns t if patchFlag is 0, -1 (hoisted), or nil."
  (let ((flag (plist-get vnode :patchFlag)))
    (or (null flag)
        (zerop flag)
        (= flag etaf-patch-flag-hoisted))))

;;; VNode ID Generator (for debugging/tracking)

(defvar etaf-vdom--id-counter 0
  "Counter for generating unique VNode IDs (debugging only).")

(defun etaf-vdom--generate-id ()
  "Generate a unique VNode ID for debugging."
  (cl-incf etaf-vdom--id-counter))

;;; ============================================================================
;;; ShapeFlags (Vue 3 compatible)
;;; ============================================================================

(defconst etaf-shape-flag-element 1
  "Element VNode (div, span, etc.).")

(defconst etaf-shape-flag-functional-component 2
  "Functional component.")

(defconst etaf-shape-flag-stateful-component 4
  "Stateful component with state.")

(defconst etaf-shape-flag-text-children 8
  "VNode has text children (string).")

(defconst etaf-shape-flag-array-children 16
  "VNode has array children (list of VNodes).")

(defconst etaf-shape-flag-slots-children 32
  "VNode has slots children.")

(defconst etaf-shape-flag-teleport 64
  "Teleport component.")

(defconst etaf-shape-flag-suspense 128
  "Suspense component.")

(defconst etaf-shape-flag-component-should-keep-alive 256
  "Component should be kept alive.")

(defconst etaf-shape-flag-component-kept-alive 512
  "Component is kept alive.")

(defconst etaf-shape-flag-component 6
  "Any component (functional or stateful).")

;;; ============================================================================
;;; VNode Types (Vue 3 compatible)
;;; ============================================================================

;; Special VNode types (using symbols like Vue 3)
(defconst etaf-vnode-fragment 'v-fgt
  "Fragment type - allows multiple root nodes without wrapper.")

(defconst etaf-vnode-text 'v-txt
  "Text node type.")

(defconst etaf-vnode-comment 'v-cmt
  "Comment node type.")

(defconst etaf-vnode-static 'v-stc
  "Static node type - fully static, never needs update.")

;;; ============================================================================
;;; Tag Metadata Creation (for interactive elements)
;;; ============================================================================

(defun etaf-vdom-create-tag-metadata (tag attrs children)
  "Create tag metadata for interactive TAG with ATTRS and CHILDREN.
This is used by the layout renderer to create interactive behavior for tags.

TAG - Tag symbol (a, button, input, textarea, summary)
ATTRS - Plist of attributes (:href \"...\" :type \"button\" etc.)
CHILDREN - Child nodes

Returns a plist with tag metadata:
- :tag - The tag name
- :attrs - Original attributes
- :children - Child nodes
- :state - Interactive state (:hovered nil :focused nil :active nil :disabled ...)
- :on-click - Click handler (if applicable)
- :on-hover-enter - Mouse enter handler
- :on-hover-leave - Mouse leave handler
- :hover-style - Hover visual style
- :active-style - Active/pressed style
- :disabled-style - Disabled style

This replaces the old etaf-etml-tag-create-instance functionality."
  (let ((metadata (list :tag tag
                        :attrs attrs
                        :children children
                        :state (list :hovered nil
                                     :focused nil
                                     :active nil
                                     :disabled (plist-get attrs :disabled)))))
    ;; Add built-in event handlers and styles based on tag type
    (pcase tag
      ('a
       (setq metadata (plist-put metadata :hover-style '((color . "#1d4ed8"))))
       (setq metadata (plist-put metadata :on-click
                                  (lambda (&optional event)
                                    (interactive)
                                    (when-let ((href (plist-get attrs :href)))
                                      (browse-url href))))))
      ('button
       (setq metadata (plist-put metadata :hover-style '((background-color . "#e5e7eb"))))
       (setq metadata (plist-put metadata :active-style '((background-color . "#d1d5db"))))
       (setq metadata (plist-put metadata :disabled-style
                                  '((background-color . "#f3f4f6")
                                    (color . "#9ca3af")
                                    (cursor . "not-allowed"))))
       (setq metadata (plist-put metadata :on-click
                                  (lambda (&optional event)
                                    (interactive)
                                    (let ((state (plist-get metadata :state)))
                                      (unless (plist-get state :disabled)
                                        (when-let ((custom-handler (plist-get attrs :on-click)))
                                          ;; Try to call with event, fall back to no args if arity mismatch
                                          (condition-case err
                                              (funcall custom-handler event)
                                            (wrong-number-of-arguments (funcall custom-handler))
                                            ;; Re-signal other errors to avoid hiding bugs
                                            (error (signal (car err) (cdr err)))))))))))
      ('input
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6"))))
       (setq metadata (plist-put metadata :disabled-style
                                  '((background-color . "#f3f4f6")
                                    (color . "#9ca3af")))))
      ('textarea
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6")))))
      ('summary
       (setq metadata (plist-put metadata :on-click
                                  (lambda (&optional event)
                                    (interactive)
                                    ;; Toggle details element
                                    (message "Summary clicked"))))))
    metadata))

;;; ============================================================================
;;; Helper Functions for Interactive Elements
;;; ============================================================================

(defun etaf-vdom-setup-keymap (tag-metadata &optional parent-keymap)
  "Setup keymap for interactive TAG-METADATA.
TAG-METADATA is a plist with :on-click, :on-hover-enter, etc.
PARENT-KEYMAP is optional parent keymap to inherit from.
Returns a keymap with event handlers bound."
  (let ((map (if parent-keymap
                 (make-composed-keymap nil parent-keymap)
               (make-sparse-keymap))))
    ;; Bind click handler
    (when-let ((on-click (plist-get tag-metadata :on-click)))
      (define-key map [mouse-1] on-click)
      (define-key map (kbd "RET") on-click))
    ;; Bind hover handlers
    (when-let ((on-hover-enter (plist-get tag-metadata :on-hover-enter)))
      (define-key map [mouse-movement] on-hover-enter))
    ;; Bind keydown handler
    (when-let ((on-keydown (plist-get tag-metadata :on-keydown)))
      (define-key map (kbd "SPC") on-keydown))
    map))

(defun etaf-vdom-help-echo-handler (tag-metadata)
  "Create help-echo handler for TAG-METADATA.
Returns a function that provides tooltip/status line help."
  (let ((tag (plist-get tag-metadata :tag))
        (attrs (plist-get tag-metadata :attrs)))
    (lambda (window object pos)
      (format "%s element%s"
              tag
              (if-let ((href (plist-get attrs :href)))
                  (format ": %s" href)
                "")))))

;;; ============================================================================
;;; VNode Creation (Vue 3 standard: createBaseVNode)
;;; ============================================================================

(defvar etaf-current-scope-id nil
  "Current scope ID for scoped styles (like Vue 3's currentScopeId).")

(defvar etaf-current-rendering-instance nil
  "Current rendering component instance (like Vue 3's currentRenderingInstance).")

(defun etaf-create-base-vnode (type props children
                                    &optional patchFlag dynamicProps
                                    shapeFlag isBlockNode
                                    needFullChildrenNormalization)
  "Create a base VNode following Vue 3's createBaseVNode.

This is the low-level VNode creation function following Vue 3 standard.

Parameters (Vue 3 compatible):
  TYPE - VNode type:
    - Symbol (element): 'div, 'span, etc.
    - etaf-vnode-text: Text node
    - etaf-vnode-fragment: Fragment
    - Component: Component definition (future)
  
  PROPS - Properties plist or nil:
    - Attributes: :class, :id, :style
    - Event handlers: :on-click, etc.
  
  CHILDREN - unknown (can be string, VNode, list of VNodes, nil)
  
  PATCHFLAG - Optimization hint (default 0)
  
  DYNAMICPROPS - List of dynamic prop names (default nil)
  
  SHAPEFLAG - Pre-computed shape flag (default: computed based on type)
  
  ISBLOCKNODE - Whether this is a block node (default nil)
  
  NEEDFULLCHILDRENNORMALIZATION - Whether to normalize children (default nil)

Returns VNode with Vue 3 compatible structure."
  (let* ((normalized-key (etaf-vnode--normalize-key props))
         (normalized-ref (etaf-vnode--normalize-ref props))
         (computed-shape-flag (or shapeFlag
                                  (if (eq type etaf-vnode-fragment)
                                      0
                                    etaf-shape-flag-element)))
         (vnode (list
                 :__v_isVNode t
                 :__v_skip t
                 :type type
                 :props props
                 :key normalized-key
                 :ref normalized-ref
                 :scopeId etaf-current-scope-id
                 :slotScopeIds nil
                 :children children
                 :component nil
                 :suspense nil
                 :ssContent nil
                 :ssFallback nil
                 :dirs nil
                 :transition nil
                 :el nil              ; Set by renderer during mount
                 :anchor nil          ; For fragments
                 :target nil          ; For teleport
                 :targetStart nil
                 :targetAnchor nil
                 :staticCount 0
                 :shapeFlag computed-shape-flag
                 :patchFlag (or patchFlag 0)
                 :dynamicProps dynamicProps
                 :dynamicChildren nil ; For block optimization
                 :appContext nil
                 :ctx etaf-current-rendering-instance)))
    
    ;; Normalize children if needed
    (when needFullChildrenNormalization
      (etaf-vnode--normalize-children vnode children))
    
    ;; Auto-detect children type and update shapeFlag
    (cond
     ((stringp children)
      (plist-put vnode :shapeFlag
                 (logior computed-shape-flag etaf-shape-flag-text-children)))
     ((and (listp children) (not (null children)))
      (plist-put vnode :shapeFlag
                 (logior computed-shape-flag etaf-shape-flag-array-children))))
    
    ;; Set parent for array children
    (when (and (listp children) (not (stringp children)))
      (dolist (child children)
        (when (and (listp child) (plist-get child :__v_isVNode))
          (plist-put child :parent vnode))))
    
    vnode))

(defun etaf-vnode--normalize-key (props)
  "Normalize key from PROPS (like Vue 3's normalizeKey)."
  (when props
    (plist-get props :key)))

(defun etaf-vnode--normalize-ref (props)
  "Normalize ref from PROPS (like Vue 3's normalizeRef)."
  (when props
    (plist-get props :ref)))

(defun etaf-vnode--normalize-children (vnode children)
  "Normalize CHILDREN for VNODE (like Vue 3's normalizeChildren).
Updates vnode's :children and :shapeFlag based on children type."
  (let ((type 0))
    (cond
     ;; Nil children
     ((null children)
      (plist-put vnode :children nil))
     
     ;; String children
     ((stringp children)
      (plist-put vnode :children children)
      (setq type etaf-shape-flag-text-children))
     
     ;; Array children
     ((listp children)
      (plist-put vnode :children children)
      (setq type etaf-shape-flag-array-children))
     
     ;; Other types - convert to string
     (t
      (plist-put vnode :children (format "%s" children))
      (setq type etaf-shape-flag-text-children)))
    
    ;; Update shapeFlag
    (let ((current-flag (plist-get vnode :shapeFlag)))
      (plist-put vnode :shapeFlag (logior current-flag type)))))

;;; ============================================================================
;;; High-level VNode Creation Functions
;;; ============================================================================

(defun etaf-create-vnode (type props children &optional patchFlag dynamicProps)
  "Create a VNode (high-level API).

This is a simplified wrapper around etaf-create-base-vnode.

TYPE - Element type symbol or special type
PROPS - Properties plist
CHILDREN - Children (string, VNode, or list of VNodes)
PATCHFLAG - Optional optimization flags
DYNAMICPROPS - Optional dynamic prop names

Example:
  (etaf-create-vnode 'div
    (:class \"box\" :id \"main\")
    (list (etaf-create-vnode etaf-vnode-text nil \"Hello\"))
    etaf-patch-flag-class
    (\"class\"))"
  (etaf-create-base-vnode type props children
                          patchFlag dynamicProps
                          nil nil nil))

(defun etaf-vdom-element (tag props children &optional patchFlag dynamicProps)
  "Create an element VNode (convenience function).

TAG - Element tag symbol ('div, 'span, etc.)
PROPS - Properties plist
CHILDREN - Children
PATCHFLAG - Optional optimization flags
DYNAMICPROPS - Optional dynamic props"
  (etaf-create-vnode tag props children patchFlag dynamicProps))

(defun etaf-vdom-text (content)
  "Create a text VNode with CONTENT."
  (etaf-create-base-vnode etaf-vnode-text
                          nil
                          content
                          0 nil
                          0))  ; shapeFlag 0 for text

(defun etaf-vdom-comment (content)
  "Create a comment VNode with CONTENT."
  (etaf-create-base-vnode etaf-vnode-comment
                          nil
                          content))

(defun etaf-vdom-fragment (children &optional patchFlag)
  "Create a fragment VNode with CHILDREN."
  (let ((flag (or patchFlag
                  ;; Auto-detect keyed/stable fragment
                  (if (and (listp children)
                           (cl-every (lambda (c)
                                      (and (listp c)
                                           (plist-get c :key)))
                                    children))
                      etaf-patch-flag-keyed-fragment
                    etaf-patch-flag-stable-fragment))))
    (etaf-create-base-vnode etaf-vnode-fragment
                            nil
                            children
                            flag)))

(defun etaf-vdom-static (content)
  "Create a static VNode (hoisted, never updated)."
  (etaf-create-base-vnode etaf-vnode-static
                          nil
                          content
                          etaf-patch-flag-hoisted))

;;; ============================================================================
;;; VNode Accessors (Vue 3 compatible)
;;; ============================================================================

(defun etaf-vdom-is-vnode-p (obj)
  "Check if OBJ is a VNode (Vue 3 compatible check)."
  (and (listp obj)
       (eq (plist-get obj :__v_isVNode) t)))

(defun etaf-vdom-get-type (vnode)
  "Get the type of VNODE."
  (plist-get vnode :type))

(defun etaf-vdom-get-tag (vnode)
  "Get the tag name of VNODE (alias for type for element nodes)."
  (let ((type (etaf-vdom-get-type vnode)))
    (when (symbolp type)
      type)))

(defun etaf-vdom-get-props (vnode)
  "Get the properties of VNODE."
  (plist-get vnode :props))

(defun etaf-vdom-get-children (vnode)
  "Get the children of VNODE."
  (plist-get vnode :children))

(defun etaf-vdom-get-key (vnode)
  "Get the key of VNODE."
  (plist-get vnode :key))

(defun etaf-vdom-get-ref (vnode)
  "Get the ref of VNODE."
  (plist-get vnode :ref))

(defun etaf-vdom-get-patch-flag (vnode)
  "Get the patchFlag of VNODE."
  (plist-get vnode :patchFlag))

(defun etaf-vdom-get-dynamic-props (vnode)
  "Get the dynamicProps of VNODE."
  (plist-get vnode :dynamicProps))

(defun etaf-vdom-get-shape-flag (vnode)
  "Get the shapeFlag of VNODE."
  (plist-get vnode :shapeFlag))

(defun etaf-vdom-get-el (vnode)
  "Get the real DOM element of VNODE (set by renderer)."
  (plist-get vnode :el))

(defun etaf-vdom-get-parent (vnode)
  "Get the parent VNode of VNODE."
  (plist-get vnode :parent))

(defun etaf-vdom-get-component (vnode)
  "Get the component instance of VNODE."
  (plist-get vnode :component))

;;; ============================================================================
;;; VNode Predicates (using shapeFlag for performance)
;;; ============================================================================

(defun etaf-vdom-vnode-p (obj)
  "Check if OBJ is a VNode."
  (etaf-vdom-is-vnode-p obj))

(defun etaf-vdom-element-p (vnode)
  "Check if VNODE is an element node (using shapeFlag)."
  (let ((shape-flag (etaf-vdom-get-shape-flag vnode)))
    (and shape-flag
         (not (zerop (logand shape-flag etaf-shape-flag-element))))))

(defun etaf-vdom-text-p (vnode)
  "Check if VNODE is a text node."
  (eq (etaf-vdom-get-type vnode) etaf-vnode-text))

(defun etaf-vdom-comment-p (vnode)
  "Check if VNODE is a comment node."
  (eq (etaf-vdom-get-type vnode) etaf-vnode-comment))

(defun etaf-vdom-fragment-p (vnode)
  "Check if VNODE is a fragment node."
  (eq (etaf-vdom-get-type vnode) etaf-vnode-fragment))

(defun etaf-vdom-static-p (vnode)
  "Check if VNODE is a static node."
  (eq (etaf-vdom-get-type vnode) etaf-vnode-static))

(defun etaf-vdom-component-p (vnode)
  "Check if VNODE is a component (using shapeFlag)."
  (let ((shape-flag (etaf-vdom-get-shape-flag vnode)))
    (and shape-flag
         (not (zerop (logand shape-flag etaf-shape-flag-component))))))

(defun etaf-vdom-has-text-children-p (vnode)
  "Check if VNODE has text children (using shapeFlag)."
  (let ((shape-flag (etaf-vdom-get-shape-flag vnode)))
    (and shape-flag
         (not (zerop (logand shape-flag etaf-shape-flag-text-children))))))

(defun etaf-vdom-has-array-children-p (vnode)
  "Check if VNODE has array children (using shapeFlag)."
  (let ((shape-flag (etaf-vdom-get-shape-flag vnode)))
    (and shape-flag
         (not (zerop (logand shape-flag etaf-shape-flag-array-children))))))

(defun etaf-vdom-same-type-p (vnode1 vnode2)
  "Check if VNODE1 and VNODE2 are the same type (for diff).
Two VNodes are the same type if they have:
- Same type
- Same key (or both have no key)"
  (and (eq (etaf-vdom-get-type vnode1) (etaf-vdom-get-type vnode2))
       (let ((key1 (etaf-vdom-get-key vnode1))
             (key2 (etaf-vdom-get-key vnode2)))
         (or (and (not key1) (not key2))
             (equal key1 key2)))))

;;; VNode Modification

(defun etaf-vdom-set-children (vnode children)
  "Set the children of VNODE to CHILDREN."
  (plist-put vnode :children children)
  ;; Set parent reference for each child
  (dolist (child children)
    (when (etaf-vdom-vnode-p child)
      (plist-put child :parent vnode)))
  vnode)

(defun etaf-vdom-append-child (vnode child)
  "Append CHILD to VNODE's children."
  (let ((children (etaf-vdom-get-children vnode)))
    (plist-put vnode :children (append children (list child)))
    (when (etaf-vdom-vnode-p child)
      (plist-put child :parent vnode)))
  vnode)

;;; ============================================================================
;;; Renderer - VNode to DOM (Step 5 in Vue 3 pipeline)
;;; ============================================================================

(defun etaf-vdom-render (vnode)
  "Render VNode tree to clean DOM tree.
This is Step 5 in the pipeline: 渲染器 (Renderer).
Converts VNode tree to clean DOM without VNode metadata in attributes.
Returns the root DOM node.

VNode → DOM conversion:
- Element VNode → (tag ((attrs...)) children...)
- Text VNode → string
- Fragment VNode → list of DOM nodes (no wrapper)
- Comment VNode → ignored (returns nil)

The DOM is 'clean' - it contains no VNode-specific fields like
:__v_isVNode, :patchFlag, :shapeFlag, etc. Only standard DOM structure."
  (cond
   ;; Text VNode - return text content
   ((etaf-vdom-text-p vnode)
    (let ((props (etaf-vdom-get-props vnode)))
      (or (plist-get props :textContent)
          (etaf-vdom-get-children vnode)
          "")))
   
   ;; Fragment VNode - render children without wrapper
   ((etaf-vdom-fragment-p vnode)
    (let ((children (etaf-vdom-get-children vnode)))
      (if (listp children)
          (mapcar #'etaf-vdom-render children)
        children)))
   
   ;; Static VNode - return content as-is
   ((etaf-vdom-static-p vnode)
    (etaf-vdom-get-children vnode))
   
   ;; Comment VNode - ignore (don't render)
   ((etaf-vdom-comment-p vnode)
    nil)
   
   ;; Element VNode - render to DOM
   ((etaf-vdom-element-p vnode)
    (let* ((type (etaf-vdom-get-type vnode))
           (props (etaf-vdom-get-props vnode))
           (children (etaf-vdom-get-children vnode))
           ;; Check for :textContent in props - used for style/script elements
           (text-content (plist-get props :textContent))
           ;; Convert props plist to attrs alist for DOM
           (attrs (etaf-vdom--props-to-attrs props))
           ;; Extract event handlers to preserve them
           (event-handlers (etaf-vdom--extract-event-handlers props))
           ;; Render children
           (child-doms (cond
                        ;; If textContent is specified, use it as the only child
                        (text-content
                         (list text-content))
                        ;; String children
                        ((stringp children)
                         (list children))
                        ;; Array children - render recursively
                        ((listp children)
                         (delq nil (mapcar #'etaf-vdom-render children)))
                        ;; Other (nil, etc.)
                        (t nil))))
      ;; Build DOM: (tag ((attrs...)) children...)
      ;; If there are event handlers, store them in a special attribute
      (when event-handlers
        (push (cons 'etaf-event-handlers event-handlers) attrs))
      (cons type (cons attrs child-doms))))
   
   ;; Unknown type - return nil
   (t nil)))

(defun etaf-vdom--props-to-attrs (props)
  "Convert VNode PROPS (plist) to DOM attributes (alist).
Filters out event handlers and VNode-specific props.

Props format: (:class \"foo\" :id \"bar\" :on-click fn :style \"color: red\")
Attrs format: ((class . \"foo\") (id . \"bar\") (style . \"color: red\"))

Event handlers (:on-*) are not included in DOM attributes."
  (when props
    (let ((result nil))
      (while props
        (let ((key (pop props))
              (value (pop props)))
          (when (keywordp key)
            (let ((attr-name (intern (substring (symbol-name key) 1))))
              ;; Skip event handlers (on-*) and VNode-specific props
              (unless (or (string-prefix-p "on-" (symbol-name attr-name))
                          (eq attr-name 'textContent)
                          (eq attr-name 'innerHTML))
                (push (cons attr-name value) result))))))
      (nreverse result))))

(defun etaf-vdom--extract-event-handlers (props)
  "Extract event handlers from VNode PROPS.
Returns an alist of (event-name . handler-function).

Event handlers are identified by :on-* keys in props."
  (when props
    (let ((result nil))
      (while props
        (let ((key (pop props))
              (value (pop props)))
          (when (keywordp key)
            (let ((attr-name (symbol-name key)))
              ;; Check if this is an event handler
              (when (string-prefix-p ":on-" attr-name)
                (let ((event-name (intern (substring attr-name 4)))) ; Remove ":on-"
                  (push (cons event-name value) result)))))))
      (nreverse result))))

;;; ============================================================================
;;; Renderer API - Mount, Unmount, Patch (Vue 3 compatible)
;;; ============================================================================

;; These functions implement the Vue 3 renderer API for mounting, unmounting,
;; and patching VNodes. This is the core of the reactive rendering system.

(defvar etaf-vdom--mounted-vnodes (make-hash-table :test 'eq)
  "Hash table mapping container elements to mounted VNode trees.
Keys are buffer positions or container identifiers, values are VNodes.")

(defvar etaf-vdom--container-regions (make-hash-table :test 'eq)
  "Hash table mapping containers to their buffer regions.
Keys are container identifiers, values are (start . end) cons cells.")

;;; Mount Function

(defun etaf-vdom-mount (vnode container)
  "Mount VNODE to CONTAINER following Vue 3's mount semantics.

VNODE is the virtual node tree to mount.
CONTAINER is a buffer position or buffer name where to mount.

This function:
1. Renders the VNode tree to DOM
2. Converts DOM to buffer string (via layout system)
3. Inserts into buffer at position
4. Stores the VNode for later patching
5. Sets up event handlers

Returns the mounted VNode with :el set to buffer region."
  (let* ((buffer (if (bufferp container)
                     container
                   (get-buffer-create container)))
         (dom (etaf-vdom-render vnode))
         ;; Store the mounted vnode
         (container-key (or container (current-buffer))))
    
    ;; Render DOM to buffer string using existing pipeline
    (with-current-buffer buffer
      (let* ((start-pos (point))
             ;; Create CSSOM for the DOM
             (cssom (etaf-css-build-cssom dom))
             ;; Build render tree
             (render-tree (etaf-render-build-tree dom cssom))
             ;; Convert to layout tree
             (layout-tree (etaf-layout-build-tree render-tree))
             ;; Convert to buffer string
             (buffer-string (etaf-layout-to-string layout-tree))
             (end-pos nil))
        
        ;; Insert the string
        (insert buffer-string)
        (setq end-pos (point))
        
        ;; Store the region
        (puthash container-key (cons start-pos end-pos) 
                 etaf-vdom--container-regions)
        
        ;; Store the mounted VNode
        (puthash container-key vnode etaf-vdom--mounted-vnodes)
        
        ;; Set :el property on vnode to reference the buffer region
        (plist-put vnode :el (cons start-pos end-pos))
        
        ;; Initialize event system if needed
        (require 'etaf-event)
        (etaf-event-init buffer)
        
        ;; Register interactive elements
        (etaf-vdom--register-event-handlers vnode start-pos buffer)
        
        vnode))))

(defun etaf-vdom--register-event-handlers (vnode base-pos buffer)
  "Recursively register event handlers for VNODE starting at BASE-POS in BUFFER.
This sets up the event system for interactive elements."
  (when (etaf-vdom-vnode-p vnode)
    (let ((props (etaf-vdom-get-props vnode))
          (children (etaf-vdom-get-children vnode))
          (type (etaf-vdom-get-type vnode)))
      
      ;; Register event handlers for this node if it's an interactive element
      (when (and (symbolp type) 
                 (memq type '(button a input textarea summary))
                 props)
        (let ((uuid (or (plist-get props :uuid)
                        (format "elem-%s-%s" type (random 1000000))))
              (on-click (plist-get props :on-click))
              (start base-pos)
              ;; Estimate end position (simplified)
              (end (+ base-pos 20)))
          
          (when on-click
            ;; Ensure UUID is set
            (unless (plist-get props :uuid)
              (plist-put props :uuid uuid))
            
            ;; Register with event system
            (with-current-buffer buffer
              (etaf-event-register-element uuid vnode start end)
              (etaf-event-add-listener uuid 'mouse-down on-click)
              ;; Also bind to click events
              (etaf-event-add-listener uuid 'state-change
                (lambda (uuid data)
                  (when (and (eq (plist-get data :key) :active)
                             (not (plist-get data :new-value)))
                    ;; Active state changed from t to nil = click
                    (funcall on-click))))))))
      
      ;; Recursively register children
      (when (listp children)
        (dolist (child children)
          (when (etaf-vdom-vnode-p child)
            (etaf-vdom--register-event-handlers child base-pos buffer)))))))

;;; Unmount Function

(defun etaf-vdom-unmount (container)
  "Unmount the VNode tree from CONTAINER following Vue 3's unmount semantics.

CONTAINER is the buffer position or buffer name used during mount.

This function:
1. Removes the rendered content from buffer
2. Cleans up event handlers
3. Removes stored VNode reference

Returns t if successfully unmounted, nil if nothing was mounted."
  (let* ((container-key (or container (current-buffer)))
         (vnode (gethash container-key etaf-vdom--mounted-vnodes))
         (region (gethash container-key etaf-vdom--container-regions)))
    
    (when (and vnode region)
      (let ((buffer (if (bufferp container)
                        container
                      (get-buffer container))))
        (when buffer
          (with-current-buffer buffer
            ;; Remove event handlers
            (etaf-vdom--unregister-event-handlers vnode)
            
            ;; Delete the buffer region
            (delete-region (car region) (cdr region))
            
            ;; Clean up stored data
            (remhash container-key etaf-vdom--mounted-vnodes)
            (remhash container-key etaf-vdom--container-regions)
            
            ;; Clear :el property
            (plist-put vnode :el nil)
            
            t))))))

(defun etaf-vdom--unregister-event-handlers (vnode)
  "Recursively unregister event handlers for VNODE."
  (when (etaf-vdom-vnode-p vnode)
    (let ((props (etaf-vdom-get-props vnode))
          (children (etaf-vdom-get-children vnode)))
      
      ;; Unregister this node's handlers
      (when-let ((uuid (plist-get props :uuid)))
        (etaf-event-unregister-element uuid))
      
      ;; Recursively unregister children
      (when (listp children)
        (dolist (child children)
          (when (etaf-vdom-vnode-p child)
            (etaf-vdom--unregister-event-handlers child)))))))

;;; Patch Function (Diff Algorithm)

(defun etaf-vdom-patch (old-vnode new-vnode container)
  "Patch OLD-VNODE with NEW-VNODE following Vue 3's patch algorithm.

This implements the Vue 3 diff algorithm with optimizations:
1. Check if nodes are same type (type + key)
2. If same type, patch props and children
3. If different type, replace entire subtree
4. Use patchFlags for optimization

CONTAINER is the buffer position or buffer name.

Returns the patched VNode."
  (cond
   ;; Case 1: Old node is nil - mount new node
   ((null old-vnode)
    (etaf-vdom-mount new-vnode container))
   
   ;; Case 2: New node is nil - unmount old node
   ((null new-vnode)
    (etaf-vdom-unmount container)
    nil)
   
   ;; Case 3: Same VNode type - patch in place
   ((etaf-vdom-same-type-p old-vnode new-vnode)
    (etaf-vdom--patch-element old-vnode new-vnode container))
   
   ;; Case 4: Different types - replace
   (t
    (etaf-vdom-unmount container)
    (etaf-vdom-mount new-vnode container))))

(defun etaf-vdom--patch-element (old-vnode new-vnode container)
  "Patch element VNode OLD-VNODE to NEW-VNODE.
This handles prop updates and children patching."
  (let ((old-props (etaf-vdom-get-props old-vnode))
        (new-props (etaf-vdom-get-props new-vnode))
        (old-children (etaf-vdom-get-children old-vnode))
        (new-children (etaf-vdom-get-children new-vnode))
        (patch-flag (etaf-vdom-get-patch-flag new-vnode)))
    
    ;; Check if we can skip patching (static content)
    (unless (etaf-vdom-is-static-p new-vnode)
      ;; Patch props if needed
      (when (or (null patch-flag)
                (> patch-flag 0))
        (etaf-vdom--patch-props old-vnode new-vnode old-props new-props))
      
      ;; Patch children
      (etaf-vdom--patch-children old-vnode new-vnode old-children new-children container))
    
    ;; Update the stored VNode
    (puthash container new-vnode etaf-vdom--mounted-vnodes)
    
    ;; Copy :el reference
    (plist-put new-vnode :el (plist-get old-vnode :el))
    
    new-vnode))

(defun etaf-vdom--patch-props (old-vnode new-vnode old-props new-props)
  "Patch props from OLD-PROPS to NEW-PROPS on vnodes.
Updates event handlers and attributes."
  ;; For now, we do a full re-render on prop changes
  ;; A more optimized version would update props in-place
  ;; This is a simplified implementation
  
  ;; Update props on the vnode
  (plist-put old-vnode :props new-props)
  
  ;; If event handlers changed, update them
  (let ((old-click (plist-get old-props :on-click))
        (new-click (plist-get new-props :on-click))
        (uuid (plist-get new-props :uuid)))
    (when (and uuid (not (equal old-click new-click)))
      ;; Update event handler
      (when old-click
        (etaf-event-remove-listener uuid 'mouse-down old-click))
      (when new-click
        (etaf-event-add-listener uuid 'mouse-down new-click)))))

(defun etaf-vdom--patch-children (old-vnode new-vnode old-children new-children container)
  "Patch children from OLD-CHILDREN to NEW-CHILDREN.
Implements a simplified diff algorithm."
  (cond
   ;; Both are text - simple replacement
   ((and (stringp old-children) (stringp new-children))
    (unless (equal old-children new-children)
      ;; Text changed - need to re-render
      (etaf-vdom-unmount container)
      (etaf-vdom-mount new-vnode container)))
   
   ;; One is text, other is array - replace
   ((or (and (stringp old-children) (listp new-children))
        (and (listp old-children) (stringp new-children)))
    (etaf-vdom-unmount container)
    (etaf-vdom-mount new-vnode container))
   
   ;; Both are arrays - diff children
   ((and (listp old-children) (listp new-children))
    (etaf-vdom--patch-children-array old-vnode new-vnode old-children new-children container))
   
   ;; Default - re-render
   (t
    (etaf-vdom-unmount container)
    (etaf-vdom-mount new-vnode container))))

(defun etaf-vdom--patch-children-array (old-vnode new-vnode old-children new-children container)
  "Patch array children using a simple diff algorithm.
This is a simplified version of Vue 3's optimized diff."
  (let ((old-len (length old-children))
        (new-len (length new-children)))
    
    ;; Simple strategy: if lengths match, patch in order
    ;; Otherwise, do a full re-render
    (if (= old-len new-len)
        ;; Patch each child in order
        (cl-loop for old-child in old-children
                 for new-child in new-children
                 do (when (and (etaf-vdom-vnode-p old-child)
                              (etaf-vdom-vnode-p new-child))
                      (if (etaf-vdom-same-type-p old-child new-child)
                          (etaf-vdom--patch-element old-child new-child container)
                        ;; Different types - would need to handle insertion/deletion
                        nil)))
      ;; Length mismatch - do full re-render for simplicity
      ;; A full implementation would handle keyed diff
      (etaf-vdom-unmount container)
      (etaf-vdom-mount new-vnode container))))

;;; Helper Functions for Testing

(defun etaf-vdom-create-test-button (label on-click-handler)
  "Create a test button VNode with LABEL and ON-CLICK-HANDLER.
Useful for testing the renderer."
  (etaf-create-vnode 'button
                     (list :on-click on-click-handler
                           :class "test-button")
                     (list (etaf-vdom-text label))))

(defun etaf-vdom-create-test-container (children)
  "Create a test container div VNode with CHILDREN.
Useful for testing the renderer."
  (etaf-create-vnode 'div
                     (list :class "test-container")
                     children))

(provide 'etaf-vdom)
;;; etaf-vdom.el ends here
