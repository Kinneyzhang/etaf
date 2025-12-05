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
                                  (lambda ()
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
                                  (lambda ()
                                    (let ((state (plist-get metadata :state)))
                                      (unless (plist-get state :disabled)
                                        (when-let ((custom-handler (plist-get attrs :on-click)))
                                          (funcall custom-handler))))))))
      ('input
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6"))))
       (setq metadata (plist-put metadata :disabled-style
                                  '((background-color . "#f3f4f6")
                                    (color . "#9ca3af")))))
      ('textarea
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6")))))
      ('summary
       (setq metadata (plist-put metadata :on-click
                                  (lambda ()
                                    ;; Toggle details element
                                    (message "Summary clicked"))))))
    metadata))

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
           ;; Convert props plist to attrs alist for DOM
           (attrs (etaf-vdom--props-to-attrs props))
           ;; Render children
           (child-doms (cond
                        ;; String children
                        ((stringp children)
                         (list children))
                        ;; Array children - render recursively
                        ((listp children)
                         (delq nil (mapcar #'etaf-vdom-render children)))
                        ;; Other (nil, etc.)
                        (t nil))))
      ;; Build DOM: (tag ((attrs...)) children...)
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

(provide 'etaf-vdom)
;;; etaf-vdom.el ends here
