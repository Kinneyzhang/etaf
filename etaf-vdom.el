;;; etaf-vdom.el --- Virtual DOM for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: dom, virtual dom, vdom
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Virtual DOM (VNode) System for ETAF - Inspired by Vue 3
;;
;; This module implements a virtual DOM layer inspired by Vue3's VNode model
;; and rendering mechanism. It provides a declarative UI model that separates
;; the UI state representation from the actual DOM manipulation.
;;
;; Based on Vue 3's design:
;; - https://cn.vuejs.org/guide/extras/rendering-mechanism
;; - https://github.com/vuejs/core
;;
;; The virtual DOM serves as:
;; 1. A "computable UI state model" - an intermediate representation layer
;;    for declarative UI that the framework can use to calculate minimal updates
;; 2. A clean separation between logical structure and rendering details
;; 3. A way to batch and optimize DOM updates
;;
;; Key concepts from Vue 3:
;; - VNode: A virtual node representing a UI element with metadata
;; - VTree: A tree of VNodes mirroring the DOM structure
;; - Diff/Patch: Algorithm to compute minimal changes between VTrees
;; - Mount/Update/Unmount: Lifecycle operations for managing VNodes
;; - Keyed Reconciliation: Using keys to efficiently update lists
;;
;; Structure of a VNode:
;;   (:id unique-id
;;    :type element|text|comment|fragment|component
;;    :tag symbol (for element nodes)
;;    :props plist of properties from original ETML
;;    :dom the clean DOM node
;;    :tag-instance etaf-tag-instance if applicable
;;    :children list of child VNodes
;;    :parent parent VNode or nil for root
;;    :key optional key for diff optimization
;;    :mounted-p whether the node has been mounted
;;    :hooks lifecycle hooks: mounted, updated, unmounted)
;;
;; Usage:
;;   ;; Convert ETML to VTree (which also produces clean DOM)
;;   (etaf-etml-to-dom-with-vdom '(div :class "container" (a :href "/test" "Link")))
;;   
;;   ;; Diff two VTrees
;;   (etaf-vdom-diff old-vnode new-vnode)
;;   
;;   ;; Apply patches to update DOM
;;   (etaf-vdom-apply-patches patches)
;;   
;;   ;; Get the clean DOM from VTree
;;   (etaf-vdom-get-dom vnode)
;;   
;;   ;; Get tag-instance for a node (if any)
;;   (etaf-vdom-get-tag-instance vnode)

;;; Code:

(require 'cl-lib)

;;; VNode ID Generator

(defvar etaf-vdom--id-counter 0
  "Counter for generating unique VNode IDs.")

(defun etaf-vdom--generate-id ()
  "Generate a unique VNode ID."
  (cl-incf etaf-vdom--id-counter))

;;; VNode Constructor

(defun etaf-vdom-create-vnode (type &rest props)
  "Create a new VNode with TYPE and PROPS.
TYPE can be `element', `text', `comment', `fragment', or `component'.
PROPS is a plist with the following keys:
- :tag - The tag symbol (for element nodes)
- :props - Original properties from ETML (plist)
- :dom - The clean DOM node
- :tag-metadata - Tag metadata (self-closing, interactive handlers, etc.)
- :children - List of child VNodes
- :parent - Parent VNode (set during tree construction)
- :key - Optional key for diff optimization (from :key prop or :id)
- :mounted-p - Whether this VNode has been mounted to DOM
- :hooks - Lifecycle hooks (plist): :mounted, :updated, :unmounted
- :state - Interactive state (plist): :hovered, :focused, :active, :disabled

Tag metadata structure (plist):
- :self-closing - Whether tag is self-closing
- :children-allowed - Whether children are allowed
- :on-click - Click event handler
- :on-hover-enter - Mouse enter handler
- :on-hover-leave - Mouse leave handler
- :on-keydown - Keydown handler
- :hover-style - Visual hover style (CSS alist)
- :active-style - Active state style
- :focus-style - Focus state style
- :disabled-style - Disabled state style"
  (let ((vnode (list :id (etaf-vdom--generate-id)
                     :type type
                     :mounted-p nil
                     :hooks nil
                     :state nil)))
    (while props
      (setq vnode (plist-put vnode (car props) (cadr props))
            props (cddr props)))
    ;; Extract key from props if not explicitly set
    (when (and (not (plist-get vnode :key))
               (plist-get vnode :props))
      (let* ((node-props (plist-get vnode :props))
             (key (or (plist-get node-props :key)
                      (plist-get node-props :id))))
        (when key
          (setq vnode (plist-put vnode :key key)))))
    ;; Initialize state if tag has interactive metadata
    (when (and (not (plist-get vnode :state))
               (plist-get vnode :tag-metadata))
      (let ((disabled (and (plist-get vnode :props)
                           (plist-get (plist-get vnode :props) :disabled))))
        (plist-put vnode :state (list :hovered nil :focused nil :active nil :disabled disabled))))
    vnode))

(defun etaf-vdom-element (tag &rest props)
  "Create an element VNode with TAG and PROPS."
  (apply #'etaf-vdom-create-vnode 'element :tag tag props))

(defun etaf-vdom-text (content)
  "Create a text VNode with CONTENT."
  (etaf-vdom-create-vnode 'text :content content :dom content))

(defun etaf-vdom-comment (content)
  "Create a comment VNode with CONTENT."
  (etaf-vdom-create-vnode 'comment :content content))

(defun etaf-vdom-fragment (&rest children)
  "Create a fragment VNode with CHILDREN.
Fragments allow multiple root nodes without a wrapper element."
  (let ((vnode (etaf-vdom-create-vnode 'fragment)))
    (when children
      (etaf-vdom-set-children vnode children))
    vnode))

;;; VNode Accessors

(defun etaf-vdom-get-id (vnode)
  "Get the unique ID of VNODE."
  (plist-get vnode :id))

(defun etaf-vdom-get-type (vnode)
  "Get the type of VNODE."
  (plist-get vnode :type))

(defun etaf-vdom-get-tag (vnode)
  "Get the tag of VNODE (for element nodes)."
  (plist-get vnode :tag))

(defun etaf-vdom-get-props (vnode)
  "Get the original ETML properties of VNODE."
  (plist-get vnode :props))

(defun etaf-vdom-get-dom (vnode)
  "Get the clean DOM node from VNODE."
  (plist-get vnode :dom))

(defun etaf-vdom-get-tag-metadata (vnode)
  "Get the tag metadata from VNODE (if any)."
  (plist-get vnode :tag-metadata))

(defun etaf-vdom-get-state (vnode)
  "Get the interactive state from VNODE (if any)."
  (plist-get vnode :state))

(defun etaf-vdom-get-tag-instance (vnode)
  "Get the tag-instance from VNODE (if any).
This is deprecated - use tag-metadata instead."
  (plist-get vnode :tag-instance))

(defun etaf-vdom-get-children (vnode)
  "Get the children of VNODE."
  (plist-get vnode :children))

(defun etaf-vdom-get-parent (vnode)
  "Get the parent of VNODE."
  (plist-get vnode :parent))

(defun etaf-vdom-get-key (vnode)
  "Get the key of VNODE for reconciliation."
  (plist-get vnode :key))

(defun etaf-vdom-get-content (vnode)
  "Get the content of VNODE (for text/comment nodes)."
  (plist-get vnode :content))

(defun etaf-vdom-get-mounted-p (vnode)
  "Check if VNODE has been mounted."
  (plist-get vnode :mounted-p))

(defun etaf-vdom-get-hooks (vnode)
  "Get lifecycle hooks of VNODE."
  (plist-get vnode :hooks))

;;; VNode Predicates

(defun etaf-vdom-vnode-p (obj)
  "Check if OBJ is a VNode."
  (and (listp obj)
       (plist-get obj :id)
       (plist-get obj :type)))

(defun etaf-vdom-element-p (vnode)
  "Check if VNODE is an element node."
  (eq (etaf-vdom-get-type vnode) 'element))

(defun etaf-vdom-text-p (vnode)
  "Check if VNODE is a text node."
  (eq (etaf-vdom-get-type vnode) 'text))

(defun etaf-vdom-comment-p (vnode)
  "Check if VNODE is a comment node."
  (eq (etaf-vdom-get-type vnode) 'comment))

(defun etaf-vdom-fragment-p (vnode)
  "Check if VNODE is a fragment node."
  (eq (etaf-vdom-get-type vnode) 'fragment))

(defun etaf-vdom-component-p (vnode)
  "Check if VNODE is a component node."
  (eq (etaf-vdom-get-type vnode) 'component))

(defun etaf-vdom-same-type-p (vnode1 vnode2)
  "Check if VNODE1 and VNODE2 are the same type.
Two VNodes are the same type if they have:
- Same type (element, text, etc.)
- Same tag (for elements)
- Same key (if present)"
  (and (eq (etaf-vdom-get-type vnode1) (etaf-vdom-get-type vnode2))
       (or (not (etaf-vdom-element-p vnode1))
           (eq (etaf-vdom-get-tag vnode1) (etaf-vdom-get-tag vnode2)))
       ;; If both have keys, they must match
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

(defun etaf-vdom-set-dom (vnode dom)
  "Set the DOM of VNODE to DOM."
  (plist-put vnode :dom dom))

(defun etaf-vdom-set-tag-instance (vnode instance)
  "Set the tag-instance of VNODE to INSTANCE.
This is deprecated - use tag-metadata instead."
  (plist-put vnode :tag-instance instance))

(defun etaf-vdom-set-tag-metadata (vnode metadata)
  "Set the tag-metadata of VNODE to METADATA."
  (plist-put vnode :tag-metadata metadata))

(defun etaf-vdom-set-state (vnode state)
  "Set the interactive state of VNODE to STATE."
  (plist-put vnode :state state))

(defun etaf-vdom-set-mounted-p (vnode mounted)
  "Set the mounted state of VNODE to MOUNTED."
  (plist-put vnode :mounted-p mounted))

(defun etaf-vdom-set-hooks (vnode hooks)
  "Set lifecycle hooks of VNODE to HOOKS (a plist)."
  (plist-put vnode :hooks hooks))

(defun etaf-vdom-add-hook (vnode hook-type func)
  "Add a lifecycle HOOK-TYPE (keyword) with FUNC to VNODE.
HOOK-TYPE can be :mounted, :updated, or :unmounted."
  (let* ((hooks (or (etaf-vdom-get-hooks vnode) nil))
         (existing (plist-get hooks hook-type))
         (new-hooks (if existing
                        (append existing (list func))
                      (list func))))
    (plist-put hooks hook-type new-hooks)
    (etaf-vdom-set-hooks vnode hooks)))

;;; VTree Mapping

(defvar etaf-vdom--dom-to-vnode-map nil
  "Hash table mapping DOM nodes to VNodes.
This allows finding the VNode for a given DOM node.")

(defun etaf-vdom-init-map ()
  "Initialize or reset the DOM-to-VNode mapping."
  (setq etaf-vdom--dom-to-vnode-map (make-hash-table :test 'eq)))

(defun etaf-vdom-register-mapping (dom vnode)
  "Register a mapping from DOM to VNODE."
  (when etaf-vdom--dom-to-vnode-map
    (puthash dom vnode etaf-vdom--dom-to-vnode-map)))

(defun etaf-vdom-get-vnode-for-dom (dom)
  "Get the VNode for a given DOM node."
  (when etaf-vdom--dom-to-vnode-map
    (gethash dom etaf-vdom--dom-to-vnode-map)))

;;; VTree Traversal

(defun etaf-vdom-walk (vnode func)
  "Walk the VNode tree rooted at VNODE, calling FUNC on each node."
  (when vnode
    (funcall func vnode)
    (dolist (child (etaf-vdom-get-children vnode))
      (etaf-vdom-walk child func))))

(defun etaf-vdom-find (vnode predicate)
  "Find the first VNode in tree rooted at VNODE satisfying PREDICATE."
  (catch 'found
    (etaf-vdom-walk vnode
                    (lambda (node)
                      (when (funcall predicate node)
                        (throw 'found node))))
    nil))

(defun etaf-vdom-find-all (vnode predicate)
  "Find all VNodes in tree rooted at VNODE satisfying PREDICATE."
  (let (results)
    (etaf-vdom-walk vnode
                    (lambda (node)
                      (when (funcall predicate node)
                        (push node results))))
    (nreverse results)))

;;; VTree to DOM Extraction

(defun etaf-vdom-extract-dom (vnode)
  "Extract the clean DOM tree from a VNode tree.
This recursively builds the DOM from VNode :dom properties."
  (if (etaf-vdom-text-p vnode)
      ;; Text node - return content directly
      (etaf-vdom-get-dom vnode)
    ;; Element node - construct DOM
    (let* ((tag (etaf-vdom-get-tag vnode))
           (dom-node (etaf-vdom-get-dom vnode))
           (children (etaf-vdom-get-children vnode)))
      (if dom-node
          ;; DOM already constructed
          dom-node
        ;; Construct DOM from VNode structure
        (let ((attrs nil)
              (child-doms nil))
          ;; Get attrs from props (converting from plist to alist)
          (let ((props (etaf-vdom-get-props vnode)))
            (while props
              (let ((key (car props))
                    (value (cadr props)))
                (when (keywordp key)
                  (push (cons (intern (substring (symbol-name key) 1)) value)
                        attrs)))
              (setq props (cddr props))))
          ;; Process children
          (setq child-doms
                (mapcar #'etaf-vdom-extract-dom children))
          ;; Build DOM node
          (cons tag (cons (nreverse attrs) child-doms)))))))

;;; VNode Search by DOM

(defun etaf-vdom-find-by-dom (root dom-node)
  "Find the VNode in tree ROOT that corresponds to DOM-NODE."
  (etaf-vdom-find root
                  (lambda (vnode)
                    (eq (etaf-vdom-get-dom vnode) dom-node))))

;;; VTree Result Structure

(cl-defstruct (etaf-vdom-result
               (:constructor etaf-vdom-make-result))
  "Structure holding both VTree and clean DOM."
  vtree  ; Root VNode of the virtual DOM tree
  dom)   ; Clean DOM without tag-instances

(defun etaf-vdom-result-get-vtree (result)
  "Get the VTree from RESULT."
  (etaf-vdom-result-vtree result))

(defun etaf-vdom-result-get-dom (result)
  "Get the clean DOM from RESULT."
  (etaf-vdom-result-dom result))

;;; VNode Lookup Functions

(defun etaf-vdom-get-tag-instance-for-dom (result dom-node)
  "Get the tag-instance for a DOM-NODE from RESULT.
RESULT is an etaf-vdom-result structure."
  (when-let* ((vtree (etaf-vdom-result-get-vtree result))
              (vnode (etaf-vdom-find-by-dom vtree dom-node)))
    (etaf-vdom-get-tag-instance vnode)))

(defun etaf-vdom-find-vnode-by-id (root target-id)
  "Find VNode with TARGET-ID in tree ROOT."
  (etaf-vdom-find root
                  (lambda (vnode)
                    (equal (etaf-vdom-get-id vnode) target-id))))

;;; ============================================================================
;;; Diff/Patch Algorithm (Vue 3 inspired)
;;; ============================================================================

;; The diff algorithm compares two VNode trees and produces a list of patches
;; (operations) that need to be applied to transform the old tree into the new one.
;; This is inspired by Vue 3's diff algorithm with optimizations for common cases.

(defconst etaf-vdom-patch-types
  '(create     ; Create a new node
    remove     ; Remove a node
    replace    ; Replace a node with a different type
    update     ; Update node properties
    reorder)   ; Reorder children (for keyed lists)
  "Types of patches that can be generated by the diff algorithm.")

(cl-defstruct (etaf-vdom-patch
               (:constructor etaf-vdom-make-patch))
  "A patch operation to transform old VTree to new VTree."
  type        ; Type of patch (from etaf-vdom-patch-types)
  old-vnode   ; Old VNode (nil for create)
  new-vnode   ; New VNode (nil for remove)
  parent      ; Parent VNode
  index       ; Index in parent's children (for positioning)
  props)      ; Additional properties specific to patch type

(defun etaf-vdom-diff (old-vnode new-vnode)
  "Compare OLD-VNODE and NEW-VNODE and return a list of patches.
This implements a simplified version of Vue 3's diff algorithm:
1. If nodes are not the same type, replace the entire subtree
2. If nodes are the same type, diff their properties and children
3. For children, use keys for efficient reconciliation when available"
  (let ((patches nil))
    (cond
     ;; Case 1: Old node is nil - CREATE new node
     ((null old-vnode)
      (push (etaf-vdom-make-patch
             :type 'create
             :new-vnode new-vnode)
            patches))
     
     ;; Case 2: New node is nil - REMOVE old node
     ((null new-vnode)
      (push (etaf-vdom-make-patch
             :type 'remove
             :old-vnode old-vnode)
            patches))
     
     ;; Case 3: Different types - REPLACE entire subtree
     ((not (etaf-vdom-same-type-p old-vnode new-vnode))
      (push (etaf-vdom-make-patch
             :type 'replace
             :old-vnode old-vnode
             :new-vnode new-vnode)
            patches))
     
     ;; Case 4: Same type - UPDATE properties and diff children
     (t
      (let ((prop-patches (etaf-vdom-diff-props old-vnode new-vnode)))
        (when prop-patches
          (push (etaf-vdom-make-patch
                 :type 'update
                 :old-vnode old-vnode
                 :new-vnode new-vnode
                 :props prop-patches)
                patches)))
      
      ;; Diff children if this is an element or fragment
      (when (or (etaf-vdom-element-p new-vnode)
                (etaf-vdom-fragment-p new-vnode))
        (let ((child-patches (etaf-vdom-diff-children old-vnode new-vnode)))
          (setq patches (append child-patches patches))))))
    
    (nreverse patches)))

(defun etaf-vdom-diff-props (old-vnode new-vnode)
  "Compare properties of OLD-VNODE and NEW-VNODE.
Returns a plist of changed properties, or nil if no changes."
  (let ((old-props (etaf-vdom-get-props old-vnode))
        (new-props (etaf-vdom-get-props new-vnode))
        (changes nil))
    
    ;; Check for new or changed properties
    (let ((props new-props))
      (while props
        (let* ((key (car props))
               (new-val (cadr props))
               (old-val (plist-get old-props key)))
          (unless (equal old-val new-val)
            (setq changes (plist-put changes key new-val))))
        (setq props (cddr props))))
    
    ;; Check for removed properties (nil value indicates removal)
    (let ((props old-props))
      (while props
        (let ((key (car props)))
          (unless (plist-member new-props key)
            (setq changes (plist-put changes key nil))))
        (setq props (cddr props))))
    
    changes))

(defun etaf-vdom-diff-children (old-vnode new-vnode)
  "Diff children of OLD-VNODE and NEW-VNODE.
Uses keys for efficient reconciliation when available.
Returns a list of patches for the children."
  (let ((old-children (etaf-vdom-get-children old-vnode))
        (new-children (etaf-vdom-get-children new-vnode)))
    
    ;; Check if children have keys
    (if (or (cl-some #'etaf-vdom-get-key old-children)
            (cl-some #'etaf-vdom-get-key new-children))
        ;; Use keyed diff for better performance with lists
        (etaf-vdom-diff-children-keyed old-children new-children old-vnode)
      ;; Use simple ordered diff
      (etaf-vdom-diff-children-ordered old-children new-children old-vnode))))

(defun etaf-vdom-diff-children-ordered (old-children new-children parent)
  "Diff children in order without using keys.
This is a simple algorithm that compares children position by position."
  (let ((patches nil)
        (max-len (max (length old-children) (length new-children))))
    (dotimes (i max-len)
      (let ((old-child (nth i old-children))
            (new-child (nth i new-children)))
        (let ((child-patches (etaf-vdom-diff old-child new-child)))
          (dolist (patch child-patches)
            ;; Set parent and index for the patch
            (setf (etaf-vdom-patch-parent patch) parent)
            (setf (etaf-vdom-patch-index patch) i)
            (push patch patches)))))
    (nreverse patches)))

(defun etaf-vdom-diff-children-keyed (old-children new-children parent)
  "Diff children using keys for efficient reconciliation.
This implements a simplified version of Vue 3's keyed diff algorithm."
  (let ((patches nil)
        (old-keyed (make-hash-table :test 'equal))
        (new-keyed (make-hash-table :test 'equal))
        (old-index 0)
        (new-index 0))
    
    ;; Build key maps
    (dolist (child old-children)
      (when-let ((key (etaf-vdom-get-key child)))
        (puthash key (cons child old-index) old-keyed))
      (cl-incf old-index))
    
    (setq new-index 0)
    (dolist (child new-children)
      (when-let ((key (etaf-vdom-get-key child)))
        (puthash key (cons child new-index) new-keyed))
      (cl-incf new-index))
    
    ;; Process new children
    (setq new-index 0)
    (dolist (new-child new-children)
      (let* ((key (etaf-vdom-get-key new-child))
             (old-entry (and key (gethash key old-keyed))))
        (if old-entry
            ;; Found matching old node by key
            (let* ((old-child (car old-entry))
                   (old-idx (cdr old-entry))
                   (child-patches (etaf-vdom-diff old-child new-child)))
              ;; Check if node needs to be moved
              (when (/= old-idx new-index)
                (push (etaf-vdom-make-patch
                       :type 'reorder
                       :old-vnode old-child
                       :new-vnode new-child
                       :parent parent
                       :index new-index)
                      patches))
              ;; Add property/children patches
              (dolist (patch child-patches)
                (setf (etaf-vdom-patch-parent patch) parent)
                (setf (etaf-vdom-patch-index patch) new-index)
                (push patch patches)))
          ;; No matching old node - create new
          (let ((patch (etaf-vdom-make-patch
                        :type 'create
                        :new-vnode new-child
                        :parent parent
                        :index new-index)))
            (push patch patches))))
      (cl-incf new-index))
    
    ;; Find removed nodes (in old but not in new)
    (maphash
     (lambda (key old-entry)
       (unless (gethash key new-keyed)
         (let ((old-child (car old-entry)))
           (push (etaf-vdom-make-patch
                  :type 'remove
                  :old-vnode old-child
                  :parent parent)
                 patches))))
     old-keyed)
    
    (nreverse patches)))

;;; ============================================================================
;;; Lifecycle Management (Vue 3 inspired)
;;; ============================================================================

(defun etaf-vdom-mount (vnode)
  "Mount VNODE and call mounted lifecycle hooks.
This marks the VNode as mounted and triggers :mounted hooks."
  (when vnode
    (etaf-vdom-set-mounted-p vnode t)
    ;; Call mounted hooks
    (when-let ((hooks (etaf-vdom-get-hooks vnode)))
      (dolist (func (plist-get hooks :mounted))
        (when (functionp func)
          (funcall func vnode))))
    ;; Recursively mount children
    (dolist (child (etaf-vdom-get-children vnode))
      (etaf-vdom-mount child))))

(defun etaf-vdom-unmount (vnode)
  "Unmount VNODE and call unmounted lifecycle hooks.
This marks the VNode as unmounted and triggers :unmounted hooks."
  (when vnode
    ;; Recursively unmount children first
    (dolist (child (etaf-vdom-get-children vnode))
      (etaf-vdom-unmount child))
    ;; Call unmounted hooks
    (when-let ((hooks (etaf-vdom-get-hooks vnode)))
      (dolist (func (plist-get hooks :unmounted))
        (when (functionp func)
          (funcall func vnode))))
    (etaf-vdom-set-mounted-p vnode nil)))

(defun etaf-vdom-update (old-vnode new-vnode)
  "Update from OLD-VNODE to NEW-VNODE and call updated lifecycle hooks.
This applies changes and triggers :updated hooks on NEW-VNODE."
  (when (and old-vnode new-vnode)
    ;; Call updated hooks on new vnode
    (when-let ((hooks (etaf-vdom-get-hooks new-vnode)))
      (dolist (func (plist-get hooks :updated))
        (when (functionp func)
          (funcall func new-vnode old-vnode))))))

(defun etaf-vdom-apply-patches (patches)
  "Apply a list of PATCHES to transform the VTree.
This is a placeholder for actual DOM manipulation.
In ETAF, this would trigger re-rendering of affected portions."
  ;; This function would integrate with ETAF's rendering pipeline
  ;; For now, it just processes the patches conceptually
  (dolist (patch patches)
    (pcase (etaf-vdom-patch-type patch)
      ('create
       (let ((vnode (etaf-vdom-patch-new-vnode patch)))
         (etaf-vdom-mount vnode)))
      
      ('remove
       (let ((vnode (etaf-vdom-patch-old-vnode patch)))
         (etaf-vdom-unmount vnode)))
      
      ('replace
       (let ((old (etaf-vdom-patch-old-vnode patch))
             (new (etaf-vdom-patch-new-vnode patch)))
         (etaf-vdom-unmount old)
         (etaf-vdom-mount new)))
      
      ('update
       (let ((old (etaf-vdom-patch-old-vnode patch))
             (new (etaf-vdom-patch-new-vnode patch)))
         (etaf-vdom-update old new)))
      
      ('reorder
       ;; Reordering would involve moving DOM nodes
       ;; This is typically handled by the render system
       nil))))

;;; ============================================================================
;;; Tag Metadata Support (replaces etaf-etml-tag.el functionality)
;;; ============================================================================

(defconst etaf-vdom-builtin-tags
  '(;; Block-level elements (display: block in UA stylesheet)
    div p h1 h2 h3 h4 h5 h6
    header footer section article aside nav main
    ul ol blockquote pre form fieldset
    figure figcaption details
    ;; Inline elements (display: inline by default)
    span em strong b i u s del ins mark small sub sup
    code kbd samp var abbr cite q label
    ;; Inline-block media elements (display: inline-block in UA stylesheet)
    video audio canvas svg
    ;; Form elements
    select
    ;; Dialog
    dialog
    ;; Table elements (display: table-* in UA stylesheet)
    table tr th td thead tbody tfoot caption
    ;; Other elements  
    option legend li)
  "List of built-in HTML-like tags.
These tags are automatically recognized and get their styles from the UA stylesheet.")

(defconst etaf-vdom-self-closing-tags
  '(br hr img input)
  "List of self-closing tags that don't allow children.")

(defun etaf-vdom-tag-defined-p (tag)
  "Check if TAG is a built-in or self-closing tag."
  (or (memq tag etaf-vdom-builtin-tags)
      (memq tag etaf-vdom-self-closing-tags)))

(defun etaf-vdom-tag-self-closing-p (tag)
  "Check if TAG is self-closing."
  (memq tag etaf-vdom-self-closing-tags))

(defun etaf-vdom-create-tag-metadata (tag attrs children)
  "Create tag metadata for TAG with ATTRS and CHILDREN.
This replaces the old etaf-etml-tag-instance functionality.
Returns a plist with:
- :tag - The tag name
- :self-closing - Whether tag is self-closing
- :children-allowed - Whether children are allowed
- :on-click - Click event handler (if tag has one)
- :on-hover-enter - Mouse enter handler
- :on-hover-leave - Mouse leave handler
- :on-keydown - Keydown handler
- :hover-style - Visual hover style
- :active-style - Active state style
- :focus-style - Focus state style
- :disabled-style - Disabled state style"
  (let ((metadata (list :tag tag
                        :self-closing (etaf-vdom-tag-self-closing-p tag)
                        :children-allowed (not (etaf-vdom-tag-self-closing-p tag)))))
    ;; Add built-in event handlers for specific tags
    (pcase tag
      ('a
       (setq metadata (plist-put metadata :hover-style '((color . "#1d4ed8"))))
       (setq metadata (plist-put metadata :on-click
                                  (lambda (event)
                                    (let* ((target (plist-get event :target))
                                           (props (etaf-vdom-get-props target))
                                           (href (plist-get props :href)))
                                      (when href
                                        (browse-url href)))))))
      ('button
       (setq metadata (plist-put metadata :hover-style '((background-color . "#e5e7eb"))))
       (setq metadata (plist-put metadata :active-style '((background-color . "#d1d5db"))))
       (setq metadata (plist-put metadata :disabled-style 
                                  '((background-color . "#f3f4f6")
                                    (color . "#9ca3af")
                                    (cursor . "not-allowed"))))
       (setq metadata (plist-put metadata :on-click
                                  (lambda (event)
                                    (let* ((target (plist-get event :target))
                                           (state (etaf-vdom-get-state target)))
                                      (unless (plist-get state :disabled)
                                        (let* ((props (etaf-vdom-get-props target))
                                               (custom-handler (plist-get props :on-click)))
                                          (when (functionp custom-handler)
                                            (condition-case nil
                                                (funcall custom-handler)
                                              (wrong-number-of-arguments
                                               (funcall custom-handler event)))))))))))
      ('input
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6"))))
       (setq metadata (plist-put metadata :disabled-style
                                  '((background-color . "#f3f4f6")
                                    (color . "#9ca3af")))))
      ('textarea
       (setq metadata (plist-put metadata :focus-style '((border-color . "#3b82f6")))))
      ('summary
       (setq metadata (plist-put metadata :on-click
                                  (lambda (event)
                                    (let* ((target (plist-get event :target))
                                           (state (etaf-vdom-get-state target)))
                                      (plist-put state :expanded
                                                 (not (plist-get state :expanded)))))))))
    metadata))

(defun etaf-vdom-has-interactive-capability-p (vnode)
  "Check if VNODE has interactive capabilities.
Returns t if the VNode has any event handlers or state styles."
  (when-let ((metadata (etaf-vdom-get-tag-metadata vnode)))
    (or (plist-get metadata :on-click)
        (plist-get metadata :on-hover-enter)
        (plist-get metadata :on-hover-leave)
        (plist-get metadata :on-keydown)
        (plist-get metadata :hover-style)
        (plist-get metadata :active-style)
        (plist-get metadata :focus-style))))

;;; ============================================================================
;;; Helper Functions for Interactive Elements (replaces etaf-etml-tag.el)
;;; ============================================================================

(defun etaf-vdom-setup-keymap (tag-metadata vnode-or-state)
  "Set up keybindings for TAG-METADATA.
VNODE-OR-STATE can be either a VNode or a state plist.
Creates a keymap with bindings for RET, SPC, mouse-1, etc.
Returns the configured keymap."
  (let* ((keymap (make-sparse-keymap))
         (on-click (plist-get tag-metadata :on-click))
         (on-keydown (plist-get tag-metadata :on-keydown))
         ;; Determine state - either from VNode or passed directly
         (state (if (and (listp vnode-or-state)
                         (plist-member vnode-or-state :id))
                    (etaf-vdom-get-state vnode-or-state)
                  vnode-or-state)))
    ;; Click handler via RET and SPC keys
    (when on-click
      (define-key keymap (kbd "RET")
        (lambda ()
          (interactive)
          (let ((event (list :type 'click
                             :target vnode-or-state
                             :timestamp (current-time))))
            (funcall on-click event))))
      (define-key keymap (kbd "SPC")
        (lambda ()
          (interactive)
          (let ((event (list :type 'click
                             :target vnode-or-state
                             :timestamp (current-time))))
            (funcall on-click event))))
      ;; Mouse click handler
      (define-key keymap [mouse-1]
        (lambda (event)
          (interactive "e")
          (let* ((posn (event-start event))
                 (pos (posn-point posn))
                 (click-event (list :type 'click
                                    :target vnode-or-state
                                    :timestamp (current-time)
                                    :mouse-event event
                                    :position pos)))
            (funcall on-click click-event)))))
    ;; Keydown handler
    (when on-keydown
      (define-key keymap [remap self-insert-command]
        (lambda ()
          (interactive)
          (let* ((key (this-command-keys-vector))
                 (event (list :type 'keydown
                              :target vnode-or-state
                              :timestamp (current-time)
                              :key key
                              :key-char (when (> (length key) 0)
                                          (aref key (1- (length key)))))))
            (funcall on-keydown event)))))
    keymap))

(defvar-local etaf-vdom--current-hover-metadata nil
  "The tag metadata currently being hovered over in this buffer.")

(defun etaf-vdom-help-echo-handler (window _obj pos)
  "Help-echo function to track mouse hover and dispatch hover events.
Uses tag metadata instead of tag instances.
WINDOW is the window where the mouse is.
POS is the buffer position under the mouse."
  (when (and window pos)
    (let* ((metadata (get-text-property pos 'etaf-tag-metadata))
           (state (get-text-property pos 'etaf-tag-state)))
      ;; Handle hover-leave for previous element
      (when (and etaf-vdom--current-hover-metadata
                 (not (eq etaf-vdom--current-hover-metadata metadata)))
        (when-let ((on-hover-leave (plist-get etaf-vdom--current-hover-metadata :on-hover-leave)))
          (let ((event (list :type 'hover-leave
                             :target metadata
                             :timestamp (current-time))))
            (funcall on-hover-leave event))))
      ;; Handle hover-enter for new element
      (when (and metadata (not (eq etaf-vdom--current-hover-metadata metadata)))
        (when-let ((on-hover-enter (plist-get metadata :on-hover-enter)))
          (let ((event (list :type 'hover-enter
                             :target metadata
                             :timestamp (current-time))))
            (funcall on-hover-enter event))))
      ;; Update current hover
      (setq etaf-vdom--current-hover-metadata metadata)
      ;; Return help-echo string
      (when metadata
        (let ((tag-name (or (plist-get metadata :tag) "element")))
          (format "Click or press RET/SPC to activate %s" tag-name))))))

(provide 'etaf-vdom)
;;; etaf-vdom.el ends here
