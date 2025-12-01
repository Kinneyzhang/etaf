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

;; Virtual DOM (VNode) System for ETAF
;;
;; This module implements a virtual DOM layer inspired by Vue3's VNode model.
;; The virtual DOM separates the clean DOM structure from extra information
;; like event handlers, tag instances, and component state.
;;
;; Key concepts:
;; - VNode: A virtual node that wraps a real DOM node with additional metadata
;; - VTree: A tree of VNodes that mirrors the real DOM structure
;; - The real DOM remains clean (no etaf-tag-instance in attributes)
;; - Event handlers and tag instances are stored in the virtual DOM layer
;;
;; Structure of a VNode:
;;   (:type element|text|comment
;;    :tag symbol (for element nodes)
;;    :props plist of properties from original ETML
;;    :dom the clean DOM node
;;    :tag-instance etaf-tag-instance if applicable
;;    :children list of child VNodes
;;    :parent parent VNode or nil for root
;;    :key optional key for diff optimization)
;;
;; Usage:
;;   ;; Convert ETML to VTree (which also produces clean DOM)
;;   (etaf-vdom-from-etml '(div :class "container" (a :href "/test" "Link")))
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
TYPE can be `element', `text', or `comment'.
PROPS is a plist with the following keys:
- :tag - The tag symbol (for element nodes)
- :props - Original properties from ETML (plist)
- :dom - The clean DOM node
- :tag-instance - The etaf-tag-instance if applicable
- :children - List of child VNodes
- :parent - Parent VNode (set during tree construction)
- :key - Optional key for diff optimization"
  (let ((vnode (list :id (etaf-vdom--generate-id)
                     :type type)))
    (while props
      (setq vnode (plist-put vnode (car props) (cadr props))
            props (cddr props)))
    vnode))

(defun etaf-vdom-element (tag &rest props)
  "Create an element VNode with TAG and PROPS."
  (apply #'etaf-vdom-create-vnode 'element :tag tag props))

(defun etaf-vdom-text (content)
  "Create a text VNode with CONTENT."
  (etaf-vdom-create-vnode 'text :content content :dom content))

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

(defun etaf-vdom-get-tag-instance (vnode)
  "Get the tag-instance from VNODE (if any)."
  (plist-get vnode :tag-instance))

(defun etaf-vdom-get-children (vnode)
  "Get the children of VNODE."
  (plist-get vnode :children))

(defun etaf-vdom-get-parent (vnode)
  "Get the parent of VNODE."
  (plist-get vnode :parent))

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
  "Set the tag-instance of VNODE to INSTANCE."
  (plist-put vnode :tag-instance instance))

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

(provide 'etaf-vdom)
;;; etaf-vdom.el ends here
