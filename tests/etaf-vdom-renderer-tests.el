;;; etaf-vdom-renderer-tests.el --- Tests for VNode renderer (mount/unmount/patch) -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: tests, vdom, renderer

;;; Commentary:

;; Tests for the VNode renderer functions:
;; - etaf-vdom-mount - Mount VNodes to buffer
;; - etaf-vdom-unmount - Unmount and clean up VNodes
;; - etaf-vdom-patch - Diff and patch VNodes for updates
;; - Event handler binding and triggering
;; - Event bubbling

;;; Code:

(require 'ert)
(require 'etaf-vdom)
(require 'etaf-event)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-layout-string)

;;; ============================================================================
;;; Test Suite 1: Basic VNode Creation
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-create-vnode ()
  "Test creating basic VNodes."
  (let ((text-node (etaf-vdom-text "Hello"))
        (element-node (etaf-create-vnode 'div
                                          (list :class "container")
                                          (list (etaf-vdom-text "Content")))))
    
    ;; Test text node
    (should (etaf-vdom-text-p text-node))
    (should (equal (etaf-vdom-get-children text-node) "Hello"))
    
    ;; Test element node
    (should (etaf-vdom-element-p element-node))
    (should (eq (etaf-vdom-get-type element-node) 'div))
    (should (equal (plist-get (etaf-vdom-get-props element-node) :class) "container"))
    (should (= (length (etaf-vdom-get-children element-node)) 1))))

(ert-deftest etaf-vdom-renderer-test-create-button ()
  "Test creating a button VNode with event handler."
  (let* ((clicked nil)
         (button (etaf-vdom-create-test-button
                  "Click Me"
                  (lambda () (setq clicked t)))))
    
    (should (etaf-vdom-element-p button))
    (should (eq (etaf-vdom-get-type button) 'button))
    (should (plist-get (etaf-vdom-get-props button) :on-click))
    
    ;; Test handler can be called
    (funcall (plist-get (etaf-vdom-get-props button) :on-click))
    (should clicked)))

;;; ============================================================================
;;; Test Suite 2: VNode Mounting
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-mount-simple ()
  "Test mounting a simple VNode to a buffer."
  (with-temp-buffer
    (let* ((vnode (etaf-create-vnode 'div
                                     (list :class "test")
                                     (list (etaf-vdom-text "Test Content"))))
           (mounted (etaf-vdom-mount vnode (current-buffer))))
      
      ;; Check vnode was mounted
      (should mounted)
      (should (plist-get mounted :el))
      
      ;; Check buffer has content
      (should (> (buffer-size) 0))
      
      ;; Check stored in mounted vnodes
      (should (gethash (current-buffer) etaf-vdom--mounted-vnodes)))))

(ert-deftest etaf-vdom-renderer-test-mount-button ()
  "Test mounting a button with event handler."
  (with-temp-buffer
    (let* ((clicked nil)
           (button (etaf-vdom-create-test-button
                    "Test Button"
                    (lambda () (setq clicked t))))
           (mounted (etaf-vdom-mount button (current-buffer))))
      
      ;; Check mounted
      (should mounted)
      (should (plist-get mounted :el))
      (should (> (buffer-size) 0))
      
      ;; Note: Event handlers are registered but we can't easily test
      ;; the full click simulation without a real buffer and mouse events
      ;; The handler function itself can be tested though
      (let ((handler (plist-get (etaf-vdom-get-props button) :on-click)))
        (should handler)
        (funcall handler)
        (should clicked)))))

;;; ============================================================================
;;; Test Suite 3: VNode Unmounting
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-unmount ()
  "Test unmounting a VNode from buffer."
  (with-temp-buffer
    (let* ((vnode (etaf-create-vnode 'div
                                     (list :class "test")
                                     (list (etaf-vdom-text "Test Content")))))
      
      ;; Mount first
      (etaf-vdom-mount vnode (current-buffer))
      (should (> (buffer-size) 0))
      
      ;; Now unmount
      (let ((result (etaf-vdom-unmount (current-buffer))))
        (should result)
        
        ;; Buffer should be empty
        (should (= (buffer-size) 0))
        
        ;; VNode should be removed from storage
        (should-not (gethash (current-buffer) etaf-vdom--mounted-vnodes))))))

;;; ============================================================================
;;; Test Suite 4: VNode Patching (Diff)
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-patch-same-type ()
  "Test patching VNodes of the same type."
  (with-temp-buffer
    (let* ((old-vnode (etaf-create-vnode 'div
                                         (list :class "old")
                                         (list (etaf-vdom-text "Old Content"))))
           (new-vnode (etaf-create-vnode 'div
                                         (list :class "new")
                                         (list (etaf-vdom-text "New Content")))))
      
      ;; Mount old vnode
      (etaf-vdom-mount old-vnode (current-buffer))
      (let ((old-content (buffer-string)))
        
        ;; Patch with new vnode
        (etaf-vdom-patch old-vnode new-vnode (current-buffer))
        
        ;; Content should be updated
        ;; (exact content depends on layout, but should be different)
        (should (not (equal old-content (buffer-string))))
        
        ;; New vnode should be stored
        (should (eq (gethash (current-buffer) etaf-vdom--mounted-vnodes)
                    new-vnode))))))

(ert-deftest etaf-vdom-renderer-test-patch-different-type ()
  "Test patching VNodes of different types (should replace)."
  (with-temp-buffer
    (let* ((old-vnode (etaf-create-vnode 'div
                                         nil
                                         (list (etaf-vdom-text "Old"))))
           (new-vnode (etaf-create-vnode 'span
                                         nil
                                         (list (etaf-vdom-text "New")))))
      
      ;; Mount old vnode
      (etaf-vdom-mount old-vnode (current-buffer))
      
      ;; Patch with different type
      (let ((result (etaf-vdom-patch old-vnode new-vnode (current-buffer))))
        
        ;; Should have replaced (unmount + mount)
        (should result)
        (should (eq (etaf-vdom-get-type result) 'span))
        
        ;; New vnode should be stored
        (should (eq (gethash (current-buffer) etaf-vdom--mounted-vnodes)
                    new-vnode))))))

(ert-deftest etaf-vdom-renderer-test-patch-text-change ()
  "Test patching when text content changes."
  (with-temp-buffer
    (let* ((old-vnode (etaf-create-vnode 'p
                                         nil
                                         (list (etaf-vdom-text "Original Text"))))
           (new-vnode (etaf-create-vnode 'p
                                         nil
                                         (list (etaf-vdom-text "Updated Text")))))
      
      ;; Mount old vnode
      (etaf-vdom-mount old-vnode (current-buffer))
      (let ((old-content (buffer-string)))
        
        ;; Patch with new text
        (etaf-vdom-patch old-vnode new-vnode (current-buffer))
        
        ;; Content should change
        (should (not (equal old-content (buffer-string))))))))

;;; ============================================================================
;;; Test Suite 5: Event System Integration
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-event-registration ()
  "Test that event handlers are registered during mount."
  (with-temp-buffer
    (let* ((clicked nil)
           (button (etaf-create-vnode 'button
                                      (list :on-click (lambda () (setq clicked t))
                                           :uuid "test-btn-1")
                                      (list (etaf-vdom-text "Click")))))
      
      ;; Mount button
      (etaf-vdom-mount button (current-buffer))
      
      ;; Check event system is initialized
      (should etaf-event--elements)
      
      ;; Check element is registered (by UUID)
      (let ((element (etaf-event-get-element "test-btn-1")))
        (should element)))))

(ert-deftest etaf-vdom-renderer-test-event-bubbling ()
  "Test event bubbling through parent chain."
  (let ((parent-clicked nil)
        (child-clicked nil))
    
    (with-temp-buffer
      ;; Initialize event system
      (etaf-event-init)
      
      ;; Create parent and child nodes with handlers
      (let* ((child (etaf-create-vnode 'button
                                       (list :on-click (lambda ()
                                                        (setq child-clicked t))
                                            :uuid "child-1")
                                       (list (etaf-vdom-text "Child"))))
             (parent (etaf-create-vnode 'div
                                        (list :on-click (lambda ()
                                                         (setq parent-clicked t))
                                             :uuid "parent-1")
                                        (list child))))
        
        ;; Set up parent reference for bubbling
        (plist-put child :parent parent)
        
        ;; Register elements manually for testing
        (etaf-event-register-element "parent-1" parent 1 100)
        (etaf-event-register-element "child-1" child 1 50)
        
        ;; Add listeners
        (etaf-event-add-listener "parent-1" 'click
                                (lambda (uuid data) (setq parent-clicked t)))
        (etaf-event-add-listener "child-1" 'click
                                (lambda (uuid data) (setq child-clicked t)))
        
        ;; Trigger click on child with bubbling
        (etaf-event-dispatch-with-bubbling "child-1" 'click)
        
        ;; Both should be clicked due to bubbling
        (should child-clicked)
        (should parent-clicked)))))

;;; ============================================================================
;;; Test Suite 6: Helper Functions
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-same-type-p ()
  "Test etaf-vdom-same-type-p for diff algorithm."
  (let ((node1 (etaf-create-vnode 'div (list :key "a") nil))
        (node2 (etaf-create-vnode 'div (list :key "a") nil))
        (node3 (etaf-create-vnode 'div (list :key "b") nil))
        (node4 (etaf-create-vnode 'span (list :key "a") nil)))
    
    ;; Same type and key
    (should (etaf-vdom-same-type-p node1 node2))
    
    ;; Same type, different key
    (should-not (etaf-vdom-same-type-p node1 node3))
    
    ;; Different type
    (should-not (etaf-vdom-same-type-p node1 node4))))

(ert-deftest etaf-vdom-renderer-test-vnode-predicates ()
  "Test VNode type predicates."
  (let ((text (etaf-vdom-text "text"))
        (element (etaf-create-vnode 'div nil nil))
        (fragment (etaf-vdom-fragment nil))
        (comment (etaf-vdom-comment "comment")))
    
    (should (etaf-vdom-text-p text))
    (should (etaf-vdom-element-p element))
    (should (etaf-vdom-fragment-p fragment))
    (should (etaf-vdom-comment-p comment))
    
    (should-not (etaf-vdom-text-p element))
    (should-not (etaf-vdom-element-p text))))

;;; ============================================================================
;;; Test Suite 7: Patch Flags (Optimization)
;;; ============================================================================

(ert-deftest etaf-vdom-renderer-test-patch-flags ()
  "Test patch flag optimization."
  (let ((vnode (etaf-create-vnode 'div nil nil)))
    
    ;; Add patch flags
    (etaf-vdom-add-patch-flag vnode etaf-patch-flag-text)
    (should (etaf-vdom-has-patch-flag-p vnode etaf-patch-flag-text))
    
    (etaf-vdom-add-patch-flag vnode etaf-patch-flag-class)
    (should (etaf-vdom-has-patch-flag-p vnode etaf-patch-flag-class))
    (should (etaf-vdom-has-patch-flag-p vnode etaf-patch-flag-text))
    
    ;; Static node check
    (let ((static-node (etaf-vdom-static "static")))
      (should (etaf-vdom-is-static-p static-node)))))

(provide 'etaf-vdom-renderer-tests)
;;; etaf-vdom-renderer-tests.el ends here
