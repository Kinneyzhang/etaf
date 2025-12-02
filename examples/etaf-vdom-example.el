;;; etaf-vdom-example.el --- Virtual DOM usage examples -*- lexical-binding: t; -*-

;;; Commentary:
;; Examples demonstrating the Vue 3-inspired Virtual DOM system in ETAF.
;; This showcases diff/patch algorithm, lifecycle hooks, and keyed reconciliation.

;;; Code:

(require 'etaf-vdom)
(require 'etaf-etml)

;;; ============================================================================
;;; Example 1: Basic VNode Creation
;;; ============================================================================

(message "=== Example 1: Basic VNode Creation ===\n")

;; Create an element VNode
(setq my-div (etaf-vdom-element 'div 
               :props '(:class "container" :id "main")))
(message "Created element VNode: %s" (etaf-vdom-get-tag my-div))
(message "VNode ID: %d" (etaf-vdom-get-id my-div))
(message "VNode props: %S" (etaf-vdom-get-props my-div))

;; Create a text VNode
(setq my-text (etaf-vdom-text "Hello World"))
(message "\nCreated text VNode: %s" (etaf-vdom-get-content my-text))

;; Create a fragment with multiple children
(setq my-fragment (etaf-vdom-fragment
                   (etaf-vdom-element 'div)
                   (etaf-vdom-element 'span)
                   (etaf-vdom-element 'p)))
(message "\nCreated fragment with %d children"
         (length (etaf-vdom-get-children my-fragment)))

;;; ============================================================================
;;; Example 2: Lifecycle Hooks
;;; ============================================================================

(message "\n=== Example 2: Lifecycle Hooks ===\n")

(setq component-vnode (etaf-vdom-element 'div :props '(:class "component")))

;; Add lifecycle hooks
(etaf-vdom-add-hook component-vnode :mounted
  (lambda (node)
    (message "✓ Component mounted! ID: %d" (etaf-vdom-get-id node))))

(etaf-vdom-add-hook component-vnode :updated
  (lambda (new-node old-node)
    (message "✓ Component updated! Old ID: %d, New ID: %d"
             (etaf-vdom-get-id old-node)
             (etaf-vdom-get-id new-node))))

(etaf-vdom-add-hook component-vnode :unmounted
  (lambda (node)
    (message "✓ Component unmounted! ID: %d" (etaf-vdom-get-id node))))

;; Mount the component
(message "Mounting component...")
(etaf-vdom-mount component-vnode)
(message "Component mounted: %s" (etaf-vdom-get-mounted-p component-vnode))

;; Unmount the component
(message "\nUnmounting component...")
(etaf-vdom-unmount component-vnode)
(message "Component mounted: %s" (etaf-vdom-get-mounted-p component-vnode))

;;; ============================================================================
;;; Example 3: Diff Algorithm - Property Changes
;;; ============================================================================

(message "\n=== Example 3: Diff Algorithm - Property Changes ===\n")

(setq old-vnode (etaf-vdom-element 'div 
                  :props '(:class "old-class" :id "my-div")))
(setq new-vnode (etaf-vdom-element 'div 
                  :props '(:class "new-class" :id "my-div" :style "color: red")))

(message "Old props: %S" (etaf-vdom-get-props old-vnode))
(message "New props: %S" (etaf-vdom-get-props new-vnode))

(setq patches (etaf-vdom-diff old-vnode new-vnode))
(message "\nGenerated %d patch(es):" (length patches))
(dolist (patch patches)
  (message "  - Patch type: %s" (etaf-vdom-patch-type patch))
  (when (eq (etaf-vdom-patch-type patch) 'update)
    (message "    Changed props: %S" (etaf-vdom-patch-props patch))))

;;; ============================================================================
;;; Example 4: Diff Algorithm - Node Replacement
;;; ============================================================================

(message "\n=== Example 4: Diff Algorithm - Node Replacement ===\n")

(setq old-div (etaf-vdom-element 'div :props '(:class "content")))
(setq new-span (etaf-vdom-element 'span :props '(:class "content")))

(message "Old node type: %s" (etaf-vdom-get-tag old-div))
(message "New node type: %s" (etaf-vdom-get-tag new-span))

(setq patches (etaf-vdom-diff old-div new-span))
(message "\nGenerated %d patch(es):" (length patches))
(dolist (patch patches)
  (message "  - Patch type: %s (different node types require replacement)"
           (etaf-vdom-patch-type patch)))

;;; ============================================================================
;;; Example 5: Keyed List Reconciliation
;;; ============================================================================

(message "\n=== Example 5: Keyed List Reconciliation ===\n")

;; Original list: [A:1, B:2, C:3]
(setq old-list-vnode (etaf-vdom-element 'ul))
(setq old-items (list
                 (etaf-vdom-element 'li :props '(:key "1" :class "item") 
                                   :children (list (etaf-vdom-text "Item A")))
                 (etaf-vdom-element 'li :props '(:key "2" :class "item")
                                   :children (list (etaf-vdom-text "Item B")))
                 (etaf-vdom-element 'li :props '(:key "3" :class "item")
                                   :children (list (etaf-vdom-text "Item C")))))
(etaf-vdom-set-children old-list-vnode old-items)

;; New list: [C:3, A:1, D:4] - reorder, remove B, add D
(setq new-list-vnode (etaf-vdom-element 'ul))
(setq new-items (list
                 (etaf-vdom-element 'li :props '(:key "3" :class "item")
                                   :children (list (etaf-vdom-text "Item C")))
                 (etaf-vdom-element 'li :props '(:key "1" :class "item")
                                   :children (list (etaf-vdom-text "Item A")))
                 (etaf-vdom-element 'li :props '(:key "4" :class "item")
                                   :children (list (etaf-vdom-text "Item D")))))
(etaf-vdom-set-children new-list-vnode new-items)

(message "Old list keys: [%s]"
         (mapconcat (lambda (item) (etaf-vdom-get-key item))
                   old-items ", "))
(message "New list keys: [%s]"
         (mapconcat (lambda (item) (etaf-vdom-get-key item))
                   new-items ", "))

(setq patches (etaf-vdom-diff old-list-vnode new-list-vnode))
(message "\nGenerated %d patch(es) for keyed list update:" (length patches))
(dolist (patch patches)
  (let ((type (etaf-vdom-patch-type patch)))
    (pcase type
      ('reorder
       (message "  - REORDER: Move node with key '%s' to index %d"
                (etaf-vdom-get-key (etaf-vdom-patch-new-vnode patch))
                (etaf-vdom-patch-index patch)))
      ('create
       (message "  - CREATE: Add node with key '%s'"
                (etaf-vdom-get-key (etaf-vdom-patch-new-vnode patch))))
      ('remove
       (message "  - REMOVE: Delete node with key '%s'"
                (etaf-vdom-get-key (etaf-vdom-patch-old-vnode patch))))
      (_
       (message "  - %s" type)))))

(message "\nKeyed reconciliation allows efficient updates:")
(message "  • Items with matching keys are reused (C and A)")
(message "  • Removed items are detected (B)")
(message "  • New items are added efficiently (D)")

;;; ============================================================================
;;; Example 6: Integration with ETAF - ETML to VNode
;;; ============================================================================

(message "\n=== Example 6: Integration with ETAF - ETML to VNode ===\n")

(setq template '(div :class "container"
                  (h1 :id "title" "Welcome")
                  (ul :class "list"
                    (li :key "1" "First item")
                    (li :key "2" "Second item")
                    (li :key "3" "Third item"))
                  (button :class "btn" "Click me")))

(message "Converting ETML template to VNode tree...")
(setq result (etaf-etml-to-dom-with-vdom template))
(setq vtree (etaf-vdom-result-get-vtree result))
(setq dom (etaf-vdom-result-get-dom result))

(message "✓ VNode tree created!")
(message "  - Root VNode type: %s" (etaf-vdom-get-type vtree))
(message "  - Root tag: %s" (etaf-vdom-get-tag vtree))
(message "  - Number of children: %d" (length (etaf-vdom-get-children vtree)))

(message "\n✓ Clean DOM generated!")
(message "  - DOM tag: %s" (car dom))
(message "  - DOM has clean structure (no tag-instances in attributes)")

;; Find specific nodes in VTree
(setq button-vnode (etaf-vdom-find vtree
                     (lambda (node)
                       (and (etaf-vdom-element-p node)
                            (eq (etaf-vdom-get-tag node) 'button)))))

(when button-vnode
  (message "\n✓ Found button VNode in tree!")
  (message "  - Has tag-instance: %s" 
           (not (null (etaf-vdom-get-tag-instance button-vnode)))))

;;; ============================================================================
;;; Example 7: Hierarchical Lifecycle Management
;;; ============================================================================

(message "\n=== Example 7: Hierarchical Lifecycle Management ===\n")

(setq parent (etaf-vdom-element 'div :props '(:class "parent")))
(setq child1 (etaf-vdom-element 'span :props '(:class "child1")))
(setq child2 (etaf-vdom-element 'p :props '(:class "child2")))
(etaf-vdom-set-children parent (list child1 child2))

(setq mount-order nil)
(setq unmount-order nil)

(etaf-vdom-add-hook parent :mounted
  (lambda (n) (push 'parent mount-order)))
(etaf-vdom-add-hook child1 :mounted
  (lambda (n) (push 'child1 mount-order)))
(etaf-vdom-add-hook child2 :mounted
  (lambda (n) (push 'child2 mount-order)))

(etaf-vdom-add-hook parent :unmounted
  (lambda (n) (push 'parent unmount-order)))
(etaf-vdom-add-hook child1 :unmounted
  (lambda (n) (push 'child1 unmount-order)))
(etaf-vdom-add-hook child2 :unmounted
  (lambda (n) (push 'child2 unmount-order)))

(message "Mounting parent with children...")
(etaf-vdom-mount parent)
(message "Mount order: %S" (nreverse mount-order))
(message "All nodes mounted: parent=%s, child1=%s, child2=%s"
         (etaf-vdom-get-mounted-p parent)
         (etaf-vdom-get-mounted-p child1)
         (etaf-vdom-get-mounted-p child2))

(message "\nUnmounting parent with children...")
(etaf-vdom-unmount parent)
(message "Unmount order: %S" (nreverse unmount-order))
(message "Note: Children are unmounted before parent (depth-first)")

;;; ============================================================================
;;; Example 8: Performance - Minimal Updates
;;; ============================================================================

(message "\n=== Example 8: Performance - Minimal Updates ===\n")

;; Large list scenario
(defun create-list-vnode (keys)
  "Create a list VNode with items for each key in KEYS."
  (let ((ul (etaf-vdom-element 'ul :props '(:class "list"))))
    (etaf-vdom-set-children ul
      (mapcar (lambda (key)
                (etaf-vdom-element 'li 
                  :props (list :key key :class "item")
                  :children (list (etaf-vdom-text (format "Item %s" key)))))
              keys))
    ul))

(setq old-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
(setq new-keys '("1" "3" "4" "5" "6" "7" "8" "9" "10" "11")) ; Remove 2, add 11

(setq old-list (create-list-vnode old-keys))
(setq new-list (create-list-vnode new-keys))

(message "Old list: %d items" (length old-keys))
(message "New list: %d items" (length new-keys))
(message "Change: Remove item '2', add item '11'")

(setq patches (etaf-vdom-diff old-list new-list))
(message "\nGenerated %d patch(es) - minimal updates!" (length patches))
(message "Without keys: Would update all 10 items")
(message "With keys: Only 2 operations (1 remove + 1 create)")

;;; ============================================================================
;;; Summary
;;; ============================================================================

(message "\n=== Virtual DOM Summary ===\n")
(message "The Vue 3-inspired Virtual DOM in ETAF provides:")
(message "  1. Efficient diff/patch algorithm for minimal updates")
(message "  2. Lifecycle hooks (mounted, updated, unmounted)")
(message "  3. Keyed reconciliation for optimized list updates")
(message "  4. Multiple VNode types (element, text, comment, fragment)")
(message "  5. Clean separation between VNode tree and DOM tree")
(message "  6. Integration with ETAF's rendering pipeline")
(message "\nSee docs/VIRTUAL-DOM.md for complete documentation.")

(provide 'etaf-vdom-example)
;;; etaf-vdom-example.el ends here
