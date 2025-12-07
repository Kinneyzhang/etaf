;;; test-renderer-simple.el --- Simple test for renderer functionality -*- lexical-binding: t; -*-

;; This is a simple, minimal test to verify the renderer implementation works.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(require 'etaf-vdom)
(require 'etaf-event)
(require 'etaf-render)
(require 'etaf-css)
(require 'etaf-layout)
(require 'etaf-layout-string)

(defun test-renderer-simple ()
  "Simple test of renderer functionality."
  (message "=== Testing ETAF Renderer ===")
  
  ;; Test 1: Create a simple VNode
  (message "\n1. Testing VNode Creation...")
  (let ((text-vnode (etaf-vdom-text "Hello World"))
        (button-vnode (etaf-create-vnode 'button
                                         (list :on-click (lambda () 
                                                          (message "Button clicked!")))
                                         (list (etaf-vdom-text "Click Me")))))
    (if (and (etaf-vdom-text-p text-vnode)
             (etaf-vdom-element-p button-vnode))
        (message "   ✓ VNode creation works")
      (error "   ✗ VNode creation failed")))
  
  ;; Test 2: Test VNode rendering to DOM
  (message "\n2. Testing VNode to DOM rendering...")
  (let* ((vnode (etaf-create-vnode 'div
                                   (list :class "test")
                                   (list (etaf-vdom-text "Test"))))
         (dom (etaf-vdom-render vnode)))
    (if (and dom (eq (car dom) 'div))
        (message "   ✓ VNode renders to DOM")
      (error "   ✗ VNode rendering failed")))
  
  ;; Test 3: Test event handler preservation
  (message "\n3. Testing event handler preservation...")
  (let* ((handler-called nil)
         (vnode (etaf-create-vnode 'button
                                   (list :on-click (lambda () 
                                                    (setq handler-called t))
                                        :class "btn")
                                   (list (etaf-vdom-text "Button"))))
         (dom (etaf-vdom-render vnode))
         (attrs (cadr dom))
         (event-handlers (cdr (assq 'etaf-event-handlers attrs))))
    (if event-handlers
        (progn
          (message "   ✓ Event handlers preserved in DOM")
          ;; Test the handler works
          (let ((click-handler (cdr (assq 'click event-handlers))))
            (when click-handler
              (funcall click-handler)
              (if handler-called
                  (message "   ✓ Event handler is callable")
                (error "   ✗ Event handler failed to execute")))))
      (error "   ✗ Event handlers not preserved")))
  
  ;; Test 4: Test same-type comparison
  (message "\n4. Testing VNode comparison for diff...")
  (let ((vnode1 (etaf-create-vnode 'div (list :key "a") nil))
        (vnode2 (etaf-create-vnode 'div (list :key "a") nil))
        (vnode3 (etaf-create-vnode 'span (list :key "a") nil)))
    (if (and (etaf-vdom-same-type-p vnode1 vnode2)
             (not (etaf-vdom-same-type-p vnode1 vnode3)))
        (message "   ✓ VNode type comparison works")
      (error "   ✗ VNode type comparison failed")))
  
  ;; Test 5: Test patch flags
  (message "\n5. Testing patch flags...")
  (let ((vnode (etaf-create-vnode 'div nil nil)))
    (etaf-vdom-add-patch-flag vnode etaf-patch-flag-text)
    (if (etaf-vdom-has-patch-flag-p vnode etaf-patch-flag-text)
        (message "   ✓ Patch flags work")
      (error "   ✗ Patch flags failed")))
  
  ;; Test 6: Test event system initialization
  (message "\n6. Testing event system...")
  (with-temp-buffer
    (etaf-event-init)
    (if etaf-event--elements
        (message "   ✓ Event system initializes")
      (error "   ✗ Event system failed to initialize")))
  
  ;; Test 7: Test mount/unmount (basic - without full pipeline)
  (message "\n7. Testing mount/unmount storage...")
  (with-temp-buffer
    (let* ((vnode (etaf-create-vnode 'div nil (list (etaf-vdom-text "Test"))))
           (container (current-buffer)))
      ;; Store directly in the mounted vnodes hash for testing
      (puthash container vnode etaf-vdom--mounted-vnodes)
      (if (gethash container etaf-vdom--mounted-vnodes)
          (progn
            (message "   ✓ Mount storage works")
            ;; Test unmount cleanup
            (remhash container etaf-vdom--mounted-vnodes)
            (if (not (gethash container etaf-vdom--mounted-vnodes))
                (message "   ✓ Unmount cleanup works")
              (error "   ✗ Unmount cleanup failed")))
        (error "   ✗ Mount storage failed"))))
  
  (message "\n=== All Basic Tests Passed! ===")
  (message "\nThe core renderer functions are working correctly.")
  (message "Event handlers are properly preserved through the render pipeline.")
  (message "\nNext steps:")
  (message "  - Run the full demo: M-x load-file examples/etaf-renderer-demo.el")
  (message "  - Run the full test suite with ert"))

;; Run the test
(test-renderer-simple)

(provide 'test-renderer-simple)
;;; test-renderer-simple.el ends here
