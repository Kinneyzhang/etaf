;;; etaf-renderer-demo.el --- Comprehensive renderer demo with interactive examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: demo, renderer, vdom, events
;; Version: 1.0.0

;;; Commentary:

;; This file demonstrates the complete Vue 3-style renderer implementation in ETAF:
;; 1. VNode creation and mounting
;; 2. Interactive buttons with event handlers
;; 3. Dynamic updates using patch/diff algorithm
;; 4. Event bubbling
;; 5. Unmounting and cleanup
;;
;; Usage:
;;   (load-file "examples/etaf-renderer-demo.el")
;;   M-x etaf-renderer-demo

;;; Code:

(require 'cl-lib)
(require 'etaf-vdom)
(require 'etaf-event)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-layout-string)

;;; ============================================================================
;;; Demo 1: Basic Button with Click Handler
;;; ============================================================================

(defun etaf-renderer-demo-1-basic-button ()
  "Demo 1: Mount a button and handle clicks."
  (interactive)
  (let* ((buffer-name "*ETAF Renderer Demo 1*")
         (buffer (get-buffer-create buffer-name))
         (click-count 0)
         ;; Create a button VNode with click handler
         (button-vnode (etaf-create-vnode 'button
                                          (list :on-click (lambda ()
                                                            (setq click-count (1+ click-count))
                                                            (message "Button clicked! Count: %d" click-count))
                                                :class "demo-button"
                                                :uuid "demo-btn-1")
                                          (list (etaf-vdom-text "Click Me!")))))
    
    ;; Switch to buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    
    ;; Mount the button
    (etaf-vdom-mount button-vnode buffer)
    
    (message "Demo 1: Button mounted. Click it to test event handling!")
    (message "Click count will be shown in the minibuffer.")))

;;; ============================================================================
;;; Demo 2: Dynamic Counter with Updates
;;; ============================================================================

(defvar etaf-renderer-demo-2-count 0
  "Counter for demo 2.")

(defvar etaf-renderer-demo-2-vnode nil
  "Current VNode for demo 2.")

(defun etaf-renderer-demo-2-create-vnode (count)
  "Create a counter VNode displaying COUNT."
  (etaf-create-vnode 'div
                     (list :class "counter-container")
                     (list
                      (etaf-create-vnode 'h2
                                         nil
                                         (list (etaf-vdom-text "Dynamic Counter Demo")))
                      (etaf-create-vnode 'p
                                         (list :class "count-display")
                                         (list (etaf-vdom-text (format "Count: %d" count))))
                      (etaf-create-vnode 'div
                                         (list :class "button-group")
                                         (list
                                          (etaf-create-vnode 'button
                                                             (list :on-click #'etaf-renderer-demo-2-increment
                                                                   :class "btn-increment"
                                                                   :uuid "demo2-inc")
                                                             (list (etaf-vdom-text "Increment")))
                                          (etaf-vdom-text " ")
                                          (etaf-create-vnode 'button
                                                             (list :on-click #'etaf-renderer-demo-2-decrement
                                                                   :class "btn-decrement"
                                                                   :uuid "demo2-dec")
                                                             (list (etaf-vdom-text "Decrement")))
                                          (etaf-vdom-text " ")
                                          (etaf-create-vnode 'button
                                                             (list :on-click #'etaf-renderer-demo-2-reset
                                                                   :class "btn-reset"
                                                                   :uuid "demo2-reset")
                                                             (list (etaf-vdom-text "Reset"))))))))

(defun etaf-renderer-demo-2-increment ()
  "Increment counter and update display."
  (interactive)
  (setq etaf-renderer-demo-2-count (1+ etaf-renderer-demo-2-count))
  (etaf-renderer-demo-2-update))

(defun etaf-renderer-demo-2-decrement ()
  "Decrement counter and update display."
  (interactive)
  (setq etaf-renderer-demo-2-count (1- etaf-renderer-demo-2-count))
  (etaf-renderer-demo-2-update))

(defun etaf-renderer-demo-2-reset ()
  "Reset counter and update display."
  (interactive)
  (setq etaf-renderer-demo-2-count 0)
  (etaf-renderer-demo-2-update))

(defun etaf-renderer-demo-2-update ()
  "Update the counter display using patch algorithm."
  (let ((buffer-name "*ETAF Renderer Demo 2*")
        (new-vnode (etaf-renderer-demo-2-create-vnode etaf-renderer-demo-2-count)))
    
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        ;; Patch the old VNode with the new one
        (if etaf-renderer-demo-2-vnode
            (progn
              (etaf-vdom-patch etaf-renderer-demo-2-vnode new-vnode (current-buffer))
              (message "Counter updated to: %d (using patch/diff)" etaf-renderer-demo-2-count))
          ;; First time - just mount
          (etaf-vdom-mount new-vnode (current-buffer)))
        
        (setq etaf-renderer-demo-2-vnode new-vnode)))))

(defun etaf-renderer-demo-2-counter ()
  "Demo 2: Interactive counter with dynamic updates."
  (interactive)
  (let* ((buffer-name "*ETAF Renderer Demo 2*")
         (buffer (get-buffer-create buffer-name)))
    
    ;; Reset state
    (setq etaf-renderer-demo-2-count 0)
    (setq etaf-renderer-demo-2-vnode nil)
    
    ;; Switch to buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    
    ;; Create initial vnode
    (let ((initial-vnode (etaf-renderer-demo-2-create-vnode 0)))
      (setq etaf-renderer-demo-2-vnode initial-vnode)
      (etaf-vdom-mount initial-vnode buffer))
    
    (message "Demo 2: Counter demo loaded. Click buttons to test updates!")
    (message "Uses patch/diff algorithm for efficient updates.")))

;;; ============================================================================
;;; Demo 3: Event Bubbling
;;; ============================================================================

(defun etaf-renderer-demo-3-event-bubbling ()
  "Demo 3: Demonstrate event bubbling."
  (interactive)
  (let* ((buffer-name "*ETAF Renderer Demo 3*")
         (buffer (get-buffer-create buffer-name))
         (child-clicked nil)
         (parent-clicked nil)
         ;; Create nested structure
         (child-button (etaf-create-vnode 'button
                                          (list :on-click (lambda ()
                                                            (setq child-clicked t)
                                                            (message "Child button clicked!"))
                                                :class "child-button"
                                                :uuid "demo3-child")
                                          (list (etaf-vdom-text "Child Button"))))
         (parent-div (etaf-create-vnode 'div
                                        (list :on-click (lambda ()
                                                          (setq parent-clicked t)
                                                          (message "Parent div clicked! (bubbled from child)"))
                                              :class "parent-container"
                                              :uuid "demo3-parent")
                                        (list
                                         (etaf-create-vnode 'p
                                                            nil
                                                            (list (etaf-vdom-text "Event Bubbling Demo")))
                                         (etaf-create-vnode 'p
                                                            nil
                                                            (list (etaf-vdom-text "Click the button - both handlers will fire:")))
                                         child-button))))
    
    ;; Set up parent reference for bubbling
    (plist-put child-button :parent parent-div)
    
    ;; Switch to buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    
    ;; Mount the parent (which includes child)
    (etaf-vdom-mount parent-div buffer)
    
    ;; Set up bubbling handlers
    (etaf-event-add-listener "demo3-child" 'click
                             (lambda (uuid data)
                               (message "1. Child click handler called")
                               (setq child-clicked t)))
    (etaf-event-add-listener "demo3-parent" 'click
                             (lambda (uuid data)
                               (message "2. Parent click handler called (bubbled)")
                               (setq parent-clicked t)))
    
    (message "Demo 3: Event bubbling demo loaded.")
    (message "Click the button to see both child and parent handlers fire.")))

;;; ============================================================================
;;; Demo 4: Mount, Update, Unmount Lifecycle
;;; ============================================================================

(defvar etaf-renderer-demo-4-mounted nil
  "Track if demo 4 is mounted.")

(defun etaf-renderer-demo-4-lifecycle ()
  "Demo 4: Full lifecycle - mount, update, unmount."
  (interactive)
  (let* ((buffer-name "*ETAF Renderer Demo 4*")
         (buffer (get-buffer-create buffer-name)))
    
    (switch-to-buffer buffer)
    (erase-buffer)
    
    (if etaf-renderer-demo-4-mounted
        ;; Unmount
        (progn
          (etaf-vdom-unmount buffer)
          (setq etaf-renderer-demo-4-mounted nil)
          (message "Demo 4: Content UNMOUNTED and cleaned up."))
      
      ;; Mount
      (let ((vnode (etaf-create-vnode 'div
                                      (list :class "lifecycle-demo")
                                      (list
                                       (etaf-create-vnode 'h2
                                                          nil
                                                          (list (etaf-vdom-text "Lifecycle Demo")))
                                       (etaf-create-vnode 'p
                                                          nil
                                                          (list (etaf-vdom-text "This content is MOUNTED.")))
                                       (etaf-create-vnode 'p
                                                          nil
                                                          (list (etaf-vdom-text "Run command again to UNMOUNT.")))
                                       (etaf-create-vnode 'button
                                                          (list :on-click (lambda ()
                                                                            (message "Button still works while mounted!"))
                                                                :uuid "demo4-btn")
                                                          (list (etaf-vdom-text "Test Button")))))))
        (etaf-vdom-mount vnode buffer)
        (setq etaf-renderer-demo-4-mounted t)
        (message "Demo 4: Content MOUNTED. Run command again to unmount.")))))

;;; ============================================================================
;;; Demo 5: Complex Interactive Application
;;; ============================================================================

(defvar etaf-renderer-demo-5-tasks '()
  "Task list for demo 5.")

(defvar etaf-renderer-demo-5-vnode nil
  "Current VNode for demo 5.")

(defvar etaf-renderer-demo-5-next-id 1
  "Next task ID.")

(defun etaf-renderer-demo-5-create-task-vnode (task)
  "Create a VNode for TASK."
  (let ((task-id (plist-get task :id))
        (task-text (plist-get task :text))
        (task-done (plist-get task :done)))
    (etaf-create-vnode 'li
                       (list :class (if task-done "task-done" "task-pending"))
                       (list
                        (etaf-create-vnode 'span
                                           nil
                                           (list (etaf-vdom-text
                                                  (format "[%s] %s"
                                                          (if task-done "✓" " ")
                                                          task-text))))
                        (etaf-vdom-text " ")
                        (etaf-create-vnode 'button
                                           (list :on-click
                                                 (lambda ()
                                                   (etaf-renderer-demo-5-toggle-task task-id))
                                                 :uuid (format "demo5-toggle-%d" task-id))
                                           (list (etaf-vdom-text "Toggle")))
                        (etaf-vdom-text " ")
                        (etaf-create-vnode 'button
                                           (list :on-click
                                                 (lambda ()
                                                   (etaf-renderer-demo-5-delete-task task-id))
                                                 :uuid (format "demo5-delete-%d" task-id))
                                           (list (etaf-vdom-text "Delete")))))))

(defun etaf-renderer-demo-5-create-app-vnode ()
  "Create the main app VNode for demo 5."
  (etaf-create-vnode 'div
                     (list :class "todo-app")
                     (list
                      (etaf-create-vnode 'h1
                                         nil
                                         (list (etaf-vdom-text "Todo List Demo")))
                      (etaf-create-vnode 'p
                                         nil
                                         (list (etaf-vdom-text
                                                (format "Tasks: %d total, %d completed"
                                                        (length etaf-renderer-demo-5-tasks)
                                                        (cl-count-if (lambda (task) (plist-get task :done))
                                                                     etaf-renderer-demo-5-tasks)))))
                      (etaf-create-vnode 'ul
                                         (list :class "task-list")
                                         (mapcar #'etaf-renderer-demo-5-create-task-vnode
                                                 etaf-renderer-demo-5-tasks))
                      (etaf-create-vnode 'div
                                         (list :class "controls")
                                         (list
                                          (etaf-create-vnode 'button
                                                             (list :on-click #'etaf-renderer-demo-5-add-task
                                                                   :uuid "demo5-add")
                                                             (list (etaf-vdom-text "Add Task")))
                                          (etaf-vdom-text " ")
                                          (etaf-create-vnode 'button
                                                             (list :on-click #'etaf-renderer-demo-5-clear-completed
                                                                   :uuid "demo5-clear")
                                                             (list (etaf-vdom-text "Clear Completed"))))))))

(defun etaf-renderer-demo-5-add-task ()
  "Add a new task."
  (interactive)
  (let ((task-text (read-string "Task: " (format "Task %d" etaf-renderer-demo-5-next-id))))
    (push (list :id etaf-renderer-demo-5-next-id
                :text task-text
                :done nil)
          etaf-renderer-demo-5-tasks)
    (setq etaf-renderer-demo-5-next-id (1+ etaf-renderer-demo-5-next-id))
    (etaf-renderer-demo-5-update)))

(defun etaf-renderer-demo-5-toggle-task (task-id)
  "Toggle completion status of task with TASK-ID."
  (setq etaf-renderer-demo-5-tasks
        (mapcar (lambda (task)
                  (if (= (plist-get task :id) task-id)
                      (plist-put (copy-sequence task) :done
                                 (not (plist-get task :done)))
                    task))
                etaf-renderer-demo-5-tasks))
  (etaf-renderer-demo-5-update))

(defun etaf-renderer-demo-5-delete-task (task-id)
  "Delete task with TASK-ID."
  (setq etaf-renderer-demo-5-tasks
        (cl-remove-if (lambda (task)
                        (= (plist-get task :id) task-id))
                      etaf-renderer-demo-5-tasks))
  (etaf-renderer-demo-5-update))

(defun etaf-renderer-demo-5-clear-completed ()
  "Clear all completed tasks."
  (interactive)
  (setq etaf-renderer-demo-5-tasks
        (cl-remove-if (lambda (task) (plist-get task :done))
                      etaf-renderer-demo-5-tasks))
  (etaf-renderer-demo-5-update))

(defun etaf-renderer-demo-5-update ()
  "Update the demo 5 display."
  (let ((buffer-name "*ETAF Renderer Demo 5*")
        (new-vnode (etaf-renderer-demo-5-create-app-vnode)))
    
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (if etaf-renderer-demo-5-vnode
            (etaf-vdom-patch etaf-renderer-demo-5-vnode new-vnode (current-buffer))
          (etaf-vdom-mount new-vnode (current-buffer)))
        
        (setq etaf-renderer-demo-5-vnode new-vnode)
        (message "Todo list updated!")))))

(defun etaf-renderer-demo-5-todo-app ()
  "Demo 5: Full todo application with CRUD operations."
  (interactive)
  (let* ((buffer-name "*ETAF Renderer Demo 5*")
         (buffer (get-buffer-create buffer-name)))
    
    ;; Reset state
    (setq etaf-renderer-demo-5-tasks
          (list (list :id 1 :text "Learn ETAF renderer" :done nil)
                (list :id 2 :text "Test event handlers" :done nil)
                (list :id 3 :text "Try diff algorithm" :done nil)))
    (setq etaf-renderer-demo-5-next-id 4)
    (setq etaf-renderer-demo-5-vnode nil)
    
    ;; Switch to buffer
    (switch-to-buffer buffer)
    (erase-buffer)
    
    ;; Mount initial app
    (let ((initial-vnode (etaf-renderer-demo-5-create-app-vnode)))
      (setq etaf-renderer-demo-5-vnode initial-vnode)
      (etaf-vdom-mount initial-vnode buffer))
    
    (message "Demo 5: Todo app loaded!")
    (message "Features: Add, toggle, delete tasks - all using patch/diff updates.")))

;;; ============================================================================
;;; Main Demo Menu
;;; ============================================================================

(defun etaf-renderer-demo ()
  "Main menu for ETAF renderer demos.
Select a demo to run:

1. Basic Button - Simple button with click handler
2. Dynamic Counter - Counter with increment/decrement using patch
3. Event Bubbling - Demonstrate event propagation
4. Lifecycle - Mount/unmount demonstration
5. Todo App - Full CRUD application

Choose a demo number:"
  (interactive)
  (let ((choice (read-string "Choose demo (1-5): ")))
    (pcase choice
      ("1" (etaf-renderer-demo-1-basic-button))
      ("2" (etaf-renderer-demo-2-counter))
      ("3" (etaf-renderer-demo-3-event-bubbling))
      ("4" (etaf-renderer-demo-4-lifecycle))
      ("5" (etaf-renderer-demo-5-todo-app))
      (_ (message "Invalid choice. Please run M-x etaf-renderer-demo and choose 1-5.")))))

(provide 'etaf-renderer-demo)
;;; etaf-renderer-demo.el ends here
