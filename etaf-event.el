;;; etaf-event.el --- Event Model for ETAF Framework -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: events, pseudo-classes, interactive
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; ETAF Event Model - Event system for interactive pseudo-class support
;;
;; This module implements a browser-like event model for ETAF, enabling support
;; for interactive pseudo-classes like :hover, :focus, :active. It combines
;; Emacs' native capabilities (text properties, overlays, keymaps) with a
;; centralized state management system.
;;
;; Key Features:
;; - Element state tracking (hover, focus, active, disabled, etc.)
;; - Event listener registration and dispatch
;; - Integration with CSS pseudo-class selectors
;; - Efficient state change propagation
;;
;; Browser Pseudo-Class Reference:
;; - :hover - Element is being hovered by the mouse
;; - :active - Element is being activated (e.g., mouse button pressed)
;; - :focus - Element has focus (for form inputs, buttons, links)
;; - :focus-within - Element or any descendant has focus
;; - :disabled - Element is disabled
;; - :enabled - Element is enabled
;; - :checked - Form element is checked
;; - :target - Element is the target of the URL fragment
;;
;; Emacs Implementation Mapping:
;; - :hover - Track via mouse position and text properties
;; - :active - Track via mouse-down/mouse-up events
;; - :focus - Track cursor position for interactive elements
;; - :disabled - Track via element attributes
;;
;; Usage Example:
;;
;;   ;; Initialize event system for a buffer
;;   (etaf-event-init)
;;
;;   ;; Register an element for state tracking
;;   (etaf-event-register-element uuid node start end)
;;
;;   ;; Get element state
;;   (etaf-event-get-state uuid)
;;   ;; => (:hover t :active nil :focus nil)
;;
;;   ;; Add state change listener
;;   (etaf-event-add-listener uuid 'state-change
;;     (lambda (element old-state new-state)
;;       (message "State changed from %S to %S" old-state new-state)))

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; Variables and Data Structures
;;; ============================================================

(defvar-local etaf-event--elements nil
  "Hash table of registered elements in current buffer.
Keys are element UUIDs (strings), values are element records (plists).")

(defvar-local etaf-event--hover-element nil
  "UUID of currently hovered element, or nil.")

(defvar-local etaf-event--active-element nil
  "UUID of currently active element (mouse pressed), or nil.")

(defvar-local etaf-event--focus-element nil
  "UUID of currently focused element, or nil.")

(defvar-local etaf-event--listeners nil
  "Hash table of event listeners.
Keys are (uuid . event-type), values are lists of listener functions.")

(defvar-local etaf-event--tracking-timer nil
  "Timer for tracking mouse position and updating hover state.")

(defcustom etaf-event-hover-delay 0.1
  "Delay in seconds before updating hover state.
Lower values are more responsive but may impact performance."
  :type 'number
  :group 'etaf)

;;; ============================================================
;;; Element State Record
;;; ============================================================

(cl-defstruct etaf-event-element
  "Record for a registered interactive element."
  uuid          ; Unique identifier
  node          ; DOM node
  start         ; Buffer start position
  end           ; Buffer end position
  state         ; Current state plist (:hover :active :focus :disabled, etc.)
  listeners)    ; Plist of event listeners by type

;;; ============================================================
;;; Initialization
;;; ============================================================

(defun etaf-event-init (&optional buffer)
  "Initialize the event system for BUFFER (or current buffer).
Sets up data structures and starts event tracking."
  (with-current-buffer (or buffer (current-buffer))
    ;; Initialize hash tables
    (unless etaf-event--elements
      (setq-local etaf-event--elements
                  (make-hash-table :test 'equal)))
    (unless etaf-event--listeners
      (setq-local etaf-event--listeners
                  (make-hash-table :test 'equal)))
    
    ;; Reset state
    (setq-local etaf-event--hover-element nil)
    (setq-local etaf-event--active-element nil)
    (setq-local etaf-event--focus-element nil)
    
    ;; Start tracking (if not already started)
    (unless etaf-event--tracking-timer
      (etaf-event-start-tracking))))

(defun etaf-event-cleanup (&optional buffer)
  "Clean up the event system for BUFFER (or current buffer).
Stops tracking and clears all data structures."
  (with-current-buffer (or buffer (current-buffer))
    ;; Stop tracking timer
    (when etaf-event--tracking-timer
      (cancel-timer etaf-event--tracking-timer)
      (setq-local etaf-event--tracking-timer nil))
    
    ;; Clear data structures
    (when etaf-event--elements
      (clrhash etaf-event--elements))
    (when etaf-event--listeners
      (clrhash etaf-event--listeners))
    
    (setq-local etaf-event--hover-element nil)
    (setq-local etaf-event--active-element nil)
    (setq-local etaf-event--focus-element nil)))

;;; ============================================================
;;; Element Registration
;;; ============================================================

(defun etaf-event-register-element (uuid node start end &optional initial-state)
  "Register an element for event tracking.
UUID is the unique identifier, NODE is the DOM node,
START and END are buffer positions, INITIAL-STATE is optional state plist."
  (unless etaf-event--elements
    (etaf-event-init))
  
  (let ((element (make-etaf-event-element
                  :uuid uuid
                  :node node
                  :start start
                  :end end
                  :state (or initial-state
                             (list :hover nil :active nil :focus nil
                                   :disabled nil :enabled t))
                  :listeners nil)))
    (puthash uuid element etaf-event--elements)))

(defun etaf-event-unregister-element (uuid)
  "Unregister an element from event tracking."
  (when etaf-event--elements
    (remhash uuid etaf-event--elements)))

(defun etaf-event-get-element (uuid)
  "Get the element record for UUID."
  (when etaf-event--elements
    (gethash uuid etaf-event--elements)))

;;; ============================================================
;;; State Management
;;; ============================================================

(defun etaf-event-get-state (uuid &optional key)
  "Get the state of element UUID.
If KEY is provided, return the value for that state key.
Otherwise, return the full state plist."
  (when-let ((element (etaf-event-get-element uuid)))
    (let ((state (etaf-event-element-state element)))
      (if key
          (plist-get state key)
        state))))

(defun etaf-event-set-state (uuid state-key value)
  "Set STATE-KEY to VALUE for element UUID.
Returns t if state changed, nil otherwise.
Triggers state-change event if the value changed."
  (when-let ((element (etaf-event-get-element uuid)))
    (let* ((state (etaf-event-element-state element))
           (old-value (plist-get state state-key)))
      (unless (equal old-value value)
        ;; Update state
        (plist-put state state-key value)
        (setf (etaf-event-element-state element) state)
        
        ;; Dispatch state-change event
        (etaf-event-dispatch uuid 'state-change
                            (list :key state-key
                                  :old-value old-value
                                  :new-value value
                                  :state state))
        t))))

(defun etaf-event-update-states (state-updates)
  "Update multiple element states at once.
STATE-UPDATES is a list of (uuid state-key value) triplets.
Returns list of UUIDs that actually changed state."
  (let ((changed-uuids '()))
    (dolist (update state-updates)
      (cl-destructuring-bind (uuid state-key value) update
        (when (etaf-event-set-state uuid state-key value)
          (push uuid changed-uuids))))
    (nreverse changed-uuids)))

;;; ============================================================
;;; Event Listeners
;;; ============================================================

(defun etaf-event-add-listener (uuid event-type callback)
  "Add an event listener for UUID and EVENT-TYPE.
CALLBACK is a function called when the event fires.
Returns the callback for later removal."
  (unless etaf-event--listeners
    (etaf-event-init))
  
  (let* ((key (cons uuid event-type))
         (listeners (gethash key etaf-event--listeners)))
    (unless (member callback listeners)
      (puthash key (cons callback listeners) etaf-event--listeners)))
  callback)

(defun etaf-event-remove-listener (uuid event-type callback)
  "Remove an event listener for UUID and EVENT-TYPE."
  (when etaf-event--listeners
    (let* ((key (cons uuid event-type))
           (listeners (gethash key etaf-event--listeners)))
      (puthash key (remove callback listeners) etaf-event--listeners))))

(defun etaf-event-dispatch (uuid event-type &optional event-data)
  "Dispatch an event to all listeners for UUID and EVENT-TYPE.
EVENT-DATA is passed to each listener function."
  (when etaf-event--listeners
    (let* ((key (cons uuid event-type))
           (listeners (gethash key etaf-event--listeners)))
      (dolist (callback listeners)
        (condition-case err
            (funcall callback uuid event-data)
          (error
           (message "Error in event listener for %s %s: %S"
                    uuid event-type err)))))))

;;; ============================================================
;;; Mouse Hover Tracking
;;; ============================================================

(defun etaf-event-start-tracking ()
  "Start tracking mouse position and updating hover states."
  (unless etaf-event--tracking-timer
    (setq-local etaf-event--tracking-timer
                (run-with-idle-timer
                 etaf-event-hover-delay t
                 #'etaf-event-update-hover-state))))

(defun etaf-event-stop-tracking ()
  "Stop tracking mouse position."
  (when etaf-event--tracking-timer
    (cancel-timer etaf-event--tracking-timer)
    (setq-local etaf-event--tracking-timer nil)))

(defun etaf-event-update-hover-state ()
  "Update hover state based on current mouse position.
This is called periodically by the tracking timer."
  (when (and etaf-event--elements
             (not (minibufferp))
             (eq (current-buffer) (window-buffer (selected-window))))
    (let* ((mouse-pos (mouse-position))
           (frame (car mouse-pos))
           (pos (when (and frame (eq frame (selected-frame)))
                  (window-point (selected-window))))
           (new-hover-uuid nil))
      
      ;; Find which element the mouse is over
      (when pos
        (maphash
         (lambda (uuid element)
           (let ((start (etaf-event-element-start element))
                 (end (etaf-event-element-end element)))
             (when (and (>= pos start) (< pos end))
               (setq new-hover-uuid uuid))))
         etaf-event--elements))
      
      ;; Update hover state if changed
      (unless (equal new-hover-uuid etaf-event--hover-element)
        ;; Clear old hover
        (when etaf-event--hover-element
          (etaf-event-set-state etaf-event--hover-element :hover nil)
          (etaf-event-dispatch etaf-event--hover-element 'hover-leave))
        
        ;; Set new hover
        (when new-hover-uuid
          (etaf-event-set-state new-hover-uuid :hover t)
          (etaf-event-dispatch new-hover-uuid 'hover-enter))
        
        (setq-local etaf-event--hover-element new-hover-uuid)))))

;;; ============================================================
;;; Mouse Active Tracking
;;; ============================================================

(defun etaf-event-handle-mouse-down (event)
  "Handle mouse-down event for active state tracking."
  (interactive "e")
  (when etaf-event--elements
    (let* ((pos (posn-point (event-start event)))
           (uuid (etaf-event-find-element-at-pos pos)))
      (when uuid
        (etaf-event-set-state uuid :active t)
        (setq-local etaf-event--active-element uuid)
        (etaf-event-dispatch uuid 'mouse-down event)))))

(defun etaf-event-handle-mouse-up (event)
  "Handle mouse-up event for active state tracking."
  (interactive "e")
  (when etaf-event--active-element
    (etaf-event-set-state etaf-event--active-element :active nil)
    (etaf-event-dispatch etaf-event--active-element 'mouse-up event)
    (setq-local etaf-event--active-element nil)))

(defun etaf-event-find-element-at-pos (pos)
  "Find the UUID of the element at buffer position POS.
Returns nil if no element found."
  (when etaf-event--elements
    (let ((found-uuid nil))
      (maphash
       (lambda (uuid element)
         (let ((start (etaf-event-element-start element))
               (end (etaf-event-element-end element)))
           (when (and (>= pos start) (< pos end))
             (setq found-uuid uuid))))
       etaf-event--elements)
      found-uuid)))

;;; ============================================================
;;; Focus Tracking
;;; ============================================================

(defun etaf-event-set-focus (uuid)
  "Set focus to element UUID.
Clears focus from previously focused element."
  (unless (equal uuid etaf-event--focus-element)
    ;; Blur old element
    (when etaf-event--focus-element
      (etaf-event-set-state etaf-event--focus-element :focus nil)
      (etaf-event-dispatch etaf-event--focus-element 'blur))
    
    ;; Focus new element
    (when uuid
      (etaf-event-set-state uuid :focus t)
      (etaf-event-dispatch uuid 'focus))
    
    (setq-local etaf-event--focus-element uuid)))

(defun etaf-event-clear-focus ()
  "Clear focus from any focused element."
  (etaf-event-set-focus nil))

;;; ============================================================
;;; Pseudo-Class State Checking
;;; ============================================================

(defun etaf-event-matches-pseudo-class-p (uuid pseudo-class)
  "Check if element UUID matches PSEUDO-CLASS.
PSEUDO-CLASS is a keyword like :hover, :active, :focus, etc.
Returns t if the element is in the required state."
  (when-let ((element (etaf-event-get-element uuid)))
    (let ((state (etaf-event-element-state element)))
      (cond
       ((eq pseudo-class :hover)
        (plist-get state :hover))
       ((eq pseudo-class :active)
        (plist-get state :active))
       ((eq pseudo-class :focus)
        (plist-get state :focus))
       ((eq pseudo-class :disabled)
        (plist-get state :disabled))
       ((eq pseudo-class :enabled)
        (plist-get state :enabled))
       (t nil)))))

;;; ============================================================
;;; Utilities
;;; ============================================================

(defun etaf-event-get-all-elements ()
  "Get list of all registered element UUIDs."
  (when etaf-event--elements
    (let ((uuids '()))
      (maphash (lambda (uuid _element)
                 (push uuid uuids))
               etaf-event--elements)
      (nreverse uuids))))

(defun etaf-event-debug-info ()
  "Return debug information about the event system state."
  (list :element-count (if etaf-event--elements
                           (hash-table-count etaf-event--elements)
                         0)
        :hover-element etaf-event--hover-element
        :active-element etaf-event--active-element
        :focus-element etaf-event--focus-element
        :tracking-active (not (null etaf-event--tracking-timer))))

(provide 'etaf-event)
;;; etaf-event.el ends here
