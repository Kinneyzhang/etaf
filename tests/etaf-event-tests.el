;;; etaf-event-tests.el --- Tests for ETAF Event Model -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; This file is part of ETAF.

;;; Commentary:

;; Unit tests for etaf-event.el module

;;; Code:

(require 'ert)
(require 'etaf-event)

;;; Initialization Tests

(ert-deftest etaf-event-test-init ()
  "Test event system initialization."
  (with-temp-buffer
    (etaf-event-init)
    (should (hash-table-p etaf-event--elements))
    (should (hash-table-p etaf-event--listeners))
    (should (= (hash-table-count etaf-event--elements) 0))
    (should (null etaf-event--hover-element))
    (should (null etaf-event--active-element))
    (should (null etaf-event--focus-element))
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-cleanup ()
  "Test event system cleanup."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    (should (= (hash-table-count etaf-event--elements) 1))
    (etaf-event-cleanup)
    (should (= (hash-table-count etaf-event--elements) 0))))

;;; Element Registration Tests

(ert-deftest etaf-event-test-register-element ()
  "Test element registration."
  (with-temp-buffer
    (etaf-event-init)
    (let ((node '(div nil)))
      (etaf-event-register-element "test-uuid" node 1 10)
      (let ((element (etaf-event-get-element "test-uuid")))
        (should (not (null element)))
        (should (equal (etaf-event-element-uuid element) "test-uuid"))
        (should (equal (etaf-event-element-node element) node))
        (should (= (etaf-event-element-start element) 1))
        (should (= (etaf-event-element-end element) 10))))
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-unregister-element ()
  "Test element unregistration."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    (should (etaf-event-get-element "test-uuid"))
    (etaf-event-unregister-element "test-uuid")
    (should (null (etaf-event-get-element "test-uuid")))
    (etaf-event-cleanup)))

;;; State Management Tests

(ert-deftest etaf-event-test-get-state ()
  "Test getting element state."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    (let ((state (etaf-event-get-state "test-uuid")))
      (should (not (null state)))
      (should (eq (plist-get state :hover) nil))
      (should (eq (plist-get state :active) nil))
      (should (eq (plist-get state :focus) nil)))
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-set-state ()
  "Test setting element state."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    
    ;; Set hover state
    (should (etaf-event-set-state "test-uuid" :hover t))
    (should (eq (etaf-event-get-state "test-uuid" :hover) t))
    
    ;; Setting same value should return nil
    (should (null (etaf-event-set-state "test-uuid" :hover t)))
    
    ;; Clear hover state
    (should (etaf-event-set-state "test-uuid" :hover nil))
    (should (eq (etaf-event-get-state "test-uuid" :hover) nil))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-update-states ()
  "Test batch state updates."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "elem1" '(div nil) 1 10)
    (etaf-event-register-element "elem2" '(span nil) 11 20)
    
    (let ((changed (etaf-event-update-states
                    '(("elem1" :hover t)
                      ("elem2" :active t)
                      ("elem1" :focus t)))))
      (should (= (length changed) 3))
      (should (member "elem1" changed))
      (should (member "elem2" changed))
      (should (eq (etaf-event-get-state "elem1" :hover) t))
      (should (eq (etaf-event-get-state "elem1" :focus) t))
      (should (eq (etaf-event-get-state "elem2" :active) t)))
    
    (etaf-event-cleanup)))

;;; Event Listener Tests

(ert-deftest etaf-event-test-add-listener ()
  "Test adding event listeners."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    
    (let ((called-ref (list nil)))  ; Use a mutable list
      (let ((callback (lambda (uuid data)
                        (setcar called-ref (list uuid data)))))
        (etaf-event-add-listener "test-uuid" 'test-event callback)
        
        ;; Dispatch event
        (etaf-event-dispatch "test-uuid" 'test-event '(:foo bar))
        (should (equal (car called-ref) '("test-uuid" (:foo bar))))))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-remove-listener ()
  "Test removing event listeners."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    
    (let ((called-ref (list nil)))  ; Use a mutable list
      (let ((callback (lambda (uuid data)
                        (setcar called-ref t))))
        (etaf-event-add-listener "test-uuid" 'test-event callback)
        (etaf-event-remove-listener "test-uuid" 'test-event callback)
        
        ;; Dispatch event - should not call removed listener
        (etaf-event-dispatch "test-uuid" 'test-event '(:foo bar))
        (should (null (car called-ref)))))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-state-change-listener ()
  "Test state-change event is dispatched on state updates."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    
    (let ((state-changes-ref (list '())))  ; Use a mutable list
      (let ((listener (lambda (uuid data)
                        (setcar state-changes-ref 
                                (cons (list uuid data) (car state-changes-ref))))))
        (etaf-event-add-listener "test-uuid" 'state-change listener)
        
        ;; Change state
        (etaf-event-set-state "test-uuid" :hover t)
        (etaf-event-set-state "test-uuid" :active t)
        
        (let ((state-changes (car state-changes-ref)))
          (should (= (length state-changes) 2))
          (let ((first-change (cadr (nth 1 state-changes))))
            (should (eq (plist-get first-change :key) :hover))
            (should (eq (plist-get first-change :old-value) nil))
            (should (eq (plist-get first-change :new-value) t))))))
    
    (etaf-event-cleanup)))

;;; Focus Management Tests

(ert-deftest etaf-event-test-set-focus ()
  "Test focus management."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "elem1" '(div nil) 1 10)
    (etaf-event-register-element "elem2" '(div nil) 11 20)
    
    ;; Set focus to elem1
    (etaf-event-set-focus "elem1")
    (should (eq (etaf-event-get-state "elem1" :focus) t))
    (should (equal etaf-event--focus-element "elem1"))
    
    ;; Move focus to elem2 (should clear elem1)
    (etaf-event-set-focus "elem2")
    (should (eq (etaf-event-get-state "elem1" :focus) nil))
    (should (eq (etaf-event-get-state "elem2" :focus) t))
    (should (equal etaf-event--focus-element "elem2"))
    
    ;; Clear focus
    (etaf-event-clear-focus)
    (should (eq (etaf-event-get-state "elem2" :focus) nil))
    (should (null etaf-event--focus-element))
    
    (etaf-event-cleanup)))

;;; Pseudo-Class Matching Tests

(ert-deftest etaf-event-test-matches-pseudo-class ()
  "Test pseudo-class state matching."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(div nil) 1 10)
    
    ;; Initially nothing should match
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :hover)))
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :active)))
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :focus)))
    
    ;; Set hover state
    (etaf-event-set-state "test-uuid" :hover t)
    (should (etaf-event-matches-pseudo-class-p "test-uuid" :hover))
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :active)))
    
    ;; Set active state
    (etaf-event-set-state "test-uuid" :active t)
    (should (etaf-event-matches-pseudo-class-p "test-uuid" :hover))
    (should (etaf-event-matches-pseudo-class-p "test-uuid" :active))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-enabled-disabled ()
  "Test enabled/disabled pseudo-class states."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "test-uuid" '(button nil) 1 10)
    
    ;; Initially enabled
    (should (etaf-event-matches-pseudo-class-p "test-uuid" :enabled))
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :disabled)))
    
    ;; Disable element
    (etaf-event-set-state "test-uuid" :disabled t)
    (etaf-event-set-state "test-uuid" :enabled nil)
    (should (etaf-event-matches-pseudo-class-p "test-uuid" :disabled))
    (should (null (etaf-event-matches-pseudo-class-p "test-uuid" :enabled)))
    
    (etaf-event-cleanup)))

;;; Utility Tests

(ert-deftest etaf-event-test-get-all-elements ()
  "Test getting all registered element UUIDs."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "elem1" '(div nil) 1 10)
    (etaf-event-register-element "elem2" '(span nil) 11 20)
    (etaf-event-register-element "elem3" '(p nil) 21 30)
    
    (let ((uuids (etaf-event-get-all-elements)))
      (should (= (length uuids) 3))
      (should (member "elem1" uuids))
      (should (member "elem2" uuids))
      (should (member "elem3" uuids)))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-debug-info ()
  "Test debug information retrieval."
  (with-temp-buffer
    (etaf-event-init)
    (etaf-event-register-element "elem1" '(div nil) 1 10)
    (etaf-event-set-focus "elem1")
    
    (let ((info (etaf-event-debug-info)))
      (should (= (plist-get info :element-count) 1))
      (should (equal (plist-get info :focus-element) "elem1"))
      (should (null (plist-get info :hover-element)))
      (should (null (plist-get info :active-element))))
    
    (etaf-event-cleanup)))

(ert-deftest etaf-event-test-find-element-at-pos ()
  "Test finding element at buffer position."
  (with-temp-buffer
    (etaf-event-init)
    (insert "0123456789")
    (etaf-event-register-element "elem1" '(div nil) 1 5)
    (etaf-event-register-element "elem2" '(span nil) 6 10)
    
    (should (equal (etaf-event-find-element-at-pos 3) "elem1"))
    (should (equal (etaf-event-find-element-at-pos 7) "elem2"))
    (should (null (etaf-event-find-element-at-pos 15)))
    
    (etaf-event-cleanup)))

(provide 'etaf-event-tests)
;;; etaf-event-tests.el ends here
