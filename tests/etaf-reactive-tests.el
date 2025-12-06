;;; etaf-reactive-tests.el --- Tests for Vue 3-style reactive system -*- lexical-binding: t; -*-

;;; Commentary:

;; Comprehensive tests for ETAF's Vue 3-inspired reactive system.
;; Tests all features described in the problem statement:
;; 1. Basic reactivity with track/trigger
;; 2. WeakMap-style bucket structure
;; 3. Branch switching cleanup
;; 4. Nested effects with effect stack
;; 5. Infinite recursion prevention
;; 6. Scheduler support
;; 7. Task deduplication with microtask queue
;; 8. Computed properties with lazy evaluation
;; 9. Watch with immediate and flush options
;; 10. onInvalidate for race condition handling

;;; Code:

(require 'etaf-ert)
(require 'etaf-component)

;;; ============================================================================
;;; Test 1: Basic Reactivity - Track and Trigger
;;; ============================================================================

(ert-deftest etaf-reactive-test-basic-track-trigger ()
  "Test basic dependency tracking and triggering."
  (let ((count (etaf-ref 0))
        (result nil))
    ;; Create effect that reads ref
    (etaf-reactive-effect
     (lambda ()
       (setq result (etaf-ref-get count))))
    
    ;; Initial value should be set
    (should (= result 0))
    
    ;; Changing ref should trigger effect
    (etaf-ref-set count 5)
    (should (= result 5))
    
    (etaf-ref-set count 10)
    (should (= result 10))))

;;; ============================================================================
;;; Test 2: WeakMap-style Bucket Structure
;;; ============================================================================

(ert-deftest etaf-reactive-test-bucket-structure ()
  "Test that dependencies are properly stored in bucket structure."
  (let ((ref1 (etaf-ref 1))
        (ref2 (etaf-ref 2))
        (result1 nil)
        (result2 nil))
    
    ;; Effect that depends on ref1
    (etaf-reactive-effect
     (lambda ()
       (setq result1 (etaf-ref-get ref1))))
    
    ;; Effect that depends on ref2
    (etaf-reactive-effect
     (lambda ()
       (setq result2 (etaf-ref-get ref2))))
    
    ;; Each ref should only trigger its own effect
    (etaf-ref-set ref1 10)
    (should (= result1 10))
    (should (= result2 2))  ; Unchanged
    
    (etaf-ref-set ref2 20)
    (should (= result1 10))  ; Unchanged
    (should (= result2 20))))

;;; ============================================================================
;;; Test 3: Branch Switching Cleanup
;;; ============================================================================

(ert-deftest etaf-reactive-test-branch-switching ()
  "Test that old dependencies are cleaned up when branch switches."
  (let ((toggle (etaf-ref t))
        (a (etaf-ref 1))
        (b (etaf-ref 2))
        (result nil)
        (effect-runs 0))
    
    ;; Effect that conditionally reads a or b
    (etaf-reactive-effect
     (lambda ()
       (setq effect-runs (1+ effect-runs))
       (if (etaf-ref-get toggle)
           (setq result (etaf-ref-get a))
         (setq result (etaf-ref-get b)))))
    
    (should (= effect-runs 1))
    (should (= result 1))
    
    ;; Change a - should trigger
    (etaf-ref-set a 10)
    (should (= effect-runs 2))
    (should (= result 10))
    
    ;; Switch branch
    (etaf-ref-set toggle nil)
    (should (= effect-runs 3))
    (should (= result 2))
    
    ;; Change a - should NOT trigger (old dependency cleaned up)
    (setq effect-runs 0)
    (etaf-ref-set a 20)
    (should (= effect-runs 0))  ; Effect not run
    (should (= result 2))  ; Still b's value
    
    ;; Change b - should trigger
    (etaf-ref-set b 30)
    (should (= effect-runs 1))
    (should (= result 30))))

;;; ============================================================================
;;; Test 4: Nested Effects with Effect Stack
;;; ============================================================================

(ert-deftest etaf-reactive-test-nested-effects ()
  "Test that nested effects track dependencies correctly."
  (let ((outer-ref (etaf-ref 1))
        (inner-ref (etaf-ref 2))
        (outer-result nil)
        (inner-result nil))
    
    ;; Outer effect with nested inner effect
    (etaf-reactive-effect
     (lambda ()
       (setq outer-result (etaf-ref-get outer-ref))
       ;; Nested effect
       (etaf-reactive-effect
        (lambda ()
          (setq inner-result (etaf-ref-get inner-ref))))))
    
    (should (= outer-result 1))
    (should (= inner-result 2))
    
    ;; Change outer-ref - should only trigger outer
    (etaf-ref-set outer-ref 10)
    (should (= outer-result 10))
    
    ;; Change inner-ref - should trigger inner (and outer creates new inner)
    (etaf-ref-set inner-ref 20)
    (should (= inner-result 20))))

;;; ============================================================================
;;; Test 5: Infinite Recursion Prevention
;;; ============================================================================

(ert-deftest etaf-reactive-test-infinite-recursion-prevention ()
  "Test that self-triggering effects don't cause infinite loops."
  (let ((count (etaf-ref 0))
        (effect-runs 0))
    
    ;; Effect that reads and writes the same ref
    (etaf-reactive-effect
     (lambda ()
       (setq effect-runs (1+ effect-runs))
       (when (< (etaf-ref-get count) 5)
         ;; This would cause infinite loop without prevention
         (etaf-ref-set count (1+ (etaf-ref-get count))))))
    
    ;; Should only run once, not infinitely
    (should (= effect-runs 1))
    (should (= (etaf-ref-get count) 1))))

;;; ============================================================================
;;; Test 6: Scheduler Support
;;; ============================================================================

(ert-deftest etaf-reactive-test-scheduler ()
  "Test custom scheduler support for effects."
  (let ((count (etaf-ref 0))
        (scheduled-jobs nil)
        (result nil))
    
    ;; Effect with custom scheduler
    (etaf-reactive-effect
     (lambda ()
       (setq result (etaf-ref-get count)))
     (list :scheduler
           (lambda (effect)
             ;; Custom scheduler: add to queue instead of running
             (push effect scheduled-jobs))))
    
    ;; Initial run (no scheduler on first run)
    (should (= result 0))
    
    ;; Change ref - should call scheduler
    (etaf-ref-set count 5)
    
    ;; Effect not run yet
    (should (= result 0))
    (should scheduled-jobs)
    
    ;; Manually run scheduled job
    (funcall (plist-get (car scheduled-jobs) :run))
    (should (= result 5))))

;;; ============================================================================
;;; Test 7: Task Deduplication with Queue
;;; ============================================================================

(ert-deftest etaf-reactive-test-task-deduplication ()
  "Test that multiple changes batch into single effect execution."
  (let ((count (etaf-ref 0))
        (effect-runs 0))
    
    ;; Effect with scheduler (simulates batching)
    (etaf-reactive-effect
     (lambda ()
       (setq effect-runs (1+ effect-runs))
       (etaf-ref-get count))
     (list :scheduler
           (lambda (effect)
             (etaf-reactive--queue-job (plist-get effect :run)))))
    
    (should (= effect-runs 1))  ; Initial run
    
    ;; Multiple synchronous changes
    (etaf-ref-set count 1)
    (etaf-ref-set count 2)
    (etaf-ref-set count 3)
    
    ;; Jobs queued but not run yet
    (should (= effect-runs 1))
    
    ;; Flush queue
    (etaf-reactive--flush-jobs)
    
    ;; Effect should run only once for all changes
    (should (= effect-runs 2))))

;;; ============================================================================
;;; Test 8: Computed Properties with Lazy Evaluation
;;; ============================================================================

(ert-deftest etaf-reactive-test-computed-lazy ()
  "Test computed properties are lazily evaluated."
  (let* ((price (etaf-ref 100))
         (quantity (etaf-ref 2))
         (getter-runs 0)
         (total (etaf-computed
                 (lambda ()
                   (setq getter-runs (1+ getter-runs))
                   (* (etaf-ref-get price)
                      (etaf-ref-get quantity))))))
    
    ;; Getter not run until accessed
    (should (= getter-runs 0))
    
    ;; First access
    (should (= (etaf-computed-get total) 200))
    (should (= getter-runs 1))
    
    ;; Second access without changes - cached
    (should (= (etaf-computed-get total) 200))
    (should (= getter-runs 1))  ; Not run again
    
    ;; Change dependency
    (etaf-ref-set price 150)
    
    ;; Still not computed until accessed
    (should (= getter-runs 1))
    
    ;; Access triggers recomputation
    (should (= (etaf-computed-get total) 300))
    (should (= getter-runs 2))))

;;; ============================================================================
;;; Test 9: Computed Triggers Dependent Effects
;;; ============================================================================

(ert-deftest etaf-reactive-test-computed-triggers-effects ()
  "Test that computed values trigger dependent effects."
  (let* ((price (etaf-ref 100))
         (quantity (etaf-ref 2))
         (total (etaf-computed
                 (lambda ()
                   (* (etaf-ref-get price)
                      (etaf-ref-get quantity)))))
         (display nil))
    
    ;; Effect that depends on computed
    (etaf-reactive-effect
     (lambda ()
       (setq display (format "Total: %d" (etaf-computed-get total)))))
    
    (should (string= display "Total: 200"))
    
    ;; Change should trigger computed and effect
    (etaf-ref-set quantity 3)
    (should (string= display "Total: 300"))))

;;; ============================================================================
;;; Test 10: Watch with Immediate Option
;;; ============================================================================

(ert-deftest etaf-reactive-test-watch-immediate ()
  "Test watch with immediate option runs callback immediately."
  (let ((count (etaf-ref 5))
        (callback-runs 0)
        (values nil))
    
    ;; Watch with immediate
    (etaf-watch
     count
     (lambda (new old on-invalidate)
       (setq callback-runs (1+ callback-runs))
       (push (list new old) values))
     (list :immediate t))
    
    ;; Should run immediately
    (should (= callback-runs 1))
    (should (equal (car values) '(5 nil)))
    
    ;; Change triggers callback
    (etaf-ref-set count 10)
    ;; Note: May need to flush queue
    (etaf-reactive--flush-jobs)
    (should (>= callback-runs 2))))

;;; ============================================================================
;;; Test 11: Watch with Flush Option
;;; ============================================================================

(ert-deftest etaf-reactive-test-watch-flush-sync ()
  "Test watch with sync flush runs immediately."
  (let ((count (etaf-ref 0))
        (callback-value nil))
    
    ;; Watch with sync flush
    (etaf-watch
     count
     (lambda (new old on-invalidate)
       (setq callback-value new))
     (list :flush 'sync))
    
    ;; Change with sync should run immediately
    (etaf-ref-set count 5)
    (should (= callback-value 5))
    
    (etaf-ref-set count 10)
    (should (= callback-value 10))))

;;; ============================================================================
;;; Test 12: Watch onInvalidate for Race Conditions
;;; ============================================================================

(ert-deftest etaf-reactive-test-watch-on-invalidate ()
  "Test onInvalidate callback handles race conditions."
  (let ((id (etaf-ref 1))
        (cleanup-called nil)
        (last-value nil))
    
    ;; Watch with onInvalidate
    (etaf-watch
     id
     (lambda (new old on-invalidate)
       ;; Register cleanup
       (funcall on-invalidate
                (lambda ()
                  (setq cleanup-called t)))
       (setq last-value new))
     (list :flush 'sync))
    
    ;; First change
    (setq cleanup-called nil)
    (etaf-ref-set id 2)
    (should (= last-value 2))
    (should (not cleanup-called))
    
    ;; Second change should call cleanup first
    (etaf-ref-set id 3)
    (should cleanup-called)
    (should (= last-value 3))))

;;; ============================================================================
;;; Test 13: WatchEffect Auto-tracking
;;; ============================================================================

(ert-deftest etaf-reactive-test-watch-effect-auto-tracking ()
  "Test watchEffect automatically tracks dependencies."
  (let ((firstName (etaf-ref "John"))
        (lastName (etaf-ref "Doe"))
        (effect-runs 0)
        (result nil))
    
    ;; WatchEffect with auto-tracking
    (etaf-watch-effect
     (lambda ()
       (setq effect-runs (1+ effect-runs))
       (setq result (format "%s %s"
                           (etaf-ref-get firstName)
                           (etaf-ref-get lastName)))))
    
    ;; Runs immediately
    (should (= effect-runs 1))
    (should (string= result "John Doe"))
    
    ;; Changes trigger effect
    (etaf-ref-set firstName "Jane")
    (should (= effect-runs 2))
    (should (string= result "Jane Doe"))
    
    (etaf-ref-set lastName "Smith")
    (should (= effect-runs 3))
    (should (string= result "Jane Smith"))))

;;; ============================================================================
;;; Test 14: Stop Function
;;; ============================================================================

(ert-deftest etaf-reactive-test-stop-function ()
  "Test that stop function stops watching."
  (let ((count (etaf-ref 0))
        (result nil)
        (stop nil))
    
    ;; Create stoppable watch
    (setq stop
          (etaf-watch
           count
           (lambda (new old on-invalidate)
             (setq result new))
           (list :flush 'sync)))
    
    ;; Initial change
    (etaf-ref-set count 5)
    (should (= result 5))
    
    ;; Stop watching
    (funcall stop)
    
    ;; Further changes should not trigger
    (etaf-ref-set count 10)
    (should (= result 5))))  ; Still old value

;;; ============================================================================
;;; Test 15: Reactive Objects
;;; ============================================================================

(ert-deftest etaf-reactive-test-reactive-objects ()
  "Test reactive objects track individual keys."
  (let ((user (etaf-reactive '(:name "Alice" :age 30)))
        (name-changes 0)
        (age-changes 0))
    
    ;; Watch name
    (etaf-watch-effect
     (lambda ()
       (etaf-reactive-get user :name)
       (setq name-changes (1+ name-changes))))
    
    ;; Watch age  
    (etaf-watch-effect
     (lambda ()
       (etaf-reactive-get user :age)
       (setq age-changes (1+ age-changes))))
    
    ;; Initial run
    (should (= name-changes 1))
    (should (= age-changes 1))
    
    ;; Change name - only name watcher triggers
    (etaf-reactive-set user :name "Bob")
    (should (= name-changes 2))
    (should (= age-changes 1))
    
    ;; Change age - only age watcher triggers
    (etaf-reactive-set user :age 31)
    (should (= name-changes 2))
    (should (= age-changes 2))))

;;; ============================================================================
;;; Test 16: Integration - Counter Component Pattern
;;; ============================================================================

(ert-deftest etaf-reactive-test-counter-pattern ()
  "Test realistic counter component pattern."
  (let* ((count (etaf-ref 0))
         (doubled (etaf-computed
                   (lambda ()
                     (* 2 (etaf-ref-get count)))))
         (display nil)
         (increment (lambda ()
                      (etaf-ref-set count (1+ (etaf-ref-get count))))))
    
    ;; Auto-update display
    (etaf-watch-effect
     (lambda ()
       (setq display
             (format "Count: %d, Doubled: %d"
                     (etaf-ref-get count)
                     (etaf-computed-get doubled)))))
    
    ;; Initial state
    (should (string= display "Count: 0, Doubled: 0"))
    
    ;; Increment
    (funcall increment)
    (should (string= display "Count: 1, Doubled: 2"))
    
    (funcall increment)
    (should (string= display "Count: 2, Doubled: 4"))
    
    ;; Direct set
    (etaf-ref-set count 10)
    (should (string= display "Count: 10, Doubled: 20"))))

(provide 'etaf-reactive-tests)
;;; etaf-reactive-tests.el ends here
