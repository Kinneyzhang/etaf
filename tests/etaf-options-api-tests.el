;;; etaf-options-api-tests.el --- Tests for Vue 2 Options API support -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Vue 2 Options API component support in ETAF.
;; Tests both Options API and ensures Composition API still works.

;;; Code:

(require 'etaf-ert)
(require 'etaf-component)

;;; ============================================================================
;;; Basic Options API Tests
;;; ============================================================================

;;; Test 1: Basic component with :data
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-data-component
    :data (lambda () (list :message "Hello")))
  
  (let ((component (etaf-component-get 'test-data-component)))
    (should component)
    (should (plist-get component :data))))

;;; Test 2: Component with :methods
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-methods-component
    :data (lambda () (list :count 0))
    :methods (list :increment (lambda () (message "incrementing"))))
  
  (let ((component (etaf-component-get 'test-methods-component)))
    (should component)
    (should (plist-get component :methods))))

;;; Test 3: Component with :computed
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-computed-component
    :data (lambda () (list :value 5))
    :computed (list :doubled (lambda () (* 2 10))))
  
  (let ((component (etaf-component-get 'test-computed-component)))
    (should component)
    (should (plist-get component :computed))))

;;; Test 4: Component with :watch
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-watch-component
    :data (lambda () (list :text ""))
    :watch (list :text (lambda (new old) (message "Text changed"))))
  
  (let ((component (etaf-component-get 'test-watch-component)))
    (should component)
    (should (plist-get component :watch))))

;;; Test 5: Component with lifecycle hooks
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-lifecycle-component
    :data (lambda () (list :ready nil))
    :mounted (lambda () (message "mounted"))
    :updated (lambda () (message "updated"))
    :unmounted (lambda () (message "unmounted")))
  
  (let ((component (etaf-component-get 'test-lifecycle-component)))
    (should component)
    (should (plist-get component :mounted))
    (should (plist-get component :updated))
    (should (plist-get component :unmounted))))

;;; ============================================================================
;;; Composition API Still Works
;;; ============================================================================

;;; Test 6: Composition API with :setup still works
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-setup-component
    :setup (lambda (props)
             (let ((count (etaf-ref 0)))
               (list :count count))))
  
  (let ((component (etaf-component-get 'test-setup-component)))
    (should component)
    (should (plist-get component :setup))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

;;; Test 7: Complete Options API component definition
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-full-options
    :props '(:initial-value)
    :data (lambda () 
            (list :count 0 :message "test"))
    :computed (list 
               :doubled (lambda () 
                         (let ((count-ref (plist-get this :count)))
                           (* 2 (etaf-ref-get count-ref)))))
    :methods (list
              :increment (lambda ()
                          (let ((count-ref (plist-get this :count)))
                            (etaf-ref-update count-ref #'1+)))
              :reset (lambda ()
                      (let ((count-ref (plist-get this :count)))
                        (etaf-ref-set count-ref 0))))
    :watch (list
            :count (lambda (new old)
                    (message "Count: %s -> %s" old new)))
    :mounted (lambda () 
              (message "Component mounted"))
    :template (lambda (data)
                `(div "Test component")))
  
  (let ((component (etaf-component-get 'test-full-options)))
    (should component)
    (should (plist-get component :data))
    (should (plist-get component :computed))
    (should (plist-get component :methods))
    (should (plist-get component :watch))
    (should (plist-get component :mounted))
    (should (plist-get component :template))))

;;; Test 8: Both APIs can coexist
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  ;; Define Options API component
  (etaf-define-component test-options-component
    :data (lambda () (list :value 1)))
  
  ;; Define Composition API component
  (etaf-define-component test-composition-component
    :setup (lambda (props) (list :value (etaf-ref 1))))
  
  ;; Both should be registered
  (should (etaf-component-defined-p 'test-options-component))
  (should (etaf-component-defined-p 'test-composition-component)))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

;;; Test 9: Component with no data or setup should still work
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-static-component
    :template '(div "Static content"))
  
  (let ((component (etaf-component-get 'test-static-component)))
    (should component)
    (should (plist-get component :template))))

;;; Test 10: Props work with Options API
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-props-options
    :props '(:title :count)
    :data (lambda () (list :internal-state 0))
    :template (lambda (data)
                `(div "{{ title }}" "{{ count }}")))
  
  (let ((component (etaf-component-get 'test-props-options)))
    (should component)
    (should (equal (plist-get component :props) '(:title :count)))
    (should (plist-get component :data))))

;;; Test 11: Empty data function
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-empty-data
    :data (lambda () nil)
    :template '(div "Empty data"))
  
  (let ((component (etaf-component-get 'test-empty-data)))
    (should component)
    (should (plist-get component :data))))

;;; Test 12: Multiple computed properties
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-multiple-computed
    :data (lambda () (list :x 2 :y 3))
    :computed (list
               :sum (lambda () (+ 2 3))
               :product (lambda () (* 2 3))
               :power (lambda () (expt 2 3))))
  
  (let ((component (etaf-component-get 'test-multiple-computed)))
    (should component)
    (let ((computed-props (plist-get component :computed)))
      (should computed-props)
      (should (plist-get computed-props :sum))
      (should (plist-get computed-props :product))
      (should (plist-get computed-props :power)))))

;;; Test 13: Multiple methods
(let ((etaf-component-registry (make-hash-table :test 'eq)))
  (etaf-define-component test-multiple-methods
    :data (lambda () (list :value 0))
    :methods (list
              :increment (lambda () (message "inc"))
              :decrement (lambda () (message "dec"))
              :reset (lambda () (message "reset"))
              :double (lambda () (message "double"))))
  
  (let ((component (etaf-component-get 'test-multiple-methods)))
    (should component)
    (let ((methods (plist-get component :methods)))
      (should methods)
      (should (plist-get methods :increment))
      (should (plist-get methods :decrement))
      (should (plist-get methods :reset))
      (should (plist-get methods :double)))))

(message "All Options API tests completed successfully!")

(provide 'etaf-options-api-tests)
;;; etaf-options-api-tests.el ends here
