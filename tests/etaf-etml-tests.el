(require 'etaf-ert)
(require 'etaf-etml)

(setq-local lisp-indent-offset 2)

;;; :style attribute tests - string format
(should-equal
 (etaf-etml-to-dom '(div :style "background: red" "Hello"))
 '(div ((style . "background: red")) "Hello"))

;;; :style attribute tests - list format (alist)
(should-equal
 (etaf-etml-to-dom '(div :style ((background . "red")) "Hello"))
 '(div ((style . "background: red")) "Hello"))

;;; :style attribute tests - list format with multiple properties
(should-equal
 (etaf-etml-to-dom '(div :style ((background . "red") (padding . "10px")) "Hello"))
 '(div ((style . "background: red; padding: 10px")) "Hello"))

;;; :style attribute tests - mixed with other attributes
(should-equal
 (etaf-etml-to-dom '(div :class "box" :style ((color . "blue")) "World"))
 '(div ((class . "box") (style . "color: blue")) "World"))

;;; Test etaf-etml-tag integration - p tag should NOT have default styles
;; (p tag has :default-style nil, so no styles are merged)
(let* ((result (etaf-etml-to-dom '(p "Hello")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; p tag should NOT have any style since :default-style is nil
  (should-equal style nil))

;;; Test etaf-etml-tag integration - p tag inline style is preserved as-is
(let* ((result (etaf-etml-to-dom '(p :style "margin-top: 2lh" "Hello")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; inline style should be preserved
  (should (stringp style))
  (should (string-match "margin-top: 2lh" style)))

;;; Test etaf-etml-tag integration - button tag should have padding styles
(let* ((result (etaf-etml-to-dom '(button "Click")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; button should have padding and border styles from etaf-etml-tag
  (should (stringp style))
  (should (string-match "padding-left" style))
  (should (string-match "padding-right" style))
  (should (string-match "border" style)))

;;; Test etaf-etml-tag integration - units consistency (px for horizontal, lh for vertical)
(let* ((result (etaf-etml-to-dom '(ul (li "Item"))))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; ul should have vertical margins in lh and horizontal padding in px
  (should (stringp style))
  (should (string-match "margin-top: 1lh" style))
  (should (string-match "margin-bottom: 1lh" style))
  (should (string-match "padding-left: 40px" style)))

(should-equal
 (etaf-etml-render '(div "Hello, {{ name }}!") '(:name "World"))
 '(div "Hello, World!"))

(should-equal
 (etaf-etml-render '(div "Count: {{ count }}") '(:count 42))
 '(div "Count: 42"))

(should-equal
 (etaf-etml-render '(p "{{ greeting }}, {{ name }}!")
                   '(:greeting "Hi" :name "Alice"))
 '(p "Hi, Alice!"))

;; Test nested property access
(should-equal
 (etaf-etml-render '(div "{{ user.name }}") '(:user (:name "Bob")))
 '(div "Bob"))

;; Test empty interpolation
(should-equal
 (etaf-etml-render '(div "Value: {{ missing }}") '(:other "x"))
 '(div "Value: "))

;;; v-if Conditional Rendering Tests

(should-equal
 (etaf-etml-render '(div (p :v-if "visible" "Visible")) '(:visible t))
 '(div (p "Visible")))

(should-equal
 (etaf-etml-render '(div (p :v-if "visible" "Visible")) '(:visible nil))
 '(div))

(should-equal
 (etaf-etml-render '(div (p :v-if "count" "Has count")) '(:count 5))
 '(div (p "Has count")))

(should-equal
 (etaf-etml-render '(div (p :v-if "count" "Has count")) '(:count 0))
 '(div))

;; Test negation
(should-equal
 (etaf-etml-render '(div (p :v-if "!hidden" "Shown")) '(:hidden nil))
 '(div (p "Shown")))

(should-equal
 (etaf-etml-render '(div (p :v-if "!hidden" "Shown")) '(:hidden t))
 '(div))

;;; v-if/v-else Tests

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "loggedIn" "Welcome!")
    (p :v-else "Please login"))
  '(:loggedIn t))
 '(div (p "Welcome!")))

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "loggedIn" "Welcome!")
    (p :v-else "Please login"))
  '(:loggedIn nil))
 '(div (p "Please login")))

;;; v-if/v-else-if/v-else Tests

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "status" :class "{{ status }}" "Status A")
    (p :v-else-if "fallback" "Fallback")
    (p :v-else "Default"))
  '(:status "active" :fallback nil))
 '(div (p :class "active" "Status A")))

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "status" "Status")
    (p :v-else-if "fallback" "Fallback")
    (p :v-else "Default"))
  '(:status nil :fallback t))
 '(div (p "Fallback")))

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "status" "Status")
    (p :v-else-if "fallback" "Fallback")
    (p :v-else "Default"))
  '(:status nil :fallback nil))
 '(div (p "Default")))

;;; v-for List Rendering Tests

(should-equal
 (etaf-etml-render
  '(ul (li :v-for "item in items" "{{ item }}"))
  '(:items ("a" "b" "c")))
 '(ul (li "a") (li "b") (li "c")))

(should-equal
 (etaf-etml-render
  '(div (span :v-for "n in numbers" "{{ n }}"))
  '(:numbers (1 2 3)))
 '(div (span "1") (span "2") (span "3")))

;; Test v-for with index
(should-equal
 (etaf-etml-render
  '(ul (li :v-for "(item, idx) in items" "{{ idx }}: {{ item }}"))
  '(:items ("x" "y" "z")))
 '(ul (li "0: x") (li "1: y") (li "2: z")))

;; Test empty list
(should-equal
 (etaf-etml-render
  '(ul (li :v-for "item in items" "{{ item }}"))
  '(:items ()))
 '(ul))

;;; v-show Tests

(should-equal
 (etaf-etml-render '(div (p :v-show "visible" "Text")) '(:visible t))
 '(div (p "Text")))

(should-equal
 (etaf-etml-render '(div (p :v-show "visible" "Text")) '(:visible nil))
 '(div (p :style "display: none" "Text")))

;; Test v-show with existing style
(should-equal
 (etaf-etml-render
  '(div (p :v-show "visible" :style "color: red" "Text"))
  '(:visible nil))
 '(div (p :style "color: red; display: none" "Text")))

;;; v-text Tests

(should-equal
 (etaf-etml-render '(div (p :v-text "message")) '(:message "Hello"))
 '(div (p "Hello")))

(should-equal
 (etaf-etml-render '(div (p :v-text "count")) '(:count 99))
 '(div (p "99")))

;; v-text overrides children
(should-equal
 (etaf-etml-render
  '(div (p :v-text "message" "This will be replaced"))
  '(:message "New content"))
 '(div (p "New content")))

;;; Nested Elements Tests

(should-equal
 (etaf-etml-render
  '(div :class "container"
        (h1 "{{ title }}")
        (ul :v-if "items"
            (li :v-for "item in items" "{{ item }}")))
  '(:title "My List" :items ("one" "two")))
 '(div :class "container"
       (h1 "My List")
       (ul (li "one") (li "two"))))

;;; Combined Features Tests

(should-equal
 (etaf-etml-render
  '(div
    (p :v-if "showGreeting" "Hello, {{ name }}!")
    (ul :v-if "items"
        (li :v-for "item in items" :class "item" "{{ item }}")))
  '(:showGreeting t :name "World" :items ("A" "B")))
 '(div
   (p "Hello, World!")
   (ul (li :class "item" "A") (li :class "item" "B"))))

;;; Expression Evaluation Tests

;; Boolean literals
(should-equal
 (etaf-etml-render '(div (p :v-if "true" "Always")) '())
 '(div (p "Always")))

(should-equal
 (etaf-etml-render '(div (p :v-if "false" "Never")) '())
 '(div))

;; Number literals in expressions
(should-equal
 (etaf-etml-render '(div "{{ '42' }}") '())
 '(div "42"))

;;; Attribute Interpolation Tests

(should-equal
 (etaf-etml-render '(a :href "/user/{{ id }}" "Link") '(:id 123))
 '(a :href "/user/123" "Link"))

(should-equal
 (etaf-etml-render '(div :class "item-{{ type }}" "Content") '(:type "active"))
 '(div :class "item-active" "Content"))


;;; ============================================================================
;;; Component System Tests
;;; ============================================================================

;;; Test component registration

(etaf-etml-define-component test-greeting
  :props '(:name)
  :template '(div "Hello, {{ name }}!"))

(should-equal (etaf-etml-component-defined-p 'test-greeting) t)
(should-equal (etaf-etml-component-defined-p 'undefined-component) nil)

;;; Test component list

(let ((components (etaf-etml-component-list-all)))
  (should (member 'test-greeting components)))

;;; Test component get

(let ((component (etaf-etml-component-get 'test-greeting)))
  (should (not (null component)))
  (should-equal (plist-get component :name) 'test-greeting)
  (should-equal (plist-get component :props) '(:name)))

;;; Test component with setup function

(etaf-etml-define-component test-counter
  :props '(:initial)
  :setup (lambda (props)
           (list :count (or (plist-get props :initial) 0)
                 :label "Count"))
  :template '(div (span "{{ label }}: {{ count }}")))

(should-equal (etaf-etml-component-defined-p 'test-counter) t)

;;; Test component with template function

(etaf-etml-define-component test-dynamic
  :props '(:items)
  :template (lambda (data)
              `(ul ,@(mapcar (lambda (item)
                               `(li ,item))
                             (plist-get data :items)))))

(should-equal (etaf-etml-component-defined-p 'test-dynamic) t)

;;; ============================================================================
;;; Reactive System Tests - Ref
;;; ============================================================================

;;; Test ref creation

(let ((count (etaf-etml-ref 0)))
  (should (etaf-etml-ref-p count))
  (should-equal (etaf-etml-ref-get count) 0))

;;; Test ref set

(let ((count (etaf-etml-ref 0)))
  (etaf-etml-ref-set count 5)
  (should-equal (etaf-etml-ref-get count) 5))

;;; Test ref update

(let ((count (etaf-etml-ref 10)))
  (etaf-etml-ref-update count (lambda (n) (+ n 5)))
  (should-equal (etaf-etml-ref-get count) 15))

;;; Test ref dependency tracking

(let* ((triggered nil)
       (count (etaf-etml-ref 0))
       (effect (lambda ()
                 (setq triggered t))))
  ;; Manually add effect as dependency
  (plist-put count :deps (list effect))
  ;; Change value should trigger effect
  (etaf-etml-ref-set count 1)
  (should triggered))

;;; Test ref doesn't trigger when value is same

(let* ((trigger-count 0)
       (count (etaf-etml-ref 5))
       (effect (lambda ()
                 (setq trigger-count (1+ trigger-count)))))
  (plist-put count :deps (list effect))
  (etaf-etml-ref-set count 5)  ; same value, should not trigger
  (should-equal trigger-count 0)
  (etaf-etml-ref-set count 6)  ; different value, should trigger
  (should-equal trigger-count 1))

;;; ============================================================================
;;; Reactive System Tests - Computed
;;; ============================================================================

;;; Test computed creation

(let ((doubled (etaf-etml-computed (lambda () (* 2 5)))))
  (should (etaf-etml-computed-p doubled))
  (should-equal (etaf-etml-computed-get doubled) 10))

;;; Test computed with ref dependency

(let* ((count (etaf-etml-ref 3))
       (doubled (etaf-etml-computed
                 (lambda () (* 2 (etaf-etml-ref-get count))))))
  (should-equal (etaf-etml-computed-get doubled) 6)
  (etaf-etml-ref-set count 5)
  (should-equal (etaf-etml-computed-get doubled) 10))

;;; Test computed caching

(let* ((compute-count 0)
       (count (etaf-etml-ref 3))
       (doubled (etaf-etml-computed
                 (lambda ()
                   (setq compute-count (1+ compute-count))
                   (* 2 (etaf-etml-ref-get count))))))
  ;; First access computes
  (etaf-etml-computed-get doubled)
  (should-equal compute-count 1)
  ;; Second access uses cache (no recompute)
  (etaf-etml-computed-get doubled)
  (should-equal compute-count 1)
  ;; After dependency changes, recomputes
  (etaf-etml-ref-set count 5)
  (etaf-etml-computed-get doubled)
  (should-equal compute-count 2))

;;; ============================================================================
;;; Reactive System Tests - Watch
;;; ============================================================================

;;; Test watch-source basic

(let* ((changes nil)
       (count (etaf-etml-ref 0))
       (stop (etaf-etml-watch-source
              count
              (lambda (new old)
                (push (list old new) changes)))))
  (etaf-etml-ref-set count 1)
  (etaf-etml-ref-set count 2)
  (should-equal (length changes) 2)
  (should-equal (car changes) '(1 2))
  ;; Stop watching
  (funcall stop)
  (etaf-etml-ref-set count 3)
  ;; No new changes after stop
  (should-equal (length changes) 2))

;;; Test watch-source immediate option

(let* ((called nil)
       (count (etaf-etml-ref 5))
       (stop (etaf-etml-watch-source
              count
              (lambda (new old)
                (push (list old new) called))
              '(:immediate t))))
  ;; Should be called immediately with current value
  (should-equal (length called) 1)
  (should-equal (car called) '(nil 5))
  (funcall stop))

;;; ============================================================================
;;; Reactive System Tests - Watch Effect
;;; ============================================================================

;;; Test watch-effect runs immediately

(let* ((run-count 0)
       (count (etaf-etml-ref 0))
       (stop (etaf-etml-watch-effect
              (lambda ()
                (etaf-etml-ref-get count)  ; access to track dependency
                (setq run-count (1+ run-count))))))
  ;; Should run once immediately
  (should-equal run-count 1)
  ;; Should run again when dependency changes
  (etaf-etml-ref-set count 1)
  (should-equal run-count 2)
  ;; Stop the effect
  (funcall stop)
  (etaf-etml-ref-set count 2)
  ;; Should not run after stop
  (should-equal run-count 2))

;;; ============================================================================
;;; Reactive System Tests - Reactive Object
;;; ============================================================================

;;; Test reactive object creation

(let ((state (etaf-etml-reactive '(:name "Alice" :age 30))))
  (should (etaf-etml-reactive-p state))
  (should-equal (etaf-etml-reactive-get state :name) "Alice")
  (should-equal (etaf-etml-reactive-get state :age) 30))

;;; Test reactive object set

(let ((state (etaf-etml-reactive '(:count 0))))
  (etaf-etml-reactive-set state :count 5)
  (should-equal (etaf-etml-reactive-get state :count) 5))

;;; Test reactive object to plist

(let ((state (etaf-etml-reactive '(:x 1 :y 2))))
  (let ((plist (etaf-etml-reactive-to-plist state)))
    (should-equal (plist-get plist :x) 1)
    (should-equal (plist-get plist :y) 2)))

;;; Test reactive object with watch

(let* ((changes nil)
       (state (etaf-etml-reactive '(:value 0)))
       (refs (plist-get state :refs))
       (value-ref (gethash :value refs)))
  (etaf-etml-watch-source
   value-ref
   (lambda (new old)
     (push new changes)))
  (etaf-etml-reactive-set state :value 1)
  (etaf-etml-reactive-set state :value 2)
  (should-equal changes '(2 1)))

;;; ============================================================================
;;; Legacy Reactive System Tests (backward compatibility)
;;; ============================================================================

;;; Test legacy create-reactive

(let ((data (etaf-etml-create-reactive '(:count 0 :name "test"))))
  (should-equal (etaf-etml-get data :count) 0)
  (should-equal (etaf-etml-get data :name) "test"))

;;; Test legacy set

(let ((data (etaf-etml-create-reactive '(:count 0))))
  (etaf-etml-set data :count 5)
  (should-equal (etaf-etml-get data :count) 5))

;;; Test legacy watch

(let* ((changes nil)
       (data (etaf-etml-create-reactive '(:count 0))))
  (etaf-etml-watch data
                   (lambda (reactive key value)
                     (push (list key value) changes)))
  (etaf-etml-set data :count 1)
  (etaf-etml-set data :count 2)
  (should-equal (length changes) 2)
  (should-equal (car changes) '(:count 2)))

;;; Test legacy unwatch

(let* ((changes nil)
       (data (etaf-etml-create-reactive '(:count 0)))
       (watcher (lambda (reactive key value)
                  (push value changes))))
  (etaf-etml-watch data watcher)
  (etaf-etml-set data :count 1)
  (etaf-etml-unwatch data watcher)
  (etaf-etml-set data :count 2)
  ;; Only one change recorded (before unwatch)
  (should-equal (length changes) 1)
  (should-equal (car changes) 1))

(provide 'etaf-etml-tests)
