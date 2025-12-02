(require 'etaf-ert)
(require 'etaf-etml)
(require 'etaf-vdom)

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

;;; :class attribute tests - string format (existing behavior)
(should-equal
  (etaf-etml-to-dom '(div :class "w-20 h-4" "Hello"))
  '(div ((class . "w-20 h-4")) "Hello"))

;;; :class attribute tests - list format (single string)
(should-equal
  (etaf-etml-to-dom '(div :class ("w-20 h-4") "Hello"))
  '(div ((class . "w-20 h-4")) "Hello"))

;;; :class attribute tests - list format with multiple strings
(should-equal
  (etaf-etml-to-dom '(div :class ("w-20 h-4" "border border-red-200" "bg-green-200") "test content"))
  '(div ((class . "w-20 h-4 border border-red-200 bg-green-200")) "test content"))

;;; :class attribute tests - list format equals concatenated string
(should-equal
  (etaf-etml-to-dom '(div :class ("w-20 h-4" "border border-red-200" "bg-green-200") "test content"))
  (etaf-etml-to-dom '(div :class "w-20 h-4 border border-red-200 bg-green-200" "test content")))

;;; :class attribute tests - list format with other attributes
(should-equal
  (etaf-etml-to-dom '(div :id "main" :class ("flex" "items-center") :style "color: blue" "Content"))
  '(div ((id . "main") (class . "flex items-center") (style . "color: blue")) "Content"))

;;; :class attribute tests - list format with empty strings (edge case)
(should-equal
  (etaf-etml-to-dom '(div :class ("w-20" "" "h-4") "Hello"))
  '(div ((class . "w-20 h-4")) "Hello"))

;;; :class attribute tests - empty list (edge case)
(should-equal
  (etaf-etml-to-dom '(div :class () "Hello"))
  '(div ((class . "")) "Hello"))

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
  ;; button uses CSS logical properties: padding-block and padding-inline
  (should (string-match "padding-block" style))
  (should (string-match "padding-inline" style))
  (should (string-match "border" style)))

;;; Test etaf-etml-tag integration - button tag should have tag-instance in VTree (not DOM)
;; Tag instances are now stored in the virtual DOM layer, not in DOM attributes
(let* ((result (etaf-etml-to-dom-with-vdom '(button "Click")))
        (vtree (etaf-vdom-result-get-vtree result))
        (dom (etaf-vdom-result-get-dom result))
        (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; DOM should NOT have etaf-tag-instance in attributes
  (should-not (assq 'etaf-tag-instance (cadr dom)))
  ;; VTree should have the tag-instance
  (should tag-instance)
  ;; tag-instance should have the button definition
  (should-equal (plist-get tag-instance :tag-name) 'button)
  ;; tag-instance should have children with button text
  (should-equal (plist-get tag-instance :children) '("Click"))
  ;; tag-instance should have definition with click handler
  (let ((definition (plist-get tag-instance :definition)))
    (should (plist-get definition :on-click))))

;;; Test etaf-etml-tag integration - button tag text should be in event
(let* ((result (etaf-etml-to-dom-with-vdom '(button "CLICK ME")))
        (vtree (etaf-vdom-result-get-vtree result))
        (tag-instance (etaf-vdom-get-tag-instance vtree))
        (event (etaf-etml-tag--make-event 'click tag-instance nil)))
  ;; event should have text property with button text
  (should-equal (plist-get event :text) "CLICK ME"))

;;; Test etaf-etml-tag integration - a tag should have tag-instance in VTree
(let* ((result (etaf-etml-to-dom-with-vdom '(a :href "/test" "Link")))
        (vtree (etaf-vdom-result-get-vtree result))
        (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; a tag has on-click handler, so VTree should have tag-instance
  (should tag-instance)
  (should-equal (plist-get tag-instance :tag-name) 'a))

;;; Test etaf-etml-tag integration - div should NOT have tag-instance (no events)
(let* ((result (etaf-etml-to-dom-with-vdom '(div "Text")))
        (vtree (etaf-vdom-result-get-vtree result))
        (tag-instance (etaf-vdom-get-tag-instance vtree)))
  ;; div has no event handlers or hover styles, so no tag-instance
  (should-not tag-instance))

;;; Test etaf-etml-tag integration - units consistency (px for horizontal, lh for vertical)
(let* ((result (etaf-etml-to-dom '(ul (li "Item"))))
        (attrs (cadr result))
        (style (cdr (assq 'style attrs))))
  ;; ul should have vertical margins in lh and horizontal padding in px
  (should (stringp style))
  (should (string-match "margin-top: 1lh" style))
  (should (string-match "margin-bottom: 1lh" style))
  (should (string-match "padding-left: 10px" style)))

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

;;; e-if Conditional Rendering Tests

(should-equal
  (etaf-etml-render '(div (p :e-if "visible" "Visible")) '(:visible t))
  '(div (p "Visible")))

(should-equal
  (etaf-etml-render '(div (p :e-if "visible" "Visible")) '(:visible nil))
  '(div))

(should-equal
  (etaf-etml-render '(div (p :e-if "count" "Has count")) '(:count 5))
  '(div (p "Has count")))

(should-equal
  (etaf-etml-render '(div (p :e-if "count" "Has count")) '(:count 0))
  '(div))

;; Test negation
(should-equal
  (etaf-etml-render '(div (p :e-if "!hidden" "Shown")) '(:hidden nil))
  '(div (p "Shown")))

(should-equal
  (etaf-etml-render '(div (p :e-if "!hidden" "Shown")) '(:hidden t))
  '(div))

;;; e-if/e-else Tests

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "loggedIn" "Welcome!")
       (p :e-else "Please login"))
    '(:loggedIn t))
  '(div (p "Welcome!")))

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "loggedIn" "Welcome!")
       (p :e-else "Please login"))
    '(:loggedIn nil))
  '(div (p "Please login")))

;;; e-if/e-else-if/e-else Tests

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "status" :class "{{ status }}" "Status A")
       (p :e-else-if "fallback" "Fallback")
       (p :e-else "Default"))
    '(:status "active" :fallback nil))
  '(div (p :class "active" "Status A")))

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "status" "Status")
       (p :e-else-if "fallback" "Fallback")
       (p :e-else "Default"))
    '(:status nil :fallback t))
  '(div (p "Fallback")))

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "status" "Status")
       (p :e-else-if "fallback" "Fallback")
       (p :e-else "Default"))
    '(:status nil :fallback nil))
  '(div (p "Default")))

;;; e-for List Rendering Tests

(should-equal
 (etaf-etml-render
  '(ul (li :e-for "item in items" "{{ item }}"))
  '(:items ("a" "b" "c")))
 '(ul (li "a") (li "b") (li "c")))

(should-equal
  (etaf-etml-render
    '(div (span :e-for "n in numbers" "{{ n }}"))
    '(:numbers (1 2 3)))
  '(div (span "1") (span "2") (span "3")))

;; Test e-for with index
(should-equal
  (etaf-etml-render
    '(ul (li :e-for "(item, idx) in items" "{{ idx }}: {{ item }}"))
    '(:items ("x" "y" "z")))
  '(ul (li "0: x") (li "1: y") (li "2: z")))

;; Test empty list
(should-equal
  (etaf-etml-render
    '(ul (li :e-for "item in items" "{{ item }}"))
    '(:items ()))
  '(ul))

;;; e-show Tests

(should-equal
  (etaf-etml-render '(div (p :e-show "visible" "Text")) '(:visible t))
  '(div (p "Text")))

(should-equal
  (etaf-etml-render '(div (p :e-show "visible" "Text")) '(:visible nil))
  '(div (p :style "display: none" "Text")))

;; Test e-show with existing style
(should-equal
  (etaf-etml-render
    '(div (p :e-show "visible" :style "color: red" "Text"))
    '(:visible nil))
  '(div (p :style "color: red; display: none" "Text")))

;;; e-text Tests

(should-equal
  (etaf-etml-render '(div (p :e-text "message")) '(:message "Hello"))
  '(div (p "Hello")))

(should-equal
  (etaf-etml-render '(div (p :e-text "count")) '(:count 99))
  '(div (p "99")))

;; e-text overrides children
(should-equal
  (etaf-etml-render
    '(div (p :e-text "message" "This will be replaced"))
    '(:message "New content"))
  '(div (p "New content")))

;;; Nested Elements Tests

(should-equal
  (etaf-etml-render
    '(div :class "container"
       (h1 "{{ title }}")
       (ul :e-if "items"
         (li :e-for "item in items" "{{ item }}")))
    '(:title "My List" :items ("one" "two")))
  '(div :class "container"
     (h1 "My List")
     (ul (li "one") (li "two"))))

;;; Combined Features Tests

(should-equal
  (etaf-etml-render
    '(div
       (p :e-if "showGreeting" "Hello, {{ name }}!")
       (ul :e-if "items"
         (li :e-for "item in items" :class "item" "{{ item }}")))
    '(:showGreeting t :name "World" :items ("A" "B")))
  '(div
     (p "Hello, World!")
     (ul (li :class "item" "A") (li :class "item" "B"))))

;;; Expression Evaluation Tests

;; Boolean literals
(should-equal
  (etaf-etml-render '(div (p :e-if "true" "Always")) '())
  '(div (p "Always")))

(should-equal
  (etaf-etml-render '(div (p :e-if "false" "Never")) '())
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

(provide 'etaf-etml-tests)
