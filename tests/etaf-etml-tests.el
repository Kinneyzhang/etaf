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


(provide 'etaf-etml-tests)
