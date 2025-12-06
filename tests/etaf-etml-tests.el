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

(provide 'etaf-etml-tests)
