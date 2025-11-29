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

;;; Test etaf-tag integration - p tag should have default margin styles
(let* ((result (etaf-etml-to-dom '(p "Hello")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; p tag should have margin-top and margin-bottom styles from etaf-tag
  (should (stringp style))
  (should (string-match "margin-top" style))
  (should (string-match "margin-bottom" style))
  (should (string-match "1lh" style)))

;;; Test etaf-tag integration - inline style overrides default
(let* ((result (etaf-etml-to-dom '(p :style "margin-top: 2lh" "Hello")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; inline style should override default
  (should (stringp style))
  (should (string-match "margin-top: 2lh" style))
  ;; default margin-bottom should still be present
  (should (string-match "margin-bottom: 1lh" style)))

;;; Test etaf-tag integration - button tag should have padding styles
(let* ((result (etaf-etml-to-dom '(button "Click")))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; button should have padding and border styles from etaf-tag
  (should (stringp style))
  (should (string-match "padding-left" style))
  (should (string-match "padding-right" style))
  (should (string-match "border" style)))

;;; Test etaf-tag integration - units consistency (px for horizontal, lh for vertical)
(let* ((result (etaf-etml-to-dom '(ul (li "Item"))))
       (attrs (cadr result))
       (style (cdr (assq 'style attrs))))
  ;; ul should have vertical margins in lh and horizontal padding in px
  (should (stringp style))
  (should (string-match "margin-top: 1lh" style))
  (should (string-match "margin-bottom: 1lh" style))
  (should (string-match "padding-left: 40px" style)))

(provide 'etaf-etml-tests)
