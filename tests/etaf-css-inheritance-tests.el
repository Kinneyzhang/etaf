;;; etaf-css-inheritance-tests.el --- Tests for CSS property inheritance -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-inheritance)
(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试可继承属性检查

(should
 (etaf-css-property-inherits-p 'color))

(should
 (etaf-css-property-inherits-p 'font-family))

(should
 (not (etaf-css-property-inherits-p 'margin)))

(should
 (not (etaf-css-property-inherits-p 'border)))

;;; 测试继承应用

(should
 (let* ((parent-style '((color . "red") (font-size . "14px") (margin . "10px")))
        (child-style '((font-size . "12px")))
        (result (etaf-css-apply-inheritance child-style parent-style)))
   ;; color 应该被继承
   (and (assq 'color result)
        (equal (cdr (assq 'color result)) "red")
        ;; font-size 不应该被继承（子元素已定义）
        (equal (cdr (assq 'font-size result)) "12px")
        ;; margin 不应该被继承（不是可继承属性）
        (not (assq 'margin result)))))

;;; 测试继承集成（带 DOM）

(should
 (let* ((test-dom (etaf-tml-to-dom
                   '(div :style "color: red; font-size: 14px;"
                     (p "Text"))))
        (cssom (etaf-css-build-cssom test-dom))
        (p-node (dom-by-tag test-dom 'p))
        (computed (etaf-css-get-computed-style cssom p-node test-dom)))
   ;; color 应该从父元素继承
   (and (assq 'color computed)
        (equal (cdr (assq 'color computed)) "red")
        ;; font-size 应该从父元素继承
        (assq 'font-size computed)
        (equal (cdr (assq 'font-size computed)) "14px"))))

(provide 'etaf-css-inheritance-tests)
;;; etaf-css-inheritance-tests.el ends here
