;;; etaf-css-inheritance-tests.el --- Tests for CSS property inheritance -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-inheritance)
(require 'etaf-etml)
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
 (let* ((test-dom (etaf-etml-to-dom
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

;;; 测试去重功能

(should
 (let* ((style-with-duplicates '((color . "red") (font-weight . "bold") (color . "blue") (font-size . "12px")))
        (result (etaf-css--remove-duplicate-properties style-with-duplicates)))
   ;; 应该只保留第一次出现的 color
   (and (= (length (seq-filter (lambda (p) (eq (car p) 'color)) result)) 1)
        (equal (cdr (assq 'color result)) "red"))))

;;; 测试继承时的去重

(should
 (let* ((parent-style '((color . "red") (font-weight . "bold")))
        ;; 子元素样式中已经有重复的 font-weight
        (child-style '((font-weight . "normal") (background . "white") (font-weight . "normal")))
        (result (etaf-css-apply-inheritance child-style parent-style)))
   ;; font-weight 应该只出现一次
   (and (= (length (seq-filter (lambda (p) (eq (car p) 'font-weight)) result)) 1)
        ;; color 应该被继承且只出现一次
        (= (length (seq-filter (lambda (p) (eq (car p) 'color)) result)) 1))))

(provide 'etaf-css-inheritance-tests)
;;; etaf-css-inheritance-tests.el ends here
