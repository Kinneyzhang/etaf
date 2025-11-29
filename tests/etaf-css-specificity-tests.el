;;; etaf-css-specificity-tests.el --- Tests for CSS specificity -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-etml)
(require 'etaf-ert)

;;; 测试特异性计算

;; 标签选择器
(should-equal
 (etaf-css-calculate-specificity "div")
 '(0 0 1))

;; 类选择器
(should-equal
 (etaf-css-calculate-specificity ".button")
 '(0 1 0))

;; ID 选择器
(should-equal
 (etaf-css-calculate-specificity "#main")
 '(1 0 0))

;; 组合选择器
(should-equal
 (etaf-css-calculate-specificity "div.button")
 '(0 1 1))

(should-equal
 (etaf-css-calculate-specificity "#main .text")
 '(1 1 0))

(should-equal
 (etaf-css-calculate-specificity "div#header.nav")
 '(1 1 1))

;; 复杂选择器
(should-equal
 (etaf-css-calculate-specificity "#nav ul li.active")
 '(1 1 2))

;;; 测试特异性比较

(should
 (etaf-css-specificity> '(1 0 0) '(0 1 0)))

(should
 (etaf-css-specificity> '(0 2 0) '(0 1 5)))

(should
 (not (etaf-css-specificity> '(0 1 0) '(0 1 1))))

;;; 测试层叠算法

;; 测试特异性覆盖
(setq etaf-css-specificity-tests-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "div { color: blue; } #test { color: green; } .button { color: yellow; }"))
         (body
          (div :id "test" :class "button" "Text")))))

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-specificity-tests-dom))
        (div-node (dom-by-id etaf-css-specificity-tests-dom "test"))
        (computed (etaf-css-get-computed-style cssom div-node etaf-css-specificity-tests-dom)))
   ;; #test (1,0,0) 应该覆盖 .button (0,1,0) 和 div (0,0,1)
   (equal (cdr (assq 'color computed)) "green")))

;; 测试内联样式优先级最高
(setq etaf-css-inline-priority-dom
      (etaf-etml-to-dom
       '(html
         (head
          (style "#test { color: blue; }"))
         (body
          (div :id "test" :style "color: red;" "Text")))))

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-inline-priority-dom))
        (div-node (dom-by-id etaf-css-inline-priority-dom "test"))
        (computed (etaf-css-get-computed-style cssom div-node etaf-css-inline-priority-dom)))
   ;; 内联样式应该覆盖 ID 选择器
   (equal (cdr (assq 'color computed)) "red")))

(provide 'etaf-css-specificity-tests)
;;; etaf-css-specificity-tests.el ends here
