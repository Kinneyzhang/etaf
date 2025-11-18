;;; etaf-css-important-tests.el --- Tests for !important and cascade -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-parser)
(require 'etaf-css-cascade)
(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试 !important 解析

(should-equal
 (etaf-css-parse-declarations "color: red !important;")
 '((color "red" t)))

(should-equal
 (etaf-css-parse-declarations "color: red!important;")
 '((color "red" t)))

(should-equal
 (etaf-css-parse-declarations "color: red  !  important  ;")
 '((color "red" t)))

(should-equal
 (etaf-css-parse-declarations "color: red; font-size: 14px !important;")
 '((color "red" nil) (font-size "14px" t)))

;;; 测试层叠算法 - !important 优先

(should
 (let* ((decl1 '("blue" (0 0 1) style-tag nil 0))   ; 普通声明
        (decl2 '("red" (0 0 1) style-tag t 1))      ; !important 声明
        (result (etaf-css-cascade-compare-declarations decl2 decl1)))
   result))  ; decl2 (!important) 应该优先

;;; 测试层叠算法 - 内联样式优先

(should
 (let* ((decl1 '("blue" (0 0 1) style-tag nil 0))
        (decl2 '("red" (0 0 0) inline nil 1))
        (result (etaf-css-cascade-compare-declarations decl2 decl1)))
   result))  ; decl2 (inline) 应该优先

;;; 测试层叠算法 - 特异性优先

(should
 (let* ((decl1 '("blue" (0 0 1) style-tag nil 0))  ; 低特异性
        (decl2 '("red" (0 1 0) style-tag nil 1))   ; 高特异性
        (result (etaf-css-cascade-compare-declarations decl2 decl1)))
   result))  ; decl2 (高特异性) 应该优先

;;; 测试层叠算法 - 文档顺序

(should
 (let* ((decl1 '("blue" (0 0 1) style-tag nil 0))  ; 先定义
        (decl2 '("red" (0 0 1) style-tag nil 1))   ; 后定义
        (result (etaf-css-cascade-compare-declarations decl2 decl1)))
   result))  ; decl2 (后定义) 应该优先

;;; 测试完整的层叠场景

(setq etaf-css-important-tests-dom
      (etaf-tml-to-dom
       '(html
         (head
          (style "div { color: blue !important; font-size: 12px; }"))
         (body
          (div :id "test" :style "color: red; font-size: 16px !important;" "Text")))))

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-important-tests-dom))
        (div-node (dom-by-id etaf-css-important-tests-dom "test"))
        (computed (etaf-css-get-computed-style
                   cssom div-node etaf-css-important-tests-dom)))
   ;; 样式表的 !important 应该战胜内联样式的普通声明
   (and (equal (cdr (assq 'color computed)) "blue")
        ;; 内联样式的 !important 应该战胜样式表的普通声明
        (equal (cdr (assq 'font-size computed)) "16px"))))

;;; 测试多规则层叠

(setq etaf-css-cascade-tests-dom
      (etaf-tml-to-dom
       '(html
         (head
          (style "div { color: green; }
                  .box { color: blue; }
                  #main { color: red; }"))
         (body
          (div :id "main" :class "box" "Text")))))

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-cascade-tests-dom))
        (div-node (dom-by-id etaf-css-cascade-tests-dom "main"))
        (computed (etaf-css-get-computed-style
                   cssom div-node etaf-css-cascade-tests-dom)))
   ;; ID 选择器特异性最高
   (equal (cdr (assq 'color computed)) "red")))

(provide 'etaf-css-important-tests)
;;; etaf-css-important-tests.el ends here
