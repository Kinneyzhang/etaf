;;; etaf-css-tests.el --- Tests for etaf-css -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试 CSS 声明解析（新格式支持 !important）

(should-equal
 (etaf-css-parse-declarations "color: red; font-size: 14px;")
 '((color "red" nil) (font-size "14px" nil)))

(should-equal
 (etaf-css-parse-declarations "  color:  red  ;  font-size:  14px  ")
 '((color "red" nil) (font-size "14px" nil)))

(should-equal
 (etaf-css-parse-declarations "display: flex;")
 '((display "flex" nil)))

(should-equal
 (etaf-css-parse-declarations "")
 nil)

(should-equal
 (etaf-css-parse-declarations "color: blue")
 '((color "blue" nil)))

;;; 测试 CSS 规则解析

(should
 (let ((rule (etaf-css-parse-rule "div { color: red; }")))
   (and (equal (plist-get rule :selector) "div")
        (equal (plist-get rule :declarations) '((color "red" nil)))
        (eq (plist-get rule :source) 'style-tag))))

(should
 (let ((rule (etaf-css-parse-rule ".button
 { background: blue; padding: 10px; }")))
   (and (equal (plist-get rule :selector) ".button")
        (equal (plist-get rule :declarations) 
               '((background "blue" nil) (padding "10px" nil))))))

;;; 测试 CSS 样式表解析

(should
 (let ((rules (etaf-css-parse-stylesheet 
               "div{color: red;}.button{background:blue;}")))
   (and (= (length rules) 2)
        (equal (plist-get (nth 0 rules) :selector) "div")
        (equal (plist-get (nth 1 rules) :selector) ".button"))))

;;; 测试内联样式提取

(setq etaf-css-tests-dom-with-inline
      (etaf-tml-to-dom
       '(div :style "color: red; font-size: 14px;"
             (p :style "margin:10px;" "Text")
             (span "No style"))))

(should
 (let ((rules (etaf-css-extract-inline-styles
               etaf-css-tests-dom-with-inline)))
   (and (= (length rules) 2)
        (eq (plist-get (nth 0 rules) :source) 'inline)
        (equal (plist-get (nth 0 rules) :declarations)
               '((color "red" nil) (font-size "14px" nil))))))

;;; 测试 style 标签提取

(setq etaf-css-tests-dom-with-style-tag
      (etaf-tml-to-dom
       '(html
         (head
          (style "div { color: blue; } .test { margin: 5px; }"))
         (body
          (div :class "test" "Content")))))

(should
 (let ((rules (etaf-css-extract-style-tags
               etaf-css-tests-dom-with-style-tag)))
   (and (= (length rules) 2)
        (equal (plist-get (nth 0 rules) :selector) "div")
        (equal (plist-get (nth 1 rules) :selector) ".test"))))

;;; 测试 CSSOM 构建

(setq etaf-css-tests-dom-full
      (etaf-tml-to-dom
       '(html
          (head
            (style "div { color: blue; font-weight: bold; } .highlight { background: yellow; }"))
          (body
            (div :class "container" :style "padding: 20px;"
              (div :class "highlight" "Highlighted text")
              (p "Normal paragraph"))))))

(should
 (let ((cssom (etaf-css-build-cssom etaf-css-tests-dom-full)))
   (and (plist-get cssom :inline-rules)
        (plist-get cssom :style-rules)
        (plist-get cssom :all-rules))))

;;; 测试节点样式查询

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-full))
        (div-node (dom-by-class etaf-css-tests-dom-full "highlight"))
        (rules (etaf-css-get-rules-for-node
                cssom div-node etaf-css-tests-dom-full)))
   (> (length rules) 0)))

;;; 测试计算样式

(should
 (let* ((test-dom (etaf-tml-to-dom
                   '(div :style "color: red; font-size: 14px;"
                      (p "Text"))))
        (cssom (etaf-css-build-cssom test-dom))
        (computed (etaf-css-get-computed-style cssom test-dom test-dom)))
   (and (assq 'color computed)
        (equal (cdr (assq 'color computed)) "red")
        (assq 'font-size computed)
        (equal (cdr (assq 'font-size computed)) "14px"))))

;;; 测试样式层叠（内联样式覆盖外部样式）

(setq etaf-css-tests-dom-cascade
      (etaf-tml-to-dom
       '(html
         (head
          (style "div { color: blue; font-size: 12px; }"))
         (body
          (div :id "test" :style "color: red;" "Text")))))

(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-cascade))
        (div-node (dom-by-id etaf-css-tests-dom-cascade "test"))
        (computed (etaf-css-get-computed-style
                   cssom div-node etaf-css-tests-dom-cascade)))
   (and (equal (cdr (assq 'color computed)) "red")  ; 内联样式覆盖
        (equal (cdr (assq 'font-size computed)) "12px"))))  ; 继承外部样式

;;; 测试 CSSOM 转字符串

(should
 (let* ((test-dom (etaf-tml-to-dom
                   '(div :style "color: red;" "Text")))
        (cssom (etaf-css-build-cssom test-dom))
        (css-string (etaf-css-cssom-to-string cssom)))
   (stringp css-string)))

;;; 测试组合选择器样式应用

;; 测试后代选择器
(setq etaf-css-tests-dom-combinators
      (etaf-tml-to-dom
       '(div :id "parent"
             (div :id "child-1" "Child 1"
                  (span :id "grandchild" "Grandchild"))
             (div :id "child-2" "Child 2")
             (div :id "child-3" "Child 3"))))

;; 测试后代选择器 - parent div 应该匹配 child-1, child-2, child-3, NOT parent
(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-combinators))
        (cssom (etaf-css-add-stylesheet cssom "#parent div { padding-left: 10px; }"))
        (parent (car (dom-by-id etaf-css-tests-dom-combinators "parent")))
        (child-1 (car (dom-by-id etaf-css-tests-dom-combinators "child-1")))
        (style-parent (etaf-css-get-computed-style cssom parent etaf-css-tests-dom-combinators))
        (style-child (etaf-css-get-computed-style cssom child-1 etaf-css-tests-dom-combinators)))
   (and (null (alist-get 'padding-left style-parent))  ; parent should NOT get padding
        (equal (alist-get 'padding-left style-child) "10px")))) ; child should get padding

;; 测试子元素选择器 - #parent > div 应该只匹配直接子元素
(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-combinators))
        (cssom (etaf-css-add-stylesheet cssom "#parent > div { margin-left: 5px; }"))
        (parent (car (dom-by-id etaf-css-tests-dom-combinators "parent")))
        (child-1 (car (dom-by-id etaf-css-tests-dom-combinators "child-1")))
        (style-parent (etaf-css-get-computed-style cssom parent etaf-css-tests-dom-combinators))
        (style-child (etaf-css-get-computed-style cssom child-1 etaf-css-tests-dom-combinators)))
   (and (null (alist-get 'margin-left style-parent))  ; parent should NOT get margin
        (equal (alist-get 'margin-left style-child) "5px")))) ; child should get margin

;; 测试相邻兄弟选择器 - #child-1 + div 应该只匹配 child-2
(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-combinators))
        (cssom (etaf-css-add-stylesheet cssom "#child-1 + div { color: blue; }"))
        (child-1 (car (dom-by-id etaf-css-tests-dom-combinators "child-1")))
        (child-2 (car (dom-by-id etaf-css-tests-dom-combinators "child-2")))
        (child-3 (car (dom-by-id etaf-css-tests-dom-combinators "child-3")))
        (style-1 (etaf-css-get-computed-style cssom child-1 etaf-css-tests-dom-combinators))
        (style-2 (etaf-css-get-computed-style cssom child-2 etaf-css-tests-dom-combinators))
        (style-3 (etaf-css-get-computed-style cssom child-3 etaf-css-tests-dom-combinators)))
   (and (null (alist-get 'color style-1))  ; child-1 should NOT get color
        (equal (alist-get 'color style-2) "blue")  ; child-2 should get color
        (null (alist-get 'color style-3))))) ; child-3 should NOT get color

;; 测试通用兄弟选择器 - #child-1 ~ div 应该匹配 child-2 和 child-3
(should
 (let* ((cssom (etaf-css-build-cssom etaf-css-tests-dom-combinators))
        (cssom (etaf-css-add-stylesheet cssom "#child-1 ~ div { font-weight: bold; }"))
        (child-1 (car (dom-by-id etaf-css-tests-dom-combinators "child-1")))
        (child-2 (car (dom-by-id etaf-css-tests-dom-combinators "child-2")))
        (child-3 (car (dom-by-id etaf-css-tests-dom-combinators "child-3")))
        (style-1 (etaf-css-get-computed-style cssom child-1 etaf-css-tests-dom-combinators))
        (style-2 (etaf-css-get-computed-style cssom child-2 etaf-css-tests-dom-combinators))
        (style-3 (etaf-css-get-computed-style cssom child-3 etaf-css-tests-dom-combinators)))
   (and (null (alist-get 'font-weight style-1))  ; child-1 should NOT get font-weight
        (equal (alist-get 'font-weight style-2) "bold")  ; child-2 should get font-weight
        (equal (alist-get 'font-weight style-3) "bold")))) ; child-3 should get font-weight

(provide 'etaf-css-tests)
;;; etaf-css-tests.el ends here
