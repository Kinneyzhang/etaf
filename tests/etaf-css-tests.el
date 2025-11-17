;;; etaf-css-tests.el --- Tests for etaf-css -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试 CSS 声明解析

(should-equal
 (etaf-css-parse-declarations "color: red; font-size: 14px;")
 '((color . "red") (font-size . "14px")))

(should-equal
 (etaf-css-parse-declarations "  color:  red  ;  font-size:  14px  ")
 '((color . "red") (font-size . "14px")))

(should-equal
 (etaf-css-parse-declarations "display: flex;")
 '((display . "flex")))

(should-equal
 (etaf-css-parse-declarations "")
 nil)

(should-equal
 (etaf-css-parse-declarations "color: blue")
 '((color . "blue")))

;;; 测试 CSS 规则解析

(should
 (let ((rule (etaf-css-parse-rule "div { color: red; }")))
   (and (equal (plist-get rule :selector) "div")
        (equal (plist-get rule :declarations) '((color . "red")))
        (eq (plist-get rule :source) 'style-tag))))

(should
 (let ((rule (etaf-css-parse-rule ".button
 { background: blue; padding: 10px; }")))
   (and (equal (plist-get rule :selector) ".button")
        (equal (plist-get rule :declarations) 
               '((background . "blue") (padding . "10px"))))))

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
               '((color . "red") (font-size . "14px"))))))

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

(provide 'etaf-css-tests)
;;; etaf-css-tests.el ends here
