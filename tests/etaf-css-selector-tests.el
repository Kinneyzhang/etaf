(require 'etaf-dom-tests)

;;; 测试基础选择器

(etaf-css-selector-basic-match-p
 etaf-dom-tests-dom
 (etaf-css-selector-parse "div.bg-white#test-id.p-10"))

(setq etaf-css-selector-tests-dom-1 (dom-by-tag etaf-dom-tests-dom 'a))
;; ((a ((href . "https://tailwindcss.com/docs") (class . "text-gray-950 dark:text-white")) "Read the docs →"))
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse
  "a[href=\"https://tailwindcss.com/docs\"].text-gray-950"))

;; 属性前缀匹配
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse "a[href^=\"https\"].text-gray-950"))

;; 属性后缀匹配
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse "a[href$=\"docs\"].text-gray-950"))

;; 属性子串匹配
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse "a[href*=\"tailwindcss\"]"))

;; 属性空格分隔值匹配
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse "a[class~=\"dark:text-white\"]"))

;; 属性连字符前缀匹配
(etaf-css-selector-basic-match-p
 etaf-css-selector-tests-dom-1
 (etaf-css-selector-parse "a[class|=\"text\"]"))

;;; 测试组合选择器

;; 后代组合器 A B: 选择所有被A包含的B元素(任意嵌套层级)
(etaf-css-selector-query etaf-dom-tests-dom "ul code")

;; 子元素组合器 A > B: 仅选择A的直接子元素B(仅第一层嵌套)
(etaf-css-selector-query etaf-dom-tests-dom "div > p")

;; 相邻兄弟组合器 A + B: 选择紧跟在A后面的第一个同级B元素
(etaf-css-selector-query etaf-dom-tests-dom "li#1 + li")

;; 后续兄弟组合器 A ~ B: 选择A之后的所有同级B元素
(etaf-css-selector-query etaf-dom-tests-dom "li#2 ~ li")

;; 组合器列表 A, B: 同时选择A、B元素
(etaf-css-selector-query etaf-dom-tests-dom "li#1, li#3")
