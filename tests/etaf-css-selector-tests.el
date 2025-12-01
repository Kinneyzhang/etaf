;; -*- lexical-binding: t; -*-

(require 'etaf-dom-tests)
(require 'etaf-ert)

(defvar etaf-dom-tests-dom
  (etaf-etml-to-dom
    '(div :class "rounded-xl bg-white p-10" :id "test-id"
       (div :class "space-y-6" :id "test-div"
         (p "An advanced online playground for Tailwind CSS, including support for things like:")
         (ul :class "space-y-3"
           (li :id "1"
             (p :class "ml-3" "Customizing your theme with"
               (code :class "text-gray-950" "@theme")))
           (li :id "2"
             (p :class "ml-3"
               "Adding custom utilities with"
               (code :class "text-gray-950" "@utility")))
           (li :id "3"
             (p :class "ml-3" "Adding custom variants with"
               (code :class "text-gray-950" "@variant")))
           (li :id "4" :class "flex"
             (p :class "ml-3" "Code completion with instant preview")))
         (p "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online."))
       (hr :class "my-6 w-full")
       (p :class "mb-3" "Want to dig deeper into Tailwind?")
       (p :class "font-semibold"
         (a :href "https://tailwindcss.com/docs"
           :class "text-gray-950 dark:text-white" "Read the docs →")))))

;;; 测试基础选择器
(should
  (etaf-css-selector-basic-match-p
    etaf-dom-tests-dom
    (etaf-css-selector-parse "div.bg-white#test-id.p-10")))

(setq etaf-css-selector-tests-attr-dom (dom-by-tag etaf-dom-tests-dom 'a))
;; ((a ((href . "https://tailwindcss.com/docs") (class . "text-gray-950 dark:text-white")) "Read the docs →"))

(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse
      "a[href=\"https://tailwindcss.com/docs\"].text-gray-950")))

;; 属性前缀匹配
(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse "a[href^=\"https\"].text-gray-950")))

;; 属性后缀匹配
(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse "a[href$=\"docs\"].text-gray-950")))

;; 属性子串匹配
(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse "a[href*=\"tailwindcss\"]")))

;; 属性空格分隔值匹配
(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse "a[class~=\"dark:text-white\"]")))

;; 属性连字符前缀匹配
(should
  (etaf-css-selector-basic-match-p
    etaf-css-selector-tests-attr-dom
    (etaf-css-selector-parse "a[class|=\"text\"]")))

;;; 测试组合选择器

;; 后代组合器 A B: 选择所有被A包含的B元素(任意嵌套层级)
(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul code")
  '((code ((class . "text-gray-950")) "@theme") (code ((class . "text-gray-950")) "@utility") (code ((class . "text-gray-950")) "@variant")))

;; 子元素组合器 A > B: 仅选择A的直接子元素B(仅第一层嵌套)
(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "div > p")
  '((p nil "An advanced online playground for Tailwind CSS, including support for things like:") (p nil "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online.") (p ((class . "mb-3")) "Want to dig deeper into Tailwind?") (p ((class . "font-semibold")) (a ((href . "https://tailwindcss.com/docs") (class . "text-gray-950 dark:text-white")) "Read the docs →"))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "div#test-div > p")
  '((p nil "An advanced online playground for Tailwind CSS, including support for things like:") (p nil "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online.")))

;; 相邻兄弟组合器 A + B: 选择紧跟在A后面的第一个同级B元素
(should
  (equal
    (etaf-css-selector-query etaf-dom-tests-dom "li#1 + li")
    '((li ((id . "2")) (p ((class . "ml-3")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility"))))))

;; 后续兄弟组合器 A ~ B: 选择A之后的所有同级B元素
(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "li#2 ~ li")
  '((li ((id . "3")) (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant"))) (li ((id . "4") (class . "flex")) (p ((class . "ml-3")) "Code completion with instant preview"))))

;; 组合器列表 A, B: 同时选择A、B元素
(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "li#1, li#3")
  '((li ((id . "1")) (p ((class . "ml-3")) "Customizing your theme with" (code ((class . "text-gray-950")) "@theme"))) (li ((id . "3")) (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant")))))

;;; 测试伪类
(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul li:first-child")
  '((li ((id . "1")) (p ((class . "ml-3")) "Customizing your theme with" (code ((class . "text-gray-950")) "@theme")))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul li:last-child")
  '((li ((id . "4") (class . "flex")) (p ((class . "ml-3")) "Code completion with instant preview"))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul li:nth-child(3)")
  '((li ((id . "3")) (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant")))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul li:nth-child(odd)")
  '((li ((id . "1")) (p ((class . "ml-3")) "Customizing your theme with" (code ((class . "text-gray-950")) "@theme"))) (li ((id . "3")) (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant")))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "ul li:nth-child(even)")
  '((li ((id . "2")) (p ((class . "ml-3")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility"))) (li ((id . "4") (class . "flex")) (p ((class . "ml-3")) "Code completion with instant preview"))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "p:only-child")
  '((p ((class . "ml-3")) "Customizing your theme with" (code ((class . "text-gray-950")) "@theme")) (p ((class . "ml-3")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility")) (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant")) (p ((class . "ml-3")) "Code completion with instant preview")))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "hr:empty")
  '((hr ((class . "my-6 w-full")))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "div#test-id > p:first-of-type")
  '((p ((class . "mb-3")) "Want to dig deeper into Tailwind?")))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "div#test-id > p:last-of-type")
  '((p ((class . "font-semibold")) (a ((href . "https://tailwindcss.com/docs") (class . "text-gray-950 dark:text-white")) "Read the docs →"))))

(should-equal
  (etaf-css-selector-query etaf-dom-tests-dom "div#test-id > hr:only-of-type")
  '((hr ((class . "my-6 w-full")))))

;;; 测试链式子组合器 (修复 #pannel-input > div > p 匹配问题)
;; 确保链式子组合器正确工作，不会匹配嵌套更深的元素

(setq etaf-css-selector-tests-nested-dom
  (etaf-etml-to-dom
   '(div :id "pannel-input"
         (p :class "font-bold" "ETAF Playground - Input")
         (div (p "ETML Structure:")
              (div (p "111")))
         (div (p "CSS Styles:")
              (div (p "222")))
         (div (p "Elisp Data:")
              (div (p "333")))
         (p "hint" "happy hacking emacs"))))

;; #pannel-input > div > p 只应匹配直接子 div 的直接子 p
;; 不应匹配 #pannel-input > div > div > p (如 "111", "222", "333")
(should-equal
  (length (etaf-css-selector-query etaf-css-selector-tests-nested-dom "#pannel-input > div > p"))
  3)

;; 验证匹配的是正确的元素
(should-equal
  (mapcar #'dom-texts (etaf-css-selector-query etaf-css-selector-tests-nested-dom "#pannel-input > div > p"))
  '("ETML Structure:" "CSS Styles:" "Elisp Data:"))

;; #pannel-input > div > div > p 应该匹配嵌套的 p 元素
(should-equal
  (length (etaf-css-selector-query etaf-css-selector-tests-nested-dom "#pannel-input > div > div > p"))
  3)

;; 验证嵌套匹配的是正确的元素
(should-equal
  (mapcar #'dom-texts (etaf-css-selector-query etaf-css-selector-tests-nested-dom "#pannel-input > div > div > p"))
  '("111" "222" "333"))
