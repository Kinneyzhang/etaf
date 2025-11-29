;;; etaf-tailwind-tests.el --- Tests for etaf-tailwind.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Keywords: test, tailwind, css

;;; Commentary:

;; 测试 etaf-tailwind.el 的功能

;;; Code:

(require 'etaf-tailwind)
(require 'etaf-ert)

;;; 类名解析测试

(ert-deftest etaf-tailwind-test-parse-simple-class ()
  "测试解析简单的 Tailwind 类名。"
  (let ((result (etaf-tailwind-parse-class "bg-red-500")))
    (should (null (plist-get result :variants)))
    (should (equal (plist-get result :utility) "bg-red-500"))
    (should (equal (plist-get result :property) "bg"))
    (should (equal (plist-get result :value) "red-500"))))

(ert-deftest etaf-tailwind-test-parse-class-with-variant ()
  "测试解析带变体的 Tailwind 类名。"
  (let ((result (etaf-tailwind-parse-class "hover:bg-blue-500")))
    (should (equal (plist-get result :variants) '("hover")))
    (should (equal (plist-get result :utility) "bg-blue-500"))
    (should (equal (plist-get result :property) "bg"))
    (should (equal (plist-get result :value) "blue-500"))))

(ert-deftest etaf-tailwind-test-parse-class-with-multiple-variants ()
  "测试解析带多个变体的 Tailwind 类名。"
  (let ((result (etaf-tailwind-parse-class "md:hover:bg-red-500")))
    (should (equal (plist-get result :variants) '("md" "hover")))
    (should (equal (plist-get result :utility) "bg-red-500"))
    (should (equal (plist-get result :property) "bg"))
    (should (equal (plist-get result :value) "red-500"))))

(ert-deftest etaf-tailwind-test-parse-arbitrary-value ()
  "测试解析任意值语法。"
  (let ((result (etaf-tailwind-parse-class "bg-[#1da1f2]")))
    (should (equal (plist-get result :property) "bg"))
    (should (equal (plist-get result :arbitrary) "#1da1f2"))))

(ert-deftest etaf-tailwind-test-parse-standalone-class ()
  "测试解析独立实用类。"
  (let ((result (etaf-tailwind-parse-class "flex")))
    (should (equal (plist-get result :utility) "flex"))
    (should (equal (plist-get result :property) "flex"))
    (should (null (plist-get result :value)))))

;;; 类名验证测试

(ert-deftest etaf-tailwind-test-class-p-valid ()
  "测试有效 Tailwind 类名验证。"
  (should (etaf-tailwind-class-p "bg-red-500"))
  (should (etaf-tailwind-class-p "hover:text-lg"))
  (should (etaf-tailwind-class-p "md:hover:bg-blue-500"))
  (should (etaf-tailwind-class-p "flex"))
  (should (etaf-tailwind-class-p "p-4"))
  (should (etaf-tailwind-class-p "dark:bg-gray-800")))

(ert-deftest etaf-tailwind-test-class-p-invalid ()
  "测试无效类名验证。"
  (should-not (etaf-tailwind-class-p nil))
  (should-not (etaf-tailwind-class-p ""))
  (should-not (etaf-tailwind-class-p "xyz-invalid-class")))

;;; 变体检查测试

(ert-deftest etaf-tailwind-test-get-variants ()
  "测试获取变体列表。"
  (should (null (etaf-tailwind-get-variants "bg-red-500")))
  (should (equal (etaf-tailwind-get-variants "hover:bg-red-500") '("hover")))
  (should (equal (etaf-tailwind-get-variants "md:hover:focus:bg-red-500")
                 '("md" "hover" "focus"))))

(ert-deftest etaf-tailwind-test-has-variant ()
  "测试变体检查。"
  (should (etaf-tailwind-has-variant-p "hover:bg-red-500" "hover"))
  (should-not (etaf-tailwind-has-variant-p "hover:bg-red-500" "focus"))
  (should (etaf-tailwind-has-variant-p "md:hover:bg-red-500" "md")))

(ert-deftest etaf-tailwind-test-has-responsive ()
  "测试响应式前缀检查。"
  (should (etaf-tailwind-has-responsive-p "md:bg-red-500"))
  (should (etaf-tailwind-has-responsive-p "lg:hover:bg-red-500"))
  (should-not (etaf-tailwind-has-responsive-p "hover:bg-red-500")))

(ert-deftest etaf-tailwind-test-has-state-variant ()
  "测试状态变体检查。"
  (should (etaf-tailwind-has-state-variant-p "hover:bg-red-500"))
  (should (etaf-tailwind-has-state-variant-p "md:focus:bg-red-500"))
  (should-not (etaf-tailwind-has-state-variant-p "md:bg-red-500")))

;;; CSS 转换测试

(ert-deftest etaf-tailwind-test-to-css-background ()
  "测试背景颜色转换。"
  (should-equal (etaf-tailwind-to-css "bg-red-500")
                '((background-color . "#ef4444"))))

(ert-deftest etaf-tailwind-test-to-css-padding ()
  "测试内边距转换。
Emacs特有：水平方向使用px，垂直方向使用lh。"
  ;; p-4 expands to all four directions with appropriate units
  (should-equal (etaf-tailwind-to-css "p-4")
                '((padding-top . "4lh") (padding-right . "4px")
                  (padding-bottom . "4lh") (padding-left . "4px")))
  ;; px-2 is horizontal only - uses px
  (should-equal (etaf-tailwind-to-css "px-2")
                '((padding-left . "2px") (padding-right . "2px")))
  ;; py-2 is vertical only - uses lh
  (should-equal (etaf-tailwind-to-css "py-2")
                '((padding-top . "2lh") (padding-bottom . "2lh"))))

(ert-deftest etaf-tailwind-test-to-css-display ()
  "测试显示属性转换。"
  (should-equal (etaf-tailwind-to-css "flex")
                '((display . "flex")))
  (should-equal (etaf-tailwind-to-css "hidden")
                '((display . "none")))
  (should-equal (etaf-tailwind-to-css "block")
                '((display . "block"))))

(ert-deftest etaf-tailwind-test-to-css-rounded ()
  "测试圆角转换。"
  (should-equal (etaf-tailwind-to-css "rounded-lg")
                '((border-radius . "0.5rem")))
  (should-equal (etaf-tailwind-to-css "rounded-full")
                '((border-radius . "9999px"))))

(ert-deftest etaf-tailwind-test-to-css-shadow ()
  "测试阴影转换。"
  (let ((shadow-md (etaf-tailwind-to-css "shadow-md")))
    (should (assq 'box-shadow shadow-md))))

(ert-deftest etaf-tailwind-test-to-css-flexbox ()
  "测试 Flexbox 属性转换。"
  (should-equal (etaf-tailwind-to-css "justify-center")
                '((justify-content . "center")))
  (should-equal (etaf-tailwind-to-css "items-center")
                '((align-items . "center"))))

(ert-deftest etaf-tailwind-test-to-css-border-width ()
  "测试边框宽度转换（修复 border-1 等类的支持）。"
  ;; border without value means 1px
  (should-equal (etaf-tailwind-to-css "border")
                '((border-width . "1px")))
  ;; border-0 means 0px
  (should-equal (etaf-tailwind-to-css "border-0")
                '((border-width . "0px")))
  ;; border-1 means 1px (这是被修复的关键案例)
  (should-equal (etaf-tailwind-to-css "border-1")
                '((border-width . "1px")))
  ;; border-2 means 2px
  (should-equal (etaf-tailwind-to-css "border-2")
                '((border-width . "2px")))
  ;; border-4 means 4px
  (should-equal (etaf-tailwind-to-css "border-4")
                '((border-width . "4px")))
  ;; border-8 means 8px
  (should-equal (etaf-tailwind-to-css "border-8")
                '((border-width . "8px"))))

(ert-deftest etaf-tailwind-test-to-css-border-color ()
  "测试边框颜色转换。"
  ;; border-red-500 means border-color: #ef4444
  (should-equal (etaf-tailwind-to-css "border-red-500")
                '((border-color . "#ef4444")))
  ;; border-blue-500 means border-color: #3b82f6
  (should-equal (etaf-tailwind-to-css "border-blue-500")
                '((border-color . "#3b82f6"))))

(ert-deftest etaf-tailwind-test-classes-to-css ()
  "测试多个类名转换为 CSS。
p-4 被展开为 padding-top/right/bottom/left。"
  (let ((result (etaf-tailwind-classes-to-css "flex items-center p-4")))
    (should (assq 'display result))
    (should (assq 'align-items result))
    ;; padding is expanded into directional properties
    (should (assq 'padding-top result))
    (should (assq 'padding-left result))))

;;; DOM 集成测试

(ert-deftest etaf-tailwind-test-dom-has-class ()
  "测试 DOM 节点类检查。"
  (let ((node '(div ((class . "flex bg-red-500")) "content")))
    (should (etaf-dom-node-has-tailwind-class-p node "flex"))
    (should (etaf-dom-node-has-tailwind-class-p node "bg-red-500"))
    (should-not (etaf-dom-node-has-tailwind-class-p node "p-4"))))

(ert-deftest etaf-tailwind-test-add-class ()
  "测试添加 Tailwind 类。"
  (let ((node '(div ((class . "flex")) "content")))
    (etaf-tailwind-add-class node "p-4")
    (should (etaf-dom-has-class node "flex"))
    (should (etaf-dom-has-class node "p-4"))))

(ert-deftest etaf-tailwind-test-remove-class ()
  "测试移除 Tailwind 类。"
  (let ((node '(div ((class . "flex p-4")) "content")))
    (etaf-tailwind-remove-class node "p-4")
    (should (etaf-dom-has-class node "flex"))
    (should-not (etaf-dom-has-class node "p-4"))))

(ert-deftest etaf-tailwind-test-toggle-class ()
  "测试切换 Tailwind 类。"
  (let ((node '(div ((class . "flex")) "content")))
    (etaf-tailwind-toggle-class node "p-4")
    (should (etaf-dom-has-class node "p-4"))
    (etaf-tailwind-toggle-class node "p-4")
    (should-not (etaf-dom-has-class node "p-4"))))

(ert-deftest etaf-tailwind-test-replace-class ()
  "测试替换 Tailwind 类。"
  (let ((node '(div ((class . "bg-red-500")) "content")))
    (etaf-tailwind-replace-class node "bg-red-500" "bg-blue-500")
    (should-not (etaf-dom-has-class node "bg-red-500"))
    (should (etaf-dom-has-class node "bg-blue-500"))))

(ert-deftest etaf-tailwind-test-get-classes-by-property ()
  "测试按属性获取类。"
  (let ((node '(div ((class . "bg-red-500 bg-opacity-50 text-white p-4")) "content")))
    (let ((bg-classes (etaf-tailwind-get-classes-by-property node "bg")))
      (should (member "bg-red-500" bg-classes))
      (should (member "bg-opacity-50" bg-classes)))))

;;; 实用函数测试

(ert-deftest etaf-tailwind-test-describe-class ()
  "测试类名描述。"
  (let ((desc (etaf-tailwind-describe-class "md:hover:bg-red-500")))
    (should (stringp desc))
    (should (string-match "Tailwind Class:" desc))
    (should (string-match "md" desc))
    (should (string-match "hover" desc))))

(ert-deftest etaf-tailwind-test-filter-classes ()
  "测试过滤类列表。"
  (let ((classes '("flex" "p-4" "invalid-xyz" "bg-red-500")))
    (let ((filtered (etaf-tailwind-filter-classes classes)))
      (should (member "flex" filtered))
      (should (member "p-4" filtered))
      (should (member "bg-red-500" filtered))
      (should-not (member "invalid-xyz" filtered)))))

(ert-deftest etaf-tailwind-test-css-to-string ()
  "测试 CSS 属性转字符串。"
  (let ((css '((display . "flex") (padding . "1rem"))))
    (let ((str (etaf-tailwind-css-to-string css)))
      (should (string-match "display: flex" str))
      (should (string-match "padding: 1rem" str)))))

;;; CSSOM 集成测试

(ert-deftest etaf-tailwind-test-cssom-integration ()
  "测试 Tailwind 类在 CSSOM 中的正确解析。
Emacs特有：padding使用px（水平）和lh（垂直）。"
  (require 'etaf-tml)
  (require 'etaf-css)
  (let* ((dom (etaf-etml-to-dom
               '(div :class "flex bg-red-500 p-4"
                     (span :class "text-lg" "Hello"))))
         (cssom (etaf-css-build-cssom dom))
         (div-style (etaf-css-get-computed-style cssom dom dom)))
    ;; 验证 div 的 Tailwind 样式被正确解析
    (should (equal (cdr (assq 'display div-style)) "flex"))
    (should (equal (cdr (assq 'background-color div-style)) "#ef4444"))
    ;; padding 应该被展开为 padding-top, padding-right, etc.
    ;; 垂直方向使用 lh，水平方向使用 px
    (should (equal (cdr (assq 'padding-top div-style)) "4lh"))
    (should (equal (cdr (assq 'padding-right div-style)) "4px"))))

(ert-deftest etaf-tailwind-test-border-cssom-integration ()
  "测试 Tailwind 边框类在 CSSOM 中的正确展开和解析。
这是修复 border-1 border-red-500 问题的关键测试。"
  (require 'etaf-tml)
  (require 'etaf-css)
  (let* ((dom (etaf-etml-to-dom
               '(div :class "border-1 border-red-500"
                     "test content")))
         (cssom (etaf-css-build-cssom dom))
         (div-style (etaf-css-get-computed-style cssom dom dom)))
    ;; 验证 border-width 被正确展开为各方向的 border-*-width
    (should (equal (cdr (assq 'border-top-width div-style)) "1px"))
    (should (equal (cdr (assq 'border-right-width div-style)) "1px"))
    (should (equal (cdr (assq 'border-bottom-width div-style)) "1px"))
    (should (equal (cdr (assq 'border-left-width div-style)) "1px"))
    ;; 验证 border-color 被正确展开为各方向的 border-*-color
    (should (equal (cdr (assq 'border-top-color div-style)) "#ef4444"))
    (should (equal (cdr (assq 'border-right-color div-style)) "#ef4444"))
    (should (equal (cdr (assq 'border-bottom-color div-style)) "#ef4444"))
    (should (equal (cdr (assq 'border-left-color div-style)) "#ef4444"))))

(ert-deftest etaf-tailwind-test-render-tree-integration ()
  "测试 Tailwind 类在渲染树中的正确应用。"
  (require 'etaf-tml)
  (require 'etaf-css)
  (require 'etaf-render)
  (let* ((dom (etaf-etml-to-dom
               '(div :class "flex items-center justify-center"
                     (span :class "text-lg font-bold" "Title"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom)))
    ;; 验证渲染树包含正确的 Tailwind 样式
    (let ((render-style (dom-attr render-tree 'render-style)))
      (should (equal (cdr (assq 'display render-style)) "flex"))
      (should (equal (cdr (assq 'align-items render-style)) "center"))
      (should (equal (cdr (assq 'justify-content render-style)) "center")))))

(provide 'etaf-tailwind-tests)

;;; etaf-tailwind-tests.el ends here
