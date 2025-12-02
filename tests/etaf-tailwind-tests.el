;;; etaf-tailwind-tests.el --- Tests for etaf-tailwind.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Keywords: test, tailwind, css

;;; Commentary:

;; 测试 etaf-tailwind.el 的功能

;;; Code:

(require 'etaf-tailwind)
(require 'etaf-ecss)
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
Emacs特有：水平方向使用cw（字符宽度），垂直方向使用lh。
如果需要像素，需要使用px后缀（如 px-4px）。"
  ;; p-4 expands to all four directions with appropriate units
  (should-equal (etaf-tailwind-to-css "p-4")
                '((padding-top . "4lh") (padding-right . "4cw")
                  (padding-bottom . "4lh") (padding-left . "4cw")))
  ;; px-2 is horizontal only - uses cw
  (should-equal (etaf-tailwind-to-css "px-2")
                '((padding-left . "2cw") (padding-right . "2cw")))
  ;; py-2 is vertical only - uses lh
  (should-equal (etaf-tailwind-to-css "py-2")
                '((padding-top . "2lh") (padding-bottom . "2lh")))
  ;; px-2px uses explicit pixels
  (should-equal (etaf-tailwind-to-css "px-2px")
                '((padding-left . "2px") (padding-right . "2px"))))

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

(ert-deftest etaf-tailwind-test-to-css-border-width-directional ()
  "测试方向性边框宽度转换 (border-x, border-y, border-t, etc.)。"
  ;; border-x default (1px)
  (should-equal (etaf-tailwind-to-css "border-x")
                '((border-left-width . "1px") (border-right-width . "1px")))
  ;; border-y default (1px)
  (should-equal (etaf-tailwind-to-css "border-y")
                '((border-top-width . "1px") (border-bottom-width . "1px")))
  ;; Individual sides default (1px)
  (should-equal (etaf-tailwind-to-css "border-t")
                '((border-top-width . "1px")))
  (should-equal (etaf-tailwind-to-css "border-r")
                '((border-right-width . "1px")))
  (should-equal (etaf-tailwind-to-css "border-b")
                '((border-bottom-width . "1px")))
  (should-equal (etaf-tailwind-to-css "border-l")
                '((border-left-width . "1px")))
  ;; Logical sides default (1px)
  (should-equal (etaf-tailwind-to-css "border-s")
                '((border-inline-start-width . "1px")))
  (should-equal (etaf-tailwind-to-css "border-e")
                '((border-inline-end-width . "1px")))
  ;; With explicit width values
  (should-equal (etaf-tailwind-to-css "border-x-2")
                '((border-left-width . "2px") (border-right-width . "2px")))
  (should-equal (etaf-tailwind-to-css "border-y-4")
                '((border-top-width . "4px") (border-bottom-width . "4px")))
  (should-equal (etaf-tailwind-to-css "border-t-0")
                '((border-top-width . "0px")))
  (should-equal (etaf-tailwind-to-css "border-r-8")
                '((border-right-width . "8px")))
  (should-equal (etaf-tailwind-to-css "border-b-2")
                '((border-bottom-width . "2px")))
  (should-equal (etaf-tailwind-to-css "border-l-4")
                '((border-left-width . "4px")))
  (should-equal (etaf-tailwind-to-css "border-s-2")
                '((border-inline-start-width . "2px")))
  (should-equal (etaf-tailwind-to-css "border-e-4")
                '((border-inline-end-width . "4px"))))

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
Emacs特有：padding使用cw（水平字符宽度）和lh（垂直行高）。"
  (require 'etaf-etml)
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
    ;; 垂直方向使用 lh，水平方向使用 cw
    (should (equal (cdr (assq 'padding-top div-style)) "4lh"))
    (should (equal (cdr (assq 'padding-right div-style)) "4cw"))))

(ert-deftest etaf-tailwind-test-border-cssom-integration ()
  "测试 Tailwind 边框类在 CSSOM 中的正确展开和解析。
这是修复 border-1 border-red-500 问题的关键测试。"
  (require 'etaf-etml)
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
  (require 'etaf-etml)
  (require 'etaf-css)
  (require 'etaf-render)
  (let* ((dom (etaf-etml-to-dom
               '(div :class "flex items-center justify-center"
                     (span :class "text-lg font-bold" "Title"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom)))
    ;; 验证渲染树包含正确的 Tailwind 样式
    (let ((computed-style (dom-attr render-tree 'computed-style)))
      (should (equal (cdr (assq 'display computed-style)) "flex"))
      (should (equal (cdr (assq 'align-items computed-style)) "center"))
      (should (equal (cdr (assq 'justify-content computed-style)) "center")))))

;;; 新增：扩展的 Tailwind 工具类测试

(ert-deftest etaf-tailwind-test-to-css-typography ()
  "测试字体排版相关转换。"
  ;; 斜体
  (should-equal (etaf-tailwind-to-css "italic")
                '((font-style . "italic")))
  (should-equal (etaf-tailwind-to-css "not-italic")
                '((font-style . "normal")))
  ;; 文本变换
  (should-equal (etaf-tailwind-to-css "uppercase")
                '((text-transform . "uppercase")))
  (should-equal (etaf-tailwind-to-css "lowercase")
                '((text-transform . "lowercase")))
  (should-equal (etaf-tailwind-to-css "capitalize")
                '((text-transform . "capitalize")))
  (should-equal (etaf-tailwind-to-css "normal-case")
                '((text-transform . "none")))
  ;; 文本装饰
  (should-equal (etaf-tailwind-to-css "underline")
                '((text-decoration-line . "underline")))
  (should-equal (etaf-tailwind-to-css "line-through")
                '((text-decoration-line . "line-through")))
  (should-equal (etaf-tailwind-to-css "no-underline")
                '((text-decoration-line . "none"))))

(ert-deftest etaf-tailwind-test-to-css-leading ()
  "测试行高 (leading) 转换。"
  (should-equal (etaf-tailwind-to-css "leading-none")
                '((line-height . "1")))
  (should-equal (etaf-tailwind-to-css "leading-tight")
                '((line-height . "1.25")))
  (should-equal (etaf-tailwind-to-css "leading-normal")
                '((line-height . "1.5")))
  (should-equal (etaf-tailwind-to-css "leading-loose")
                '((line-height . "2"))))

(ert-deftest etaf-tailwind-test-to-css-tracking ()
  "测试字符间距 (tracking) 转换。"
  (should-equal (etaf-tailwind-to-css "tracking-tighter")
                '((letter-spacing . "-0.05em")))
  (should-equal (etaf-tailwind-to-css "tracking-tight")
                '((letter-spacing . "-0.025em")))
  (should-equal (etaf-tailwind-to-css "tracking-normal")
                '((letter-spacing . "0em")))
  (should-equal (etaf-tailwind-to-css "tracking-wide")
                '((letter-spacing . "0.025em"))))

(ert-deftest etaf-tailwind-test-to-css-position ()
  "测试定位属性转换。
水平方向默认使用cw（字符宽度），使用px后缀指定像素。"
  ;; 位置值 - 现在使用 cw 作为默认单位
  (should-equal (etaf-tailwind-to-css "top-0")
                '((top . "0cw")))
  (should-equal (etaf-tailwind-to-css "right-4")
                '((right . "4cw")))
  (should-equal (etaf-tailwind-to-css "left-auto")
                '((left . "auto")))
  (should-equal (etaf-tailwind-to-css "inset-0")
                '((top . "0cw") (right . "0cw") (bottom . "0cw") (left . "0cw")))
  ;; 使用px后缀指定像素
  (should-equal (etaf-tailwind-to-css "top-10px")
                '((top . "10px"))))

(ert-deftest etaf-tailwind-test-to-css-flex-grid ()
  "测试 Flexbox 和 Grid 属性转换。"
  ;; Flex
  (should-equal (etaf-tailwind-to-css "grow")
                '((flex-grow . "1")))
  (should-equal (etaf-tailwind-to-css "grow-0")
                '((flex-grow . "0")))
  (should-equal (etaf-tailwind-to-css "shrink")
                '((flex-shrink . "1")))
  (should-equal (etaf-tailwind-to-css "basis-auto")
                '((flex-basis . "auto")))
  ;; Order
  (should-equal (etaf-tailwind-to-css "order-first")
                '((order . "-9999")))
  (should-equal (etaf-tailwind-to-css "order-last")
                '((order . "9999")))
  ;; Grid
  (should-equal (etaf-tailwind-to-css "cols-3")
                '((grid-template-columns . "repeat(3, minmax(0, 1fr))")))
  (should-equal (etaf-tailwind-to-css "col-span-2")
                '((grid-column . "span 2 / span 2")))
  (should-equal (etaf-tailwind-to-css "col-span-full")
                '((grid-column . "1 / -1"))))

(ert-deftest etaf-tailwind-test-to-css-gap ()
  "测试 gap 属性转换。
column-gap使用cw（字符宽度），row-gap使用lh（行高）。"
  (let ((gap-result (etaf-tailwind-to-css "gap-4")))
    (should (equal (cdr (assq 'column-gap gap-result)) "4cw"))
    (should (equal (cdr (assq 'row-gap gap-result)) "4lh")))
  (should-equal (etaf-tailwind-to-css "gap-x-4")
                '((column-gap . "4cw")))
  (should-equal (etaf-tailwind-to-css "gap-y-4")
                '((row-gap . "4lh")))
  ;; 使用px后缀指定像素
  (should-equal (etaf-tailwind-to-css "gap-x-10px")
                '((column-gap . "10px"))))

(ert-deftest etaf-tailwind-test-to-css-content-self ()
  "测试 content 和 self 对齐属性转换。"
  (should-equal (etaf-tailwind-to-css "content-center")
                '((align-content . "center")))
  (should-equal (etaf-tailwind-to-css "content-between")
                '((align-content . "space-between")))
  (should-equal (etaf-tailwind-to-css "self-center")
                '((align-self . "center")))
  (should-equal (etaf-tailwind-to-css "self-start")
                '((align-self . "flex-start"))))

(ert-deftest etaf-tailwind-test-to-css-min-max-size ()
  "测试 min/max 尺寸属性转换。
min-width/max-width使用cw（字符宽度）。"
  (should-equal (etaf-tailwind-to-css "min-w-0")
                '((min-width . "0cw")))
  (should-equal (etaf-tailwind-to-css "min-w-full")
                '((min-width . "100%")))
  (should-equal (etaf-tailwind-to-css "max-w-md")
                '((max-width . "28rem")))
  (should-equal (etaf-tailwind-to-css "max-w-prose")
                '((max-width . "65ch")))
  (should-equal (etaf-tailwind-to-css "min-h-full")
                '((min-height . "100%")))
  (should-equal (etaf-tailwind-to-css "max-h-full")
                '((max-height . "100%")))
  ;; 使用px后缀指定像素
  (should-equal (etaf-tailwind-to-css "min-w-100px")
                '((min-width . "100px"))))

(ert-deftest etaf-tailwind-test-to-css-size ()
  "测试 size 属性转换（同时设置 width 和 height）。
width使用cw，height使用lh。"
  (let ((size-result (etaf-tailwind-to-css "size-4")))
    (should (equal (cdr (assq 'width size-result)) "4cw"))
    (should (equal (cdr (assq 'height size-result)) "4lh")))
  (let ((size-full (etaf-tailwind-to-css "size-full")))
    (should (equal (cdr (assq 'width size-full)) "100%"))
    (should (equal (cdr (assq 'height size-full)) "100%"))))

(ert-deftest etaf-tailwind-test-to-css-logical-spacing ()
  "测试逻辑间距属性转换 (ps, pe, ms, me)。
水平方向使用cw（字符宽度）。"
  (should-equal (etaf-tailwind-to-css "ps-4")
                '((padding-inline-start . "4cw")))
  (should-equal (etaf-tailwind-to-css "pe-4")
                '((padding-inline-end . "4cw")))
  (should-equal (etaf-tailwind-to-css "ms-4")
                '((margin-inline-start . "4cw")))
  (should-equal (etaf-tailwind-to-css "me-4")
                '((margin-inline-end . "4cw")))
  ;; 使用px后缀指定像素
  (should-equal (etaf-tailwind-to-css "ps-10px")
                '((padding-inline-start . "10px"))))

(ert-deftest etaf-tailwind-test-to-css-table ()
  "测试表格属性转换。"
  (should-equal (etaf-tailwind-to-css "table-auto")
                '((table-layout . "auto")))
  (should-equal (etaf-tailwind-to-css "table-fixed")
                '((table-layout . "fixed")))
  (should-equal (etaf-tailwind-to-css "border-collapse")
                '((border-collapse . "collapse")))
  (should-equal (etaf-tailwind-to-css "border-separate")
                '((border-collapse . "separate"))))

(ert-deftest etaf-tailwind-test-to-css-cursor ()
  "测试光标属性转换。"
  (should-equal (etaf-tailwind-to-css "cursor-pointer")
                '((cursor . "pointer")))
  (should-equal (etaf-tailwind-to-css "cursor-wait")
                '((cursor . "wait")))
  (should-equal (etaf-tailwind-to-css "cursor-not-allowed")
                '((cursor . "not-allowed"))))

(ert-deftest etaf-tailwind-test-to-css-select ()
  "测试用户选择属性转换。"
  (should-equal (etaf-tailwind-to-css "select-none")
                '((user-select . "none")))
  (should-equal (etaf-tailwind-to-css "select-text")
                '((user-select . "text")))
  (should-equal (etaf-tailwind-to-css "select-all")
                '((user-select . "all"))))

(ert-deftest etaf-tailwind-test-to-css-truncate ()
  "测试 truncate 属性转换。"
  (let ((truncate-result (etaf-tailwind-to-css "truncate")))
    (should (equal (cdr (assq 'overflow truncate-result)) "hidden"))
    (should (equal (cdr (assq 'text-overflow truncate-result)) "ellipsis"))
    (should (equal (cdr (assq 'white-space truncate-result)) "nowrap"))))

(ert-deftest etaf-tailwind-test-to-css-sr-only ()
  "测试屏幕阅读器专用类转换。"
  (let ((sr-result (etaf-tailwind-to-css "sr-only")))
    (should (equal (cdr (assq 'position sr-result)) "absolute"))
    (should (equal (cdr (assq 'width sr-result)) "1px"))
    (should (equal (cdr (assq 'height sr-result)) "1px"))))

(ert-deftest etaf-tailwind-test-font-family ()
  "测试字体族转换。"
  (should-equal (etaf-tailwind-to-css "font-sans")
                '((font-family . "ui-sans-serif, system-ui, sans-serif")))
  (should-equal (etaf-tailwind-to-css "font-serif")
                '((font-family . "ui-serif, Georgia, serif")))
  (should-equal (etaf-tailwind-to-css "font-mono")
                '((font-family . "ui-monospace, monospace"))))

(ert-deftest etaf-tailwind-test-numeric-font-size ()
  "测试数值型字体大小转换 (text-N.N → font-size: N.Nlh)。"
  ;; Integer font size
  (should-equal (etaf-tailwind-to-css "text-1")
                '((font-size . "1lh")))
  (should-equal (etaf-tailwind-to-css "text-2")
                '((font-size . "2lh")))
  ;; Decimal font size
  (should-equal (etaf-tailwind-to-css "text-1.6")
                '((font-size . "1.6lh")))
  (should-equal (etaf-tailwind-to-css "text-1.4")
                '((font-size . "1.4lh")))
  (should-equal (etaf-tailwind-to-css "text-1.3")
                '((font-size . "1.3lh")))
  (should-equal (etaf-tailwind-to-css "text-1.2")
                '((font-size . "1.2lh")))
  (should-equal (etaf-tailwind-to-css "text-1.1")
                '((font-size . "1.1lh")))
  (should-equal (etaf-tailwind-to-css "text-1.0")
                '((font-size . "1.0lh")))
  (should-equal (etaf-tailwind-to-css "text-0.875")
                '((font-size . "0.875lh")))
  ;; Large sizes
  (should-equal (etaf-tailwind-to-css "text-3")
                '((font-size . "3lh")))
  (should-equal (etaf-tailwind-to-css "text-10")
                '((font-size . "10lh"))))

;;; 新增: 完整的 Tailwind 颜色测试

(ert-deftest etaf-tailwind-test-all-color-families ()
  "测试所有 Tailwind CSS 颜色族是否支持。"
  ;; 灰色系
  (should-equal (etaf-tailwind-to-css "bg-zinc-500")
                '((background-color . "#71717a")))
  (should-equal (etaf-tailwind-to-css "bg-neutral-500")
                '((background-color . "#737373")))
  (should-equal (etaf-tailwind-to-css "bg-stone-500")
                '((background-color . "#78716c")))
  ;; 黄色系
  (should-equal (etaf-tailwind-to-css "bg-amber-500")
                '((background-color . "#f59e0b")))
  ;; 绿色系
  (should-equal (etaf-tailwind-to-css "bg-lime-500")
                '((background-color . "#84cc16")))
  (should-equal (etaf-tailwind-to-css "bg-emerald-500")
                '((background-color . "#10b981")))
  (should-equal (etaf-tailwind-to-css "bg-teal-500")
                '((background-color . "#14b8a6")))
  ;; 蓝色系
  (should-equal (etaf-tailwind-to-css "bg-cyan-500")
                '((background-color . "#06b6d4")))
  (should-equal (etaf-tailwind-to-css "bg-sky-500")
                '((background-color . "#0ea5e9")))
  ;; 紫色系
  (should-equal (etaf-tailwind-to-css "bg-violet-500")
                '((background-color . "#8b5cf6")))
  (should-equal (etaf-tailwind-to-css "bg-fuchsia-500")
                '((background-color . "#d946ef")))
  ;; 粉色系
  (should-equal (etaf-tailwind-to-css "bg-rose-500")
                '((background-color . "#f43f5e"))))

(ert-deftest etaf-tailwind-test-color-shades ()
  "测试颜色的所有色阶（50-950）。"
  ;; 测试 cyan 的所有色阶
  (should-equal (etaf-tailwind-to-css "text-cyan-50")
                '((color . "#ecfeff")))
  (should-equal (etaf-tailwind-to-css "text-cyan-100")
                '((color . "#cffafe")))
  (should-equal (etaf-tailwind-to-css "text-cyan-200")
                '((color . "#a5f3fc")))
  (should-equal (etaf-tailwind-to-css "text-cyan-300")
                '((color . "#67e8f9")))
  (should-equal (etaf-tailwind-to-css "text-cyan-400")
                '((color . "#22d3ee")))
  (should-equal (etaf-tailwind-to-css "text-cyan-500")
                '((color . "#06b6d4")))
  (should-equal (etaf-tailwind-to-css "text-cyan-600")
                '((color . "#0891b2")))
  (should-equal (etaf-tailwind-to-css "text-cyan-700")
                '((color . "#0e7490")))
  (should-equal (etaf-tailwind-to-css "text-cyan-800")
                '((color . "#155e75")))
  (should-equal (etaf-tailwind-to-css "text-cyan-900")
                '((color . "#164e63")))
  (should-equal (etaf-tailwind-to-css "text-cyan-950")
                '((color . "#083344"))))

(ert-deftest etaf-tailwind-test-border-colors ()
  "测试边框颜色（新增颜色）。"
  (should-equal (etaf-tailwind-to-css "border-emerald-500")
                '((border-color . "#10b981")))
  (should-equal (etaf-tailwind-to-css "border-teal-500")
                '((border-color . "#14b8a6")))
  (should-equal (etaf-tailwind-to-css "border-sky-500")
                '((border-color . "#0ea5e9")))
  (should-equal (etaf-tailwind-to-css "border-violet-500")
                '((border-color . "#8b5cf6"))))

;;; 新增: Dark mode 测试

(ert-deftest etaf-tailwind-test-dark-variant-detection ()
  "测试 dark 变体检测。"
  (should (etaf-tailwind-has-dark-variant-p "dark:bg-gray-800"))
  (should-not (etaf-tailwind-has-dark-variant-p "bg-white"))
  (should (etaf-tailwind-has-dark-variant-p "md:dark:bg-gray-800")))

(ert-deftest etaf-tailwind-test-class-applies-in-light-mode ()
  "测试亮色模式下类名应用规则。"
  ;; 在亮色模式下，非 dark 变体的类应该应用
  (should (etaf-tailwind-class-applies-p "bg-white" :light))
  (should (etaf-tailwind-class-applies-p "text-black" :light))
  ;; 在亮色模式下，dark 变体的类不应该应用
  (should-not (etaf-tailwind-class-applies-p "dark:bg-gray-800" :light))
  (should-not (etaf-tailwind-class-applies-p "dark:text-white" :light)))

(ert-deftest etaf-tailwind-test-class-applies-in-dark-mode ()
  "测试暗色模式下类名应用规则。"
  ;; 在暗色模式下，非 dark 变体的类应该应用
  (should (etaf-tailwind-class-applies-p "bg-white" :dark))
  (should (etaf-tailwind-class-applies-p "text-black" :dark))
  ;; 在暗色模式下，dark 变体的类应该应用
  (should (etaf-tailwind-class-applies-p "dark:bg-gray-800" :dark))
  (should (etaf-tailwind-class-applies-p "dark:text-white" :dark)))

(ert-deftest etaf-tailwind-test-filter-classes-by-mode ()
  "测试根据模式过滤类名列表。"
  (let ((classes "bg-white dark:bg-gray-800 text-black dark:text-white p-4"))
    ;; 亮色模式：只保留非 dark 变体的类
    (should-equal (etaf-tailwind-filter-classes-by-mode classes :light)
                  '("bg-white" "text-black" "p-4"))
    ;; 暗色模式：保留所有类（包括 dark 变体）
    (should-equal (etaf-tailwind-filter-classes-by-mode classes :dark)
                  '("bg-white" "dark:bg-gray-800" "text-black" "dark:text-white" "p-4"))))

(ert-deftest etaf-tailwind-test-classes-to-css-light-mode ()
  "测试亮色模式下的 CSS 转换。"
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "bg-white dark:bg-gray-800" :light)))
    ;; 亮色模式：应该使用 bg-white 的颜色
    (should (equal (cdr (assq 'background-color result)) "#ffffff"))))

(ert-deftest etaf-tailwind-test-classes-to-css-dark-mode ()
  "测试暗色模式下的 CSS 转换。"
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "bg-white dark:bg-gray-800" :dark)))
    ;; 暗色模式：dark:bg-gray-800 应该覆盖 bg-white
    (should (equal (cdr (assq 'background-color result)) "#1f2937"))))

(ert-deftest etaf-tailwind-test-dark-mode-override ()
  "测试暗色模式下 dark 变体类覆盖普通类的行为。"
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "text-gray-900 dark:text-gray-100" :dark)))
    ;; 暗色模式：dark:text-gray-100 应该覆盖 text-gray-900
    (should (equal (cdr (assq 'color result)) "#f3f4f6"))))

(ert-deftest etaf-tailwind-test-multiple-dark-properties ()
  "测试多个 dark 模式属性。"
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "bg-white dark:bg-slate-800 text-slate-900 dark:text-slate-100 border-slate-200 dark:border-slate-700"
                 :dark)))
    ;; 暗色模式：所有 dark 变体应该覆盖对应的普通类
    (should (equal (cdr (assq 'background-color result)) "#1e293b"))
    (should (equal (cdr (assq 'color result)) "#f1f5f9"))
    (should (equal (cdr (assq 'border-color result)) "#334155"))))

(ert-deftest etaf-tailwind-test-dark-variant-first-order ()
  "测试 dark 变体在输入中排在前面时仍能正确覆盖基础类。
这是问题 'dark:bg-gray-800 bg-white 在暗色模式下返回 #ffffff' 的修复测试。"
  ;; 在暗色模式下，无论输入顺序如何，dark:bg-gray-800 都应该生效
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "dark:bg-gray-800 bg-white" :dark)))
    (should (equal (cdr (assq 'background-color result)) "#1f2937")))
  ;; 在亮色模式下，dark:bg-gray-800 应该被忽略
  (let ((result (etaf-tailwind-classes-to-css-with-mode
                 "dark:bg-gray-800 bg-white" :light)))
    (should (equal (cdr (assq 'background-color result)) "#ffffff"))))

;;; 新增: Dual mode 测试

(ert-deftest etaf-tailwind-test-dual-mode-basic ()
  "测试 etaf-tailwind-classes-to-css-dual-mode 基本功能。"
  (let ((result (etaf-tailwind-classes-to-css-dual-mode "bg-white dark:bg-gray-800")))
    ;; 亮色模式应该使用 bg-white
    (should (equal (cdr (assq 'background-color (plist-get result :light))) "#ffffff"))
    ;; 暗色模式应该使用 bg-gray-800
    (should (equal (cdr (assq 'background-color (plist-get result :dark))) "#1f2937"))))

(ert-deftest etaf-tailwind-test-dual-mode-multiple-properties ()
  "测试 dual mode 多属性处理。"
  (let ((result (etaf-tailwind-classes-to-css-dual-mode
                 "bg-stone-600 dark:bg-gray-600 text-black dark:text-white")))
    ;; 亮色模式
    (should (equal (cdr (assq 'background-color (plist-get result :light))) "#57534e"))
    (should (equal (cdr (assq 'color (plist-get result :light))) "#000000"))
    ;; 暗色模式
    (should (equal (cdr (assq 'background-color (plist-get result :dark))) "#4b5563"))
    (should (equal (cdr (assq 'color (plist-get result :dark))) "#ffffff"))))

(ert-deftest etaf-tailwind-test-dual-mode-no-dark-variant ()
  "测试没有 dark 变体时的 dual mode 行为。"
  (let ((result (etaf-tailwind-classes-to-css-dual-mode "bg-red-500 text-white")))
    ;; 亮色和暗色模式应该相同
    (should (equal (plist-get result :light) (plist-get result :dark)))))

(ert-deftest etaf-tailwind-test-dual-mode-only-dark-variant ()
  "测试只有 dark 变体时的 dual mode 行为。"
  (let ((result (etaf-tailwind-classes-to-css-dual-mode "dark:bg-gray-800")))
    ;; 亮色模式应该没有背景色（dark变体不应用）
    (should (null (cdr (assq 'background-color (plist-get result :light)))))
    ;; 暗色模式应该有背景色
    (should (equal (cdr (assq 'background-color (plist-get result :dark))) "#1f2937"))))

;;; 新增: 水平距离单位测试 (cw 默认, px 后缀)

(ert-deftest etaf-tailwind-test-width-default-cw ()
  "测试 width 默认使用 cw（字符宽度）单位。
w-20 应该产生 20cw 而不是 20px。"
  (should-equal (etaf-tailwind-to-css "w-20")
                '((width . "20cw")))
  (should-equal (etaf-tailwind-to-css "w-4")
                '((width . "4cw")))
  (should-equal (etaf-tailwind-to-css "w-0")
                '((width . "0cw"))))

(ert-deftest etaf-tailwind-test-width-explicit-px ()
  "测试 width 使用 px 后缀指定像素。
w-20px 应该产生 20px。"
  (should-equal (etaf-tailwind-to-css "w-20px")
                '((width . "20px")))
  (should-equal (etaf-tailwind-to-css "w-100px")
                '((width . "100px")))
  (should-equal (etaf-tailwind-to-css "w-0px")
                '((width . "0px"))))

(ert-deftest etaf-tailwind-test-height-default-lh ()
  "测试 height 默认使用 lh（行高）单位。"
  (should-equal (etaf-tailwind-to-css "h-20")
                '((height . "20lh")))
  (should-equal (etaf-tailwind-to-css "h-4")
                '((height . "4lh"))))

(ert-deftest etaf-tailwind-test-margin-default-cw ()
  "测试 margin 水平方向默认使用 cw 单位。"
  (should-equal (etaf-tailwind-to-css "ml-4")
                '((margin-left . "4cw")))
  (should-equal (etaf-tailwind-to-css "mr-8")
                '((margin-right . "8cw")))
  (should-equal (etaf-tailwind-to-css "mx-4")
                '((margin-left . "4cw") (margin-right . "4cw"))))

(ert-deftest etaf-tailwind-test-margin-explicit-px ()
  "测试 margin 使用 px 后缀指定像素。"
  (should-equal (etaf-tailwind-to-css "ml-10px")
                '((margin-left . "10px")))
  (should-equal (etaf-tailwind-to-css "mx-20px")
                '((margin-left . "20px") (margin-right . "20px"))))

(ert-deftest etaf-tailwind-test-flex-basis-default-cw ()
  "测试 flex-basis 默认使用 cw 单位。"
  (should-equal (etaf-tailwind-to-css "basis-20")
                '((flex-basis . "20cw"))))

(ert-deftest etaf-tailwind-test-flex-basis-explicit-px ()
  "测试 flex-basis 使用 px 后缀指定像素。"
  (should-equal (etaf-tailwind-to-css "basis-100px")
                '((flex-basis . "100px"))))

(provide 'etaf-tailwind-tests)

;;; etaf-tailwind-tests.el ends here
