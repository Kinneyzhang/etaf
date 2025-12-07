;;; etaf-tailwind-example.el --- Tailwind CSS support example -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; This file demonstrates the Tailwind CSS support for ETAF.
;;
;; 主要功能演示：
;; - Tailwind 类名解析
;; - 类名验证
;; - 变体和响应式前缀支持
;; - Tailwind 到 CSS 转换
;; - DOM 集成操作

;;; Code:

(require 'etaf-tailwind)
(require 'etaf-dom)

;;; Example 1: Class Parsing

(defun etaf-tailwind-example-parsing ()
  "演示 Tailwind 类名解析。"
  (interactive)
  (message "=== Tailwind Class Parsing Example ===\n")
  
  ;; 简单类名
  (let ((result (etaf-tailwind-parse-class "bg-red-500")))
    (message "Parsing 'bg-red-500':")
    (message "  Variants: %S" (plist-get result :variants))
    (message "  Utility: %S" (plist-get result :utility))
    (message "  Property: %S" (plist-get result :property))
    (message "  Value: %S\n" (plist-get result :value)))
  
  ;; 带变体的类名
  (let ((result (etaf-tailwind-parse-class "md:hover:bg-blue-500")))
    (message "Parsing 'md:hover:bg-blue-500':")
    (message "  Variants: %S" (plist-get result :variants))
    (message "  Utility: %S" (plist-get result :utility))
    (message "  Property: %S" (plist-get result :property))
    (message "  Value: %S\n" (plist-get result :value)))
  
  ;; 任意值语法
  (let ((result (etaf-tailwind-parse-class "bg-[#1da1f2]")))
    (message "Parsing 'bg-[#1da1f2]' (arbitrary value):")
    (message "  Property: %S" (plist-get result :property))
    (message "  Arbitrary: %S\n" (plist-get result :arbitrary))))

;;; Example 2: Class Validation

(defun etaf-tailwind-example-validation ()
  "演示 Tailwind 类名验证。"
  (interactive)
  (message "=== Tailwind Class Validation Example ===\n")
  
  (let ((classes '("bg-red-500" "hover:text-lg" "md:flex" "p-4"
                   "invalid-xyz" "foo-bar-baz" "text-center")))
    (dolist (class classes)
      (message "'%s' is %s"
               class
               (if (etaf-tailwind-class-p class)
                   "a valid Tailwind class"
                 "NOT a valid Tailwind class")))
    (message "")))

;;; Example 3: Variant Detection

(defun etaf-tailwind-example-variants ()
  "演示变体检测功能。"
  (interactive)
  (message "=== Tailwind Variant Detection Example ===\n")
  
  (let ((classes '("bg-red-500" "hover:bg-blue-500" "md:text-lg"
                   "md:hover:bg-green-500" "dark:bg-gray-800")))
    (dolist (class classes)
      (message "'%s':" class)
      (message "  Variants: %S" (etaf-tailwind-get-variants class))
      (message "  Has responsive: %S" (etaf-tailwind-has-responsive-p class))
      (message "  Has state variant: %S" (etaf-tailwind-has-state-variant-p class)))
    (message "")))

;;; Example 4: CSS Conversion

(defun etaf-tailwind-example-css-conversion ()
  "演示 Tailwind 到 CSS 转换。"
  (interactive)
  (message "=== Tailwind to CSS Conversion Example ===\n")
  
  (let ((classes '("flex" "bg-red-500" "p-4" "px-2" "py-3"
                   "text-lg" "rounded-lg" "shadow-md"
                   "justify-center" "items-center"
                   "w-full" "h-screen")))
    (dolist (class classes)
      (let ((css (etaf-tailwind-to-css class)))
        (message "'%s' -> %S" class css)))
    (message "")))

;;; Example 5: Multiple Classes to CSS

(defun etaf-tailwind-example-multiple-classes ()
  "演示多个 Tailwind 类转换为 CSS。"
  (interactive)
  (message "=== Multiple Classes to CSS Example ===\n")
  
  (let ((class-string "flex items-center justify-between p-4 bg-white rounded-lg shadow-md"))
    (message "Classes: %s\n" class-string)
    
    (let ((css (etaf-tailwind-classes-to-css class-string)))
      (message "Generated CSS properties:")
      (dolist (prop css)
        (message "  %s: %s" (car prop) (cdr prop)))
      
      (message "\nCSS string:")
      (message "  %s" (etaf-tailwind-css-to-string css)))))

;;; Example 6: DOM Integration

(defun etaf-tailwind-example-dom-integration ()
  "演示 DOM 集成操作。"
  (interactive)
  (message "=== Tailwind DOM Integration Example ===\n")
  
  ;; 创建一个 DOM 节点
  (let ((node '(div ((class . "container")) "Hello World")))
    (message "Initial node: %S\n" node)
    
    ;; 添加 Tailwind 类
    (etaf-tailwind-add-class node "flex")
    (message "After adding 'flex': %S" node)
    
    (etaf-tailwind-add-class node "items-center")
    (message "After adding 'items-center': %S" node)
    
    (etaf-tailwind-add-class node "bg-blue-500")
    (message "After adding 'bg-blue-500': %S\n" node)
    
    ;; 检查类
    (message "Has 'flex': %S" (etaf-dom-has-class node "flex"))
    (message "Has 'p-4': %S\n" (etaf-dom-has-class node "p-4"))
    
    ;; 切换类
    (etaf-tailwind-toggle-class node "p-4")
    (message "After toggling 'p-4' (add): %S" node)
    
    (etaf-tailwind-toggle-class node "p-4")
    (message "After toggling 'p-4' (remove): %S\n" node)
    
    ;; 替换类
    (etaf-tailwind-replace-class node "bg-blue-500" "bg-red-500")
    (message "After replacing 'bg-blue-500' with 'bg-red-500': %S\n" node)
    
    ;; 移除类
    (etaf-tailwind-remove-class node "items-center")
    (message "After removing 'items-center': %S" node)))

;;; Example 7: Query by Tailwind Class

(defun etaf-tailwind-example-query ()
  "演示按 Tailwind 类查询 DOM。"
  (interactive)
  (message "=== Tailwind DOM Query Example ===\n")
  
  (let ((dom '(div ((class . "container"))
               (div ((class . "flex items-center"))
                 (span ((class . "text-lg font-bold")) "Title")
                 (span ((class . "text-sm text-gray-500")) "Subtitle"))
               (div ((class . "flex flex-col"))
                 (p ((class . "mb-2")) "Paragraph 1")
                 (p ((class . "mb-2 text-red-500")) "Paragraph 2")))))
    
    (message "DOM structure: %S\n" dom)
    
    ;; 查询带有 'flex' 类的节点
    (let ((flex-nodes (etaf-dom-query-tailwind dom "flex")))
      (message "Nodes with 'flex' class: %S\n" flex-nodes))
    
    ;; 使用模式查询
    (let ((text-nodes (etaf-dom-query-tailwind-pattern dom "^text-")))
      (message "Nodes matching '^text-' pattern: %S\n" text-nodes))))

;;; Example 8: Class Description

(defun etaf-tailwind-example-describe ()
  "演示 Tailwind 类描述功能。"
  (interactive)
  (message "=== Tailwind Class Description Example ===\n")
  
  (let ((classes '("bg-red-500" "md:hover:text-lg" "p-4" "flex")))
    (dolist (class classes)
      (message "%s\n" (etaf-tailwind-describe-class class)))))

;;; Example 9: Filter Classes

(defun etaf-tailwind-example-filter ()
  "演示过滤类列表功能。"
  (interactive)
  (message "=== Tailwind Filter Classes Example ===\n")
  
  (let ((all-classes '("flex" "custom-class" "p-4" "bg-red-500"
                       "my-component" "text-lg" "invalid-xyz")))
    (message "All classes: %S\n" all-classes)
    
    ;; 只保留有效的 Tailwind 类
    (let ((valid (etaf-tailwind-filter-classes all-classes)))
      (message "Valid Tailwind classes: %S\n" valid))
    
    ;; 只保留背景相关的类
    (let ((bg-classes (etaf-tailwind-filter-classes
                       all-classes
                       (lambda (parsed)
                         (string= (plist-get parsed :property) "bg")))))
      (message "Background classes: %S" bg-classes))))

;;; Example 10: Get Classes by Property

(defun etaf-tailwind-example-by-property ()
  "演示按属性获取类功能。"
  (interactive)
  (message "=== Get Classes by Property Example ===\n")
  
  (let ((node '(div ((class . "flex bg-red-500 bg-opacity-50 text-white text-lg p-4 m-2"))
                "Content")))
    (message "Node: %S\n" node)
    
    (message "Background classes: %S"
             (etaf-tailwind-get-classes-by-property node "bg"))
    (message "Text classes: %S"
             (etaf-tailwind-get-classes-by-property node "text"))
    (message "Padding classes: %S"
             (etaf-tailwind-get-classes-by-property node "p"))
    (message "Margin classes: %S"
             (etaf-tailwind-get-classes-by-property node "m"))))

;;; Example 11: Intrinsic Width Utilities

(defun etaf-tailwind-example-intrinsic-widths ()
  "演示内容尺寸工具类（w-fit, w-min, w-max）。"
  (interactive)
  (message "=== Intrinsic Width Utilities Example ===\n")
  
  (let ((width-classes '("w-fit" "w-min" "w-max"
                         "min-w-fit" "min-w-min" "min-w-max"
                         "max-w-fit" "max-w-min" "max-w-max"
                         "h-fit" "h-min" "h-max")))
    (message "These utilities allow elements to size based on their content:\n")
    (message "  - w-fit/h-fit: 适应内容大小 (fit-content)")
    (message "  - w-min/h-min: 收缩到最小内容宽度/高度 (min-content)")
    (message "  - w-max/h-max: 扩展到最大内容宽度/高度 (max-content)\n")
    
    (dolist (class width-classes)
      (let ((css (etaf-tailwind-to-css class)))
        (message "'%s' -> %S" class css)))
    (message "\n")
    
    ;; 实际应用示例
    (message "实际应用示例:")
    (message "  <button :class=\"w-fit\">适应按钮文本的宽度</button>")
    (message "  <div :class=\"w-min\">收缩到最小内容宽度</div>")
    (message "  <div :class=\"w-max\">扩展到最大内容宽度</div>")))

;;; Run All Examples

(defun etaf-tailwind-run-all-examples ()
  "运行所有 Tailwind 示例。"
  (interactive)
  (etaf-tailwind-example-parsing)
  (etaf-tailwind-example-validation)
  (etaf-tailwind-example-variants)
  (etaf-tailwind-example-css-conversion)
  (etaf-tailwind-example-multiple-classes)
  (etaf-tailwind-example-dom-integration)
  (etaf-tailwind-example-query)
  (etaf-tailwind-example-describe)
  (etaf-tailwind-example-filter)
  (etaf-tailwind-example-by-property)
  (etaf-tailwind-example-intrinsic-widths)
  (message "\n=== All Tailwind Examples Complete ==="))

;; To run all examples interactively, use M-x etaf-tailwind-run-all-examples
;; or evaluate (etaf-tailwind-run-all-examples) manually.

(provide 'etaf-tailwind-example)
;;; etaf-tailwind-example.el ends here
