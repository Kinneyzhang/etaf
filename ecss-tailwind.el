;;; ecss-tailwind.el --- Tailwind CSS support for ECSS -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ECSS Contributors
;; Keywords: css, tailwind, utility-first
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这个库为ECSS添加Tailwind CSS支持。
;; Tailwind CSS是一个实用优先的CSS框架，使用大量的实用类来构建界面。
;;
;; 主要功能：
;; - 识别和解析Tailwind CSS类名模式
;; - 支持响应式前缀（sm:, md:, lg:, xl:, 2xl:）
;; - 支持状态变体（hover:, focus:, active:, disabled:等）
;; - 支持任意值语法（如 bg-[#1da1f2]）
;; - 提供Tailwind类名验证和匹配功能
;; - 为DOM节点添加和移除Tailwind类
;;
;; 使用示例：
;;
;;   ;; 检查是否为有效的Tailwind类
;;   (ecss-tailwind-class-p "bg-red-500")  ; => t
;;   (ecss-tailwind-class-p "hover:text-lg")  ; => t
;;
;;   ;; 解析Tailwind类
;;   (ecss-tailwind-parse-class "md:hover:bg-blue-500")
;;   ;; => (:variants ("md" "hover") :utility "bg-blue-500" :property "bg" :value "blue-500")
;;
;;   ;; 在DOM中查询具有特定Tailwind类的节点
;;   (ecss-dom-query-tailwind dom "flex")
;;   (ecss-dom-query-tailwind dom "bg-red-500")
;;
;;   ;; 添加Tailwind类到DOM节点
;;   (ecss-tailwind-add-class node "hover:bg-blue-500")

;;; Code:

(require 'cl-lib)
(require 'ecss-dom)

;;; Tailwind CSS patterns and utilities

(defconst ecss-tailwind-responsive-prefixes
  '("sm" "md" "lg" "xl" "2xl")
  "Tailwind CSS响应式断点前缀。")

(defconst ecss-tailwind-state-variants
  '("hover" "focus" "active" "visited" "disabled" "checked"
    "focus-within" "focus-visible" "group-hover" "group-focus"
    "first" "last" "odd" "even" "required" "invalid" "placeholder-shown"
    "before" "after" "selection" "marker" "file")
  "Tailwind CSS状态变体。")

(defconst ecss-tailwind-utility-prefixes
  '(;; Layout
    "container" "box" "block" "inline" "flex" "grid" "table" "hidden"
    "float" "clear" "object" "overflow" "overscroll" "position"
    "top" "right" "bottom" "left" "inset" "visible" "invisible" "z"
    
    ;; Flexbox & Grid
    "basis" "direction" "wrap" "grow" "shrink" "order"
    "cols" "col" "rows" "row" "gap" "justify" "content" "items" "self" "place"
    
    ;; Spacing
    "p" "px" "py" "pt" "pr" "pb" "pl" "ps" "pe"
    "m" "mx" "my" "mt" "mr" "mb" "ml" "ms" "me"
    "space"
    
    ;; Sizing
    "w" "min-w" "max-w" "h" "min-h" "max-h" "size"
    
    ;; Typography
    "font" "text" "leading" "tracking" "break" "hyphens"
    "antialiased" "subpixel-antialiased"
    "italic" "not-italic" "normal-case" "uppercase" "lowercase" "capitalize"
    "underline" "overline" "line-through" "no-underline"
    "indent" "align" "whitespace" "truncate"
    
    ;; Backgrounds
    "bg" "from" "via" "to" "gradient"
    
    ;; Borders
    "border" "divide" "outline" "ring"
    "rounded"
    
    ;; Effects
    "shadow" "opacity" "mix" "blur" "brightness" "contrast"
    "drop-shadow" "grayscale" "hue-rotate" "invert" "saturate" "sepia"
    "backdrop"
    
    ;; Filters
    "filter" "backdrop-filter"
    
    ;; Tables
    "border-collapse" "border-separate" "table-auto" "table-fixed"
    
    ;; Transitions & Animation
    "transition" "duration" "ease" "delay" "animate"
    
    ;; Transforms
    "scale" "rotate" "translate" "skew" "origin"
    
    ;; Interactivity
    "appearance" "cursor" "caret" "pointer-events" "resize"
    "scroll" "snap" "touch" "select" "will-change"
    
    ;; SVG
    "fill" "stroke"
    
    ;; Accessibility
    "sr")
  "Tailwind CSS实用类前缀列表。")

(defconst ecss-tailwind-standalone-utilities
  '("container" "flex" "grid" "block" "inline" "hidden" "visible" "invisible"
    "static" "fixed" "absolute" "relative" "sticky"
    "truncate" "italic" "not-italic" "antialiased" "subpixel-antialiased"
    "uppercase" "lowercase" "capitalize" "normal-case")
  "独立的Tailwind CSS实用类（不需要值）。")

;;; Tailwind class parsing

(defun ecss-tailwind-parse-class (class-name)
  "解析Tailwind CSS类名，返回其组成部分。
返回一个plist，包含：
  :variants - 变体列表（如响应式前缀和状态）
  :utility - 完整的实用类名（不含变体）
  :property - 属性名称（如'bg'从'bg-red-500'）
  :value - 值部分（如'red-500'从'bg-red-500'）
  :arbitrary - 如果使用任意值语法，包含方括号内的值

示例：
  (ecss-tailwind-parse-class \"md:hover:bg-red-500\")
  => (:variants (\"md\" \"hover\") :utility \"bg-red-500\" :property \"bg\" :value \"red-500\")"
  (let ((parts (split-string class-name ":"))
        (variants '())
        (utility nil)
        (property nil)
        (value nil)
        (arbitrary nil))
    
    ;; 分离变体和实用类
    (when (> (length parts) 1)
      (setq variants (butlast parts)
            utility (car (last parts)))
      ;; 过滤掉空字符串
      (setq variants (cl-remove-if #'string-empty-p variants)))
    
    (when (= (length parts) 1)
      (setq utility (car parts)))
    
    ;; 解析实用类
    (when utility
      ;; 检查任意值语法 [...]
      (if (string-match "\\(.*\\)\\[\\([^]]+\\)\\]" utility)
          (progn
            (setq property (match-string 1 utility)
                  arbitrary (match-string 2 utility))
            ;; 移除property末尾的 '-' 如果存在
            (when (string-suffix-p "-" property)
              (setq property (substring property 0 -1))))
        ;; 标准格式：property-value
        (let ((hyphen-pos (string-match "-" utility)))
          (if hyphen-pos
              (setq property (substring utility 0 hyphen-pos)
                    value (substring utility (1+ hyphen-pos)))
            ;; 独立实用类（无连字符）
            (setq property utility)))))
    
    (list :variants variants
          :utility utility
          :property property
          :value value
          :arbitrary arbitrary)))

(defun ecss-tailwind-class-p (class-name)
  "检查CLASS-NAME是否是有效的Tailwind CSS类名。
这是一个简化的验证，检查类名是否符合Tailwind的命名模式。"
  (when (and class-name (stringp class-name))
    (let* ((parsed (ecss-tailwind-parse-class class-name))
           (variants (plist-get parsed :variants))
           (property (plist-get parsed :property))
           (arbitrary (plist-get parsed :arbitrary)))
      
      ;; 检查变体是否有效
      (let ((valid-variants t))
        (dolist (variant variants)
          (unless (or (member variant ecss-tailwind-responsive-prefixes)
                      (member variant ecss-tailwind-state-variants)
                      ;; 允许dark和自定义变体
                      (string= variant "dark")
                      (string-prefix-p "group-" variant)
                      (string-prefix-p "peer-" variant))
            (setq valid-variants nil)))
        
        ;; 检查属性是否有效
        (and valid-variants
             (or
              ;; 独立实用类
              (member property ecss-tailwind-standalone-utilities)
              ;; 有前缀的实用类
              (cl-some (lambda (prefix)
                         (string-prefix-p prefix property))
                       ecss-tailwind-utility-prefixes)
              ;; 任意值
              arbitrary))))))

(defun ecss-tailwind-get-variants (class-name)
  "从Tailwind类名中提取所有变体。"
  (plist-get (ecss-tailwind-parse-class class-name) :variants))

(defun ecss-tailwind-get-utility (class-name)
  "从Tailwind类名中提取实用类部分（不含变体）。"
  (plist-get (ecss-tailwind-parse-class class-name) :utility))

(defun ecss-tailwind-get-property (class-name)
  "从Tailwind类名中提取属性名称。"
  (plist-get (ecss-tailwind-parse-class class-name) :property))

(defun ecss-tailwind-has-variant-p (class-name variant)
  "检查Tailwind类名是否包含指定的变体。"
  (member variant (ecss-tailwind-get-variants class-name)))

(defun ecss-tailwind-has-responsive-p (class-name)
  "检查Tailwind类名是否有响应式前缀。"
  (let ((variants (ecss-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v ecss-tailwind-responsive-prefixes))
             variants)))

(defun ecss-tailwind-has-state-variant-p (class-name)
  "检查Tailwind类名是否有状态变体。"
  (let ((variants (ecss-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v ecss-tailwind-state-variants))
             variants)))

;;; DOM integration

(defun ecss-dom-node-has-tailwind-class-p (node class-name)
  "检查DOM节点是否有指定的Tailwind类。
支持精确匹配和模式匹配。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs))))
      (when class-attr
        (let ((classes (split-string class-attr)))
          (member class-name classes))))))

(defun ecss-dom-query-tailwind (dom tailwind-class)
  "在DOM树中查询所有具有指定Tailwind类的节点。
TAILWIND-CLASS是要查询的Tailwind类名。

示例：
  (ecss-dom-query-tailwind dom \"flex\")
  (ecss-dom-query-tailwind dom \"bg-red-500\")
  (ecss-dom-query-tailwind dom \"md:hover:text-lg\")"
  (let ((results '()))
    (ecss-dom-walk
     (lambda (node)
       (when (ecss-dom-node-has-tailwind-class-p node tailwind-class)
         (push node results)))
     dom)
    (nreverse results)))

(defun ecss-dom-query-tailwind-pattern (dom pattern)
  "在DOM树中查询匹配Tailwind模式的节点。
PATTERN可以是：
  - 属性前缀，如'bg'匹配所有背景类
  - 变体，如'hover:'匹配所有hover变体
  - 正则表达式

示例：
  (ecss-dom-query-tailwind-pattern dom \"^bg-\")  ; 所有背景类
  (ecss-dom-query-tailwind-pattern dom \"^hover:\") ; 所有hover类"
  (let ((results '())
        (regexp (if (stringp pattern) pattern (regexp-quote pattern))))
    (ecss-dom-walk
     (lambda (node)
       (when (and node (listp node))
         (let* ((attrs (dom-attributes node))
                (class-attr (cdr (assq 'class attrs))))
           (when class-attr
             (let ((classes (split-string class-attr)))
               (when (cl-some (lambda (class)
                                (string-match-p regexp class))
                              classes)
                 (push node results)))))))
     dom)
    (nreverse results)))

(defun ecss-tailwind-add-class (node class-name)
  "为DOM节点添加Tailwind CSS类。
如果类名不是有效的Tailwind类，会发出警告但仍然添加。"
  (unless (ecss-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (ecss-dom-add-class node class-name))

(defun ecss-tailwind-remove-class (node class-name)
  "从DOM节点移除Tailwind CSS类。"
  (ecss-dom-remove-class node class-name))

(defun ecss-tailwind-toggle-class (node class-name)
  "切换DOM节点的Tailwind CSS类。"
  (unless (ecss-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (ecss-dom-toggle-class node class-name))

(defun ecss-tailwind-replace-class (node old-class new-class)
  "替换DOM节点的Tailwind类。"
  (when (ecss-dom-has-class node old-class)
    (ecss-dom-remove-class node old-class)
    (ecss-tailwind-add-class node new-class)))

(defun ecss-tailwind-get-classes-by-property (node property)
  "获取DOM节点中指定属性的所有Tailwind类。
例如，获取所有'bg'（背景）类或所有'text'（文本）类。"
  (when (and node (listp node))
    (let* ((attrs (dom-attributes node))
           (class-attr (cdr (assq 'class attrs)))
           (results '()))
      (when class-attr
        (let ((classes (split-string class-attr)))
          (dolist (class classes)
            (when (ecss-tailwind-class-p class)
              (let* ((parsed (ecss-tailwind-parse-class class))
                     (class-property (plist-get parsed :property)))
                (when (string= class-property property)
                  (push class results)))))))
      (nreverse results))))

(defun ecss-tailwind-filter-classes (classes &optional filter-fn)
  "过滤Tailwind类列表。
FILTER-FN是一个函数，接受解析后的类信息，返回t表示保留。
如果FILTER-FN为nil，只返回有效的Tailwind类。"
  (cl-remove-if-not
   (lambda (class)
     (and (ecss-tailwind-class-p class)
          (or (null filter-fn)
              (funcall filter-fn (ecss-tailwind-parse-class class)))))
   classes))

;;; Utility functions

(defun ecss-tailwind-describe-class (class-name)
  "描述Tailwind类的详细信息。
返回一个人类可读的描述字符串。"
  (let* ((parsed (ecss-tailwind-parse-class class-name))
         (variants (plist-get parsed :variants))
         (utility (plist-get parsed :utility))
         (property (plist-get parsed :property))
         (value (plist-get parsed :value))
         (arbitrary (plist-get parsed :arbitrary)))
    (format "Tailwind Class: %s\n  Variants: %s\n  Utility: %s\n  Property: %s\n  Value: %s\n  Arbitrary: %s\n  Valid: %s"
            class-name
            (if variants (mapconcat #'identity variants ", ") "none")
            (or utility "none")
            (or property "none")
            (or value "none")
            (or arbitrary "none")
            (if (ecss-tailwind-class-p class-name) "yes" "no"))))

;;; CSS conversion - Tailwind to native CSS

(defconst ecss-tailwind-color-palette
  '(;; Slate
    ("slate-50" . "#f8fafc") ("slate-100" . "#f1f5f9") ("slate-200" . "#e2e8f0")
    ("slate-300" . "#cbd5e1") ("slate-400" . "#94a3b8") ("slate-500" . "#64748b")
    ("slate-600" . "#475569") ("slate-700" . "#334155") ("slate-800" . "#1e293b")
    ("slate-900" . "#0f172a") ("slate-950" . "#020617")
    ;; Gray
    ("gray-50" . "#f9fafb") ("gray-100" . "#f3f4f6") ("gray-200" . "#e5e7eb")
    ("gray-300" . "#d1d5db") ("gray-400" . "#9ca3af") ("gray-500" . "#6b7280")
    ("gray-600" . "#4b5563") ("gray-700" . "#374151") ("gray-800" . "#1f2937")
    ("gray-900" . "#111827") ("gray-950" . "#030712")
    ;; Red
    ("red-50" . "#fef2f2") ("red-100" . "#fee2e2") ("red-200" . "#fecaca")
    ("red-300" . "#fca5a5") ("red-400" . "#f87171") ("red-500" . "#ef4444")
    ("red-600" . "#dc2626") ("red-700" . "#b91c1c") ("red-800" . "#991b1b")
    ("red-900" . "#7f1d1d") ("red-950" . "#450a0a")
    ;; Orange
    ("orange-50" . "#fff7ed") ("orange-100" . "#ffedd5") ("orange-200" . "#fed7aa")
    ("orange-300" . "#fdba74") ("orange-400" . "#fb923c") ("orange-500" . "#f97316")
    ("orange-600" . "#ea580c") ("orange-700" . "#c2410c") ("orange-800" . "#9a3412")
    ("orange-900" . "#7c2d12") ("orange-950" . "#431407")
    ;; Yellow
    ("yellow-50" . "#fefce8") ("yellow-100" . "#fef9c3") ("yellow-200" . "#fef08a")
    ("yellow-300" . "#fde047") ("yellow-400" . "#facc15") ("yellow-500" . "#eab308")
    ("yellow-600" . "#ca8a04") ("yellow-700" . "#a16207") ("yellow-800" . "#854d0e")
    ("yellow-900" . "#713f12") ("yellow-950" . "#422006")
    ;; Green
    ("green-50" . "#f0fdf4") ("green-100" . "#dcfce7") ("green-200" . "#bbf7d0")
    ("green-300" . "#86efac") ("green-400" . "#4ade80") ("green-500" . "#22c55e")
    ("green-600" . "#16a34a") ("green-700" . "#15803d") ("green-800" . "#166534")
    ("green-900" . "#14532d") ("green-950" . "#052e16")
    ;; Blue
    ("blue-50" . "#eff6ff") ("blue-100" . "#dbeafe") ("blue-200" . "#bfdbfe")
    ("blue-300" . "#93c5fd") ("blue-400" . "#60a5fa") ("blue-500" . "#3b82f6")
    ("blue-600" . "#2563eb") ("blue-700" . "#1d4ed8") ("blue-800" . "#1e40af")
    ("blue-900" . "#1e3a8a") ("blue-950" . "#172554")
    ;; Indigo
    ("indigo-50" . "#eef2ff") ("indigo-100" . "#e0e7ff") ("indigo-200" . "#c7d2fe")
    ("indigo-300" . "#a5b4fc") ("indigo-400" . "#818cf8") ("indigo-500" . "#6366f1")
    ("indigo-600" . "#4f46e5") ("indigo-700" . "#4338ca") ("indigo-800" . "#3730a3")
    ("indigo-900" . "#312e81") ("indigo-950" . "#1e1b4b")
    ;; Purple
    ("purple-50" . "#faf5ff") ("purple-100" . "#f3e8ff") ("purple-200" . "#e9d5ff")
    ("purple-300" . "#d8b4fe") ("purple-400" . "#c084fc") ("purple-500" . "#a855f7")
    ("purple-600" . "#9333ea") ("purple-700" . "#7e22ce") ("purple-800" . "#6b21a8")
    ("purple-900" . "#581c87") ("purple-950" . "#3b0764")
    ;; Pink
    ("pink-50" . "#fdf2f8") ("pink-100" . "#fce7f3") ("pink-200" . "#fbcfe8")
    ("pink-300" . "#f9a8d4") ("pink-400" . "#f472b6") ("pink-500" . "#ec4899")
    ("pink-600" . "#db2777") ("pink-700" . "#be185d") ("pink-800" . "#9d174d")
    ("pink-900" . "#831843") ("pink-950" . "#500724")
    ;; Common colors
    ("white" . "#ffffff") ("black" . "#000000")
    ("transparent" . "transparent") ("current" . "currentColor"))
  "Tailwind CSS颜色调色板。")

(defconst ecss-tailwind-spacing-scale
  '(("0" . "0px") ("px" . "1px")
    ("0.5" . "0.125rem") ("1" . "0.25rem") ("1.5" . "0.375rem")
    ("2" . "0.5rem") ("2.5" . "0.625rem") ("3" . "0.75rem")
    ("3.5" . "0.875rem") ("4" . "1rem") ("5" . "1.25rem")
    ("6" . "1.5rem") ("7" . "1.75rem") ("8" . "2rem")
    ("9" . "2.25rem") ("10" . "2.5rem") ("11" . "2.75rem")
    ("12" . "3rem") ("14" . "3.5rem") ("16" . "4rem")
    ("20" . "5rem") ("24" . "6rem") ("28" . "7rem")
    ("32" . "8rem") ("36" . "9rem") ("40" . "10rem")
    ("44" . "11rem") ("48" . "12rem") ("52" . "13rem")
    ("56" . "14rem") ("60" . "15rem") ("64" . "16rem")
    ("72" . "18rem") ("80" . "20rem") ("96" . "24rem"))
  "Tailwind CSS间距比例尺。")

(defconst ecss-tailwind-font-sizes
  '(("xs" . ("0.75rem" "1rem"))
    ("sm" . ("0.875rem" "1.25rem"))
    ("base" . ("1rem" "1.5rem"))
    ("lg" . ("1.125rem" "1.75rem"))
    ("xl" . ("1.25rem" "1.75rem"))
    ("2xl" . ("1.5rem" "2rem"))
    ("3xl" . ("1.875rem" "2.25rem"))
    ("4xl" . ("2.25rem" "2.5rem"))
    ("5xl" . ("3rem" "1"))
    ("6xl" . ("3.75rem" "1"))
    ("7xl" . ("4.5rem" "1"))
    ("8xl" . ("6rem" "1"))
    ("9xl" . ("8rem" "1")))
  "Tailwind CSS字体大小。值为 (font-size line-height)。")

(defun ecss-tailwind-to-css (class-name)
  "将Tailwind类名转换为原生CSS属性和值。
返回一个alist: ((property . value) ...)。
如果无法转换，返回nil。

示例：
  (ecss-tailwind-to-css \"bg-red-500\")
  ;; => ((background-color . \"#ef4444\"))
  
  (ecss-tailwind-to-css \"text-lg\")
  ;; => ((font-size . \"1.125rem\") (line-height . \"1.75rem\"))
  
  (ecss-tailwind-to-css \"p-4\")
  ;; => ((padding . \"1rem\"))"
  (let* ((parsed (ecss-tailwind-parse-class class-name))
         (property (plist-get parsed :property))
         (value (plist-get parsed :value))
         (arbitrary (plist-get parsed :arbitrary))
         (css-props '()))
    
    (when (or property value arbitrary)
      ;; 如果有任意值，直接使用
      (if arbitrary
          (setq css-props (ecss-tailwind-convert-arbitrary property arbitrary))
        ;; 否则根据属性和值转换
        (setq css-props (ecss-tailwind-convert-standard property value))))
    
    css-props))

(defun ecss-tailwind-convert-arbitrary (property arbitrary)
  "转换任意值语法的Tailwind类。"
  (let ((css-property (ecss-tailwind-property-to-css-property property)))
    (when css-property
      (list (cons (intern css-property) arbitrary)))))

(defun ecss-tailwind-convert-standard (property value)
  "转换标准Tailwind类到CSS属性。"
  (cond
   ;; Display
   ((string= property "block") '((display . "block")))
   ((string= property "inline") '((display . "inline")))
   ((string= property "inline-block") '((display . "inline-block")))
   ((string= property "flex") '((display . "flex")))
   ((string= property "inline-flex") '((display . "inline-flex")))
   ((string= property "grid") '((display . "grid")))
   ((string= property "inline-grid") '((display . "inline-grid")))
   ((string= property "hidden") '((display . "none")))
   ((string= property "table") '((display . "table")))
   
   ;; Position
   ((string= property "static") '((position . "static")))
   ((string= property "fixed") '((position . "fixed")))
   ((string= property "absolute") '((position . "absolute")))
   ((string= property "relative") '((position . "relative")))
   ((string= property "sticky") '((position . "sticky")))
   
   ;; Visibility
   ((string= property "visible") '((visibility . "visible")))
   ((string= property "invisible") '((visibility . "hidden")))
   
   ;; Background color
   ((string= property "bg")
    (let ((color (cdr (assoc value ecss-tailwind-color-palette))))
      (when color
        (list (cons 'background-color color)))))
   
   ;; Text color
   ((string= property "text")
    (cond
     ;; Color
     ((cdr (assoc value ecss-tailwind-color-palette))
      (list (cons 'color (cdr (assoc value ecss-tailwind-color-palette)))))
     ;; Font size
     ((cdr (assoc value ecss-tailwind-font-sizes))
      (let* ((size-data (cdr (assoc value ecss-tailwind-font-sizes)))
             (font-size (car size-data))
             (line-height (cadr size-data)))
        (list (cons 'font-size font-size)
              (cons 'line-height line-height))))
     ;; Text alignment
     ((member value '("left" "center" "right" "justify" "start" "end"))
      (list (cons 'text-align value)))))
   
   ;; Border color
   ((string= property "border")
    (cond
     ;; Border color
     ((cdr (assoc value ecss-tailwind-color-palette))
      (list (cons 'border-color (cdr (assoc value ecss-tailwind-color-palette)))))
     ;; Border width
     ((member value '("0" "2" "4" "8"))
      (list (cons 'border-width (concat value "px"))))
     ;; Default border
     ((null value)
      (list (cons 'border-width "1px")))))
   
   ;; Padding
   ((string-prefix-p "p" property)
    (ecss-tailwind-convert-spacing property value 'padding))
   
   ;; Margin
   ((string-prefix-p "m" property)
    (ecss-tailwind-convert-spacing property value 'margin))
   
   ;; Width
   ((string= property "w")
    (ecss-tailwind-convert-size 'width value))
   
   ;; Height
   ((string= property "h")
    (ecss-tailwind-convert-size 'height value))
   
   ;; Font weight
   ((string= property "font")
    (cond
     ((string= value "thin") '((font-weight . "100")))
     ((string= value "extralight") '((font-weight . "200")))
     ((string= value "light") '((font-weight . "300")))
     ((string= value "normal") '((font-weight . "400")))
     ((string= value "medium") '((font-weight . "500")))
     ((string= value "semibold") '((font-weight . "600")))
     ((string= value "bold") '((font-weight . "700")))
     ((string= value "extrabold") '((font-weight . "800")))
     ((string= value "black") '((font-weight . "900")))))
   
   ;; Rounded (border-radius)
   ((string= property "rounded")
    (cond
     ((null value) '((border-radius . "0.25rem")))
     ((string= value "none") '((border-radius . "0")))
     ((string= value "sm") '((border-radius . "0.125rem")))
     ((string= value "md") '((border-radius . "0.375rem")))
     ((string= value "lg") '((border-radius . "0.5rem")))
     ((string= value "xl") '((border-radius . "0.75rem")))
     ((string= value "2xl") '((border-radius . "1rem")))
     ((string= value "3xl") '((border-radius . "1.5rem")))
     ((string= value "full") '((border-radius . "9999px")))))
   
   ;; Shadow
   ((string= property "shadow")
    (cond
     ((null value)
      '((box-shadow . "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)")))
     ((string= value "sm")
      '((box-shadow . "0 1px 2px 0 rgb(0 0 0 / 0.05)")))
     ((string= value "md")
      '((box-shadow . "0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)")))
     ((string= value "lg")
      '((box-shadow . "0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1)")))
     ((string= value "xl")
      '((box-shadow . "0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1)")))
     ((string= value "2xl")
      '((box-shadow . "0 25px 50px -12px rgb(0 0 0 / 0.25)")))
     ((string= value "none")
      '((box-shadow . "none")))))
   
   ;; Opacity
   ((string= property "opacity")
    (when value
      (let ((opacity-value (/ (string-to-number value) 100.0)))
        (list (cons 'opacity (format "%.2f" opacity-value))))))
   
   ;; Flex properties
   ((string= property "flex")
    (cond
     ((string= value "1") '((flex . "1 1 0%")))
     ((string= value "auto") '((flex . "1 1 auto")))
     ((string= value "initial") '((flex . "0 1 auto")))
     ((string= value "none") '((flex . "none")))))
   
   ((string= property "justify")
    (cond
     ((string= value "start") '((justify-content . "flex-start")))
     ((string= value "end") '((justify-content . "flex-end")))
     ((string= value "center") '((justify-content . "center")))
     ((string= value "between") '((justify-content . "space-between")))
     ((string= value "around") '((justify-content . "space-around")))
     ((string= value "evenly") '((justify-content . "space-evenly")))))
   
   ((string= property "items")
    (cond
     ((string= value "start") '((align-items . "flex-start")))
     ((string= value "end") '((align-items . "flex-end")))
     ((string= value "center") '((align-items . "center")))
     ((string= value "baseline") '((align-items . "baseline")))
     ((string= value "stretch") '((align-items . "stretch")))))
   
   ;; Gap
   ((string= property "gap")
    (let ((spacing (cdr (assoc value ecss-tailwind-spacing-scale))))
      (when spacing
        (list (cons 'gap spacing)))))
   
   ;; Z-index
   ((string= property "z")
    (cond
     ((string= value "0") '((z-index . "0")))
     ((string= value "10") '((z-index . "10")))
     ((string= value "20") '((z-index . "20")))
     ((string= value "30") '((z-index . "30")))
     ((string= value "40") '((z-index . "40")))
     ((string= value "50") '((z-index . "50")))
     ((string= value "auto") '((z-index . "auto")))))
   
   ;; Default: return nil if not recognized
   (t nil)))

(defun ecss-tailwind-convert-spacing (property value type)
  "转换间距类（padding/margin）到CSS。
PROPERTY是Tailwind属性（如'p', 'px', 'pt'等）。
VALUE是间距值（如'4', '8'等）。
TYPE是'padding或'margin。"
  (let ((spacing (cdr (assoc value ecss-tailwind-spacing-scale))))
    (when spacing
      (cond
       ;; 全部方向
       ((string= property (if (eq type 'padding) "p" "m"))
        (list (cons type spacing)))
       ;; 水平方向
       ((string= property (if (eq type 'padding) "px" "mx"))
        (list (cons (intern (concat (symbol-name type) "-left")) spacing)
              (cons (intern (concat (symbol-name type) "-right")) spacing)))
       ;; 垂直方向
       ((string= property (if (eq type 'padding) "py" "my"))
        (list (cons (intern (concat (symbol-name type) "-top")) spacing)
              (cons (intern (concat (symbol-name type) "-bottom")) spacing)))
       ;; 单个方向
       ((string= property (if (eq type 'padding) "pt" "mt"))
        (list (cons (intern (concat (symbol-name type) "-top")) spacing)))
       ((string= property (if (eq type 'padding) "pr" "mr"))
        (list (cons (intern (concat (symbol-name type) "-right")) spacing)))
       ((string= property (if (eq type 'padding) "pb" "mb"))
        (list (cons (intern (concat (symbol-name type) "-bottom")) spacing)))
       ((string= property (if (eq type 'padding) "pl" "ml"))
        (list (cons (intern (concat (symbol-name type) "-left")) spacing)))))))

(defun ecss-tailwind-convert-size (property value)
  "转换尺寸类（width/height）到CSS。"
  (let ((size (cond
               ;; 使用spacing scale
               ((cdr (assoc value ecss-tailwind-spacing-scale))
                (cdr (assoc value ecss-tailwind-spacing-scale)))
               ;; 百分比
               ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" value)
                (let ((numerator (string-to-number (match-string 1 value)))
                      (denominator (string-to-number (match-string 2 value))))
                  (format "%.6f%%" (* 100.0 (/ (float numerator) denominator)))))
               ;; 特殊值
               ((string= value "full") "100%")
               ((string= value "screen")
                (if (eq property 'width) "100vw" "100vh"))
               ((string= value "auto") "auto")
               ((string= value "min") "min-content")
               ((string= value "max") "max-content")
               ((string= value "fit") "fit-content"))))
    (when size
      (list (cons property size)))))

(defun ecss-tailwind-property-to-css-property (tailwind-prop)
  "将Tailwind属性名转换为CSS属性名。"
  (cond
   ((string= tailwind-prop "bg") "background-color")
   ((string= tailwind-prop "text") "color")
   ((string= tailwind-prop "border") "border-color")
   ((string= tailwind-prop "w") "width")
   ((string= tailwind-prop "h") "height")
   ((string= tailwind-prop "min-w") "min-width")
   ((string= tailwind-prop "max-w") "max-width")
   ((string= tailwind-prop "min-h") "min-height")
   ((string= tailwind-prop "max-h") "max-height")
   (t tailwind-prop)))

(defun ecss-tailwind-classes-to-css (class-names)
  "将多个Tailwind类名转换为CSS属性列表。
CLASS-NAMES是类名列表或空格分隔的字符串。
返回合并后的CSS属性alist。

示例：
  (ecss-tailwind-classes-to-css \"flex items-center bg-red-500 p-4\")
  ;; => ((display . \"flex\") (align-items . \"center\") 
  ;;     (background-color . \"#ef4444\") (padding . \"1rem\"))"
  (let ((classes (if (stringp class-names)
                     (split-string class-names)
                   class-names))
        (css-props '()))
    (dolist (class classes)
      (let ((props (ecss-tailwind-to-css class)))
        (when props
          (dolist (prop props)
            ;; 后面的类会覆盖前面的同名属性
            (setq css-props (assq-delete-all (car prop) css-props))
            (push prop css-props)))))
    (nreverse css-props)))

(defun ecss-tailwind-apply-css-to-node (node class-names)
  "将Tailwind类名转换为CSS并应用到DOM节点。
NODE是DOM节点，CLASS-NAMES是Tailwind类名列表或字符串。
这会将Tailwind类转换为内联样式添加到节点的style属性。

示例：
  (ecss-tailwind-apply-css-to-node node \"flex items-center bg-blue-500 p-4\")"
  (let ((css-props (ecss-tailwind-classes-to-css class-names)))
    (when css-props
      (ecss-dom-set-styles node css-props))))

(defun ecss-tailwind-css-to-string (css-props)
  "将CSS属性alist转换为CSS字符串。
CSS-PROPS是 ((property . value) ...) 格式的alist。

示例：
  (ecss-tailwind-css-to-string '((display . \"flex\") (padding . \"1rem\")))
  ;; => \"display: flex; padding: 1rem\""
  (mapconcat (lambda (prop)
               (format "%s: %s" (car prop) (cdr prop)))
             css-props "; "))

(provide 'ecss-tailwind)

;;; ecss-tailwind.el ends here
