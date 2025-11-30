;;; etaf-tailwind.el --- Tailwind CSS support for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Keywords: css, tailwind, utility-first
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这个库为 ETAF 添加Tailwind CSS支持。
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
;;   (etaf-tailwind-class-p "bg-red-500")  ; => t
;;   (etaf-tailwind-class-p "hover:text-lg")  ; => t
;;
;;   ;; 解析Tailwind类
;;   (etaf-tailwind-parse-class "md:hover:bg-blue-500")
;;   ;; => (:variants ("md" "hover") :utility "bg-blue-500" :property "bg" :value "blue-500")
;;
;;   ;; 添加Tailwind类到DOM节点
;;   (etaf-tailwind-add-class node "hover:bg-blue-500")
;;
;; ============================================================================
;; Tailwind CSS 支持的工具类和 Emacs 渲染能力文档
;; ============================================================================
;;
;; === 完全支持解析和渲染的 Tailwind 工具类 ===
;;
;; 以下工具类可以被解析并在 Emacs 中原生渲染（通过 face 属性）：
;;
;; 颜色 (Color):
;;   - text-{color} -> :foreground (所有 Tailwind 调色板颜色)
;;   - bg-{color} -> :background (所有 Tailwind 调色板颜色)
;;
;; 字体 (Typography):
;;   - font-{weight} -> :weight (thin, light, normal, medium, semibold, bold, black)
;;   - font-{family} -> :family (sans, serif, mono)
;;   - text-{size} -> :height (xs, sm, base, lg, xl, 2xl, 3xl, 4xl, 5xl, 6xl, 7xl, 8xl, 9xl)
;;   - italic, not-italic -> :slant
;;   - underline, no-underline -> :underline
;;   - overline -> :overline
;;   - line-through -> :strike-through
;;
;; === 支持解析但无法通过 Emacs 原生能力渲染的工具类 ===
;;
;; 以下工具类可以被正确解析为 CSS，但 Emacs 无法原生渲染它们：
;; (标记: [LAYOUT] = 布局相关, [VISUAL] = 视觉效果, [INTERACTIVE] = 交互)
;;
;; [LAYOUT] 布局相关:
;;   - flex, inline-flex, grid, inline-grid -> display (Emacs 不支持 flexbox/grid 布局)
;;   - justify-*, items-*, content-*, self-* -> flex/grid 对齐 (无视觉效果)
;;   - flex-row, flex-col, flex-wrap -> flex 方向/换行
;;   - cols-*, rows-*, gap-* -> grid 模板
;;   - order-* -> 排序
;;   - basis-*, grow, shrink -> flex 尺寸
;;   - position (static, fixed, absolute, relative, sticky)
;;   - top-*, right-*, bottom-*, left-*, inset-*
;;   - z-*
;;   - float-*, clear-*
;;   - overflow-*, overscroll-*
;;
;; [VISUAL] 视觉效果 (Emacs 无法渲染):
;;   - shadow-* -> box-shadow (Emacs 不支持阴影效果)
;;   - rounded-* -> border-radius (Emacs 不支持圆角)
;;   - opacity-* -> 透明度 (Emacs 终端不支持 alpha 通道)
;;   - ring-* -> focus ring 效果
;;   - blur-*, brightness-*, contrast-*, grayscale-*, sepia-* -> CSS filters
;;   - backdrop-* -> backdrop filters
;;   - mix-blend-* -> 混合模式
;;
;; [VISUAL] 边框 (部分支持):
;;   - border, border-{width} -> 可以用 :box face 属性近似
;;   - border-{color} -> :box 可以设置颜色
;;   - border-{style} (solid, dashed, dotted) -> :box 支持 line-style
;;   - divide-* -> 子元素分隔线 (需要布局支持)
;;   - outline-* -> :box 可近似
;;
;; [INTERACTIVE] 交互相关 (Emacs 文本不适用):
;;   - cursor-* -> 鼠标指针样式
;;   - pointer-events-* -> 指针事件
;;   - select-* -> 文本选择
;;   - resize-* -> 调整大小
;;   - scroll-* -> 滚动行为
;;   - snap-* -> 滚动吸附
;;   - touch-* -> 触摸交互
;;
;; [VISUAL] 动画和过渡 (Emacs 不支持):
;;   - transition-* -> CSS transitions
;;   - duration-*, delay-* -> 时间控制
;;   - ease-* -> 缓动函数
;;   - animate-* -> CSS animations
;;
;; [VISUAL] 变换 (Emacs 不支持):
;;   - scale-*, rotate-*, translate-*, skew-* -> CSS transforms
;;   - origin-* -> transform origin
;;
;; [VISUAL] 渐变 (Emacs 不支持):
;;   - from-*, via-*, to-* -> 渐变颜色停止点
;;   - bg-gradient-* -> 渐变方向
;;
;; === 间距和尺寸 (布局时使用) ===
;;
;; 这些值被解析为 CSS，在布局计算时使用：
;;   - p-*, px-*, py-*, pt-*, pr-*, pb-*, pl-*, ps-*, pe-* -> padding
;;   - m-*, mx-*, my-*, mt-*, mr-*, mb-*, ml-*, ms-*, me-* -> margin
;;   - w-*, min-w-*, max-w-* -> width
;;   - h-*, min-h-*, max-h-* -> height
;;   - size-* -> width 和 height
;;   - space-x-*, space-y-* -> 子元素间距
;;
;; === 响应式和状态变体 ===
;;
;; 响应式前缀 (sm:, md:, lg:, xl:, 2xl:):
;;   - 被解析和识别，但 Emacs 文本缓冲区没有"视口"概念
;;   - 可以在构建 CSSOM 时使用 media-env 控制
;;
;; 状态变体 (hover:, focus:, active:, disabled: 等):
;;   - 被解析和识别
;;   - hover:/focus: 等状态需要通过 Emacs 的 overlay 或事件处理实现
;;   - disabled: 可以通过修改文本属性模拟
;;
;; === 表格相关 ===
;;   - table-auto, table-fixed -> table-layout
;;   - border-collapse, border-separate -> border-collapse
;;   - caption-*, table-header-group, table-footer-group, etc. -> 表格结构
;;
;; === 可访问性 ===
;;   - sr-only -> 屏幕阅读器专用（在 Emacs 中可以隐藏文本）
;;
;; === SVG 相关 ===
;;   - fill-*, stroke-* -> SVG 填充和描边 (Emacs 图像支持时可用)
;;
;; ============================================================================
;; 总结：Emacs 原生可渲染的属性
;; ============================================================================
;;
;; Emacs face 属性可以直接映射的 CSS 属性：
;;   - color -> :foreground
;;   - background-color -> :background
;;   - font-weight -> :weight
;;   - font-style -> :slant
;;   - font-size -> :height
;;   - font-family -> :family
;;   - text-decoration (underline) -> :underline
;;   - text-decoration (overline) -> :overline
;;   - text-decoration (line-through) -> :strike-through
;;   - border (简化) -> :box
;;
;; 其他 CSS 属性被解析为 CSSOM 用于布局计算，但不直接影响文本外观。
;;

;;; Code:

(require 'cl-lib)
(require 'etaf-dom)

;;; Tailwind CSS patterns and utilities

(defconst etaf-tailwind-responsive-prefixes
  '("sm" "md" "lg" "xl" "2xl")
  "Tailwind CSS响应式断点前缀。")

(defconst etaf-tailwind-state-variants
  '("hover" "focus" "active" "visited" "disabled" "checked"
    "focus-within" "focus-visible" "group-hover" "group-focus"
    "first" "last" "odd" "even" "required" "invalid" "placeholder-shown"
    "before" "after" "selection" "marker" "file")
  "Tailwind CSS状态变体。")

(defconst etaf-tailwind-utility-prefixes
  '(;; Layout
    "container" "box" "block" "inline" "flex" "grid" "table" "hidden"
    "float" "clear" "object" "overflow" "overscroll" "position"
    "top" "right" "bottom" "left" "inset" "visible" "invisible" "z"
    
    ;; Flexbox & Grid
    "basis" "direction" "wrap" "grow" "shrink" "order"
    "cols" "col" "rows" "row" "gap" "gap-x" "gap-y"
    "justify" "content" "items" "self" "place"
    
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

(defconst etaf-tailwind-standalone-utilities
  '("container" "flex" "grid" "block" "inline" "hidden" "visible" "invisible"
    "static" "fixed" "absolute" "relative" "sticky"
    "truncate" "italic" "not-italic" "antialiased" "subpixel-antialiased"
    "uppercase" "lowercase" "capitalize" "normal-case"
    "underline" "overline" "line-through" "no-underline"
    "table-auto" "table-fixed" "border-collapse" "border-separate"
    "inline-block" "inline-flex" "inline-grid" "flow-root" "contents" "list-item")
  "独立的Tailwind CSS实用类（不需要值）。")

;;; Constants for Tailwind CSS values

(defconst etaf-tailwind-order-first -9999
  "Order value for order-first utility.
Per Tailwind CSS specification, this ensures the element appears first.")

(defconst etaf-tailwind-order-last 9999
  "Order value for order-last utility.
Per Tailwind CSS specification, this ensures the element appears last.")

(defconst etaf-tailwind-leading-scale-factor 0.25
  "Scale factor for numeric leading values.
Each unit corresponds to 0.25rem in Tailwind CSS.")

(defconst etaf-tailwind-cursor-values
  '("auto" "default" "pointer" "wait" "text" "move"
    "help" "not-allowed" "none" "context-menu"
    "progress" "cell" "crosshair" "vertical-text"
    "alias" "copy" "no-drop" "grab" "grabbing"
    "all-scroll" "col-resize" "row-resize"
    "n-resize" "e-resize" "s-resize" "w-resize"
    "ne-resize" "nw-resize" "se-resize" "sw-resize"
    "ew-resize" "ns-resize" "nesw-resize" "nwse-resize"
    "zoom-in" "zoom-out")
  "Valid cursor values for Tailwind CSS cursor utilities.")

(defconst etaf-tailwind-sr-only-styles
  '((position . "absolute")
    (width . "1px")
    (height . "1px")
    (padding . "0")
    (margin . "-1px")
    (overflow . "hidden")
    (clip . "rect(0, 0, 0, 0)")
    (white-space . "nowrap")
    (border-width . "0"))
  "CSS styles for screen reader only (sr-only) utility.
Visually hides content while keeping it accessible to screen readers.")

(defconst etaf-tailwind-not-sr-only-styles
  '((position . "static")
    (width . "auto")
    (height . "auto")
    (padding . "0")
    (margin . "0")
    (overflow . "visible")
    (clip . "auto")
    (white-space . "normal"))
  "CSS styles to undo screen reader only (not-sr-only) utility.")

;;; Tailwind class parsing

(defun etaf-tailwind-parse-class (class-name)
  "解析Tailwind CSS类名，返回其组成部分。
返回一个plist，包含：
  :variants - 变体列表（如响应式前缀和状态）
  :utility - 完整的实用类名（不含变体）
  :property - 属性名称（如'bg'从'bg-red-500'）
  :value - 值部分（如'red-500'从'bg-red-500'）
  :arbitrary - 如果使用任意值语法，包含方括号内的值

示例：
  (etaf-tailwind-parse-class \"md:hover:bg-red-500\")
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
      ;; 检查是否是独立实用类（不应被分割）
      (if (member utility etaf-tailwind-standalone-utilities)
          (setq property utility)
        ;; 检查任意值语法 [...]
        (if (string-match "\\(.*\\)\\[\\([^]]+\\)\\]" utility)
            (progn
              (setq property (match-string 1 utility)
                    arbitrary (match-string 2 utility))
              ;; 移除property末尾的 '-' 如果存在
              (when (string-suffix-p "-" property)
                (setq property (substring property 0 -1))))
          ;; 标准格式：property-value
          ;; 对于带有多个连字符的属性，找到正确的分割点
          (let ((hyphen-pos (string-match "-" utility)))
            (if hyphen-pos
                ;; 检查是否需要保留更多的属性名称
                ;; 例如: "min-w-0" -> property: "min-w", value: "0"
                ;; 例如: "gap-x-4" -> property: "gap-x", value: "4"
                (let* ((first-part (substring utility 0 hyphen-pos))
                       (rest (substring utility (1+ hyphen-pos)))
                       ;; 检查是否是需要特殊处理的前缀
                       (compound-prefix
                        (cond
                         ;; min-* max-* prefixes
                         ((and (member first-part '("min" "max"))
                               (string-match "^\\([a-z]+\\)-\\(.+\\)$" rest))
                          (concat first-part "-" (match-string 1 rest)))
                         ;; gap-x, gap-y, space-x, space-y, overflow-x, overflow-y, etc.
                         ((and (member first-part '("gap" "space" "overflow" "overscroll"
                                                    "scroll" "snap" "border" "divide"
                                                    "ring" "col" "row"))
                               (string-match "^\\([xy]\\)-\\(.+\\)$" rest))
                          (concat first-part "-" (match-string 1 rest)))
                         ;; not-* prefix
                         ((and (string= first-part "not")
                               (string-match "^\\([a-z]+\\)$" rest))
                          utility)
                         ;; no-* prefix
                         ((and (string= first-part "no")
                               (string-match "^\\([a-z]+\\)$" rest))
                          utility)
                         ;; normal-case type
                         ((and (string= first-part "normal")
                               (string-match "^\\([a-z]+\\)$" rest))
                          utility)
                         ;; line-through
                         ((and (string= first-part "line")
                               (string= rest "through"))
                          utility)
                         ;; col-span-*, row-span-*, col-start-*, col-end-*, etc
                         ((and (member first-part '("col" "row"))
                               (string-match "^\\(span\\|start\\|end\\)-\\(.+\\)$" rest))
                          first-part)
                         (t nil))))
                  (if (and compound-prefix (not (string= compound-prefix utility)))
                      ;; 使用复合前缀
                      (let ((value-start (+ (length compound-prefix) 1)))
                        (if (< value-start (length utility))
                            (setq property compound-prefix
                                  value (substring utility value-start))
                          (setq property compound-prefix)))
                    ;; 检查独立的compound utilities
                    (if compound-prefix
                        (setq property compound-prefix)
                      ;; 标准分割
                      (setq property first-part
                            value rest))))
              ;; 独立实用类（无连字符）
              (setq property utility))))))
    
    (list :variants variants
          :utility utility
          :property property
          :value value
          :arbitrary arbitrary)))

(defun etaf-tailwind-class-p (class-name)
  "检查CLASS-NAME是否是有效的Tailwind CSS类名。
这是一个简化的验证，检查类名是否符合Tailwind的命名模式。"
  (when (and class-name (stringp class-name))
    (let* ((parsed (etaf-tailwind-parse-class class-name))
           (variants (plist-get parsed :variants))
           (property (plist-get parsed :property))
           (arbitrary (plist-get parsed :arbitrary)))
      
      ;; 检查变体是否有效
      (let ((valid-variants t))
        (dolist (variant variants)
          (unless (or (member variant etaf-tailwind-responsive-prefixes)
                      (member variant etaf-tailwind-state-variants)
                      ;; 允许dark和自定义变体
                      (string= variant "dark")
                      (string-prefix-p "group-" variant)
                      (string-prefix-p "peer-" variant))
            (setq valid-variants nil)))
        
        ;; 检查属性是否有效
        (and valid-variants
             (or
              ;; 独立实用类
              (member property etaf-tailwind-standalone-utilities)
              ;; 有前缀的实用类
              (cl-some (lambda (prefix)
                         (string-prefix-p prefix property))
                       etaf-tailwind-utility-prefixes)
              ;; 任意值
              arbitrary))))))

(defun etaf-tailwind-get-variants (class-name)
  "从Tailwind类名中提取所有变体。"
  (plist-get (etaf-tailwind-parse-class class-name) :variants))

(defun etaf-tailwind-get-utility (class-name)
  "从Tailwind类名中提取实用类部分（不含变体）。"
  (plist-get (etaf-tailwind-parse-class class-name) :utility))

(defun etaf-tailwind-get-property (class-name)
  "从Tailwind类名中提取属性名称。"
  (plist-get (etaf-tailwind-parse-class class-name) :property))

(defun etaf-tailwind-has-variant-p (class-name variant)
  "检查Tailwind类名是否包含指定的变体。"
  (member variant (etaf-tailwind-get-variants class-name)))

(defun etaf-tailwind-has-responsive-p (class-name)
  "检查Tailwind类名是否有响应式前缀。"
  (let ((variants (etaf-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v etaf-tailwind-responsive-prefixes))
             variants)))

(defun etaf-tailwind-has-state-variant-p (class-name)
  "检查Tailwind类名是否有状态变体。"
  (let ((variants (etaf-tailwind-get-variants class-name)))
    (cl-some (lambda (v) (member v etaf-tailwind-state-variants))
             variants)))

;;; DOM integration

(defun etaf-dom-node-has-tailwind-class-p (node class-name)
  "检查DOM节点是否有指定的Tailwind类。
支持精确匹配和模式匹配。"
  (when (and node (listp node))
    (when-let ((class-attr (dom-attr node 'class)))
      (let ((classes (split-string class-attr)))
        (member class-name classes)))))

(defun etaf-dom-query-tailwind (dom tailwind-class)
  "在DOM树中查询所有具有指定Tailwind类的节点。
TAILWIND-CLASS是要查询的Tailwind类名。

示例：
  (etaf-dom-query-tailwind dom \"flex\")
  (etaf-dom-query-tailwind dom \"bg-red-500\")
  (etaf-dom-query-tailwind dom \"md:hover:text-lg\")"
  (dom-search dom
              (lambda (node)
                (etaf-dom-node-has-tailwind-class-p node tailwind-class))))

(defun etaf-dom-query-tailwind-pattern (dom pattern)
  "在DOM树中查询匹配Tailwind模式的节点。
PATTERN可以是：
  - 属性前缀，如'bg'匹配所有背景类
  - 变体，如'hover:'匹配所有hover变体
  - 正则表达式

示例：
  (etaf-dom-query-tailwind-pattern dom \"^bg-\")  ; 所有背景类
  (etaf-dom-query-tailwind-pattern dom \"^hover:\") ; 所有hover类"
  (let ((regexp (if (stringp pattern) pattern (regexp-quote pattern))))
    (dom-search dom
                (lambda (node)
                  (when (and node (listp node))
                    (when-let ((class-attr (dom-attr node 'class)))
                      (let ((classes (split-string class-attr)))
                        (cl-some (lambda (class)
                                   (string-match-p regexp class))
                                 classes))))))))

(defun etaf-tailwind-add-class (node class-name)
  "为DOM节点添加Tailwind CSS类。
如果类名不是有效的Tailwind类，会发出警告但仍然添加。"
  (unless (etaf-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (etaf-dom-add-class node class-name))

(defun etaf-tailwind-remove-class (node class-name)
  "从DOM节点移除Tailwind CSS类。"
  (etaf-dom-remove-class node class-name))

(defun etaf-tailwind-toggle-class (node class-name)
  "切换DOM节点的Tailwind CSS类。"
  (unless (etaf-tailwind-class-p class-name)
    (message "Warning: '%s' may not be a valid Tailwind class" class-name))
  (etaf-dom-toggle-class node class-name))

(defun etaf-tailwind-replace-class (node old-class new-class)
  "替换DOM节点的Tailwind类。"
  (when (etaf-dom-has-class node old-class)
    (etaf-dom-remove-class node old-class)
    (etaf-tailwind-add-class node new-class)))

(defun etaf-tailwind-get-classes-by-property (node property)
  "获取DOM节点中指定属性的所有Tailwind类。
例如，获取所有'bg'（背景）类或所有'text'（文本）类。"
  (when (and node (listp node))
    (let ((results '()))
      (when-let ((class-attr (dom-attr node 'class)))
        (let ((classes (split-string class-attr)))
          (dolist (class classes)
            (when (etaf-tailwind-class-p class)
              (let* ((parsed (etaf-tailwind-parse-class class))
                     (class-property (plist-get parsed :property)))
                (when (string= class-property property)
                  (push class results)))))))
      (nreverse results))))

(defun etaf-tailwind-filter-classes (classes &optional filter-fn)
  "过滤Tailwind类列表。
FILTER-FN是一个函数，接受解析后的类信息，返回t表示保留。
如果FILTER-FN为nil，只返回有效的Tailwind类。"
  (cl-remove-if-not
   (lambda (class)
     (and (etaf-tailwind-class-p class)
          (or (null filter-fn)
              (funcall filter-fn (etaf-tailwind-parse-class class)))))
   classes))

;;; Utility functions

(defun etaf-tailwind-describe-class (class-name)
  "描述Tailwind类的详细信息。
返回一个人类可读的描述字符串。"
  (let* ((parsed (etaf-tailwind-parse-class class-name))
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
            (if (etaf-tailwind-class-p class-name) "yes" "no"))))

;;; CSS conversion - Tailwind to native CSS

(defconst etaf-tailwind-color-palette
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

(defconst etaf-tailwind-spacing-scale
  '(("0" . "0") ("1" . "1") ("2" . "2") ("3" . "3")
    ("4" . "4") ("5" . "5") ("6" . "6") ("7" . "7")
    ("8" . "8") ("9" . "9") ("10" . "10") ("11" . "11")
    ("12" . "12") ("13" . "13") ("14" . "14") ("15" . "15")
    ("16" . "16") ("17" . "17") ("18" . "18") ("19" . "19")
    ("20" . "20") ("24" . "24") ("28" . "28") ("32" . "32")
    ("36" . "36") ("40" . "40") ("44" . "44") ("48" . "48")
    ("52" . "52") ("56" . "56") ("60" . "60") ("64" . "64")
    ("72" . "72") ("80" . "80") ("96" . "96"))
  "Tailwind CSS间距数值（不含单位）。
单位由方向决定：水平方向使用px，垂直方向使用lh。")

(defun etaf-tailwind--spacing-value (value direction)
  "Convert spacing VALUE to CSS value based on DIRECTION.
DIRECTION is either 'horizontal, 'vertical, or 'all.
For Emacs compatibility:
- Horizontal (left/right): value in px (pixels)
- Vertical (top/bottom): value in lh (line-height units)
- All: use px for horizontal, lh for vertical components
VALUE can be a string number or from the spacing scale."
  (let ((num-str (or (cdr (assoc value etaf-tailwind-spacing-scale))
                     ;; Support arbitrary numeric values
                     (when (and value
                                (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$"
                                                value))
                       value))))
    (when num-str
      (pcase direction
        ('horizontal (concat num-str "px"))
        ('vertical (concat num-str "lh"))
        ('all num-str)  ; Used for shorthand properties
        (_ (concat num-str "px"))))))

(defconst etaf-tailwind-font-sizes
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

(defun etaf-tailwind-to-css (class-name)
  "将Tailwind类名转换为原生CSS属性和值。
返回一个alist: ((property . value) ...)。
如果无法转换，返回nil。

示例：
  (etaf-tailwind-to-css \"bg-red-500\")
  ;; => ((background-color . \"#ef4444\"))
  
  (etaf-tailwind-to-css \"text-lg\")
  ;; => ((font-size . \"1.125rem\") (line-height . \"1.75rem\"))
  
  (etaf-tailwind-to-css \"p-4\")
  ;; => ((padding . \"1rem\"))"
  (let* ((parsed (etaf-tailwind-parse-class class-name))
         (property (plist-get parsed :property))
         (value (plist-get parsed :value))
         (arbitrary (plist-get parsed :arbitrary))
         (css-props '()))
    
    (when (or property value arbitrary)
      ;; 如果有任意值，直接使用
      (if arbitrary
          (setq css-props (etaf-tailwind-convert-arbitrary
                           property arbitrary))
        ;; 否则根据属性和值转换
        (setq css-props (etaf-tailwind-convert-standard
                         property value))))
    
    css-props))

(defun etaf-tailwind-convert-arbitrary (property arbitrary)
  "转换任意值语法的Tailwind类。"
  (let ((css-property (etaf-tailwind-property-to-css-property property)))
    (when css-property
      (list (cons (intern css-property) arbitrary)))))

(defun etaf-tailwind-convert-standard (property value)
  "转换标准Tailwind类到CSS属性。"
  (cond
   ;; Display
   ((string= property "block") '((display . "block")))
   ((string= property "inline") '((display . "inline")))
   ((string= property "inline-block") '((display . "inline-block")))
   ((string= property "grid") '((display . "grid")))
   ((string= property "inline-grid") '((display . "inline-grid")))
   ((string= property "hidden") '((display . "none")))
   ((string= property "table") '((display . "table")))
   ((string= property "table-caption") '((display . "table-caption")))
   ((string= property "table-cell") '((display . "table-cell")))
   ((string= property "table-column") '((display . "table-column")))
   ((string= property "table-column-group") '((display . "table-column-group")))
   ((string= property "table-footer-group") '((display . "table-footer-group")))
   ((string= property "table-header-group") '((display . "table-header-group")))
   ((string= property "table-row") '((display . "table-row")))
   ((string= property "table-row-group") '((display . "table-row-group")))
   ((string= property "flow-root") '((display . "flow-root")))
   ((string= property "contents") '((display . "contents")))
   ((string= property "list-item") '((display . "list-item")))
   
   ;; Box sizing
   ((string= property "box")
    (cond
     ((string= value "border") '((box-sizing . "border-box")))
     ((string= value "content") '((box-sizing . "content-box")))))
   
   ;; Position
   ((string= property "static") '((position . "static")))
   ((string= property "fixed") '((position . "fixed")))
   ((string= property "absolute") '((position . "absolute")))
   ((string= property "relative") '((position . "relative")))
   ((string= property "sticky") '((position . "sticky")))
   
   ;; Position inset values (top, right, bottom, left, inset)
   ((string= property "top")
    (etaf-tailwind--convert-position-value 'top value))
   ((string= property "right")
    (etaf-tailwind--convert-position-value 'right value))
   ((string= property "bottom")
    (etaf-tailwind--convert-position-value 'bottom value))
   ((string= property "left")
    (etaf-tailwind--convert-position-value 'left value))
   ((string= property "inset")
    (let ((pos-val (etaf-tailwind--get-position-value value)))
      (when pos-val
        (list (cons 'top pos-val) (cons 'right pos-val)
              (cons 'bottom pos-val) (cons 'left pos-val)))))
   
   ;; Visibility
   ((string= property "visible") '((visibility . "visible")))
   ((string= property "invisible") '((visibility . "hidden")))
   ((string= property "collapse") '((visibility . "collapse")))
   
   ;; Float
   ((string= property "float")
    (cond
     ((string= value "right") '((float . "right")))
     ((string= value "left") '((float . "left")))
     ((string= value "none") '((float . "none")))))
   
   ;; Clear
   ((string= property "clear")
    (cond
     ((string= value "left") '((clear . "left")))
     ((string= value "right") '((clear . "right")))
     ((string= value "both") '((clear . "both")))
     ((string= value "none") '((clear . "none")))))
   
   ;; Object fit (for images)
   ((string= property "object")
    (cond
     ((string= value "contain") '((object-fit . "contain")))
     ((string= value "cover") '((object-fit . "cover")))
     ((string= value "fill") '((object-fit . "fill")))
     ((string= value "none") '((object-fit . "none")))
     ((string= value "scale-down") '((object-fit . "scale-down")))))
   
   ;; Overflow
   ((string= property "overflow")
    (cond
     ((string= value "auto") '((overflow . "auto")))
     ((string= value "hidden") '((overflow . "hidden")))
     ((string= value "clip") '((overflow . "clip")))
     ((string= value "visible") '((overflow . "visible")))
     ((string= value "scroll") '((overflow . "scroll")))
     ((string= value "x-auto") '((overflow-x . "auto")))
     ((string= value "y-auto") '((overflow-y . "auto")))
     ((string= value "x-hidden") '((overflow-x . "hidden")))
     ((string= value "y-hidden") '((overflow-y . "hidden")))
     ((string= value "x-clip") '((overflow-x . "clip")))
     ((string= value "y-clip") '((overflow-y . "clip")))
     ((string= value "x-visible") '((overflow-x . "visible")))
     ((string= value "y-visible") '((overflow-y . "visible")))
     ((string= value "x-scroll") '((overflow-x . "scroll")))
     ((string= value "y-scroll") '((overflow-y . "scroll")))))
   
   ;; Overscroll
   ((string= property "overscroll")
    (cond
     ((string= value "auto") '((overscroll-behavior . "auto")))
     ((string= value "contain") '((overscroll-behavior . "contain")))
     ((string= value "none") '((overscroll-behavior . "none")))
     ((string= value "y-auto") '((overscroll-behavior-y . "auto")))
     ((string= value "y-contain") '((overscroll-behavior-y . "contain")))
     ((string= value "y-none") '((overscroll-behavior-y . "none")))
     ((string= value "x-auto") '((overscroll-behavior-x . "auto")))
     ((string= value "x-contain") '((overscroll-behavior-x . "contain")))
     ((string= value "x-none") '((overscroll-behavior-x . "none")))))
   
   ;; Background color
   ((string= property "bg")
    (let ((color (cdr (assoc value etaf-tailwind-color-palette))))
      (when color
        (list (cons 'background-color color)))))
   
   ;; Text color and properties
   ((string= property "text")
    (cond
     ;; Color
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'color (cdr (assoc value etaf-tailwind-color-palette)))))
     ;; Font size
     ((cdr (assoc value etaf-tailwind-font-sizes))
      (let* ((size-data (cdr (assoc value etaf-tailwind-font-sizes)))
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
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'border-color
                  (cdr (assoc value etaf-tailwind-color-palette)))))
     ;; Border width (Tailwind CSS uses: border-0, border-1, border-2, border-4, border-8)
     ((member value '("0" "1" "2" "4" "8"))
      (list (cons 'border-width (concat value "px"))))
     ;; Default border (border without value means 1px)
     ((null value)
      (list (cons 'border-width "1px")))
     ;; Border styles
     ((string= value "solid") '((border-style . "solid")))
     ((string= value "dashed") '((border-style . "dashed")))
     ((string= value "dotted") '((border-style . "dotted")))
     ((string= value "double") '((border-style . "double")))
     ((string= value "hidden") '((border-style . "hidden")))
     ((string= value "none") '((border-style . "none")))))
   
   ;; Padding - expanded to handle ps and pe
   ((or (string= property "p")
        (string= property "px") (string= property "py")
        (string= property "pt") (string= property "pr")
        (string= property "pb") (string= property "pl")
        (string= property "ps") (string= property "pe"))
    (etaf-tailwind-convert-spacing property value 'padding))
   
   ;; Margin - expanded to handle ms and me
   ((or (string= property "m")
        (string= property "mx") (string= property "my")
        (string= property "mt") (string= property "mr")
        (string= property "mb") (string= property "ml")
        (string= property "ms") (string= property "me"))
    (etaf-tailwind-convert-spacing property value 'margin))
   
   ;; Space between
   ((string= property "space")
    (etaf-tailwind-convert-space-between value))
   
   ;; Width
   ((string= property "w")
    (etaf-tailwind-convert-size 'width value))
   
   ;; Min-width
   ((string= property "min-w")
    (etaf-tailwind--convert-min-max-size 'min-width value))
   
   ;; Max-width
   ((string= property "max-w")
    (etaf-tailwind--convert-max-width value))
   
   ;; Height
   ((string= property "h")
    (etaf-tailwind-convert-size 'height value))
   
   ;; Min-height
   ((string= property "min-h")
    (etaf-tailwind--convert-min-max-size 'min-height value))
   
   ;; Max-height
   ((string= property "max-h")
    (etaf-tailwind--convert-min-max-size 'max-height value))
   
   ;; Size (width and height together)
   ((string= property "size")
    (let ((w-css (etaf-tailwind-convert-size 'width value))
          (h-css (etaf-tailwind-convert-size 'height value)))
      (when (and w-css h-css)
        (append w-css h-css))))
   
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
     ((string= value "black") '((font-weight . "900")))
     ;; Font families
     ((string= value "sans") '((font-family . "ui-sans-serif, system-ui, sans-serif")))
     ((string= value "serif") '((font-family . "ui-serif, Georgia, serif")))
     ((string= value "mono") '((font-family . "ui-monospace, monospace")))))
   
   ;; Typography - italic
   ((string= property "italic") '((font-style . "italic")))
   ((string= property "not-italic") '((font-style . "normal")))
   
   ;; Typography - text transform
   ((string= property "uppercase") '((text-transform . "uppercase")))
   ((string= property "lowercase") '((text-transform . "lowercase")))
   ((string= property "capitalize") '((text-transform . "capitalize")))
   ((string= property "normal-case") '((text-transform . "none")))
   
   ;; Typography - text decoration
   ((string= property "underline") '((text-decoration-line . "underline")))
   ((string= property "overline") '((text-decoration-line . "overline")))
   ((string= property "line-through") '((text-decoration-line . "line-through")))
   ((string= property "no-underline") '((text-decoration-line . "none")))
   
   ;; Typography - line height (leading)
   ((string= property "leading")
    (etaf-tailwind--convert-leading value))
   
   ;; Typography - letter spacing (tracking)
   ((string= property "tracking")
    (etaf-tailwind--convert-tracking value))
   
   ;; Typography - word break
   ((string= property "break")
    (cond
     ((string= value "normal") '((overflow-wrap . "normal") (word-break . "normal")))
     ((string= value "words") '((overflow-wrap . "break-word")))
     ((string= value "all") '((word-break . "break-all")))
     ((string= value "keep") '((word-break . "keep-all")))))
   
   ;; Typography - whitespace
   ((string= property "whitespace")
    (cond
     ((string= value "normal") '((white-space . "normal")))
     ((string= value "nowrap") '((white-space . "nowrap")))
     ((string= value "pre") '((white-space . "pre")))
     ((string= value "pre-line") '((white-space . "pre-line")))
     ((string= value "pre-wrap") '((white-space . "pre-wrap")))
     ((string= value "break-spaces") '((white-space . "break-spaces")))))
   
   ;; Typography - truncate
   ((string= property "truncate")
    '((overflow . "hidden") (text-overflow . "ellipsis") (white-space . "nowrap")))
   
   ;; Typography - text indent
   ((string= property "indent")
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons 'text-indent spacing)))))
   
   ;; Typography - vertical align
   ((string= property "align")
    (cond
     ((string= value "baseline") '((vertical-align . "baseline")))
     ((string= value "top") '((vertical-align . "top")))
     ((string= value "middle") '((vertical-align . "middle")))
     ((string= value "bottom") '((vertical-align . "bottom")))
     ((string= value "text-top") '((vertical-align . "text-top")))
     ((string= value "text-bottom") '((vertical-align . "text-bottom")))
     ((string= value "sub") '((vertical-align . "sub")))
     ((string= value "super") '((vertical-align . "super")))))
   
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
   
   ;; Outline
   ((string= property "outline")
    (cond
     ((null value) '((outline-style . "solid") (outline-width . "2px")))
     ((string= value "none") '((outline . "2px solid transparent") (outline-offset . "2px")))
     ((string= value "dashed") '((outline-style . "dashed")))
     ((string= value "dotted") '((outline-style . "dotted")))
     ((string= value "double") '((outline-style . "double")))
     ((member value '("0" "1" "2" "4" "8"))
      (list (cons 'outline-width (concat value "px"))))
     ;; Outline colors
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'outline-color (cdr (assoc value etaf-tailwind-color-palette)))))))
   
   ;; Ring (simplified - box-shadow based in real Tailwind)
   ((string= property "ring")
    (cond
     ((null value) '((box-shadow . "0 0 0 3px rgba(59, 130, 246, 0.5)")))
     ((string= value "0") '((box-shadow . "none")))
     ((string= value "1") '((box-shadow . "0 0 0 1px rgba(59, 130, 246, 0.5)")))
     ((string= value "2") '((box-shadow . "0 0 0 2px rgba(59, 130, 246, 0.5)")))
     ((string= value "4") '((box-shadow . "0 0 0 4px rgba(59, 130, 246, 0.5)")))
     ((string= value "8") '((box-shadow . "0 0 0 8px rgba(59, 130, 246, 0.5)")))
     ((string= value "inset") '((box-shadow . "inset 0 0 0 3px rgba(59, 130, 246, 0.5)")))))
   
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
     ((string= value "inner")
      '((box-shadow . "inset 0 2px 4px 0 rgb(0 0 0 / 0.05)")))
     ((string= value "none")
      '((box-shadow . "none")))))
   
   ;; Opacity
   ((string= property "opacity")
    (when value
      (let ((opacity-value (/ (string-to-number value) 100.0)))
        (list (cons 'opacity (format "%.2f" opacity-value))))))
   
   ;; Flex properties
   ((string= property "inline-flex") '((display . "inline-flex")))
   ((string= property "flex")
    (cond
     ((null value) '((display . "flex")))
     ((string= value "nowrap") '((flex-wrap . "nowrap")))
     ((string= value "wrap") '((flex-wrap . "wrap")))
     ((string= value "wrap-reverse") '((flex-wrap . "wrap-reverse")))
     ((string= value "row") '((flex-direction . "row")))
     ((string= value "row-reverse") '((flex-direction . "row-reverse")))
     ((string= value "col") '((flex-direction . "column")))
     ((string= value "col-reverse") '((flex-direction . "column-reverse")))
     ((string= value "1") '((flex . "1 1 0%")))
     ((string= value "auto") '((flex . "1 1 auto")))
     ((string= value "initial") '((flex . "0 1 auto")))
     ((string= value "none") '((flex . "none")))))
   
   ;; Flex basis
   ((string= property "basis")
    (etaf-tailwind--convert-flex-basis value))
   
   ;; Flex grow
   ((string= property "grow")
    (cond
     ((null value) '((flex-grow . "1")))
     ((string= value "0") '((flex-grow . "0")))))
   
   ;; Flex shrink
   ((string= property "shrink")
    (cond
     ((null value) '((flex-shrink . "1")))
     ((string= value "0") '((flex-shrink . "0")))))
   
   ;; Order
   ((string= property "order")
    (cond
     ((string= value "first")
      (list (cons 'order (number-to-string etaf-tailwind-order-first))))
     ((string= value "last")
      (list (cons 'order (number-to-string etaf-tailwind-order-last))))
     ((string= value "none") '((order . "0")))
     (value (list (cons 'order value)))))
   
   ;; Grid columns
   ((string= property "cols")
    (list (cons 'grid-template-columns
                (if (string= value "none") "none"
                  (format "repeat(%s, minmax(0, 1fr))" value)))))
   
   ;; Grid column span
   ((string= property "col")
    (cond
     ((string= value "auto") '((grid-column . "auto")))
     ((string-prefix-p "span-" value)
      (let ((span (substring value 5)))
        (if (string= span "full")
            '((grid-column . "1 / -1"))
          (list (cons 'grid-column (format "span %s / span %s" span span))))))
     ((string-prefix-p "start-" value)
      (list (cons 'grid-column-start (substring value 6))))
     ((string-prefix-p "end-" value)
      (list (cons 'grid-column-end (substring value 4))))))
   
   ;; Grid rows
   ((string= property "rows")
    (list (cons 'grid-template-rows
                (if (string= value "none") "none"
                  (format "repeat(%s, minmax(0, 1fr))" value)))))
   
   ;; Grid row span
   ((string= property "row")
    (cond
     ((string= value "auto") '((grid-row . "auto")))
     ((string-prefix-p "span-" value)
      (let ((span (substring value 5)))
        (if (string= span "full")
            '((grid-row . "1 / -1"))
          (list (cons 'grid-row (format "span %s / span %s" span span))))))
     ((string-prefix-p "start-" value)
      (list (cons 'grid-row-start (substring value 6))))
     ((string-prefix-p "end-" value)
      (list (cons 'grid-row-end (substring value 4))))))
   
   ((string= property "justify")
    (cond
     ((string= value "start") '((justify-content . "flex-start")))
     ((string= value "end") '((justify-content . "flex-end")))
     ((string= value "center") '((justify-content . "center")))
     ((string= value "between") '((justify-content . "space-between")))
     ((string= value "around") '((justify-content . "space-around")))
     ((string= value "evenly") '((justify-content . "space-evenly")))
     ((string= value "normal") '((justify-content . "normal")))
     ((string= value "stretch") '((justify-content . "stretch")))))
   
   ((string= property "items")
    (cond
     ((string= value "start") '((align-items . "flex-start")))
     ((string= value "end") '((align-items . "flex-end")))
     ((string= value "center") '((align-items . "center")))
     ((string= value "baseline") '((align-items . "baseline")))
     ((string= value "stretch") '((align-items . "stretch")))))
   
   ;; Content alignment
   ((string= property "content")
    (cond
     ((string= value "normal") '((align-content . "normal")))
     ((string= value "center") '((align-content . "center")))
     ((string= value "start") '((align-content . "flex-start")))
     ((string= value "end") '((align-content . "flex-end")))
     ((string= value "between") '((align-content . "space-between")))
     ((string= value "around") '((align-content . "space-around")))
     ((string= value "evenly") '((align-content . "space-evenly")))
     ((string= value "baseline") '((align-content . "baseline")))
     ((string= value "stretch") '((align-content . "stretch")))))
   
   ;; Self alignment
   ((string= property "self")
    (cond
     ((string= value "auto") '((align-self . "auto")))
     ((string= value "start") '((align-self . "flex-start")))
     ((string= value "end") '((align-self . "flex-end")))
     ((string= value "center") '((align-self . "center")))
     ((string= value "stretch") '((align-self . "stretch")))
     ((string= value "baseline") '((align-self . "baseline")))))
   
   ;; Place content
   ((string= property "place")
    (etaf-tailwind--convert-place-property value))
   
   ;; Gap - use px for column-gap, lh for row-gap
   ((string= property "gap")
    (let ((h-spacing (etaf-tailwind--spacing-value value 'horizontal))
          (v-spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when (and h-spacing v-spacing)
        (list (cons 'column-gap h-spacing)
              (cons 'row-gap v-spacing)))))
   
   ;; Gap-x (column-gap only)
   ((string= property "gap-x")
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons 'column-gap spacing)))))
   
   ;; Gap-y (row-gap only)
   ((string= property "gap-y")
    (let ((spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when spacing
        (list (cons 'row-gap spacing)))))
   
   ;; Z-index
   ((string= property "z")
    (cond
     ((string= value "0") '((z-index . "0")))
     ((string= value "10") '((z-index . "10")))
     ((string= value "20") '((z-index . "20")))
     ((string= value "30") '((z-index . "30")))
     ((string= value "40") '((z-index . "40")))
     ((string= value "50") '((z-index . "50")))
     ((string= value "auto") '((z-index . "auto")))
     ;; Support arbitrary z-index values
     (value (list (cons 'z-index value)))))
   
   ;; Cursor
   ((string= property "cursor")
    (when (member value etaf-tailwind-cursor-values)
      (list (cons 'cursor value))))
   
   ;; Pointer events
   ((string= property "pointer-events")
    (cond
     ((string= value "none") '((pointer-events . "none")))
     ((string= value "auto") '((pointer-events . "auto")))))
   
   ;; User select
   ((string= property "select")
    (cond
     ((string= value "none") '((user-select . "none")))
     ((string= value "text") '((user-select . "text")))
     ((string= value "all") '((user-select . "all")))
     ((string= value "auto") '((user-select . "auto")))))
   
   ;; Resize
   ((string= property "resize")
    (cond
     ((null value) '((resize . "both")))
     ((string= value "none") '((resize . "none")))
     ((string= value "y") '((resize . "vertical")))
     ((string= value "x") '((resize . "horizontal")))))
   
   ;; Scroll behavior
   ((string= property "scroll")
    (cond
     ((string= value "auto") '((scroll-behavior . "auto")))
     ((string= value "smooth") '((scroll-behavior . "smooth")))))
   
   ;; Table layout
   ((string= property "table-auto") '((table-layout . "auto")))
   ((string= property "table-fixed") '((table-layout . "fixed")))
   
   ;; Border collapse
   ((string= property "border-collapse") '((border-collapse . "collapse")))
   ((string= property "border-separate") '((border-collapse . "separate")))
   
   ;; SVG fill
   ((string= property "fill")
    (cond
     ((string= value "none") '((fill . "none")))
     ((string= value "current") '((fill . "currentColor")))
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'fill (cdr (assoc value etaf-tailwind-color-palette)))))))
   
   ;; SVG stroke
   ((string= property "stroke")
    (cond
     ((string= value "none") '((stroke . "none")))
     ((string= value "current") '((stroke . "currentColor")))
     ((member value '("0" "1" "2"))
      (list (cons 'stroke-width value)))
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'stroke (cdr (assoc value etaf-tailwind-color-palette)))))))
   
   ;; Accessibility - screen reader only
   ((string= property "sr")
    (when (string= value "only")
      (copy-alist etaf-tailwind-sr-only-styles)))
   
   ;; Not screen reader only
   ((and (string= property "not-sr") (string= value "only"))
    (copy-alist etaf-tailwind-not-sr-only-styles))
   
   ;; Appearance
   ((string= property "appearance")
    (cond
     ((string= value "none") '((appearance . "none")))
     ((string= value "auto") '((appearance . "auto")))))
   
   ;; Caret color
   ((string= property "caret")
    (let ((color (cdr (assoc value etaf-tailwind-color-palette))))
      (when color
        (list (cons 'caret-color color)))))
   
   ;; Divide (border between children - generates CSS variables in real Tailwind)
   ;; Simplified implementation
   ((string= property "divide")
    (cond
     ((string= value "x") '((border-left-width . "1px")))
     ((string= value "y") '((border-top-width . "1px")))
     ((cdr (assoc value etaf-tailwind-color-palette))
      (list (cons 'border-color (cdr (assoc value etaf-tailwind-color-palette)))))))
   
   ;; Default: return nil if not recognized
   (t nil)))

;;; Additional helper functions for Tailwind CSS conversion

(defun etaf-tailwind--get-position-value (value)
  "Convert position VALUE to CSS.
VALUE can be a number from spacing scale or special keywords."
  (cond
   ;; Numeric values from spacing scale
   ((cdr (assoc value etaf-tailwind-spacing-scale))
    (etaf-tailwind--spacing-value value 'horizontal))
   ;; Direct numeric values
   ((string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" value)
    (concat value "px"))
   ;; Auto and special values
   ((string= value "auto") "auto")
   ((string= value "full") "100%")
   ;; Fractions
   ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" value)
    (let ((num (string-to-number (match-string 1 value)))
          (den (string-to-number (match-string 2 value))))
      (format "%.6f%%" (* 100.0 (/ (float num) den)))))
   (t nil)))

(defun etaf-tailwind--convert-position-value (prop value)
  "Convert position property PROP with VALUE.
PROP is one of 'top, 'right, 'bottom, 'left."
  (let ((css-val (etaf-tailwind--get-position-value value)))
    (when css-val
      (list (cons prop css-val)))))

(defun etaf-tailwind--convert-leading (value)
  "Convert leading (line-height) VALUE to CSS."
  (cond
   ((string= value "none") '((line-height . "1")))
   ((string= value "tight") '((line-height . "1.25")))
   ((string= value "snug") '((line-height . "1.375")))
   ((string= value "normal") '((line-height . "1.5")))
   ((string= value "relaxed") '((line-height . "1.625")))
   ((string= value "loose") '((line-height . "2")))
   ;; Numeric values (3, 4, 5, 6, 7, 8, 9, 10)
   ((string-match-p "^[0-9]+$" value)
    (let ((num (string-to-number value)))
      (list (cons 'line-height
                  (format "%.2frem" (* etaf-tailwind-leading-scale-factor num))))))
   (t nil)))

(defun etaf-tailwind--convert-tracking (value)
  "Convert tracking (letter-spacing) VALUE to CSS."
  (cond
   ((string= value "tighter") '((letter-spacing . "-0.05em")))
   ((string= value "tight") '((letter-spacing . "-0.025em")))
   ((string= value "normal") '((letter-spacing . "0em")))
   ((string= value "wide") '((letter-spacing . "0.025em")))
   ((string= value "wider") '((letter-spacing . "0.05em")))
   ((string= value "widest") '((letter-spacing . "0.1em")))
   (t nil)))

(defun etaf-tailwind--convert-min-max-size (prop value)
  "Convert min/max size property PROP with VALUE.
PROP is one of 'min-width, 'max-width, 'min-height, 'max-height."
  (let* ((is-width (or (eq prop 'min-width) (eq prop 'max-width)))
         (direction (if is-width 'horizontal 'vertical))
         (size (cond
                ;; Numeric values from spacing scale
                ((cdr (assoc value etaf-tailwind-spacing-scale))
                 (etaf-tailwind--spacing-value value direction))
                ;; Direct numeric values
                ((string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" value)
                 (if is-width (concat value "px") (concat value "lh")))
                ;; Special keywords
                ((string= value "0") "0")
                ((string= value "full") "100%")
                ((string= value "screen")
                 (if is-width "100vw" "100vh"))
                ((string= value "none") "none")
                ((string= value "min") "min-content")
                ((string= value "max") "max-content")
                ((string= value "fit") "fit-content")
                (t nil))))
    (when size
      (list (cons prop size)))))

(defconst etaf-tailwind-max-width-scale
  '(("xs" . "20rem")
    ("sm" . "24rem")
    ("md" . "28rem")
    ("lg" . "32rem")
    ("xl" . "36rem")
    ("2xl" . "42rem")
    ("3xl" . "48rem")
    ("4xl" . "56rem")
    ("5xl" . "64rem")
    ("6xl" . "72rem")
    ("7xl" . "80rem")
    ("prose" . "65ch")
    ("screen-sm" . "640px")
    ("screen-md" . "768px")
    ("screen-lg" . "1024px")
    ("screen-xl" . "1280px")
    ("screen-2xl" . "1536px"))
  "Tailwind max-width preset scale.")

(defun etaf-tailwind--convert-max-width (value)
  "Convert max-width VALUE to CSS."
  (let ((size (cond
               ;; Named sizes
               ((cdr (assoc value etaf-tailwind-max-width-scale))
                (cdr (assoc value etaf-tailwind-max-width-scale)))
               ;; Standard size conversions
               ((cdr (assoc value etaf-tailwind-spacing-scale))
                (etaf-tailwind--spacing-value value 'horizontal))
               ;; Direct numeric values
               ((string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" value)
                (concat value "px"))
               ;; Special keywords
               ((string= value "none") "none")
               ((string= value "full") "100%")
               ((string= value "min") "min-content")
               ((string= value "max") "max-content")
               ((string= value "fit") "fit-content")
               (t nil))))
    (when size
      (list (cons 'max-width size)))))

(defun etaf-tailwind--convert-flex-basis (value)
  "Convert flex-basis VALUE to CSS."
  (let ((size (cond
               ;; Numeric values from spacing scale
               ((cdr (assoc value etaf-tailwind-spacing-scale))
                (etaf-tailwind--spacing-value value 'horizontal))
               ;; Direct numeric values
               ((string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" value)
                (concat value "px"))
               ;; Fractions
               ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" value)
                (let ((num (string-to-number (match-string 1 value)))
                      (den (string-to-number (match-string 2 value))))
                  (format "%.6f%%" (* 100.0 (/ (float num) den)))))
               ;; Special keywords
               ((string= value "auto") "auto")
               ((string= value "full") "100%")
               (t nil))))
    (when size
      (list (cons 'flex-basis size)))))

(defun etaf-tailwind--convert-place-property (value)
  "Convert place-* property VALUE.
VALUE is in format 'content-center', 'items-start', 'self-auto', etc."
  (cond
   ;; place-content-*
   ((string-prefix-p "content-" value)
    (let ((align-val (substring value 8)))
      (cond
       ((string= align-val "center") '((place-content . "center")))
       ((string= align-val "start") '((place-content . "start")))
       ((string= align-val "end") '((place-content . "end")))
       ((string= align-val "between") '((place-content . "space-between")))
       ((string= align-val "around") '((place-content . "space-around")))
       ((string= align-val "evenly") '((place-content . "space-evenly")))
       ((string= align-val "baseline") '((place-content . "baseline")))
       ((string= align-val "stretch") '((place-content . "stretch"))))))
   ;; place-items-*
   ((string-prefix-p "items-" value)
    (let ((align-val (substring value 6)))
      (cond
       ((string= align-val "start") '((place-items . "start")))
       ((string= align-val "end") '((place-items . "end")))
       ((string= align-val "center") '((place-items . "center")))
       ((string= align-val "baseline") '((place-items . "baseline")))
       ((string= align-val "stretch") '((place-items . "stretch"))))))
   ;; place-self-*
   ((string-prefix-p "self-" value)
    (let ((align-val (substring value 5)))
      (cond
       ((string= align-val "auto") '((place-self . "auto")))
       ((string= align-val "start") '((place-self . "start")))
       ((string= align-val "end") '((place-self . "end")))
       ((string= align-val "center") '((place-self . "center")))
       ((string= align-val "stretch") '((place-self . "stretch"))))))
   (t nil)))

(defun etaf-tailwind-convert-space-between (value)
  "Convert space-x-* and space-y-* utilities to CSS.
VALUE is like 'x-4' or 'y-2'."
  (cond
   ;; space-x-*
   ((string-prefix-p "x-" value)
    (let* ((spacing-val (substring value 2))
           (spacing (etaf-tailwind--spacing-value spacing-val 'horizontal)))
      (when spacing
        ;; In real Tailwind, this uses > * + * selector
        ;; We approximate with margin-left for all children
        (list (cons 'margin-left spacing)))))
   ;; space-y-*
   ((string-prefix-p "y-" value)
    (let* ((spacing-val (substring value 2))
           (spacing (etaf-tailwind--spacing-value spacing-val 'vertical)))
      (when spacing
        (list (cons 'margin-top spacing)))))
   (t nil)))

(defun etaf-tailwind-convert-spacing (property value type)
  "转换间距类（padding/margin）到CSS。
PROPERTY是Tailwind属性（如'p', 'px', 'pt'等）。
VALUE是间距值（如'4', '8'等）。
TYPE是'padding或'margin。

Emacs特有的单位处理：
- 水平方向（left/right）使用px像素
- 垂直方向（top/bottom）使用lh行高单位"
  (cond
   ;; 全部方向 - 使用方向特定的单位
   ((string= property (if (eq type 'padding) "p" "m"))
    (let ((h-spacing (etaf-tailwind--spacing-value value 'horizontal))
          (v-spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when (and h-spacing v-spacing)
        (list (cons (intern (concat (symbol-name type) "-top")) v-spacing)
              (cons (intern (concat (symbol-name type) "-right")) h-spacing)
              (cons (intern (concat (symbol-name type) "-bottom")) v-spacing)
              (cons (intern (concat (symbol-name type) "-left")) h-spacing)))))
   ;; 水平方向 - 使用px
   ((string= property (if (eq type 'padding) "px" "mx"))
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-left")) spacing)
              (cons (intern (concat (symbol-name type) "-right")) spacing)))))
   ;; 垂直方向 - 使用lh
   ((string= property (if (eq type 'padding) "py" "my"))
    (let ((spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-top")) spacing)
              (cons (intern (concat (symbol-name type) "-bottom")) spacing)))))
   ;; 单个方向
   ((string= property (if (eq type 'padding) "pt" "mt"))
    (let ((spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-top")) spacing)))))
   ((string= property (if (eq type 'padding) "pr" "mr"))
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-right")) spacing)))))
   ((string= property (if (eq type 'padding) "pb" "mb"))
    (let ((spacing (etaf-tailwind--spacing-value value 'vertical)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-bottom")) spacing)))))
   ((string= property (if (eq type 'padding) "pl" "ml"))
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-left")) spacing)))))
   ;; Start (inline-start) - maps to left in LTR
   ((string= property (if (eq type 'padding) "ps" "ms"))
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-inline-start")) spacing)))))
   ;; End (inline-end) - maps to right in LTR
   ((string= property (if (eq type 'padding) "pe" "me"))
    (let ((spacing (etaf-tailwind--spacing-value value 'horizontal)))
      (when spacing
        (list (cons (intern (concat (symbol-name type) "-inline-end")) spacing)))))))

(defun etaf-tailwind-convert-size (property value)
  "转换尺寸类（width/height）到CSS。
Width使用px（水平方向），Height使用lh（垂直方向）。"
  (let* ((direction (if (eq property 'width) 'horizontal 'vertical))
         (size (cond
                ;; 使用spacing scale或任意数值
                ((or (cdr (assoc value etaf-tailwind-spacing-scale))
                     (string-match-p "^[0-9]+\\(\\.[0-9]+\\)?$" value))
                 (etaf-tailwind--spacing-value value direction))
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

(defun etaf-tailwind-property-to-css-property (tailwind-prop)
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

(defun etaf-tailwind-classes-to-css (class-names)
  "将多个Tailwind类名转换为CSS属性列表。
CLASS-NAMES是类名列表或空格分隔的字符串。
返回合并后的CSS属性alist。

示例：
  (etaf-tailwind-classes-to-css \"flex items-center bg-red-500 p-4\")
  ;; => ((display . \"flex\") (align-items . \"center\") 
  ;;     (background-color . \"#ef4444\") (padding . \"1rem\"))"
  (let ((classes (if (stringp class-names)
                     (split-string class-names)
                   class-names))
        (css-props '()))
    (dolist (class classes)
      (let ((props (etaf-tailwind-to-css class)))
        (when props
          (dolist (prop props)
            ;; 后面的类会覆盖前面的同名属性
            (setq css-props (assq-delete-all (car prop) css-props))
            (push prop css-props)))))
    (nreverse css-props)))

(defun etaf-tailwind-apply-css-to-node (node class-names)
  "将Tailwind类名转换为CSS并应用到DOM节点。
NODE是DOM节点，CLASS-NAMES是Tailwind类名列表或字符串。
这会将Tailwind类转换为内联样式添加到节点的style属性。

示例：
  (etaf-tailwind-apply-css-to-node node \"flex items-center bg-blue-500 p-4\")"
  (let ((css-props (etaf-tailwind-classes-to-css class-names)))
    (when css-props
      (etaf-dom-set-styles node css-props))))

(defun etaf-tailwind-css-to-string (css-props)
  "将CSS属性alist转换为CSS字符串。
CSS-PROPS是 ((property . value) ...) 格式的alist。

示例：
  (etaf-tailwind-css-to-string '((display . \"flex\") (padding . \"1rem\")))
  ;; => \"display: flex; padding: 1rem\""
  (mapconcat (lambda (prop)
               (format "%s: %s" (car prop) (cdr prop)))
             css-props "; "))

(provide 'etaf-tailwind)

;;; etaf-tailwind.el ends here
