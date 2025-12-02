# ETAF User Manual / 用户手册

This manual provides comprehensive guidance for using ETAF (Emacs Text-based Application Framework).

本手册提供 ETAF（Emacs Text-based Application Framework）的完整使用指南。

---

## Table of Contents / 目录

1. [Installation / 安装](#installation)
2. [Basic Concepts / 基础概念](#basic-concepts)
3. [TML Syntax / TML 语法](#tml-syntax)
4. [CSS Styling / CSS 样式](#css-styling)
5. [Template Directives / 模板指令](#template-directives)
6. [Component System / 组件系统](#component-system)
7. [Reactive System / 响应式系统](#reactive-system)
8. [Tailwind CSS / Tailwind CSS 支持](#tailwind-css)
9. [Layout System / 布局系统](#layout-system)
10. [ECSS Expressions / ECSS 表达式](#ecss-expressions)
11. [API Reference / API 参考](#api-reference)

---

<a name="installation"></a>
## 1. Installation / 安装

### Requirements / 依赖

- Emacs 27.1 or later
- `cl-lib` (built-in)
- `dom` (built-in)

### Installation Steps / 安装步骤

1. Clone the repository:
```bash
git clone https://github.com/Kinneyzhang/ETML.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/ETML")
(require 'etaf)
```

3. Verify installation:
```elisp
(etaf-paint-to-buffer "*test*"
  '(div "Hello ETAF!"))
```

---

<a name="basic-concepts"></a>
## 2. Basic Concepts / 基础概念

### Rendering Pipeline / 渲染流程

ETAF follows a browser-like rendering pipeline:

```
TML → DOM → CSSOM → Render Tree → Layout Tree → Buffer String
```

1. **TML** - Template Markup Language (S-expression based HTML-like syntax)
2. **DOM** - Document Object Model (alist-based tree structure)
3. **CSSOM** - CSS Object Model (parsed CSS rules and computed styles)
4. **Render Tree** - Visible elements with computed styles
5. **Layout Tree** - Elements with box model and position information
6. **Buffer String** - Final text with properties for display in Emacs buffer

### Main Entry Point / 主入口函数

```elisp
;; Render TML to a buffer
(etaf-paint-to-buffer BUFFER-NAME TML &optional DATA ECSS WIDTH HEIGHT)

;; Convert TML to styled string
(etaf-paint-string TML &optional DATA ECSS WIDTH HEIGHT)
```

---

<a name="tml-syntax"></a>
## 3. TML Syntax / TML 语法

TML (Template Markup Language) uses S-expressions to represent HTML-like structures.

### Basic Format / 基础格式

```elisp
(tag :attr1 value1 :attr2 value2 child1 child2 ...)
```

### Examples / 示例

```elisp
;; Simple element
(div "Hello World")

;; With attributes
(div :class "container" :id "main"
  "Content")

;; Nested elements
(div :class "card"
  (h1 "Title")
  (p "Description")
  (button :class "btn" "Click"))

;; With inline styles
(div :style "color: red; padding: 10px"
  "Styled content")

;; List format for styles
(div :style ((color . "red") (padding . "10px"))
  "Also styled")
```

### Text Content / 文本内容

Strings are rendered as text content:

```elisp
(p "This is paragraph text.")

(div
  "Multiple "
  "strings "
  "are concatenated.")
```

---

<a name="css-styling"></a>
## 4. CSS Styling / CSS 样式

### Inline Styles / 内联样式

```elisp
;; String format
(div :style "background: blue; color: white; padding: 10px"
  "Styled box")

;; Alist format
(div :style ((background . "blue")
             (color . "white")
             (padding . "10px"))
  "Styled box")
```

### Style Tags / style 标签

```elisp
(html
  (head
    (style "
      .container { width: 800px; margin: 0 auto; }
      .card { background: white; padding: 20px; border: 1px solid #ccc; }
      .card h1 { color: #333; }
    "))
  (body
    (div :class "container"
      (div :class "card"
        (h1 "Card Title")
        (p "Card content")))))
```

### Supported CSS Features / 支持的 CSS 功能

- **Selectors**: tag, class, ID, attribute, pseudo-class, combinators
- **Properties**: color, background, padding, margin, border, font, display, etc.
- **Cascade**: specificity calculation, !important, source order
- **Inheritance**: inheritable properties (color, font-*, etc.)
- **Media Queries**: @media rules with type and feature conditions

---

<a name="template-directives"></a>
## 5. Template Directives / 模板指令

ETAF supports Vue-style template directives for dynamic content.

### Text Interpolation / 文本插值

```elisp
(setq data '(:name "Alice" :count 42))

(etaf-paint-to-buffer "*demo*"
  '(div
     (p "Hello, {{ name }}!")
     (p "Count: {{ count }}"))
  data)
```

### Conditional Rendering / 条件渲染

```elisp
(setq data '(:loggedIn t :role "admin"))

;; e-if
(p :e-if "loggedIn" "Welcome back!")

;; e-else-if and e-else
(div
  (p :e-if "role == 'admin'" "Admin Panel")
  (p :e-else-if "role == 'user'" "User Dashboard")
  (p :e-else "Please login"))
```

### List Rendering / 列表渲染

```elisp
(setq data '(:items ("Apple" "Banana" "Cherry")))

;; Basic e-for
(ul
  (li :e-for "item in items" "{{ item }}"))

;; With index
(ul
  (li :e-for "(item, index) in items"
      "{{ index }}: {{ item }}"))
```

### Show/Hide / 显示隐藏

```elisp
;; e-show adds display:none when false
(div :e-show "isVisible" "Conditionally visible")
```

### Text Directive / 文本指令

```elisp
;; e-text sets text content from expression
(span :e-text "message")
```

---

<a name="component-system"></a>
## 6. Component System / 组件系统

ETAF provides a Vue3-style component system.

### Defining Components / 定义组件

```elisp
;; Simple component
(etaf-define-component my-button
  :props '(:label :type)
  :template '(button :class "btn btn-{{ type }}"
                     "{{ label }}"))

;; Component with setup function
(etaf-define-component counter
  :props '(:initial)
  :setup (lambda (props)
           (let* ((count (etaf-ref
                          (or (plist-get props :initial) 0)))
                  (increment (lambda ()
                               (etaf-ref-update count #'1+))))
             (list :count count
                   :increment increment)))
  :template (lambda (data)
              `(div :class "counter"
                    (span ,(format "Count: %s"
                                   (etaf-ref-get
                                    (plist-get data :count))))
                    (button :on-click ,(plist-get data :increment)
                            "+"))))
```

### Using Components / 使用组件

```elisp
;; Use component with props
(my-button :label "Submit" :type "primary")

;; With slot content
(my-card :title "Card Title"
  (p "This goes into the slot"))
```

### Component API / 组件 API

| Function | Description |
|----------|-------------|
| `etaf-define-component` | Define a new component |
| `etaf-component-get` | Get component definition |
| `etaf-component-defined-p` | Check if component exists |
| `etaf-component-list-all` | List all components |

---

<a name="reactive-system"></a>
## 7. Reactive System / 响应式系统

ETAF implements Vue3-style reactivity.

### Ref / 响应式引用

```elisp
;; Create ref
(setq count (etaf-ref 0))

;; Get value
(etaf-ref-get count)  ;; => 0

;; Set value
(etaf-ref-set count 5)

;; Update based on current value
(etaf-ref-update count #'1+)  ;; => 6
```

### Computed / 计算属性

```elisp
(let* ((count (etaf-ref 3))
       (doubled (etaf-computed
                  (lambda ()
                    (* 2 (etaf-ref-get count))))))
  (etaf-computed-get doubled)  ;; => 6
  (etaf-ref-set count 5)
  (etaf-computed-get doubled)) ;; => 10 (auto recomputed)
```

### Watch / 侦听器

```elisp
;; Watch a reactive source
(let* ((count (etaf-ref 0))
       (stop (etaf-watch count
               (lambda (new old)
                 (message "Changed: %s -> %s" old new)))))
  (etaf-ref-set count 1)  ;; triggers callback
  (funcall stop)               ;; stop watching
  (etaf-ref-set count 2)) ;; no callback
```

### Watch Effect / 副作用

```elisp
(let* ((count (etaf-ref 0))
       (stop (etaf-etml-watch-effect
               (lambda ()
                 ;; Auto-tracks dependencies
                 (message "Count is: %s"
                          (etaf-ref-get count))))))
  (etaf-ref-set count 1)  ;; re-runs effect
  (funcall stop))              ;; cleanup
```

### Reactive Object / 响应式对象

```elisp
(let ((state (etaf-reactive '(:name "Alice" :age 30))))
  (etaf-reactive-get state :name)    ;; => "Alice"
  (etaf-reactive-set state :age 31)
  (etaf-reactive-to-plist state))    ;; => (:name "Alice" :age 31)
```

---

<a name="tailwind-css"></a>
## 8. Tailwind CSS / Tailwind CSS 支持

ETAF has built-in support for Tailwind CSS utility classes.

### Basic Usage / 基础用法

```elisp
(div :class "flex items-center justify-between p-4 bg-blue-500"
  (span :class "text-white font-bold" "Title")
  (button :class "bg-white text-blue-500 px-4 py-2 rounded" "Action"))
```

### Supported Features / 支持的功能

| Feature | Examples |
|---------|----------|
| Colors | `bg-red-500`, `text-gray-700`, `border-blue-300` |
| Spacing | `p-4`, `mx-auto`, `mt-2`, `gap-4` |
| Typography | `text-lg`, `font-bold`, `text-center` |
| Flexbox | `flex`, `items-center`, `justify-between` |
| Display | `block`, `inline-block`, `hidden` |
| Border | `border`, `rounded-lg`, `border-2` |
| Shadow | `shadow-md`, `shadow-lg` |

### Responsive Prefixes / 响应式前缀

```elisp
(div :class "w-full md:w-1/2 lg:w-1/3"
  "Responsive width")
```

Prefixes: `sm:`, `md:`, `lg:`, `xl:`, `2xl:`

### State Variants / 状态变体

```elisp
(button :class "bg-blue-500 hover:bg-blue-700 focus:ring-2"
  "Hover me")
```

Variants: `hover:`, `focus:`, `active:`, `disabled:`

### Arbitrary Values / 任意值

```elisp
(div :class "bg-[#1da1f2] w-[200px]"
  "Custom values")
```

---

<a name="layout-system"></a>
## 9. Layout System / 布局系统

### Box Model / 盒模型

ETAF implements the CSS box model:

```
┌─────────────────────────────────────┐
│           margin (外边距)            │
│  ┌───────────────────────────────┐  │
│  │      border (边框)             │  │
│  │  ┌─────────────────────────┐  │  │
│  │  │   padding (内边距)       │  │  │
│  │  │  ┌───────────────────┐  │  │  │
│  │  │  │  content (内容)   │  │  │  │
│  │  │  └───────────────────┘  │  │  │
│  │  └─────────────────────────┘  │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

### Display Types / 显示类型

```elisp
;; Block element
(div :style "display: block" "Block")

;; Inline element
(span :style "display: inline" "Inline")

;; Flex container
(div :style "display: flex; gap: 10px"
  (div "Item 1")
  (div "Item 2"))
```

### Flexbox Layout / Flexbox 布局

```elisp
(div :style "display: flex; flex-direction: row; justify-content: space-between"
  (div "Left")
  (div "Right"))

;; Or with Tailwind
(div :class "flex flex-row justify-between"
  (div "Left")
  (div "Right"))
```

---

<a name="ecss-expressions"></a>
## 10. ECSS Expressions / ECSS 表达式

ECSS provides Lisp-style CSS expressions, similar to `rx` for regex.

### CSS Rules / CSS 规则

```elisp
(require 'etaf-ecss)

;; Single rule
(etaf-ecss ".card"
  '(background "white")
  '(padding 20)
  '(border 1 solid "#ccc"))
;; => ".card { background: white; padding: 20px; border: 1px solid #ccc; }"

;; Stylesheet
(etaf-ecss
  '(".container" (width 800) (margin 0 auto))
  '(".card" (background "white") (padding 20))
  '(".btn" (padding 10 20) (background "blue") (color "white")))
```

### Selector Expressions / 选择器表达式

```elisp
(etaf-ecss-selector 'div)                    ;; => "div"
(etaf-ecss-selector '(class "box"))          ;; => ".box"
(etaf-ecss-selector '(id "main"))            ;; => "#main"
(etaf-ecss-selector '(descendant "nav" "a")) ;; => "nav a"
(etaf-ecss-selector '(child "ul" "li"))      ;; => "ul > li"
(etaf-ecss-selector '(and (tag "div") (class "box"))) ;; => "div.box"
```

### Property Expressions / 属性表达式

```elisp
(etaf-ecss-property 'padding 10)       ;; => "padding: 10px"
(etaf-ecss-property 'margin 0 'auto)   ;; => "margin: 0 auto"
(etaf-ecss-property 'height 5)         ;; => "height: 5lh" (vertical uses lh)
```

### CSS Units / CSS 单位

ETAF supports the following CSS units:

| Unit | Direction | Description |
|------|-----------|-------------|
| `px` | Horizontal | Pixel value |
| `cw` | Horizontal | Character width (uses `frame-char-width` as base value) |
| `lh` | Vertical | Line height (number of lines) |
| `%` | Both | Percentage (relative to parent) |
| `em` | Both | Relative unit (1em = 16px for width, 1 line for height) |

```elisp
;; Horizontal dimension examples
"100px"   ;; 100 pixels
"10cw"    ;; 10 character widths (10 * frame-char-width)
"50%"     ;; 50% of parent width

;; Vertical dimension examples (use line count)
"5lh"     ;; 5 lines
"3"       ;; 3 lines (number without unit)
```

### Integration with TML / 与 TML 集成

```elisp
;; Generate :style value (alist format)
(div :style (etaf-ecss-props '(background "red") '(padding 10))
  "content")

;; Generate :style value (string format)
(div :style (etaf-ecss-style '(color "red") '(padding 10))
  "content")
```

---

<a name="api-reference"></a>
## 11. API Reference / API 参考

### Main Functions / 主要函数

| Function | Description |
|----------|-------------|
| `etaf-paint-to-buffer` | Render TML to a buffer |
| `etaf-paint-string | Convert TML to styled string |
| `etaf-etml-to-dom` | Convert TML to DOM |
| `etaf-etml-render` | Render template with data |

### CSS Functions / CSS 函数

| Function | Description |
|----------|-------------|
| `etaf-css-build-cssom` | Build CSSOM from DOM |
| `etaf-css-get-computed-style` | Get computed style for node |
| `etaf-css-add-stylesheet` | Add external CSS to CSSOM |

### Render Functions / 渲染函数

| Function | Description |
|----------|-------------|
| `etaf-render-build-tree` | Build render tree from DOM |
| `etaf-render-get-style` | Get style property from render node |

### Layout Functions / 布局函数

| Function | Description |
|----------|-------------|
| `etaf-layout-build-tree` | Build layout tree from render tree |
| `etaf-layout-to-string` | Convert layout tree to buffer string |
| `etaf-layout-get-box-model` | Get box model from layout node |

### Component Functions / 组件函数

| Function | Description |
|----------|-------------|
| `etaf-define-component` | Define a component |
| `etaf-component-get` | Get component definition |
| `etaf-component-defined-p` | Check if component exists |

### Reactive Functions / 响应式函数

| Function | Description |
|----------|-------------|
| `etaf-ref` | Create reactive reference |
| `etaf-ref-get` | Get ref value |
| `etaf-ref-set` | Set ref value |
| `etaf-computed` | Create computed value |
| `etaf-watch` | Watch reactive source |
| `etaf-etml-watch-effect` | Create auto-tracking effect |
| `etaf-reactive` | Create reactive object |

---

## Need Help? / 需要帮助？

- Check the [examples](../examples/) directory for working code examples
- Read the [Architecture documentation](ARCHITECTURE.md) for system design details
- Read the [Data Structures documentation](DATA-STRUCTURES.md) for detailed data structure reference
- File issues on GitHub for bugs or feature requests
