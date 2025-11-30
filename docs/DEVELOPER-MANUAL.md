# ETAF Developer Manual / 开发者手册

This manual provides architecture details and extension guides for ETAF contributors and developers.

本手册为 ETAF 贡献者和开发者提供架构详情和扩展指南。

---

## Table of Contents / 目录

1. [Architecture Overview / 架构概览](#architecture-overview)
2. [Module Structure / 模块结构](#module-structure)
3. [Data Structures / 数据结构](#data-structures)
4. [Rendering Pipeline / 渲染流程](#rendering-pipeline)
5. [CSS System / CSS 系统](#css-system)
6. [Layout Engine / 布局引擎](#layout-engine)
7. [Extension Points / 扩展点](#extension-points)
8. [Testing / 测试](#testing)
9. [Contributing / 贡献指南](#contributing)

---

<a name="architecture-overview"></a>
## 1. Architecture Overview / 架构概览

### System Architecture / 系统架构

```
┌─────────────────────────────────────────────────────────────────┐
│                         etaf.el (Entry)                          │
│                    etaf-string: TML → Rendered String            │
└───────────────────────────────┬─────────────────────────────────┘
                                │
        ┌───────────────────────┼───────────────────────┐
        │                       │                       │
        ▼                       ▼                       │
┌───────────────────┐  ┌─────────────────┐              │
│    etaf-etml.el   │  │  etaf-css.el    │              │
│  TML → DOM        │  │  CSS Main Entry │              │
│  + Template       │  │                 │              │
└───────┬───────────┘  └────────┬────────┘              │
        │                       │                       │
        ▼                       ▼                       │
┌───────────────┐      ┌─────────────────┐              │
│  etaf-dom.el  │◄─────│  CSS Submodules │              │
│  DOM Ops      │      │  ├ selector     │              │
└───────────────┘      │  ├ parser       │              │
                       │  ├ cascade      │              │
                       │  ├ inheritance  │              │
                       │  ├ shorthand    │              │
                       │  ├ media        │              │
                       │  ├ cache        │              │
                       │  ├ index        │              │
                       │  └ face         │              │
                       └────────┬────────┘              │
                                │                       │
        ┌───────────────────────┼───────────────────────┘
        │                       │
        ▼                       ▼
┌───────────────┐      ┌─────────────────┐
│etaf-render.el │◄─────│etaf-tailwind.el │
│ Render Tree   │      │ Tailwind CSS    │
└───────┬───────┘      └─────────────────┘
        │
        ▼
┌───────────────┐
│etaf-layout.el │
│ Layout Calc   │
└───────┬───────┘
        │
        ▼
┌─────────────────────────────────────────┐
│           Layout Submodules             │
│  ├ etaf-layout-parse.el (Value Parse)   │
│  ├ etaf-layout-box.el (Box Model)       │
│  ├ etaf-layout-flex.el (Flexbox)        │
│  └ etaf-layout-string.el (Output)       │
└─────────────────────────────────────────┘
```

### Data Flow / 数据流

```
1. TML (Template Markup Language)
   (div :class "container" (p "Hello"))
        │
        ▼ etaf-etml-to-dom
2. DOM (Document Object Model)
   (div ((class . "container")) (p nil "Hello"))
        │
        ▼ etaf-css-build-cssom + etaf-render-build-tree
3. Render Tree
   Contains computed CSS styles.
        │
        ▼ etaf-layout-build-tree
4. Layout Tree
   Contains box model info (position, size).
        │
        ▼ etaf-layout-to-string
5. String (with text properties)
   Ready for insertion into Emacs buffer
```

---

<a name="module-structure"></a>
## 2. Module Structure / 模块结构

### Core Modules / 核心模块

| Module | Purpose |
|--------|---------|
| `etaf.el` | Main entry point, high-level API |
| `etaf-etml.el` | TML→DOM, templates, components, reactivity |
| `etaf-etml-tag.el` | Custom tag definition system |
| `etaf-dom.el` | DOM operations, queries, traversal |

### CSS Modules / CSS 模块

| Module | Purpose |
|--------|---------|
| `etaf-css.el` | CSSOM main entry, style computation |
| `etaf-css-parser.el` | CSS parsing (!important, @media) |
| `etaf-css-selector.el` | Selector parsing and matching |
| `etaf-css-cascade.el` | Cascade algorithm, specificity |
| `etaf-css-inheritance.el` | Property inheritance |
| `etaf-css-shorthand.el` | Shorthand property expansion |
| `etaf-css-media.el` | Media query support |
| `etaf-css-cache.el` | Computed style caching |
| `etaf-css-index.el` | Rule indexing optimization |
| `etaf-css-face.el` | CSS to Emacs face mapping |

### Layout Modules / 布局模块

| Module | Purpose |
|--------|---------|
| `etaf-layout.el` | Layout main coordinator |
| `etaf-layout-parse.el` | CSS value parsing |
| `etaf-layout-box.el` | Box model data structures |
| `etaf-layout-flex.el` | Flexbox layout algorithm |
| `etaf-layout-string.el` | Layout to string conversion |

### Other Modules / 其他模块

| Module | Purpose |
|--------|---------|
| `etaf-render.el` | Render tree construction |
| `etaf-tailwind.el` | Tailwind CSS support |
| `etaf-ecss.el` | Lisp-style CSS expressions |
| `etaf-utils.el` | Utility functions |
| `etaf-pixel.el` | Pixel-level string operations |

---

<a name="data-structures"></a>
## 3. Data Structures / 数据结构

### TML Format / TML 格式

```elisp
;; Format: (tag :attr1 value1 :attr2 value2 child1 child2 ...)
(div :class "container" :id "main"
  (h1 :class "title" "Title Text")
  (p "Paragraph content"))
```

### DOM Format / DOM 格式

```elisp
;; Format: (tag ((attr1 . value1) (attr2 . value2)) child1 child2 ...)
(div ((class . "container") (id . "main"))
  (h1 ((class . "title")) "Title Text")
  (p nil "Paragraph content"))
```

### CSSOM Structure / CSSOM 结构

```elisp
(:inline-rules (...)      ;; Inline style rules
 :style-rules (...)       ;; Stylesheet rules
 :all-rules (...)         ;; All rules in order
 :rule-index (:by-tag <hash> :by-class <hash> :by-id <hash>)
 :cache <hash-table>      ;; Computed style cache
 :media-env ((type . screen) (width . 1024) ...))
```

### CSS Rule / CSS 规则

```elisp
(:selector "div.class#id"
 :declarations ((property value important) ...)
 :specificity (id-count class-count type-count)
 :source inline|style-tag|external
 :media "screen and (min-width: 768px)"  ;; optional
 :node <dom-node>)  ;; inline styles only
```

### Render Node / 渲染节点

```elisp
;; Uses DOM format with render- prefixed attributes
(tag ((render-style . ((color . "red") ...))
      (render-display . "block")
      (class . "foo")     ;; original DOM attribute
      (id . "bar"))       ;; original DOM attribute
  child1 child2 ...)
```

### Layout Node / 布局节点

```elisp
;; Uses DOM format with layout- prefixed attributes
(tag ((layout-box-model . <box-model>)
      (render-style . ((color . "red") ...))
      (render-display . "block"))
  child1 child2 ...)
```

### Box Model / 盒模型

```elisp
(:box-sizing "content-box"|"border-box"
 :content (:width <number> :height <number>)
 :padding (:top <n> :right <n> :bottom <n> :left <n>)
 :border (:top-width <n> :right-width <n> ...
          :top-color <color> :right-color <color> ...)
 :margin (:top <n> :right <n> :bottom <n> :left <n>))
```

---

<a name="rendering-pipeline"></a>
## 4. Rendering Pipeline / 渲染流程

### Step 1: TML to DOM / TML 转 DOM

```elisp
(defun etaf-etml-to-dom (template &optional data)
  "Render TEMPLATE with DATA and convert to DOM."
  (etaf-etml--to-dom (etaf-etml-render template data)))
```

Key functions:
- `etaf-plist-to-alist` - Convert plist attributes to alist
- `etaf-etml-render` - Process template directives
- `etaf-etml--to-dom` - Recursive TML to DOM conversion

### Step 2: Build CSSOM / 构建 CSSOM

```elisp
(defun etaf-css-build-cssom (dom &optional media-env)
  "Build CSSOM from DOM tree.")
```

Key functions:
- `etaf-css-extract-inline-styles` - Extract style attributes
- `etaf-css-extract-style-tags` - Parse <style> tags
- `etaf-css-index-build` - Build rule index
- `etaf-css-cache-create` - Initialize style cache

### Step 3: Build Render Tree / 构建渲染树

```elisp
(defun etaf-render-build-tree (dom cssom)
  "Build render tree from DOM and CSSOM.")
```

Key functions:
- `etaf-css-get-computed-style` - Compute final styles
- `etaf-render-node-visible-p` - Filter invisible nodes
- `etaf-render-create-node` - Create render node

### Step 4: Build Layout Tree / 构建布局树

```elisp
(defun etaf-layout-build-tree (render-tree viewport)
  "Build layout tree from render tree.")
```

Key functions:
- `etaf-layout-compute-box-model` - Calculate box model
- `etaf-layout-block-formatting-context` - Block layout
- `etaf-layout-flex-format` - Flexbox layout

### Step 5: Generate String / 生成字符串

```elisp
(defun etaf-layout-to-string (layout-tree)
  "Convert layout tree to buffer string.")
```

Key functions:
- `etaf-layout-string-render` - Recursive rendering
- `etaf-css-apply-face-to-string` - Apply Emacs face properties

---

<a name="css-system"></a>
## 5. CSS System / CSS 系统

### Selector Parsing / 选择器解析

The selector parser (`etaf-css-selector.el`) tokenizes and parses CSS selectors into AST:

```elisp
(etaf-css-selector-parse "div.class#id:hover")
;; => (:type root :nodes ((:type selector :nodes (...))))
```

Supported selectors:
- Tag: `div`, `p`, `span`
- Class: `.class-name`
- ID: `#element-id`
- Attribute: `[attr]`, `[attr=value]`, `[attr^=prefix]`
- Pseudo-class: `:first-child`, `:nth-child(n)`, `:hover`
- Combinators: descendant (space), child (`>`), sibling (`+`, `~`)

### Cascade Algorithm / 层叠算法

Priority order (high to low):
1. `!important` inline styles
2. `!important` stylesheet styles
3. Normal inline styles
4. Normal stylesheet styles (by specificity and order)

Specificity format: `(id-count class-count type-count)`

### Shorthand Expansion / 复合属性展开

```elisp
;; border: 1px solid red
;; expands to:
;; border-top-width: 1px
;; border-right-width: 1px
;; border-top-style: solid
;; border-top-color: red
;; ... etc
```

Supported shorthands:
- `margin`, `padding` (1-4 values)
- `border`, `border-width`, `border-style`, `border-color`
- `flex`, `flex-flow`, `gap`

---

<a name="layout-engine"></a>
## 6. Layout Engine / 布局引擎

### Block Layout / 块级布局

Block elements stack vertically and fill parent width:

```elisp
(defun etaf-layout-block-formatting-context (render-node parent-context)
  "Layout block elements in normal flow.")
```

### Flexbox Layout / Flexbox 布局

```elisp
(defun etaf-layout-flex-format (render-node parent-context)
  "Layout flex container and items.")
```

Supported properties:
- `flex-direction`: row, column
- `justify-content`: flex-start, flex-end, center, space-between, space-around
- `align-items`: flex-start, flex-end, center, stretch
- `flex-wrap`: nowrap, wrap
- `gap`: row-gap, column-gap

### Emacs-specific Units / Emacs 特有单位

- Horizontal: `px` (pixels)
- Vertical: `lh` (line-height units)

This is because Emacs buffer rendering is line-based for vertical positioning.

---

<a name="extension-points"></a>
## 7. Extension Points / 扩展点

### Custom Components / 自定义组件

```elisp
(etaf-etml-define-component my-component
  :props '(:prop1 :prop2)
  :setup (lambda (props) ...)
  :template '(div ...))
```

### Custom Tags / 自定义标签

```elisp
(define-etaf-etml-tag my-tag
  :display 'block
  :default-style '((background-color . "blue"))
  :hover-style '((background-color . "darkblue"))
  :on-click (lambda (event) (message "Clicked!")))
```

### Custom CSS Properties / 自定义 CSS 属性

Add to `etaf-css-inheritance.el` for inheritable properties:

```elisp
(defconst etaf-css-inheritable-properties
  '(color font-family font-size ... my-custom-property))
```

### Custom Layout / 自定义布局

Create new formatting context in `etaf-layout.el`:

```elisp
(defun etaf-layout-my-format (render-node parent-context)
  "Custom layout algorithm."
  ...)
```

---

<a name="testing"></a>
## 8. Testing / 测试

### Test Files / 测试文件

| File | Coverage |
|------|----------|
| `etaf-css-tests.el` | CSS main functionality |
| `etaf-css-selector-tests.el` | Selector parsing/matching |
| `etaf-css-cascade-tests.el` | Cascade algorithm |
| `etaf-etml-tests.el` | Template and components |
| `etaf-layout-tests.el` | Layout system |
| `etaf-tailwind-tests.el` | Tailwind CSS |

### Running Tests / 运行测试

```bash
cd tests
emacs -batch -l etaf-ert.el -l etaf-css-tests.el -f ert-run-tests-batch-and-exit
```

### Writing Tests / 编写测试

```elisp
(ert-deftest test-my-feature ()
  "Test description."
  (let ((dom (etaf-etml-to-dom '(div :class "test" "content"))))
    (should (eq (dom-tag dom) 'div))
    (should (string= (dom-attr dom 'class) "test"))))
```

---

<a name="contributing"></a>
## 9. Contributing / 贡献指南

### Code Style / 代码风格

- Use `etaf-` prefix for all public functions
- Use `etaf--` prefix for internal functions
- Document all public functions with docstrings
- Follow Emacs Lisp coding conventions

### Commit Messages / 提交信息

```
type(scope): description

- type: feat, fix, docs, style, refactor, test, chore
- scope: css, layout, etml, render, etc.
```

### Pull Request Process / PR 流程

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Ensure all tests pass
5. Update documentation if needed
6. Submit pull request

### Areas for Contribution / 可贡献的领域

- [ ] Grid layout support
- [ ] Animation system
- [ ] More CSS properties
- [ ] Performance optimizations
- [ ] Additional Tailwind utilities
- [ ] Documentation improvements
- [ ] Bug fixes

---

## Contact / 联系

For questions or discussions, please open an issue on GitHub.

如有问题或讨论，请在 GitHub 上创建 issue。
