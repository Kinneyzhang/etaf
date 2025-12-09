# ETAF - Emacs Text-based Application Framework

<p align="center">
  <strong>🚀 Build beautiful text-based UIs in Emacs with HTML/CSS-like syntax</strong>
</p>

<p align="center">
  <a href="#english">English</a> | <a href="#中文">中文</a>
</p>

---

<a name="english"></a>

## Overview

ETAF (Emacs Text-based Application Framework) is a comprehensive framework for building rich text-based user interfaces in Emacs. It brings web development concepts like DOM, CSS, and reactive components to the Emacs ecosystem, enabling developers to create sophisticated UI components using familiar HTML/CSS-like syntax.

### Key Features

- 🏗️ **HTML-like Templating** - Write UI using S-expression based TML (Template Markup Language)
- 🎨 **CSS Support** - Full CSS parsing, cascade algorithm, and computed styles
- 📦 **Component System** - Vue 2 Options API and Vue 3 Composition API support with reactive components
- 🔄 **Reactive System** - ref, computed, watch, and watchEffect for state management
- 🌳 **Virtual DOM** - Vue 3-inspired virtual DOM with diff/patch algorithm for efficient updates
- 🎯 **Tailwind CSS** - Built-in support for Tailwind utility classes
- 📐 **Layout Engine** - Box model and Flexbox layout support
- 📝 **Smart Typesetting** - Integrated Knuth-Plass algorithm for hybrid CJK and Latin text justification
- ⚡ **Performance Optimized** - Rule indexing and style caching
- 📊 **Performance Monitoring** - Built-in profiling tool to optimize first-screen loading time

### Rendering Pipeline

```
ETML Template → Compiler (etaf-etml) → VNode Tree (Virtual DOM with metadata)
                                              ↓
                                      Renderer extracts clean DOM
                                              ↓
                                            CSSOM → Render Tree → Layout Tree → Buffer String
```

The pipeline follows Vue 3's architecture:
1. **Template (ETML)**: S-expression based markup
2. **Compiler**: Converts ETML to VNode tree with metadata
3. **Virtual DOM (VNode)**: Stores tag metadata, event handlers, state
4. **Renderer**: Extracts clean DOM from VNode
5. **CSSOM, Render Tree, Layout**: Existing CSS and layout pipeline
6. **Buffer String**: Final text with properties for interactivity

## Quick Start

### Basic Usage

```elisp
(require 'etaf)

;; Simple rendering
(etaf-paint-to-buffer "*demo*"
  '(div :class "container"
     (h1 :style "color: blue" "Hello ETAF!")
     (p "Build beautiful UIs in Emacs")))

;; With Tailwind CSS classes
(etaf-paint-to-buffer "*demo*"
  '(div :class "flex items-center p-2 bg-blue-500"
     (span :class "text-white font-bold" "Styled with Tailwind!")))
```

### Template Directives

```elisp
(setq my-data '(:name "Alice" :items ("Apple" "Banana" "Cherry")))

(etaf-paint-to-buffer "*demo*"
  '(div
     (h1 "Hello, {{ name }}!")
     (ul
       (li :e-for "item in items" "{{ item }}")))
  my-data)
```

### Component System

```elisp
;; Define a component
(etaf-define-component my-button
  :props '(:label :disabled)
  :template '(button :class "btn" "{{ label }}"))

;; Use the component
(etaf-paint-to-buffer "*demo*"
  '(my-button :label "Click Me"))
```

### Reactive State

```elisp
(let* ((count (etaf-ref 0))
       (doubled (etaf-computed
                  (lambda () (* 2 (etaf-ref-get count))))))
  (etaf-ref-get count)      ;; => 0
  (etaf-ref-set count 5)
  (etaf-computed-get doubled)) ;; => 10
```

### Table Component

```elisp
(require 'etaf-table)

;; Define columns
(setq columns
  '((:prop "name" :label "Name" :width 150 :sortable t)
    (:prop "age" :label "Age" :width 80 :sortable t)
    (:prop "email" :label "Email" :width 200)
    (:prop "status" :label "Status" :width 100
     :formatter (lambda (row col value)
                 (if (equal value "active") "✓ Active" "✗ Inactive")))))

;; Define data
(setq data
  '((:id 1 :name "Alice" :age 28 :email "alice@example.com" :status "active")
    (:id 2 :name "Bob" :age 32 :email "bob@example.com" :status "inactive")))

;; Render table with features
(etaf-paint-to-buffer "*table-demo*"
  '(etaf-table :data data
               :columns columns
               :stripe t
               :border t
               :show-selection t
               :show-pagination t
               :page-size 10
               :row-key "id"))
```

## Documentation

| Document | Description |
|----------|-------------|
| [User Manual](docs/USER-MANUAL.md) | Complete guide for using ETAF |
| [Developer Manual](docs/DEVELOPER-MANUAL.md) | Architecture and extension guide |
| [Component System](docs/COMPONENT-SYSTEM.md) | Vue 3-style component system and reactive state management |
| [Architecture](docs/ARCHITECTURE.md) | System architecture and module relationships |
| [Data Structures](docs/DATA-STRUCTURES.md) | Detailed data structure documentation |
| [Event Model](docs/EVENT-MODEL.md) | Interactive pseudo-classes and event system |
| [Virtual DOM](docs/VIRTUAL-DOM.md) | Vue 3-inspired virtual DOM system |
| [Performance Monitoring](docs/ETAF-PERF.md) | Performance profiling tool for optimizing first-screen loading |
| [ETAF-EORM](docs/ETAF-EORM.md) | Multi-database ORM (SQLite, PostgreSQL, MySQL) inspired by Diesel |

## Installation

1. Clone the repository:
```bash
git clone https://github.com/Kinneyzhang/etaf.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/etaf")
(require 'etaf)
```

## Examples

Run interactive component demos:
```elisp
;; Component system examples - Options API and Composition API
(load-file "examples/etaf-component-examples.el")
M-x etaf-component-demo
```

Example files:
- `examples/etaf-component-examples.el` - Component system examples (Options API & Composition API)
- `examples/etaf-table-example.el` - Table component examples (sorting, pagination, selection)
- `examples/etaf-tailwind-example.el` - Tailwind CSS examples
- `examples/etaf-layout-example.el` - Layout system examples
- `examples/etaf-render-example.el` - Render examples
- `examples/etaf-css-example.el` - CSS examples

## License

GNU General Public License v3.0 or later.

---

<a name="中文"></a>

## 概述

ETAF（Emacs Text-based Application Framework）是一个在 Emacs 中构建丰富文本界面的综合框架。它将 DOM、CSS 和响应式组件等 Web 开发概念引入 Emacs 生态系统，使开发者能够使用熟悉的 HTML/CSS 语法创建复杂的 UI 组件。

### 核心特性

- 🏗️ **类 HTML 模板** - 使用基于 S-expression 的 TML（模板标记语言）编写 UI
- 🎨 **CSS 支持** - 完整的 CSS 解析、层叠算法和计算样式
- 📦 **组件系统** - 同时支持 Vue 2 选项式 API 和 Vue 3 组合式 API 的响应式组件
- 🔄 **响应式系统** - ref、computed、watch 和 watchEffect 状态管理
- 🌳 **虚拟 DOM** - 参考 Vue 3 设计的虚拟 DOM，支持 diff/patch 算法实现高效更新
- 🎯 **Tailwind CSS** - 内置 Tailwind 工具类支持
- 📐 **布局引擎** - 盒模型和 Flexbox 布局支持
- 📝 **智能排版** - 集成 Knuth-Plass 算法，支持 CJK 与拉丁系语言的混合排版
- ⚡ **性能优化** - 规则索引和样式缓存
- 📊 **性能监控** - 内置性能分析工具，用于优化首屏加载时间

### 渲染流程

## 快速开始

### 基础用法

```elisp
(require 'etaf)

;; 简单渲染
(etaf-paint-to-buffer "*demo*"
  '(div :class "container"
     (h1 :style "color: blue" "Hello ETAF!")
     (p "在 Emacs 中构建精美 UI")))

;; 使用 Tailwind CSS 类
(etaf-paint-to-buffer "*demo*"
  '(div :class "flex items-center p-2 bg-blue-500"
     (span :class "text-white font-bold" "Tailwind 样式!")))
```

### 模板指令

ETAF 支持 Vue 风格的模板指令：

| 指令 | 说明 | 示例 |
|------|------|------|
| `{{ expr }}` | 文本插值 | `"Hello, {{ name }}"` |
| `:e-if` | 条件渲染 | `(p :e-if "visible" "Text")` |
| `:e-else-if` | 多条件渲染 | `(p :e-else-if "other" "Alt")` |
| `:e-else` | 默认分支 | `(p :e-else "Default")` |
| `:e-for` | 列表渲染 | `(li :e-for "item in items" "{{ item }}")` |
| `:e-show` | 显示/隐藏 | `(div :e-show "visible" "Content")` |

```elisp
(setq my-data '(:name "Alice" :items ("苹果" "香蕉" "樱桃")))

(etaf-paint-to-buffer "*demo*"
  '(div
     (h1 "你好，{{ name }}！")
     (ul
       (li :e-for "item in items" "{{ item }}")))
  my-data)
```

### 组件系统

```elisp
;; 定义组件
(etaf-define-component my-button
  :props '(:label :disabled)
  :template '(button :class "btn" "{{ label }}"))

;; 使用组件
(etaf-paint-to-buffer "*demo*"
  '(my-button :label "点击我"))
```

### 响应式系统

```elisp
;; 创建响应式引用
(let* ((count (etaf-ref 0))
       (doubled (etaf-computed
                  (lambda () (* 2 (etaf-ref-get count))))))
  (etaf-ref-get count)      ;; => 0
  (etaf-ref-set count 5)
  (etaf-computed-get doubled)) ;; => 10
```

### 表格组件

```elisp
(require 'etaf-table)

;; 定义列
(setq columns
  '((:prop "name" :label "姓名" :width 150 :sortable t)
    (:prop "age" :label "年龄" :width 80 :sortable t)
    (:prop "email" :label "邮箱" :width 200)
    (:prop "status" :label "状态" :width 100
     :formatter (lambda (row col value)
                 (if (equal value "active") "✓ 活跃" "✗ 停用")))))

;; 定义数据
(setq data
  '((:id 1 :name "张三" :age 28 :email "zhang@example.com" :status "active")
    (:id 2 :name "李四" :age 32 :email "li@example.com" :status "inactive")))

;; 渲染带功能的表格
(etaf-paint-to-buffer "*table-demo*"
  '(etaf-table :data data
               :columns columns
               :stripe t
               :border t
               :show-selection t
               :show-pagination t
               :page-size 10
               :row-key "id"))
```

### Tailwind CSS 支持

```elisp
;; 直接使用 Tailwind 类
(etaf-paint-to-buffer "*demo*"
  '(div :class "flex items-center justify-between bg-white rounded-lg shadow-md p-2"
     (h1 :class "text-lg font-bold text-gray-900" "标题")
     (button :class "bg-blue-500 text-white px-2 py-1 rounded" "按钮")))
```

支持的 Tailwind 功能：
- 响应式前缀：`sm:`, `md:`, `lg:`, `xl:`, `2xl:`
- 状态变体：`hover:`, `focus:`, `active:` (需要 etaf-event 模块)
- 颜色系统：完整的 Tailwind 调色板
- 间距、Flexbox、圆角、阴影等
- 水平方向默认使用字符宽度(cw)，使用px后缀指定像素（如 `w-20px`）

### 交互式伪类和事件模型

ETAF 提供完整的事件模型来支持交互式伪类选择器：

```elisp
(require 'etaf-event)

;; 初始化事件系统
(etaf-event-init)

;; 注册可交互元素（需要 uuid 属性）
(let ((button '(button ((uuid . "btn-1") (class . "primary")) "Click Me")))
  (etaf-event-register-element "btn-1" button 100 120)
  
  ;; 添加事件监听器
  (etaf-event-add-listener "btn-1" 'hover-enter
    (lambda (uuid data)
      (message "Button hovered!")))
  
  ;; CSS 选择器会自动使用事件状态
  ;; button:hover 只在鼠标悬停时匹配
  (etaf-css-selector-query dom "button:hover"))
```

支持的交互式伪类：
- `:hover` - 鼠标悬停
- `:active` - 激活状态（鼠标按下）
- `:focus` - 焦点状态
- `:disabled` / `:enabled` - 禁用/启用状态

详见 [事件模型文档](docs/EVENT-MODEL.md)。

### ECSS：Emacs 风格 CSS

ECSS 提供统一的字符串格式来表达 CSS 规则，选择器使用原生 CSS 语法，样式属性使用 Tailwind 类名。

```elisp
(require 'etaf-ecss)

;; 统一格式（推荐）：选择器{Tailwind类名}
(etaf-ecss "div>p:nth-child(odd){pl-6px pr-2 py-1 border border-gray-500}")
;; => "div>p:nth-child(odd) { padding-left: 6px; padding-right: 2cw; ... }"

(etaf-ecss ".card{flex items-center bg-blue-500 p-4}")
;; => ".card { display: flex; align-items: center; ... }"

;; 构建样式表
(etaf-ecss
  ".container{flex items-center w-800px}"
  ".box{bg-blue-500 p-4}"
  "nav>a{text-white}")
```

### 智能文本排版：etaf-kp

ETAF 集成了 Knuth-Plass 排版算法（etaf-kp），实现了 CJK 与拉丁系语言的混合排版，支持智能断词和文本对齐。

```elisp
(require 'etaf-kp)

;; 设置排版语言（默认为 "en_US"）
(setq etaf-kp-latin-lang "en_US")

;; 设置排版参数（可选）
;; 参数依次为：拉丁语单词间的理想/拉伸/压缩像素宽度，
;;           拉丁语与 CJK 间的理想/拉伸/压缩像素宽度，
;;           CJK 字符间的理想/拉伸/压缩像素宽度
(etaf-kp-param-set 7 3 2 5 2 1 0 2 0)

;; 将文本按指定像素宽度排版（自动换行和对齐）
(etaf-kp-pixel-justify
 "This is a test string with English words and 中文字符 mixed together."
 400)

;; 在像素范围内寻找最优排版
(etaf-kp-pixel-range-justify
 "测试文本 test text 测试"
 300 500)  ;; 返回 (排版后文本 . 最优像素值)

;; 在 ETAF 布局中使用（通过 etaf-pixel-wrap）
(etaf-paint-to-buffer "*demo*"
  '(div :style "width: 400px"
     (p "Long text that will be automatically wrapped and justified 这是一段会被自动换行和对齐的长文本。")))
```

支持的语言包括：英语、德语、法语、西班牙语、中文、日文、韩文等（完整列表见 `dictionaries/` 目录）。

## 文档

| 文档 | 说明 |
|------|------|
| [用户手册](docs/USER-MANUAL.md) | 完整的使用指南 |
| [开发者手册](docs/DEVELOPER-MANUAL.md) | 架构和扩展指南 |
| [组件系统](docs/COMPONENT-SYSTEM-CN.md) | Vue 3 风格的组件系统和响应式状态管理 |
| [架构文档](docs/ARCHITECTURE.md) | 系统架构和模块关系 |
| [数据结构](docs/DATA-STRUCTURES.md) | 详细的数据结构文档 |
| [事件模型](docs/EVENT-MODEL.md) | 交互式伪类和事件系统 |
| [虚拟 DOM](docs/VIRTUAL-DOM.md) | 参考 Vue 3 设计的虚拟 DOM 系统 |
| [性能监控](docs/ETAF-PERF.md) | 性能分析工具，用于优化首屏加载时间 |
| [ETAF-EORM](docs/ETAF-EORM.md) | 参考 Diesel 设计的 多数据库 ORM（SQLite、PostgreSQL、MySQL） |

## 核心模块

| 模块 | 说明 |
|------|------|
| `etaf.el` | 主入口，高层 API |
| `etaf-etml.el` | TML 到 DOM 转换、模板指令、编译器（生成 VNode） |
| `etaf-vdom.el` | 虚拟 DOM (VNode)，存储标签元数据和交互处理器 |
| `etaf-component.el` | 组件系统、响应式系统（ref、computed、watch） |
| `etaf-table.el` | 功能丰富的表格组件（排序、分页、选择、筛选） |
| `etaf-event.el` | 事件模型，支持交互式伪类（:hover, :focus 等） |
| `etaf-css.el` | CSS 对象模型（CSSOM）主入口 |
| `etaf-render.el` | 渲染树构建（从 VNode 提取的 DOM + CSSOM） |
| `etaf-layout.el` | 盒模型和布局计算 |
| `etaf-layout-string.el` | 布局树到最终文本字符串的转换 |
| `etaf-tailwind.el` | Tailwind CSS 支持 |
| `etaf-ecss.el` | Emacs 风格的 CSS 表达式 |
| `etaf-perf.el` | 性能监控和分析工具 |
| `etaf-eorm.el` | 多数据库 ORM 库（SQLite、PostgreSQL、MySQL），参考 Diesel 设计 |
| `etaf-kp.el` | Knuth-Plass 排版算法实现，支持 CJK 与拉丁系语言的混合排版 |
| `etaf-pixel.el` | 像素级字符串操作，集成 etaf-kp 实现文本自动换行和对齐 |

## 安装

1. 克隆仓库：
```bash
git clone https://github.com/Kinneyzhang/etaf.git
```

2. 添加到 Emacs 配置：
```elisp
(add-to-list 'load-path "/path/to/etaf")
(require 'etaf)
```

## 示例

运行交互式组件演示：
```elisp
;; 组件系统示例 - 选项式 API 和组合式 API
(load-file "examples/etaf-component-examples.el")
M-x etaf-component-demo
```

示例文件：
- `examples/etaf-component-examples.el` - 组件系统示例（选项式 API 和组合式 API）
- `examples/etaf-table-example.el` - 表格组件示例（排序、分页、选择）
- `examples/etaf-tailwind-example.el` - Tailwind CSS 示例
- `examples/etaf-layout-example.el` - 布局系统示例
- `examples/etaf-render-example.el` - 渲染示例
- `examples/etaf-css-example.el` - CSS 示例

## 测试

运行测试套件：
```bash
cd tests
emacs -batch -l etaf-ert.el -l etaf-css-tests.el -f ert-run-tests-batch-and-exit
```

## 贡献

欢迎贡献代码、报告问题或提出改进建议！

## 许可证
GNU General Public License v3.0 或更高版本。

## 相关资源

- [CSS 规范](https://www.w3.org/Style/CSS/)
- [CSSOM 规范](https://www.w3.org/TR/cssom-1/)
- [CSS 盒模型规范](https://www.w3.org/TR/css-box-3/)
- [emacs-kp](https://github.com/Kinneyzhang/emacs-kp) - ETAF 的 etaf-kp 模块基于此项目集成
