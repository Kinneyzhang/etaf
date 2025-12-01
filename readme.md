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
- 📦 **Component System** - Vue3-style reactive components with props, setup, and templates
- 🔄 **Reactive System** - ref, computed, watch, and watchEffect for state management
- 🎯 **Tailwind CSS** - Built-in support for Tailwind utility classes
- 📐 **Layout Engine** - Box model and Flexbox layout support
- ⚡ **Performance Optimized** - Rule indexing and style caching

### Rendering Pipeline

```
TML → DOM → CSSOM → Render Tree → Layout Tree → Buffer String
```

## Quick Start

### Basic Usage

```elisp
(require 'etaf)

;; Simple rendering
(etaf-render-to-buffer "*demo*"
  '(div :class "container"
     (h1 :style "color: blue" "Hello ETAF!")
     (p "Build beautiful UIs in Emacs")))

;; With Tailwind CSS classes
(etaf-render-to-buffer "*demo*"
  '(div :class "flex items-center p-2 bg-blue-500"
     (span :class "text-white font-bold" "Styled with Tailwind!")))
```

### Template Directives

```elisp
(setq my-data '(:name "Alice" :items ("Apple" "Banana" "Cherry")))

(etaf-render-to-buffer "*demo*"
  '(div
     (h1 "Hello, {{ name }}!")
     (ul
       (li :e-for "item in items" "{{ item }}")))
  my-data)
```

### Component System

```elisp
;; Define a component
(etaf-etml-define-component my-button
  :props '(:label :disabled)
  :template '(button :class "btn" "{{ label }}"))

;; Use the component
(etaf-render-to-buffer "*demo*"
  '(my-button :label "Click Me"))
```

### Reactive State

```elisp
(let* ((count (etaf-etml-ref 0))
       (doubled (etaf-etml-computed
                  (lambda () (* 2 (etaf-etml-ref-get count))))))
  (etaf-etml-ref-get count)      ;; => 0
  (etaf-etml-ref-set count 5)
  (etaf-etml-computed-get doubled)) ;; => 10
```

## Documentation

| Document | Description |
|----------|-------------|
| [User Manual](docs/USER-MANUAL.md) | Complete guide for using ETAF |
| [Developer Manual](docs/DEVELOPER-MANUAL.md) | Architecture and extension guide |
| [Architecture](docs/ARCHITECTURE.md) | System architecture and module relationships |
| [Data Structures](docs/DATA-STRUCTURES.md) | Detailed data structure documentation |

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

Run interactive demos:
```elisp
(load-file "examples/etaf-interactive-examples.el")
M-x etaf-interactive-demo
```

## License

GNU General Public License v3.0 or later.

---

<a name="中文"></a>

## 概述

ETAF（Emacs Text-based Application Framework）是一个在 Emacs 中构建丰富文本界面的综合框架。它将 DOM、CSS 和响应式组件等 Web 开发概念引入 Emacs 生态系统，使开发者能够使用熟悉的 HTML/CSS 语法创建复杂的 UI 组件。

### 核心特性

- 🏗️ **类 HTML 模板** - 使用基于 S-expression 的 TML（模板标记语言）编写 UI
- 🎨 **CSS 支持** - 完整的 CSS 解析、层叠算法和计算样式
- 📦 **组件系统** - Vue3 风格的响应式组件，支持 props、setup 和 templates
- 🔄 **响应式系统** - ref、computed、watch 和 watchEffect 状态管理
- 🎯 **Tailwind CSS** - 内置 Tailwind 工具类支持
- 📐 **布局引擎** - 盒模型和 Flexbox 布局支持
- ⚡ **性能优化** - 规则索引和样式缓存

### 渲染流程

```
TML → DOM 树 → CSSOM → 渲染树 → 布局树 → Buffer 字符串
```

## 快速开始

### 基础用法

```elisp
(require 'etaf)

;; 简单渲染
(etaf-render-to-buffer "*demo*"
  '(div :class "container"
     (h1 :style "color: blue" "Hello ETAF!")
     (p "在 Emacs 中构建精美 UI")))

;; 使用 Tailwind CSS 类
(etaf-render-to-buffer "*demo*"
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

(etaf-render-to-buffer "*demo*"
  '(div
     (h1 "你好，{{ name }}！")
     (ul
       (li :e-for "item in items" "{{ item }}")))
  my-data)
```

### 组件系统

```elisp
;; 定义组件
(etaf-etml-define-component my-button
  :props '(:label :disabled)
  :template '(button :class "btn" "{{ label }}"))

;; 使用组件
(etaf-render-to-buffer "*demo*"
  '(my-button :label "点击我"))
```

### 响应式系统

```elisp
;; 创建响应式引用
(let* ((count (etaf-etml-ref 0))
       (doubled (etaf-etml-computed
                  (lambda () (* 2 (etaf-etml-ref-get count))))))
  (etaf-etml-ref-get count)      ;; => 0
  (etaf-etml-ref-set count 5)
  (etaf-etml-computed-get doubled)) ;; => 10
```

### Tailwind CSS 支持

```elisp
;; 直接使用 Tailwind 类
(etaf-render-to-buffer "*demo*"
  '(div :class "flex items-center justify-between bg-white rounded-lg shadow-md p-2"
     (h1 :class "text-lg font-bold text-gray-900" "标题")
     (button :class "bg-blue-500 text-white px-2 py-1 rounded" "按钮")))
```

支持的 Tailwind 功能：
- 响应式前缀：`sm:`, `md:`, `lg:`, `xl:`, `2xl:`
- 状态变体：`hover:`, `focus:`, `active:`
- 颜色系统：完整的 Tailwind 调色板
- 间距、Flexbox、圆角、阴影等
- 水平方向默认使用字符宽度(cw)，使用px后缀指定像素（如 `w-20px`）

### ECSS：Emacs 风格 CSS

```elisp
(require 'etaf-ecss)

;; 创建 CSS 规则
(etaf-ecss ".box"
  '(background "red")
  '(padding 10)
  '(border 1 solid "black"))
;; => ".box { background: red; padding: 10px; border: 1px solid black; }"
```

## 文档

| 文档 | 说明 |
|------|------|
| [用户手册](docs/USER-MANUAL.md) | 完整的使用指南 |
| [开发者手册](docs/DEVELOPER-MANUAL.md) | 架构和扩展指南 |
| [架构文档](docs/ARCHITECTURE.md) | 系统架构和模块关系 |
| [数据结构](docs/DATA-STRUCTURES.md) | 详细的数据结构文档 |

## 核心模块

| 模块 | 说明 |
|------|------|
| `etaf.el` | 主入口，高层 API |
| `etaf-etml.el` | TML 到 DOM 转换、模板指令、组件系统、响应式系统 |
| `etaf-css.el` | CSS 对象模型（CSSOM）主入口 |
| `etaf-render.el` | 渲染树构建 |
| `etaf-layout.el` | 盒模型和布局计算 |
| `etaf-tailwind.el` | Tailwind CSS 支持 |
| `etaf-ecss.el` | Emacs 风格的 CSS 表达式 |

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

运行交互式演示：
```elisp
(load-file "examples/etaf-interactive-examples.el")
M-x etaf-interactive-demo
```

示例文件：
- `examples/etaf-interactive-examples.el` - 交互式演示
- `examples/etaf-component-examples.el` - 组件系统示例
- `examples/etaf-tailwind-example.el` - Tailwind CSS 示例
- `examples/etaf-layout-example.el` - 布局系统示例

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
