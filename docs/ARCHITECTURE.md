# ETML/ETAF 架构文档

# ETML/ETAF Architecture Documentation

> **English Summary**: ETML (Emacs Template Markup Language) / ETAF (Emacs Text Application Framework) is a comprehensive framework for implementing HTML/CSS-like rendering in Emacs. It provides complete DOM operations, CSS parsing, box model layout, and text rendering capabilities. This document details all modules, their functions, and call relationships to help you thoroughly understand the repository implementation.

## 概述

ETML (Emacs Template Markup Language) / ETAF (Emacs Text Application Framework) 是一个在 Emacs 中实现类似 HTML/CSS 渲染的框架。它提供了完整的 DOM 操作、CSS 解析、盒模型布局和文本渲染功能。

## 整体架构

```
┌─────────────────────────────────────────────────────────────────┐
│                         etaf.el (入口)                           │
│                    etaf-paint-string TML → 渲染字符串                  │
└───────────────────────────────┬─────────────────────────────────┘
                                │
        ┌───────────────────────┼───────────────────────┐
        │                       │                       │
        ▼                       ▼                       │
┌───────────────────┐  ┌─────────────────┐              │
│    etaf-etml.el    │  │  etaf-css.el    │              │
│  TML → DOM        │  │  CSS 主入口     │              │
│  + 模板指令支持   │  │                 │              │
└───────┬───────────┘  └────────┬────────┘              │
        │                       │                       │
        ▼                       ▼                       │
┌───────────────┐      ┌─────────────────┐              │
│  etaf-dom.el  │◄─────│  CSS 子模块     │              │
│  DOM 操作     │      │  ├ core         │              │
└───────────────┘      │  ├ parser       │              │
                       │  ├ selector     │              │
                       │  └ face         │              │
                       └────────┬────────┘              │
                                │                       │
        ┌───────────────────────┼───────────────────────┘
        │                       │
        ▼                       ▼
┌───────────────┐      ┌─────────────────┐
│etaf-render.el │◄─────│etaf-tailwind.el │
│ 渲染树构建    │      │ Tailwind 支持   │
└───────┬───────┘      └─────────────────┘
        │
        ▼
┌───────────────┐
│etaf-layout.el │
│ 布局计算      │
└───────┬───────┘
        │
        ▼
┌───────────────┐    ┌─────────────────────┐    ┌─────────────────┐
│  etaf-box.el  │◄───│etaf-scroll-bar.el   │◄───│  etaf-utils.el  │
│  盒模型渲染   │    │ 滚动条              │    │  工具函数       │
└───────┬───────┘    └─────────────────────┘    └────────┬────────┘
        │                                            │
        └────────────────────┬───────────────────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │ etaf-pixel.el   │
                    │ 像素级操作      │
                    └─────────────────┘
```

## 数据流程

```
1. TML (模板标记语言)
   (div :class "container" (p "Hello"))
        │
        ▼ etaf-etml-to-dom
2. DOM (文档对象模型)
   (div ((class . "container")) (p nil "Hello"))
        │
        ▼ etaf-css-build-cssom + etaf-render-build-tree
3. Render Tree (渲染树)
   包含计算后的 CSS 样式
        │
        ▼ etaf-layout-build-tree
4. Layout Tree (布局树)
   包含盒模型信息（位置、尺寸）
        │
        ▼ etaf-layout-to-string
5. String (带属性的字符串)
   可直接插入 Emacs buffer 显示
```

---

## 模块详解

### 1. etaf.el - 主入口模块

**职责**: 提供高层 API，整合所有子模块完成完整的渲染流程。

#### 函数

| 函数 | 功能 | 调用关系 |
|------|------|----------|
| `etaf-paint-string | 将 TML 转换为可渲染的字符串 | 调用: `etaf-etml-to-dom` → `etaf-css-build-cssom` → `etaf-render-build-tree` → `etaf-layout-build-tree` → `etaf-layout-to-string` |

---

### 2. etaf-etml.el - TML 到 DOM 转换

**职责**: 将 TML (plist 格式) 转换为 DOM (alist 格式)。

#### 数据格式

```elisp
;; TML 格式 (plist)
(tag :attr1 val1 :attr2 val2 child1 child2 ...)

;; DOM 格式 (alist)  
(tag ((attr1 . val1) (attr2 . val2)) child1 child2 ...)
```

#### 函数

| 函数 | 功能 | 调用关系 |
|------|------|----------|
| `etaf-plist-to-alist` | plist 转 alist | 被 `etaf-etml-to-dom` 调用 |
| `etaf-etml-to-dom` | TML 递归转 DOM | 调用 `etaf-plist-to-alist`，递归调用自身 |

---

### 3. etaf-dom.el - DOM 操作模块

**职责**: 提供 DOM 节点查询、匹配和样式操作功能。

#### 核心函数

##### DOM 节点匹配
| 函数 | 功能 | 说明 |
|------|------|------|
| `etaf-dom-tag-match-p` | 检查节点是否匹配标签选择器 | 支持通配符 `*` |
| `etaf-dom-class-match-p` | 检查节点是否匹配类选择器 | 解析 class 属性 |
| `etaf-dom-id-match-p` | 检查节点是否匹配 ID 选择器 | - |

##### DOM 树遍历
| 函数 | 功能 | 说明 |
|------|------|------|
| `etaf-dom-map` | 遍历 DOM 树所有节点 | 对每个节点调用回调函数 |
| `etaf-dom-get-parent` | 获取父节点 | 依赖 `dom-parent` |
| `etaf-dom-get-previous-sibling` | 获取前一个兄弟节点 | 跳过文本节点 |
| `etaf-dom-get-previous-siblings` | 获取所有前面的兄弟节点 | - |
| `etaf-dom-get-element-children` | 获取所有元素子节点 | 跳过文本节点 |
| `etaf-dom-is-descendant-of` | 检查是否是后代 | - |

##### 伪类检查
| 函数 | 功能 |
|------|------|
| `etaf-dom-is-first-child` | 是否是第一个子元素 |
| `etaf-dom-is-last-child` | 是否是最后一个子元素 |
| `etaf-dom-is-first-of-type` | 是否是该类型的第一个 |
| `etaf-dom-is-last-of-type` | 是否是该类型的最后一个 |
| `etaf-dom-is-only-of-type` | 是否是该类型唯一的 |
| `etaf-dom-is-empty` | 是否为空节点 |
| `etaf-dom-get-child-index` | 获取子元素索引位置 |

##### 样式操作
| 函数 | 功能 |
|------|------|
| `ecss-dom-set-styles` | 设置节点 CSS 样式 |
| `ecss-dom-apply-style` | 为匹配选择器的节点应用样式 |
| `ecss-dom-get-style` | 获取节点指定 CSS 属性值 |
| `ecss-dom-add-class` | 添加 CSS 类 |
| `ecss-dom-remove-class` | 移除 CSS 类 |
| `ecss-dom-has-class` | 检查是否有指定类 |
| `ecss-dom-toggle-class` | 切换 CSS 类 |

##### 转换
| 函数 | 功能 |
|------|------|
| `etaf-dom-to-tml` | DOM 转回 TML 格式 |
| `etaf-alist-to-plist` | alist 转 plist |

---

### 4. etaf-css.el - CSS 主模块

**职责**: CSS 系统的主入口，整合所有 CSS 子模块。

#### 核心流程

```
DOM + <style> 标签
        │
        ▼ etaf-css-build-cssom
    CSSOM (CSS Object Model)
    ├── inline-rules (内联样式)
    ├── style-rules (样式表规则)
    ├── all-rules (所有规则)
    ├── rule-index (规则索引)
    ├── cache (样式缓存)
    └── media-env (媒体查询环境)
        │
        ▼ etaf-css-get-computed-style
    节点的最终计算样式
```

#### 函数

| 函数 | 功能 | 调用关系 |
|------|------|----------|
| `etaf-css-build-cssom` | 从 DOM 构建 CSSOM | 调用: `etaf-css-extract-inline-styles`, `etaf-css-extract-style-tags`, `etaf-css-index-build`, `etaf-css-cache-create` |
| `etaf-css-extract-inline-styles` | 提取内联样式 | 调用: `etaf-dom-map`, `etaf-css-parse-declarations` |
| `etaf-css-extract-style-tags` | 提取 style 标签内容 | 调用: `dom-search`, `etaf-css-parse-stylesheet` |
| `etaf-css-get-rules-for-node` | 获取节点适用的规则 | 调用: `etaf-css-index-query-candidates`, `etaf-css-media-match-p`, `etaf-css-selector-parse`, `etaf-css-selector-node-matches-p` |
| `etaf-css-get-computed-style` | 计算节点最终样式 | 调用: `etaf-css-cache-get/set`, `etaf-css-get-rules-for-node`, `etaf-css-cascade-merge-rules`, `etaf-tailwind-classes-to-css`, `etaf-css-apply-inheritance` |
| `etaf-css-add-stylesheet` | 添加外部样式表 | 调用: `etaf-css-parse-stylesheet`, `etaf-css-index-build`, `etaf-css-clear-cache` |

---

### 5. etaf-css-selector.el - CSS 选择器解析器

**职责**: 解析 CSS 选择器字符串为 AST，并提供选择器匹配功能。

#### 词法分析

| 函数 | 功能 |
|------|------|
| `etaf-css-selector-tokenize` | 将选择器字符串分解为 token 列表 |
| `etaf-css-selector-consume-escape` | 消费转义序列 |
| `etaf-css-selector-consume-word` | 消费单词 |

#### 节点构造

| 函数 | 创建的节点类型 |
|------|---------------|
| `etaf-css-selector-make-root` | 根节点 |
| `etaf-css-selector-make-selector` | 选择器节点 |
| `etaf-css-selector-make-tag` | 标签选择器 |
| `etaf-css-selector-make-class` | 类选择器 |
| `etaf-css-selector-make-id` | ID 选择器 |
| `etaf-css-selector-make-attribute` | 属性选择器 |
| `etaf-css-selector-make-pseudo` | 伪类/伪元素 |
| `etaf-css-selector-make-universal` | 通配符 |
| `etaf-css-selector-make-combinator` | 组合器 |
| `etaf-css-selector-make-nesting` | 嵌套选择器 |
| `etaf-css-selector-make-comment` | 注释 |

#### 语法分析

| 函数 | 功能 |
|------|------|
| `etaf-css-selector-parse` | 主解析函数，返回 AST |
| `etaf-css-selector-parser-loop` | 主解析循环 |
| `etaf-css-selector-parser-parse` | 解析当前 token |
| `etaf-css-selector-parser-word` | 处理单词 token |
| `etaf-css-selector-parser-pseudo` | 处理伪类 |
| `etaf-css-selector-parser-combinator` | 处理组合器 |
| `etaf-css-selector-parser-attribute` | 处理属性选择器 |

#### 匹配函数

| 函数 | 功能 |
|------|------|
| `etaf-css-selector-node-matches-p` | 检查节点是否匹配选择器 (支持组合器) |
| `etaf-css-selector-basic-match-p` | 基础选择器匹配 |
| `etaf-css-selector-pseudo-match-p` | 伪类匹配 |
| `etaf-css-selector-attribute-match-p` | 属性选择器匹配 |
| `etaf-css-selector-descendant-match-p` | 后代组合器匹配 |
| `etaf-css-selector-child-match-p` | 子元素组合器匹配 |
| `etaf-css-selector-adjacent-sibling-match-p` | 相邻兄弟组合器匹配 |
| `etaf-css-selector-general-sibling-match-p` | 通用兄弟组合器匹配 |

#### 查询函数

| 函数 | 功能 |
|------|------|
| `etaf-css-selector-query` | 查询所有匹配的节点 |
| `etaf-css-selector-query-by-ast` | 使用 AST 查询 |
| `etaf-css-selector-walk` | 遍历 AST |

---

### 6. etaf-css-parser.el - 完整 CSS 解析模块

**职责**: 解析 CSS 声明、规则、样式表、值、媒体查询和复合属性展开。

#### 主要功能

**CSS 解析**
- 解析 CSS 声明字符串（支持 !important）
- 解析 CSS 规则和样式表
- 处理 @media 规则

**值解析**
- 支持单位：px, %, em, lh, cw
- 长度值和高度值解析
- Flex 数值属性解析

**媒体查询**
- 解析和评估 @media 规则
- 支持媒体类型和特性匹配
- 默认媒体环境：screen, width: 1024, height: 768

**复合属性展开**
- border, margin, padding
- flex, flex-flow, gap
- place-content, place-items, place-self
- grid-column, grid-row, grid-area

#### 函数

| 函数 | 功能 | 返回格式 |
|------|------|----------|
| `etaf-css-parse-declarations` | 解析 CSS 声明字符串 | `((property value important) ...)` |
| `etaf-css-parse-declarations-compat` | 兼容格式解析 | `((property . value) ...)` |
| `etaf-css-parse-rule` | 解析单个 CSS 规则 | `(:selector ... :declarations ... :specificity ...)` |
| `etaf-css-parse-stylesheet` | 解析完整样式表 | 规则列表 |
| `etaf-css-parse-length` | 解析 CSS 长度值 | 像素值或 'auto/'none |
| `etaf-css-parse-height` | 解析 CSS 高度值 | 行数或 'auto/'none |
| `etaf-css-media-match-p` | 检查媒体查询是否匹配 | t/nil |
| `etaf-css-expand-shorthand` | 展开单个复合属性 | 声明列表 |
| `etaf-css-expand-declarations` | 展开声明列表中所有复合属性 | 展开后的声明列表 |

---

### 7. etaf-css-core.el - 核心 CSS 系统

**职责**: CSS 层叠算法、特异性计算、属性继承、样式缓存和规则索引。

#### 层叠算法

**特异性计算**
```
特异性格式: (id-count class-count type-count)

示例:
  'div'          => (0 0 1)
  '.button'      => (0 1 0)
  '#main'        => (1 0 0)
  'div.button'   => (0 1 1)
  '#main .text'  => (1 1 0)
```

**层叠规则优先级 (从低到高)**
1. 正常声明（按特异性和顺序）
2. `!important` 声明（按特异性和顺序）
3. 内联样式正常声明
4. 内联样式 `!important` 声明

#### 属性继承

**可继承的属性**: color, font-family, font-size, font-style, font-weight, line-height, text-align, text-indent, visibility, white-space, cursor, direction 等

#### 样式缓存

缓存节点的计算样式，避免重复计算，提高性能。

#### 规则索引

**索引结构**
```elisp
(:by-tag   <hash-table>   ;; 按标签索引
 :by-class <hash-table>   ;; 按类索引
 :by-id    <hash-table>)  ;; 按 ID 索引
```

按选择器类型索引 CSS 规则，优化查询性能。

#### 函数

| 函数 | 功能 |
|------|------|
| `etaf-css-calculate-specificity` | 计算选择器特异性 |
| `etaf-css-specificity>` | 比较特异性大小 |
| `etaf-css-specificity=` | 检查特异性相等 |
| `etaf-css-cascade-compare-declarations` | 比较两个声明的优先级 |
| `etaf-css-cascade-apply` | 应用层叠算法 |
| `etaf-css-cascade-merge-rules` | 合并多个规则的声明 |
| `etaf-css-property-inherits-p` | 检查属性是否可继承 |
| `etaf-css-apply-inheritance` | 应用继承属性到子元素 |
| `etaf-css-cache-create` | 创建缓存 |
| `etaf-css-cache-get` | 获取缓存 |
| `etaf-css-cache-set` | 设置缓存 |
| `etaf-css-cache-clear` | 清空缓存 |
| `etaf-css-index-create` | 创建索引结构 |
| `etaf-css-index-build` | 从规则列表构建索引 |
| `etaf-css-index-query-candidates` | 查询候选规则 |

---
---

### 13. etaf-css-face.el - CSS 到 Emacs Face 映射

**职责**: 将 CSS 样式转换为 Emacs face 属性。

#### 属性映射

| CSS 属性 | Emacs Face 属性 |
|---------|-----------------|
| `color` | `:foreground` |
| `background-color` | `:background` |
| `font-weight: bold` | `:weight bold` |
| `font-style: italic` | `:slant italic` |
| `text-decoration: underline` | `:underline` |
| `text-decoration: line-through` | `:strike-through` |
| `font-size` | `:height` |
| `font-family` | `:family` |

#### 函数

| 函数 | 功能 |
|------|------|
| `etaf-css-color-to-emacs` | CSS 颜色转 Emacs 颜色 |
| `etaf-css-font-weight-to-emacs` | font-weight 转换 |
| `etaf-css-font-style-to-emacs` | font-style 转换 |
| `etaf-css-text-decoration-to-emacs` | text-decoration 转换 |
| `etaf-css-font-size-to-emacs` | font-size 转换 |
| `etaf-css-style-to-face` | 完整样式转 face plist |
| `etaf-css-apply-face-to-string` | 将样式应用到字符串 |

---

### 14. etaf-render.el - 渲染树构建

**职责**: 从 DOM 和 CSSOM 构建渲染树。

#### 渲染树结构

```elisp
(tag ((render-style . ((color . "red") ...))
      (render-display . "block")
      (class . "foo")     ;; 原始 DOM 属性
      (id . "bar"))       ;; 原始 DOM 属性
  child1 child2 ...)      ;; 子渲染节点
```

#### 函数

| 函数 | 功能 |
|------|------|
| `etaf-render-build-tree` | 构建渲染树 |
| `etaf-render--build-node` | 递归构建渲染节点 |
| `etaf-render-create-node` | 创建渲染节点 |
| `etaf-render-node-visible-p` | 判断节点是否可见 |
| `etaf-render-get-default-display` | 获取默认 display 值 |
| `etaf-render-walk` | 遍历渲染树 |
| `etaf-render-get-style` | 获取样式属性 |
| `etaf-render-get-display` | 获取 display 类型 |
| `etaf-render-get-computed-style` | 获取完整计算样式 |
| `etaf-render-find-by-tag` | 按标签查找节点 |
| `etaf-render-find-by-display` | 按 display 查找节点 |
| `etaf-render-to-string` | 渲染树转可读字符串 |
| `etaf-render-stats` | 渲染树统计信息 |

---

### 15. etaf-layout.el - 布局计算

**职责**: 实现 CSS 盒模型和布局算法。

#### 盒模型结构

```elisp
(:box-sizing "content-box"|"border-box"
 :content (:width <number> :height <number>)
 :padding (:top <n> :right <n> :bottom <n> :left <n>)
 :border (:top-width <n> :right-width <n> ...
          :top-color <color> :right-color <color> ...)
 :margin (:top <n> :right <n> :bottom <n> :left <n>))
```

#### 核心函数

##### 布局树构建
| 函数 | 功能 |
|------|------|
| `etaf-layout-build-tree` | 构建布局树 |
| `etaf-layout-node` | 递归布局节点 |
| `etaf-layout-create-node` | 创建布局节点 |
| `etaf-layout-compute-box-model` | 计算盒模型 |

##### 布局算法
| 函数 | 功能 |
|------|------|
| `etaf-layout-block-formatting-context` | 块级格式化上下文布局 |
| `etaf-layout-flex-format` | Flex 格式化上下文布局 |
| `etaf-layout-flex-compute-main-axis` | Flex 主轴计算 |
| `etaf-layout-flex-compute-cross-axis` | Flex 交叉轴计算 |
| `etaf-layout-flex-justify-space` | justify-content 空间分配 |

##### 值解析
| 函数 | 功能 |
|------|------|
| `etaf-layout-parse-length` | 解析 CSS 长度值 |
| `etaf-layout-parse-height` | 解析高度值 (行数单位) |
| `etaf-layout-parse-flex-number` | 解析 flex 数值属性 |

##### 盒模型辅助
| 函数 | 功能 |
|------|------|
| `etaf-layout-box-create` | 创建空盒模型 |
| `etaf-layout-box-content-width/height` | 获取内容尺寸 |
| `etaf-layout-box-padding-width/height` | 获取 padding 尺寸 |
| `etaf-layout-box-border-width/height` | 获取 border 尺寸 |
| `etaf-layout-box-margin-width/height` | 获取 margin 尺寸 |
| `etaf-layout-box-total-width/height` | 获取总尺寸 |

##### 字符串生成
| 函数 | 功能 |
|------|------|
| `etaf-layout-to-string` | 布局树转字符串 |
| `etaf-layout-node-string` | 单个节点转字符串 |
| `etaf-layout--merge-children-by-display` | 按 display 合并子元素 |
| `etaf-layout--merge-flex-children` | Flex 布局合并子元素 |

---

### 16. etaf-box.el - 盒模型渲染

**职责**: 实现底层的盒模型渲染和滚动功能。

#### 类定义

##### etaf-text-css
文本样式属性类:
- `color`, `bgcolor`: 文本颜色
- `font-family`, `font-size`, `font-weight`: 字体属性
- `text-underline`, `text-overline`, `text-strike`: 文本装饰
- `text-align`, `vertical-align`: 对齐方式

##### etaf-box (继承 etaf-text-css)
盒模型类:
- `uuid`: 唯一标识
- `content`: 内容区域
- `display`: 显示类型
- `width`, `height`, `min-width`, `max-width`, `min-height`, `max-height`: 尺寸
- `padding-*-pixel`, `margin-*-pixel`: 内外边距
- `border-*-pixel`, `border-*-color`: 边框
- `overflow-y`: 垂直溢出处理
- `v-scroll-bar`: 垂直滚动条

#### 核心函数

##### 尺寸计算
| 函数 | 功能 |
|------|------|
| `etaf-box-content-pixel` | 计算内容最终像素宽度 |
| `etaf-box-content-height` | 计算内容高度 (行数) |
| `etaf-box-content-linum` | 原始文本行数 |
| `etaf-box-side-pixel` | 非内容部分像素宽度 |
| `etaf-box-total-pixel` | 总像素宽度 |
| `etaf-box-side-height` | 非内容部分高度 |
| `etaf-box-total-height` | 总高度 |

##### 渲染
| 函数 | 功能 |
|------|------|
| `etaf-box-string` | 渲染 box 为字符串 |
| `etaf-box-render` | 渲染到 buffer |
| `etaf-box-content` | 获取处理后的内容 |
| `etaf-box-original-content` | 获取原始内容 |

##### 滚动
| 函数 | 功能 |
|------|------|
| `etaf-box-scroll` | 滚动内容 |
| `etaf-box-scroll-up` | 向上滚动 |
| `etaf-box-scroll-down` | 向下滚动 |
| `etaf-box-v-scroll-bar-p` | 是否显示滚动条 |
| `etaf-box-v-scroll-bar-string` | 渲染滚动条字符串 |
| `etaf-box-v-scroll-bar-info` | 滚动条信息 |

---

### 17. etaf-scroll-bar.el - 滚动条

**职责**: 实现滚动条的模型和渲染。

#### etaf-scroll-bar 类

**轨道属性**:
- `track-height`: 轨道高度
- `track-color`: 轨道颜色
- `track-margin-*-pixel`: 轨道外边距
- `track-padding-*-pixel`: 轨道内边距
- `track-border-*-pixel/color`: 轨道边框

**滑块属性**:
- `thumb-offset`: 初始偏移量
- `thumb-height`: 滑块高度
- `thumb-pixel`: 滑块像素宽度
- `thumb-border-p/color`: 滑块边框
- `thumb-color`: 滑块颜色

#### 函数

| 函数 | 功能 |
|------|------|
| `etaf-scroll-bar-pixel` | 滚动栏总像素宽度 |
| `etaf-scroll-bar-track-face` | 轨道 face |
| `etaf-scroll-bar-thumb-face` | 滑块 face |
| `etaf-scroll-bar-render` | 渲染滚动栏 |
| `etaf-scroll-bar-define` | 定义滚动条风格 |

---

### 18. etaf-etml.el - 模板指令支持（原 etaf-template.el 已合并）

**职责**: 实现类 Vue.js 的模板语法。模板功能现已合并到 etaf-etml.el 中，函数名从 `etaf-template-*` 更名为 `etaf-etml-*`。

#### 支持的语法

| 语法 | 功能 | 示例 |
|------|------|------|
| `{{ expr }}` | 文本插值 | `{{ name }}` |
| `:e-if` | 条件渲染 | `:e-if "visible"` |
| `:e-else-if` | 条件分支 | `:e-else-if "count > 0"` |
| `:e-else` | 否则分支 | `:e-else` |
| `:e-for` | 列表渲染 | `:e-for "item in items"` |
| `:e-bind:attr` | 属性绑定 | `:e-bind:class "className"` |
| `:e-text` | 文本内容 | `:e-text "message"` |
| `:e-show` | 显示/隐藏 | `:e-show "isVisible"` |

#### 核心函数

##### 插值和表达式
| 函数 | 功能 |
|------|------|
| `etaf-etml--interpolate-string` | 替换 `{{ }}` 插值 |
| `etaf-etml--eval-expr` | 评估表达式 |
| `etaf-etml--to-string` | 值转字符串 |

##### 指令解析
| 函数 | 功能 |
|------|------|
| `etaf-etml--parse-e-for` | 解析 e-for 表达式 |
| `etaf-etml--truthy-p` | 判断真值 |
| `etaf-etml--split-attrs-and-children` | 分离属性和子元素 |
| `etaf-etml--process-bindings` | 处理属性绑定 |

##### 渲染
| 函数 | 功能 |
|------|------|
| `etaf-etml-render` | 渲染模板 |
| `etaf-etml--render-node` | 递归渲染节点 |
| `etaf-etml--render-element` | 渲染元素 |
| `etaf-etml-to-dom` | 模板渲染并转 DOM |

##### 响应式数据
| 函数 | 功能 |
|------|------|
| `etaf-create-reactive` | 创建响应式数据 |
| `etaf-get` | 获取响应式数据值 |
| `etaf-set` | 设置值并触发更新 |
| `etaf-watch` | 添加数据监听器 |
| `etaf-unwatch` | 移除监听器 |

---

### 19. etaf-tailwind.el - Tailwind CSS 支持

**职责**: 支持 Tailwind CSS 实用类的解析和转换。

#### 类名解析

```elisp
(etaf-tailwind-parse-class "md:hover:bg-red-500")
;; => (:variants ("md" "hover") 
;;     :utility "bg-red-500" 
;;     :property "bg" 
;;     :value "red-500")
```

#### 核心函数

##### 解析和验证
| 函数 | 功能 |
|------|------|
| `etaf-tailwind-parse-class` | 解析类名结构 |
| `etaf-tailwind-class-p` | 验证是否为有效 Tailwind 类 |
| `etaf-tailwind-get-variants` | 获取变体 |
| `etaf-tailwind-get-utility` | 获取实用类部分 |
| `etaf-tailwind-get-property` | 获取属性名 |
| `etaf-tailwind-has-variant-p` | 检查是否有指定变体 |
| `etaf-tailwind-has-responsive-p` | 是否有响应式前缀 |

##### DOM 操作
| 函数 | 功能 |
|------|------|
| `etaf-dom-node-has-tailwind-class-p` | 检查节点是否有指定类 |
| `etaf-dom-query-tailwind` | 查询有指定类的节点 |
| `etaf-dom-query-tailwind-pattern` | 按模式查询 |
| `etaf-tailwind-add-class` | 添加类 |
| `etaf-tailwind-remove-class` | 移除类 |
| `etaf-tailwind-toggle-class` | 切换类 |
| `etaf-tailwind-replace-class` | 替换类 |

##### CSS 转换
| 函数 | 功能 |
|------|------|
| `etaf-tailwind-to-css` | 类名转 CSS 属性 |
| `etaf-tailwind-classes-to-css` | 多个类名转 CSS |
| `etaf-tailwind-convert-standard` | 转换标准类 |
| `etaf-tailwind-convert-arbitrary` | 转换任意值类 |
| `etaf-tailwind-convert-spacing` | 转换间距类 |
| `etaf-tailwind-convert-size` | 转换尺寸类 |
| `etaf-tailwind-apply-css-to-node` | 应用 CSS 到节点 |
| `etaf-tailwind-css-to-string` | CSS 属性转字符串 |

---

### 20. etaf-utils.el - 工具函数

**职责**: 提供各种通用工具函数。

#### 类型转换
| 函数 | 功能 |
|------|------|
| `etaf-keyword->symbol` | 关键字转符号 |
| `etaf-symbol->keyword` | 符号转关键字 |
| `etaf-string-to-keyword` | 字符串转关键字 |
| `etaf-alist->plist` | alist 转 plist |
| `etaf-plist->alist` | plist 转 alist |
| `etaf-plist-remove-keys` | 从 plist 移除指定键 |

#### 文本属性操作
| 函数 | 功能 |
|------|------|
| `etaf-propertize` | 添加文本属性 (不覆盖) |
| `etaf-propertize-underline` | 添加下划线 |
| `etaf-propertize-overline` | 添加上划线 |
| `etaf-get-text-properties` | 获取文本所有属性区间 |
| `etaf-remove-face-attributes` | 移除 face 属性 |

#### 区域操作
| 函数 | 功能 |
|------|------|
| `etaf-region-replace` | 替换区域文本 |
| `etaf-region-swap` | 交换两个区域 |
| `etaf-property-forward-region` | 向前搜索属性区域 |
| `etaf-property-backward-region` | 向后搜索属性区域 |
| `etaf-property-map-regions` | 映射所有匹配区域 |

#### 字符串操作
| 函数 | 功能 |
|------|------|
| `etaf-string-linum` | 计算字符串行数 |
| `etaf-string-join` | 用换行连接字符串 |
| `etaf-string-duplines` | 复制字符串多行 |
| `etaf-string-concat` | 水平拼接字符串 |
| `etaf-maplines` | 对每行应用函数 |

#### 行对齐
| 函数 | 功能 |
|------|------|
| `etaf-lines-pad` | 垂直填充 |
| `etaf-lines-justify` | 水平调整每行 |
| `etaf-lines-align` | 垂直对齐 |
| `etaf-lines-concat` | 水平拼接多个块 |
| `etaf-lines-stack` | 垂直堆叠多个块 |

#### 像素相关
| 函数 | 功能 |
|------|------|
| `etaf-width-pixel` | 计算宽度像素 |
| `etaf-pixel-blank` | 创建空白块 |
| `etaf-pixel-border` | 创建边框块 |

#### 其他工具
| 函数 | 功能 |
|------|------|
| `etaf-interleave` | 交叉组合两个序列 |
| `etaf-split-size` | 将数值分成 N 等份 |
| `etaf-flex-line-breaks` | 计算 Flex 布局换行点 |
| `etaf-oset` | 批量设置对象属性 |

---

### 21. etaf-pixel.el - 像素级操作

**职责**: 提供像素级的字符串操作。

#### 函数

| 函数 | 功能 |
|------|------|
| `etaf-pixel-spacing` | 创建指定像素宽度的空格 |
| `etaf-pixel-pad` | 在字符串前后添加像素空格 |
| `etaf-pixel-reach` | 使字符串达到指定像素宽度 |
| `etaf-pixel-align` | 像素对齐 |
| `etaf-pixel-center` | 居中对齐 |
| `etaf-pixel-left` | 左对齐 |
| `etaf-pixel-right` | 右对齐 |
| `etaf-pixel-wrap` | 按像素宽度换行 |
| `etaf-pixel-typeset` | 排版 (换行+对齐) |
| `etaf-pixel-keep-left` | 保留左边部分 |
| `etaf-pixel-keep-right` | 保留右边部分 |
| `etaf-pixel-chop-left` | 裁剪左边 |
| `etaf-pixel-chop-right` | 裁剪右边 |

---

## 函数调用关系图

### 完整渲染流程

```
etaf-paint-string(etaf.el)
├── etaf-etml-to-dom (etaf-etml.el)
│   └── etaf-plist-to-alist
│
├── etaf-css-build-cssom (etaf-css.el)
│   ├── etaf-css-extract-inline-styles
│   │   ├── etaf-dom-map (etaf-dom.el)
│   │   └── etaf-css-parse-declarations (etaf-css-parser.el)
│   │       └── etaf-css-expand-shorthand (etaf-css-parser.el)
│   │
│   ├── etaf-css-extract-style-tags
│   │   └── etaf-css-parse-stylesheet (etaf-css-parser.el)
│   │       ├── etaf-css-parse-rule
│   │       │   ├── etaf-css-parse-declarations
│   │       │   └── etaf-css-calculate-specificity (etaf-css-core.el)
│   │       └── etaf-css-media-extract-at-media-blocks (etaf-css-parser.el)
│   │
│   ├── etaf-css-index-build (etaf-css-core.el)
│   │   └── etaf-css-index-add-rule
│   │       └── etaf-css-index-extract-selector-keys
│   │
│   └── etaf-css-cache-create (etaf-css-core.el)
│
├── etaf-render-build-tree (etaf-render.el)
│   └── etaf-render--build-node (递归)
│       ├── etaf-css-get-computed-style (etaf-css.el)
│       │   ├── etaf-css-cache-get/set (etaf-css-core.el)
│       │   ├── etaf-css-get-rules-for-node
│       │   │   ├── etaf-css-index-query-candidates (etaf-css-core.el)
│       │   │   ├── etaf-css-media-match-p (etaf-css-parser.el)
│       │   │   └── etaf-css-selector-node-matches-p (etaf-css-selector.el)
│       │   │       ├── etaf-css-selector-parse
│       │   │       ├── etaf-css-selector-basic-match-p
│       │   │       │   ├── etaf-dom-tag-match-p (etaf-dom.el)
│       │   │       │   ├── etaf-dom-class-match-p
│       │   │       │   ├── etaf-dom-id-match-p
│       │   │       │   └── etaf-css-selector-pseudo-match-p
│       │   │       └── etaf-css-selector-combinator-match-p
│       │   │
│       │   ├── etaf-css-cascade-merge-rules (etaf-css-core.el)
│       │   │   └── etaf-css-cascade-apply
│       │   │       └── etaf-css-cascade-compare-declarations
│       │   │
│       │   ├── etaf-tailwind-classes-to-css (etaf-tailwind.el)
│       │   │   └── etaf-tailwind-to-css
│       │   │       └── etaf-tailwind-convert-standard
│       │   │
│       │   └── etaf-css-apply-inheritance (etaf-css-core.el)
│       │
│       ├── etaf-render-node-visible-p
│       └── etaf-render-create-node
│
└── etaf-layout-build-tree (etaf-layout.el)
    └── etaf-layout-node (递归)
        ├── etaf-layout-block-formatting-context
        │   ├── etaf-layout-compute-box-model
        │   │   ├── etaf-layout-parse-length
        │   │   ├── etaf-layout-parse-height
        │   │   └── etaf-layout-parse-style-value
        │   └── etaf-layout-create-node
        │
        └── etaf-layout-flex-format
            ├── etaf-layout-compute-box-model
            ├── etaf-layout-flex-compute-main-axis
            │   └── etaf-layout-flex-justify-space
            └── etaf-layout-flex-compute-cross-axis

etaf-layout-to-string (etaf-layout.el)
└── etaf-layout-node-string (递归)
    ├── etaf-layout-box-* (获取盒模型各部分)
    ├── etaf-css-apply-face-to-string (etaf-css-face.el)
    │   └── etaf-css-style-to-face
    │       ├── etaf-css-color-to-emacs
    │       ├── etaf-css-font-weight-to-emacs
    │       └── etaf-css-text-decoration-to-emacs
    │
    ├── etaf-layout--merge-children-by-display
    │   └── etaf-layout--merge-inline-with-wrap
    │       └── etaf-flex-line-breaks (etaf-utils.el)
    │
    ├── etaf-layout--merge-flex-children
    │   ├── etaf-layout--flex-content-justify
    │   ├── etaf-layout--flex-concat-with-gaps
    │   │   └── etaf-layout--align-item-cross-axis
    │   └── etaf-layout--flex-stack-with-gaps
    │
    ├── etaf-lines-justify (etaf-utils.el)
    │   └── etaf-pixel-typeset (etaf-pixel.el)
    ├── etaf-lines-align
    ├── etaf-lines-stack
    ├── etaf-lines-concat
    ├── etaf-pixel-blank
    ├── etaf-pixel-border
    ├── etaf-propertize-overline
    └── etaf-propertize-underline
```

---

## 使用示例

### 基本使用

```elisp
;; 1. 定义 TML
(setq my-tml
  '(div :class "container" :style "padding: 10px; border: 1px solid red;"
     (h1 :style "color: blue;" "Hello World")
     (p "This is a paragraph.")))

;; 2. 渲染为字符串
(setq result (etaf-paint-string my-tml 400))  ; 宽度 400 像素

;; 3. 插入到 buffer
(insert result)
```

### 使用 Vue 风格模板

```elisp
;; 定义数据
(setq my-data '(:name "John" :items ("Apple" "Banana" "Cherry") :visible t))

;; 定义模板
(setq my-template
  '(div
    (h1 "Hello, {{ name }}!")
    (p :e-if "visible" "This section is visible")
    (ul
      (li :e-for "item in items" "{{ item }}"))))

;; 渲染
(etaf-etml-render my-template my-data)
```

### 使用 Tailwind CSS

```elisp
;; 使用 Tailwind 类
(setq tw-tml
  '(div :class "flex items-center justify-between p-4 bg-blue-500"
     (span :class "text-white font-bold" "Title")
     (button :class "bg-white text-blue-500 px-4 py-2 rounded" "Click")))

;; Tailwind 类会自动转换为 CSS
(etaf-paint-string tw-tml 600)
```

### 底层 Box 渲染

```elisp
;; 创建 box 对象
(setq my-box
  (etaf-box
   :content "Hello World"
   :width 200
   :height 5
   :padding-left-pixel 10
   :padding-right-pixel 10
   :padding-top-height 1
   :padding-bottom-height 1
   :border-left-pixel 2
   :border-right-pixel 2
   :border-top-p t
   :border-bottom-p t
   :bgcolor "lightblue"))

;; 渲染到 buffer
(etaf-box-render "*my-box*" my-box)
```

---

## 依赖关系

### 外部依赖

- `cl-lib`: Common Lisp 兼容库
- `dom`: Emacs 内置 DOM 操作库
- `s`: 字符串操作库
- `dash`: 列表操作库
- `ekp`: 像素排版库
- `elog`: 日志库
- `org-id`: UUID 生成

### 模块间依赖

```
etaf.el
├── etaf-etml.el (包含模板功能)
├── etaf-css.el
│   ├── etaf-dom.el
│   ├── etaf-css-selector.el
│   ├── etaf-css-parser.el (包含解析、值解析、媒体查询、复合属性展开)
│   ├── etaf-css-core.el (包含层叠、继承、缓存、索引)
│   ├── etaf-css-face.el
│   └── etaf-tailwind.el
│       └── etaf-dom.el
├── etaf-render.el
│   ├── etaf-dom.el
│   └── etaf-css.el
├── etaf-layout.el
│   ├── etaf-render.el
│   ├── etaf-utils.el
│   └── etaf-css-face.el
├── etaf-tailwind.el
│   └── etaf-dom.el
└── etaf-box.el (独立模块)
    ├── etaf-utils.el
    ├── etaf-scroll-bar.el
    │   └── etaf-utils.el
    └── elog

etaf-utils.el
├── etaf-pixel.el
│   ├── s
│   └── ekp
└── dash
```

---

## 总结

ETML/ETAF 是一个功能完整的 Emacs 文本渲染框架，实现了：

1. **完整的 CSS 支持**: 选择器解析、层叠算法、属性继承、媒体查询
2. **盒模型布局**: block、flex 布局，支持 padding、border、margin
3. **像素级精确渲染**: 使用 Emacs 的 display 属性实现像素级定位
4. **模板系统**: Vue 风格的响应式模板语法
5. **Tailwind CSS**: 开箱即用的 Tailwind 类支持
6. **滚动支持**: 内容溢出时的滚动功能

整个框架采用流水线架构：`TML → DOM → CSSOM → Render Tree → Layout Tree → String`，每一步都有清晰的职责划分，便于理解和扩展。
