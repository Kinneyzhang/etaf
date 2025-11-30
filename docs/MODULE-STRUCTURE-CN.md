# ETAF 模块结构

本文档描述了优化后的 ETAF (Emacs Template and Framework) 项目的模块化架构。

## 设计原则

1. **模块内聚性**: 每个模块都有明确、专注的目的
2. **最小化接口**: 模块只暴露必要的公共函数
3. **隐藏实现细节**: 内部细节被隐藏在模块内部
4. **依赖清晰性**: 模块依赖是明确且最小化的

## 模块组织

### 核心入口

#### etaf.el
- **目的**: ETAF 系统的主入口点
- **依赖**: etaf-etml, etaf-css
- **公共接口**: (无 - 仅聚合子模块)
- **用法**: `(require 'etaf)` 加载整个 ETAF 系统

### 标记处理模块

#### etaf-etml.el
- **目的**: TML (模板标记语言) 到 DOM 的转换
- **依赖**: (无)
- **公共接口**:
  - `etaf-etml-to-dom (sexp)` - 将 TML s 表达式转换为 DOM 格式
  - `etaf-plist-to-alist (plist)` - 将属性列表转换为关联列表
- **用法**: 将标记从 TML 格式 `(tag :attr val child...)` 转换为 DOM 格式 `(tag ((attr . val)) child...)`

#### etaf-tag.el
- **目的**: ETML 标签定义系统 - 定义类似 HTML 的标签，包括内容、样式和交互行为
- **依赖**: (无)
- **公共接口**:
  - `define-etaf-tag (name &rest props)` - 定义自定义标签的宏
  - `etaf-tag-defined-p (name)` - 检查标签是否已定义
  - `etaf-tag-get-definition (name)` - 获取标签定义
  - `etaf-tag-list-all ()` - 列出所有已定义的标签
  - `etaf-tag-parse (sexp)` - 解析 SEXP 为标签实例
  - `etaf-tag-to-dom (sexp)` - 将带标签的 SEXP 转换为 DOM 格式
  - `etaf-tag-create-instance (tag-name attrs children)` - 创建标签实例
  - `etaf-tag-render-to-dom (tag-instance)` - 将标签实例渲染为 DOM
  - `etaf-tag-get-computed-style (tag-instance)` - 获取标签实例的计算样式
- **标签定义属性**:
  - `:display` - 显示类型 (`block`, `inline`, `inline-block`, `flex`, `none`)
  - `:default-style` - 默认样式 alist
  - `:hover-style` - 悬停样式
  - `:active-style` - 激活样式
  - `:focus-style` - 聚焦样式
  - `:disabled-style` - 禁用样式
  - `:on-click` - 点击事件处理器
  - `:on-hover-enter` - 鼠标进入处理器
  - `:on-hover-leave` - 鼠标离开处理器
  - `:on-focus` - 聚焦处理器
  - `:on-blur` - 失焦处理器
  - `:on-change` - 变更处理器
  - `:on-input` - 输入处理器
  - `:on-keydown` - 按键按下处理器
  - `:on-keyup` - 按键释放处理器
  - `:children-allowed` - 是否允许子元素
  - `:self-closing` - 是否自闭合
  - `:inherit` - 继承的父标签
  - `:render` - 自定义渲染函数
- **内置标签**: div, span, p, h1-h6, a, button, input, form, table, ul, ol, li 等 70+ 个 HTML 标签
- **用法**: 定义自定义标签组件，包含样式和交互行为

```elisp
;; 定义自定义按钮
(define-etaf-tag my-button
  :display 'inline-block
  :default-style '((background-color . "blue")
                   (color . "white"))
  :hover-style '((background-color . "darkblue"))
  :on-click (lambda (event)
              (message "Button clicked!")))

;; 使用标签
(etaf-tag-to-dom '(my-button :class "primary" "Click Me"))
```

#### etaf-dom.el
- **目的**: DOM 操作、查询和遍历操作
- **依赖**: dom (内置)
- **公共接口**:
  - `etaf-dom-to-tml (sexp)` - 将 DOM 转换为 TML 格式
  - `etaf-alist-to-plist (alist)` - 将关联列表转换为属性列表
  - `etaf-dom-tag-match-p (node tag-name)` - 检查节点是否匹配标签选择器
  - `etaf-dom-class-match-p (node class-name)` - 检查节点是否匹配类选择器
  - `etaf-dom-id-match-p (node id-name)` - 检查节点是否匹配 ID 选择器
  - `etaf-dom-map (func dom)` - 遍历 DOM 树并对每个节点应用函数
  - `etaf-dom-get-previous-sibling (node dom)` - 获取前一个兄弟元素
  - `etaf-dom-get-previous-siblings (node dom)` - 获取所有前面的兄弟元素
  - `etaf-dom-is-descendant-of (node ancestor)` - 检查后代关系
  - `etaf-dom-get-parent (node dom)` - 获取父节点
  - `etaf-dom-get-element-children (node)` - 获取元素子节点（跳过文本节点）
  - `etaf-dom-is-first-child (node)` - 检查节点是否为第一个子元素
  - `etaf-dom-is-last-child (node)` - 检查节点是否为最后一个子元素
  - `etaf-dom-get-child-index (node)` - 获取子元素索引位置
  - `etaf-dom-is-first-of-type (node)` - 检查是否为该类型的第一个
  - `etaf-dom-is-last-of-type (node)` - 检查是否为该类型的最后一个
  - `etaf-dom-is-only-of-type (node)` - 检查是否为该类型的唯一一个
  - `etaf-dom-is-empty (node)` - 检查节点是否为空
  - 样式操作函数（ecss-dom-* 前缀）
  - 类操作函数（ecss-dom-* 前缀）
- **用法**: 操作和查询 DOM 结构，应用样式

### CSS 处理模块

#### etaf-css.el
- **目的**: 主 CSS 系统协调器和 CSSOM 构建器
- **依赖**: etaf-dom, etaf-css-selector, etaf-css-media, etaf-css-parser, etaf-css-cascade, etaf-css-inheritance, etaf-css-cache, etaf-css-index
- **公共接口**:
  - `etaf-css-build-cssom (dom &optional media-env)` - 从 DOM 构建 CSS 对象模型
  - `etaf-css-get-computed-style (cssom node dom)` - 获取节点的计算样式
  - `etaf-css-get-rules-for-node (cssom node dom)` - 获取节点的所有适用规则
  - `etaf-css-extract-inline-styles (dom)` - 从 DOM 提取内联样式
  - `etaf-css-extract-style-tags (dom)` - 提取 <style> 标签内容
  - `etaf-css-cssom-to-string (cssom)` - 将 CSSOM 转换为可读字符串
  - `etaf-css-clear-cache (cssom)` - 清空 CSSOM 缓存
- **CSSOM 结构**: plist，键为：:inline-rules, :style-rules, :all-rules, :rule-index, :cache, :media-env
- **用法**: CSS 操作的主要接口 - 构建 CSSOM 并查询计算样式

#### etaf-css-parser.el
- **目的**: 解析 CSS 声明、规则和样式表
- **依赖**: etaf-css-cascade
- **公共接口**:
  - `etaf-css-parse-declarations (css-string)` - 解析 CSS 声明（支持 !important）
  - `etaf-css-parse-rule (rule-string)` - 解析单个 CSS 规则
  - `etaf-css-parse-stylesheet (css-string &optional media-query)` - 解析完整样式表
- **声明格式**: `((property value important) ...)` 其中 important 是布尔值
- **用法**: 将 CSS 文本解析为结构化数据

#### etaf-css-selector.el
- **目的**: CSS 选择器解析和匹配（大型复杂模块）
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-selector-parse (selector-string)` - 将选择器解析为 AST
  - `etaf-css-selector-basic-match-p (node selector)` - 检查节点是否匹配选择器
  - `etaf-css-selector-query (selector-string dom)` - 使用选择器查询 DOM
- **内部函数**: 45+ 个函数用于词法分析、解析和 AST 操作
- **用法**: CSS 匹配的核心选择器引擎

#### etaf-css-cascade.el
- **目的**: CSS 层叠算法和选择器特异性计算
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-calculate-specificity (selector)` - 计算选择器特异性
  - `etaf-css-specificity> (spec1 spec2)` - 比较两个特异性值
  - `etaf-css-specificity= (spec1 spec2)` - 检查特异性是否相等
  - `etaf-css-cascade-compare-declarations (decl1 decl2)` - 比较声明优先级
  - `etaf-css-cascade-merge-rules (rules)` - 使用层叠算法合并规则
- **特异性格式**: `(id-count class-count type-count)`
- **层叠顺序**: (1) 普通声明 → (2) !important 声明 → (3) 内联普通 → (4) 内联 !important
- **用法**: 使用 CSS 层叠规则解决样式冲突

#### etaf-css-inheritance.el
- **目的**: CSS 属性继承系统
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-property-inherits-p (property)` - 检查属性是否可继承
  - `etaf-css-apply-inheritance (computed-style parent-style)` - 应用继承属性
- **可继承属性**: color, font-*, text-*, line-height, visibility 等
- **用法**: 将属性从父元素继承到子元素

#### etaf-css-cache.el
- **目的**: 计算样式缓存以优化性能
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-cache-create ()` - 创建新缓存
  - `etaf-css-cache-get (cache node)` - 获取节点的缓存样式
  - `etaf-css-cache-set (cache node style)` - 将样式存入缓存
  - `etaf-css-cache-clear (cache)` - 清空所有缓存数据
  - `etaf-css-cache-remove (cache node)` - 从缓存中移除特定节点
  - `etaf-css-cache-size (cache)` - 获取缓存条目数
- **实现**: 以节点为键的哈希表
- **用法**: 避免冗余的样式计算

#### etaf-css-index.el
- **目的**: CSS 规则索引以优化性能
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-index-build (rules)` - 从规则列表构建索引
  - `etaf-css-index-query-candidates (index node)` - 获取节点的候选规则
  - `etaf-css-index-extract-selector-keys (selector)` - 从选择器提取可索引的键
- **索引结构**: plist，包含 :by-tag、:by-class、:by-id 哈希表
- **用法**: 通过标签/类/ID 快速找到可能匹配的规则

#### etaf-css-media.el
- **目的**: 媒体查询评估和 @media 规则解析
- **依赖**: (无)
- **公共接口**:
  - `etaf-css-media-match-p (query-str &optional env)` - 检查媒体查询是否匹配
  - `etaf-css-media-extract-at-media-blocks (css-string)` - 提取 @media 块
  - `etaf-css-media-parse-feature (feature-str)` - 解析媒体特性表达式
  - `etaf-css-media-evaluate-feature (feature operator value &optional env)` - 评估特性
- **默认环境**: `((type . screen) (width . 1024) (height . 768))`
- **支持的特性**: width, height, min-width, max-width, min-height, max-height
- **用法**: 评估响应式设计的媒体查询

### 渲染模块

#### etaf-render.el
- **目的**: 从 DOM 和 CSSOM 构建渲染树
- **依赖**: etaf-dom, etaf-css
- **公共接口**:
  - `etaf-render-build-tree (dom cssom)` - 构建渲染树
  - `etaf-render-walk (render-tree func)` - 遍历渲染树
  - `etaf-render-get-style (render-node property)` - 获取样式属性值
  - `etaf-render-find-by-tag (render-tree tag)` - 按标签查找节点
  - `etaf-render-find-by-display (render-tree display)` - 按显示类型查找节点
  - `etaf-render-to-string (render-tree &optional indent)` - 将树转换为字符串
  - `etaf-render-stats (render-tree)` - 获取树统计信息
- **渲染节点结构**: plist，包含 :node、:tag、:computed-style、:display、:children
- **用法**: 为布局和绘制创建渲染树（排除 display:none、<script> 等）

### 测试工具

#### etaf-ert.el
- **目的**: 测试工具和宏
- **依赖**: ert (内置)
- **公共接口**:
  - `should-equal (s1 s2)` - 相等断言宏
- **用法**: 跨所有测试文件的一致测试接口

## 模块依赖图

```
etaf.el
├── etaf-etml.el (无依赖)
├── etaf-tag.el (无依赖) - 标签定义系统
└── etaf-css.el
    ├── etaf-dom.el
    │   └── dom (内置)
    ├── etaf-css-selector.el (无依赖)
    ├── etaf-css-media.el (无依赖)
    ├── etaf-css-parser.el
    │   └── etaf-css-cascade.el (无依赖)
    ├── etaf-css-cascade.el (无依赖)
    ├── etaf-css-inheritance.el (无依赖)
    ├── etaf-css-cache.el (无依赖)
    └── etaf-css-index.el (无依赖)

etaf-render.el
├── etaf-dom.el
└── etaf-css.el

etaf-ert.el
└── ert (内置)
```

## 优化总结

### 合并的模块

1. **etaf-utils.el** → 合并到 etaf-etml.el 和 etaf-dom.el
   - 原因：过于简单（仅 2 个工具函数），100% 外部使用率
   - 影响：函数移至实际使用它们的模块

2. **etaf-css-specificity.el** → 合并到 etaf-css-cascade.el
   - 原因：与层叠算法紧密耦合
   - 影响：相关功能现在在单个模块中

### 保留的模块

1. **etaf-ert.el** - 尽管简单但保留
   - 原因：提供一致的测试接口
   - 原因：在所有测试文件中使用
   - 原因：可能会扩展更多测试工具

2. **etaf-css-cache.el** - 作为独立模块保留
   - 原因：清晰、专注的缓存接口
   - 原因：67% 公共函数比率（4/6 函数导出）
   - 原因：可能有不同实现（内存 vs. 持久化）

3. **etaf-css-index.el** - 作为独立模块保留
   - 原因：清晰的索引抽象
   - 原因：60% 公共函数比率（3/5 函数导出）
   - 原因：可切换的性能优化

## 使用示例

### 基本 TML 到 DOM 转换
```elisp
(require 'etaf-etml)

(setq dom (etaf-etml-to-dom
           '(div :class "container"
                (h1 :id "title" "Hello World")
                (p "这是一个段落。"))))
;; => (div ((class . "container"))
;;         (h1 ((id . "title")) "Hello World")
;;         (p nil "这是一个段落。"))
```

### 构建 CSSOM 和查询样式
```elisp
(require 'etaf-css)

;; 创建带样式的 DOM
(setq dom (etaf-etml-to-dom
           '(html
              (head
                (style ".button { color: red; font-size: 14px; }"))
              (body
                (button :class "button" "点击我")))))

;; 构建 CSSOM
(setq cssom (etaf-css-build-cssom dom))

;; 获取计算样式
(setq button (dom-by-class dom "button"))
(setq styles (etaf-css-get-computed-style cssom button dom))
;; => ((color . "red") (font-size . "14px"))
```

### 构建渲染树
```elisp
(require 'etaf-render)

(setq render-tree (etaf-render-build-tree dom cssom))
(etaf-render-walk render-tree
  (lambda (node)
    (message "标签: %s, 显示: %s"
             (plist-get node :tag)
             (plist-get node :display))))
```

## 这种结构的优点

1. **清晰的边界**: 每个模块都有明确定义的目的
2. **最小耦合**: 依赖是明确且最小化的
3. **易于测试**: 模块可以独立测试
4. **性能**: 缓存和索引是独立的可选优化
5. **可维护性**: 相关功能被分组在一起
6. **可扩展性**: 可以添加新功能而不会破坏现有模块

## 未来改进

1. 如果 DOM 工具函数列表增长，考虑提取到独立模块
2. 如果 etaf-css-selector.el 变得过大，可能会拆分（目前 1132 行）
3. 考虑向 etaf-ert.el 添加更多测试工具以证明其存在
4. 可能在 etaf-css-cache.el 中添加持久化缓存选项
