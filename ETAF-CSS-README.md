# ETAF-CSS 实现文档

## 概述

`etaf-css.el` 是 ETAF 项目的 CSS 解析模块，用于从 DOM 树中提取内联样式和外部样式，并构建 CSSOM (CSS Object Model)。

## 主要功能

### 1. CSS 声明解析
解析 CSS 属性声明字符串（如 `style` 属性的值）。

```elisp
(etaf-css-parse-declarations "color: red; font-size: 14px;")
;; => ((color . "red") (font-size . "14px"))
```

### 2. CSS 规则解析
解析完整的 CSS 规则（包括选择器和声明块）。

```elisp
(etaf-css-parse-rule "div { color: red; }")
;; => (:selector "div" :declarations ((color . "red")) :source style-tag)
```

### 3. CSS 样式表解析
解析包含多个规则的 CSS 样式表。

```elisp
(etaf-css-parse-stylesheet "div { color: red; } .button { background: blue; }")
;; => 返回包含两个规则的列表
```

### 4. 内联样式提取
从 DOM 节点的 `style` 属性提取内联样式。

```elisp
(etaf-css-extract-inline-styles dom)
;; => 返回所有内联样式规则列表
```

### 5. 外部样式提取
从 `<style>` 标签中提取 CSS 规则。

```elisp
(etaf-css-extract-style-tags dom)
;; => 返回所有外部样式规则列表
```

### 6. CSSOM 构建
构建完整的 CSS 对象模型，包含所有样式规则。

```elisp
(etaf-css-build-cssom dom)
;; => 返回 CSSOM 结构
;;    (:inline-rules ... :style-rules ... :all-rules ...)
```

### 7. 样式查询和计算
查询匹配特定节点的样式规则，并计算层叠后的最终样式。

```elisp
(etaf-css-get-computed-style cssom node dom)
;; => 返回该节点的计算样式
```

## CSSOM 结构

CSSOM 是一个 plist，包含以下键：

- `:inline-rules` - 所有内联样式规则
- `:style-rules` - 所有 `<style>` 标签中的规则
- `:all-rules` - 合并后的所有规则（外部样式在前，内联样式在后）

每个规则的结构：

```elisp
(:selector "div.class"
 :declarations ((color . "red") (font-size . "14px"))
 :source inline  ; 或 style-tag
 :node <node>)   ; 仅对内联样式有效
```

## 使用示例

### 完整示例

```elisp
;; 1. 创建 DOM
(setq my-dom
  (etaf-tml-to-dom
   '(html
      (head
        (style "
          .header { background: navy; color: white; }
          .highlight { background: yellow; }
        "))
      (body
        (div :class "header" :style "padding: 20px;"
          (h1 "标题"))
        (p :class "highlight" :style "font-weight: bold;"
          "重要内容")))))

;; 2. 构建 CSSOM
(setq my-cssom (etaf-css-build-cssom my-dom))

;; 3. 获取特定节点
(setq highlight-para (dom-by-class my-dom "highlight"))

;; 4. 计算该节点的最终样式
(setq computed-styles 
  (etaf-css-get-computed-style my-cssom highlight-para my-dom))

;; computed-styles 将包含：
;; ((background . "yellow")      ; 来自 .highlight 规则
;;  (font-weight . "bold"))      ; 来自内联样式
```

## 样式层叠规则

实现遵循 CSS 层叠规则：

1. 外部样式（`<style>` 标签）先应用
2. 内联样式（`style` 属性）后应用，覆盖外部样式
3. 相同属性，后面的值覆盖前面的值

## API 参考

### 解析函数

#### `etaf-css-parse-declarations`
解析 CSS 声明字符串。

**参数:**
- `css-string` - CSS 声明字符串

**返回:** alist `((property . value) ...)`

#### `etaf-css-parse-rule`
解析单个 CSS 规则。

**参数:**
- `rule-string` - CSS 规则字符串

**返回:** plist `(:selector ... :declarations ... :source ...)`

#### `etaf-css-parse-stylesheet`
解析 CSS 样式表。

**参数:**
- `css-string` - CSS 样式表字符串

**返回:** 规则列表

### 提取函数

#### `etaf-css-extract-inline-styles`
从 DOM 提取内联样式。

**参数:**
- `dom` - DOM 树

**返回:** 内联样式规则列表

#### `etaf-css-extract-style-tags`
从 DOM 提取 `<style>` 标签中的样式。

**参数:**
- `dom` - DOM 树

**返回:** 样式规则列表

### CSSOM 函数

#### `etaf-css-build-cssom`
构建 CSSOM。

**参数:**
- `dom` - DOM 树

**返回:** CSSOM 结构

#### `etaf-css-get-rules-for-node`
获取匹配节点的所有规则。

**参数:**
- `cssom` - CSSOM 结构
- `node` - DOM 节点
- `dom` - 根 DOM 节点

**返回:** 匹配的规则列表

#### `etaf-css-get-computed-style`
计算节点的最终样式。

**参数:**
- `cssom` - CSSOM 结构
- `node` - DOM 节点
- `dom` - 根 DOM 节点

**返回:** 计算后的样式 alist

### 工具函数

#### `etaf-css-rule-to-string`
将规则转换为字符串。

**参数:**
- `rule` - 规则 plist

**返回:** CSS 规则字符串

#### `etaf-css-cssom-to-string`
将 CSSOM 转换为 CSS 字符串。

**参数:**
- `cssom` - CSSOM 结构

**返回:** 完整的 CSS 字符串

## 测试

测试文件位于 `tests/etaf-css-tests.el`，包含以下测试：

- CSS 声明解析测试
- CSS 规则解析测试
- 样式表解析测试
- 内联样式提取测试
- 外部样式提取测试
- CSSOM 构建测试
- 样式查询测试
- 样式计算测试
- 样式层叠测试

## 限制和注意事项

1. **CSS 解析简化**: 当前实现使用正则表达式进行简单的 CSS 解析，不支持复杂的 CSS 语法（如媒体查询、@规则等）
2. **选择器匹配**: 依赖 `etaf-css-selector.el` 进行选择器匹配
3. **继承**: 当前不实现 CSS 属性继承，只返回直接应用的样式
4. **特异性**: 简化的特异性计算，主要依靠规则顺序和内联样式优先级

## 未来改进

可能的改进方向：

1. 支持更复杂的 CSS 语法解析
2. 实现 CSS 属性继承
3. 完整的选择器特异性计算
4. 支持 CSS 变量 (custom properties)
5. 支持 `@import` 规则
6. 支持媒体查询

## 许可证

GNU General Public License v3.0 或更高版本
