# CSSOM 扁平结构 - 基于 Chromium 实现

## 概述

CSSOM (CSS Object Model) 采用扁平的 plist 结构，这是一个全局的规则集合，加上索引方便查询。这种设计参考了 Chromium 浏览器的实现，不使用树形结构，避免不必要的 DOM 复制。

## 设计原则

### 扁平结构模式

CSSOM 是一个简单的 plist（属性列表），包含：

1. **规则集合**：所有 CSS 规则的列表
2. **索引结构**：按标签、类、ID 分类的快速查询索引
3. **缓存机制**：计算样式的缓存
4. **环境信息**：媒体查询环境等元数据

```elisp
;; CSSOM 结构示例
(:ua-rules (...)           ; User Agent 样式规则
 :style-rules (...)        ; 样式表规则
 :inline-rules (...)       ; 内联样式规则
 :all-rules (...)          ; 所有规则（按优先级排序）
 :rule-index (...)         ; 规则索引（hash表）
 :cache #<hash-table>      ; 计算样式缓存
 :media-env (...))         ; 媒体查询环境
```

### 为什么采用扁平结构？

1. **简单直接**：CSSOM 就是规则的集合，不需要复杂的树形结构
2. **避免复制**：不需要深度复制整个 DOM 树
3. **性能优化**：通过索引快速查找匹配的规则
4. **符合标准**：与 Chromium 等浏览器的实现一致

## CSSOM 属性说明

### 规则集合属性

| 属性名 | 类型 | 说明 |
|--------|------|------|
| `:ua-rules` | list | User Agent 样式规则列表（最低优先级） |
| `:style-rules` | list | 样式表规则列表（来自 `<style>` 标签） |
| `:inline-rules` | list | 内联样式规则列表（最高优先级） |
| `:all-rules` | list | 所有规则（按优先级顺序：UA < Author < Inline） |

### 优化属性

| 属性名 | 类型 | 说明 |
|--------|------|------|
| `:rule-index` | plist | 规则索引（包含 :by-tag、:by-class、:by-id 三个 hash 表） |
| `:cache` | hash-table | 计算样式缓存 |
| `:media-env` | alist | 媒体查询环境 |

### 规则格式（plist）

每个 CSS 规则是一个 plist：

```elisp
(:selector "div.button"
 :declarations ((color "red" nil) (font-size "14px" t))  ; (property value important)
 :specificity (0 1 1)  ; (id class type)
 :source inline|style-tag|ua
 :media "screen and (min-width: 768px)"  ; 可选
 :node <dom-node>)  ; 仅内联样式有此字段
```

## API 使用

### 构建 CSSOM

```elisp
(require 'etaf-css)
(require 'etaf-etml)

;; 创建 DOM
(setq dom (etaf-etml-to-dom
           '(html
             (head
              (style "div { color: blue; font-size: 16px; }"))
             (body
              (div :id "test" :style "color: red;" "Hello")))))

;; 构建 CSSOM（返回扁平的 plist）
(setq cssom (etaf-css-build-cssom dom))

;; CSSOM 是一个 plist
(keywordp (car cssom))  ; => t
```

### 访问 CSSOM 属性

```elisp
;; 获取 CSSOM 属性
(setq cache (plist-get cssom :cache))
(setq all-rules (plist-get cssom :all-rules))
(setq rule-index (plist-get cssom :rule-index))
(setq media-env (plist-get cssom :media-env))

;; 查看规则数量
(length (plist-get cssom :ua-rules))
(length (plist-get cssom :style-rules))
(length (plist-get cssom :inline-rules))

;; 注意：空列表在 plist-get 中返回 nil
;; 使用 plist-member 检查键是否存在
(plist-member cssom :style-rules)  ; => (:style-rules () ...)
```

### 查询节点样式

```elisp
;; 获取匹配节点的规则
(let* ((div-node (dom-by-id dom "test"))
       (rules (etaf-css-get-rules-for-node cssom div-node dom)))
  (message "Found %d rules" (length rules)))

;; 计算节点的最终样式
(let* ((div-node (dom-by-id dom "test"))
       (computed-style (etaf-css-get-computed-style cssom div-node dom)))
  (message "Color: %s" (cdr (assq 'color computed-style)))
  (message "Font size: %s" (cdr (assq 'font-size computed-style))))
```

### 动态添加样式表

```elisp
;; 向 CSSOM 添加新的 CSS 规则
(etaf-css-add-stylesheet cssom ".new-class { margin: 10px; }")

;; 清空缓存以使新样式生效
(etaf-css-clear-cache cssom)
```

## 实现细节

### 构建过程

1. **提取规则**：
   - 从 `<style>` 标签提取 CSS 规则
   - 从 `style` 属性提取内联样式
   - 加载 User Agent 样式表

2. **构建索引**：
   - 按标签、类、ID 建立规则索引
   - 提高规则匹配性能

3. **创建 plist**：
   - 将所有信息组织成扁平的 plist
   - 不复制 DOM 树

### 规则匹配

使用规则索引快速查找候选规则，然后逐个测试选择器匹配：

```elisp
;; 1. 从索引获取候选规则
(let ((candidates (etaf-css-index-query-candidates 
                   (plist-get cssom :rule-index) 
                   node)))
  ;; 2. 测试选择器匹配
  (dolist (rule candidates)
    (when (etaf-css-selector-node-matches-p node dom selector-ast)
      (push rule matching-rules))))
```

### 样式缓存

计算样式会被缓存在 `:cache` 中：

```elisp
(let ((cache (plist-get cssom :cache)))
  ;; 检查缓存
  (or (etaf-css-cache-get cache node)
      ;; 缓存未命中，计算并存储
      (let ((computed (etaf-css-cascade-merge-rules rules)))
        (etaf-css-cache-set cache node computed)
        computed)))
```

## 与 Render Tree 和 Layout Tree 的关系

### 数据流

```
DOM (树形结构)
  ↓
CSSOM (扁平结构) ← 不同于 DOM！
  ↓
Render Tree (树形结构，基于 DOM)
  ↓
Layout Tree (树形结构，基于 DOM)
```

### 关键区别

- **DOM**: 树形结构，表示文档层次
- **CSSOM**: 扁平结构，全局规则集合
- **Render Tree**: 树形结构（基于 DOM，附加渲染信息）
- **Layout Tree**: 树形结构（基于 DOM，附加布局信息）

### CSSOM 的独特性

CSSOM 是唯一不使用树形结构的组件，因为：
1. CSS 规则本质上是全局的
2. 规则通过选择器匹配，不需要树形结构
3. 扁平结构更高效，更易于索引和查询

## 最佳实践

### 1. 访问 CSSOM 属性

```elisp
;; ✓ 正确：使用 plist-get
(plist-get cssom :cache)
(plist-get cssom :all-rules)

;; ✓ 正确：检查键是否存在
(plist-member cssom :style-rules)

;; ✗ 错误：CSSOM 不是 DOM 树
(dom-attr cssom 'cssom-cache)  ; 不再有效
```

### 2. 查询样式

```elisp
;; ✓ 正确：传入 DOM 和 CSSOM 作为独立参数
(etaf-css-get-computed-style cssom node dom)

;; ✓ 正确：CSSOM 是独立的数据结构
(let ((cssom (etaf-css-build-cssom dom)))
  ;; cssom 和 dom 是两个不同的对象
  ...)
```

### 3. 性能考虑

```elisp
;; ✓ 好：利用索引快速查询
(etaf-css-get-rules-for-node cssom node dom)

;; ✓ 好：利用缓存避免重复计算
(etaf-css-get-computed-style cssom node dom)

;; ✓ 好：CSSOM 构建不复制 DOM，性能更好
(etaf-css-build-cssom dom)
```

## 性能优势

### 与旧实现（树结构）对比

| 方面 | 旧实现（树结构） | 新实现（扁平结构） |
|------|-----------------|-------------------|
| DOM 复制 | 需要深度复制整个 DOM | 不需要复制 |
| 内存使用 | 高（双倍 DOM） | 低（只存储规则） |
| 构建速度 | 慢（复制开销） | 快（只组织规则） |
| 查询效率 | 需要树遍历 | 直接索引查询 |
| 符合标准 | 非标准设计 | 符合浏览器实现 |

## 总结

CSSOM 采用扁平的 plist 结构，参考 Chromium 浏览器实现：

- **核心**：全局规则集合 + 索引
- **优势**：简单、高效、符合标准
- **设计**：不需要树形结构
- **性能**：避免 DOM 复制，快速索引查询

这种设计使 ETAF 的 CSS 系统更加高效和易于理解。
