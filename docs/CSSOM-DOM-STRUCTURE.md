# CSSOM 树结构 - 基于 DOM 节点

## 概述

CSSOM (CSS Object Model) 采用与 render tree 和 layout tree 相同的设计模式：基于原始 DOM 的标签节点结构，在节点上附加 CSSOM 相关属性。

## 设计原则

### 统一的树结构模式

ETAF 中的所有树结构（DOM、CSSOM、Render Tree、Layout Tree）都遵循相同的模式：

1. **DOM 树**：原始文档结构
   ```elisp
   (html ((id . "root"))
     (head nil
       (style nil "div { color: blue; }"))
     (body nil
       (div ((id . "test") (style . "color: red;")) "Text")))
   ```

2. **CSSOM 树**：DOM 结构 + CSSOM 属性
   ```elisp
   (html ((cssom-ua-rules . (...))
          (cssom-style-rules . (...))
          (cssom-inline-rules . (...))
          (cssom-all-rules . (...))
          (cssom-rule-index . (...))
          (cssom-cache . #<hash-table>)
          (cssom-media-env . (...))
          (id . "root"))
     (head nil
       (style nil "div { color: blue; }"))
     (body nil
       (div ((id . "test") (style . "color: red;")) "Text")))
   ```

3. **Render Tree**：DOM 结构 + 渲染属性
   ```elisp
   (html ((render-style . ((color . "black") ...))
          (render-display . "block"))
     ...)
   ```

4. **Layout Tree**：DOM 结构 + 布局属性
   ```elisp
   (html ((layout-box-model . (:content ...))
          (render-style . (...)))
     ...)
   ```

### 为什么这样设计？

1. **保持 DOM 结构**：不创建新的树结构，而是增强现有的 DOM 树
2. **统一的API**：所有树都可以用 `dom-tag`、`dom-attr`、`dom-children` 等函数操作
3. **模块化**：每个层次只添加自己需要的属性，不破坏原有结构
4. **可组合性**：可以轻松地在同一个树上组合多个层次的信息

## CSSOM 属性说明

### 全局属性（附加在根节点）

| 属性名 | 类型 | 说明 |
|--------|------|------|
| `cssom-ua-rules` | list | User Agent 样式规则列表（最低优先级） |
| `cssom-style-rules` | list | 样式表规则列表（来自 `<style>` 标签） |
| `cssom-inline-rules` | list | 内联样式规则列表（最高优先级） |
| `cssom-all-rules` | list | 所有规则（按优先级顺序：UA < Author < Inline） |
| `cssom-rule-index` | plist | 规则索引（按标签、类、ID 分类） |
| `cssom-cache` | hash-table | 计算样式缓存 |
| `cssom-media-env` | alist | 媒体查询环境 |

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

;; 构建 CSSOM（返回带 CSSOM 属性的 DOM 树）
(setq cssom (etaf-css-build-cssom dom))

;; CSSOM 是一个 DOM 树，可以用 DOM 函数操作
(dom-tag cssom)  ; => 'html
(dom-children cssom)  ; => ((head ...) (body ...))
```

### 访问 CSSOM 属性

```elisp
;; 获取全局 CSSOM 属性
(setq cache (dom-attr cssom 'cssom-cache))
(setq all-rules (dom-attr cssom 'cssom-all-rules))
(setq rule-index (dom-attr cssom 'cssom-rule-index))
(setq media-env (dom-attr cssom 'cssom-media-env))

;; 查看规则数量
(length (dom-attr cssom 'cssom-ua-rules))
(length (dom-attr cssom 'cssom-style-rules))
(length (dom-attr cssom 'cssom-inline-rules))
```

### 查询节点样式

```elisp
;; 获取匹配节点的规则
(let* ((div-node (dom-by-id cssom "test"))
       (rules (etaf-css-get-rules-for-node cssom div-node cssom)))
  (message "Found %d rules" (length rules)))

;; 计算节点的最终样式
(let* ((div-node (dom-by-id cssom "test"))
       (computed-style (etaf-css-get-computed-style cssom div-node cssom)))
  (message "Color: %s" (cdr (assq 'color computed-style)))
  (message "Font size: %s" (cdr (assq 'font-size computed-style))))
```

### 遍历 CSSOM 树

```elisp
;; 使用 etaf-dom-map 遍历（就像遍历普通 DOM 树）
(etaf-dom-map
  (lambda (node)
    (when (eq (dom-tag node) 'div)
      (let ((id (dom-attr node 'id))
            (style-attr (dom-attr node 'style)))
        (message "Div: id=%s, style=%s" id style-attr))))
  cssom)
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

3. **附加到 DOM**：
   - 复制 DOM 结构
   - 在根节点附加 CSSOM 全局属性
   - 子节点保持原样

### 节点复制

CSSOM 构建时会深度复制 DOM 树，确保不修改原始 DOM：

```elisp
(defun etaf-css--copy-dom-node (node)
  "复制 DOM 节点（深度复制）。"
  (if (and (listp node) (symbolp (car node)))
      (let ((tag (dom-tag node))
            (attrs (dom-attributes node))
            (children (dom-children node)))
        (cons tag
              (cons attrs
                    (mapcar (lambda (child)
                              (if (and (listp child) (symbolp (car child)))
                                  (etaf-css--copy-dom-node child)
                                child))
                            children))))
    node))
```

### 规则匹配

使用规则索引快速查找候选规则，然后逐个测试选择器匹配：

```elisp
;; 1. 从索引获取候选规则
(let ((candidates (etaf-css-index-query-candidates 
                   (dom-attr cssom 'cssom-rule-index) 
                   node)))
  ;; 2. 测试选择器匹配
  (dolist (rule candidates)
    (when (etaf-css-selector-node-matches-p node cssom selector-ast)
      (push rule matching-rules))))
```

### 样式缓存

计算样式会被缓存在 `cssom-cache` 中：

```elisp
(let ((cache (dom-attr cssom 'cssom-cache)))
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
DOM 
  ↓
CSSOM (DOM + CSS 属性)
  ↓
Render Tree (DOM + 渲染属性)
  ↓
Layout Tree (DOM + 布局属性)
```

### 共同特点

1. 都基于 DOM 节点结构
2. 都通过属性附加信息
3. 都可以用 DOM 函数操作
4. 都保持树的层次关系

### 示例对比

**CSSOM 节点**：
```elisp
(html ((cssom-cache . #<hash>)
       (cssom-all-rules . (...)))
  (body nil
    (div ((id . "test")) "Text")))
```

**Render Tree 节点**：
```elisp
(html ((render-style . ((color . "black")))
       (render-display . "block"))
  (body ((render-display . "block"))
    (div ((render-style . ((color . "red")))
          (render-display . "block")
          (id . "test")) "Text")))
```

**Layout Tree 节点**：
```elisp
(html ((layout-box-model . (:content ...))
       (render-style . (...))
       (render-display . "block"))
  ...)
```

## 最佳实践

### 1. 访问 CSSOM 属性

```elisp
;; ✓ 正确：使用 dom-attr
(dom-attr cssom 'cssom-cache)

;; ✗ 错误：不要使用 plist-get（CSSOM 不是 plist）
(plist-get cssom :cache)
```

### 2. 遍历树

```elisp
;; ✓ 正确：使用 etaf-dom-map
(etaf-dom-map func cssom)

;; ✓ 正确：使用 dom-children
(dolist (child (dom-children cssom))
  ...)
```

### 3. 节点查询

```elisp
;; ✓ 正确：使用 dom-by-id, dom-by-class, dom-by-tag
(dom-by-id cssom "test")
(dom-by-class cssom "button")
(dom-by-tag cssom 'div)
```

### 4. 样式计算

```elisp
;; ✓ 正确：传入 CSSOM 而不是原始 DOM
(etaf-css-get-computed-style cssom node cssom)

;; 注意：node 可以是原始 DOM 节点或 CSSOM 节点
```

## 性能考虑

### 规则索引

索引按标签、类、ID 分类，避免对每个节点测试所有规则：

```elisp
;; 索引结构
(:by-tag #<hash-table tag -> rules>
 :by-class #<hash-table class -> rules>
 :by-id #<hash-table id -> rules>)

;; 查询时只检查相关规则
(etaf-css-index-query-candidates index node)
```

### 样式缓存

避免重复计算相同节点的样式：

```elisp
;; 第一次：计算并缓存
(etaf-css-get-computed-style cssom node cssom)  ; 慢

;; 第二次：从缓存读取
(etaf-css-get-computed-style cssom node cssom)  ; 快
```

### 深度复制开销

CSSOM 构建时会复制整个 DOM 树。对于大型 DOM：
- 只在需要时构建 CSSOM
- 复用 CSSOM 而不是重复构建
- 在 DOM 变化时重建 CSSOM

## 总结

CSSOM 采用与 DOM、Render Tree、Layout Tree 相同的设计模式：

- **基础**：保持 DOM 的标签节点结构
- **扩展**：通过属性附加 CSSOM 信息
- **操作**：使用统一的 DOM API
- **优化**：规则索引和样式缓存

这种设计使 ETAF 的各个层次保持一致，便于理解和使用。
