# CSSOM 设计对比：当前实现 vs 浏览器标准

## 快速对比表

| 特性 | 当前实现 | 浏览器 CSSOM | 推荐改进 |
|------|---------|-------------|---------|
| **基本结构** | ✓ 扁平化 plist | ✓ 层次化对象 | ✓ 保持扁平 |
| **规则分类** | ✓ 按来源分类 | ✓ 按样式表分类 | ✓ 增强分类 |
| **特异性计算** | ✗ 不支持 | ✓ 完整支持 | ✓ **必须添加** |
| **!important** | ✗ 不支持 | ✓ 支持 | ○ 建议添加 |
| **层叠算法** | △ 简化版 | ✓ 完整实现 | ✓ **必须改进** |
| **继承** | ✗ 不支持 | ✓ 支持 | ○ 可选 |
| **缓存** | ✗ 不支持 | ✓ 高度优化 | ✓ **建议添加** |
| **媒体查询** | ✗ 不支持 | ✓ 支持 | ○ 可选 |
| **@规则** | ✗ 不支持 | ✓ 全部支持 | ○ 未来扩展 |
| **动态更新** | ✗ 不支持 | ✓ 实时更新 | ○ 未来扩展 |

图例：✓ 支持 | △ 部分支持 | ✗ 不支持 | ○ 可选

---

## 结构对比

### 当前实现

```
CSSOM
├── inline-rules
│   └── [规则1, 规则2, ...]
├── style-rules  
│   └── [规则3, 规则4, ...]
└── all-rules
    └── [所有规则按顺序]

规则结构:
{
  selector: "div.class",
  declarations: [(prop . value), ...],
  source: inline|style-tag,
  node: <ref>  // 仅 inline
}
```

**优点：**
- ✓ 简单直观
- ✓ 易于实现
- ✓ 满足基本需求

**缺点：**
- ✗ 无特异性计算
- ✗ 层叠算法过于简单
- ✗ 无性能优化

---

### 浏览器 CSSOM (W3C 标准)

```
Document
└── styleSheets [CSSStyleSheetList]
    ├── CSSStyleSheet (inline)
    │   ├── ownerNode: <style>
    │   ├── cssRules [CSSRuleList]
    │   │   ├── CSSStyleRule
    │   │   │   ├── selectorText: "div"
    │   │   │   └── style: CSSStyleDeclaration
    │   │   │       ├── color: "red"
    │   │   │       ├── cssText: "color: red;"
    │   │   │       └── [所有 CSS 属性]
    │   │   ├── CSSMediaRule
    │   │   │   ├── conditionText: "(min-width: 600px)"
    │   │   │   └── cssRules: [嵌套规则]
    │   │   └── ...
    │   └── media: MediaList
    └── CSSStyleSheet (external)
        ├── href: "styles.css"
        └── ...

计算样式:
getComputedStyle(element)
├── 收集所有匹配规则
├── 按特异性排序
├── 应用层叠算法
├── 处理继承
└── 返回计算值
```

**优点：**
- ✓ 完整的规则支持
- ✓ 精确的层叠控制
- ✓ 高性能
- ✓ 标准化 API

**缺点：**
- ✗ 非常复杂
- ✗ 难以实现

---

### 推荐的增强实现

```
CSSOM
├── stylesheets
│   ├── [样式表1]
│   │   ├── id: 1
│   │   ├── type: inline|style-tag
│   │   ├── media: "all"
│   │   └── rules
│   │       └── [规则列表]
│   │           ├── selector: "div.class"
│   │           ├── specificity: (0, 1, 1)  // NEW
│   │           ├── declarations
│   │           │   └── [(prop, value, important)]  // NEW
│   │           └── source-order: 1
│   └── [样式表2]
│       └── ...
├── rule-index (NEW)
│   ├── by-tag: {div: [rule1, rule2], ...}
│   ├── by-class: {button: [rule3], ...}
│   └── by-id: {main: [rule4], ...}
└── computed-cache (NEW)
    └── {node1: styles, node2: styles, ...}
```

**优点：**
- ✓ 保持简单
- ✓ 添加关键特性
- ✓ 性能优化
- ✓ 易于扩展

**缺点：**
- △ 不支持所有 CSS 特性
- △ 需要更多代码

---

## 层叠算法对比

### 当前实现

```elisp
;; 简化的层叠：后面的覆盖前面的
(dolist (rule rules)
  (dolist (decl declarations)
    (let ((prop (car decl)))
      (if (assq prop result)
          (setcdr (assq prop result) (cdr decl))  ; 覆盖
        (push decl result)))))
```

**问题：**
- 不考虑特异性
- 不支持 !important
- inline 样式的优先级处理简单

---

### 浏览器标准层叠

```
层叠顺序（从低到高）:
1. User agent (浏览器默认)
2. User (用户设置)
3. Author - 正常声明
   ├── 按来源: external < style tag < inline
   ├── 按特异性: (id, class, type)
   └── 按顺序: 后定义优先
4. Author - !important 声明
   └── (顺序相反)
5. User !important
6. User agent !important
```

**特异性计算规则：**
```
(a, b, c, d)
a = inline 样式 (1 或 0)
b = ID 选择器数量
c = 类/属性/伪类选择器数量  
d = 标签/伪元素选择器数量

示例:
style="..."           => (1, 0, 0, 0)
#nav                  => (0, 1, 0, 0)
.button               => (0, 0, 1, 0)
div                   => (0, 0, 0, 1)
#nav .button div      => (0, 1, 1, 1)
```

---

### 推荐的增强层叠

```elisp
;; 完整的层叠算法
(defun compare-declarations (d1 d2)
  (cond
   ;; 1. !important 优先
   ((and (important? d1) (not (important? d2))) t)
   ((and (important? d2) (not (important? d1))) nil)
   
   ;; 2. inline 优先
   ((and (inline? d1) (not (inline? d2))) t)
   ((and (inline? d2) (not (inline? d1))) nil)
   
   ;; 3. 特异性比较
   ((specificity> d1 d2) t)
   ((specificity> d2 d1) nil)
   
   ;; 4. 文档顺序（后定义优先）
   (t (> (order d1) (order d2)))))
```

---

## 性能对比

### 当前实现

```
查询节点样式:
1. 遍历所有规则 O(n)
2. 选择器匹配 O(m)
3. 声明合并 O(k)

总复杂度: O(n * m * k)
```

---

### 浏览器优化

```
查询节点样式:
1. 索引查找匹配规则 O(1)
2. 缓存命中 O(1)
3. 选择器匹配（仅候选） O(m')
4. 特异性排序 O(m' log m')

总复杂度: O(m' log m')  where m' << m
```

**优化技术：**
- 规则索引（按标签、类、ID）
- 计算样式缓存
- 增量更新
- Bloom filters
- 并行处理

---

### 推荐的优化

```elisp
;; 1. 规则索引
(build-rule-index rules)
;; => {tag: {div: [r1, r2]}, class: {button: [r3]}, ...}

;; 2. 快速查找候选规则
(query-candidate-rules node index)
;; 只检查可能匹配的规则

;; 3. 缓存计算样式
(cache-computed-style node styles)
;; 避免重复计算
```

**预期提升：**
- 查询速度：10-100x
- 内存：增加 20-50%
- 实现复杂度：+30%

---

## 实施建议

### 第一阶段（必需）：基础增强

**目标：** 修复层叠算法

```elisp
;; 1. 添加特异性计算
(defun calculate-specificity (selector)
  ;; 返回 (id-count class-count type-count)
  ...)

;; 2. 增强规则结构
(:selector "div.class"
 :declarations ((color . "red"))
 :specificity (0 1 1)  ; NEW
 :source inline
 :order 1)  ; NEW

;; 3. 改进层叠算法
(sort rules by specificity and order)
```

**工作量：** 2-3 小时  
**影响：** 修复 CSS 行为  
**优先级：** 🔴 高

---

### 第二阶段（推荐）：性能优化

**目标：** 10x 性能提升

```elisp
;; 1. 添加规则索引
(:rule-index
 (:by-tag ...)
 (:by-class ...)
 (:by-id ...))

;; 2. 添加缓存
(:computed-cache <hash-table>)

;; 3. 优化查询
(use-index-for-query node)
```

**工作量：** 3-4 小时  
**影响：** 显著性能提升  
**优先级：** 🟡 中

---

### 第三阶段（可选）：功能扩展

**目标：** 更多 CSS 特性

```elisp
;; 1. !important 支持
(declarations ((color "red" t)))  ; important flag

;; 2. 基础媒体查询
(:media "(min-width: 600px)"
 :rules ...)

;; 3. CSS 变量
(declarations ((--main-color . "blue")))
```

**工作量：** 4-6 小时  
**影响：** 功能完善  
**优先级：** 🟢 低

---

## 总结

### 当前设计评估

**适用场景：**
- ✓ 简单的样式处理
- ✓ 小型 DOM 树（< 100 节点）
- ✓ 不需要精确的 CSS 语义

**限制：**
- ✗ CSS 层叠不正确
- ✗ 性能受限
- ✗ 缺少关键特性

### 推荐行动

1. **立即实施（第一阶段）**
   - 添加特异性计算
   - 修复层叠算法
   - 确保 CSS 语义正确

2. **近期实施（第二阶段）**
   - 添加缓存机制
   - 构建规则索引
   - 提升查询性能

3. **未来考虑（第三阶段）**
   - !important 支持
   - 媒体查询
   - CSS 变量

### 预期成果

实施所有建议后：
- ✓ CSS 行为正确（符合标准）
- ✓ 性能提升 10-100x
- ✓ 支持更多特性
- ✓ 易于维护和扩展

---

## 参考资源

**W3C 规范：**
- [CSSOM](https://www.w3.org/TR/cssom-1/)
- [CSS Cascade](https://www.w3.org/TR/css-cascade-4/)
- [CSS Selectors](https://www.w3.org/TR/selectors-4/)

**浏览器实现：**
- [Chromium Style Resolver](https://chromium.googlesource.com/chromium/src/+/refs/heads/main/third_party/blink/renderer/core/css/)
- [Firefox Style System](https://firefox-source-docs.mozilla.org/layout/style-system.html)

**相关文章：**
- [How Browser Engines Work](https://hacks.mozilla.org/2017/08/inside-a-super-fast-css-engine-quantum-css-aka-stylo/)
- [CSS Performance](https://developers.google.com/web/fundamentals/performance/rendering/reduce-the-scope-and-complexity-of-style-calculations)
