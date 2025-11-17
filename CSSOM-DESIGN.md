# CSSOM 设计说明与改进建议

## 当前设计

### 当前 CSSOM 结构

```elisp
(:inline-rules (
  (:selector "div#id.class"
   :declarations ((color . "red") (font-size . "14px"))
   :source inline
   :node <dom-node>)
  ...)
 :style-rules (
  (:selector "div"
   :declarations ((color . "blue"))
   :source style-tag)
  ...)
 :all-rules (...))  ; 合并的所有规则
```

### 设计理由

1. **简单扁平化结构**
   - 使用 plist 格式，符合 Emacs Lisp 惯例
   - 易于访问和操作
   - 适合小到中型 DOM 树

2. **按来源分类**
   - `:inline-rules` - 内联样式
   - `:style-rules` - 外部样式
   - `:all-rules` - 合并后的规则（用于快速查询）

3. **规则结构**
   - `:selector` - CSS 选择器字符串
   - `:declarations` - 样式声明 alist
   - `:source` - 来源标识（inline/style-tag）
   - `:node` - 仅内联样式有，直接引用 DOM 节点

## 浏览器 CSSOM 实现

### W3C CSSOM 标准结构

浏览器中的 CSSOM 遵循 W3C 标准，结构更加复杂：

```javascript
// CSSStyleSheet
{
  cssRules: [
    // CSSStyleRule
    {
      selectorText: "div.class",
      style: CSSStyleDeclaration {
        color: "red",
        fontSize: "14px",
        // ... 所有 CSS 属性
      },
      parentStyleSheet: CSSStyleSheet,
      parentRule: null
    },
    // CSSMediaRule
    {
      media: MediaList,
      cssRules: [...],
      conditionText: "(min-width: 600px)"
    },
    // CSSImportRule
    {
      href: "external.css",
      styleSheet: CSSStyleSheet
    }
  ],
  href: "style.css",
  ownerNode: <style> or <link>,
  disabled: false,
  media: MediaList,
  title: null,
  parentStyleSheet: null
}
```

### 关键特性

1. **层次化结构**
   - CSSStyleSheet 包含 CSSRuleList
   - 支持嵌套规则（@media, @supports 等）
   - 规则之间有父子关系

2. **完整的规则类型**
   - CSSStyleRule - 普通样式规则
   - CSSMediaRule - 媒体查询
   - CSSImportRule - @import
   - CSSKeyframesRule - 动画
   - CSSFontFaceRule - 字体定义
   - 等等...

3. **CSSStyleDeclaration**
   - 提供所有 CSS 属性的访问
   - 支持 camelCase 和 kebab-case
   - 计算值和指定值分离
   - 实时更新

4. **级联和继承**
   - 特异性计算（Specificity）
   - 重要性（!important）
   - 来源优先级（user agent → user → author → inline）
   - 继承属性的处理

5. **性能优化**
   - 样式表索引
   - 选择器哈希
   - 增量更新
   - 缓存计算样式

## 改进建议

### 方案 1：增强型简化 CSSOM（推荐）

保持简单但增加关键特性：

```elisp
(:stylesheets (
  ;; 每个样式表
  (:source inline|style-tag|external
   :href nil  ; 仅 external 有值
   :media "all"  ; 媒体查询
   :disabled nil
   :rules (
     (:type style|media|import
      :selector "div.class"
      :selector-specificity (0 1 1)  ; [id, class, type]
      :declarations ((color . "red" :important nil))
      :media nil  ; 仅 media 规则
      :rules nil)  ; 仅 media/supports 规则
     ...))
  ...)
 :computed-styles-cache (
   ;; 缓存节点的计算样式
   (<node> . ((color . "red") ...))
   ...))
```

**优点：**
- 保持简单性
- 支持基本的样式表层次
- 添加特异性计算
- 性能缓存
- 向后兼容

### 方案 2：完整 CSSOM 模拟

更接近浏览器实现：

```elisp
;; CSSStyleSheet 对象
(defstruct etaf-css-stylesheet
  href          ; 样式表 URL
  owner-node    ; <style> 或 <link> 节点
  css-rules     ; CSSRule 列表
  disabled      ; 是否禁用
  media         ; 媒体查询列表
  title         ; 标题
  type          ; "text/css"
  parent-sheet) ; 父样式表（用于 @import）

;; CSSRule 基类
(defstruct etaf-css-rule
  type              ; style|media|import|keyframes|font-face
  parent-rule       ; 父规则
  parent-stylesheet ; 所属样式表
  css-text)         ; 规则的 CSS 文本

;; CSSStyleRule
(defstruct (etaf-css-style-rule (:include etaf-css-rule))
  selector-text     ; "div.class"
  style            ; CSSStyleDeclaration
  specificity)     ; (0 1 1)

;; CSSStyleDeclaration
(defstruct etaf-css-style-declaration
  properties       ; alist ((color . ("red" . nil)))  ; (value . important)
  parent-rule)     ; 所属规则

;; 使用示例
(setq stylesheet
  (make-etaf-css-stylesheet
   :css-rules (list
     (make-etaf-css-style-rule
      :selector-text "div"
      :style (make-etaf-css-style-declaration
              :properties '((color . ("blue" . nil))))
      :specificity '(0 0 1)))))
```

**优点：**
- 完全符合 W3C 标准
- 易于扩展新规则类型
- 清晰的对象模型
- 可以直接映射到浏览器 API

**缺点：**
- 更复杂
- 需要更多代码
- 对简单用例可能过度设计

### 方案 3：混合方案（平衡）

结合两者优点：

```elisp
(:stylesheets (
  (:id 1
   :type inline|style-tag|external
   :media "all"
   :rules (
     (:selector "div.class"
      :specificity (0 1 1)
      :declarations (
        (color "red" nil)      ; (property value important)
        (font-size "14px" nil))
      :line-number 1
      :column-number 5)
     ...))
  ...)
 :rule-index (
   ;; 按选择器类型索引（性能优化）
   :by-tag ((div . (rule1 rule2)) ...)
   :by-class ((button . (rule3 rule4)) ...)
   :by-id ((main . (rule5)) ...))
 :computed-cache <hash-table>)  ; 节点 -> 计算样式
```

## 具体改进建议

### 1. 添加特异性计算

```elisp
(defun etaf-css-calculate-specificity (selector)
  "计算 CSS 选择器的特异性。
返回 (inline-flag id-count class-count type-count)。"
  (let ((id-count 0)
        (class-count 0)
        (type-count 0)
        (ast (etaf-css-selector-parse selector)))
    (etaf-css-selector-walk
     (lambda (node)
       (let ((type (plist-get node :type)))
         (cond
          ((eq type 'id) (cl-incf id-count))
          ((eq type 'class) (cl-incf class-count))
          ((eq type 'attribute) (cl-incf class-count))
          ((eq type 'pseudo) (cl-incf class-count))
          ((eq type 'tag) (cl-incf type-count)))))
     ast)
    (list 0 id-count class-count type-count)))
```

### 2. 改进层叠算法

```elisp
(defun etaf-css-compare-rules (rule1 rule2)
  "比较两个规则的优先级。
返回 t 如果 rule1 优先级更高。"
  (let ((spec1 (plist-get rule1 :specificity))
        (spec2 (plist-get rule2 :specificity))
        (important1 (plist-get rule1 :important))
        (important2 (plist-get rule2 :important))
        (source1 (plist-get rule1 :source))
        (source2 (plist-get rule2 :source)))
    (cond
     ;; !important 优先
     ((and important1 (not important2)) t)
     ((and important2 (not important1)) nil)
     ;; inline 样式优先
     ((and (eq source1 'inline) (not (eq source2 'inline))) t)
     ((and (eq source2 'inline) (not (eq source1 'inline))) nil)
     ;; 比较特异性
     (t (etaf-css-specificity> spec1 spec2)))))
```

### 3. 添加缓存机制

```elisp
(defun etaf-css-get-computed-style-cached (cssom node dom)
  "获取计算样式，使用缓存。"
  (let ((cache (plist-get cssom :computed-cache)))
    (or (gethash node cache)
        (let ((computed (etaf-css-get-computed-style cssom node dom)))
          (puthash node computed cache)
          computed))))
```

### 4. 支持媒体查询（可选）

```elisp
(defun etaf-css-match-media (media-query)
  "检查媒体查询是否匹配当前环境。
简化实现，只支持基本的 width/height。"
  (cond
   ((string= media-query "all") t)
   ((string= media-query "screen") t)
   ((string-match "min-width: \\([0-9]+\\)px" media-query)
    (let ((min-width (string-to-number (match-string 1 media-query))))
      (>= (frame-pixel-width) min-width)))
   (t t)))  ; 默认匹配
```

## 推荐方案

**建议采用方案 3（混合方案）**，理由如下：

1. **平衡性好**
   - 保持简单，不过度复杂
   - 添加关键特性（特异性、缓存）
   - 为将来扩展留有空间

2. **实用性强**
   - 满足大多数使用场景
   - 性能可接受
   - 易于理解和维护

3. **渐进式改进**
   - 可以从当前设计平滑过渡
   - 不破坏现有 API
   - 按需添加特性

## 实施步骤

### 第一阶段：向后兼容增强
1. 添加 `:specificity` 字段到规则
2. 实现特异性计算
3. 改进层叠算法使用特异性

### 第二阶段：性能优化
1. 添加 `:computed-cache` 到 CSSOM
2. 实现缓存机制
3. 添加规则索引（按选择器类型）

### 第三阶段：功能扩展（可选）
1. 支持 `!important`
2. 支持基本的媒体查询
3. 支持 CSS 变量（自定义属性）

## 总结

当前的 CSSOM 设计是一个**合理的简化实现**，适合：
- 小到中型项目
- 不需要复杂 CSS 特性
- Emacs Lisp 环境的限制

但如果需要更接近浏览器行为，建议：
1. ✓ 添加特异性计算（必需）
2. ✓ 实现完整的层叠算法（必需）
3. ✓ 添加缓存机制（推荐）
4. ○ 支持 !important（可选）
5. ○ 支持媒体查询（可选）

这样既保持简单性，又提供了足够的功能和性能。
