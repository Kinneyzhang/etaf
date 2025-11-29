# ETAF CSS 模块化架构

ETAF CSS 系统已经重构为模块化架构，每个模块专注于特定的功能。

## 模块概览

### 核心模块

#### etaf-css.el
主入口点，整合所有子模块。提供：
- CSSOM 构建 (`etaf-css-build-cssom`)
- 计算样式查询 (`etaf-css-get-computed-style`)
- 样式提取 (`etaf-css-extract-inline-styles`, `etaf-css-extract-style-tags`)

#### etaf-css-parser.el
CSS 解析器，支持：
- CSS 声明解析（支持 !important）
- CSS 规则解析
- CSS 样式表解析

#### etaf-css-specificity.el
选择器特异性计算：
- `etaf-css-calculate-specificity` - 计算选择器特异性
- `etaf-css-specificity>` - 比较特异性
- `etaf-css-specificity=` - 检查特异性相等

#### etaf-css-cascade.el
CSS 层叠算法：
- 支持 !important 声明
- 内联样式优先级
- 选择器特异性比较
- 文档顺序处理

#### etaf-css-inheritance.el
CSS 属性继承：
- `etaf-css-property-inherits-p` - 检查属性是否可继承
- `etaf-css-apply-inheritance` - 应用继承规则
- 支持常见可继承属性（color, font-family, font-size 等）

#### etaf-css-cache.el
计算样式缓存：
- `etaf-css-cache-create` - 创建缓存
- `etaf-css-cache-get` - 获取缓存值
- `etaf-css-cache-set` - 设置缓存值
- `etaf-css-cache-clear` - 清空缓存

#### etaf-css-index.el
规则索引（性能优化）：
- 按标签、类、ID 索引规则
- 快速查找候选规则
- 减少选择器匹配次数

#### etaf-css-media.el
媒体查询支持：
- `etaf-css-media-match-p` - 检查媒体查询是否匹配
- `etaf-css-media-parse-feature` - 解析媒体特性
- `etaf-css-media-evaluate-feature` - 评估媒体特性
- 支持常见媒体类型（screen, print, all）
- 支持基本媒体特性（width, height, min-width, max-width）
- 可配置的媒体环境

## 新功能

### 1. !important 支持

```elisp
;; 解析带 !important 的声明
(etaf-css-parse-declarations "color: red !important; font-size: 14px;")
;; => ((color "red" t) (font-size "14px" nil))

;; 在层叠中，!important 声明优先级最高
(setq dom (etaf-etml-to-dom
           '(html
             (head (style "div { color: blue !important; }"))
             (body (div :style "color: red;" "Text")))))
(setq cssom (etaf-css-build-cssom dom))
(setq computed (etaf-css-get-computed-style cssom (dom-by-tag dom 'div) dom))
;; => (color . "blue")  ; !important 战胜内联样式
```

### 2. 增强的层叠算法

层叠顺序（从低到高）：
1. 普通声明 - 按特异性和文档顺序
2. !important 声明 - 按特异性和文档顺序
3. 内联样式普通声明
4. 内联样式 !important 声明

```elisp
;; 示例：多个规则的层叠
(setq dom (etaf-etml-to-dom
           '(html
             (head (style "
                 div { color: green; }
                 .box { color: blue; }
                 #main { color: red; }
               "))
             (body (div :id "main" :class "box" "Text")))))
(setq cssom (etaf-css-build-cssom dom))
(setq computed (etaf-css-get-computed-style cssom (dom-by-id dom "main") dom))
;; => (color . "red")  ; ID 选择器特异性最高
```

### 3. 属性继承

某些 CSS 属性（如 color, font-family）会自动从父元素继承：

```elisp
(setq dom (etaf-etml-to-dom
           '(div :style "color: red; font-size: 14px;"
             (p "继承的文本"))))
(setq cssom (etaf-css-build-cssom dom))
(setq computed (etaf-css-get-computed-style cssom (dom-by-tag dom 'p) dom))
;; => 包含 (color . "red") 和 (font-size . "14px")
```

支持的可继承属性：
- color
- font-family, font-size, font-style, font-weight
- line-height, letter-spacing, word-spacing
- text-align, text-indent, text-transform
- list-style, visibility, cursor
- 等等

### 4. 计算样式缓存

第二次查询相同节点的样式时，直接从缓存获取：

```elisp
(setq cssom (etaf-css-build-cssom dom))
;; 第一次计算
(etaf-css-get-computed-style cssom node dom)  ; 计算并缓存
;; 第二次查询
(etaf-css-get-computed-style cssom node dom)  ; 从缓存获取，快速

;; DOM 或样式改变后，清除缓存
(etaf-css-clear-cache cssom)
```

### 5. 规则索引

自动构建规则索引，按标签、类、ID 组织规则：

```elisp
(setq cssom (etaf-css-build-cssom dom))
(plist-get cssom :rule-index)
;; => (:by-tag <hash-table> :by-class <hash-table> :by-id <hash-table>)

;; 查询时只检查可能匹配的候选规则，大幅提升性能
```

### 6. 媒体查询支持

支持 @media 规则和媒体查询评估：

```elisp
;; 创建带媒体查询的 DOM
(setq dom (etaf-etml-to-dom
           '(html
             (head (style "
               .header { padding: 10px; }
               @media screen and (min-width: 768px) {
                 .header { padding: 20px; }
               }
             ")))))

;; 在桌面环境下构建 CSSOM
(setq desktop-cssom 
      (etaf-css-build-cssom dom '((type . screen) (width . 1024))))

;; 在移动环境下构建 CSSOM
(setq mobile-cssom
      (etaf-css-build-cssom dom '((type . screen) (width . 375))))

;; 媒体查询会自动应用，不匹配的规则被过滤
```

## 数据格式

### CSS 声明格式（新）

```elisp
;; 新格式：(property value important)
'((color "red" nil)
  (font-size "14px" t))  ; !important
```

### CSS 规则格式

```elisp
;; 规则 plist
'(:selector "div.button"
  :declarations ((color "red" nil) (background "blue" t))
  :specificity (0 1 1)  ; (id-count class-count type-count)
  :source style-tag     ; 或 inline
  :node <node-ref>)     ; 仅内联样式有
```

### CSSOM 结构

```elisp
;; CSSOM plist
'(:inline-rules (...)    ; 内联样式规则
  :style-rules (...)     ; 样式表规则
  :all-rules (...)       ; 所有规则（按顺序）
  :rule-index (...)      ; 规则索引
  :cache <hash-table>)   ; 计算样式缓存
```

### 计算样式格式（向后兼容）

```elisp
;; 输出格式保持向后兼容
'((color . "red")
  (font-size . "14px"))
```

## 性能优化

### 优化前
- 查询复杂度：O(n * m)，n = 规则数，m = 节点数
- 无缓存，重复计算

### 优化后
- 索引查询：O(k * log k)，k << n（候选规则数远小于总规则数）
- 缓存命中：O(1)
- 预期性能提升：10-100x

## 向后兼容

为保持向后兼容：
1. `etaf-css-get-computed-style` 返回格式保持 `((property . value) ...)`
2. 提供 `etaf-css-parse-declarations-compat` 用于旧格式
3. 所有公共 API 保持不变

## 测试

每个模块都有对应的测试文件：
- `tests/etaf-css-tests.el` - 主测试套件
- `tests/etaf-css-important-tests.el` - !important 和层叠测试
- `tests/etaf-css-cache-tests.el` - 缓存测试
- `tests/etaf-css-index-tests.el` - 索引测试
- `tests/etaf-css-inheritance-tests.el` - 继承测试
- `tests/etaf-css-media-tests.el` - 媒体查询测试

## 未实现功能

根据需求，以下功能未实现：
- **实时更新** (Real-time updates) - 明确排除
- **@规则** (@rules) - 部分支持（@media 已实现）

## 参考文档

- CSSOM-COMPARISON.md - 浏览器 CSSOM 对比
- CSSOM-DESIGN.md - 设计文档
- ETAF-CSS-README.md - 原始文档
