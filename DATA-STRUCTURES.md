# ETAF 数据结构详解

本文档详细说明 ETAF (Emacs Template and Framework) 项目中涉及的核心数据结构，以及这些数据结构在盒模型渲染和布局中的使用方法。

## 目录

1. [概述](#概述)
2. [核心数据结构](#核心数据结构)
3. [DOM 格式表示](#dom-格式表示)
4. [数据结构间的关系](#数据结构间的关系)
5. [盒模型渲染的数据流](#盒模型渲染的数据流)
6. [布局算法的实现指南](#布局算法的实现指南)
7. [实际使用示例](#实际使用示例)
8. [性能优化建议](#性能优化建议)

---

## 概述

ETAF 项目实现了类似浏览器的渲染管线，包含以下核心数据结构：

```
TML 格式 → DOM 树 → CSSOM → 渲染树 → 布局树 → 绘制
```

每个阶段都有特定的数据结构，本文档将详细介绍这些结构及其用途。

### 统一的 DOM 格式表示

**重要更新**: CSSOM、渲染树和布局树现在都支持统一的 DOM 格式表示 `(tag ((attr . val) ...) children...)`，使得所有数据结构可以用一致的方式进行遍历和操作。这提供了：

- 统一的数据访问接口
- 一致的树遍历模式
- 更好的互操作性
- 简化的工具函数开发

---

## 核心数据结构

### 1. TML 格式 (Template Markup Language)

**定义位置**: `etaf-tml.el`

**格式**:
```elisp
(tag :attr1 value1 :attr2 value2 child1 child2 ...)
```

**示例**:
```elisp
(div :class "container" :id "main"
  (h1 :class "title" "标题文本")
  (p "段落内容"))
```

**特点**:
- 使用 Emacs Lisp 原生的 plist 格式
- 属性以关键字形式表示（`:attr`）
- 子节点可以是字符串（文本节点）或嵌套的 TML 结构

**用途**: 作为输入格式，易于在 Emacs Lisp 中编写和操作

---

### 2. DOM 树 (Document Object Model)

**定义位置**: `etaf-dom.el`

**格式**:
```elisp
(tag ((attr1 . value1) (attr2 . value2)) child1 child2 ...)
```

**示例**:
```elisp
(div ((class . "container") (id . "main"))
  (h1 ((class . "title")) "标题文本")
  (p nil "段落内容"))
```

**结构说明**:
- **第一个元素**: 标签名（symbol）
- **第二个元素**: 属性列表（alist），如果没有属性则为 `nil`
- **其余元素**: 子节点（可以是 DOM 节点或字符串）

**关键属性访问**:
```elisp
(dom-tag node)              ; 获取标签名
(dom-attr node 'class)      ; 获取 class 属性
(dom-attributes node)       ; 获取所有属性
(dom-children node)         ; 获取所有子节点
(dom-texts node)            ; 获取所有文本内容
```

**用途**: 标准的文档对象模型，用于选择器匹配、样式计算和渲染树构建

---

## DOM 格式表示

**新特性**: CSSOM、渲染树和布局树现在支持统一的 DOM 格式表示。

### 为什么需要 DOM 格式？

内部数据结构使用 plist 格式（`:key value ...`）进行存储和处理，这是高效的。但 DOM 格式（`(tag ((attr . val) ...) children...)`）提供了：

1. **统一的接口**: 所有数据结构使用相同的访问模式
2. **一致的遍历**: 可以使用相同的代码遍历不同的数据结构
3. **更好的互操作性**: 与 DOM 操作函数兼容
4. **简化工具开发**: 编写通用的工具函数更容易

### DOM 格式转换函数

每个数据结构都提供了转换函数：

#### CSSOM DOM 格式

```elisp
;; Plist → DOM 格式
(etaf-css-cssom-to-dom cssom)
;; => (cssom ((inline-rules . ...) (style-rules . ...) (all-rules . ...) 
;;            (rule-index . ...) (cache . ...) (media-env . ...)))

;; DOM 格式 → Plist
(etaf-css-cssom-from-dom cssom-dom)
;; => (:inline-rules ... :style-rules ... :all-rules ...)
```

**示例**:
```elisp
(let* ((dom (etaf-tml-to-dom '(html (body (div "Text")))))
       (cssom (etaf-css-build-cssom dom))
       (cssom-dom (etaf-css-cssom-to-dom cssom)))
  ;; 访问 DOM 格式的属性
  (let ((attrs (cadr cssom-dom)))
    (cdr (assq 'all-rules attrs))))  ; 获取所有规则
```

#### 渲染树 DOM 格式

```elisp
;; Plist → DOM 格式
(etaf-render-to-dom render-tree)
;; => (render-node ((tag . div) (display . "block") 
;;                  (computed-style . ...) (node . ...))
;;     child1 child2 ...)

;; DOM 格式 → Plist
(etaf-render-from-dom render-dom)
;; => (:node ... :tag ... :computed-style ... :display ... :children ...)
```

**示例**:
```elisp
(let* ((render-tree (etaf-render-build-tree dom cssom))
       (render-dom (etaf-render-to-dom render-tree)))
  ;; 遍历 DOM 格式的渲染树
  (cl-labels ((walk (node)
                (when (eq (car node) 'render-node)
                  (let ((attrs (cadr node)))
                    (message "Tag: %s" (cdr (assq 'tag attrs))))
                  (dolist (child (cddr node))
                    (walk child)))))
    (walk render-dom)))
```

#### 布局树 DOM 格式

```elisp
;; Plist → DOM 格式
(etaf-layout-to-dom layout-tree)
;; => (layout-node ((position . ...) (box-model . ...) 
;;                  (render-node . ...) (bounds . ...) (content-box . ...))
;;     child1 child2 ...)

;; DOM 格式 → Plist
(etaf-layout-from-dom layout-dom)
;; => (:render-node ... :box-model ... :position ... :children ...)
```

**示例**:
```elisp
(let* ((layout-tree (etaf-layout-build-tree render-tree viewport))
       (layout-dom (etaf-layout-to-dom layout-tree)))
  ;; 访问盒模型信息
  (let* ((attrs (cadr layout-dom))
         (box-model (cdr (assq 'box-model attrs)))
         (position (cdr (assq 'position attrs))))
    (message "Position: (%d,%d)" 
             (plist-get position :x) 
             (plist-get position :y))))
```

### 何时使用 DOM 格式？

- **内部处理**: 使用 plist 格式（性能更好）
- **外部接口**: 可以转换为 DOM 格式（更易使用）
- **工具开发**: 使用 DOM 格式（统一接口）
- **调试/可视化**: 使用 DOM 格式（更直观）

### 性能考虑

- 转换操作很轻量（仅重组数据结构）
- 可以按需转换（不需要一直保持 DOM 格式）
- Round-trip 转换不会丢失数据
- 缓存和索引在转换中保持引用

---

### 3. CSSOM (CSS Object Model)

**定义位置**: `etaf-css.el`

**内部格式** (Plist):
```elisp
(:inline-rules (...)
 :style-rules (...)
 :all-rules (...)
 :rule-index (...)
 :cache <hash-table>
 :media-env ((type . screen) (width . 1024) ...))
```

**DOM 格式**:
```elisp
(cssom ((inline-rules . (...))
        (style-rules . (...))
        (all-rules . (...))
        (rule-index . (...))
        (cache . <hash-table>)
        (media-env . ((type . screen) (width . 1024) ...))))
```

**详细结构**:

#### 3.1 CSS 规则（Rule）

```elisp
(:selector "div.class#id"
 :declarations ((property value important) ...)
 :specificity (id-count class-count type-count)
 :source inline|style-tag|external
 :media "screen and (min-width: 768px)"  ; 可选
 :node <dom-node>)  ; 仅内联样式有此字段
```

**字段说明**:
- `:selector`: CSS 选择器字符串
- `:declarations`: 样式声明列表
  - `property`: 属性名（symbol）
  - `value`: 属性值（string）
  - `important`: 是否为 !important 声明（boolean）
- `:specificity`: 选择器特异性，格式为 `(id-count class-count type-count)`
- `:source`: 样式来源
- `:media`: 媒体查询字符串（可选）
- `:node`: 对应的 DOM 节点（仅内联样式）

**示例**:
```elisp
(:selector "div.button"
 :declarations ((color "red" nil) 
                (font-size "14px" nil)
                (font-weight "bold" t))  ; !important
 :specificity (0 1 1)
 :source style-tag
 :media nil)
```

#### 3.2 规则索引（Rule Index）

```elisp
(:by-tag #<hash-table>    ; tag -> [rule1 rule2 ...]
 :by-class #<hash-table>  ; class -> [rule1 rule2 ...]
 :by-id #<hash-table>)    ; id -> [rule1 rule2 ...]
```

**用途**: 快速查找可能匹配特定节点的候选规则

#### 3.3 计算样式缓存

```elisp
#<hash-table: node -> computed-style>
```

其中 `computed-style` 格式为:
```elisp
((property . value) ...)
```

**用途**: 缓存已计算的样式，避免重复计算

---

### 4. 渲染树 (Render Tree)

**定义位置**: `etaf-render.el`

**内部格式** (Plist):
```elisp
(:node <dom-node>
 :tag symbol
 :computed-style ((property . value) ...)
 :display "block"|"inline"|"inline-block"|...
 :children (render-node1 render-node2 ...))
```

**DOM 格式**:
```elisp
(render-node ((tag . div)
              (display . "block")
              (computed-style . ((color . "red") ...))
              (node . <dom-node>))
  child-render-node1
  child-render-node2)
```

**字段说明**:
- `:node`: 对应的 DOM 节点引用
- `:tag`: 标签名（symbol）
- `:computed-style`: 最终计算出的样式（alist）
- `:display`: display 属性值，从计算样式中提取
- `:children`: 子渲染节点列表

**示例**:
```elisp
(:node <div-node>
 :tag div
 :computed-style ((color . "red")
                  (font-size . "16px")
                  (display . "block")
                  (width . "100px")
                  (padding . "10px"))
 :display "block"
 :children ((:node <span-node>
             :tag span
             :computed-style ((color . "blue"))
             :display "inline"
             :children ())))
```

**与 DOM 的区别**:
1. 不包含不可见元素（`display: none`）
2. 不包含非渲染元素（`<head>`, `<script>`, `<style>` 等）
3. 每个节点都附带计算后的样式
4. 结构可能与 DOM 不完全相同（伪元素、匿名盒子等）

**用途**: 作为布局和绘制的输入

---

### 5. 布局树 (Layout Tree)

**定义位置**: `etaf-layout.el`

**内部格式** (Plist):
```elisp
(:render-node <render-node>
 :box-model (:content (:width w :height h)
             :padding (:top t :right r :bottom b :left l)
             :border (:top-width t :right-width r ...)
             :margin (:top t :right r :bottom b :left l))
 :position (:x x :y y)
 :bounds (:x x :y y :width w :height h)
 :content-box (:x x :y y :width w :height h)
 :children (layout-node1 layout-node2 ...))
```

**DOM 格式**:
```elisp
(layout-node ((render-node . <render-node>)
              (box-model . (:content ... :padding ... :border ... :margin ...))
              (position . (:x 0 :y 0))
              (bounds . (:x 0 :y 0 :width 200 :height 100))
              (content-box . (:x 10 :y 10 :width 180 :height 80)))
  child-layout-node1
  child-layout-node2)
```
 :children (layout-node1 layout-node2 ...))
```

**用途**: 存储每个元素的盒模型信息和最终位置

---

## 数据结构间的关系

```
┌─────────────┐
│  TML 格式   │  用户输入或模板
└──────┬──────┘
       │ etaf-tml-to-dom
       ▼
┌─────────────┐
│   DOM 树    │  文档对象模型
└──────┬──────┘
       │
       ├──────────────────┐
       │                  │
       │ 选择器匹配       │ 样式提取
       │                  │
       ▼                  ▼
┌─────────────┐    ┌──────────────┐
│  DOM 查询   │    │    CSSOM     │  CSS 对象模型
└─────────────┘    └──────┬───────┘
                          │
       ┌──────────────────┤
       │                  │
       │ 样式计算         │ 层叠 + 继承
       │                  │
       ▼                  ▼
┌─────────────────────────────┐
│        渲染树                │  可见元素 + 计算样式
└─────────────┬───────────────┘
              │
              │ 布局计算
              │
              ▼
┌─────────────────────────────┐
│        布局树                │  盒模型 + 位置信息
└─────────────┬───────────────┘
              │
              │ 绘制
              │
              ▼
┌─────────────────────────────┐
│      最终输出                │
└─────────────────────────────┘
```

---

## 盒模型渲染的数据流

### 第一步：构建 DOM 树

```elisp
;; 从 TML 创建 DOM
(setq dom (etaf-tml-to-dom
           '(div :class "container"
              (div :class "box" "内容"))))
```

### 第二步：构建 CSSOM

```elisp
;; 从 DOM 中提取样式并构建 CSSOM
(setq cssom (etaf-css-build-cssom dom))

;; CSSOM 包含:
;; - 内联样式规则 (:inline-rules)
;; - 样式表规则 (:style-rules)
;; - 规则索引 (:rule-index)
;; - 样式缓存 (:cache)
```

### 第三步：构建渲染树

```elisp
;; 结合 DOM 和 CSSOM 构建渲染树
(setq render-tree (etaf-render-build-tree dom cssom))

;; 渲染树节点包含:
;; - 原始 DOM 节点引用 (:node)
;; - 计算后的样式 (:computed-style)
;; - display 类型 (:display)
;; - 子渲染节点 (:children)
```

### 第四步：计算盒模型（待实现）

对于渲染树中的每个节点，需要计算其盒模型：

```elisp
(defun etaf-layout-compute-box-model (render-node)
  "计算渲染节点的盒模型。"
  (let* ((style (plist-get render-node :computed-style))
         ;; 提取盒模型相关属性
         (width (or (cdr (assq 'width style)) "auto"))
         (height (or (cdr (assq 'height style)) "auto"))
         (padding-top (or (cdr (assq 'padding-top style)) "0"))
         (padding-right (or (cdr (assq 'padding-right style)) "0"))
         (padding-bottom (or (cdr (assq 'padding-bottom style)) "0"))
         (padding-left (or (cdr (assq 'padding-left style)) "0"))
         (border-top-width (or (cdr (assq 'border-top-width style)) "0"))
         (border-right-width (or (cdr (assq 'border-right-width style)) "0"))
         (border-bottom-width (or (cdr (assq 'border-bottom-width style)) "0"))
         (border-left-width (or (cdr (assq 'border-left-width style)) "0"))
         (margin-top (or (cdr (assq 'margin-top style)) "0"))
         (margin-right (or (cdr (assq 'margin-right style)) "0"))
         (margin-bottom (or (cdr (assq 'margin-bottom style)) "0"))
         (margin-left (or (cdr (assq 'margin-left style)) "0")))
    
    ;; 解析 CSS 值（处理单位）
    (list :content (list :width (etaf-layout-parse-length width)
                        :height (etaf-layout-parse-length height))
          :padding (list :top (etaf-layout-parse-length padding-top)
                        :right (etaf-layout-parse-length padding-right)
                        :bottom (etaf-layout-parse-length padding-bottom)
                        :left (etaf-layout-parse-length padding-left))
          :border (list :top (etaf-layout-parse-length border-top-width)
                       :right (etaf-layout-parse-length border-right-width)
                       :bottom (etaf-layout-parse-length border-bottom-width)
                       :left (etaf-layout-parse-length border-left-width))
          :margin (list :top (etaf-layout-parse-length margin-top)
                       :right (etaf-layout-parse-length margin-right)
                       :bottom (etaf-layout-parse-length margin-bottom)
                       :left (etaf-layout-parse-length margin-left)))))
```

### 第五步：布局计算（待实现）

根据盒模型信息和布局算法（正常流、浮动、定位等）计算每个元素的最终位置：

```elisp
(defun etaf-layout-compute-position (render-node parent-box)
  "计算渲染节点的位置。
RENDER-NODE 是渲染节点。
PARENT-BOX 是父容器的盒模型和位置信息。"
  (let* ((box-model (etaf-layout-compute-box-model render-node))
         (display (plist-get render-node :display))
         (position-type (cdr (assq 'position 
                                   (plist-get render-node :computed-style)))))
    
    (cond
     ;; 块级元素
     ((string= display "block")
      (etaf-layout-block-layout render-node box-model parent-box))
     
     ;; 内联元素
     ((string= display "inline")
      (etaf-layout-inline-layout render-node box-model parent-box))
     
     ;; 内联块元素
     ((string= display "inline-block")
      (etaf-layout-inline-block-layout render-node box-model parent-box))
     
     ;; flex 容器
     ((string= display "flex")
      (etaf-layout-flex-layout render-node box-model parent-box))
     
     ;; 默认
     (t (etaf-layout-block-layout render-node box-model parent-box)))))
```

---

## 布局算法的实现指南

### 1. 盒模型尺寸计算

CSS 盒模型包含四个区域：

```
┌─────────────────────────────────────┐
│           margin（外边距）           │
│  ┌───────────────────────────────┐  │
│  │      border（边框）           │  │
│  │  ┌─────────────────────────┐  │  │
│  │  │   padding（内边距）     │  │  │
│  │  │  ┌───────────────────┐  │  │  │
│  │  │  │  content（内容）  │  │  │  │
│  │  │  │                   │  │  │  │
│  │  │  └───────────────────┘  │  │  │
│  │  └─────────────────────────┘  │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

**总宽度计算**:
```elisp
(defun etaf-layout-compute-total-width (box-model)
  "计算盒模型的总宽度。"
  (let ((padding (plist-get box-model :padding))
        (border (plist-get box-model :border))
        (margin (plist-get box-model :margin))
        (content (plist-get box-model :content)))
    (+ (plist-get content :width)
       (plist-get padding :left)
       (plist-get padding :right)
       (plist-get border :left)
       (plist-get border :right)
       (plist-get margin :left)
       (plist-get margin :right))))
```

**总高度计算**:
```elisp
(defun etaf-layout-compute-total-height (box-model)
  "计算盒模型的总高度。"
  (let ((padding (plist-get box-model :padding))
        (border (plist-get box-model :border))
        (margin (plist-get box-model :margin))
        (content (plist-get box-model :content)))
    (+ (plist-get content :height)
       (plist-get padding :top)
       (plist-get padding :bottom)
       (plist-get border :top)
       (plist-get border :bottom)
       (plist-get margin :top)
       (plist-get margin :bottom))))
```

### 2. 块级布局（Normal Flow）

块级元素垂直排列，占据父容器的全部宽度：

```elisp
(defun etaf-layout-block-layout (render-node box-model parent-box)
  "计算块级元素的布局。"
  (let* ((parent-content-width (plist-get parent-box :content-width))
         (margin (plist-get box-model :margin))
         (padding (plist-get box-model :padding))
         (border (plist-get box-model :border))
         
         ;; 计算内容宽度（默认填充父容器）
         (content-width (- parent-content-width
                          (plist-get margin :left)
                          (plist-get margin :right)
                          (plist-get padding :left)
                          (plist-get padding :right)
                          (plist-get border :left)
                          (plist-get border :right)))
         
         ;; X 位置 = 父容器 X + 左外边距
         (x (+ (plist-get parent-box :x)
               (plist-get margin :left)))
         
         ;; Y 位置 = 父容器当前 Y（累积）
         (y (plist-get parent-box :current-y))
         
         ;; 计算内容高度（如果是 auto，需要根据子元素计算）
         (content-height (etaf-layout-compute-content-height 
                         render-node box-model content-width)))
    
    ;; 返回布局信息
    (list :x x
          :y y
          :content-width content-width
          :content-height content-height
          :box-model box-model)))
```

### 3. 内联布局（Inline Layout）

内联元素水平排列，会自动换行：

```elisp
(defun etaf-layout-inline-layout (render-node box-model parent-box)
  "计算内联元素的布局。"
  (let* ((current-line-x (plist-get parent-box :line-x))
         (current-line-y (plist-get parent-box :line-y))
         (line-max-width (plist-get parent-box :line-max-width))
         
         ;; 内联元素的宽度由内容决定
         (content-width (etaf-layout-measure-inline-width render-node))
         (content-height (etaf-layout-measure-inline-height render-node))
         
         ;; 检查是否需要换行
         (need-wrap (> (+ current-line-x content-width) line-max-width))
         
         ;; 计算位置
         (x (if need-wrap
                (plist-get parent-box :line-start-x)
              current-line-x))
         (y (if need-wrap
                (+ current-line-y (plist-get parent-box :line-height))
              current-line-y)))
    
    (list :x x
          :y y
          :content-width content-width
          :content-height content-height
          :box-model box-model)))
```

### 4. Flexbox 布局（未来扩展）

```elisp
(defun etaf-layout-flex-layout (render-node box-model parent-box)
  "计算 flex 容器的布局。"
  (let* ((style (plist-get render-node :computed-style))
         (flex-direction (or (cdr (assq 'flex-direction style)) "row"))
         (justify-content (or (cdr (assq 'justify-content style)) "flex-start"))
         (align-items (or (cdr (assq 'align-items style)) "stretch"))
         (children (plist-get render-node :children)))
    
    ;; 根据 flex-direction 确定主轴和交叉轴
    ;; 计算子元素的大小和位置
    ;; 应用 justify-content 和 align-items
    ;; ...
    ))
```

### 5. 使用渲染树进行布局

```elisp
(defun etaf-layout-build-layout-tree (render-tree viewport)
  "从渲染树构建布局树。
RENDER-TREE 是渲染树根节点。
VIEWPORT 是视口大小 (:width w :height h)。"
  (let ((root-box (list :x 0
                       :y 0
                       :content-width (plist-get viewport :width)
                       :content-height (plist-get viewport :height)
                       :current-y 0)))
    (etaf-layout-compute-tree render-tree root-box)))

(defun etaf-layout-compute-tree (render-node parent-box)
  "递归计算布局树。"
  (when render-node
    (let* ((box-model (etaf-layout-compute-box-model render-node))
           (position (etaf-layout-compute-position render-node parent-box))
           (layout-node (list :render-node render-node
                             :box-model box-model
                             :position position
                             :children '())))
      
      ;; 递归处理子节点
      (let ((child-box (list :x (plist-get position :x)
                            :y (plist-get position :y)
                            :content-width (plist-get position :content-width)
                            :content-height (plist-get position :content-height)
                            :current-y (plist-get position :y)))
            (children '()))
        (dolist (child (plist-get render-node :children))
          (when-let ((child-layout (etaf-layout-compute-tree child child-box)))
            (push child-layout children)))
        
        (plist-put layout-node :children (nreverse children)))
      
      layout-node)))
```

---

## 实际使用示例

### 示例 1: 完整的渲染流程

```elisp
;; 1. 创建 DOM
(setq my-dom
      (etaf-tml-to-dom
       '(html
          (head
            (style "
              .container { width: 800px; padding: 20px; }
              .box { 
                width: 200px; 
                height: 100px; 
                margin: 10px; 
                padding: 15px;
                border: 2px solid black;
              }"))
          (body
            (div :class "container"
              (div :class "box" "Box 1")
              (div :class "box" "Box 2"))))))

;; 2. 构建 CSSOM
(setq my-cssom (etaf-css-build-cssom my-dom))

;; 3. 构建渲染树
(setq my-render-tree (etaf-render-build-tree my-dom my-cssom))

;; 4. 查看渲染树结构
(message "渲染树:\n%s" (etaf-render-to-string my-render-tree))

;; 5. 遍历渲染树查询样式
(etaf-render-walk my-render-tree
  (lambda (node)
    (when (eq (plist-get node :tag) 'div)
      (let ((width (etaf-render-get-style node 'width))
            (padding (etaf-render-get-style node 'padding)))
        (message "Div: width=%s, padding=%s" width padding)))))

;; 6. 计算布局（伪代码，待实现）
;; (setq my-layout-tree 
;;       (etaf-layout-build-layout-tree 
;;        my-render-tree 
;;        '(:width 1024 :height 768)))
```

### 示例 2: 查询特定节点的样式

```elisp
;; 查找所有 class 为 "box" 的渲染节点
(let ((box-nodes '()))
  (etaf-render-walk my-render-tree
    (lambda (node)
      (let* ((dom-node (plist-get node :node))
             (class (dom-attr dom-node 'class)))
        (when (and class (string-match-p "box" class))
          (push node box-nodes)))))
  
  ;; 输出每个 box 的样式信息
  (dolist (box (nreverse box-nodes))
    (message "Box styles:")
    (message "  width: %s" (etaf-render-get-style box 'width))
    (message "  height: %s" (etaf-render-get-style box 'height))
    (message "  padding: %s" (etaf-render-get-style box 'padding))
    (message "  margin: %s" (etaf-render-get-style box 'margin))
    (message "  border-width: %s" 
             (etaf-render-get-style box 'border-width))))
```

### 示例 3: 响应式设计（媒体查询）

```elisp
;; 创建带媒体查询的 DOM
(setq responsive-dom
      (etaf-tml-to-dom
       '(html
          (head
            (style "
              .box { width: 100%; padding: 10px; }
              @media (min-width: 768px) {
                .box { width: 50%; padding: 20px; }
              }
              @media (min-width: 1024px) {
                .box { width: 33.333%; padding: 30px; }
              }"))
          (body
            (div :class "box" "Responsive Box")))))

;; 在不同视口宽度下构建 CSSOM
(let ((mobile-cssom (etaf-css-build-cssom 
                     responsive-dom 
                     '((type . screen) (width . 375))))
      (tablet-cssom (etaf-css-build-cssom 
                     responsive-dom 
                     '((type . screen) (width . 768))))
      (desktop-cssom (etaf-css-build-cssom 
                      responsive-dom 
                      '((type . screen) (width . 1024)))))
  
  ;; 查看在不同视口下的样式
  (dolist (env-cssom (list (cons "Mobile" mobile-cssom)
                          (cons "Tablet" tablet-cssom)
                          (cons "Desktop" desktop-cssom)))
    (let* ((label (car env-cssom))
           (cssom (cdr env-cssom))
           (render-tree (etaf-render-build-tree responsive-dom cssom))
           (box-node (car (etaf-render-find-by-tag render-tree 'div))))
      (message "%s - width: %s, padding: %s"
               label
               (etaf-render-get-style box-node 'width)
               (etaf-render-get-style box-node 'padding)))))
```

### 示例 4: 计算盒模型尺寸（实现指南）

```elisp
(defun etaf-layout-example-box-dimensions (render-node)
  "计算并显示盒模型各部分的尺寸。"
  (let* ((style (plist-get render-node :computed-style))
         
         ;; 提取样式值
         (content-width (etaf-layout-parse-length 
                        (or (cdr (assq 'width style)) "auto")))
         (padding-left (etaf-layout-parse-length 
                       (or (cdr (assq 'padding-left style)) "0")))
         (padding-right (etaf-layout-parse-length 
                        (or (cdr (assq 'padding-right style)) "0")))
         (border-left (etaf-layout-parse-length 
                      (or (cdr (assq 'border-left-width style)) "0")))
         (border-right (etaf-layout-parse-length 
                       (or (cdr (assq 'border-right-width style)) "0")))
         (margin-left (etaf-layout-parse-length 
                      (or (cdr (assq 'margin-left style)) "0")))
         (margin-right (etaf-layout-parse-length 
                       (or (cdr (assq 'margin-right style)) "0")))
         
         ;; 计算总宽度
         (total-width (+ content-width
                        padding-left padding-right
                        border-left border-right
                        margin-left margin-right)))
    
    (message "盒模型尺寸:")
    (message "  内容宽度: %d" content-width)
    (message "  + 左内边距: %d" padding-left)
    (message "  + 右内边距: %d" padding-right)
    (message "  + 左边框: %d" border-left)
    (message "  + 右边框: %d" border-right)
    (message "  + 左外边距: %d" margin-left)
    (message "  + 右外边距: %d" margin-right)
    (message "  = 总宽度: %d" total-width)))

;; 辅助函数：解析 CSS 长度值
(defun etaf-layout-parse-length (value)
  "解析 CSS 长度值，返回像素数。
支持: px, %, em, rem, auto"
  (cond
   ((string= value "auto") 0)
   ((string= value "0") 0)
   ((string-match "\\([0-9.]+\\)px$" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\([0-9.]+\\)%$" value)
    ;; 百分比需要根据父元素计算，这里简化处理
    (string-to-number (match-string 1 value)))
   ((string-match "\\([0-9.]+\\)em$" value)
    ;; em 需要根据字体大小计算，这里假设 1em = 16px
    (* (string-to-number (match-string 1 value)) 16))
   (t 0)))
```

---

## 性能优化建议

### 1. 使用缓存

CSSOM 内置了样式缓存机制，确保重复查询时使用缓存：

```elisp
;; 第一次查询会计算并缓存
(setq style1 (etaf-css-get-computed-style cssom node dom))

;; 第二次查询直接从缓存获取（快 10-100 倍）
(setq style2 (etaf-css-get-computed-style cssom node dom))

;; 如果 DOM 或样式发生变化，需要清除缓存
(etaf-css-clear-cache cssom)
```

### 2. 利用规则索引

CSSOM 会自动构建规则索引，按标签、类、ID 分类：

```elisp
;; 索引自动在 etaf-css-build-cssom 时创建
;; 查询时会自动使用索引，只检查可能匹配的规则

;; 对于有 class 或 id 的节点，性能提升明显
(setq node-with-class (dom-by-class dom "button"))
(setq styles (etaf-css-get-computed-style cssom node-with-class dom))
;; 上面的查询只会检查包含 ".button" 的规则，而不是所有规则
```

### 3. 批量处理

对于大量节点，批量处理比逐个处理更高效：

```elisp
;; 不推荐：逐个查询
(dolist (node nodes)
  (setq style (etaf-css-get-computed-style cssom node dom))
  (process-style style))

;; 推荐：批量查询（利用缓存）
(let ((styles '()))
  (dolist (node nodes)
    (push (etaf-css-get-computed-style cssom node dom) styles))
  (dolist (style (nreverse styles))
    (process-style style)))
```

### 4. 避免不必要的重建

```elisp
;; 不好：频繁重建 CSSOM
(dotimes (i 100)
  (setq cssom (etaf-css-build-cssom dom))
  (do-something cssom))

;; 好：重用 CSSOM
(setq cssom (etaf-css-build-cssom dom))
(dotimes (i 100)
  (do-something cssom))
```

### 5. 延迟计算

对于不可见的元素（`display: none`），渲染树会自动跳过，避免不必要的样式计算：

```elisp
;; 渲染树自动排除不可见元素
(setq render-tree (etaf-render-build-tree dom cssom))

;; 只包含可见元素，布局时更高效
(etaf-render-walk render-tree
  (lambda (node)
    ;; 这里只会处理可见节点
    (compute-layout node)))
```

---

## 总结

本文档详细介绍了 ETAF 项目中的核心数据结构及其在盒模型渲染和布局中的应用：

1. **TML 格式**: 用户友好的输入格式
2. **DOM 树**: 标准的文档对象模型
3. **CSSOM**: CSS 对象模型，包含样式规则、索引和缓存
4. **渲染树**: 可见元素及其计算样式
5. **布局树**: 盒模型和位置信息（待实现）

### 关键要点

- 渲染树是布局的主要输入，包含所有必要的样式信息
- 盒模型包含四个区域：content、padding、border、margin
- 布局算法需要根据 display 类型选择不同的计算方式
- 性能优化依赖缓存、索引和批量处理

### 下一步

1. 实现 `etaf-layout.el` 模块，包含：
   - 盒模型计算函数
   - 块级布局算法
   - 内联布局算法
   - 定位算法（static、relative、absolute、fixed）
   - Flexbox 布局（可选）

2. 实现 `etaf-paint.el` 模块，包含：
   - 绘制顺序管理
   - 背景绘制
   - 边框绘制
   - 文本绘制
   - 图层合成（可选）

3. 测试和文档：
   - 添加布局测试用例
   - 创建更多实际示例
   - 性能基准测试
