# 盒模型与布局系统实现总结

## 设计决策：使用 plist 而非 defclass

### 背景分析

在实现盒模型和布局系统时，我们面临两种设计方案的选择：

1. **使用 EIEIO defclass**（现有 `etaf-box.el` 的方式）
   - 优点：面向对象，支持继承
   - 缺点：
     - 代码冗长（etaf-box.el 达 40KB）
     - 学习曲线陡峭
     - 与项目其他部分（etaf-render.el, etaf-css.el）风格不一致
     - 性能开销较大
     - 调试困难

2. **使用 plist 数据结构**（新的 `etaf-layout.el` 方式）
   - 优点：
     - 简单直观
     - 与现有代码风格一致
     - 易于调试和查看
     - 性能更好
     - 代码更简洁
   - 缺点：
     - 没有类型检查（可通过文档和测试弥补）
     - 没有继承机制（但布局对象通常不需要深层继承）

### 最终决策

**选择 plist 方案**，理由如下：

1. **一致性原则**：etaf-render.el 和 etaf-css.el 都使用 plist 结构，新的布局模块应保持一致
2. **简洁性原则**：浏览器布局算法本质上是数据转换，不需要复杂的 OOP 特性
3. **可维护性**：plist 结构更容易理解和维护，降低了项目的学习成本
4. **文档驱动**：BOX-MODEL-LAYOUT.md 中的示例都使用 plist 风格

## 实现架构

### 数据结构设计

#### 盒模型 (Box Model)

```elisp
(:box-sizing "content-box"
 :content (:width 200 :height 100)
 :padding (:top 10 :right 15 :bottom 10 :left 15)
 :border (:top-width 2 :right-width 2 :bottom-width 2 :left-width 2
          :top-color "black" :right-color "black" 
          :bottom-color "black" :left-color "black")
 :margin (:top 5 :right 10 :bottom 5 :left 10))
```

**设计要点**：
- 分离的 top/right/bottom/left 属性，便于精确控制
- 明确区分 width（边框宽度）和 color（边框颜色）
- 支持 box-sizing 属性

#### 布局节点 (Layout Node)

```elisp
(:render-node <render-node>
 :box-model <box-model>
 :position (:x 100 :y 50)
 :bounds (:x 90 :y 40 :width 240 :height 160)
 :content-box (:x 115 :y 62 :width 200 :height 100)
 :children (<layout-node> ...))
```

**设计要点**：
- 保留对原始渲染节点的引用
- 分离 position（元素位置）和 bounds（包含 margin 的边界）
- content-box 用于子元素定位
- 递归的子节点结构

### 核心算法

#### 1. 盒模型计算 (`etaf-layout-compute-box-model`)

```
输入：render-node + parent-context
处理：
  1. 从 computed-style 提取 CSS 属性
  2. 解析长度值（px、%、em、auto）
  3. 计算 content width/height
  4. 处理 auto 值
输出：box-model plist
```

**关键特性**：
- 支持百分比相对于父容器计算
- 正确处理 auto 值
- 默认值处理（如 padding/margin 默认为 0）

#### 2. 块级格式化上下文 (`etaf-layout-block-formatting-context`)

```
输入：render-node + parent-context
处理：
  1. 计算盒模型
  2. 计算元素位置（x, y）
  3. 递归布局子元素
  4. 累积子元素高度
  5. 处理 height: auto
输出：layout-node
```

**关键特性**：
- 垂直排列的块级元素
- 自动高度根据子元素计算
- 正确处理嵌套的 padding/border/margin

#### 3. 布局树构建 (`etaf-layout-build-tree`)

```
输入：render-tree + viewport
处理：
  创建根上下文 → 递归布局节点
输出：layout-tree
```

### 辅助函数系统

提供了完整的辅助函数集：

```elisp
;; CSS 值解析
etaf-layout-parse-length          ; 解析 px/%, em, auto
etaf-layout-get-style-value        ; 从样式中获取值

;; 盒模型查询
etaf-box-model-content-width       ; 内容宽度
etaf-box-model-padding-width       ; 左右内边距之和
etaf-box-model-border-width        ; 左右边框之和
etaf-box-model-margin-width        ; 左右外边距之和
etaf-box-model-total-width         ; 总宽度（包含 margin）

;; 布局树遍历
etaf-layout-walk                   ; 深度优先遍历
etaf-layout-to-string              ; 转换为可读字符串
```

## 实现细节

### 1. CSS 值解析

支持多种 CSS 值格式：

- **像素值**：`"100px"` → `100`
- **百分比**：`"50%"` → `parent-width * 0.5`
- **em 单位**：`"2em"` → `32` (假设 1em = 16px)
- **auto**：`"auto"` → `'auto` (符号)
- **数字**：`0` → `0`

### 2. width: auto 的处理

```elisp
(if (eq width 'auto)
    ;; 填充父容器：parent-width - padding - border - margin
    (max 0 (- parent-width
             padding-left padding-right
             border-left border-right
             margin-left margin-right))
  ;; 使用指定值
  width)
```

### 3. height: auto 的处理

```elisp
;; 初始设置为 0
(content-height 0)

;; 布局所有子元素后累积高度
(dolist (child children)
  (cl-incf accumulated-height 
          (etaf-box-model-total-height child-box-model)))

;; 更新盒模型高度
(when (= content-height 0)
  (plist-put content :height accumulated-height))
```

### 4. 位置计算

```elisp
;; 元素的绝对位置 = 父容器偏移 + 自身 margin-left
(x (+ parent-x-offset margin-left))

;; 内容区域位置 = 元素位置 + border + padding
(content-x (+ x border-left padding-left))
```

## 测试覆盖

实现了 15 个全面的测试用例：

### 基础测试
- `etaf-layout-test-box-model-create` - 盒模型创建
- `etaf-layout-test-box-model-dimensions` - 尺寸计算

### CSS 解析测试
- `etaf-layout-test-parse-length-px` - 像素值解析
- `etaf-layout-test-parse-length-percent` - 百分比解析
- `etaf-layout-test-parse-length-auto` - auto 解析
- `etaf-layout-test-parse-length-em` - em 单位解析

### 布局测试
- `etaf-layout-test-simple-block-layout` - 简单块级布局
- `etaf-layout-test-nested-blocks` - 嵌套布局
- `etaf-layout-test-width-auto` - auto 宽度计算
- `etaf-layout-test-height-auto` - auto 高度计算
- `etaf-layout-test-padding-border-margin` - 盒模型属性
- `etaf-layout-test-position-calculation` - 位置计算

### 工具测试
- `etaf-layout-test-layout-walk` - 遍历功能
- `etaf-layout-test-layout-to-string` - 字符串转换

### 集成测试
- `etaf-layout-test-full-pipeline` - 完整流程

**测试结果**：全部 15 个测试通过 ✅

## 使用示例

### 示例 1：简单布局

```elisp
(require 'etaf)

(let* ((dom (etaf-tml-to-dom
             '(html
               (head
                (style ".box { width: 200px; height: 100px; padding-left: 10px; }"))
               (body
                (div :class "box" "Content")))))
       (cssom (etaf-css-build-cssom dom))
       (render-tree (etaf-render-build-tree dom cssom))
       (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
  
  ;; 查看布局结果
  (message "%s" (etaf-layout-to-string layout-tree)))
```

### 示例 2：复杂嵌套布局

```elisp
(let* ((dom (etaf-tml-to-dom
             '(html
               (head
                (style "
                  .container { width: 800px; padding-left: 20px; padding-right: 20px; }
                  .box { width: 200px; height: 100px; margin-left: 10px; margin-bottom: 10px; }
                "))
               (body
                (div :class "container"
                  (div :class "box" "Box 1")
                  (div :class "box" "Box 2")
                  (div :class "box" "Box 3"))))))
       (cssom (etaf-css-build-cssom dom))
       (render-tree (etaf-render-build-tree dom cssom))
       (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
  
  ;; 遍历并打印每个元素的布局信息
  (etaf-layout-walk layout-tree
    (lambda (node)
      (let ((pos (plist-get node :position))
            (box (plist-get node :box-model)))
        (message "Position: (%d,%d) Size: %dx%d"
                 (plist-get pos :x)
                 (plist-get pos :y)
                 (etaf-box-model-content-width box)
                 (etaf-box-model-content-height box))))))
```

## 性能特点

### 时间复杂度

- **布局树构建**：O(n)，n 为节点数
- **布局遍历**：O(n)
- **盒模型查询**：O(1)

### 空间复杂度

- **布局树**：O(n)，每个渲染节点对应一个布局节点
- **盒模型**：O(1)，固定大小的 plist

### 优化策略

1. **惰性计算**：高度 auto 的元素延迟计算到子元素布局完成
2. **直接访问**：使用 plist-get 而非函数调用
3. **避免重复计算**：盒模型计算一次后缓存在布局节点中

## 与现有系统的集成

### 1. 与 etaf-render.el 集成

```
渲染树 (render-tree) → 布局树 (layout-tree)
```

布局树直接使用渲染树的节点，通过 `:render-node` 引用。

### 2. 与 etaf-css.el 集成

```
计算样式 (computed-style) → 盒模型 (box-model)
```

从 `computed-style` alist 中提取 CSS 属性值来计算盒模型。

### 3. 数据流

```
TML → DOM → CSSOM → 渲染树 → 布局树
```

完整的管线处理，每个阶段输出作为下一阶段的输入。

## 未来扩展

### 短期计划

1. **外边距折叠 (Margin Collapsing)**
   - 实现相邻块级元素的垂直外边距折叠
   - 处理父子元素的外边距折叠

2. **定位方案 (Positioning)**
   - relative：相对定位
   - absolute：绝对定位
   - fixed：固定定位
   - sticky：粘性定位

3. **内联布局 (Inline Layout)**
   - 行框 (line box) 算法
   - 文本换行
   - 基线对齐

### 长期计划

1. **Flexbox 布局**
   - flex-direction
   - justify-content
   - align-items
   - flex-wrap

2. **Grid 布局**
   - 网格容器
   - 网格项目
   - 自动填充

3. **性能优化**
   - 增量布局（只重新计算改变的部分）
   - 并行布局（利用 Emacs 的并发特性）
   - 缓存优化

## 总结

本次实现成功完成了以下目标：

1. ✅ **设计决策明确**：选择 plist 方案，保持代码简洁和一致性
2. ✅ **功能完整**：实现了核心的盒模型和块级布局算法
3. ✅ **测试充分**：15 个测试全部通过，覆盖各种场景
4. ✅ **文档完善**：提供了详细的实现文档和使用示例
5. ✅ **易于扩展**：清晰的架构便于未来添加新功能

通过这次实现，ETAF 项目建立了一个坚实的布局系统基础，为后续的增强功能（如 Flexbox、定位等）打下了良好的基础。plist 方案的选择被证明是正确的，它使代码更加简洁、易懂和易维护。
