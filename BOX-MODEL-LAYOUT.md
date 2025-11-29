# 盒模型与布局实现指南

本文档专注于 ETAF 项目中盒模型渲染和布局算法的具体实现方法。

## 目录

1. [CSS 盒模型概述](#css-盒模型概述)
2. [盒模型数据结构设计](#盒模型数据结构设计)
3. [布局算法详解](#布局算法详解)
4. [定位方案](#定位方案)
5. [文本和内联布局](#文本和内联布局)
6. [Flexbox 布局](#flexbox-布局)
7. [实现示例代码](#实现示例代码)

---

## CSS 盒模型概述

### 盒模型的四个区域

每个渲染元素都由四个嵌套的矩形区域组成：

```
┌─────────────────────────────────────────────────────┐
│  Margin Area (外边距区域，透明)                     │
│  ┌───────────────────────────────────────────────┐  │
│  │ Border Area (边框区域)                        │  │
│  │ ┌─────────────────────────────────────────┐  │  │
│  │ │ Padding Area (内边距区域，背景延伸)    │  │  │
│  │ │ ┌───────────────────────────────────┐  │  │  │
│  │ │ │ Content Area (内容区域)          │  │  │  │
│  │ │ │                                   │  │  │  │
│  │ │ │  width × height                   │  │  │  │
│  │ │ │                                   │  │  │  │
│  │ │ └───────────────────────────────────┘  │  │  │
│  │ └─────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
```

### box-sizing 属性

- **content-box** (默认): width/height 只包含内容区域
- **border-box**: width/height 包含内容 + padding + border

```
content-box:
  实际占用宽度 = width + padding-left + padding-right + border-left + border-right

border-box:
  实际占用宽度 = width (已包含 padding 和 border)
  内容宽度 = width - padding-left - padding-right - border-left - border-right
```

---

## 盒模型数据结构设计

### 建议的盒模型结构

```elisp
(:box-model
  (:box-sizing "content-box"|"border-box"
   
   ;; 内容区域 (Content Box)
   :content (:width <number>    ; 内容宽度（像素）
             :height <number>)  ; 内容高度（像素）
   
   ;; 内边距 (Padding)
   :padding (:top <number>
             :right <number>
             :bottom <number>
             :left <number>)
   
   ;; 边框 (Border)
   :border (:top-width <number>
            :right-width <number>
            :bottom-width <number>
            :left-width <number>
            :top-style "solid"|"dashed"|...
            :right-style "solid"|"dashed"|...
            :bottom-style "solid"|"dashed"|...
            :left-style "solid"|"dashed"|...
            :top-color "color"
            :right-color "color"
            :bottom-color "color"
            :left-color "color")
   
   ;; 外边距 (Margin)
   :margin (:top <number>
            :right <number>
            :bottom <number>
            :left <number>)))
```

### 布局节点完整结构

```elisp
(:layout-node
  ;; 关联的渲染节点
  :render-node <render-node>
  
  ;; 盒模型
  :box-model <box-model>
  
  ;; 在父容器中的位置（左上角坐标）
  :position (:x <number>
             :y <number>)
  
  ;; 边界框（用于绘制和命中测试）
  :bounds (:x <number>          ; 包含 margin 的左上角
           :y <number>
           :width <number>       ; 包含 margin 的总宽度
           :height <number>)     ; 包含 margin 的总高度
  
  ;; 内容框（用于子元素定位）
  :content-box (:x <number>     ; 内容区域的左上角
                :y <number>
                :width <number>  ; 内容区域宽度
                :height <number>) ; 内容区域高度
  
  ;; 子布局节点
  :children (layout-node1 layout-node2 ...))
```

### 辅助函数

```elisp
(defun etaf-box-model-create ()
  "创建空的盒模型结构。"
  (list :box-sizing "content-box"
        :content (list :width 0 :height 0)
        :padding (list :top 0 :right 0 :bottom 0 :left 0)
        :border (list :top-width 0 :right-width 0 :bottom-width 0 :left-width 0)
        :margin (list :top 0 :right 0 :bottom 0 :left 0)))

(defun etaf-box-model-content-width (box-model)
  "获取盒模型的内容宽度。"
  (plist-get (plist-get box-model :content) :width))

(defun etaf-box-model-content-height (box-model)
  "获取盒模型的内容高度。"
  (plist-get (plist-get box-model :content) :height))

(defun etaf-box-model-padding-width (box-model)
  "获取盒模型的左右内边距之和。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :left)
       (plist-get padding :right))))

(defun etaf-box-model-padding-height (box-model)
  "获取盒模型的上下内边距之和。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :top)
       (plist-get padding :bottom))))

(defun etaf-box-model-border-width (box-model)
  "获取盒模型的左右边框之和。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :left-width)
       (plist-get border :right-width))))

(defun etaf-box-model-border-height (box-model)
  "获取盒模型的上下边框之和。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :top-width)
       (plist-get border :bottom-width))))

(defun etaf-box-model-margin-width (box-model)
  "获取盒模型的左右外边距之和。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :left)
       (plist-get margin :right))))

(defun etaf-box-model-margin-height (box-model)
  "获取盒模型的上下外边距之和。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :top)
       (plist-get margin :bottom))))

(defun etaf-box-model-total-width (box-model)
  "计算盒模型的总宽度（包含 margin）。"
  (+ (etaf-box-model-content-width box-model)
     (etaf-box-model-padding-width box-model)
     (etaf-box-model-border-width box-model)
     (etaf-box-model-margin-width box-model)))

(defun etaf-box-model-total-height (box-model)
  "计算盒模型的总高度（包含 margin）。"
  (+ (etaf-box-model-content-height box-model)
     (etaf-box-model-padding-height box-model)
     (etaf-box-model-border-height box-model)
     (etaf-box-model-margin-height box-model)))
```

---

## 布局算法详解

### 1. 正常流（Normal Flow）布局

正常流是最基本的布局模式，包括块级格式化上下文（BFC）和内联格式化上下文（IFC）。

#### 块级格式化上下文（Block Formatting Context）

块级元素在 BFC 中垂直排列：

```elisp
(defun etaf-layout-block-formatting-context (render-node parent-context)
  "在块级格式化上下文中布局节点。
RENDER-NODE 是要布局的渲染节点。
PARENT-CONTEXT 包含父容器的上下文信息：
  :content-width  - 可用内容宽度
  :content-height - 可用内容高度
  :current-y      - 当前 Y 坐标（累积）
  :x-offset       - X 偏移量"
  (let* ((style (plist-get render-node :computed-style))
         (display (plist-get render-node :display))
         
         ;; 1. 从样式计算盒模型
         (box-model (etaf-layout-compute-box-model render-node parent-context))
         
         ;; 2. 计算位置
         (margin (plist-get box-model :margin))
         (x (+ (plist-get parent-context :x-offset)
               (plist-get margin :left)))
         (y (+ (plist-get parent-context :current-y)
               (plist-get margin :top)))
         
         ;; 3. 计算内容区域
         (content-width (etaf-box-model-content-width box-model))
         (content-height (etaf-box-model-content-height box-model))
         
         ;; 4. 创建布局节点
         (layout-node (list :render-node render-node
                           :box-model box-model
                           :position (list :x x :y y)
                           :content-box (list :x (+ x 
                                                    (plist-get margin :left)
                                                    (plist-get (plist-get box-model :padding) :left)
                                                    (plist-get (plist-get box-model :border) :left-width))
                                             :y (+ y
                                                   (plist-get margin :top)
                                                   (plist-get (plist-get box-model :padding) :top)
                                                   (plist-get (plist-get box-model :border) :top-width))
                                             :width content-width
                                             :height content-height)
                           :children '())))
    
    ;; 5. 递归布局子元素
    (when (plist-get render-node :children)
      (let ((child-context (list :content-width content-width
                                :current-y (plist-get (plist-get layout-node :content-box) :y)
                                :x-offset (plist-get (plist-get layout-node :content-box) :x)))
            (children '()))
        (dolist (child (plist-get render-node :children))
          (let ((child-layout (etaf-layout-node child child-context)))
            (when child-layout
              (push child-layout children)
              ;; 更新 Y 坐标以累积子元素高度
              (plist-put child-context :current-y
                        (+ (plist-get child-context :current-y)
                           (etaf-box-model-total-height 
                            (plist-get child-layout :box-model)))))))
        (plist-put layout-node :children (nreverse children))))
    
    layout-node))
```

#### 宽度计算规则

对于块级元素：

```elisp
(defun etaf-layout-compute-block-width (style parent-width)
  "计算块级元素的宽度。
STYLE 是计算后的样式。
PARENT-WIDTH 是父容器的内容宽度。"
  (let* ((width-value (cdr (assq 'width style)))
         (margin-left (cdr (assq 'margin-left style)))
         (margin-right (cdr (assq 'margin-right style)))
         (padding-left (cdr (assq 'padding-left style)))
         (padding-right (cdr (assq 'padding-right style)))
         (border-left (cdr (assq 'border-left-width style)))
         (border-right (cdr (assq 'border-right-width style)))
         
         ;; 解析值
         (width (etaf-layout-parse-length width-value parent-width))
         (ml (etaf-layout-parse-length margin-left parent-width))
         (mr (etaf-layout-parse-length margin-right parent-width))
         (pl (etaf-layout-parse-length padding-left parent-width))
         (pr (etaf-layout-parse-length padding-right parent-width))
         (bl (etaf-layout-parse-length border-left parent-width))
         (br (etaf-layout-parse-length border-right parent-width)))
    
    (cond
     ;; 1. width 为 auto：填充父容器
     ((or (null width) (eq width 'auto))
      (let ((available (- parent-width ml mr pl pr bl br)))
        (max 0 available)))
     
     ;; 2. width 指定值：使用指定值
     (t width))))

(defun etaf-layout-parse-length (value reference-width)
  "解析 CSS 长度值。
VALUE 是 CSS 值字符串。
REFERENCE-WIDTH 是参考宽度（用于百分比计算）。
返回像素值或 'auto。"
  (cond
   ((null value) 'auto)
   ((string= value "auto") 'auto)
   ((string= value "0") 0)
   ((string-match "\\([0-9.]+\\)px$" value)
    (string-to-number (match-string 1 value)))
   ((string-match "\\([0-9.]+\\)%$" value)
    (* (/ (string-to-number (match-string 1 value)) 100.0)
       reference-width))
   ((string-match "\\([0-9.]+\\)em$" value)
    ;; 简化：假设 1em = 16px
    (* (string-to-number (match-string 1 value)) 16))
   (t 'auto)))
```

#### 高度计算规则

```elisp
(defun etaf-layout-compute-block-height (render-node box-model parent-height)
  "计算块级元素的高度。
如果 height 为 auto，根据子元素内容计算高度。"
  (let* ((style (plist-get render-node :computed-style))
         (height-value (cdr (assq 'height style)))
         (height (etaf-layout-parse-length height-value parent-height)))
    
    (if (eq height 'auto)
        ;; auto：根据子元素计算
        (etaf-layout-compute-auto-height render-node box-model)
      ;; 指定值：使用指定值
      height)))

(defun etaf-layout-compute-auto-height (render-node box-model)
  "计算 height: auto 时的高度（根据子元素）。"
  (let ((total-height 0))
    ;; 遍历子节点，累加高度
    (dolist (child-layout (plist-get render-node :children))
      (when child-layout
        (let ((child-box (plist-get child-layout :box-model)))
          (cl-incf total-height (etaf-box-model-total-height child-box)))))
    total-height))
```

### 2. 内联格式化上下文（Inline Formatting Context）

内联元素水平排列，形成行框（line box）：

```elisp
(defun etaf-layout-inline-formatting-context (render-node parent-context)
  "在内联格式化上下文中布局节点。
内联元素水平排列，自动换行。"
  (let* ((style (plist-get render-node :computed-style))
         (line-height (or (cdr (assq 'line-height style)) "normal"))
         
         ;; 当前行的状态
         (current-line (list :x (plist-get parent-context :x-offset)
                           :y (plist-get parent-context :current-y)
                           :width 0
                           :height 0
                           :boxes '()))
         (lines (list current-line))
         (max-width (plist-get parent-context :content-width)))
    
    ;; 处理内联内容
    (dolist (child (plist-get render-node :children))
      (cond
       ;; 文本节点
       ((stringp child)
        (etaf-layout-inline-text child current-line max-width lines))
       
       ;; 内联元素
       ((string= (plist-get child :display) "inline")
        (etaf-layout-inline-element child current-line max-width lines))
       
       ;; 内联块元素
       ((string= (plist-get child :display) "inline-block")
        (etaf-layout-inline-block-element child current-line max-width lines))))
    
    ;; 返回布局结果
    (list :type 'inline-formatting-context
          :lines lines)))

(defun etaf-layout-inline-text (text current-line max-width lines)
  "布局文本内容。
TEXT 是文本字符串。
CURRENT-LINE 是当前行。
MAX-WIDTH 是最大行宽。
LINES 是所有行的列表。"
  ;; 测量文本宽度（简化：假设每个字符宽度相同）
  (let* ((char-width 8)  ; 假设字符宽度
         (text-width (* (length text) char-width))
         (current-x (plist-get current-line :x))
         (current-width (plist-get current-line :width)))
    
    ;; 检查是否需要换行
    (if (> (+ current-width text-width) max-width)
        ;; 需要换行：创建新行
        (let ((new-line (list :x (plist-get current-line :x)
                            :y (+ (plist-get current-line :y)
                                  (plist-get current-line :height))
                            :width text-width
                            :height 20  ; 假设行高
                            :boxes (list (list :type 'text
                                              :content text
                                              :width text-width)))))
          (push new-line lines))
      ;; 不需要换行：添加到当前行
      (plist-put current-line :width (+ current-width text-width))
      (push (list :type 'text
                 :content text
                 :width text-width)
            (plist-get current-line :boxes)))))
```

### 3. 外边距折叠（Margin Collapsing）

在块级格式化上下文中，相邻块级元素的垂直外边距会折叠：

```elisp
(defun etaf-layout-compute-collapsed-margin (margin1 margin2)
  "计算折叠后的外边距。
规则：
1. 两个正值：取较大值
2. 两个负值：取绝对值较大的负值
3. 一正一负：相加"
  (cond
   ((and (>= margin1 0) (>= margin2 0))
    (max margin1 margin2))
   ((and (< margin1 0) (< margin2 0))
    (min margin1 margin2))
   (t (+ margin1 margin2))))

(defun etaf-layout-apply-margin-collapsing (layout-nodes)
  "对布局节点列表应用外边距折叠。"
  (let ((prev-node nil))
    (dolist (node layout-nodes)
      (when prev-node
        (let* ((prev-box (plist-get prev-node :box-model))
               (prev-margin-bottom (plist-get (plist-get prev-box :margin) :bottom))
               (curr-box (plist-get node :box-model))
               (curr-margin-top (plist-get (plist-get curr-box :margin) :top))
               
               ;; 计算折叠后的外边距
               (collapsed (etaf-layout-compute-collapsed-margin 
                          prev-margin-bottom curr-margin-top))
               
               ;; 调整当前节点的 Y 位置
               (curr-y (plist-get (plist-get node :position) :y))
               (adjustment (- collapsed (+ prev-margin-bottom curr-margin-top))))
          
          (plist-put (plist-get node :position) :y (+ curr-y adjustment))))
      
      (setq prev-node node))))
```

---

## 定位方案

CSS 提供了多种定位方案，通过 `position` 属性控制：

### 1. 静态定位（static）- 默认

元素在正常流中定位，忽略 `top/right/bottom/left` 属性。

### 2. 相对定位（relative）

```elisp
(defun etaf-layout-relative-position (layout-node)
  "应用相对定位。
相对于元素的正常位置进行偏移，不影响其他元素。"
  (let* ((style (plist-get (plist-get layout-node :render-node) :computed-style))
         (top (etaf-layout-parse-length (cdr (assq 'top style)) 0))
         (left (etaf-layout-parse-length (cdr (assq 'left style)) 0))
         (position (plist-get layout-node :position))
         (current-x (plist-get position :x))
         (current-y (plist-get position :y)))
    
    ;; 应用偏移
    (when (and left (not (eq left 'auto)))
      (plist-put position :x (+ current-x left)))
    (when (and top (not (eq top 'auto)))
      (plist-put position :y (+ current-y top)))))
```

### 3. 绝对定位（absolute）

```elisp
(defun etaf-layout-absolute-position (layout-node containing-block)
  "应用绝对定位。
相对于最近的非 static 定位祖先元素定位。
CONTAINING-BLOCK 是包含块的位置和尺寸。"
  (let* ((style (plist-get (plist-get layout-node :render-node) :computed-style))
         (top (etaf-layout-parse-length (cdr (assq 'top style)) 0))
         (right (etaf-layout-parse-length (cdr (assq 'right style)) 0))
         (bottom (etaf-layout-parse-length (cdr (assq 'bottom style)) 0))
         (left (etaf-layout-parse-length (cdr (assq 'left style)) 0))
         (cb-x (plist-get containing-block :x))
         (cb-y (plist-get containing-block :y))
         (cb-width (plist-get containing-block :width))
         (cb-height (plist-get containing-block :height)))
    
    ;; 计算位置
    (let ((x (if (not (eq left 'auto))
                (+ cb-x left)
              (if (not (eq right 'auto))
                  (- (+ cb-x cb-width) right 
                     (etaf-box-model-total-width (plist-get layout-node :box-model)))
                cb-x)))
          (y (if (not (eq top 'auto))
                (+ cb-y top)
              (if (not (eq bottom 'auto))
                  (- (+ cb-y cb-height) bottom
                     (etaf-box-model-total-height (plist-get layout-node :box-model)))
                cb-y))))
      
      (plist-put (plist-get layout-node :position) :x x)
      (plist-put (plist-get layout-node :position) :y y))))
```

### 4. 固定定位（fixed）

```elisp
(defun etaf-layout-fixed-position (layout-node viewport)
  "应用固定定位。
相对于视口定位，滚动时不移动。"
  (let* ((style (plist-get (plist-get layout-node :render-node) :computed-style))
         (top (etaf-layout-parse-length (cdr (assq 'top style)) 0))
         (left (etaf-layout-parse-length (cdr (assq 'left style)) 0))
         (viewport-width (plist-get viewport :width))
         (viewport-height (plist-get viewport :height)))
    
    (let ((x (if (not (eq left 'auto)) left 0))
          (y (if (not (eq top 'auto)) top 0)))
      (plist-put (plist-get layout-node :position) :x x)
      (plist-put (plist-get layout-node :position) :y y))))
```

---

## 文本和内联布局

### 文本测量

```elisp
(defun etaf-layout-measure-text (text style)
  "测量文本的宽度和高度。
TEXT 是文本字符串。
STYLE 是计算后的样式。
返回 (:width w :height h)。"
  (let* ((font-size (or (cdr (assq 'font-size style)) "16px"))
         (font-size-px (etaf-layout-parse-length font-size 0))
         (font-family (or (cdr (assq 'font-family style)) "sans-serif"))
         
         ;; 简化：假设字符宽度与字体大小成正比
         (char-width (* font-size-px 0.6))  ; 近似值
         (text-width (* (length text) char-width))
         (text-height (* font-size-px 1.2)))  ; 包含行高
    
    (list :width text-width :height text-height)))
```

### 行框布局

```elisp
(defun etaf-layout-line-box (inline-boxes max-width)
  "将内联盒子排列成行框。
INLINE-BOXES 是内联盒子列表。
MAX-WIDTH 是最大行宽。
返回行框列表。"
  (let ((lines '())
        (current-line '())
        (current-width 0)
        (line-height 0))
    
    (dolist (box inline-boxes)
      (let ((box-width (plist-get box :width))
            (box-height (plist-get box :height)))
        
        ;; 检查是否需要换行
        (if (and (> current-width 0)
                 (> (+ current-width box-width) max-width))
            ;; 换行
            (progn
              (push (list :boxes (nreverse current-line)
                         :width current-width
                         :height line-height)
                    lines)
              (setq current-line (list box)
                    current-width box-width
                    line-height box-height))
          ;; 添加到当前行
          (push box current-line)
          (setq current-width (+ current-width box-width))
          (setq line-height (max line-height box-height)))))
    
    ;; 添加最后一行
    (when current-line
      (push (list :boxes (nreverse current-line)
                 :width current-width
                 :height line-height)
            lines))
    
    (nreverse lines)))
```

---

## Flexbox 布局

### Flexbox 数据结构

```elisp
(:flex-container
  :flex-direction "row"|"column"|"row-reverse"|"column-reverse"
  :flex-wrap "nowrap"|"wrap"|"wrap-reverse"
  :justify-content "flex-start"|"flex-end"|"center"|"space-between"|"space-around"
  :align-items "flex-start"|"flex-end"|"center"|"baseline"|"stretch"
  :align-content "flex-start"|"flex-end"|"center"|"space-between"|"space-around"|"stretch"
  :items (flex-item1 flex-item2 ...))

(:flex-item
  :flex-grow <number>
  :flex-shrink <number>
  :flex-basis "auto"|<length>
  :align-self "auto"|"flex-start"|"flex-end"|"center"|"baseline"|"stretch")
```

### Flexbox 布局算法

```elisp
(defun etaf-layout-flexbox (render-node parent-context)
  "计算 flexbox 布局。"
  (let* ((style (plist-get render-node :computed-style))
         (flex-direction (or (cdr (assq 'flex-direction style)) "row"))
         (justify-content (or (cdr (assq 'justify-content style)) "flex-start"))
         (align-items (or (cdr (assq 'align-items style)) "stretch"))
         (flex-wrap (or (cdr (assq 'flex-wrap style)) "nowrap"))
         
         ;; 确定主轴和交叉轴
         (is-row (or (string= flex-direction "row")
                    (string= flex-direction "row-reverse")))
         (main-size (if is-row
                       (plist-get parent-context :content-width)
                     (plist-get parent-context :content-height)))
         
         ;; flex 项目
         (flex-items (mapcar (lambda (child)
                              (etaf-layout-flex-item child style))
                            (plist-get render-node :children))))
    
    ;; 1. 确定 flex 项目的 flex-basis
    (dolist (item flex-items)
      (etaf-layout-compute-flex-basis item style is-row))
    
    ;; 2. 计算可用空间
    (let* ((total-basis (apply '+ (mapcar (lambda (item)
                                           (plist-get item :flex-basis))
                                         flex-items)))
           (free-space (- main-size total-basis)))
      
      ;; 3. 分配可用空间（flex-grow 或 flex-shrink）
      (if (> free-space 0)
          (etaf-layout-flex-grow flex-items free-space)
        (etaf-layout-flex-shrink flex-items (abs free-space)))
      
      ;; 4. 根据 justify-content 排列项目
      (etaf-layout-justify-content flex-items justify-content main-size is-row)
      
      ;; 5. 根据 align-items 对齐项目
      (etaf-layout-align-items flex-items align-items is-row))))

(defun etaf-layout-flex-grow (flex-items free-space)
  "分配额外空间给 flex-grow > 0 的项目。"
  (let ((total-grow (apply '+ (mapcar (lambda (item)
                                       (plist-get item :flex-grow))
                                     flex-items))))
    (when (> total-grow 0)
      (dolist (item flex-items)
        (let* ((flex-grow (plist-get item :flex-grow))
               (grow-amount (if (> flex-grow 0)
                               (* free-space (/ flex-grow total-grow))
                             0))
               (flex-basis (plist-get item :flex-basis)))
          (plist-put item :main-size (+ flex-basis grow-amount)))))))

(defun etaf-layout-justify-content (flex-items justify-content main-size is-row)
  "根据 justify-content 排列 flex 项目。"
  (let* ((total-size (apply '+ (mapcar (lambda (item)
                                        (plist-get item :main-size))
                                      flex-items)))
         (free-space (- main-size total-size))
         (item-count (length flex-items))
         (current-pos 0))
    
    (cond
     ;; flex-start: 默认，从起点开始
     ((string= justify-content "flex-start")
      (dolist (item flex-items)
        (plist-put item :main-pos current-pos)
        (cl-incf current-pos (plist-get item :main-size))))
     
     ;; flex-end: 从终点开始
     ((string= justify-content "flex-end")
      (setq current-pos free-space)
      (dolist (item flex-items)
        (plist-put item :main-pos current-pos)
        (cl-incf current-pos (plist-get item :main-size))))
     
     ;; center: 居中
     ((string= justify-content "center")
      (setq current-pos (/ free-space 2))
      (dolist (item flex-items)
        (plist-put item :main-pos current-pos)
        (cl-incf current-pos (plist-get item :main-size))))
     
     ;; space-between: 项目之间均分空间
     ((string= justify-content "space-between")
      (let ((gap (if (> item-count 1)
                    (/ free-space (1- item-count))
                  0)))
        (dolist (item flex-items)
          (plist-put item :main-pos current-pos)
          (cl-incf current-pos (+ (plist-get item :main-size) gap)))))
     
     ;; space-around: 项目两侧均分空间
     ((string= justify-content "space-around")
      (let ((gap (/ free-space item-count)))
        (setq current-pos (/ gap 2))
        (dolist (item flex-items)
          (plist-put item :main-pos current-pos)
          (cl-incf current-pos (+ (plist-get item :main-size) gap))))))))
```

---

## 实现示例代码

### 完整的布局模块骨架

```elisp
;;; etaf-layout.el --- Layout computation for render tree -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'etaf-render)

;;; 盒模型计算

(defun etaf-layout-compute-box-model (render-node parent-context)
  "从渲染节点计算盒模型。"
  (let* ((style (plist-get render-node :computed-style))
         (parent-width (plist-get parent-context :content-width))
         
         ;; 提取样式值
         (width (etaf-layout-parse-length (cdr (assq 'width style)) parent-width))
         (height (etaf-layout-parse-length (cdr (assq 'height style)) parent-width))
         (padding-top (etaf-layout-parse-length (cdr (assq 'padding-top style)) parent-width))
         (padding-right (etaf-layout-parse-length (cdr (assq 'padding-right style)) parent-width))
         (padding-bottom (etaf-layout-parse-length (cdr (assq 'padding-bottom style)) parent-width))
         (padding-left (etaf-layout-parse-length (cdr (assq 'padding-left style)) parent-width))
         (border-top (etaf-layout-parse-length (cdr (assq 'border-top-width style)) parent-width))
         (border-right (etaf-layout-parse-length (cdr (assq 'border-right-width style)) parent-width))
         (border-bottom (etaf-layout-parse-length (cdr (assq 'border-bottom-width style)) parent-width))
         (border-left (etaf-layout-parse-length (cdr (assq 'border-left-width style)) parent-width))
         (margin-top (etaf-layout-parse-length (cdr (assq 'margin-top style)) parent-width))
         (margin-right (etaf-layout-parse-length (cdr (assq 'margin-right style)) parent-width))
         (margin-bottom (etaf-layout-parse-length (cdr (assq 'margin-bottom style)) parent-width))
         (margin-left (etaf-layout-parse-length (cdr (assq 'margin-left style)) parent-width))
         
         ;; 计算内容宽度
         (content-width (if (eq width 'auto)
                           (- parent-width
                              (or padding-left 0) (or padding-right 0)
                              (or border-left 0) (or border-right 0)
                              (or margin-left 0) (or margin-right 0))
                         width)))
    
    ;; 构建盒模型
    (list :box-sizing "content-box"
          :content (list :width content-width
                        :height (or height 0))
          :padding (list :top (or padding-top 0)
                        :right (or padding-right 0)
                        :bottom (or padding-bottom 0)
                        :left (or padding-left 0))
          :border (list :top-width (or border-top 0)
                       :right-width (or border-right 0)
                       :bottom-width (or border-bottom 0)
                       :left-width (or border-left 0))
          :margin (list :top (or margin-top 0)
                       :right (or margin-right 0)
                       :bottom (or margin-bottom 0)
                       :left (or margin-left 0)))))

;;; 布局树构建

(defun etaf-layout-build-tree (render-tree viewport)
  "从渲染树构建布局树。
RENDER-TREE 是渲染树根节点。
VIEWPORT 是视口大小 (:width w :height h)。"
  (let ((root-context (list :content-width (plist-get viewport :width)
                           :content-height (plist-get viewport :height)
                           :current-y 0
                           :x-offset 0)))
    (etaf-layout-node render-tree root-context)))

(defun etaf-layout-node (render-node parent-context)
  "递归布局渲染节点。"
  (when render-node
    (let* ((display (plist-get render-node :display))
           (style (plist-get render-node :computed-style))
           (position (cdr (assq 'position style))))
      
      (cond
       ;; 块级元素
       ((string= display "block")
        (etaf-layout-block-formatting-context render-node parent-context))
       
       ;; 内联元素
       ((string= display "inline")
        (etaf-layout-inline-formatting-context render-node parent-context))
       
       ;; flex 容器
       ((string= display "flex")
        (etaf-layout-flexbox render-node parent-context))
       
       ;; 默认按块级处理
       (t (etaf-layout-block-formatting-context render-node parent-context))))))

(provide 'etaf-layout)
;;; etaf-layout.el ends here
```

### 使用示例

```elisp
;; 1. 创建 DOM 和 CSSOM
(setq my-dom (etaf-etml-to-dom
              '(html
                 (head
                   (style "
                     .container { width: 800px; padding: 20px; }
                     .box { width: 200px; height: 100px; margin: 10px; }"))
                 (body
                   (div :class "container"
                     (div :class "box" "Box 1")
                     (div :class "box" "Box 2"))))))

(setq my-cssom (etaf-css-build-cssom my-dom))

;; 2. 构建渲染树
(setq my-render-tree (etaf-render-build-tree my-dom my-cssom))

;; 3. 构建布局树
(setq my-layout-tree (etaf-layout-build-tree my-render-tree 
                                             '(:width 1024 :height 768)))

;; 4. 查询布局信息
(defun print-layout-tree (layout-node &optional indent)
  "打印布局树结构。"
  (setq indent (or indent 0))
  (let* ((render-node (plist-get layout-node :render-node))
         (tag (plist-get render-node :tag))
         (position (plist-get layout-node :position))
         (box-model (plist-get layout-node :box-model))
         (content (plist-get box-model :content)))
    
    (message "%s<%s> pos=(%d,%d) size=%dx%d"
             (make-string (* indent 2) ?\s)
             tag
             (plist-get position :x)
             (plist-get position :y)
             (plist-get content :width)
             (plist-get content :height))
    
    (dolist (child (plist-get layout-node :children))
      (print-layout-tree child (1+ indent)))))

(print-layout-tree my-layout-tree)
```

---

## 总结

本文档提供了在 ETAF 项目中实现盒模型渲染和布局算法的详细指南：

1. **盒模型数据结构**: 定义了完整的盒模型表示，包括 content、padding、border、margin
2. **布局算法**: 详细说明了块级布局、内联布局、定位方案和 Flexbox 布局
3. **实现代码**: 提供了可直接使用的代码框架和示例

### 下一步实现建议

1. 在 `etaf-layout.el` 中实现基础布局算法
2. 添加文本测量和换行支持
3. 实现定位方案（relative、absolute、fixed）
4. 添加 Flexbox 布局支持（可选）
5. 编写完整的测试用例

通过结合本文档和 `DATA-STRUCTURES.md`，开发者应该能够完整实现 ETAF 的布局系统。
