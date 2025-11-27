# 布局字符串生成功能

## 概述

新增的 `etaf-layout-to-string` 函数提供了一种适合 Emacs buffer 渲染的布局方式。与浏览器渲染需要精确的 x,y 坐标不同，这个函数通过文本拼接的方式生成最终的布局字符串，可以直接插入到 buffer 中显示。

## 设计理念

### 浏览器渲染 vs Emacs 渲染

**浏览器渲染方式**：
- 使用精确的 x,y 坐标定位每个元素
- 通过 `etaf-layout-to-string` 函数查看位置信息
- 适用于调试和分析布局

**Emacs 渲染方式**：
- 通过文本拼接生成最终布局
- 使用 `etaf-layout-to-string` 函数生成可插入的字符串
- 适用于实际在 Emacs buffer 中显示内容

## 核心函数

### `etaf-layout-to-string`

```elisp
(etaf-layout-to-string layout-tree)
```

**参数**：
- `layout-tree`：布局树根节点

**返回值**：
- 拼接好的布局字符串，可以直接插入到 buffer 中

**功能**：
将布局树转换为可插入 Emacs buffer 的字符串。这个函数会：
1. 递归处理所有子节点
2. 根据盒模型（padding、border、margin）拼接文本
3. 使用像素空格和边框字符创建视觉效果
4. 生成可以直接显示的最终字符串

## 使用示例

### 基础示例

```elisp
(require 'etaf)

;; 1. 创建 DOM
(let* ((dom (etaf-tml-to-dom
             '(html
               (head
                (style "body { width: 300px; height: 100px; padding: 20px; }"))
               (body "Hello, ETAF!"))))
       
       ;; 2. 构建 CSSOM
       (cssom (etaf-css-build-cssom dom))
       
       ;; 3. 构建渲染树
       (render-tree (etaf-render-build-tree dom cssom))
       
       ;; 4. 构建布局树
       (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
       
       ;; 5. 生成 buffer 字符串
       (buffer-string (etaf-layout-to-string layout-tree)))
  
  ;; 6. 在 buffer 中显示
  (with-current-buffer (get-buffer-create "*ETAF Demo*")
    (erase-buffer)
    (insert buffer-string)
    (display-buffer (current-buffer))))
```

### 嵌套布局示例

```elisp
(let* ((dom (etaf-tml-to-dom
             '(html
               (head
                (style "
                  body { width: 400px; padding: 20px; }
                  .box { width: 100%; height: 50px; margin-bottom: 10px; padding: 10px; border-left-width: 2px; }
                "))
               (body
                (div :class "box" "Box 1")
                (div :class "box" "Box 2")
                (div :class "box" "Box 3")))))
       (cssom (etaf-css-build-cssom dom))
       (render-tree (etaf-render-build-tree dom cssom))
       (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
       (buffer-string (etaf-layout-to-string layout-tree)))
  
  ;; 显示结果
  (with-current-buffer (get-buffer-create "*ETAF Nested Demo*")
    (erase-buffer)
    (insert buffer-string)
    (display-buffer (current-buffer))))
```

## 实现细节

### 后序遍历（Post-order Traversal）

函数使用后序遍历从叶子节点开始构建整个结构的文本：

1. 首先递归处理所有子节点
2. 子节点的结果按垂直方向堆叠（使用 `\n` 连接）
3. 然后在当前节点应用盒模型（padding、border、margin）
4. 返回完整构建的字符串

这确保了文本内容从最深的叶子节点开始逐层构建。

### 高度单位：行数而非像素

在 Emacs 中，垂直方向的高度使用**行数（lines）**而不是像素：

- `content-height` - 内容的行数
- `padding-top`/`padding-bottom` - 内边距的行数（使用 `floor` 向下取整）
- `margin-top`/`margin-bottom` - 外边距的行数（使用 `floor` 向下取整）

水平方向仍然使用像素宽度。

### 文本拼接顺序

生成布局字符串的步骤（由内向外）：

1. **内容区域**：处理元素的文本内容或递归处理子元素
2. **Padding（垂直）**：在内容上下添加 padding
3. **Padding（水平）**：在内容左右添加 padding
4. **Border（水平）**：在左右添加边框
5. **Margin（水平）**：在左右添加 margin
6. **Margin（垂直）**：在上下添加 margin

### 使用的工具函数

- `etaf-lines-concat`：水平方向拼接字符串
- `etaf-lines-stack`：垂直方向堆叠字符串
- `etaf-pixel-blank`：创建指定像素宽度和高度的空白区域
- `etaf-pixel-border`：创建指定像素宽度和高度的边框
- `etaf-lines-justify`：调整文本宽度
- `etaf-lines-align`：调整文本高度

### 与 etaf-box.el 的对比

| 特性 | etaf-box.el | etaf-layout.el (新功能) |
|------|-------------|------------------------|
| 数据结构 | EIEIO defclass | plist |
| 输入来源 | 手动创建 box 对象 | 从 DOM/CSSOM/渲染树自动生成 |
| 功能重点 | 滚动、交互等高级功能 | 基础布局和文本拼接 |
| 使用场景 | 复杂的交互式组件 | 静态布局渲染 |
| 代码量 | 约 40KB | 约 5KB（新增部分） |

## 运行示例

### 使用提供的示例文件

```elisp
;; 加载示例
(load-file "examples/etaf-layout-buffer-string-example.el")

;; 运行简单示例
(etaf-layout-buffer-string-example-simple)

;; 运行嵌套示例
(etaf-layout-buffer-string-example-nested)

;; 运行对比示例
(etaf-layout-buffer-string-example-comparison)

;; 运行所有示例
(etaf-layout-buffer-string-run-all-examples)
```

## 测试

运行测试套件：

```elisp
;; 加载测试
(load-file "tests/etaf-layout-buffer-string-tests.el")

;; 运行所有测试
(ert-run-tests-batch-and-exit 'etaf-layout-test-.*-buffer-string)
```

## 优势

1. **简洁性**：代码简洁，易于理解和维护
2. **一致性**：与项目其他模块（etaf-render.el, etaf-css.el）的 plist 风格保持一致
3. **实用性**：直接生成可插入 buffer 的字符串，适合 Emacs 渲染
4. **灵活性**：支持完整的盒模型（content、padding、border、margin）
5. **可扩展性**：易于添加新功能，如样式、颜色等

## 未来扩展

1. **样式支持**：添加背景色、前景色、字体等样式
2. **边框样式**：支持不同样式的边框（实线、虚线等）
3. **文本对齐**：支持更多的文本对齐方式
4. **溢出处理**：处理内容溢出的情况
5. **性能优化**：缓存已生成的字符串，避免重复计算

## 相关文档

- [LAYOUT-IMPLEMENTATION.md](../LAYOUT-IMPLEMENTATION.md) - 布局系统实现总结
- [BOX-MODEL-LAYOUT.md](../BOX-MODEL-LAYOUT.md) - 盒模型与布局实现指南
- [etaf-layout-example.el](etaf-layout-example.el) - 布局系统完整示例
- [etaf-layout-buffer-string-example.el](etaf-layout-buffer-string-example.el) - 布局字符串生成示例
