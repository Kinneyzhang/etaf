# ETAF - Emacs Template and Framework

ETAF 是一个在 Emacs Lisp 中实现的类浏览器渲染系统，包含 DOM 树、CSS 对象模型（CSSOM）、渲染树和布局引擎。

## 项目概述

ETAF 实现了完整的 CSS 样式计算管线：

```
TML 格式 → DOM 树 → CSSOM → 渲染树 → 布局树 → 绘制
```

## 核心模块

- **etaf-tml.el** - TML (Template Markup Language) 到 DOM 的转换
- **etaf-dom.el** - DOM 操作、查询和遍历
- **etaf-css.el** - CSS 对象模型（CSSOM）主入口
- **etaf-css-parser.el** - CSS 解析器（支持 !important 和 @media）
- **etaf-css-selector.el** - CSS 选择器解析和匹配
- **etaf-css-cascade.el** - CSS 层叠算法和特异性计算
- **etaf-css-inheritance.el** - CSS 属性继承
- **etaf-css-media.el** - 媒体查询支持
- **etaf-css-cache.el** - 计算样式缓存
- **etaf-css-index.el** - 规则索引优化
- **etaf-render.el** - 渲染树构建

## 文档

### 核心文档

- **[DATA-STRUCTURES.md](DATA-STRUCTURES.md)** - 📘 数据结构详解
  - TML、DOM、CSSOM、渲染树的完整说明
  - 数据结构之间的关系和数据流
  - 盒模型渲染的数据流程
  - 实际使用示例和最佳实践

- **[BOX-MODEL-LAYOUT.md](BOX-MODEL-LAYOUT.md)** - 📐 盒模型与布局实现指南
  - CSS 盒模型详解（content、padding、border、margin）
  - 布局算法实现（块级、内联、Flexbox）
  - 定位方案（static、relative、absolute、fixed）
  - 完整的实现代码示例

### 架构文档

- **[MODULE-STRUCTURE-CN.md](MODULE-STRUCTURE-CN.md)** - 模块结构说明（中文）
- **[MODULE-STRUCTURE.md](MODULE-STRUCTURE.md)** - Module Structure (English)
- **[IMPLEMENTATION-SUMMARY.md](IMPLEMENTATION-SUMMARY.md)** - CSSOM 实现总结

### CSS 功能文档

- **[CSSOM-DESIGN.md](CSSOM-DESIGN.md)** - CSSOM 设计说明与改进建议
- **[CSSOM-COMPARISON.md](CSSOM-COMPARISON.md)** - CSSOM 与浏览器实现对比
- **[CSS-MODULES.md](CSS-MODULES.md)** - CSS 模块详解
- **[MEDIA-QUERY-IMPLEMENTATION.md](MEDIA-QUERY-IMPLEMENTATION.md)** - 媒体查询实现
- **[ETAF-CSS-README.md](ETAF-CSS-README.md)** - CSS 系统使用指南
- **[VALIDATION.md](VALIDATION.md)** - 验证和测试

## 快速开始

### 基础使用

```elisp
(require 'etaf)

;; 1. 从 TML 创建 DOM
(setq my-dom
      (etaf-tml-to-dom
       '(html
          (head
            (style "
              .container { width: 800px; padding: 20px; }
              .box { width: 200px; height: 100px; margin: 10px; }"))
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

;; 5. 查询节点样式
(etaf-render-walk my-render-tree
  (lambda (node)
    (message "Tag: %s, Display: %s"
             (plist-get node :tag)
             (plist-get node :display))))
```

### 响应式设计（媒体查询）

```elisp
;; 在不同视口宽度下构建 CSSOM
(setq mobile-cssom 
      (etaf-css-build-cssom dom '((type . screen) (width . 375))))
(setq desktop-cssom 
      (etaf-css-build-cssom dom '((type . screen) (width . 1024))))
```

## 功能特性

### 已实现功能 ✅

- ✅ **完整的 CSS 选择器支持**
  - 标签、类、ID、属性选择器
  - 后代、子元素、相邻兄弟、通用兄弟组合器
  - 伪类选择器（:first-child、:last-child 等）

- ✅ **CSS 层叠算法**
  - 选择器特异性计算
  - !important 支持
  - 内联样式优先级
  - 文档顺序处理

- ✅ **属性继承**
  - 可继承属性自动传递（color、font-* 等）
  - 子元素可覆盖继承值

- ✅ **媒体查询**
  - @media 规则解析
  - 媒体类型匹配（screen、print、all）
  - 媒体特性（width、height、min-width、max-width 等）

- ✅ **性能优化**
  - 规则索引（按标签、类、ID）
  - 计算样式缓存
  - 选择器匹配优化

- ✅ **渲染树构建**
  - 过滤不可见元素（display: none）
  - 附加计算后的样式
  - 支持遍历和查询

### 计划实现功能 📋

- 📋 **布局系统**（参见 BOX-MODEL-LAYOUT.md）
  - 盒模型计算
  - 块级布局（Normal Flow）
  - 内联布局和文本换行
  - 定位方案（relative、absolute、fixed）
  - Flexbox 布局

- 📋 **绘制系统**
  - 背景和边框绘制
  - 文本渲染
  - 图层合成

## 性能特性

### 规则索引

CSSOM 自动构建索引，按标签、类、ID 分类规则，显著提升查询性能：

- **未索引**: O(n × m)，n = 规则数，m = 选择器复杂度
- **已索引**: O(k × log k)，k << n（候选规则数远小于总规则数）

### 样式缓存

计算样式会自动缓存，重复查询时直接返回缓存结果：

- **首次查询**: 完整计算
- **重复查询**: 10-100x 性能提升

```elisp
;; 第一次查询：计算并缓存
(setq style1 (etaf-css-get-computed-style cssom node dom))

;; 第二次查询：从缓存获取
(setq style2 (etaf-css-get-computed-style cssom node dom))

;; DOM 变化后清除缓存
(etaf-css-clear-cache cssom)
```

## 测试

运行测试套件：

```bash
cd tests
emacs -batch -l etaf-ert.el -l etaf-css-tests.el -f ert-run-tests-batch-and-exit
```

测试文件：
- `etaf-css-tests.el` - CSS 主功能测试
- `etaf-css-important-tests.el` - !important 和层叠测试
- `etaf-css-cache-tests.el` - 缓存测试
- `etaf-css-index-tests.el` - 索引测试
- `etaf-css-inheritance-tests.el` - 继承测试
- `etaf-css-media-tests.el` - 媒体查询测试

## 示例

查看 `examples/` 目录获取更多示例：
- `etaf-css-example.el` - CSS 功能演示
- `etaf-render-example.el` - 渲染树使用示例

## 贡献

欢迎贡献代码、报告问题或提出改进建议！

## 许可证

本项目采用 GNU General Public License v3.0 或更高版本。

## 相关资源

- [CSS 规范](https://www.w3.org/Style/CSS/)
- [CSSOM 规范](https://www.w3.org/TR/cssom-1/)
- [CSS 层叠规范](https://www.w3.org/TR/css-cascade/)
- [CSS 盒模型规范](https://www.w3.org/TR/css-box-3/)
