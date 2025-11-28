# ETAF - Emacs Text-based Application Framework

ETAF 是一个在 Emacs Lisp 中实现的类浏览器渲染系统，包含 DOM 树、CSS 对象模型（CSSOM）、渲染树和布局引擎。

## 项目概述

ETAF 实现了完整的 CSS 样式计算管线：

```
TML 格式 → DOM 树 → CSSOM → 渲染树 → 布局树 → 绘制
```

现在还支持 Vue.js 风格的模板语法：

```
Vue 模板 → 渲染后的 TML → DOM 树 → ...
```

## 核心模块

- **etaf-tml.el** - TML (Template Markup Language) 到 DOM 的转换
- **etaf-template.el** - Vue.js 风格的模板语法支持
- **etaf-dom.el** - DOM 操作、查询和遍历
- **etaf-tailwind.el** - Tailwind CSS 支持（新增）
- **etaf-css.el** - CSS 对象模型（CSSOM）主入口
- **etaf-css-parser.el** - CSS 解析器（支持 !important 和 @media）
- **etaf-css-selector.el** - CSS 选择器解析和匹配
- **etaf-css-cascade.el** - CSS 层叠算法和特异性计算
- **etaf-css-inheritance.el** - CSS 属性继承
- **etaf-css-media.el** - 媒体查询支持
- **etaf-css-cache.el** - 计算样式缓存
- **etaf-css-index.el** - 规则索引优化
- **etaf-render.el** - 渲染树构建
- **etaf-layout.el** - 盒模型和布局计算

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

- **[LAYOUT-BUFFER-STRING.md](LAYOUT-BUFFER-STRING.md)** - 🎨 布局字符串生成功能（新增）
  - 将布局树转换为可插入 buffer 的字符串
  - 适合 Emacs 渲染的文本拼接方式
  - 使用示例和实现细节

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
              .container { width: 800px; padding-left: 20px; padding-right: 20px; }
              .box { width: 200px; height: 100px; margin-left: 10px; margin-right: 10px; margin-top: 10px; margin-bottom: 10px; }"))
          (body
            (div :class "container"
              (div :class "box" "Box 1")
              (div :class "box" "Box 2"))))))

;; 2. 构建 CSSOM
(setq my-cssom (etaf-css-build-cssom my-dom))

;; 3. 构建渲染树
(setq my-render-tree (etaf-render-build-tree my-dom my-cssom))

;; 4. 构建布局树（新增）
(setq my-layout-tree (etaf-layout-build-tree my-render-tree '(:width 1024 :height 768)))

;; 5. 查看布局树结构
(message "布局树:\n%s" (etaf-layout-to-string my-layout-tree))

;; 6. 生成可插入 buffer 的布局字符串（Emacs 渲染方式）
(setq buffer-string (etaf-layout-to-string my-layout-tree))
(with-current-buffer (get-buffer-create "*ETAF Layout*")
  (erase-buffer)
  (insert buffer-string)
  (display-buffer (current-buffer)))

;; 7. 查询节点布局信息
(etaf-layout-walk my-layout-tree
  (lambda (node)
    (let ((pos (plist-get node :position))
          (box (plist-get node :box-model)))
      (message "Tag: %s, Position: (%d,%d), Size: %dx%d"
               (plist-get (plist-get node :render-node) :tag)
               (plist-get pos :x)
               (plist-get pos :y)
               (etaf-box-model-content-width box)
               (etaf-box-model-content-height box)))))
```

### 响应式设计（媒体查询）

```elisp
;; 在不同视口宽度下构建 CSSOM
(setq mobile-cssom 
      (etaf-css-build-cssom dom '((type . screen) (width . 375))))
(setq desktop-cssom 
      (etaf-css-build-cssom dom '((type . screen) (width . 1024))))
```

### Vue.js 风格模板语法（新增）

```elisp
(require 'etaf-template)

;; 定义数据
(setq my-data '(:name "Alice" 
                :loggedIn t 
                :items ("Apple" "Banana" "Cherry")))

;; 创建带有 Vue 风格指令的模板
(setq my-template
  '(div :class "app"
     ;; 文本插值
     (h1 "Hello, {{ name }}!")
     
     ;; 条件渲染
     (p :v-if "loggedIn" "Welcome back!")
     (p :v-else "Please login")
     
     ;; 列表渲染
     (ul
       (li :v-for "item in items" "{{ item }}"))))

;; 渲染模板
(setq rendered (etaf-template-render my-template my-data))
;; => (div :class "app" 
;;      (h1 "Hello, Alice!") 
;;      (p "Welcome back!")
;;      (ul (li "Apple") (li "Banana") (li "Cherry")))

;; 转换为 DOM
(setq my-dom (etaf-tml-to-dom rendered))
```

#### 支持的模板指令

| 指令 | 说明 | 示例 |
|------|------|------|
| `{{ expr }}` | 文本插值 | `"Hello, {{ name }}"` |
| `:v-if` | 条件渲染 | `(p :v-if "visible" "Text")` |
| `:v-else-if` | 多条件渲染 | `(p :v-else-if "other" "Alt")` |
| `:v-else` | 默认分支 | `(p :v-else "Default")` |
| `:v-for` | 列表渲染 | `(li :v-for "item in items" "{{ item }}")` |
| `:v-show` | 显示/隐藏 | `(div :v-show "visible" "Content")` |
| `:v-text` | 文本内容 | `(span :v-text "message")` |

#### v-for 支持的格式

```elisp
;; 基本格式
(li :v-for "item in items" "{{ item }}")

;; 带索引
(li :v-for "(item, index) in items" "{{ index }}: {{ item }}")
```

#### 响应式数据

```elisp
;; 创建响应式数据
(setq reactive (etaf-template-create-reactive '(:count 0 :name "Counter")))

;; 添加数据变化监听
(etaf-template-watch reactive
  (lambda (r key value)
    (message "Data changed: %S = %S" key value)))

;; 获取和设置数据
(etaf-template-get reactive :count)  ;; => 0
(etaf-template-set reactive :count 5) ;; 触发 watcher
```

### Tailwind CSS 支持（新增）

Tailwind CSS 类名现在可以直接在 TML 中使用，会被自动解析到 CSSOM 并正确渲染：

```elisp
(require 'etaf)

;; 直接在 TML 中使用 Tailwind 类
(setq dom (etaf-tml-to-dom
           '(div :class "flex items-center justify-between bg-white rounded-lg shadow-md p-4"
              (h1 :class "text-lg font-bold text-gray-900" "Title")
              (button :class "bg-blue-500 text-white px-4 py-2 rounded" "Click me"))))

;; 构建 CSSOM - Tailwind 类会自动转换为 CSS 属性
(setq cssom (etaf-css-build-cssom dom))

;; 获取计算样式 - 包含 Tailwind 转换后的 CSS
(etaf-css-get-computed-style cssom dom dom)
;; => ((display . "flex") (align-items . "center") (justify-content . "space-between")
;;     (background-color . "#ffffff") (border-radius . "0.5rem")
;;     (box-shadow . "0 4px 6px -1px rgb(0 0 0 / 0.1), ...") (padding . "1rem"))

;; 构建渲染树 - 样式会正确应用
(setq render-tree (etaf-render-build-tree dom cssom))
```

#### 手动使用 Tailwind 工具函数

```elisp
(require 'etaf-tailwind)

;; 1. 解析 Tailwind 类名
(etaf-tailwind-parse-class "md:hover:bg-red-500")
;; => (:variants ("md" "hover") :utility "bg-red-500" :property "bg" :value "red-500")

;; 2. 验证 Tailwind 类名
(etaf-tailwind-class-p "bg-red-500")   ;; => t
(etaf-tailwind-class-p "hover:text-lg") ;; => t
(etaf-tailwind-class-p "invalid-xyz")   ;; => nil

;; 3. 将 Tailwind 类转换为 CSS
(etaf-tailwind-to-css "bg-red-500")
;; => ((background-color . "#ef4444"))

(etaf-tailwind-to-css "p-4")
;; => ((padding . "1rem"))

(etaf-tailwind-to-css "flex")
;; => ((display . "flex"))

;; 4. 多个类转换为 CSS
(etaf-tailwind-classes-to-css "flex items-center bg-white p-4")
;; => ((display . "flex") (align-items . "center") 
;;     (background-color . "#ffffff") (padding . "1rem"))

;; 5. DOM 集成
(setq node '(div ((class . "container")) "Hello"))
(etaf-tailwind-add-class node "flex")
(etaf-tailwind-add-class node "bg-blue-500")
;; node => (div ((class . "container flex bg-blue-500")) "Hello")

;; 6. 查询 DOM 中的 Tailwind 类
(etaf-dom-query-tailwind dom "flex")  ;; 查找所有 flex 类节点
(etaf-dom-query-tailwind-pattern dom "^bg-")  ;; 查找所有背景类节点
```

#### 支持的 Tailwind 功能

| 功能 | 说明 | 示例 |
|------|------|------|
| 响应式前缀 | sm, md, lg, xl, 2xl | `md:flex` |
| 状态变体 | hover, focus, active 等 | `hover:bg-blue-500` |
| 任意值 | 方括号语法 | `bg-[#1da1f2]` |
| 颜色 | 完整的 Tailwind 调色板 | `bg-red-500`, `text-gray-700` |
| 间距 | padding, margin | `p-4`, `mx-auto`, `mt-2` |
| Flexbox | 弹性布局 | `flex`, `justify-center`, `items-center` |
| 圆角 | border-radius | `rounded-lg`, `rounded-full` |
| 阴影 | box-shadow | `shadow-md`, `shadow-lg` |

## 功能特性

### 已实现功能 ✅

- ✅ **Vue.js 风格模板语法**（新增）
  - 文本插值 `{{ expression }}`
  - 条件渲染 `v-if` / `v-else-if` / `v-else`
  - 列表渲染 `v-for`（支持索引）
  - 显示控制 `v-show`
  - 文本指令 `v-text`
  - 响应式数据系统

- ✅ **Tailwind CSS 支持**（新增）
  - 类名解析和验证
  - 响应式前缀支持（sm、md、lg、xl、2xl）
  - 状态变体支持（hover、focus、active 等）
  - 任意值语法支持
  - Tailwind 到 CSS 属性转换
  - DOM 集成操作（添加、移除、切换类）
  - **CSSOM 集成**：TML 中的 Tailwind 类自动解析到计算样式
  - **渲染树集成**：正确渲染为最终样式

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

- ✅ **布局系统**（新实现）
  - 盒模型计算（content、padding、border、margin）
  - 块级布局（Block Formatting Context）
  - width/height 计算（包括 auto 处理）
  - 位置计算（嵌套元素的精确定位）
  - plist 基础的清晰数据结构
  - **布局字符串生成**（新增）：将布局树转换为可插入 Emacs buffer 的字符串，通过文本拼接而非坐标定位实现渲染

### 计划实现功能 📋

- 📋 **布局系统增强**
  - 内联布局和文本换行
  - 外边距折叠（Margin Collapsing）
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
- `etaf-template-tests.el` - Vue.js 风格模板语法测试
- `etaf-tailwind-tests.el` - Tailwind CSS 支持测试（新增）
- `etaf-css-tests.el` - CSS 主功能测试
- `etaf-css-important-tests.el` - !important 和层叠测试
- `etaf-css-cache-tests.el` - 缓存测试
- `etaf-css-index-tests.el` - 索引测试
- `etaf-css-inheritance-tests.el` - 继承测试
- `etaf-css-media-tests.el` - 媒体查询测试
- `etaf-layout-tests.el` - 布局系统测试
- `etaf-layout-buffer-string-tests.el` - 布局字符串生成测试

## 示例

查看 `examples/` 目录获取更多示例：
- `etaf-template-example.el` - Vue.js 风格模板语法示例
- `etaf-tailwind-example.el` - Tailwind CSS 功能示例（新增）
- `etaf-css-example.el` - CSS 功能演示
- `etaf-render-example.el` - 渲染树使用示例
- `etaf-layout-example.el` - 布局系统完整示例
- `etaf-layout-buffer-string-example.el` - 布局字符串生成示例

## 贡献

欢迎贡献代码、报告问题或提出改进建议！

## 许可证

本项目采用 GNU General Public License v3.0 或更高版本。

## 相关资源

- [CSS 规范](https://www.w3.org/Style/CSS/)
- [CSSOM 规范](https://www.w3.org/TR/cssom-1/)
- [CSS 层叠规范](https://www.w3.org/TR/css-cascade/)
- [CSS 盒模型规范](https://www.w3.org/TR/css-box-3/)
