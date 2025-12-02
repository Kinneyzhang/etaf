# ETAF 组件系统

*参考 Vue 3 设计的 ETAF 组件系统*

## 目录

- [概述](#概述)
- [快速开始](#快速开始)
- [组件基础](#组件基础)
- [Props（属性）](#props属性)
- [Setup 函数](#setup-函数)
- [模板](#模板)
- [响应式系统](#响应式系统)
  - [ref（响应式引用）](#ref响应式引用)
  - [computed（计算属性）](#computed计算属性)
  - [watch（侦听器）](#watch侦听器)
  - [watchEffect（自动侦听）](#watcheffect自动侦听)
  - [reactive（响应式对象）](#reactive响应式对象)
- [插槽（Slots）](#插槽slots)
- [组件生命周期](#组件生命周期)
- [与 Vue 3 的对比](#与-vue-3-的对比)
- [API 参考](#api-参考)
- [示例](#示例)

## 概述

ETAF 的组件系统借鉴了 Vue 3 的组合式 API（Composition API），为 Emacs 带来了响应式组件。它提供：

- **声明式组件** - 定义可复用的 UI 组件，支持属性和状态
- **响应式状态管理** - 自动依赖追踪和更新
- **组合式 API** - 灵活可组合的逻辑组织方式
- **类型安全** - 结构化的组件定义，契约清晰

组件系统建立在三大支柱之上：

1. **组件定义** - 使用 `etaf-define-component` 宏
2. **响应式系统** - ref、computed、watch 用于状态管理
3. **模板渲染** - 与 ETAF 的 TML 模板系统集成

## 快速开始

下面是一个简单的计数器组件：

```elisp
(require 'etaf-component)

;; 定义计数器组件
(etaf-define-component my-counter
  :props '(:initial-count)
  :setup (lambda (props)
           (let* ((count (etaf-ref 
                          (or (plist-get props :initial-count) 0)))
                  (increment (lambda ()
                               (etaf-ref-set count 
                                 (1+ (etaf-ref-get count))))))
             (list :count count
                   :increment increment)))
  :template (lambda (data)
              `(div :class "counter"
                    (button :on-click ,(plist-get data :increment)
                            "计数: " 
                            ,(format "%d" (etaf-ref-get 
                                           (plist-get data :count)))))))

;; 使用组件
(etaf-paint-to-buffer "*demo*"
  '(my-counter :initial-count 5))
```

## 组件基础

### 定义组件

组件使用 `etaf-define-component` 宏定义：

```elisp
(etaf-define-component 组件名
  :props '(:属性1 :属性2)
  :setup setup-函数
  :template 模板函数)
```

### 组件结构

组件定义包含：

- **name（名称）** - 标识组件的符号
- **:props** - 组件接受的属性列表
- **:setup** - 设置响应式状态和方法的函数
- **:template** - 用于渲染的模板（函数或 s-表达式）
- **:emits**（可选）- 组件可以发出的事件列表

### 使用组件

定义后，组件可以像普通 HTML 元素一样使用：

```elisp
;; 简单使用
(my-component)

;; 带属性
(my-component :title "Hello" :count 42)

;; 带子元素（插槽）
(my-component :title "容器"
  (p "子元素 1")
  (p "子元素 2"))
```

## Props（属性）

Props 是从父组件向子组件传递数据的方式。

### 声明 Props

```elisp
(etaf-define-component user-card
  :props '(:name :email :avatar)
  :template (lambda (data)
              `(div :class "user-card"
                    (img :src ,(plist-get data :avatar))
                    (h3 ,(plist-get data :name))
                    (p ,(plist-get data :email)))))
```

### 使用 Props

Props 会自动提取并传递给 setup 函数和模板：

```elisp
;; 父组件使用：
(user-card :name "张三" 
           :email "zhangsan@example.com"
           :avatar "/images/zhangsan.png")
```

### 高级 Props（未来特性）

未来版本可能支持 prop 验证：

```elisp
:props '((:name :type string :required t)
         (:count :type number :default 0)
         (:disabled :type boolean))
```

## Setup 函数

Setup 函数是定义组件响应式状态和方法的地方。它在组件创建时运行一次。

### 基本 Setup

```elisp
:setup (lambda (props)
         (let* ((count (etaf-ref 0))
                (increment (lambda ()
                            (etaf-ref-update count #'1+))))
           ;; 返回模板使用的数据和方法
           (list :count count
                 :increment increment)))
```

### Setup 返回值

Setup 函数返回一个 plist，包含：
- **响应式引用** - 使用 `etaf-ref` 创建
- **计算属性** - 使用 `etaf-computed` 创建
- **方法** - Lambda 函数
- **静态数据** - 普通值

### 访问 Props

Props 作为第一个参数传入：

```elisp
:setup (lambda (props)
         (let ((initial (plist-get props :initial-value)))
           (list :value (etaf-ref initial))))
```

## 模板

模板定义组件如何渲染。可以是静态的或动态的。

### 函数模板

最常用 - 接收数据并返回 ETML 的函数：

```elisp
:template (lambda (data)
            `(div :class "widget"
                  (h2 "计数: " ,(etaf-ref-get (plist-get data :count)))
                  (button :on-click ,(plist-get data :increment)
                          "增加")))
```

### 静态模板

用于没有动态数据的简单组件：

```elisp
:template '(div :class "static-component"
                (p "这永远不会改变"))
```

### 模板上下文

模板可以访问：
- 从 setup 返回的所有数据
- Props（合并到 data 中）
- 特殊的 `$slots` prop（子元素）

## 响应式系统

ETAF 的响应式系统自动追踪依赖关系，并在数据变化时触发更新。

### ref（响应式引用）

创建对值的响应式引用：

```elisp
(let ((count (etaf-ref 0)))
  ;; 读取值
  (etaf-ref-get count)  ; => 0
  
  ;; 设置值（触发更新）
  (etaf-ref-set count 5)
  
  ;; 使用函数更新
  (etaf-ref-update count #'1+))
```

**要点：**
- 使用 `etaf-ref` 创建响应式引用
- 使用 `etaf-ref-get` 读取（追踪依赖）
- 使用 `etaf-ref-set` 写入（触发更新）
- 使用 `etaf-ref-update` 进行函数式更新

### computed（计算属性）

创建派生的响应式值：

```elisp
(let* ((price (etaf-ref 100))
       (quantity (etaf-ref 2))
       (total (etaf-computed
               (lambda ()
                 (* (etaf-ref-get price)
                    (etaf-ref-get quantity))))))
  
  (etaf-computed-get total)  ; => 200
  
  ;; 改变依赖
  (etaf-ref-set quantity 3)
  
  ;; 自动重新计算
  (etaf-computed-get total)  ; => 300
)
```

**特性：**
- 惰性求值 - 只在访问时计算
- 自动缓存 - 只在依赖改变时重新计算
- 自动依赖追踪
- 可以依赖其他计算属性

### watch（侦听器）

侦听特定的响应式源，并在其改变时运行回调：

```elisp
(let* ((count (etaf-ref 0))
       (stop (etaf-watch
              count
              (lambda (new-val old-val)
                (message "计数改变: %s -> %s" old-val new-val)))))
  
  (etaf-ref-set count 1)  ; 输出: "计数改变: 0 -> 1"
  (etaf-ref-set count 2)  ; 输出: "计数改变: 1 -> 2"
  
  ;; 停止侦听
  (funcall stop)
  
  (etaf-ref-set count 3)  ; 无输出
)
```

**选项：**
```elisp
(etaf-watch source callback 
  '(:immediate t))  ; 立即用当前值运行回调
```

### watchEffect（自动侦听）

自动追踪依赖并在其改变时运行 effect：

```elisp
(let* ((firstName (etaf-ref "张"))
       (lastName (etaf-ref "三"))
       (stop (etaf-watch-effect
              (lambda ()
                (message "姓名: %s%s"
                        (etaf-ref-get firstName)
                        (etaf-ref-get lastName))))))
  
  ;; 立即输出: "姓名: 张三"
  
  (etaf-ref-set firstName "李")  ; 输出: "姓名: 李三"
  (etaf-ref-set lastName "四")   ; 输出: "姓名: 李四"
  
  ;; 停止 effect
  (funcall stop)
)
```

**与 watch 的主要区别：**
- 立即运行
- 自动追踪所有访问的 refs
- 无需显式指定源
- 最常用于副作用

### reactive（响应式对象）

从 plist 创建响应式对象：

```elisp
(let ((user (etaf-reactive '(:name "张三" :age 30))))
  
  ;; 读取值
  (etaf-reactive-get user :name)  ; => "张三"
  
  ;; 更新值
  (etaf-reactive-set user :name "李四")
  (etaf-reactive-set user :age 31)
  
  ;; 侦听变化
  (etaf-watch-effect
   (lambda ()
     (message "用户: %s, 年龄: %d"
             (etaf-reactive-get user :name)
             (etaf-reactive-get user :age))))
  
  ;; 转换回 plist
  (etaf-reactive-to-plist user)  ; => (:name "李四" :age 31)
)
```

**何时使用：**
- 多个相关属性
- 类对象数据结构
- 需要动态添加属性

## 插槽（Slots）

插槽允许组件接受子元素。

### 基本插槽

子元素通过特殊的 `:$slots` prop 传递：

```elisp
(etaf-define-component card
  :props '(:title)
  :template (lambda (data)
              (let ((title (plist-get data :title))
                    (slots (plist-get data :$slots)))
                `(div :class "card"
                      (h2 :class "card-title" ,title)
                      (div :class "card-body"
                           ,@slots)))))

;; 使用插槽
(card :title "我的卡片"
  (p "这是卡片内容")
  (button "操作"))
```

### 检查插槽

```elisp
:template (lambda (data)
            (let ((slots (plist-get data :$slots)))
              (if slots
                  `(div ,@slots)
                `(div (p "未提供内容")))))
```

## 组件生命周期

组件与 ETAF 的虚拟 DOM 生命周期钩子集成：

### Mounted 钩子

组件首次渲染时调用：

```elisp
:setup (lambda (props)
         (let ((data (etaf-ref nil)))
           ;; 组件挂载时获取数据
           (etaf-watch-effect
            (lambda ()
              ;; 这在挂载时运行，并在依赖改变时重新运行
              (setq data (fetch-data))))
           (list :data data)))
```

### Update 钩子

使用 `watch` 或 `watchEffect` 响应变化：

```elisp
:setup (lambda (props)
         (let ((count (etaf-ref 0)))
           ;; 侦听变化
           (etaf-watch count
             (lambda (new-val old-val)
               (message "计数更新: %s" new-val)))
           (list :count count)))
```

### 清理

Watch 和 watchEffect 返回停止函数用于清理：

```elisp
:setup (lambda (props)
         (let* ((count (etaf-ref 0))
                (stop (etaf-watch-effect
                       (lambda ()
                         ;; Effect 逻辑
                         ))))
           ;; 如需要可保存停止函数
           (list :count count
                 :cleanup stop)))
```

## 与 Vue 3 的对比

ETAF 的组件系统受 Vue 3 的组合式 API 启发：

| 特性 | Vue 3 | ETAF |
|------|-------|------|
| 组件定义 | `defineComponent()` | `etaf-define-component` |
| 响应式引用 | `ref()` | `etaf-ref` |
| 计算属性 | `computed()` | `etaf-computed` |
| 侦听器 | `watch()` | `etaf-watch` |
| 自动侦听 | `watchEffect()` | `etaf-watch-effect` |
| 响应式对象 | `reactive()` | `etaf-reactive` |
| Props | `props` 选项 | `:props` 关键字 |
| Setup | `setup()` | `:setup` 关键字 |
| 模板 | `template` 选项 | `:template` 关键字 |
| 插槽 | `<slot>` | `:$slots` prop |

### 主要区别

1. **语言** - Vue 使用 JavaScript，ETAF 使用 Emacs Lisp
2. **模板** - Vue 使用类 HTML 语法，ETAF 使用 S-表达式
3. **数据结构** - ETAF 使用 plists 而不是 JavaScript 对象
4. **渲染** - ETAF 渲染到文本 buffer，而非 DOM

### 设计原则（来自 Vue 3）

两个系统共享这些原则：

- **组合优于继承**
- **显式优于隐式**
- **灵活且可组合**
- **自动依赖追踪**

## API 参考

### 组件管理

#### `etaf-define-component`
```elisp
(etaf-define-component name &rest options)
```
定义组件。返回组件名称。

**选项：**
- `:props` - prop 名称列表
- `:setup` - Setup 函数 (props) → data plist
- `:template` - 模板函数或 s-表达式
- `:emits` - 发出的事件列表

#### `etaf-component-get`
```elisp
(etaf-component-get name)
```
按名称获取组件定义。

#### `etaf-component-defined-p`
```elisp
(etaf-component-defined-p name)
```
检查组件是否已注册。

#### `etaf-component-list-all`
```elisp
(etaf-component-list-all)
```
列出所有已注册的组件名称。

### 响应式系统

#### `etaf-ref`
```elisp
(etaf-ref initial-value)
```
创建响应式引用。

#### `etaf-ref-get`
```elisp
(etaf-ref-get ref)
```
获取 ref 的当前值。

#### `etaf-ref-set`
```elisp
(etaf-ref-set ref value)
```
设置新值并触发更新。

#### `etaf-ref-update`
```elisp
(etaf-ref-update ref update-fn)
```
通过对当前值应用函数来更新 ref。

#### `etaf-computed`
```elisp
(etaf-computed getter-fn)
```
创建计算属性。

#### `etaf-computed-get`
```elisp
(etaf-computed-get computed)
```
获取计算值（如果脏则重新计算）。

#### `etaf-watch`
```elisp
(etaf-watch source callback &optional options)
```
侦听 ref 或计算属性。返回停止函数。

**选项：**
- `:immediate` - 立即运行回调

#### `etaf-watch-effect`
```elisp
(etaf-watch-effect effect-fn)
```
运行 effect 并在依赖改变时重新运行。返回停止函数。

#### `etaf-reactive`
```elisp
(etaf-reactive plist)
```
创建响应式对象。

#### `etaf-reactive-get`
```elisp
(etaf-reactive-get reactive key)
```
从响应式对象获取值。

#### `etaf-reactive-set`
```elisp
(etaf-reactive-set reactive key value)
```
在响应式对象中设置值。

#### `etaf-reactive-to-plist`
```elisp
(etaf-reactive-to-plist reactive)
```
将响应式对象转换为普通 plist。

## 示例

### 示例 1：简单按钮

```elisp
(etaf-define-component simple-button
  :props '(:label :variant)
  :template (lambda (data)
              (let ((label (plist-get data :label))
                    (variant (or (plist-get data :variant) "primary")))
                `(button :class ,(format "btn btn-%s" variant)
                         ,label))))

;; 使用
(simple-button :label "点击我" :variant "success")
```

### 示例 2：带状态的计数器

```elisp
(etaf-define-component counter
  :props '(:initial)
  :setup (lambda (props)
           (let* ((count (etaf-ref (or (plist-get props :initial) 0)))
                  (increment (lambda ()
                               (etaf-ref-update count #'1+)))
                  (decrement (lambda ()
                               (etaf-ref-update count #'1-)))
                  (reset (lambda ()
                          (etaf-ref-set count 0))))
             (list :count count
                   :increment increment
                   :decrement decrement
                   :reset reset)))
  :template (lambda (data)
              `(div :class "counter"
                    (button :on-click ,(plist-get data :decrement) "-")
                    (span :class "count" 
                          ,(format "%d" (etaf-ref-get (plist-get data :count))))
                    (button :on-click ,(plist-get data :increment) "+")
                    (button :on-click ,(plist-get data :reset) "重置"))))

;; 使用
(counter :initial 10)
```

### 示例 3：待办事项列表

```elisp
(etaf-define-component todo-list
  :props '(:initial-items)
  :setup (lambda (props)
           (let* ((items (etaf-ref (or (plist-get props :initial-items) '())))
                  (new-text (etaf-ref ""))
                  (add-item (lambda ()
                              (let ((text (etaf-ref-get new-text)))
                                (when (not (string-empty-p text))
                                  (etaf-ref-set items
                                    (append (etaf-ref-get items)
                                           (list (list :text text :done nil))))
                                  (etaf-ref-set new-text "")))))
                  (toggle-item (lambda (index)
                                 (let* ((current (etaf-ref-get items))
                                        (item (nth index current))
                                        (updated (plist-put (copy-sequence item)
                                                           :done
                                                           (not (plist-get item :done)))))
                                   (setf (nth index current) updated)
                                   (etaf-ref-set items current))))
                  (remaining (etaf-computed
                              (lambda ()
                                (length (seq-filter
                                        (lambda (item)
                                          (not (plist-get item :done)))
                                        (etaf-ref-get items)))))))
             (list :items items
                   :new-text new-text
                   :add-item add-item
                   :toggle-item toggle-item
                   :remaining remaining)))
  :template (lambda (data)
              (let ((items (etaf-ref-get (plist-get data :items))))
                `(div :class "todo-list"
                      (div :class "todo-input"
                           (input :type "text"
                                  :placeholder "需要做什么？")
                           (button :on-click ,(plist-get data :add-item)
                                   "添加"))
                      (ul :class "todo-items"
                          ,@(cl-loop for item in items
                                    for index from 0
                                    collect
                                    `(li :class ,(if (plist-get item :done)
                                                    "done" "")
                                         (input :type "checkbox"
                                                :checked ,(plist-get item :done)
                                                :on-change (lambda ()
                                                            (funcall ,(plist-get data :toggle-item)
                                                                    ,index)))
                                         (span ,(plist-get item :text)))))
                      (div :class "todo-footer"
                           ,(format "剩余 %d 项"
                                   (etaf-computed-get (plist-get data :remaining))))))))

;; 使用
(todo-list :initial-items '((:text "学习 ETAF" :done nil)
                            (:text "构建应用" :done nil)))
```

## 最佳实践

### 1. 保持组件小巧

将复杂的 UI 分解为更小的可复用组件：

```elisp
;; 好
(etaf-define-component user-card ...)
(etaf-define-component user-avatar ...)
(etaf-define-component user-bio ...)

;; 而不是一个大组件
```

### 2. 使用 Computed 处理派生状态

不要重复状态，而是计算它：

```elisp
;; 好
(let ((items (etaf-ref '(...)))
      (active-items (etaf-computed
                     (lambda ()
                       (seq-filter #'is-active (etaf-ref-get items))))))
  ...)

;; 避免
(let ((items (etaf-ref '(...)))
      (active-items (etaf-ref '(...))))  ; 必须手动保持同步
  ...)
```

### 3. 使用 watchEffect 处理副作用

当不需要旧值时，优先使用 `watchEffect` 而非 `watch`：

```elisp
;; 好 - 自动依赖追踪
(etaf-watch-effect
 (lambda ()
   (message "用户: %s" (etaf-ref-get name))))

;; 冗长 - 显式依赖
(etaf-watch name
  (lambda (new old)
    (message "用户: %s" new)))
```

### 4. 清晰地命名 Props

使用描述性的 prop 名称：

```elisp
;; 好
:props '(:user-name :user-email :is-admin)

;; 避免
:props '(:name :email :flag)
```

### 5. 为复杂组件编写文档

为 setup 函数添加文档字符串：

```elisp
:setup (lambda (props)
         "TodoList 组件的 setup。
         管理待办事项列表，支持添加/删除/切换操作。"
         ...)
```

## 故障排除

### 组件未找到

**问题：** 错误："组件未注册"

**解决：** 确保在使用前定义组件：

```elisp
;; 先定义
(etaf-define-component my-component ...)

;; 然后使用
(my-component)
```

### 响应式值未更新

**问题：** 当 ref 改变时 UI 不更新

**解决：** 确保使用 `etaf-ref-set` 而非直接修改：

```elisp
;; 好
(etaf-ref-set count (1+ (etaf-ref-get count)))

;; 坏 - 不会触发更新
(plist-put count :value (1+ (plist-get count :value)))
```

### Computed 未重新计算

**问题：** 计算属性显示过期数据

**解决：** 确保使用 `etaf-ref-get` 读取依赖：

```elisp
;; 好 - 追踪依赖
(etaf-computed
 (lambda ()
   (etaf-ref-get my-ref)))

;; 坏 - 不追踪
(etaf-computed
 (lambda ()
   my-ref))  ; 返回 ref 对象，而非值
```

## 迁移指南

### 从 etaf-etml-* 到 etaf-*

组件系统从 `etaf-etml.el` 提取到 `etaf-component.el`。
**注意：向后兼容别名已被移除。** 您必须更新代码以使用新的函数名称。

### 更新代码

更新现有代码：

1. 更改 requires：
   ```elisp
   ;; 添加这个
   (require 'etaf-component)
   ```

2. 更新函数名称（必须）：
   ```elisp
   ;; 查找/替换 - 旧名称不再有效
   etaf-etml-define-component → etaf-define-component
   etaf-etml-ref → etaf-ref
   etaf-etml-computed → etaf-computed
   etaf-etml-watch-source → etaf-watch
   etaf-etml-watch-effect → etaf-watch-effect
   etaf-etml-reactive → etaf-reactive
   etaf-etml-ref-get → etaf-ref-get
   etaf-etml-ref-set → etaf-ref-set
   etaf-etml-ref-update → etaf-ref-update
   etaf-etml-computed-get → etaf-computed-get
   etaf-etml-reactive-get → etaf-reactive-get
   etaf-etml-reactive-set → etaf-reactive-set
   etaf-etml-reactive-to-plist → etaf-reactive-to-plist
   etaf-etml-component-* → etaf-component-*
   ```

3. 更新所有函数名称后测试代码

## 延伸阅读

- [Vue 3 组合式 API](https://cn.vuejs.org/guide/extras/composition-api-faq.html)
- [Vue 3 深入响应式系统](https://cn.vuejs.org/guide/extras/reactivity-in-depth.html)
- [ETAF 虚拟 DOM](VIRTUAL-DOM.md)
- [ETAF 事件模型](EVENT-MODEL.md)
- [组件示例](../examples/etaf-component-examples.el)

---

*如有问题，请访问 [ETAF GitHub 仓库](https://github.com/Kinneyzhang/etaf)。*
