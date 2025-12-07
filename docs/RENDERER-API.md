# ETAF Renderer API Documentation

## 概述 (Overview)

本文档描述 ETAF 的 Vue 3 风格渲染器 API，包括虚拟节点的挂载、卸载和更新（diff 算法）。

This document describes ETAF's Vue 3-style renderer API, including virtual node mounting, unmounting, and updating (diff algorithm).

## 核心概念 (Core Concepts)

### VNode (虚拟节点)

VNode 是描述 UI 的纯数据结构，遵循 Vue 3 标准：

```elisp
(:type element                    ; 节点类型
 :tag div                         ; 标签名
 :props (:class "box" :on-click fn) ; 属性（包括事件处理器）
 :children (child-vnode1 ...)     ; 子节点
 :key "unique-key"                ; diff 优化的 key
 :patchFlag 3                     ; 优化标志
 :shapeFlag 1                     ; 节点形状标志
 :el (start . end))               ; 挂载后的 buffer 区域
```

### 渲染管道 (Render Pipeline)

```
VNode → DOM → Render Tree → Layout Tree → Buffer String
         ↓
    Event Handlers Preserved
```

## API 函数 (API Functions)

### 1. VNode 创建 (VNode Creation)

#### `etaf-create-vnode`

创建一个 VNode。

```elisp
(etaf-create-vnode type props children &optional patchFlag dynamicProps)
```

**参数：**
- `type`: 元素类型（symbol），如 `'div`, `'span`, `'button`
- `props`: 属性 plist，如 `(:class "box" :on-click fn)`
- `children`: 子节点（字符串、VNode 或 VNode 列表）
- `patchFlag`: 可选，优化标志
- `dynamicProps`: 可选，动态属性名列表

**示例：**

```elisp
;; 简单的 div
(etaf-create-vnode 'div
  '(:class "container")
  (list (etaf-vdom-text "Hello")))

;; 带事件处理器的按钮
(etaf-create-vnode 'button
  '(:on-click (lambda () (message "Clicked!"))
    :class "btn-primary")
  (list (etaf-vdom-text "Click Me")))
```

#### `etaf-vdom-text`

创建文本 VNode。

```elisp
(etaf-vdom-text content)
```

#### `etaf-vdom-fragment`

创建片段 VNode（多个根节点）。

```elisp
(etaf-vdom-fragment children &optional patchFlag)
```

### 2. 挂载 (Mounting)

#### `etaf-vdom-mount`

将 VNode 挂载到 buffer。

```elisp
(etaf-vdom-mount vnode container)
```

**参数：**
- `vnode`: 要挂载的 VNode 树
- `container`: buffer 对象或 buffer 名称

**返回：** 挂载后的 VNode（`:el` 属性被设置）

**功能：**
1. 将 VNode 渲染为 DOM
2. 通过布局系统转换为 buffer 字符串
3. 插入到 buffer
4. 设置事件处理器
5. 存储 VNode 用于后续更新

**示例：**

```elisp
(with-current-buffer (get-buffer-create "*Demo*")
  (let ((vnode (etaf-create-vnode 'div
                 '(:class "demo")
                 (list (etaf-vdom-text "Hello World")))))
    (etaf-vdom-mount vnode (current-buffer))))
```

### 3. 卸载 (Unmounting)

#### `etaf-vdom-unmount`

从 buffer 卸载 VNode。

```elisp
(etaf-vdom-unmount container)
```

**参数：**
- `container`: 挂载时使用的 buffer 对象或名称

**返回：** `t` 成功，`nil` 无内容可卸载

**功能：**
1. 清理事件处理器
2. 从 buffer 删除内容
3. 清理存储的 VNode 引用

**示例：**

```elisp
(etaf-vdom-unmount (current-buffer))
```

### 4. 更新/补丁 (Patching/Diffing)

#### `etaf-vdom-patch`

使用 diff 算法更新 VNode。

```elisp
(etaf-vdom-patch old-vnode new-vnode container)
```

**参数：**
- `old-vnode`: 旧的 VNode
- `new-vnode`: 新的 VNode
- `container`: buffer 对象或名称

**返回：** 更新后的 VNode

**Diff 算法：**
1. 检查节点类型是否相同（type + key）
2. 相同类型：更新属性和子节点
3. 不同类型：替换整个子树
4. 使用 patchFlags 优化

**示例：**

```elisp
(let* ((old-vnode (etaf-create-vnode 'div nil 
                    (list (etaf-vdom-text "Old"))))
       (new-vnode (etaf-create-vnode 'div nil 
                    (list (etaf-vdom-text "New")))))
  ;; 先挂载
  (etaf-vdom-mount old-vnode (current-buffer))
  ;; 然后更新
  (etaf-vdom-patch old-vnode new-vnode (current-buffer)))
```

### 5. 事件处理 (Event Handling)

#### 事件处理器绑定

在 VNode props 中使用 `:on-*` 属性：

```elisp
(etaf-create-vnode 'button
  '(:on-click (lambda () (message "Clicked!"))
    :on-hover-enter (lambda () (message "Hover enter"))
    :on-hover-leave (lambda () (message "Hover leave")))
  (list (etaf-vdom-text "Interactive Button")))
```

**支持的事件：**
- `:on-click` - 点击事件
- `:on-hover-enter` - 鼠标进入
- `:on-hover-leave` - 鼠标离开
- `:on-keydown` - 按键按下
- `:on-focus` - 获得焦点
- `:on-blur` - 失去焦点

#### 事件冒泡

事件会从目标元素向上冒泡到父元素：

```elisp
(let ((child (etaf-create-vnode 'button
               '(:on-click (lambda () (message "Child clicked"))
                 :uuid "child-1")
               (list (etaf-vdom-text "Child"))))
      (parent (etaf-create-vnode 'div
                '(:on-click (lambda () (message "Parent clicked"))
                  :uuid "parent-1")
                (list child))))
  ;; 设置父子关系
  (plist-put child :parent parent)
  ;; 点击 child 时，两个处理器都会被调用
  (etaf-vdom-mount parent (current-buffer)))
```

使用 `etaf-event-dispatch-with-bubbling` 显式触发冒泡事件。

## 优化标志 (Optimization Flags)

### PatchFlags

用于优化 diff 算法，指示哪些部分是动态的：

```elisp
etaf-patch-flag-text        ; 1   - 动态文本内容
etaf-patch-flag-class       ; 2   - 动态 class
etaf-patch-flag-style       ; 4   - 动态 style
etaf-patch-flag-props       ; 8   - 动态属性
etaf-patch-flag-hydrate-events ; 32 - 有事件监听器
etaf-patch-flag-hoisted     ; -1  - 静态节点
```

**示例：**

```elisp
;; 创建带优化标志的 VNode
(etaf-create-vnode 'div
  '(:class "dynamic")
  (list (etaf-vdom-text "Text"))
  etaf-patch-flag-class)  ; 标记 class 是动态的
```

## 完整示例 (Complete Examples)

### 示例 1：简单计数器

```elisp
(defvar my-count 0)
(defvar my-vnode nil)

(defun create-counter-vnode (count)
  (etaf-create-vnode 'div nil
    (list
      (etaf-create-vnode 'p nil
        (list (etaf-vdom-text (format "Count: %d" count))))
      (etaf-create-vnode 'button
        '(:on-click increment-counter)
        (list (etaf-vdom-text "Increment"))))))

(defun increment-counter ()
  (setq my-count (1+ my-count))
  (let ((new-vnode (create-counter-vnode my-count)))
    (etaf-vdom-patch my-vnode new-vnode (current-buffer))
    (setq my-vnode new-vnode)))

;; 初始化
(with-current-buffer (get-buffer-create "*Counter*")
  (setq my-vnode (create-counter-vnode 0))
  (etaf-vdom-mount my-vnode (current-buffer))
  (switch-to-buffer (current-buffer)))
```

### 示例 2：待办事项列表

```elisp
(defvar my-tasks '())

(defun create-task-vnode (task)
  (etaf-create-vnode 'li nil
    (list
      (etaf-vdom-text (plist-get task :text))
      (etaf-vdom-text " ")
      (etaf-create-vnode 'button
        `(:on-click ,(lambda () (delete-task (plist-get task :id))))
        (list (etaf-vdom-text "Delete"))))))

(defun create-todo-vnode (tasks)
  (etaf-create-vnode 'div nil
    (list
      (etaf-create-vnode 'h1 nil
        (list (etaf-vdom-text "Todo List")))
      (etaf-create-vnode 'ul nil
        (mapcar #'create-task-vnode tasks))
      (etaf-create-vnode 'button
        '(:on-click add-task)
        (list (etaf-vdom-text "Add Task"))))))
```

## 测试 (Testing)

### 运行简单测试

```bash
cd tests
emacs -batch -l test-renderer-simple.el
```

### 运行完整测试套件

```bash
cd tests
emacs -batch -l etaf-ert.el -l etaf-vdom-renderer-tests.el -f ert-run-tests-batch-and-exit
```

### 运行交互式演示

```elisp
(load-file "examples/etaf-renderer-demo.el")
(etaf-renderer-demo)
```

## 注意事项 (Notes)

1. **事件处理器必须通过渲染管道传递**：VNode 中的 `:on-*` 属性会被保留为 `etaf-event-handlers`，确保在布局字符串生成时可以正确绑定。

2. **Key 用于优化**：为列表项提供 `:key` 可以提高 diff 性能：
   ```elisp
   (etaf-create-vnode 'li '(:key "item-1") ...)
   ```

3. **内存管理**：使用 `etaf-vdom-unmount` 清理不再需要的内容，避免内存泄漏。

4. **静态优化**：对于静态内容，使用 `etaf-vdom-static` 创建永不更新的节点。

## 相关文档 (Related Documentation)

- [Virtual DOM Architecture](VIRTUAL-DOM.md)
- [Event Model](EVENT-MODEL.md)
- [Component System](COMPONENT-SYSTEM.md)
- [Vue 3 VNode Specification](https://github.com/vuejs/core/blob/main/packages/runtime-core/src/vnode.ts)

## 贡献 (Contributing)

欢迎提交 issue 和 pull request！

Welcome to submit issues and pull requests!
