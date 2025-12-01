# ETAF Event Model Documentation

## 概述 (Overview)

ETAF Event Model 是一个用于支持交互式伪类选择器（如 `:hover`, `:active`, `:focus`）的底层事件系统。它结合了浏览器事件模型的设计理念和 Emacs 原生能力来实现状态跟踪和事件分发。

The ETAF Event Model is an underlying event system that supports interactive pseudo-class selectors (like `:hover`, `:active`, `:focus`). It combines browser event model design principles with Emacs native capabilities to implement state tracking and event dispatching.

## 架构设计 (Architecture Design)

### 核心组件 (Core Components)

```
┌─────────────────────────────────────────────────────────────┐
│                    etaf-event.el                            │
│                   Event Model Core                          │
└──────────────┬──────────────────────────┬───────────────────┘
               │                          │
               ▼                          ▼
    ┌──────────────────┐       ┌──────────────────┐
    │ Element Registry │       │ State Manager    │
    │                  │       │                  │
    │ - Registration   │       │ - State Tracking │
    │ - Lifecycle      │       │ - State Changes  │
    │ - Lookup         │       │ - Event Dispatch │
    └──────────────────┘       └──────────────────┘
               │                          │
               └───────────┬──────────────┘
                           ▼
                ┌──────────────────────┐
                │  Event Listeners     │
                │                      │
                │ - Registration       │
                │ - Dispatch           │
                │ - Callbacks          │
                └──────────────────────┘
                           │
                           ▼
                ┌──────────────────────┐
                │ Emacs Integration    │
                │                      │
                │ - Mouse Tracking     │
                │ - Timer System       │
                │ - Text Properties    │
                └──────────────────────┘
```

### 与 CSS 选择器的集成 (CSS Selector Integration)

```
┌────────────────────────────────────────────────────────────┐
│                  etaf-css-selector.el                      │
│               CSS Selector Matching                        │
└────────────────────┬───────────────────────────────────────┘
                     │
                     │ Pseudo-class matching
                     ▼
    ┌────────────────────────────────────────┐
    │ etaf-css-selector-pseudo-match-p       │
    │                                        │
    │ Structural:        Interactive:       │
    │ :first-child  ──┐  :hover       ──┐  │
    │ :nth-child      │  :active        │  │
    │ :empty          │  :focus         │  │
    │                 │  :disabled      │  │
    │                 │                 │  │
    │                 ▼                 ▼  │
    │          Direct Check    Event Model │
    └───────────────────────────────┬──────┘
                                    │
                                    ▼
                    ┌───────────────────────────────┐
                    │ etaf-event-matches-pseudo-    │
                    │ class-p                       │
                    │                               │
                    │ Query element state from      │
                    │ event system                  │
                    └───────────────────────────────┘
```

## 数据结构 (Data Structures)

### Element Record

```elisp
(cl-defstruct etaf-event-element
  "记录一个已注册的交互式元素"
  uuid          ; 唯一标识符
  node          ; DOM 节点
  start         ; Buffer 起始位置
  end           ; Buffer 结束位置
  state         ; 当前状态 plist (:hover :active :focus :disabled, 等)
  listeners)    ; 按类型分组的事件监听器 plist
```

### State Plist

元素状态使用 plist 存储，支持以下状态键：

```elisp
(:hover nil       ; 鼠标悬停状态
 :active nil      ; 激活状态（鼠标按下）
 :focus nil       ; 焦点状态
 :disabled nil    ; 禁用状态
 :enabled t)      ; 启用状态（与 disabled 相反）
```

## API 文档 (API Documentation)

### 初始化和清理 (Initialization and Cleanup)

#### `etaf-event-init (&optional buffer)`

初始化事件系统。

- **参数**: `buffer` - 可选，要初始化的 buffer（默认为当前 buffer）
- **返回值**: 无
- **副作用**: 
  - 创建 `etaf-event--elements` hash table
  - 创建 `etaf-event--listeners` hash table
  - 启动鼠标跟踪定时器

```elisp
(with-current-buffer my-buffer
  (etaf-event-init))
```

#### `etaf-event-cleanup (&optional buffer)`

清理事件系统，释放资源。

- **参数**: `buffer` - 可选，要清理的 buffer（默认为当前 buffer）
- **返回值**: 无

```elisp
(etaf-event-cleanup)
```

### 元素注册 (Element Registration)

#### `etaf-event-register-element (uuid node start end &optional initial-state)`

注册一个元素以便进行事件跟踪。

- **参数**:
  - `uuid` - 唯一标识符字符串
  - `node` - DOM 节点
  - `start` - Buffer 起始位置
  - `end` - Buffer 结束位置
  - `initial-state` - 可选的初始状态 plist
- **返回值**: 无

```elisp
(etaf-event-register-element 
  "btn-123"                           ; UUID
  '(button ((class . "primary")) "Click Me")  ; DOM node
  100 120                             ; Buffer positions
  '(:disabled t :enabled nil))        ; Initial state
```

#### `etaf-event-unregister-element (uuid)`

取消注册一个元素。

```elisp
(etaf-event-unregister-element "btn-123")
```

### 状态管理 (State Management)

#### `etaf-event-get-state (uuid &optional key)`

获取元素的状态。

- **参数**:
  - `uuid` - 元素的唯一标识符
  - `key` - 可选，特定状态键（如 `:hover`）
- **返回值**: 如果提供 `key`，返回该键的值；否则返回完整的状态 plist

```elisp
;; 获取完整状态
(etaf-event-get-state "btn-123")
;; => (:hover nil :active nil :focus t :disabled nil :enabled t)

;; 获取特定状态
(etaf-event-get-state "btn-123" :hover)
;; => nil
```

#### `etaf-event-set-state (uuid state-key value)`

设置元素的状态。

- **参数**:
  - `uuid` - 元素的唯一标识符
  - `state-key` - 状态键（如 `:hover`）
  - `value` - 新值
- **返回值**: 如果状态改变返回 `t`，否则返回 `nil`
- **副作用**: 如果状态改变，会触发 `state-change` 事件

```elisp
(etaf-event-set-state "btn-123" :hover t)
;; => t (状态改变)

(etaf-event-set-state "btn-123" :hover t)
;; => nil (状态未改变)
```

### 事件监听 (Event Listeners)

#### `etaf-event-add-listener (uuid event-type callback)`

为元素添加事件监听器。

- **参数**:
  - `uuid` - 元素的唯一标识符
  - `event-type` - 事件类型符号
  - `callback` - 回调函数 `(lambda (uuid event-data) ...)`
- **返回值**: 回调函数（用于后续删除）

支持的事件类型：
- `state-change` - 状态改变时触发
- `hover-enter` - 鼠标进入时触发
- `hover-leave` - 鼠标离开时触发
- `mouse-down` - 鼠标按下时触发
- `mouse-up` - 鼠标释放时触发
- `focus` - 获得焦点时触发
- `blur` - 失去焦点时触发

```elisp
(etaf-event-add-listener "btn-123" 'hover-enter
  (lambda (uuid data)
    (message "Mouse entered element %s" uuid)))

(etaf-event-add-listener "btn-123" 'state-change
  (lambda (uuid data)
    (message "State changed: %S -> %S" 
             (plist-get data :old-value)
             (plist-get data :new-value))))
```

#### `etaf-event-remove-listener (uuid event-type callback)`

移除事件监听器。

```elisp
(let ((callback (etaf-event-add-listener "btn-123" 'hover-enter 
                  (lambda (uuid data) ...))))
  ;; 稍后移除
  (etaf-event-remove-listener "btn-123" 'hover-enter callback))
```

### 伪类状态检查 (Pseudo-Class State Checking)

#### `etaf-event-matches-pseudo-class-p (uuid pseudo-class)`

检查元素是否匹配指定的伪类。

- **参数**:
  - `uuid` - 元素的唯一标识符
  - `pseudo-class` - 伪类关键字（如 `:hover`）
- **返回值**: 如果匹配返回 `t`，否则返回 `nil`

```elisp
(etaf-event-matches-pseudo-class-p "btn-123" :hover)
;; => t (如果元素正在被悬停)

(etaf-event-matches-pseudo-class-p "btn-123" :disabled)
;; => nil (如果元素未被禁用)
```

## 浏览器兼容性对照 (Browser Compatibility Mapping)

| 浏览器伪类 | ETAF 支持 | Emacs 实现方式 | 说明 |
|-----------|----------|--------------|------|
| `:hover` | ✅ | 定时器 + 像素坐标转换 | 使用 `run-with-timer` 定期检查鼠标像素位置并转换为 buffer 位置 |
| `:active` | ✅ | 鼠标事件 | 通过 `mouse-down` 和 `mouse-up` 事件跟踪 |
| `:focus` | ✅ | 手动管理 | 通过 API 调用设置焦点状态 |
| `:focus-within` | ⚠️ | 需要扩展 | 可以通过遍历父节点实现 |
| `:disabled` | ✅ | 状态属性 | 直接存储在元素状态中 |
| `:enabled` | ✅ | 状态属性 | `disabled` 的反向状态 |
| `:checked` | 🔜 | 计划中 | 可以添加到状态系统 |
| `:target` | ❌ | 不适用 | Emacs buffer 没有 URL fragment 概念 |
| `:visited` | ❌ | 不适用 | 需要外部历史跟踪 |

**实现细节**:
- `:hover` 使用 `mouse-pixel-position` 获取鼠标的屏幕像素坐标
- 通过 `window-at` 找到鼠标所在的窗口
- 使用 `posn-at-x-y` 将窗口内相对坐标转换为 buffer 位置
- 这种方法正确处理了多窗口和分屏情况

## 使用示例 (Usage Examples)

### 基础示例：跟踪按钮悬停状态

```elisp
(require 'etaf-event)
(require 'etaf-css-selector)

;; 初始化事件系统
(etaf-event-init)

;; 创建一个按钮 DOM 节点（需要 uuid 属性）
(let ((button-node '(button ((uuid . "my-btn") 
                              (class . "primary")) 
                      "Click Me")))
  
  ;; 注册元素
  (etaf-event-register-element "my-btn" button-node 100 120)
  
  ;; 添加悬停监听器
  (etaf-event-add-listener "my-btn" 'hover-enter
    (lambda (uuid data)
      (message "Button is now hovered!")))
  
  ;; 添加状态改变监听器
  (etaf-event-add-listener "my-btn" 'state-change
    (lambda (uuid data)
      (when (eq (plist-get data :key) :hover)
        (message "Hover state changed to: %S" 
                 (plist-get data :new-value)))))
  
  ;; 模拟鼠标悬停
  (etaf-event-set-state "my-btn" :hover t)
  
  ;; 使用 CSS 选择器检查
  (when (etaf-css-selector-basic-match-p
         button-node
         (car (plist-get (etaf-css-selector-parse "button:hover") 
                        :nodes)))
    (message "Button matches :hover selector!")))

;; 清理
(etaf-event-cleanup)
```

### 高级示例：表单输入焦点管理

```elisp
(defun my-form-setup ()
  "设置表单输入字段的焦点管理"
  (etaf-event-init)
  
  ;; 注册多个输入字段
  (etaf-event-register-element "input-name" 
    '(input ((uuid . "input-name") (type . "text"))) 
    200 220)
  (etaf-event-register-element "input-email" 
    '(input ((uuid . "input-email") (type . "email"))) 
    230 250)
  
  ;; 添加焦点管理
  (dolist (id '("input-name" "input-email"))
    (etaf-event-add-listener id 'focus
      (lambda (uuid data)
        (message "Field %s gained focus" uuid)))
    (etaf-event-add-listener id 'blur
      (lambda (uuid data)
        (message "Field %s lost focus" uuid))))
  
  ;; 设置初始焦点
  (etaf-event-set-focus "input-name"))

;; 在用户操作时切换焦点
(defun my-move-focus-to-email ()
  (interactive)
  (etaf-event-set-focus "input-email"))
```

### 与渲染系统集成

```elisp
(defun my-render-with-hover ()
  "渲染支持 :hover 样式的元素"
  (etaf-event-init)
  
  (let ((button-node '(button ((uuid . "hover-btn")
                                (class . "fancy"))
                        "Hover Me")))
    ;; 注册元素
    (etaf-event-register-element "hover-btn" button-node 100 120)
    
    ;; 添加状态改变监听器，触发重新渲染
    (etaf-event-add-listener "hover-btn" 'state-change
      (lambda (uuid data)
        (when (eq (plist-get data :key) :hover)
          ;; 重新计算样式并更新显示
          (my-rerender-element uuid))))))

(defun my-rerender-element (uuid)
  "根据当前状态重新渲染元素"
  (let* ((element (etaf-event-get-element uuid))
         (node (etaf-event-element-node element))
         ;; 检查是否匹配 :hover 伪类
         (has-hover (etaf-css-selector-basic-match-p
                     node
                     (car (plist-get 
                           (etaf-css-selector-parse "button:hover")
                           :nodes))))
         ;; 根据状态选择样式...
         ))
```

## 性能考虑 (Performance Considerations)

### 鼠标跟踪优化

- 使用 `run-with-timer` 进行定期轮询（而非 idle timer）以保证响应性
- 默认延迟为 0.1 秒，可通过 `etaf-event-hover-delay` 自定义
- 使用 `mouse-pixel-position` 和 `posn-at-x-y` 进行准确的位置检测
- 正确处理多窗口场景，只跟踪鼠标实际所在窗口的 buffer

### 状态更新优化

- 只在状态实际改变时触发事件
- 使用 hash table 进行 O(1) 元素查找
- 批量状态更新使用 `etaf-event-update-states`

### 内存管理

- 及时调用 `etaf-event-cleanup` 清理不再使用的 buffer
- 使用 `etaf-event-unregister-element` 移除不再需要的元素

## 调试工具 (Debugging Tools)

### `etaf-event-debug-info ()`

返回当前事件系统的调试信息。

```elisp
(etaf-event-debug-info)
;; => (:element-count 5
;;     :hover-element "btn-123"
;;     :active-element nil
;;     :focus-element "input-name"
;;     :tracking-active t)
```

### `etaf-event-get-all-elements ()`

获取所有已注册元素的 UUID 列表。

```elisp
(etaf-event-get-all-elements)
;; => ("btn-123" "input-name" "input-email" "link-456")
```

## 限制和未来工作 (Limitations and Future Work)

### 当前限制

1. **鼠标跟踪精度**: 基于定时器的跟踪有 0.1 秒延迟（可配置）
2. **焦点管理**: 需要手动调用 API，不能自动检测  
3. **没有捕获/冒泡阶段**: 与浏览器事件模型不同，所有事件都是直接分发的
4. **Buffer 本地**: 事件系统是 buffer-local 的，不跨 buffer 工作

### 计划中的功能

1. **自动焦点跟踪**: 基于光标位置自动设置焦点
2. **事件冒泡**: 支持事件从子元素传播到父元素
3. **更多伪类**: `:checked`, `:valid`, `:invalid` 等
4. **手势支持**: 长按、双击等
5. **键盘事件**: 键盘导航支持

## 相关模块 (Related Modules)

- `etaf-css-selector.el` - CSS 选择器解析和匹配
- `etaf-render.el` - 渲染系统（未来将集成状态变化响应）
- `etaf-dom.el` - DOM 操作
- `etaf-layout.el` - 布局系统

## 参考资源 (References)

- [CSS Pseudo-classes - MDN](https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-classes)
- [DOM Events - W3C](https://www.w3.org/TR/DOM-Level-3-Events/)
- [Emacs Lisp Reference - Text Properties](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Properties.html)
- [Emacs Lisp Reference - Timers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html)
