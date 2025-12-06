# ETAF 组件系统示例文档

本文档详细说明 `examples/etaf-component-examples.el` 中的所有组件示例。

## 目录

- [概述](#概述)
- [Vue 3 组合式 API 示例](#vue-3-组合式-api-示例)
  - [1. 基础计数器 - ref](#1-基础计数器---ref)
  - [2. 购物车 - computed](#2-购物车---computed)
  - [3. 用户资料 - watch](#3-用户资料---watch)
  - [4. 日志查看器 - watchEffect](#4-日志查看器---watcheffect)
  - [5. 待办事项 - reactive](#5-待办事项---reactive)
- [Vue 2 选项式 API 示例](#vue-2-选项式-api-示例)
  - [6. 选项式计数器 - data & methods](#6-选项式计数器---data--methods)
  - [7. 计算器 - computed](#7-计算器---computed)
  - [8. 表单验证 - watch](#8-表单验证---watch)
  - [9. 系统仪表板 - 综合示例](#9-系统仪表板---综合示例)
- [使用方法](#使用方法)
- [学习建议](#学习建议)

## 概述

本示例文件展示了 ETAF 组件系统的两种 API 风格：

1. **Vue 3 组合式 API (Composition API)**: 使用 `setup` 函数和响应式工具函数（ref、computed、watch、watchEffect）
2. **Vue 2 选项式 API (Options API)**: 使用 data、methods、computed、watch 等选项

每个示例都专注于展示响应式系统的特定功能，从简单到复杂，帮助您全面理解 ETAF 的响应式能力。

## Vue 3 组合式 API 示例

### 1. 基础计数器 - ref

**组件名**: `composition-counter`

**演示功能**: 
- 使用 `etaf-ref` 创建响应式引用
- 使用 `etaf-ref-get` 读取值
- 使用 `etaf-ref-set` 和 `etaf-ref-update` 修改值

**关键代码**:
```elisp
(let* ((count (etaf-ref initial))
       (increment (lambda ()
                   (etaf-ref-update count (lambda (val) (+ val step))))))
  (list :count count :increment increment))
```

**学习要点**:
- `etaf-ref` 创建一个响应式容器，包装基础值
- 修改 ref 的值会自动触发依赖它的计算属性和监听器
- `etaf-ref-update` 是修改值的便捷方法，接收一个转换函数

**Props**:
- `:initial-count` - 初始计数值（默认 0）
- `:step` - 每次增减的步长（默认 1）

---

### 2. 购物车 - computed

**组件名**: `composition-cart`

**演示功能**:
- 使用 `etaf-computed` 创建计算属性
- 计算属性的自动缓存和惰性求值
- 计算属性之间的依赖链

**关键代码**:
```elisp
(let* ((items (etaf-ref '(...)))
       (subtotal (etaf-computed
                  (lambda ()
                    (let ((sum 0))
                      (dolist (item (etaf-ref-get items))
                        (setq sum (+ sum (* price qty))))
                      sum))))
       (total (etaf-computed
               (lambda ()
                 (* (etaf-computed-get subtotal) (- 1 discount))))))
  ...)
```

**学习要点**:
- `etaf-computed` 接收一个 getter 函数，返回派生值
- 计算属性会自动追踪依赖（这里是 items ref）
- 计算属性可以依赖其他计算属性（total 依赖 subtotal）
- 只有在依赖变化且被访问时才重新计算（惰性 + 缓存）

**Props**:
- `:discount-rate` - 折扣率（0-1 之间，默认 0）

**展示内容**:
- 商品列表（名称、价格、数量）
- 小计金额（自动计算）
- 总计金额（应用折扣后的金额）
- 商品种类数量

---

### 3. 用户资料 - watch

**组件名**: `composition-profile`

**演示功能**:
- 使用 `etaf-watch` 监听响应式值的变化
- watch 的 callback 参数：新值、旧值、onInvalidate
- 清理函数的使用（处理副作用）

**关键代码**:
```elisp
(let* ((username (etaf-ref "游客"))
       (stop-watch
        (etaf-watch
         username
         (lambda (new-name old-name on-invalidate)
           (etaf-ref-set status-message
                        (format "用户从 '%s' 变更为 '%s'" old-name new-name))
           (funcall on-invalidate
                   (lambda ()
                     (message "清理旧用户 %s 的数据..." old-name))))
         (list :immediate t))))
  ...)
```

**学习要点**:
- `etaf-watch` 显式监听特定的响应式源
- callback 在源变化时调用，接收新值和旧值
- `on-invalidate` 用于注册清理函数，在下次变化前执行
- `:immediate t` 选项让 watcher 立即执行一次
- `stop-watch` 是返回的停止函数，可以停止监听

**Props**:
- `:username` - 初始用户名（默认 "游客"）

**展示内容**:
- 当前用户名
- 登录次数（每次用户名变化时递增）
- 状态消息（显示用户变更历史）

---

### 4. 日志查看器 - watchEffect

**组件名**: `composition-logger`

**演示功能**:
- 使用 `etaf-watch-effect` 进行自动依赖追踪
- watchEffect 与 watch 的区别
- 副作用的自动重运行

**关键代码**:
```elisp
(let* ((log-level (etaf-ref "INFO"))
       (message-count (etaf-ref 0))
       (stop-effect
        (etaf-watch-effect
         (lambda ()
           (let ((level (etaf-ref-get log-level))
                 (count (etaf-ref-get message-count)))
             (message "[WatchEffect] 日志级别: %s, 消息数: %d" level count)
             (etaf-ref-set logs
                          (format "自动监控 - 级别:%s 数量:%d" level count)))))))
  ...)
```

**学习要点**:
- `etaf-watch-effect` 自动追踪函数内访问的所有响应式值
- 不需要显式指定监听源，更加便捷
- 立即执行一次，然后在任何依赖变化时重新执行
- 适合需要同步多个响应式状态的场景

**对比 watch**:
- watch: 显式指定监听源，获得旧值和新值
- watchEffect: 自动追踪，不提供旧值，更简洁

**展示内容**:
- 当前日志级别
- 消息数量
- 自动监控状态（由 watchEffect 自动更新）

---

### 5. 待办事项 - reactive

**组件名**: `composition-todos`

**演示功能**:
- 使用 ref 包装复杂对象（模拟 reactive 行为）
- 多个计算属性协同工作
- 过滤和状态管理

**关键代码**:
```elisp
(let* ((todos (etaf-ref '((:id 1 :text "学习 ETAF" :done nil) ...)))
       (filter (etaf-ref 'all))
       (completed-count (etaf-computed
                         (lambda ()
                           (cl-count-if (lambda (todo) (plist-get todo :done))
                                       (etaf-ref-get todos)))))
       (filtered-todos (etaf-computed
                        (lambda ()
                          (let ((all-todos (etaf-ref-get todos))
                                (current-filter (etaf-ref-get filter)))
                            (cond
                             ((eq current-filter 'active)
                              (cl-remove-if (lambda (todo) (plist-get todo :done))
                                          all-todos))
                             ...))))))
  ...)
```

**学习要点**:
- 可以用 ref 包装列表、plist 等复杂数据结构
- 多个计算属性可以同时依赖同一个 ref
- 计算属性可以依赖多个 ref（todos 和 filter）
- 不可变更新模式：通过 mapcar 创建新列表而非修改原列表

**展示内容**:
- 待办事项列表（根据过滤器显示）
- 活跃和已完成的数量统计
- 当前过滤器状态

---

## Vue 2 选项式 API 示例

### 6. 选项式计数器 - data & methods

**组件名**: `options-counter`

**演示功能**:
- 使用 `:data` 定义响应式数据
- 使用 `:methods` 定义方法
- 使用 `:computed` 定义计算属性
- 使用 `:mounted` 生命周期钩子

**关键代码**:
```elisp
(etaf-define-component options-counter
  :data (lambda ()
          (list :count 0 :history nil))
  :methods (list
            :increment (lambda ()
                        (let ((count-ref (plist-get this :count)))
                          (etaf-ref-set count-ref (1+ (etaf-ref-get count-ref))))))
  :computed (list
             :is-max (lambda ()
                      (>= (etaf-ref-get (plist-get this :count)) max)))
  :mounted (lambda ()
            (message "Options Counter 组件已挂载！")))
```

**学习要点**:
- `:data` 函数返回的 plist，每个属性自动转为 ref
- `:methods` 中的函数可以通过 `this` 访问组件数据
- `:computed` 中的函数也可以访问 `this`
- `:mounted` 在组件挂载后执行

**内部实现**:
- Options API 在内部被转换为 Composition API
- data 属性 → ref
- methods → 绑定 this 的函数
- computed → etaf-computed
- mounted → 在 setup 结束后调用

**Props**:
- `:initial` - 初始值
- `:max` - 最大值限制（默认 999）

---

### 7. 计算器 - computed

**组件名**: `options-calculator`

**演示功能**:
- 复杂的计算属性逻辑
- 计算属性依赖多个数据源
- 条件计算

**关键代码**:
```elisp
:computed (list
           :result (lambda ()
                    (let* ((num1 (etaf-ref-get (plist-get this :num1)))
                           (num2 (etaf-ref-get (plist-get this :num2)))
                           (op (etaf-ref-get (plist-get this :operator))))
                      (cond
                       ((eq op 'add) (+ num1 num2))
                       ((eq op 'divide)
                        (if (= num2 0) "错误：除数为零" (/ (float num1) num2)))
                       ...)))
           :operator-symbol (lambda ()
                             (cond ((eq op 'add) "+") ...)))
```

**学习要点**:
- 计算属性可以有复杂的条件逻辑
- 可以返回不同类型的值（数字或字符串）
- 多个计算属性可以协同工作（result 和 operator-symbol）
- 错误处理可以在计算属性中完成

**展示内容**:
- 两个操作数
- 运算符
- 计算结果（自动更新）

---

### 8. 表单验证 - watch

**组件名**: `options-form`

**演示功能**:
- 使用 `:watch` 监听数据变化
- 实时表单验证
- 多个 watcher 协同工作

**关键代码**:
```elisp
:watch (list
        :email (lambda (new-val old-val)
                (let ((error-ref (plist-get this :email-error)))
                  (if (or (string-empty-p new-val)
                          (not (string-match-p "@" new-val)))
                      (etaf-ref-set error-ref "邮箱格式无效")
                    (etaf-ref-set error-ref nil))))
        :password (lambda (new-val old-val)
                   (if (< (length new-val) 6)
                       (etaf-ref-set error-ref "密码至少6个字符")
                     (etaf-ref-set error-ref nil))))
```

**学习要点**:
- `:watch` 中的每个 watcher 监听对应的 data 属性
- watcher 接收新值和旧值作为参数
- 可以在 watcher 中执行验证逻辑并更新其他状态
- 适合需要在数据变化时执行副作用的场景

**展示内容**:
- 邮箱输入和验证状态
- 密码输入和验证状态
- 整体表单有效性
- 提交次数统计

---

### 9. 系统仪表板 - 综合示例

**组件名**: `options-dashboard`

**演示功能**:
- 综合使用 data、methods、computed、watch
- 多个计算属性形成派生数据链
- watch 用于副作用（告警）

**关键代码**:
```elisp
:data (lambda ()
        (list :cpu-usage 45 :memory-usage 67 :disk-usage 23))
:methods (list
          :simulate-update (lambda ()
                            (etaf-ref-set (plist-get this :cpu-usage) (+ 20 (random 60)))
                            ...))
:computed (list
           :cpu-status (lambda ()
                        (let ((usage (etaf-ref-get (plist-get this :cpu-usage))))
                          (cond ((> usage 80) "危险") ((> usage 60) "警告") (t "正常"))))
           :overall-health (lambda ()
                            (let ((avg (/ (+ cpu mem disk) 3.0)))
                              (cond ((> avg 75) "不健康") ...))))
:watch (list
        :cpu-usage (lambda (new-val old-val)
                    (when (and (> new-val 80) (<= old-val 80))
                      (message "警告：CPU 使用率过高！"))))
```

**学习要点**:
- 可以组合使用所有 Options API 功能
- computed 可以形成多层派生（cpu-status、overall-health）
- watch 用于阈值检测和告警
- methods 用于触发状态变化

**展示内容**:
- CPU、内存、磁盘使用率
- 各项状态评估（危险/警告/正常）
- 整体健康状态
- 更新次数和时间戳

---

## 使用方法

### 1. 加载示例文件

```elisp
(load-file "examples/etaf-component-examples.el")
```

### 2. 运行交互式演示

```elisp
;; 启动菜单选择器
M-x etaf-component-demo

;; 或直接渲染单个组件
M-x etaf-component-demo-simple
```

### 3. 在代码中使用组件

```elisp
;; 组合式 API 组件
(etaf-paint-to-buffer "*demo*"
  '(composition-counter :initial-count 10 :step 2))

;; 选项式 API 组件
(etaf-paint-to-buffer "*demo*"
  '(options-calculator))

;; 查看所有示例
(etaf-paint-to-buffer "*demo*"
  '(div
     (h2 "组合式 API 示例")
     (composition-counter :initial-count 0 :step 1)
     (composition-cart :discount-rate 0.1)
     (hr)
     (h2 "选项式 API 示例")
     (options-counter :max 50)
     (options-form)))
```

### 4. 添加自定义样式

```elisp
(etaf-paint-to-buffer "*styled-demo*"
  '(composition-counter :initial-count 0)
  nil
  '("div { padding: 2cw; }"
    "h3 { color: blue; font-weight: bold; }"
    "p { margin: 0.5 0; }")
  80)
```

---

## 学习建议

### 对于初学者

1. **按顺序学习**: 从简单到复杂，按示例编号顺序学习
2. **动手实践**: 修改 props 参数，观察效果变化
3. **阅读代码**: 仔细阅读每个组件的 setup/data 函数
4. **理解概念**: 
   - ref: 响应式的容器
   - computed: 派生状态，自动更新
   - watch: 显式监听，执行副作用
   - watchEffect: 自动监听，更简洁

### 选择 API 风格

**使用组合式 API，如果**:
- 组件逻辑复杂，需要更好的组织
- 需要在多个组件间复用逻辑
- 喜欢函数式编程风格
- 需要更灵活的代码组织

**使用选项式 API，如果**:
- 组件相对简单
- 团队熟悉 Vue 2 风格
- 喜欢关注点明确分离的结构
- 快速原型开发

### 进阶学习

1. **组合函数**: 创建可复用的组合式函数
   ```elisp
   (defun use-counter (initial-value)
     (let* ((count (etaf-ref initial-value))
            (increment (lambda () (etaf-ref-update count #'1+))))
       (list :count count :increment increment)))
   ```

2. **混合使用**: 在同一个项目中混用两种风格
   ```elisp
   ;; 组合式 API 组件调用选项式 API 组件
   (etaf-paint-to-buffer "*mixed*"
     '(div
        (composition-counter)
        (options-calculator)))
   ```

3. **自定义组件**: 基于示例创建自己的组件
4. **性能优化**: 理解 computed 的缓存机制
5. **调试技巧**: 使用 message 函数查看响应式更新

---

## 常见问题

### Q: 为什么 computed 比普通函数好？

**A**: computed 有两个关键优势：
1. **缓存**: 只有依赖变化时才重新计算
2. **响应式**: 自己也是响应式的，可以被其他 computed/watch 依赖

### Q: watch 和 watchEffect 有什么区别？

**A**:
- **watch**: 显式指定监听源，获得新旧值，适合需要比较新旧值的场景
- **watchEffect**: 自动追踪依赖，立即执行，适合单纯的副作用

### Q: Options API 和 Composition API 可以混用吗？

**A**: 可以！它们本质上是同一个系统的不同接口。你可以在同一个应用中使用两种风格的组件。

### Q: 如何停止一个 watcher？

**A**: 
```elisp
(let ((stop (etaf-watch source callback)))
  ;; ... 稍后
  (funcall stop))  ; 停止监听
```

### Q: 为什么我的组件没有自动更新？

**A**: 检查：
1. 是否使用了 `etaf-ref-set` 而非直接赋值
2. 是否在 computed/watch 中使用了 `etaf-ref-get`
3. 是否正确地将响应式值传递给了模板

---

## 总结

这些示例全面展示了 ETAF 响应式系统的能力：

**组合式 API** 提供：
- 灵活的代码组织
- 更好的类型推断（在支持的环境中）
- 更容易的逻辑复用

**选项式 API** 提供：
- 清晰的结构
- 熟悉的 Vue 2 风格
- 更少的样板代码

选择适合你的风格，或者在项目中混用两者！

## 相关文档

- [组件系统](COMPONENT-SYSTEM-CN.md) - 完整的组件系统文档
- [响应式系统](REACTIVE-SYSTEM.md) - 响应式系统深入讲解
- [用户手册](USER-MANUAL.md) - ETAF 用户手册
