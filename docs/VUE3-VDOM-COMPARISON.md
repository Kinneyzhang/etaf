# Vue 3 Virtual DOM Implementation in ETAF

## 背景 / Background

本文档说明 ETAF 如何参考 Vue 3 的设计实现虚拟 DOM 系统。

This document explains how ETAF implements a virtual DOM system inspired by Vue 3's design.

## 参考资料 / References

1. [Vue 3 渲染机制 | Vue.js](https://cn.vuejs.org/guide/extras/rendering-mechanism)
2. [Vue Core Repository](https://github.com/vuejs/core)
3. Vue 3 VNode 源码: `packages/runtime-core/src/vnode.ts`
4. Vue 3 Renderer 源码: `packages/runtime-core/src/renderer.ts`

## 虚拟 DOM 的必要性 / Why Virtual DOM?

### Vue 3 的解释

根据 Vue 3 官方文档的说明：

> 虚拟 DOM 的必要性来自：声明式 UI 需要一个"中间表示层"（UI 的数据模型），框架必须在这个表示层上运行 diff 算法去计算 UI 如何更新；而虚拟 DOM 就是这个"中间表示层"的实现方式之一。

关键概念：
1. **可计算的 UI 状态模型** - 框架需要一个可以计算的 UI 数据模型来判断如何以最小代价更新 DOM
2. **中间表示层** - 虚拟 DOM 是这个"可计算模型"的一种具体实现
3. **Diff 算法** - 在虚拟 DOM 上运行 diff 算法来计算最小更新

### ETAF 的实现

ETAF 采用了相同的架构理念：

```
ETML (模板) → VNode Tree (虚拟 DOM) → Diff/Patch → 真实 DOM 更新
```

## 核心概念对比 / Core Concepts Comparison

### 1. VNode 结构 / VNode Structure

#### Vue 3 VNode

```typescript
interface VNode {
  type: VNodeTypes          // 组件类型或 HTML 标签
  props: VNodeProps | null  // 属性
  children: VNodeChildren   // 子节点
  key: string | number      // 用于 diff 优化
  ref: Ref                  // 引用
  // ... 其他字段
}
```

#### ETAF VNode

```elisp
(:id unique-id                    ; 唯一标识
 :type element|text|comment|fragment|component
 :tag symbol                      ; HTML 标签
 :props plist                     ; 属性（plist 格式）
 :children list-of-vnodes         ; 子节点列表
 :key key                         ; diff 优化的 key
 :dom dom-node                    ; 对应的 DOM 节点
 :tag-instance tag-instance       ; ETAF 标签实例
 :parent parent-vnode             ; 父节点引用
 :mounted-p boolean               ; 是否已挂载
 :hooks plist)                    ; 生命周期钩子
```

**相似点：**
- 都有 type/tag 区分节点类型
- 都有 props 存储属性
- 都有 children 存储子节点
- 都有 key 用于列表 diff 优化

**差异点：**
- ETAF 保存了 `:dom` 字段指向真实 DOM（Vue 3 在 runtime 中管理）
- ETAF 增加了 `:tag-instance` 存储 ETAF 特有的标签实例
- ETAF 显式存储了 `:parent` 引用和 `:mounted-p` 状态

### 2. VNode 类型 / VNode Types

#### Vue 3

```typescript
type VNodeTypes =
  | string                // 原生元素
  | Component             // 组件
  | typeof Text           // 文本
  | typeof Comment        // 注释
  | typeof Fragment       // 片段
  | typeof Teleport       // Teleport
  | typeof Suspense       // Suspense
```

#### ETAF

```elisp
;; VNode 类型
'element    ; 原生 HTML 元素
'text       ; 文本节点
'comment    ; 注释节点
'fragment   ; 片段（多个根节点）
'component  ; 组件（预留，未来实现）
```

**实现状态：**
- ✅ element, text, comment, fragment - 已实现
- ⏳ component - 预留接口，未来可扩展
- ❌ teleport, suspense - 暂不支持（ETAF 场景不需要）

### 3. 生命周期钩子 / Lifecycle Hooks

#### Vue 3 组件生命周期

```javascript
onBeforeMount(() => { /* ... */ })
onMounted(() => { /* ... */ })
onBeforeUpdate(() => { /* ... */ })
onUpdated(() => { /* ... */ })
onBeforeUnmount(() => { /* ... */ })
onUnmounted(() => { /* ... */ })
```

#### ETAF VNode 生命周期

```elisp
;; 添加生命周期钩子
(etaf-vdom-add-hook vnode :mounted
  (lambda (node) 
    ;; 节点挂载时调用
    ))

(etaf-vdom-add-hook vnode :updated
  (lambda (new-node old-node)
    ;; 节点更新时调用
    ))

(etaf-vdom-add-hook vnode :unmounted
  (lambda (node)
    ;; 节点卸载时调用
    ))
```

**相似点：**
- 都提供了 mounted, updated, unmounted 钩子
- 都可以在这些钩子中执行副作用操作

**差异点：**
- Vue 3 钩子是组件级别，ETAF 是 VNode 级别
- ETAF 简化了钩子系统，只保留核心的三个钩子

### 4. Diff 算法 / Diff Algorithm

#### Vue 3 Diff 策略

Vue 3 的 diff 算法包含多个优化：

1. **静态提升** - 静态节点只创建一次
2. **Patch Flags** - 标记需要更新的属性类型
3. **Block Tree** - 只 diff 动态节点
4. **最长递增子序列** - 优化列表重排序

核心代码在 `patchChildren`, `patchKeyedChildren` 等函数中。

#### ETAF Diff 实现

```elisp
(defun etaf-vdom-diff (old-vnode new-vnode)
  "比较两个 VNode 并返回补丁列表"
  ;; 1. 节点不存在 → CREATE
  ;; 2. 新节点为 nil → REMOVE
  ;; 3. 类型不同 → REPLACE
  ;; 4. 类型相同 → UPDATE properties + diff children
  )

(defun etaf-vdom-diff-children-keyed (old-children new-children parent)
  "使用 key 进行高效的子节点 diff"
  ;; 1. 构建 key 映射表
  ;; 2. 处理新子节点列表
  ;; 3. 检测移动、添加、删除操作
  ;; 4. 生成最小补丁列表
  )
```

**实现的优化：**
- ✅ 同类型节点快速路径（只 diff 属性和子节点）
- ✅ 不同类型节点直接替换
- ✅ Key 映射表用于列表 diff
- ✅ 位置变化检测（REORDER patch）

**未实现的优化：**
- ❌ 静态提升（ETAF 不做编译时优化）
- ❌ Patch Flags（简化实现）
- ❌ 最长递增子序列算法（使用简化的移动检测）

### 5. Patch 操作 / Patch Operations

#### Vue 3

```typescript
// Vue 3 内部的 patch 操作
patch(n1, n2, container)
mount(vnode, container)
unmount(vnode)
move(vnode, container)
```

#### ETAF

```elisp
;; ETAF 的补丁类型
'create    ; 创建新节点
'remove    ; 删除节点
'replace   ; 替换节点（类型不同）
'update    ; 更新属性
'reorder   ; 重排序（key 列表）

;; 应用补丁
(etaf-vdom-apply-patches patches)
```

**相似点：**
- 都定义了基本的 DOM 操作类型
- 都支持增量更新

**差异点：**
- ETAF 使用声明式的补丁列表，而不是直接操作 DOM
- ETAF 的 `apply-patches` 是占位符，实际与渲染系统集成

## 关键列表 Diff 算法对比 / Keyed List Diff Comparison

### Vue 3 算法

Vue 3 使用了复杂的优化策略：

```typescript
// patchKeyedChildren 简化逻辑
function patchKeyedChildren(c1, c2) {
  // 1. 从头部开始，patch 相同的节点
  // 2. 从尾部开始，patch 相同的节点
  // 3. 处理中间的未知序列
  //    - 构建 key -> index 映射
  //    - 计算最长递增子序列
  //    - 移动需要移动的节点
}
```

### ETAF 实现

```elisp
(defun etaf-vdom-diff-children-keyed (old-children new-children parent)
  "简化的 keyed diff 算法"
  ;; 1. 构建 old/new key 映射表
  ;; 2. 遍历新子节点：
  ;;    - 在 old 中找到匹配 key → 检测是否需要移动
  ;;    - 未找到 → 创建新节点
  ;; 3. 找出被删除的旧节点
  )
```

**优势：**
- 简单实现，易于理解和维护
- 对大多数场景足够高效
- 正确处理添加、删除、移动操作

**权衡：**
- 未实现最长递增子序列优化（复杂场景可能多做一些移动操作）
- 但对于 ETAF 的使用场景（Emacs buffer 渲染）已经足够

## 示例对比 / Example Comparison

### Vue 3 用法

```vue
<template>
  <ul>
    <li v-for="item in items" :key="item.id">
      {{ item.text }}
    </li>
  </ul>
</template>

<script setup>
import { ref } from 'vue'
const items = ref([
  { id: 1, text: 'Item 1' },
  { id: 2, text: 'Item 2' }
])
</script>
```

### ETAF 用法

```elisp
;; 创建带 key 的列表
(setq items '((:id "1" :text "Item 1")
              (:id "2" :text "Item 2")))

(setq old-vnode
  (etaf-vdom-element 'ul
    :children (mapcar
               (lambda (item)
                 (etaf-vdom-element 'li
                   :props (list :key (plist-get item :id))
                   :children (list (etaf-vdom-text (plist-get item :text)))))
               items)))

;; 更新数据后进行 diff
(setq new-items '((:id "2" :text "Item 2")
                  (:id "3" :text "Item 3")))
(setq new-vnode (create-list-vnode new-items))

;; 计算最小更新
(setq patches (etaf-vdom-diff old-vnode new-vnode))
```

## 性能考虑 / Performance Considerations

### Vue 3 的优化策略

1. **编译时优化** - PatchFlag, Static Hoisting
2. **运行时优化** - Block Tree, 最长递增子序列
3. **响应式系统** - Proxy-based reactivity
4. **调度器** - 批量更新，优先级队列

### ETAF 的优化策略

1. **Key-based reconciliation** - 高效列表更新
2. **Same type fast path** - 跳过不必要的比较
3. **Property-level diff** - 只更新变化的属性
4. **Clean DOM separation** - VNode 和 DOM 分离，便于 CSS 查询

**ETAF 不需要的优化：**
- 编译时优化（ETAF 使用 S-expression，不需要编译）
- 复杂的调度器（Emacs buffer 渲染是同步的）
- Proxy reactivity（ETAF 有自己的响应式系统）

## 实现统计 / Implementation Statistics

| 特性 | Vue 3 | ETAF | 状态 |
|-----|-------|------|-----|
| VNode 基础结构 | ✅ | ✅ | 完全实现 |
| 多种 VNode 类型 | ✅ | ✅ | element, text, comment, fragment |
| Key-based diff | ✅ | ✅ | 完全实现 |
| 生命周期钩子 | ✅ | ✅ | mounted, updated, unmounted |
| Fragment 支持 | ✅ | ✅ | 完全实现 |
| 组件 VNode | ✅ | ⏳ | 预留接口 |
| Patch Flags | ✅ | ❌ | 不需要 |
| 静态提升 | ✅ | ❌ | 不需要 |
| 最长递增子序列 | ✅ | ❌ | 简化实现 |

## 测试覆盖 / Test Coverage

ETAF 实现了全面的测试（见 `tests/etaf-vdom-tests.el`）：

- ✅ VNode 创建和类型检查
- ✅ 生命周期钩子（mounted, updated, unmounted）
- ✅ Diff 算法各种场景（create, remove, replace, update）
- ✅ Key-based 列表 diff
- ✅ 属性变化检测
- ✅ 层级挂载/卸载
- ✅ 与 ETAF 集成测试

## 文档资源 / Documentation

- `docs/VIRTUAL-DOM.md` - 完整的 API 文档和使用指南
- `examples/etaf-vdom-example.el` - 8个交互式示例
- `tests/etaf-vdom-tests.el` - 全面的测试套件

## 总结 / Conclusion

ETAF 成功实现了 Vue 3 虚拟 DOM 的核心理念：

1. **声明式 UI 的中间表示层** - VNode 树作为可计算的 UI 模型
2. **高效的 Diff/Patch 算法** - 计算最小更新操作
3. **Key-based 协调** - 优化列表更新性能
4. **生命周期管理** - 挂载、更新、卸载钩子
5. **干净的分离** - VNode 和 DOM 各司其职

实现质量：
- ✅ 核心功能完整
- ✅ API 设计清晰
- ✅ 测试覆盖全面
- ✅ 文档详细完善
- ✅ 示例丰富实用

ETAF 的虚拟 DOM 系统为构建高性能、声明式的 Emacs 文本 UI 应用提供了坚实的基础。

---

**参考实现时间**: 2024
**基于 Vue 3 版本**: 3.x
**ETAF 版本**: 1.0.0
