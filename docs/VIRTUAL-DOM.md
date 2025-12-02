# Virtual DOM (VNode) System

## Overview

ETAF's Virtual DOM system is inspired by Vue 3's VNode model and rendering mechanism. It provides a declarative UI model that serves as an intermediate representation layer between your UI description and the actual DOM.

### What is Virtual DOM?

The Virtual DOM is a lightweight JavaScript representation of the actual DOM. It acts as:

1. **A Computable UI State Model** - The framework uses this model to calculate minimal updates needed to transform the UI from one state to another
2. **A Clean Separation** - It separates the logical structure (what you want to render) from rendering details (how it's rendered)
3. **An Optimization Layer** - It enables batching and optimization of DOM updates

### Why Virtual DOM?

As explained in [Vue 3's rendering mechanism](https://cn.vuejs.org/guide/extras/rendering-mechanism):

> Declarative UI needs an "intermediate representation layer" (a data model of UI), and the framework must run a diff algorithm on this representation layer to compute how to update the UI. Virtual DOM is one implementation of this "intermediate representation layer."

## VNode Structure

A VNode (Virtual Node) is a plain Emacs Lisp plist with the following structure:

```elisp
(:id unique-id                    ; Unique identifier
 :type element|text|comment|fragment|component
 :tag symbol                      ; Tag name (for element nodes)
 :props plist                     ; Properties from ETML
 :dom dom-node                    ; Clean DOM node
 :tag-instance tag-instance       ; ETAF tag instance (for interactive elements)
 :children list-of-vnodes         ; Child VNodes
 :parent parent-vnode             ; Parent VNode reference
 :key key                         ; Key for list reconciliation
 :mounted-p boolean               ; Whether node is mounted
 :hooks plist)                    ; Lifecycle hooks
```

### VNode Types

1. **Element** - Represents an HTML element (`div`, `span`, etc.)
2. **Text** - Represents text content
3. **Comment** - Represents comments (not rendered)
4. **Fragment** - Multiple root nodes without a wrapper
5. **Component** - A reusable component (future enhancement)

## Creating VNodes

### Element VNodes

```elisp
(etaf-vdom-element 'div 
  :props '(:class "container" :id "main")
  :key "unique-key")
```

### Text VNodes

```elisp
(etaf-vdom-text "Hello World")
```

### Comment VNodes

```elisp
(etaf-vdom-comment "This is a comment")
```

### Fragment VNodes

```elisp
(etaf-vdom-fragment
  (etaf-vdom-element 'div)
  (etaf-vdom-element 'span)
  (etaf-vdom-element 'p))
```

## Lifecycle Management

Virtual DOM nodes support lifecycle hooks inspired by Vue 3:

### Lifecycle Hooks

1. **:mounted** - Called when a VNode is added to the DOM
2. **:updated** - Called when a VNode's properties or children change
3. **:unmounted** - Called when a VNode is removed from the DOM

### Adding Lifecycle Hooks

```elisp
(let ((vnode (etaf-vdom-element 'div)))
  ;; Add a mounted hook
  (etaf-vdom-add-hook vnode :mounted
    (lambda (node)
      (message "Node mounted!")))
  
  ;; Add an updated hook
  (etaf-vdom-add-hook vnode :updated
    (lambda (new-node old-node)
      (message "Node updated!")))
  
  ;; Add an unmounted hook
  (etaf-vdom-add-hook vnode :unmounted
    (lambda (node)
      (message "Node unmounted!"))))
```

### Mounting and Unmounting

```elisp
;; Mount a VNode tree (marks as mounted and calls hooks)
(etaf-vdom-mount vnode)

;; Check if mounted
(etaf-vdom-get-mounted-p vnode)  ; => t

;; Unmount a VNode tree
(etaf-vdom-unmount vnode)
```

## Diff/Patch Algorithm

The diff algorithm compares two VNode trees and produces a list of patches (minimal operations) needed to transform the old tree into the new one.

### Patch Types

1. **create** - Create a new node
2. **remove** - Remove an existing node
3. **replace** - Replace a node with a different type
4. **update** - Update node properties
5. **reorder** - Reorder children (for keyed lists)

### Basic Diffing

```elisp
(let* ((old-vnode (etaf-vdom-element 'div :props '(:class "old")))
       (new-vnode (etaf-vdom-element 'div :props '(:class "new")))
       (patches (etaf-vdom-diff old-vnode new-vnode)))
  ;; patches contains the minimal operations to transform old to new
  (dolist (patch patches)
    (message "Patch type: %s" (etaf-vdom-patch-type patch))))
```

### Diff Optimization Strategy

The diff algorithm uses several optimizations:

1. **Type Checking** - If nodes are different types, replace the entire subtree
2. **Property Diffing** - Only compute changed properties
3. **Keyed Reconciliation** - Use keys for efficient list updates
4. **Same Type Fast Path** - Skip unnecessary comparisons for identical nodes

## Keyed Reconciliation

Keys help the diff algorithm identify which items have changed, been added, or been removed in a list.

### Without Keys (Positional Matching)

```elisp
;; Old list: [A, B, C]
;; New list: [C, A, D]
;; Without keys: Replace all three items

(let ((old-children (list (etaf-vdom-element 'li)
                         (etaf-vdom-element 'li)
                         (etaf-vdom-element 'li))))
  ...)
```

### With Keys (Identity Matching)

```elisp
;; Old list: [A:1, B:2, C:3]
;; New list: [C:3, A:1, D:4]
;; With keys: Move C, keep A, remove B, add D

(let ((old-children (list 
                      (etaf-vdom-element 'li :props '(:key "1"))
                      (etaf-vdom-element 'li :props '(:key "2"))
                      (etaf-vdom-element 'li :props '(:key "3")))))
  ...)
```

### Key Extraction

Keys are automatically extracted from:
1. The `:key` property
2. The `:id` property (fallback)

```elisp
;; Both create keyed VNodes
(etaf-vdom-element 'div :props '(:key "unique-1"))
(etaf-vdom-element 'div :props '(:id "my-id"))
```

## Integration with ETAF

### Converting ETML to VNode

ETAF automatically creates VNodes when using `etaf-etml-to-dom-with-vdom`:

```elisp
(let* ((result (etaf-etml-to-dom-with-vdom 
                '(div :class "container"
                  (p "Hello")
                  (a :href "/link" "Click me"))))
       (vtree (etaf-vdom-result-get-vtree result))
       (dom (etaf-vdom-result-get-dom result)))
  ;; vtree contains the VNode tree with tag instances
  ;; dom contains the clean DOM without tag instances
  )
```

### Clean DOM vs VNode

ETAF maintains a clean separation:

- **DOM** - Contains only standard HTML structure and attributes
- **VNode** - Contains additional metadata like tag instances, hooks, and keys

This separation allows:
1. CSS selectors to work on clean DOM
2. Event handlers and lifecycle hooks in VNodes
3. Efficient diffing and patching

## Use Cases

### 1. Reactive Re-rendering

When state changes, diff the old and new VNode trees:

```elisp
(let* ((old-vtree (etaf-etml-to-dom-with-vdom template old-data))
       (new-vtree (etaf-etml-to-dom-with-vdom template new-data))
       (patches (etaf-vdom-diff 
                 (etaf-vdom-result-get-vtree old-vtree)
                 (etaf-vdom-result-get-vtree new-vtree))))
  ;; Apply patches to update only what changed
  (etaf-vdom-apply-patches patches))
```

### 2. List Management with Keys

Efficiently update lists:

```elisp
(setq items '("Item 1" "Item 2" "Item 3"))

;; Create keyed list
(let ((vnode-list
       (mapcar (lambda (item)
                 (etaf-vdom-element 'li 
                   :props (list :key item)
                   :children (list (etaf-vdom-text item))))
               items)))
  ...)

;; When items change, diff algorithm uses keys
;; to minimize DOM updates
```

### 3. Component Lifecycle

Track when components are mounted/unmounted:

```elisp
(defun create-interactive-button ()
  (let ((vnode (etaf-vdom-element 'button)))
    (etaf-vdom-add-hook vnode :mounted
      (lambda (node)
        (message "Button mounted - setup event listeners")))
    (etaf-vdom-add-hook vnode :unmounted
      (lambda (node)
        (message "Button unmounted - cleanup event listeners")))
    vnode))
```

## Comparison with Vue 3

| Feature | Vue 3 | ETAF VNode |
|---------|-------|------------|
| VNode Types | element, text, comment, fragment, component | ✓ Implemented |
| Keyed Reconciliation | ✓ | ✓ Implemented |
| Lifecycle Hooks | onMounted, onUpdated, onUnmounted | ✓ Implemented |
| Diff Algorithm | Optimized with flags and fast paths | ✓ Simplified version |
| Fragment Support | ✓ | ✓ Implemented |
| Component VNodes | ✓ | Deferred (can be added) |

## Performance Considerations

1. **Key Usage** - Always use keys for dynamic lists to enable efficient reconciliation
2. **Immutability** - VNodes are not meant to be mutated directly; create new ones
3. **Batch Updates** - Diff/patch operations should be batched when possible
4. **Shallow Comparison** - Properties are compared using `equal`, not deep equality

## API Reference

### VNode Creation

- `(etaf-vdom-element tag &rest props)` - Create element VNode
- `(etaf-vdom-text content)` - Create text VNode
- `(etaf-vdom-comment content)` - Create comment VNode
- `(etaf-vdom-fragment &rest children)` - Create fragment VNode

### VNode Accessors

- `(etaf-vdom-get-type vnode)` - Get VNode type
- `(etaf-vdom-get-tag vnode)` - Get element tag
- `(etaf-vdom-get-props vnode)` - Get properties
- `(etaf-vdom-get-children vnode)` - Get children
- `(etaf-vdom-get-key vnode)` - Get key
- `(etaf-vdom-get-mounted-p vnode)` - Check if mounted

### VNode Predicates

- `(etaf-vdom-vnode-p obj)` - Check if object is a VNode
- `(etaf-vdom-element-p vnode)` - Check if element
- `(etaf-vdom-text-p vnode)` - Check if text
- `(etaf-vdom-fragment-p vnode)` - Check if fragment
- `(etaf-vdom-same-type-p vnode1 vnode2)` - Check if same type

### Lifecycle

- `(etaf-vdom-mount vnode)` - Mount VNode tree
- `(etaf-vdom-unmount vnode)` - Unmount VNode tree
- `(etaf-vdom-update old new)` - Update VNode
- `(etaf-vdom-add-hook vnode hook-type func)` - Add lifecycle hook

### Diff/Patch

- `(etaf-vdom-diff old-vnode new-vnode)` - Compute patches
- `(etaf-vdom-apply-patches patches)` - Apply patches
- `(etaf-vdom-diff-props old new)` - Diff properties
- `(etaf-vdom-diff-children old new)` - Diff children

## References

- [Vue 3 Rendering Mechanism](https://cn.vuejs.org/guide/extras/rendering-mechanism)
- [Vue 3 Core Repository](https://github.com/vuejs/core)
- [Virtual DOM and Internals - Vue.js](https://vuejs.org/guide/extras/rendering-mechanism.html)

## Future Enhancements

1. **Component VNodes** - Full support for component-based VNodes
2. **Suspense** - Support for async component loading
3. **Teleport** - Render content to different DOM locations
4. **More Optimizations** - Static hoisting, patch flags, block trees
5. **Scheduler** - Priority-based update scheduling
