# ETAF Architecture Redesign

## Overview

ETAF has been redesigned to follow Vue 3's rendering pipeline architecture, providing cleaner separation of concerns and better alignment with modern web framework patterns.

## New Rendering Pipeline

```
模板(ETML) → 编译器 → 渲染函数 → 虚拟DOM → 渲染器 → 真实DOM → CSSOM → 渲染树 → 布局树 → 最终文本
Template  → Compiler → Render Fn → VNodes → Renderer → Clean DOM → CSSOM → Render → Layout → String
```

### Detailed Flow

1. **ETML Template** - S-expression based markup
   - Example: `(button :class "btn" "Click Me")`
   
2. **Compiler (etaf-etml.el)** - Converts ETML to VNode tree
   - Parses template syntax
   - Creates VNode tree with metadata
   - Stores tag metadata (event handlers, styles) in VNodes
   
3. **Virtual DOM (VNode Tree)** - Intermediate representation
   - Contains tag metadata (`:tag-metadata`)
   - Stores interactive capabilities (event handlers, state styles)
   - Maintains element state (`:state`)
   - Generates clean DOM (`:dom`)
   
4. **Renderer** - Extracts clean DOM from VNode
   - Clean DOM has no tag-metadata in attributes
   - Interactive info stays in VNode layer
   
5. **CSSOM** - CSS Object Model (unchanged)
   
6. **Render Tree** - Styled DOM nodes (unchanged)
   
7. **Layout Tree** - Box model and positioning (unchanged)
   
8. **Buffer String** - Final text with Emacs properties

## Key Changes

### Before (Old Architecture)

- **etaf-etml-tag.el**: Separate module for tag definitions
- Tag instances stored in DOM attributes
- Event handlers accessed through DOM
- Mixed concerns: logic in DOM layer

### After (New Architecture)

- **etaf-vdom.el**: Single source for tag metadata
- Tag metadata stored in VNode structure
- Event handlers accessed through VNode layer
- Clean separation: logic in VNode, structure in DOM

## VNode Structure

```elisp
(:id unique-id                    ; Unique identifier
 :type element                    ; element, text, comment, fragment
 :tag button                      ; Tag name
 :props (:class "btn")            ; Original ETML props
 :dom (button ((class . "btn")) "Click Me")  ; Clean DOM
 :tag-metadata                    ; Tag metadata (NEW)
   (:tag button
    :self-closing nil
    :on-click #<function>
    :hover-style ((background-color . "#e5e7eb")))
 :state                           ; Interactive state (NEW)
   (:hovered nil :focused nil :active nil :disabled nil)
 :children (...)                  ; Child VNodes
 :mounted-p nil                   ; Lifecycle state
 :hooks nil)                      ; Lifecycle hooks
```

## Tag Metadata

Tag metadata replaces the old tag-instance system:

```elisp
;; Old approach (etaf-etml-tag.el)
(define-etaf-etml-tag button
  :hover-style '((background-color . "#e5e7eb"))
  :on-click (lambda (event) ...))

;; New approach (built into etaf-vdom.el)
(etaf-vdom-create-tag-metadata 'button attrs children)
;; Returns:
(:tag button
 :self-closing nil
 :children-allowed t
 :on-click #<function>
 :hover-style ((background-color . "#e5e7eb"))
 :active-style ((background-color . "#d1d5db"))
 :disabled-style ((background-color . "#f3f4f6") ...))
```

## Built-in Tags

Built-in tags are now defined in `etaf-vdom.el`:

- **Block-level**: div, p, h1-h6, header, footer, section, article, etc.
- **Inline**: span, em, strong, b, i, u, s, etc.
- **Inline-block**: button, input, img, video, canvas, svg
- **Self-closing**: br, hr, img, input
- **Interactive**: a, button, input, textarea, summary

Tag styles come from the User Agent Stylesheet (etaf-ua-stylesheet.el), following browser standards.

## Event Handling

Event handlers are now stored in VNode metadata and accessed through helper functions:

```elisp
;; Old approach
(etaf-etml-tag-setup-keymap tag-instance)
(etaf-etml-tag--help-echo-handler window obj pos)

;; New approach
(etaf-vdom-setup-keymap tag-metadata vnode-or-state)
(etaf-vdom-help-echo-handler window obj pos)
```

## Interactive Capabilities

Interactive elements (buttons, links, inputs) have their metadata managed in the VNode layer:

1. **At ETML compilation**: `etaf-vdom-create-tag-metadata` creates metadata
2. **At layout rendering**: `etaf-vdom-setup-keymap` creates keymaps
3. **At buffer display**: Text properties reference tag-metadata (not tag-instance)

## Migration Guide

### For Module Developers

If you were using `etaf-etml-tag` functions:

**Remove these:**
```elisp
(require 'etaf-etml-tag)
(etaf-etml-tag-defined-p tag)
(etaf-etml-tag-has-interactive-capability-p tag)
(etaf-etml-tag-create-instance tag attrs children)
(etaf-etml-tag-setup-keymap tag-instance)
```

**Use these instead:**
```elisp
(require 'etaf-vdom)
(etaf-vdom-tag-defined-p tag)
(etaf-vdom-has-interactive-capability-p vnode)
(etaf-vdom-create-tag-metadata tag attrs children)
(etaf-vdom-setup-keymap tag-metadata vnode-or-state)
```

### For Custom Tags

Custom tags should now be defined through VNode metadata in components, not through `define-etaf-etml-tag`.

## Benefits

1. **Cleaner Architecture**: Follows Vue 3's proven design patterns
2. **Better Separation**: Logic (VNode) vs Structure (DOM)
3. **Browser Standards**: Aligns with how browsers work
4. **Maintainability**: Single source of truth for tag metadata
5. **Extensibility**: Easier to add new features to VNode layer

## Files Changed

### Removed
- `etaf-etml-tag.el` - Functionality moved to etaf-vdom.el
- `examples/etaf-etml-tag-example.el` - Examples no longer needed
- `tests/etaf-etml-tag-tests.el` - Tests integrated into vdom tests

### Modified
- `etaf-vdom.el` - Added tag metadata support
- `etaf-etml.el` - Uses VNode metadata instead of tag-instance
- `etaf-render.el` - Works with VNode-generated DOM
- `etaf-layout-string.el` - Accesses metadata from VNode
- `etaf.el` - Removed etaf-etml-tag require

### Unchanged
- CSS system (etaf-css*.el)
- Component system (etaf-component.el)
- Layout system (etaf-layout*.el)
- Event system (etaf-event.el)

## Testing

All existing functionality is preserved. The changes are internal refactoring to improve architecture while maintaining the same user-facing API.

## References

- [Vue 3 Rendering Mechanism](https://cn.vuejs.org/guide/extras/rendering-mechanism)
- [Vue 3 VNode Specification](https://github.com/vuejs/core/tree/main/packages/runtime-core/src)
- ETAF Virtual DOM Documentation: `docs/VIRTUAL-DOM.md`
