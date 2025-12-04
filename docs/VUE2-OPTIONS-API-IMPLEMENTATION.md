# Vue 2 Options API Support - Implementation Summary

## Overview

This implementation adds comprehensive support for Vue 2's Options API to ETAF's component system while maintaining full backward compatibility with the existing Vue 3 Composition API.

## Problem Statement

The original request (in Chinese) was: "请同时支持 vue2(https://github.com/vuejs/vue) 的选项式API 和 vu3(https://github.com/vuejs/core) 的组合式API。"

Translation: "Please support both Vue 2's Options API and Vue 3's Composition API simultaneously."

## Solution

### Architecture

The implementation uses an internal conversion strategy:
1. Components can be defined with either Options API or Composition API
2. Options API components are automatically converted to Composition API internally
3. Both styles use the same underlying reactive system
4. Zero breaking changes to existing code

### Key Components

#### 1. Extended Component Definition (`etaf-component-create`)

Added support for these Options API options:
- `:data` - Function returning initial reactive data
- `:methods` - Plist of method functions  
- `:computed` - Plist of computed property getters
- `:watch` - Plist of watcher functions
- `:mounted` - Lifecycle hook (component mounted)
- `:updated` - Lifecycle hook (component updated)
- `:unmounted` - Lifecycle hook (component unmounted)

#### 2. API Converter (`etaf-component--options-to-setup`)

Converts Options API to Composition API:
- Transforms `:data` properties into reactive refs
- Wraps methods with proper `this` context binding
- Creates computed values from computed property getters
- Sets up watchers for watch properties
- Invokes lifecycle hooks at appropriate times

#### 3. Automatic Detection (`etaf-component--render`)

The renderer automatically:
- Detects which API style is being used
- Converts Options API to Composition API if needed
- Renders the component using unified internal representation

## Examples

### Vue 2 Options API Style

```elisp
(etaf-define-component my-counter
  :props '(:initial)
  :data (lambda ()
          (list :count 0))
  :methods (list
            :increment (lambda ()
                        (etaf-ref-update (plist-get this :count) #'1+)))
  :computed (list
             :doubled (lambda ()
                       (* 2 (etaf-ref-get (plist-get this :count)))))
  :watch (list
          :count (lambda (new old)
                  (message "Count changed: %s -> %s" old new)))
  :mounted (lambda ()
            (message "Counter mounted"))
  :template (lambda (data)
              `(div :class "counter"
                    (button :on-click ,(plist-get data :increment)
                            "Count: " ,(format "%d" (etaf-ref-get 
                                                     (plist-get data :count)))))))
```

### Vue 3 Composition API Style (Existing)

```elisp
(etaf-define-component my-counter
  :props '(:initial)
  :setup (lambda (props)
           (let* ((count (etaf-ref 0))
                  (doubled (etaf-computed
                           (lambda ()
                             (* 2 (etaf-ref-get count)))))
                  (increment (lambda ()
                              (etaf-ref-update count #'1+))))
             (etaf-watch count
               (lambda (new old)
                 (message "Count changed: %s -> %s" old new)))
             (list :count count
                   :doubled doubled
                   :increment increment)))
  :template (lambda (data)
              `(div :class "counter"
                    (button :on-click ,(plist-get data :increment)
                            "Count: " ,(format "%d" (etaf-ref-get 
                                                     (plist-get data :count)))))))
```

Both styles work identically!

## Files Modified

### Core Implementation
- `etaf-component.el` - Extended component system with Options API support

### Documentation
- `docs/COMPONENT-SYSTEM.md` - English documentation with Options API
- `docs/COMPONENT-SYSTEM-CN.md` - Chinese documentation with Options API
- `readme.md` - Updated to highlight dual API support

### Examples & Tests
- `examples/etaf-vue2-options-api-examples.el` - 5 comprehensive examples
- `tests/etaf-options-api-tests.el` - 13 test cases

## API Comparison

| Feature | Vue 2 | Vue 3 | ETAF |
|---------|-------|-------|------|
| Component Definition | `Vue.component()` | `defineComponent()` | `etaf-define-component` |
| **Options API** |
| Data | `data()` | `data()` | `:data` |
| Methods | `methods: {}` | `methods: {}` | `:methods` plist |
| Computed | `computed: {}` | `computed: {}` | `:computed` plist |
| Watch | `watch: {}` | `watch: {}` | `:watch` plist |
| Mounted Hook | `mounted()` | `mounted()` | `:mounted` |
| **Composition API** |
| Setup | N/A | `setup()` | `:setup` |
| Reactive Refs | N/A | `ref()` | `etaf-ref` |
| Computed Values | N/A | `computed()` | `etaf-computed` |
| Watch | N/A | `watch()` | `etaf-watch` |
| Watch Effect | N/A | `watchEffect()` | `etaf-watch-effect` |

## Benefits

### For Users
1. **Familiarity** - Vue 2 developers can use Options API immediately
2. **Flexibility** - Choose the API style that fits your needs
3. **Gradual Migration** - Mix both styles in same project
4. **Learning Curve** - Options API is easier for beginners

### For Codebase
1. **No Breaking Changes** - Existing Composition API code works unchanged
2. **Unified Internals** - Both APIs use same reactive system
3. **Code Reuse** - Options API builds on existing Composition API
4. **Maintainability** - Single reactive system to maintain

## Testing

Created comprehensive test suite (`etaf-options-api-tests.el`) with 13 tests covering:
- Basic Options API features (data, methods, computed, watch, lifecycle)
- Composition API still works (backward compatibility)
- Integration tests with full component definitions
- Edge cases (empty data, multiple computed properties, etc.)
- Both APIs can coexist in the same project

## Documentation

### English Documentation
- Detailed Options API sections in COMPONENT-SYSTEM.md
- Comparison tables between Vue 2, Vue 3, and ETAF
- Migration guides from both Vue versions
- Complete API reference

### Chinese Documentation
- Full translation of Options API documentation
- Chinese examples and explanations
- Localized comparison tables

### Examples
- Basic counter with Options API
- Price calculator with computed properties
- Search box with watchers
- Complete Todo List application
- Side-by-side comparison of both API styles

## Implementation Details

### `this` Context Binding

In Options API, methods and computed properties access component data via `this`:

```elisp
:methods (list
          :increment (lambda ()
                      (let ((count-ref (plist-get this :count)))
                        (etaf-ref-update count-ref #'1+))))
```

The `this` variable is dynamically bound during component rendering by the conversion function.

### Reactive Data Conversion

All `:data` properties are automatically wrapped in `etaf-ref`:

```elisp
:data (lambda () (list :count 0 :message "Hello"))
```

Internally becomes:

```elisp
(list :count (etaf-ref 0) :message (etaf-ref "Hello"))
```

### Lifecycle Hook Integration

Lifecycle hooks are called at appropriate times:
- `:mounted` - Called immediately during setup
- `:updated` - Requires vdom integration (placeholder)
- `:unmounted` - Requires vdom integration (placeholder)

## Future Enhancements

Potential improvements:
1. Full `:updated` and `:unmounted` hook integration with vdom lifecycle
2. Prop validation and type checking
3. Named slots support in Options API
4. Provide/inject for dependency injection
5. Mixins for code reuse across components

## Conclusion

This implementation successfully adds Vue 2 Options API support to ETAF while:
- Maintaining 100% backward compatibility
- Using elegant internal conversion to Composition API
- Providing comprehensive documentation and examples
- Following Vue's design principles and API surface

Users can now choose the component style that best fits their needs, making ETAF more accessible to both Vue 2 and Vue 3 developers.
