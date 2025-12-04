# ETAF Component System

*Support for both Vue 2 Options API and Vue 3 Composition API*

## Table of Contents

- [Overview](#overview)
- [API Styles](#api-styles)
  - [Vue 3 Composition API](#vue-3-composition-api)
  - [Vue 2 Options API](#vue-2-options-api)
- [Quick Start](#quick-start)
  - [Composition API Example](#composition-api-example)
  - [Options API Example](#options-api-example)
- [Component Basics](#component-basics)
- [Props](#props)
- [Setup Function (Composition API)](#setup-function)
- [Options API](#options-api-detailed)
  - [data](#data)
  - [methods](#methods)
  - [computed (Options)](#computed-options)
  - [watch (Options)](#watch-options)
  - [Lifecycle Hooks](#lifecycle-hooks)
- [Templates](#templates)
- [Reactive System](#reactive-system)
  - [ref](#ref)
  - [computed](#computed)
  - [watch](#watch)
  - [watchEffect](#watcheffect)
  - [reactive](#reactive)
- [Slots](#slots)
- [Component Lifecycle](#component-lifecycle)
- [Comparison: Vue 2 vs Vue 3](#comparison-with-vue)
- [API Reference](#api-reference)
- [Examples](#examples)

## Overview

ETAF's component system now supports **both** Vue 2's Options API and Vue 3's Composition API, giving you flexibility in how you write components. Choose the style that best fits your needs or use both styles in the same project!

- **Declarative Components** - Define reusable UI components with props and state
- **Reactive State Management** - Automatic dependency tracking and updates
- **Two API Styles** - Options API (Vue 2) or Composition API (Vue 3)
- **Type Safety** - Structured component definitions with clear contracts

The component system is built on three main pillars:

1. **Component Definition** - Using `etaf-define-component` macro
2. **Reactive System** - ref, computed, watch for state management  
3. **Template Rendering** - Integration with ETAF's TML templating

## API Styles

### Vue 3 Composition API

The Composition API uses a `setup` function to define reactive state and logic:

```elisp
(etaf-define-component my-counter
  :props '(:initial)
  :setup (lambda (props)
           (let* ((count (etaf-ref 0))
                  (increment (lambda () (etaf-ref-update count #'1+))))
             (list :count count :increment increment)))
  :template ...)
```

**Pros:**
- More flexible for complex logic
- Better code reusability through composables
- Explicit reactive references
- Better TypeScript support (in JavaScript)

### Vue 2 Options API

The Options API uses data, methods, computed, and watch options:

```elisp
(etaf-define-component my-counter
  :props '(:initial)
  :data (lambda () (list :count 0))
  :methods (list :increment (lambda () ...))
  :computed (list :doubled (lambda () ...))
  :watch (list :count (lambda (new old) ...))
  :template ...)
```

**Pros:**
- Familiar to Vue 2 developers
- More organized structure for simple components
- Clear separation of concerns
- Less boilerplate for simple cases

## Quick Start

### Composition API Example

Here's a simple counter using the Composition API:

```elisp
(require 'etaf-component)

;; Define a counter component using Composition API
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
                            "Count: " 
                            ,(format "%d" (etaf-ref-get 
                                           (plist-get data :count)))))))

;; Use the component
(etaf-paint-to-buffer "*demo*"
  '(my-counter :initial-count 5))
```

### Options API Example

The same counter using the Options API:

```elisp
(require 'etaf-component)

;; Define a counter component using Options API
(etaf-define-component my-counter
  :props '(:initial-count)
  :data (lambda ()
          (list :count 0))
  :methods (list
            :increment (lambda ()
                        (let ((count-ref (plist-get this :count)))
                          (etaf-ref-update count-ref #'1+))))
  :computed (list
             :doubled (lambda ()
                       (let ((count-ref (plist-get this :count)))
                         (* 2 (etaf-ref-get count-ref)))))
  :mounted (lambda ()
            (message "Counter mounted!"))
  :template (lambda (data)
              `(div :class "counter"
                    (button :on-click ,(plist-get data :increment)
                            "Count: " 
                            ,(format "%d" (etaf-ref-get 
                                           (plist-get data :count)))))))

;; Use the component  
(etaf-paint-to-buffer "*demo*"
  '(my-counter :initial-count 5))
```

## Component Basics

### Defining a Component

Components are defined using the `etaf-define-component` macro with either API style:

**Composition API:**
```elisp
(etaf-define-component component-name
  :props '(:prop1 :prop2)
  :setup setup-function
  :template template-function)
```

**Options API:**
```elisp
(etaf-define-component component-name
  :props '(:prop1 :prop2)
  :data data-function
  :methods methods-plist
  :computed computed-plist
  :watch watch-plist
  :mounted mounted-hook
  :template template-function)
```

### Component Structure

A component definition can include:

**Common to both APIs:**
- **name** - Symbol identifying the component
- **:props** - List of properties the component accepts
- **:template** - Template for rendering (function or s-expression)
- **:emits** (optional) - List of events the component can emit

**Composition API specific:**
- **:setup** - Function that sets up reactive state and methods

**Options API specific:**
- **:data** - Function returning initial reactive data
- **:methods** - Plist of method functions
- **:computed** - Plist of computed property getters
- **:watch** - Plist of watcher functions
- **:mounted/:updated/:unmounted** - Lifecycle hooks

### Using Components

Once defined, components can be used like regular HTML elements:

```elisp
;; Simple usage
(my-component)

;; With props
(my-component :title "Hello" :count 42)

;; With children (slots)
(my-component :title "Container"
  (p "Child 1")
  (p "Child 2"))
```

## Props

Props are the way to pass data from parent to child components.

### Declaring Props

```elisp
(etaf-define-component user-card
  :props '(:name :email :avatar)
  :template (lambda (data)
              `(div :class "user-card"
                    (img :src ,(plist-get data :avatar))
                    (h3 ,(plist-get data :name))
                    (p ,(plist-get data :email)))))
```

### Using Props

Props are automatically extracted and passed to the setup function and template:

```elisp
;; Parent component uses it:
(user-card :name "Alice" 
           :email "alice@example.com"
           :avatar "/images/alice.png")
```

### Advanced Props (Future)

Future versions may support prop validation:

```elisp
:props '((:name :type string :required t)
         (:count :type number :default 0)
         (:disabled :type boolean))
```

## Setup Function

The setup function is where you define the component's reactive state and methods. It runs once when the component is created.

### Basic Setup

```elisp
:setup (lambda (props)
         (let* ((count (etaf-ref 0))
                (increment (lambda ()
                            (etaf-ref-update count #'1+))))
           ;; Return data and methods for the template
           (list :count count
                 :increment increment)))
```

### Setup Return Value

The setup function returns a plist of:
- **Reactive References** - Created with `etaf-ref`
- **Computed Values** - Created with `etaf-computed`
- **Methods** - Lambda functions
- **Static Data** - Regular values

### Accessing Props

Props are passed as the first argument:

```elisp
:setup (lambda (props)
         (let ((initial (plist-get props :initial-value)))
           (list :value (etaf-ref initial))))
```

## Options API (Detailed)

The Options API provides a structured way to define components using distinct options for data, methods, computed properties, and watchers.

### data

The `:data` option is a function that returns the initial state as a plist. All values are automatically converted to reactive refs.

```elisp
(etaf-define-component my-form
  :data (lambda ()
          (list :username ""
                :email ""
                :age 0
                :terms-accepted nil)))
```

**Key Points:**
- Data function is called once when component is created
- All values become reactive refs automatically
- Access with `(plist-get this :property-name)`
- Values are wrapped in refs, so use `etaf-ref-get` and `etaf-ref-set`

### methods

The `:methods` option is a plist of functions that can be called from the template or other methods.

```elisp
(etaf-define-component todo-item
  :data (lambda ()
          (list :done nil
                :text ""))
  :methods (list
            :toggle (lambda ()
                     (let ((done-ref (plist-get this :done)))
                       (etaf-ref-set done-ref
                                    (not (etaf-ref-get done-ref)))))
            :update-text (lambda (new-text)
                          (let ((text-ref (plist-get this :text)))
                            (etaf-ref-set text-ref new-text)))
            :reset (lambda ()
                    (let ((done-ref (plist-get this :done))
                          (text-ref (plist-get this :text)))
                      (etaf-ref-set done-ref nil)
                      (etaf-ref-set text-ref "")))))
```

**Key Points:**
- Methods have access to `this` - the component's data
- Methods can call other methods via `(plist-get this :method-name)`
- Methods are automatically bound to the component context

### computed (Options)

The `:computed` option defines derived state that automatically updates when dependencies change.

```elisp
(etaf-define-component shopping-cart
  :data (lambda ()
          (list :items '()
                :tax-rate 0.08))
  :computed (list
             :subtotal (lambda ()
                        (let ((items-ref (plist-get this :items)))
                          (apply #'+ (mapcar (lambda (item)
                                              (plist-get item :price))
                                            (etaf-ref-get items-ref)))))
             :tax (lambda ()
                   (let ((subtotal-computed (plist-get this :subtotal))
                         (tax-rate-ref (plist-get this :tax-rate)))
                     (* (etaf-computed-get subtotal-computed)
                        (etaf-ref-get tax-rate-ref))))
             :total (lambda ()
                     (let ((subtotal-computed (plist-get this :subtotal))
                           (tax-computed (plist-get this :tax)))
                       (+ (etaf-computed-get subtotal-computed)
                          (etaf-computed-get tax-computed))))))
```

**Key Points:**
- Computed properties are cached and only recompute when dependencies change
- Access computed values with `etaf-computed-get`
- Computed properties can depend on other computed properties
- Use `this` to access component data and other computed properties

### watch (Options)

The `:watch` option allows you to react to data changes.

```elisp
(etaf-define-component search-input
  :data (lambda ()
          (list :query ""
                :results '()))
  :watch (list
          :query (lambda (new-val old-val)
                  (message "Search query changed: %s -> %s" old-val new-val)
                  ;; Perform search with new query
                  (when (> (length new-val) 2)
                    ;; Trigger search...
                    )))
  :methods (list
            :search (lambda ()
                     ;; Search logic here
                     )))
```

**Key Points:**
- Watchers receive `(new-value old-value)` as arguments
- Watchers are called whenever the watched property changes
- Watch works with both data properties and computed properties
- Use watchers for side effects like API calls, logging, etc.

### Lifecycle Hooks

The Options API supports lifecycle hooks for component initialization and cleanup.

```elisp
(etaf-define-component data-fetcher
  :props '(:user-id)
  :data (lambda ()
          (list :user-data nil
                :loading t
                :error nil))
  :mounted (lambda ()
            (message "Component mounted, fetching data...")
            ;; Fetch data when component mounts
            (let ((user-id (plist-get props :user-id)))
              ;; Async data fetch would go here
              ))
  :updated (lambda ()
            (message "Component updated"))
  :unmounted (lambda ()
              (message "Component unmounted, cleaning up...")
              ;; Cleanup subscriptions, timers, etc.
              ))
```

**Available Hooks:**
- **:mounted** - Called when component is first rendered and added to the DOM
- **:updated** - Called when component re-renders due to data changes
- **:unmounted** - Called when component is removed from the DOM

**Note:** Currently, `:mounted` is fully supported. `:updated` and `:unmounted` hooks require deeper integration with the virtual DOM lifecycle system.

## Templates

Templates define how components render. They can be static or dynamic.

### Function Template

Most common - a function that receives data and returns ETML:

```elisp
:template (lambda (data)
            `(div :class "widget"
                  (h2 "Count: " ,(etaf-ref-get (plist-get data :count)))
                  (button :on-click ,(plist-get data :increment)
                          "Increment")))
```

### Static Template

For simple components without dynamic data:

```elisp
:template '(div :class "static-component"
                (p "This never changes"))
```

### Template Context

Templates have access to:
- All data returned from setup
- Props (merged into data)
- Special `$slots` prop for children

## Reactive System

ETAF's reactive system automatically tracks dependencies and triggers updates when data changes.

### ref

Create a reactive reference to a value:

```elisp
(let ((count (etaf-ref 0)))
  ;; Read value
  (etaf-ref-get count)  ; => 0
  
  ;; Set value (triggers updates)
  (etaf-ref-set count 5)
  
  ;; Update with function
  (etaf-ref-update count #'1+))
```

**Key Points:**
- Use `etaf-ref` to create reactive references
- Use `etaf-ref-get` to read (tracks dependencies)
- Use `etaf-ref-set` to write (triggers updates)
- Use `etaf-ref-update` for functional updates

### computed

Create derived reactive values:

```elisp
(let* ((price (etaf-ref 100))
       (quantity (etaf-ref 2))
       (total (etaf-computed
               (lambda ()
                 (* (etaf-ref-get price)
                    (etaf-ref-get quantity))))))
  
  (etaf-computed-get total)  ; => 200
  
  ;; Change dependency
  (etaf-ref-set quantity 3)
  
  ;; Automatically recomputes
  (etaf-computed-get total)  ; => 300
)
```

**Features:**
- Lazy evaluation - only computes when accessed
- Automatic caching - only recomputes when dependencies change
- Automatic dependency tracking
- Can depend on other computed values

### watch

Watch a specific reactive source and run a callback when it changes:

```elisp
(let* ((count (etaf-ref 0))
       (stop (etaf-watch
              count
              (lambda (new-val old-val)
                (message "Count changed: %s -> %s" old-val new-val)))))
  
  (etaf-ref-set count 1)  ; Logs: "Count changed: 0 -> 1"
  (etaf-ref-set count 2)  ; Logs: "Count changed: 1 -> 2"
  
  ;; Stop watching
  (funcall stop)
  
  (etaf-ref-set count 3)  ; No log
)
```

**Options:**
```elisp
(etaf-watch source callback 
  '(:immediate t))  ; Run callback immediately with current value
```

### watchEffect

Automatically track dependencies and run effect when they change:

```elisp
(let* ((firstName (etaf-ref "John"))
       (lastName (etaf-ref "Doe"))
       (stop (etaf-watch-effect
              (lambda ()
                (message "Name: %s %s"
                        (etaf-ref-get firstName)
                        (etaf-ref-get lastName))))))
  
  ;; Immediately logs: "Name: John Doe"
  
  (etaf-ref-set firstName "Jane")  ; Logs: "Name: Jane Doe"
  (etaf-ref-set lastName "Smith")  ; Logs: "Name: Jane Smith"
  
  ;; Stop effect
  (funcall stop)
)
```

**Key Differences from watch:**
- Runs immediately
- Automatically tracks all accessed refs
- No need to specify sources explicitly
- Most commonly used for side effects

### reactive

Create a reactive object from a plist:

```elisp
(let ((user (etaf-reactive '(:name "Alice" :age 30))))
  
  ;; Read values
  (etaf-reactive-get user :name)  ; => "Alice"
  
  ;; Update values
  (etaf-reactive-set user :name "Bob")
  (etaf-reactive-set user :age 31)
  
  ;; Watch changes
  (etaf-watch-effect
   (lambda ()
     (message "User: %s, Age: %d"
             (etaf-reactive-get user :name)
             (etaf-reactive-get user :age))))
  
  ;; Convert back to plist
  (etaf-reactive-to-plist user)  ; => (:name "Bob" :age 31)
)
```

**When to Use:**
- Multiple related properties
- Object-like data structures
- Need to add properties dynamically

## Slots

Slots allow components to accept children elements.

### Basic Slots

Children are passed via the special `:$slots` prop:

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

;; Usage with slots
(card :title "My Card"
  (p "This is the card content")
  (button "Action"))
```

### Checking for Slots

```elisp
:template (lambda (data)
            (let ((slots (plist-get data :$slots)))
              (if slots
                  `(div ,@slots)
                `(div (p "No content provided")))))
```

## Component Lifecycle

Components integrate with ETAF's Virtual DOM lifecycle hooks:

### Mounted Hook

Called when component is first rendered:

```elisp
:setup (lambda (props)
         (let ((data (etaf-ref nil)))
           ;; Fetch data when component mounts
           (etaf-watch-effect
            (lambda ()
              ;; This runs on mount and whenever dependencies change
              (setq data (fetch-data))))
           (list :data data)))
```

### Update Hook

Use `watch` or `watchEffect` to respond to changes:

```elisp
:setup (lambda (props)
         (let ((count (etaf-ref 0)))
           ;; Watch for changes
           (etaf-watch count
             (lambda (new-val old-val)
               (message "Count updated: %s" new-val)))
           (list :count count)))
```

### Cleanup

Watch and watchEffect return stop functions for cleanup:

```elisp
:setup (lambda (props)
         (let* ((count (etaf-ref 0))
                (stop (etaf-watch-effect
                       (lambda ()
                         ;; Effect logic
                         ))))
           ;; Save stop function if needed
           (list :count count
                 :cleanup stop)))
```

## Comparison with Vue

ETAF supports both Vue 2's Options API and Vue 3's Composition API:

### API Comparison Table

| Feature | Vue 2 | Vue 3 | ETAF |
|---------|-------|-------|------|
| Component Definition | `Vue.component()` / `export default` | `defineComponent()` | `etaf-define-component` |
| **Options API** |
| Data | `data()` | `data()` | `:data` function |
| Methods | `methods: {}` | `methods: {}` | `:methods` plist |
| Computed | `computed: {}` | `computed: {}` | `:computed` plist |
| Watch | `watch: {}` | `watch: {}` | `:watch` plist |
| Lifecycle - Mounted | `mounted()` | `mounted()` | `:mounted` |
| Lifecycle - Updated | `updated()` | `updated()` | `:updated` |
| Lifecycle - Unmounted | `beforeDestroy()` | `unmounted()` | `:unmounted` |
| **Composition API** |
| Setup | Not available | `setup()` | `:setup` |
| Reactive Refs | Not available | `ref()` | `etaf-ref` |
| Computed Values | Not available | `computed()` | `etaf-computed` |
| Watch | Not available | `watch()` | `etaf-watch` |
| Watch Effect | Not available | `watchEffect()` | `etaf-watch-effect` |
| Reactive Objects | Not available | `reactive()` | `etaf-reactive` |
| **Common** |
| Props | `props: []` | `props: []` | `:props` list |
| Template | `template: ""` | `template: ""` | `:template` |
| Emits | `$emit()` | `emits: []` | `:emits` list |
| Slots | `<slot>` | `<slot>` | `:$slots` prop |

### Key Differences from Vue

1. **Language** - Vue uses JavaScript, ETAF uses Emacs Lisp
2. **Templates** - Vue uses HTML-like syntax, ETAF uses S-expressions (ETML)
3. **Data Structures** - ETAF uses plists instead of JavaScript objects
4. **Rendering** - ETAF renders to text buffers, Vue renders to DOM
5. **Context Binding** - ETAF uses `this` variable in Options API (similar to Vue 2)

### Design Principles (from Vue)

All three (Vue 2, Vue 3, and ETAF) share these principles:

- **Declarative Rendering**
- **Component-Based Architecture**
- **Reactive Data Binding**
- **Automatic Dependency Tracking**

ETAF's dual API support means:
- **Options API** - Great for beginners and simple components (like Vue 2)
- **Composition API** - Better for complex logic and code reuse (like Vue 3)
- **Your Choice** - Use either or both styles in the same project!

### Migration from Vue

If you're familiar with Vue, here's how to think about ETAF:

**From Vue 2:**
```javascript
// Vue 2
export default {
  data() {
    return { count: 0 }
  },
  methods: {
    increment() { this.count++ }
  }
}
```

```elisp
;; ETAF Options API
(etaf-define-component my-component
  :data (lambda () (list :count 0))
  :methods (list
            :increment (lambda ()
                        (etaf-ref-update (plist-get this :count) #'1+))))
```

**From Vue 3:**
```javascript
// Vue 3
import { ref } from 'vue'
export default {
  setup() {
    const count = ref(0)
    const increment = () => count.value++
    return { count, increment }
  }
}
```

```elisp
;; ETAF Composition API
(etaf-define-component my-component
  :setup (lambda (props)
           (let* ((count (etaf-ref 0))
                  (increment (lambda () (etaf-ref-update count #'1+))))
             (list :count count :increment increment))))
```

## API Reference

### Component Management

#### `etaf-define-component`
```elisp
(etaf-define-component name &rest options)
```
Define a component. Returns the component name.

**Options:**
- `:props` - List of prop names
- `:setup` - Setup function (props) → data plist
- `:template` - Template function or s-expression
- `:emits` - List of emitted events

#### `etaf-component-get`
```elisp
(etaf-component-get name)
```
Get component definition by name.

#### `etaf-component-defined-p`
```elisp
(etaf-component-defined-p name)
```
Check if component is registered.

#### `etaf-component-list-all`
```elisp
(etaf-component-list-all)
```
List all registered component names.

### Component Definition Options

#### Composition API Options

- **:setup** - Setup function `(lambda (props) -> plist)`
  - Receives props, returns reactive data and methods
  - Has access to all Composition API functions (ref, computed, watch, etc.)

#### Options API Options

- **:data** - Data function `(lambda () -> plist)`
  - Returns initial component data
  - All values automatically become reactive refs
  
- **:methods** - Methods plist `(list :method1 fn1 :method2 fn2 ...)`
  - Functions that modify component state
  - Have access to `this` (component data)
  
- **:computed** - Computed properties plist `(list :prop1 getter1 :prop2 getter2 ...)`
  - Derived state that updates automatically
  - Getters have access to `this`
  
- **:watch** - Watchers plist `(list :prop1 watcher1 :prop2 watcher2 ...)`
  - Watch functions receive `(new-value old-value)`
  - Triggered when watched property changes
  
- **:mounted** - Lifecycle hook `(lambda () ...)`
  - Called when component is first rendered
  
- **:updated** - Lifecycle hook `(lambda () ...)`
  - Called when component re-renders (requires vdom integration)
  
- **:unmounted** - Lifecycle hook `(lambda () ...)`
  - Called when component is removed (requires vdom integration)

#### Common Options

- **:props** - List of prop names `'(:prop1 :prop2 ...)`
- **:template** - Template function or s-expression
- **:render** - Custom render function (advanced)
- **:emits** - List of emittable events `'(:event1 :event2 ...)`

### Reactive System

#### `etaf-ref`
```elisp
(etaf-ref initial-value)
```
Create a reactive reference.

#### `etaf-ref-get`
```elisp
(etaf-ref-get ref)
```
Get the current value of a ref.

#### `etaf-ref-set`
```elisp
(etaf-ref-set ref value)
```
Set a new value and trigger updates.

#### `etaf-ref-update`
```elisp
(etaf-ref-update ref update-fn)
```
Update ref by applying function to current value.

#### `etaf-computed`
```elisp
(etaf-computed getter-fn)
```
Create a computed value.

#### `etaf-computed-get`
```elisp
(etaf-computed-get computed)
```
Get the computed value (recomputes if dirty).

#### `etaf-watch`
```elisp
(etaf-watch source callback &optional options)
```
Watch a ref or computed value. Returns stop function.

**Options:**
- `:immediate` - Run callback immediately

#### `etaf-watch-effect`
```elisp
(etaf-watch-effect effect-fn)
```
Run effect and re-run when dependencies change. Returns stop function.

#### `etaf-reactive`
```elisp
(etaf-reactive plist)
```
Create a reactive object.

#### `etaf-reactive-get`
```elisp
(etaf-reactive-get reactive key)
```
Get value from reactive object.

#### `etaf-reactive-set`
```elisp
(etaf-reactive-set reactive key value)
```
Set value in reactive object.

#### `etaf-reactive-to-plist`
```elisp
(etaf-reactive-to-plist reactive)
```
Convert reactive object to plain plist.

## Examples

### Example 1: Simple Button

```elisp
(etaf-define-component simple-button
  :props '(:label :variant)
  :template (lambda (data)
              (let ((label (plist-get data :label))
                    (variant (or (plist-get data :variant) "primary")))
                `(button :class ,(format "btn btn-%s" variant)
                         ,label))))

;; Usage
(simple-button :label "Click Me" :variant "success")
```

### Example 2: Counter with State

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
                    (button :on-click ,(plist-get data :reset) "Reset"))))

;; Usage
(counter :initial 10)
```

### Example 3: Todo List

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
                                  :placeholder "What needs to be done?")
                           (button :on-click ,(plist-get data :add-item)
                                   "Add"))
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
                           ,(format "%d items left"
                                   (etaf-computed-get (plist-get data :remaining))))))))

;; Usage
(todo-list :initial-items '((:text "Learn ETAF" :done nil)
                            (:text "Build app" :done nil)))
```

### Example 4: Form with Validation

```elisp
(etaf-define-component user-form
  :setup (lambda (props)
           (let* ((form (etaf-reactive '(:name "" :email "" :age nil)))
                  (errors (etaf-ref '()))
                  (validate (lambda ()
                              (let ((errs '()))
                                (when (string-empty-p (etaf-reactive-get form :name))
                                  (push '(:name . "Name is required") errs))
                                (when (string-empty-p (etaf-reactive-get form :email))
                                  (push '(:email . "Email is required") errs))
                                (etaf-ref-set errors errs)
                                (null errs))))
                  (submit (lambda ()
                            (when (funcall validate)
                              (message "Form submitted: %S"
                                      (etaf-reactive-to-plist form))))))
             (list :form form
                   :errors errors
                   :validate validate
                   :submit submit)))
  :template (lambda (data)
              (let ((errors (etaf-ref-get (plist-get data :errors))))
                `(form :class "user-form"
                       (div :class "form-field"
                            (label "Name:")
                            (input :type "text"
                                   :name "name")
                            ,@(when-let ((err (cdr (assq :name errors))))
                                `((span :class "error" ,err))))
                       (div :class "form-field"
                            (label "Email:")
                            (input :type "email"
                                   :name "email")
                            ,@(when-let ((err (cdr (assq :email errors))))
                                `((span :class "error" ,err))))
                       (button :type "submit"
                               :on-click ,(plist-get data :submit)
                               "Submit")))))

;; Usage
(user-form)
```

## Best Practices

### 1. Keep Components Small

Break down complex UIs into smaller, reusable components:

```elisp
;; Good
(etaf-define-component user-card ...)
(etaf-define-component user-avatar ...)
(etaf-define-component user-bio ...)

;; Instead of one large component
```

### 2. Use Computed for Derived State

Don't duplicate state, compute it:

```elisp
;; Good
(let ((items (etaf-ref '(...)))
      (active-items (etaf-computed
                     (lambda ()
                       (seq-filter #'is-active (etaf-ref-get items))))))
  ...)

;; Avoid
(let ((items (etaf-ref '(...)))
      (active-items (etaf-ref '(...))))  ; Must keep in sync manually
  ...)
```

### 3. Use watchEffect for Side Effects

Prefer `watchEffect` over `watch` when you don't need old values:

```elisp
;; Good - automatic dependency tracking
(etaf-watch-effect
 (lambda ()
   (message "User: %s" (etaf-ref-get name))))

;; Verbose - explicit dependencies
(etaf-watch name
  (lambda (new old)
    (message "User: %s" new)))
```

### 4. Name Props Clearly

Use descriptive prop names:

```elisp
;; Good
:props '(:user-name :user-email :is-admin)

;; Avoid
:props '(:name :email :flag)
```

### 5. Document Complex Components

Add docstrings to setup functions:

```elisp
:setup (lambda (props)
         "Setup for TodoList component.
         Manages a list of todo items with add/remove/toggle operations."
         ...)
```

## Troubleshooting

### Component Not Found

**Problem:** Error: "Component not registered"

**Solution:** Make sure to define the component before using it:

```elisp
;; Define first
(etaf-define-component my-component ...)

;; Then use
(my-component)
```

### Reactive Value Not Updating

**Problem:** UI doesn't update when ref changes

**Solution:** Make sure you're using `etaf-ref-set`, not direct mutation:

```elisp
;; Good
(etaf-ref-set count (1+ (etaf-ref-get count)))

;; Bad - won't trigger updates
(plist-put count :value (1+ (plist-get count :value)))
```

### Computed Not Recomputing

**Problem:** Computed value shows stale data

**Solution:** Ensure you're reading dependencies with `etaf-ref-get`:

```elisp
;; Good - tracks dependency
(etaf-computed
 (lambda ()
   (etaf-ref-get my-ref)))

;; Bad - doesn't track
(etaf-computed
 (lambda ()
   my-ref))  ; Returns ref object, not value
```

## Migration Guide

### From etaf-etml-* to etaf-*

The component system was extracted from `etaf-etml.el` to `etaf-component.el`.
**Note: Backward compatibility aliases have been removed.** You must update your code to use the new function names.

### Updating Code

To update existing code:

1. Change requires:
   ```elisp
   ;; Add this
   (require 'etaf-component)
   ```

2. Update function names (required):
   ```elisp
   ;; Find/replace - old names no longer work
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
   etaf-component-* → etaf-component-*
   ```

3. Test your code after updating all function names

## Further Reading

- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [Vue 3 Reactivity in Depth](https://vuejs.org/guide/extras/reactivity-in-depth.html)
- [ETAF Virtual DOM](VIRTUAL-DOM.md)
- [ETAF Event Model](EVENT-MODEL.md)
- [Component Examples](../examples/etaf-component-examples.el)

---

*For questions or issues, please visit the [ETAF GitHub repository](https://github.com/Kinneyzhang/etaf).*
