# ETAF Component System

*Vue 3-inspired Component System for ETAF*

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Component Basics](#component-basics)
- [Props](#props)
- [Setup Function](#setup-function)
- [Templates](#templates)
- [Reactive System](#reactive-system)
  - [ref](#ref)
  - [computed](#computed)
  - [watch](#watch)
  - [watchEffect](#watcheffect)
  - [reactive](#reactive)
- [Slots](#slots)
- [Component Lifecycle](#component-lifecycle)
- [Comparison with Vue 3](#comparison-with-vue-3)
- [API Reference](#api-reference)
- [Examples](#examples)

## Overview

ETAF's component system is inspired by Vue 3's Composition API, bringing reactive components to Emacs. It provides:

- **Declarative Components** - Define reusable UI components with props and state
- **Reactive State Management** - Automatic dependency tracking and updates
- **Composition API** - Flexible and composable logic organization
- **Type Safety** - Structured component definitions with clear contracts

The component system is built on three main pillars:

1. **Component Definition** - Using `etaf-define-component` macro
2. **Reactive System** - ref, computed, watch for state management  
3. **Template Rendering** - Integration with ETAF's TML templating

## Quick Start

Here's a simple counter component:

```elisp
(require 'etaf-component)

;; Define a counter component
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

## Component Basics

### Defining a Component

Components are defined using the `etaf-define-component` macro:

```elisp
(etaf-define-component component-name
  :props '(:prop1 :prop2)
  :setup setup-function
  :template template-function)
```

### Component Structure

A component definition consists of:

- **name** - Symbol identifying the component
- **:props** - List of properties the component accepts
- **:setup** - Function that sets up reactive state and methods
- **:template** - Template for rendering (function or s-expression)
- **:emits** (optional) - List of events the component can emit

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

## Comparison with Vue 3

ETAF's component system is inspired by Vue 3's Composition API:

| Feature | Vue 3 | ETAF |
|---------|-------|------|
| Component Definition | `defineComponent()` | `etaf-define-component` |
| Reactive Refs | `ref()` | `etaf-ref` |
| Computed Values | `computed()` | `etaf-computed` |
| Watch | `watch()` | `etaf-watch` |
| Watch Effect | `watchEffect()` | `etaf-watch-effect` |
| Reactive Objects | `reactive()` | `etaf-reactive` |
| Props | `props` option | `:props` keyword |
| Setup | `setup()` | `:setup` keyword |
| Template | `template` option | `:template` keyword |
| Slots | `<slot>` | `:$slots` prop |

### Key Differences

1. **Language** - Vue uses JavaScript, ETAF uses Emacs Lisp
2. **Templates** - Vue uses HTML-like syntax, ETAF uses S-expressions
3. **Data Structures** - ETAF uses plists instead of JavaScript objects
4. **Rendering** - ETAF renders to text buffers, not DOM

### Design Principles (from Vue 3)

Both systems share these principles:

- **Composition over Inheritance**
- **Explicit over Implicit**
- **Flexible and Composable**
- **Automatic Dependency Tracking**

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
   etaf-etml-component-* → etaf-component-*
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
