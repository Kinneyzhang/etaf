# ETAF Reactive System

## Overview

ETAF's reactive system is inspired by Vue 3's reactivity implementation, providing a powerful and efficient way to build reactive applications in Emacs Lisp. The system automatically tracks dependencies and updates the UI when data changes.

This document explains the implementation based on Vue 3's reactivity principles, as described in the Vue 3 Reactivity documentation.

## Table of Contents

1. [Core Concepts](#core-concepts)
2. [Basic Usage](#basic-usage)
3. [Advanced Features](#advanced-features)
4. [Implementation Details](#implementation-details)
5. [Migration Guide](#migration-guide)
6. [API Reference](#api-reference)

## Core Concepts

### 1. Reactive References (ref)

The foundation of the reactive system. A `ref` is a reactive container that holds a single value.

```elisp
(let ((count (etaf-ref 0)))
  (etaf-ref-get count)    ; => 0
  (etaf-ref-set count 5)  ; Sets to 5 and triggers updates
  (etaf-ref-get count))   ; => 5
```

### 2. Computed Properties

Derived reactive values that automatically update when their dependencies change.

```elisp
(let* ((price (etaf-ref 100))
       (quantity (etaf-ref 2))
       (total (etaf-computed
               (lambda ()
                 (* (etaf-ref-get price)
                    (etaf-ref-get quantity))))))
  (etaf-computed-get total)  ; => 200
  (etaf-ref-set quantity 3)
  (etaf-computed-get total)) ; => 300 (automatically recomputed)
```

### 3. Watch

Explicitly watch a reactive source and run a callback when it changes.

```elisp
(let ((count (etaf-ref 0)))
  (etaf-watch
   count
   (lambda (new old on-invalidate)
     (message "Count changed from %s to %s" old new)))
  
  (etaf-ref-set count 5))  ; Logs: "Count changed from 0 to 5"
```

### 4. WatchEffect

Automatically track dependencies and re-run when they change.

```elisp
(let ((firstName (etaf-ref "John"))
      (lastName (etaf-ref "Doe")))
  (etaf-watch-effect
   (lambda ()
     (message "Name: %s %s"
             (etaf-ref-get firstName)
             (etaf-ref-get lastName))))
  
  (etaf-ref-set firstName "Jane"))  ; Logs: "Name: Jane Doe"
```

## Basic Usage

### Creating Reactive State

```elisp
;; Simple ref
(let ((count (etaf-ref 0)))
  ;; Read value
  (etaf-ref-get count)
  
  ;; Set value
  (etaf-ref-set count 10)
  
  ;; Update with function
  (etaf-ref-update count #'1+))

;; Reactive object (for multiple properties)
(let ((user (etaf-reactive '(:name "Alice" :age 30))))
  ;; Read property
  (etaf-reactive-get user :name)  ; => "Alice"
  
  ;; Set property
  (etaf-reactive-set user :age 31)
  
  ;; Convert to plist
  (etaf-reactive-to-plist user))  ; => (:name "Alice" :age 31)
```

### Computed Properties

Computed properties are lazily evaluated and cached:

```elisp
(let* ((items (etaf-ref '("apple" "banana" "cherry")))
       (count (etaf-computed
               (lambda ()
                 (length (etaf-ref-get items)))))
       (summary (etaf-computed
                 (lambda ()
                   (format "%d items" (etaf-computed-get count))))))
  
  (etaf-computed-get summary)  ; => "3 items"
  
  (etaf-ref-set items '("apple" "banana"))
  (etaf-computed-get summary)) ; => "2 items"
```

### Watching Changes

#### Basic Watch

```elisp
(let ((count (etaf-ref 0)))
  (etaf-watch
   count
   (lambda (new old on-invalidate)
     (message "Changed: %s -> %s" old new)))
  
  (etaf-ref-set count 1)
  (etaf-ref-set count 2))
```

#### Watch with Immediate

Run the callback immediately with the current value:

```elisp
(let ((count (etaf-ref 5)))
  (etaf-watch
   count
   (lambda (new old on-invalidate)
     (message "Value: %s" new))
   (list :immediate t)))  ; Immediately logs: "Value: 5"
```

#### Watch with Flush Options

Control when the callback executes:

```elisp
;; Sync - runs immediately
(etaf-watch count callback (list :flush 'sync))

;; Pre - runs before DOM updates (default)
(etaf-watch count callback (list :flush 'pre))

;; Post - runs after DOM updates
(etaf-watch count callback (list :flush 'post))
```

### WatchEffect

Automatic dependency tracking - the most commonly used watch function:

```elisp
(let ((firstName (etaf-ref "John"))
      (lastName (etaf-ref "Doe"))
      (title (etaf-ref "Dr.")))
  
  (etaf-watch-effect
   (lambda ()
     ;; Automatically tracks firstName and lastName
     (message "%s %s %s"
             (etaf-ref-get title)
             (etaf-ref-get firstName)
             (etaf-ref-get lastName))))
  
  ;; All of these will trigger the effect
  (etaf-ref-set firstName "Jane")
  (etaf-ref-set lastName "Smith")
  (etaf-ref-set title "Prof."))
```

## Advanced Features

### 1. Branch Switching and Dependency Cleanup

The system automatically cleans up old dependencies when effects re-run, preventing stale subscriptions:

```elisp
(let ((toggle (etaf-ref t))
      (a (etaf-ref 1))
      (b (etaf-ref 2)))
  
  (etaf-watch-effect
   (lambda ()
     (if (etaf-ref-get toggle)
         (message "A: %s" (etaf-ref-get a))
       (message "B: %s" (etaf-ref-get b)))))
  
  ;; Initially watches: toggle, a
  (etaf-ref-set toggle nil)
  ;; Now watches: toggle, b (a is automatically cleaned up)
  
  (etaf-ref-set a 10)  ; Does NOT trigger effect
  (etaf-ref-set b 20)) ; DOES trigger effect
```

### 2. Infinite Recursion Prevention

The system prevents effects from infinitely triggering themselves:

```elisp
(let ((count (etaf-ref 0)))
  (etaf-watch-effect
   (lambda ()
     (when (< (etaf-ref-get count) 5)
       ;; This would cause infinite loop without prevention
       (etaf-ref-set count (1+ (etaf-ref-get count))))))
  
  ;; Effect runs only once, not infinitely
  (etaf-ref-get count)) ; => 1
```

### 3. Effect Scheduling

Customize when and how effects run:

```elisp
(let ((count (etaf-ref 0))
      (scheduled-jobs nil))
  
  ;; Effect with custom scheduler
  (etaf-reactive-effect
   (lambda ()
     (message "Count: %s" (etaf-ref-get count)))
   (list :scheduler
         (lambda (effect)
           ;; Custom logic: add to queue instead of running immediately
           (push effect scheduled-jobs))))
  
  ;; Changes are scheduled, not executed immediately
  (etaf-ref-set count 5)
  
  ;; Manually process scheduled jobs
  (dolist (effect scheduled-jobs)
    (funcall (plist-get effect :run))))
```

### 4. Task Batching and Deduplication

Multiple synchronous changes are automatically batched:

```elisp
(let ((count (etaf-ref 0))
      (runs 0))
  
  (etaf-watch
   count
   (lambda (new old on-invalidate)
     (setq runs (1+ runs))))
  
  ;; Multiple changes
  (etaf-ref-set count 1)
  (etaf-ref-set count 2)
  (etaf-ref-set count 3)
  
  ;; Flush the queue
  (etaf-reactive--flush-jobs)
  
  ;; Effect runs only once for all changes
  runs) ; => 1
```

### 5. Race Condition Handling with onInvalidate

Handle race conditions in async operations:

```elisp
(let ((id (etaf-ref 1)))
  (etaf-watch
   id
   (lambda (new old on-invalidate)
     (let ((pending t))
       ;; Register cleanup
       (funcall on-invalidate
                (lambda ()
                  ;; Mark this request as cancelled
                  (setq pending nil)))
       
       ;; Simulate async operation
       (run-with-timer
        1 nil
        (lambda ()
          ;; Only process if not cancelled
          (when pending
            (message "Fetched data for ID: %s" new))))))
   (list :flush 'sync))
  
  ;; Fast successive changes
  (etaf-ref-set id 2)  ; Request for ID 2
  (etaf-ref-set id 3)) ; Cancels ID 2, requests ID 3
```

### 6. Stopping Watchers

All watch functions return a stop function:

```elisp
(let ((count (etaf-ref 0))
      (stop nil))
  
  (setq stop
        (etaf-watch
         count
         (lambda (new old on-invalidate)
           (message "Count: %s" new))
         (list :flush 'sync)))
  
  (etaf-ref-set count 5)  ; Logs: "Count: 5"
  
  ;; Stop watching
  (funcall stop)
  
  (etaf-ref-set count 10)) ; No log
```

## Implementation Details

### Bucket Structure (WeakMap-style)

The system uses a global bucket to store dependencies:

```
etaf-reactive--bucket
  ├─ ref-1 → {
  │     :value → [effect-a, effect-b]
  │  }
  ├─ ref-2 → {
  │     :value → [effect-c]
  │  }
  └─ computed-1 → {
        :value → [effect-d]
     }
```

### Effect Lifecycle

1. **Creation**: Effect is created with `etaf-reactive-effect`
2. **Initial Run**: Effect runs immediately (unless `:lazy t`)
3. **Dependency Tracking**: As effect runs, refs/computed are tracked
4. **Change Detection**: When a ref/computed changes, triggers effects
5. **Cleanup**: Before re-running, old dependencies are cleaned up
6. **Re-run**: Effect runs again, tracks new dependencies

### Key Components

1. **`etaf-reactive--track`**: Records dependency between effect and reactive value
2. **`etaf-reactive--trigger`**: Triggers effects that depend on a value
3. **`etaf-reactive--cleanup-effect`**: Removes effect from old dependencies
4. **`etaf-reactive-effect`**: Creates a reactive effect
5. **`etaf-reactive--queue-job`**: Queues effect for batched execution
6. **`etaf-reactive--flush-jobs`**: Processes queued effects

### Effect Stack

For nested effects, the system uses a stack:

```elisp
;; Outer effect
(etaf-watch-effect
 (lambda ()
   (message "Outer: %s" (etaf-ref-get outer-ref))
   ;; Inner effect
   (etaf-watch-effect
    (lambda ()
      (message "Inner: %s" (etaf-ref-get inner-ref))))))

;; Stack ensures correct dependency tracking:
;; 1. outer-ref → outer effect
;; 2. inner-ref → inner effect
```

## Migration Guide

### From Old Reactive System

The old system is still available for compatibility. To migrate:

**Old:**
```elisp
(let ((data (etaf-create-reactive '(:count 0))))
  (etaf-get data :count)
  (etaf-set data :count 5)
  (etaf-watch-reactive
   data
   (lambda (reactive key value)
     (message "Changed: %s = %s" key value))))
```

**New:**
```elisp
(let ((data (etaf-reactive '(:count 0))))
  (etaf-reactive-get data :count)
  (etaf-reactive-set data :count 5)
  (etaf-watch-effect
   (lambda ()
     (message "Count: %s" (etaf-reactive-get data :count)))))
```

### Best Practices

1. **Use `ref` for primitive values**, `reactive` for objects
2. **Prefer `watchEffect` over `watch`** for automatic tracking
3. **Use `computed` for derived values** instead of manually updating
4. **Use `:immediate t`** when you need initial callback execution
5. **Use `:flush 'sync'`** when you need immediate updates
6. **Use `onInvalidate`** for async operations that can be cancelled

## API Reference

### `etaf-ref (value)`

Create a reactive reference.

**Parameters:**
- `value`: Initial value

**Returns:** Ref object

**Example:**
```elisp
(let ((count (etaf-ref 0)))
  (etaf-ref-get count)
  (etaf-ref-set count 10))
```

### `etaf-ref-get (ref)`

Get the current value of a ref.

**Parameters:**
- `ref`: Ref object

**Returns:** Current value

### `etaf-ref-set (ref value)`

Set a new value and trigger effects.

**Parameters:**
- `ref`: Ref object
- `value`: New value

### `etaf-ref-update (ref fn)`

Update ref by applying a function to current value.

**Parameters:**
- `ref`: Ref object
- `fn`: Function that takes current value and returns new value

**Returns:** New value

**Example:**
```elisp
(etaf-ref-update count #'1+)  ; Increment
```

### `etaf-computed (getter)`

Create a computed value.

**Parameters:**
- `getter`: Function that computes the value (no arguments)

**Returns:** Computed object

**Example:**
```elisp
(let* ((price (etaf-ref 100))
       (tax (etaf-computed
             (lambda ()
               (* (etaf-ref-get price) 0.1)))))
  (etaf-computed-get tax))
```

### `etaf-computed-get (computed)`

Get the current value of a computed property.

**Parameters:**
- `computed`: Computed object

**Returns:** Computed value

### `etaf-watch (source callback &optional options)`

Watch a reactive source and run callback on changes.

**Parameters:**
- `source`: Ref, computed, or getter function
- `callback`: Function `(new old on-invalidate) -> void`
- `options`: Optional plist
  - `:immediate` - Run callback immediately
  - `:flush` - Execution timing ('sync', 'pre', or 'post')

**Returns:** Stop function

**Example:**
```elisp
(let ((stop (etaf-watch
             count
             (lambda (new old on-invalidate)
               (message "Changed: %s -> %s" old new))
             (list :immediate t :flush 'sync))))
  ;; Later: (funcall stop)
  )
```

### `etaf-watch-effect (effect-fn &optional options)`

Run effect immediately and re-run on dependency changes.

**Parameters:**
- `effect-fn`: Function to run
- `options`: Optional plist
  - `:flush` - Execution timing (default 'sync')

**Returns:** Stop function

**Example:**
```elisp
(etaf-watch-effect
 (lambda ()
   (message "Count: %s" (etaf-ref-get count))))
```

### `etaf-reactive (data)`

Create a reactive object from a plist.

**Parameters:**
- `data`: Plist with key-value pairs

**Returns:** Reactive object

**Example:**
```elisp
(let ((user (etaf-reactive '(:name "Alice" :age 30))))
  (etaf-reactive-get user :name)
  (etaf-reactive-set user :age 31))
```

### `etaf-reactive-get (reactive key)`

Get a property from reactive object.

**Parameters:**
- `reactive`: Reactive object
- `key`: Property key

**Returns:** Property value

### `etaf-reactive-set (reactive key value)`

Set a property in reactive object.

**Parameters:**
- `reactive`: Reactive object
- `key`: Property key
- `value`: New value

### `etaf-reactive-to-plist (reactive)`

Convert reactive object to plain plist.

**Parameters:**
- `reactive`: Reactive object

**Returns:** Plist with current values

## Performance Considerations

1. **Lazy Computed**: Computed properties only recompute when accessed and dirty
2. **Branch Cleanup**: Old dependencies are cleaned up to prevent memory leaks
3. **Batching**: Multiple changes are batched into single update cycle
4. **Deduplication**: Same effect won't run multiple times in one cycle
5. **Recursion Prevention**: Self-triggering effects are prevented

## Debugging Tips

1. **Check dependencies**: Look at `etaf-reactive--bucket` to see tracked deps
2. **Watch effect runs**: Add counters to track how many times effects run
3. **Use immediate flush**: Set `:flush 'sync` to debug timing issues
4. **Check cleanup**: Verify old dependencies are removed with branch switching

## References

- [Vue 3 Reactivity Documentation](https://vuejs.org/guide/extras/reactivity-in-depth.html)
- [Vue 3 Composition API](https://vuejs.org/guide/extras/composition-api-faq.html)
- [Vue 3 Core Source Code](https://github.com/vuejs/core)
- [ECMA-262 Set Iteration Specification](https://tc39.es/ecma262/#sec-set.prototype.forEach)
