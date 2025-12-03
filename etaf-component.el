;;; etaf-component.el --- Vue3-style component system for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: components, reactive, vue3
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Vue3-style Component System for ETAF
;;
;; This module provides a complete component system inspired by Vue 3's
;; Composition API. It includes:
;;
;; 1. Component Definition and Management
;;    - Define components with props, setup, and templates
;;    - Component registry for global component management
;;    - Support for slots (children) and prop validation
;;
;; 2. Reactive System (Vue 3 Composition API)
;;    - ref: Basic reactive references
;;    - computed: Derived reactive values with caching
;;    - watch: Explicit dependency watching
;;    - watchEffect: Automatic dependency tracking
;;    - reactive: Reactive objects for plist-like data
;;
;; 3. Component Lifecycle
;;    - Setup function runs before component renders
;;    - Integration with virtual DOM lifecycle hooks
;;
;; Design Philosophy (from Vue 3):
;; - Composition over Inheritance
;; - Explicit over Implicit
;; - Flexible and Composable
;; - TypeScript-friendly structure (adapted to Elisp)
;;
;; References:
;; - Vue 3 Composition API: https://vuejs.org/guide/extras/composition-api-faq.html
;; - Vue 3 Reactivity: https://vuejs.org/guide/extras/reactivity-in-depth.html
;; - Vue 3 Core: https://github.com/vuejs/core

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; Component Registry and Management
;;; ============================================================================

(defvar etaf-component-registry (make-hash-table :test 'eq)
  "Hash table storing all registered components.
Keys are component name symbols, values are component definitions.")

(defun etaf-component-create (name &rest options)
  "Create a component definition with NAME and OPTIONS.

OPTIONS is a plist that can include:

- :props - List of prop names or plist with prop definitions
  Examples:
    \\='(:title :count :disabled)
    \\='((:title :type string :required t)
      (:count :type number :default 0))

- :setup - Setup function that receives props and returns reactive state
  Signature: (lambda (props) -> plist)
  The setup function should return a plist of data and methods
  that will be available in the template.

- :template - Component template, can be:
  1. An ETML s-expression
  2. A function (lambda (data) -> ETML)
  
- :render - Custom render function (advanced)
  Signature: (lambda (data) -> ETML)
  Takes precedence over :template if provided.

- :emits - List of events the component can emit
  Example: \\='(:click :change :submit)

Returns a component definition plist."
  (let ((component (list :name name
                         :props (plist-get options :props)
                         :setup (plist-get options :setup)
                         :template (plist-get options :template)
                         :render (plist-get options :render)
                         :emits (plist-get options :emits))))
    component))

(defun etaf-component-register (name component)
  "Register COMPONENT with NAME in the global component registry.
NAME should be a symbol, COMPONENT should be a component definition."
  (puthash name component etaf-component-registry))

(defun etaf-component-unregister (name)
  "Unregister the component with NAME from the registry."
  (remhash name etaf-component-registry))

(defun etaf-component-get (name)
  "Get the component definition for NAME from the registry.
Returns nil if component is not registered."
  (gethash name etaf-component-registry))

(defun etaf-component-defined-p (name)
  "Check if a component with NAME is registered.
Returns t if component exists, nil otherwise."
  (not (null (gethash name etaf-component-registry))))

(defun etaf-component-list-all ()
  "Return a list of all registered component names."
  (let ((names nil))
    (maphash (lambda (key _value) (push key names))
             etaf-component-registry)
    (nreverse names)))

(defun etaf-component-clear-registry ()
  "Clear all registered components.
Useful for testing or resetting the component system."
  (clrhash etaf-component-registry))

;;;###autoload
(defmacro etaf-define-component (name &rest options)
  "Define a component with NAME and OPTIONS.

NAME is the component symbol (e.g., my-button, user-card).

OPTIONS is a plist that can include:
- :props - List of prop names the component accepts
- :setup - Setup function (props) -> plist of reactive data and methods
- :template - Template function (data) -> ETML sexp, or ETML sexp directly
- :emits - List of events this component can emit

This macro creates a component definition variable and registers it
in the global component registry.

Example:

  (etaf-define-component my-counter
    :props \\='(:initial-count)
    :setup (lambda (props)
             (let* ((count (etaf-ref
                            (or (plist-get props :initial-count) 0)))
                    (increment (lambda ()
                                 (etaf-ref-set
                                  count (1+ (etaf-ref-get count))))))
               (list :count count :increment increment)))
    :template (lambda (data)
                `(div :class \"counter\"
                      (span ,(format \"Count: %s\" (etaf-ref-get
                                                    (plist-get data :count))))
                      (button :on-click ,(plist-get data :increment)
                              \"Increment\"))))

After definition, the component can be used in templates:

  (my-counter :initial-count 10)

The component system integrates with ETAF's template rendering
to provide a Vue3-like component experience."
  (declare (indent defun))
  (let ((component-var (intern (format "etaf-component--%s" name))))
    `(progn
       (defvar ,component-var
         (etaf-component-create ',name ,@options)
         ,(format "Component definition for %s." name))
       (etaf-component-register ',name ,component-var)
       ',name)))

;;; ============================================================================
;;; Component Rendering Utilities
;;; ============================================================================

(defun etaf-component--extract-props (attrs prop-names)
  "Extract props from ATTRS based on PROP-NAMES.
ATTRS is a plist of attributes passed to the component.
PROP-NAMES is a list of prop symbols to extract.
Returns a plist of prop values."
  (let ((props nil))
    (dolist (prop-name prop-names)
      (let* ((key (if (keywordp prop-name) prop-name
                    (intern (concat ":" (symbol-name prop-name)))))
             (value (plist-get attrs key)))
        (when value
          (setq props (plist-put props key value)))))
    props))

(defun etaf-component--render (component attrs children data)
  "Render COMPONENT with ATTRS and CHILDREN using DATA context.
Returns the rendered ETML.

This function:
1. Extracts props from attrs based on component's prop definitions
2. Runs the component's setup function to create reactive state
3. Merges component data with parent data context
4. Renders the component using its template or render function"
  (let* ((prop-names (plist-get component :props))
         (props (etaf-component--extract-props attrs prop-names))
         (setup-fn (plist-get component :setup))
         (template-fn (plist-get component :template))
         (render-fn (plist-get component :render))
         ;; Add special $slots prop for children (Vue-like slots)
         (props-with-slots (plist-put props :$slots children)))
    ;; Run setup function if provided
    (let ((component-data (if setup-fn
                              (funcall setup-fn props-with-slots)
                            props-with-slots)))
      ;; Merge component data with parent data
      (let ((merged-data (append component-data data)))
        (cond
         ;; Custom render function takes precedence
         (render-fn
          (funcall render-fn component-data))
         ;; Template function
         ((functionp template-fn)
          (let ((template (funcall template-fn component-data)))
            ;; Note: Actual rendering is handled by etaf-etml-render
            ;; This function just returns the template
            template))
         ;; Template as ETML expression
         (template-fn
          template-fn)
         ;; No template - render children with component data
         (t
          (if children
              (if (= (length children) 1)
                  (car children)
                `(div ,@children))
            nil)))))))

(defun etaf-component-is-component-p (tag)
  "Check if TAG is a registered component.
Returns t if TAG is a component name, nil otherwise."
  (etaf-component-defined-p tag))

;;; ============================================================================
;;; Reactive System - Ref
;;; ============================================================================

(defvar etaf-reactive--current-effect nil
  "The currently running effect, used for dependency tracking.
When a ref is accessed during effect execution, the effect is
registered as a dependency of that ref.")

(defvar etaf-reactive--effect-stack nil
  "Stack of currently running effects for nested effect tracking.
Allows proper dependency tracking when effects are nested.")

(defvar etaf-reactive--effect-deps-tracker nil
  "Tracks dependencies collected during effect execution for cleanup.
This is used to properly clean up old dependencies when an effect re-runs.")

(defun etaf-ref (value)
  "Create a reactive reference with initial VALUE.

A ref is a reactive container that holds a single value.
When the value changes via `etaf-ref-set', all dependent effects
and computeds are automatically notified and re-run.

Returns a ref object that can be:
- Accessed with `etaf-ref-get'
- Modified with `etaf-ref-set'
- Updated with `etaf-ref-update'

Example:

  (let ((count (etaf-ref 0)))
    (etaf-ref-get count)           ; => 0
    (etaf-ref-set count 1)         ; sets to 1 and triggers watchers
    (etaf-ref-get count))          ; => 1

Refs are the foundation of ETAF's reactivity system, similar to
Vue 3's ref() function."
  (let* ((id (cl-gensym "etaf-ref-"))
         (ref (list :type 'ref
                    :id id
                    :value value
                    :deps nil)))    ; dependencies (effects that read this ref)
    ref))

(defun etaf-ref-p (obj)
  "Check if OBJ is a ref object.
Returns t if OBJ is a ref, nil otherwise."
  (and (listp obj)
       (eq (plist-get obj :type) 'ref)))

(defun etaf-ref-get (ref)
  "Get the current value of REF.

If called within an effect (created by `etaf-watch-effect'), the effect
is automatically registered as a dependency of this ref. When the ref
value changes, the effect will be re-run.

Returns the current value stored in the ref."
  (when (etaf-ref-p ref)
    ;; Track dependency if inside an effect
    (when etaf-reactive--current-effect
      (let ((deps (plist-get ref :deps)))
        (unless (member etaf-reactive--current-effect deps)
          (plist-put ref :deps (cons etaf-reactive--current-effect deps))))
      ;; Also track for effect cleanup
      (when etaf-reactive--effect-deps-tracker
        (let ((tracked (plist-get etaf-reactive--effect-deps-tracker :deps)))
          (unless (member ref tracked)
            (plist-put etaf-reactive--effect-deps-tracker :deps (cons ref tracked))))))
    (plist-get ref :value)))

(defun etaf-ref-set (ref value)
  "Set REF to VALUE and trigger all dependent effects.

This function:
1. Updates the ref's internal value
2. Triggers all effects that have accessed this ref
3. Triggers all watch callbacks registered on this ref

If the new value is equal to the old value (checked with `equal'),
no updates are triggered."
  (when (etaf-ref-p ref)
    (let ((old-value (plist-get ref :value)))
      (unless (equal old-value value)
        (plist-put ref :value value)
        ;; Trigger all dependent effects
        (dolist (effect (plist-get ref :deps))
          (when (functionp effect)
            (funcall effect)))))))

(defun etaf-ref-update (ref fn)
  "Update REF by applying FN to its current value.

FN receives the current value and should return the new value.
This is a convenience function equivalent to:
  (etaf-ref-set ref (funcall fn (etaf-ref-get ref)))

Example:

  (let ((count (etaf-ref 5)))
    (etaf-ref-update count #\\='1+)    ; increment by 1
    (etaf-ref-get count))           ; => 6

Returns the new value."
  (when (etaf-ref-p ref)
    (let* ((current (etaf-ref-get ref))
           (new-value (funcall fn current)))
      (etaf-ref-set ref new-value)
      new-value)))

;;; ============================================================================
;;; Reactive System - Computed
;;; ============================================================================

(defun etaf-computed (getter)
  "Create a computed value from GETTER function.

A computed value is a reactive value derived from other reactive values.
It is lazily evaluated and cached - the getter only runs when:
1. The computed value is accessed for the first time
2. One of its dependencies has changed

GETTER is a function that:
- Takes no arguments
- Returns the computed value
- Can access other refs/computed values

The computed value automatically tracks its dependencies. When any
dependency changes, the computed value is marked as dirty and will
recompute on next access.

Example:

  (let* ((price (etaf-ref 100))
         (quantity (etaf-ref 2))
         (total (etaf-computed
                 (lambda ()
                   (* (etaf-ref-get price)
                      (etaf-ref-get quantity))))))
    (etaf-computed-get total)    ; => 200
    (etaf-ref-set quantity 3)
    (etaf-computed-get total))   ; => 300 (automatically recomputed)

Computed values are similar to Vue 3's computed() function."
  (let* ((id (cl-gensym "etaf-computed-"))
         (computed (list :type 'computed
                         :id id
                         :getter getter
                         :value nil
                         :dirty t       ; needs recomputation
                         :deps nil)))   ; effects that depend on this computed
    ;; Create an effect to recompute when dependencies change
    (let ((recompute-effect
           (lambda ()
             (plist-put computed :dirty t)
             ;; Trigger dependent effects
             (dolist (effect (plist-get computed :deps))
               (when (functionp effect)
                 (funcall effect))))))
      (plist-put computed :recompute-effect recompute-effect))
    computed))

(defun etaf-computed-p (obj)
  "Check if OBJ is a computed object.
Returns t if OBJ is a computed value, nil otherwise."
  (and (listp obj)
       (eq (plist-get obj :type) 'computed)))

(defun etaf-computed-get (computed)
  "Get the current value of COMPUTED.

If the computed value is dirty (dependencies have changed),
it will be recomputed before returning the value.

If called within an effect, the effect is registered as a
dependency of this computed value.

Returns the computed value."
  (when (etaf-computed-p computed)
    ;; Track as dependency if inside an effect
    (when etaf-reactive--current-effect
      (let ((deps (plist-get computed :deps)))
        (unless (member etaf-reactive--current-effect deps)
          (plist-put computed :deps (cons etaf-reactive--current-effect deps))))
      ;; Also track for effect cleanup
      (when etaf-reactive--effect-deps-tracker
        (let ((tracked (plist-get etaf-reactive--effect-deps-tracker :deps)))
          (unless (member computed tracked)
            (plist-put etaf-reactive--effect-deps-tracker :deps (cons computed tracked))))))
    ;; Recompute if dirty
    (when (plist-get computed :dirty)
      (let* ((getter (plist-get computed :getter))
             (recompute-effect (plist-get computed :recompute-effect))
             ;; Track dependencies during computation
             (etaf-reactive--current-effect recompute-effect)
             (value (funcall getter)))
        (plist-put computed :value value)
        (plist-put computed :dirty nil)))
    (plist-get computed :value)))

;;; ============================================================================
;;; Reactive System - Watch
;;; ============================================================================

(defun etaf-watch (source callback &optional options)
  "Watch SOURCE (ref or computed) and call CALLBACK when it changes.

SOURCE can be:
- A ref created with `etaf-ref'
- A computed value created with `etaf-computed'

CALLBACK is a function that receives (new-value old-value) as arguments
and is called whenever the source changes.

OPTIONS is an optional plist that can include:
- :immediate - If t, run callback immediately with current value
- :deep - If t, deep watch objects (not yet implemented)

Returns a stop function. Call the stop function to remove the watcher:
  (funcall stop)

Example:

  (let* ((count (etaf-ref 0))
         (stop (etaf-watch
                count
                (lambda (new old)
                  (message \"Count changed: %s -> %s\" old new)))))
    (etaf-ref-set count 1)    ; logs \"Count changed: 0 -> 1\"
    (etaf-ref-set count 2)    ; logs \"Count changed: 1 -> 2\"
    (funcall stop)            ; stop watching
    (etaf-ref-set count 3))   ; no callback

This is similar to Vue 3's watch() function."
  (let* ((immediate (plist-get options :immediate))
         (old-value (cond
                     ((etaf-ref-p source)
                      (etaf-ref-get source))
                     ((etaf-computed-p source)
                      (etaf-computed-get source))))
         (watcher (lambda ()
                    (let ((new-value (cond
                                      ((etaf-ref-p source)
                                       (etaf-ref-get source))
                                      ((etaf-computed-p source)
                                       (etaf-computed-get source)))))
                      (unless (equal new-value old-value)
                        (funcall callback new-value old-value)
                        (setq old-value new-value))))))
    ;; Add watcher to source's deps
    (let ((deps (plist-get source :deps)))
      (plist-put source :deps (cons watcher deps)))
    ;; Run immediately if requested
    (when immediate
      (funcall callback old-value nil))
    ;; Return stop function
    (lambda ()
      (let ((deps (plist-get source :deps)))
        (plist-put source :deps (delete watcher deps))))))

;;; ============================================================================
;;; Reactive System - WatchEffect
;;; ============================================================================

(defun etaf-watch-effect (effect-fn)
  "Run EFFECT-FN immediately and re-run when dependencies change.

EFFECT-FN is a function that will be run immediately and then
automatically re-run whenever any reactive values it accesses change.

Dependencies are automatically tracked - you don't need to explicitly
declare them. Any ref or computed value accessed during effect execution
is automatically registered as a dependency.

Returns a stop function. Call the stop function to stop the effect:
  (funcall stop)

Example:

  (let* ((firstName (etaf-ref \"John\"))
         (lastName (etaf-ref \"Doe\"))
         (stop (etaf-watch-effect
                (lambda ()
                  (message \"Name: %s %s\"
                          (etaf-ref-get firstName)
                          (etaf-ref-get lastName))))))
    ;; Immediately logs \"Name: John Doe\"
    (etaf-ref-set firstName \"Jane\")    ; logs \"Name: Jane Doe\"
    (etaf-ref-set lastName \"Smith\")    ; logs \"Name: Jane Smith\"
    (funcall stop)                       ; stop watching
    (etaf-ref-set firstName \"Bob\"))    ; no log

This is similar to Vue 3's watchEffect() function and is the most
commonly used watch function due to its automatic dependency tracking."
  (let* ((tracked-deps nil)  ; refs/computed that this effect depends on
         (runner nil)
         (active t))         ; whether the effect is still active
    ;; Create the runner that will execute the effect
    (setq runner
          (lambda ()
            (when active
              ;; Clear old dependencies before re-running
              (dolist (dep tracked-deps)
                (let ((old-deps (plist-get dep :deps)))
                  (plist-put dep :deps (delete runner old-deps))))
              (setq tracked-deps nil)
              ;; Run effect with dependency tracking
              ;; When refs are accessed, they add runner to their deps
              (let ((etaf-reactive--current-effect runner)
                    (etaf-reactive--effect-deps-tracker (list :deps nil)))
                (push runner etaf-reactive--effect-stack)
                (unwind-protect
                    (funcall effect-fn)
                  (pop etaf-reactive--effect-stack)
                  ;; Capture tracked deps for cleanup later
                  (setq tracked-deps (plist-get etaf-reactive--effect-deps-tracker :deps)))))))
    ;; Run immediately
    (funcall runner)
    ;; Return stop function
    (lambda ()
      (setq active nil)
      (dolist (dep tracked-deps)
        (let ((old-deps (plist-get dep :deps)))
          (plist-put dep :deps (delete runner old-deps)))))))

;;; ============================================================================
;;; Reactive System - Reactive Objects
;;; ============================================================================

(defun etaf-reactive (data)
  "Create a reactive wrapper around DATA plist.

Each key in the plist becomes individually reactive. Reading and
writing to keys automatically tracks dependencies and triggers updates.

DATA is a plist like (:name \"Alice\" :age 30).

Access values with `etaf-reactive-get':
  (etaf-reactive-get state :name)

Modify values with `etaf-reactive-set':
  (etaf-reactive-set state :name \"Bob\")

Convert back to plist with `etaf-reactive-to-plist':
  (etaf-reactive-to-plist state)

Example:

  (let ((user (etaf-reactive \\='(:name \"Alice\" :age 30))))
    (etaf-watch-effect
     (lambda ()
       (message \"User: %s, Age: %d\"
               (etaf-reactive-get user :name)
               (etaf-reactive-get user :age))))
    ;; Immediately logs \"User: Alice, Age: 30\"
    (etaf-reactive-set user :name \"Bob\")  ; logs \"User: Bob, Age: 30\"
    (etaf-reactive-set user :age 31))      ; logs \"User: Bob, Age: 31\"

This is similar to Vue 3's reactive() function, adapted for Elisp's
plist data structure."
  (let ((refs (make-hash-table :test 'eq)))
    ;; Create refs for each key in data
    (let ((rest data))
      (while rest
        (let ((key (car rest))
              (value (cadr rest)))
          (puthash key (etaf-ref value) refs))
        (setq rest (cddr rest))))
    (list :type 'reactive
          :refs refs
          :data data)))

(defun etaf-reactive-p (obj)
  "Check if OBJ is a reactive object.
Returns t if OBJ is a reactive object, nil otherwise."
  (and (listp obj)
       (eq (plist-get obj :type) 'reactive)))

(defun etaf-reactive-get (reactive key)
  "Get value for KEY from REACTIVE object.
Automatically tracks the access if called within an effect."
  (when (etaf-reactive-p reactive)
    (let* ((refs (plist-get reactive :refs))
           (ref (gethash key refs)))
      (if ref
          (etaf-ref-get ref)
        nil))))

(defun etaf-reactive-set (reactive key value)
  "Set KEY to VALUE in REACTIVE object.
Automatically triggers dependent effects."
  (when (etaf-reactive-p reactive)
    (let* ((refs (plist-get reactive :refs))
           (ref (gethash key refs)))
      (if ref
          (etaf-ref-set ref value)
        ;; Create new ref for new key
        (puthash key (etaf-ref value) refs)
        value))))

(defun etaf-reactive-to-plist (reactive)
  "Convert REACTIVE object to a plain plist.
Returns a new plist with current values from the reactive object."
  (when (etaf-reactive-p reactive)
    (let ((result nil)
          (refs (plist-get reactive :refs)))
      (maphash (lambda (key ref)
                 (setq result (plist-put result key (etaf-ref-get ref))))
               refs)
      result)))

;;; ============================================================================
;;; Legacy Reactive System
;;; ============================================================================

;; The old reactive system is kept for compatibility with existing code.
;; New code should use the new ref/computed/watch system above.

(defvar etaf-reactive--watchers (make-hash-table :test 'eq)
  "Hash table mapping data objects to their watchers.
This is part of the legacy reactive system.")

(defun etaf-create-reactive (data)
  "Create a reactive data wrapper around DATA plist (legacy).

This is the old reactive system kept for backward compatibility.
New code should use `etaf-reactive' instead.

Returns a reactive data object that can trigger re-renders."
  (let ((reactive (list :data data
                        :version 0
                        :watchers nil)))
    reactive))

(defun etaf-get (reactive key)
  "Get value for KEY from REACTIVE data object (legacy).
Works with objects created by `etaf-create-reactive'."
  (plist-get (plist-get reactive :data) key))

(defun etaf-set (reactive key value)
  "Set KEY to VALUE in REACTIVE data object and trigger watchers (legacy).
Works with objects created by `etaf-create-reactive'."
  (let* ((data (plist-get reactive :data))
         (new-data (plist-put data key value)))
    (plist-put reactive :data new-data)
    (plist-put reactive :version (1+ (plist-get reactive :version)))
    ;; Trigger watchers
    (dolist (watcher (plist-get reactive :watchers))
      (funcall watcher reactive key value))
    value))

(defun etaf-watch-reactive (reactive callback)
  "Add CALLBACK as watcher to REACTIVE data (legacy).
CALLBACK receives (reactive key value) when data changes.
Works with objects created by `etaf-create-reactive'."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (cons callback watchers))))

(defun etaf-unwatch-reactive (reactive callback)
  "Remove CALLBACK from REACTIVE data watchers (legacy).
Works with objects created by `etaf-create-reactive'."
  (let ((watchers (plist-get reactive :watchers)))
    (plist-put reactive :watchers (delete callback watchers))))

;;; ============================================================================
;;; Reactive Buffer Binding
;;; ============================================================================

(defvar-local etaf-reactive-buffer-bindings nil
  "Buffer-local list of reactive bindings for incremental updates.
Each binding is (uuid ref format-fn) tuple.")

(defun etaf-reactive-span (ref &optional format-fn)
  "Create a reactive span element that updates incrementally when REF changes.
FORMAT-FN is an optional function to format the ref value (default: formats as string).

Returns a span ETML element with a unique ID that will be automatically
updated when the ref value changes.

Example:
  (let ((count (etaf-ref 0)))
    `(div
      (button :on-click ,(lambda () (etaf-ref-update count #'1+)) \"+\")
      ,(etaf-reactive-span count)  ; This span will auto-update
      (button :on-click ,(lambda () (etaf-ref-update count #'1-)) \"-\")))"
  (let* ((uuid (format "reactive-%s" (cl-gensym)))
         (format-fn (or format-fn (lambda (v) (format "%s" v))))
         (initial-text (funcall format-fn (etaf-ref-get ref))))
    ;; Store binding for setup after render
    (push (list uuid ref format-fn) etaf-reactive-buffer-bindings)
    ;; Return span with special marker
    `(span :data-reactive-id ,uuid ,initial-text)))

(defun etaf-setup-reactive-watchers (buffer)
  "Set up watchers for all reactive spans in BUFFER.
Should be called automatically by etaf-paint-to-buffer."
  (require 'etaf-layout-interactive)
  (with-current-buffer buffer
    (dolist (binding etaf-reactive-buffer-bindings)
      (let ((uuid (nth 0 binding))
            (ref (nth 1 binding))  
            (format-fn (nth 2 binding)))
        ;; Find the text region with this reactive ID
        (save-excursion
          (goto-char (point-min))
          (when (search-forward uuid nil t)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              ;; Add text property to mark this region
              (put-text-property start end 'etaf-reactive-id uuid)
              ;; Set up watcher
              (etaf-watch
               ref
               (lambda (new-value _old-value)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (let ((inhibit-read-only t))
                       (save-excursion
                         (goto-char (point-min))
                         (while (let ((pos (text-property-search-forward 'etaf-reactive-id uuid t)))
                                  (when pos
                                    (let ((region-start (prop-match-beginning pos))
                                          (region-end (prop-match-end pos))
                                          (new-text (funcall format-fn new-value)))
                                      (goto-char region-start)
                                      (delete-region region-start region-end)
                                      (insert (propertize new-text 'etaf-reactive-id uuid)) nil)))))))))))))))))

(provide 'etaf-component)
;;; etaf-component.el ends here
