;;; etaf-component.el --- Vue3-style component system for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: components, reactive, vue3, composition-api
;; Version: 2.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Vue 3-Style Component System for ETAF
;; ======================================
;;
;; This module provides a complete component system inspired by Vue 3's
;; Composition API, enabling reactive UI development in Emacs Lisp.
;;
;; Module Structure:
;; -----------------
;; 1. Component Registry - Global component registration and lookup
;; 2. Component Definition - Define components with props, setup, templates
;; 3. Options API Conversion - Vue 2 style to Vue 3 style conversion
;; 4. Component Rendering - Template evaluation and rendering
;; 5. Reactive System Core - Effect tracking infrastructure
;; 6. Reactive Primitives - ref, computed, reactive
;; 7. Watch System - watch, watchEffect
;; 8. Reactive Buffer Binding - DOM update automation
;;
;; Reactive System Overview:
;; -------------------------
;; The reactive system follows Vue 3's design principles:
;;
;;   ┌─────────────┐
;;   │   ref(0)    │  ← Reactive reference holding a value
;;   └──────┬──────┘
;;          │
;;          │ dependency tracking
;;          ▼
;;   ┌─────────────┐
;;   │   effect    │  ← Automatically re-runs when deps change
;;   └──────┬──────┘
;;          │
;;          │ triggers update
;;          ▼
;;   ┌─────────────┐
;;   │   UI/DOM    │  ← Rendered output
;;   └─────────────┘
;;
;; Key Concepts:
;; - ref: Basic reactive container for a single value
;; - computed: Cached, derived value that updates when deps change
;; - effect: Function that tracks deps and re-runs automatically
;; - watch: Explicit dependency watching with callbacks
;; - watchEffect: Automatic dependency tracking
;;
;; Component API Styles:
;; ---------------------
;; Composition API (Vue 3 - recommended):
;;   (etaf-define-component my-counter
;;     :props '(:initial-count)
;;     :setup (lambda (props)
;;              (let ((count (etaf-ref 0)))
;;                (list :count count
;;                      :increment (lambda () (etaf-ref-update count #'1+)))))
;;     :template (lambda (data)
;;                 `(div (span ,(etaf-ref-get (plist-get data :count)))
;;                       (button :on-click ,(plist-get data :increment) "+"))))
;;
;; Options API (Vue 2 - also supported):
;;   (etaf-define-component my-counter
;;     :data (lambda () '(:count 0))
;;     :methods '(:increment (lambda () (etaf-ref-update count #'1+)))
;;     :template ...)
;;
;; Related Modules:
;; ----------------
;; - etaf-vdom.el: Virtual DOM that renders component output
;; - etaf-type.el: Type definitions (etaf-ref, etaf-computed, etaf-component-def)
;;
;; References:
;; -----------
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

Vue 3 Composition API style (recommended):
- :props - List of prop names or plist with prop definitions
  Examples:
    \\='(:title :count :disabled)
    \\='((:title :type string :required t)
      (:count :type number :default 0))

- :setup - Setup function that receives props and returns reactive state
  Signature: (lambda (props) -> plist)
  The setup function should return a plist of data and methods
  that will be available in the template.

Vue 2 Options API style:
- :data - Function that returns initial data as plist
  Signature: (lambda () -> plist)
  All values are automatically made reactive.

- :methods - Plist of methods
  Example: \\='(:increment (lambda () ...) :decrement (lambda () ...))

- :computed - Plist of computed properties
  Example: \\='(:doubled (lambda () (* 2 (etaf-ref-get count))))

- :watch - Plist of watchers
  Example: \\='(:count (lambda (new old) (message \"Changed: %s\" new)))

- :mounted - Lifecycle hook called when component is mounted
- :updated - Lifecycle hook called when component updates
- :unmounted - Lifecycle hook called when component is unmounted

Common to both styles:
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
                         ;; Composition API
                         :setup (plist-get options :setup)
                         ;; Options API
                         :data (plist-get options :data)
                         :methods (plist-get options :methods)
                         :computed (plist-get options :computed)
                         :watch (plist-get options :watch)
                         :mounted (plist-get options :mounted)
                         :updated (plist-get options :updated)
                         :unmounted (plist-get options :unmounted)
                         ;; Common
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

Vue 3 Composition API style:
- :props - List of prop names the component accepts
- :setup - Setup function (props) -> plist of reactive data and methods
- :template - Template function (data) -> ETML sexp, or ETML sexp directly
- :emits - List of events this component can emit

Vue 2 Options API style:
- :props - List of prop names the component accepts
- :data - Function returning initial data plist (automatically reactive)
- :methods - Plist of method functions
- :computed - Plist of computed property functions
- :watch - Plist of watcher functions
- :mounted - Lifecycle hook function
- :updated - Lifecycle hook function
- :unmounted - Lifecycle hook function
- :template - Template function (data) -> ETML sexp, or ETML sexp directly
- :emits - List of events this component can emit

This macro creates a component definition variable and registers it
in the global component registry.

Example (Composition API - Vue 3 style):

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

Example (Options API - Vue 2 style):

  (etaf-define-component my-counter
    :props \\='(:initial-count)
    :data (lambda ()
            (list :count 0))
    :computed (list :doubled (lambda ()
                               (* 2 (etaf-ref-get (plist-get this :count)))))
    :methods (list :increment (lambda ()
                                (etaf-ref-update (plist-get this :count) #'1+)))
    :template (lambda (data)
                `(div :class \"counter\"
                      (span ,(format \"Count: %s\" (etaf-ref-get
                                                    (plist-get data :count))))
                      (button :on-click ,(plist-get data :increment)
                              \"Increment\"))))

After definition, the component can be used in templates:

  (my-counter :initial-count 10)

The component system integrates with ETAF's template rendering
to provide both Vue 2 Options API and Vue 3 Composition API styles."
  (declare (indent defun))
  (let ((component-var (intern (format "etaf-component--%s" name))))
    `(progn
       (defvar ,component-var
         (etaf-component-create ',name ,@options)
         ,(format "Component definition for %s." name))
       (etaf-component-register ',name ,component-var)
       ',name)))

;;; ============================================================================
;;; Options API to Composition API Conversion
;;; ============================================================================

(defun etaf-component--options-to-setup (data-fn methods computed-props watch-props mounted updated unmounted)
  "Convert Vue 2 Options API to Vue 3 Composition API setup function.

DATA-FN is the :data function that returns initial data plist.
METHODS is a plist of method functions.
COMPUTED-PROPS is a plist of computed property getter functions.
WATCH-PROPS is a plist of watcher functions.
MOUNTED, UPDATED, UNMOUNTED are lifecycle hook functions.

Returns a setup function compatible with the Composition API."
  (lambda (props)
    "Setup function generated from Options API."
    (let ((result-data nil)
          (cleanup-functions nil))
      
      ;; 1. Initialize reactive data from :data function
      (when data-fn
        (let ((initial-data (funcall data-fn)))
          ;; Convert each data property to a ref
          (let ((rest initial-data))
            (while rest
              (let ((key (car rest))
                    (value (cadr rest)))
                (setq result-data (plist-put result-data key (etaf-ref value))))
              (setq rest (cddr rest))))))
      
      ;; 2. Create computed properties
      (when computed-props
        (let ((rest computed-props))
          (while rest
            (let* ((key (car rest))
                   (getter (cadr rest))
                   ;; Create a computed value with access to component data
                   (computed-val (etaf-computed
                                  (lambda ()
                                    ;; Bind 'this' context for the getter
                                    (let ((this result-data))
                                      (funcall getter))))))
              (setq result-data (plist-put result-data key computed-val)))
            (setq rest (cddr rest)))))
      
      ;; 3. Add methods (bind 'this' context to each method)
      (when methods
        (let ((rest methods))
          (while rest
            (let* ((key (car rest))
                   (method (cadr rest))
                   ;; Create a method with bound 'this' context
                   (bound-method (lambda (&rest args)
                                   (let ((this result-data))
                                     (apply method args)))))
              (setq result-data (plist-put result-data key bound-method)))
            (setq rest (cddr rest)))))
      
      ;; 4. Set up watchers
      (when watch-props
        (let ((rest watch-props))
          (while rest
            (let* ((key (car rest))
                   (watcher (cadr rest))
                   (source (plist-get result-data key)))
              (when (and source (or (etaf-ref-p source) (etaf-computed-p source)))
                (let ((stop-fn (etaf-watch source watcher)))
                  (push stop-fn cleanup-functions))))
            (setq rest (cddr rest)))))
      
      ;; 5. Call mounted hook (simulated)
      (when mounted
        (let ((this result-data))
          (funcall mounted)))
      
      ;; Note: updated and unmounted hooks would need integration with
      ;; the virtual DOM lifecycle system to work properly
      
      ;; Return the component data (refs, computed, methods)
      result-data)))

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
2. Detects if using Options API or Composition API
3. Converts Options API to Composition API setup if needed
4. Runs the component's setup function to create reactive state
5. Merges component data with parent data context
6. Renders the component using its template or render function"
  (let* ((prop-names (plist-get component :props))
         (props (etaf-component--extract-props attrs prop-names))
         ;; Check if using Options API or Composition API
         (data-fn (plist-get component :data))
         (methods (plist-get component :methods))
         (computed-props (plist-get component :computed))
         (watch-props (plist-get component :watch))
         (mounted (plist-get component :mounted))
         (updated (plist-get component :updated))
         (unmounted (plist-get component :unmounted))
         (setup-fn (plist-get component :setup))
         (template-fn (plist-get component :template))
         (render-fn (plist-get component :render))
         ;; Add special $slots prop for children (Vue-like slots)
         (props-with-slots (plist-put props :$slots children)))
    
    ;; Determine which API style to use
    ;; If Options API properties exist, convert to Composition API
    (when (and (not setup-fn)
               (or data-fn methods computed-props watch-props
                   mounted updated unmounted))
      (setq setup-fn (etaf-component--options-to-setup
                      data-fn methods computed-props watch-props
                      mounted updated unmounted)))
    
    ;; Run setup function if provided (either original or converted)
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
;;; Reactive System - Core Infrastructure (Vue 3 Style)
;;; ============================================================================

;; Global dependency tracking bucket structure
;; This follows Vue 3's design: WeakMap<target, Map<key, Set<effect>>>
;; In Elisp, we use a hash-table instead of WeakMap
(defvar etaf-reactive--bucket (make-hash-table :test 'eq)
  "Global bucket for storing dependency relationships.
Maps reactive objects (refs/computed) to their dependent effects.
Structure: hash-table<target, hash-table<key, list<effect>>>")

(defvar etaf-reactive--active-effect nil
  "The currently active effect being executed.
Used for automatic dependency collection during effect execution.")

(defvar etaf-reactive--effect-stack nil
  "Stack of effects for handling nested effects.
When an effect runs, it's pushed onto this stack. When effects are
nested (e.g., parent component -> child component), this ensures
correct dependency tracking.")

(defvar etaf-reactive--should-track t
  "Flag to control whether dependency tracking should occur.
Set to nil to temporarily disable tracking.")

;;; ============================================================================
;;; Effect Management
;;; ============================================================================

(defun etaf-reactive--track (target key)
  "Track dependency between TARGET reactive object and KEY.
Records that the current active effect depends on TARGET[KEY].
This function is called during reactive data reads."
  (when (and etaf-reactive--active-effect etaf-reactive--should-track)
    (let* ((deps-map (or (gethash target etaf-reactive--bucket)
                         (let ((new-map (make-hash-table :test 'eq)))
                           (puthash target new-map etaf-reactive--bucket)
                           new-map)))
           (dep (or (gethash key deps-map)
                    (let ((new-dep nil))
                      (puthash key new-dep deps-map)
                      new-dep))))
      ;; Add current effect to dependency set
      (unless (member etaf-reactive--active-effect dep)
        (puthash key (cons etaf-reactive--active-effect dep) deps-map)
        ;; Also track in effect's deps for cleanup
        (let ((effect-obj etaf-reactive--active-effect))
          (when (listp effect-obj)
            ;; Initialize :deps if it doesn't exist
            (unless (plist-member effect-obj :deps)
              (plist-put effect-obj :deps nil))
            (plist-put effect-obj :deps 
                      (cons (cons target key) 
                            (plist-get effect-obj :deps)))))))))

(defun etaf-reactive--trigger (target key)
  "Trigger all effects that depend on TARGET[KEY].
This function is called when reactive data is modified."
  (let ((deps-map (gethash target etaf-reactive--bucket)))
    (when deps-map
      (let ((effects (gethash key deps-map)))
        (when effects
          ;; Create a new list to avoid infinite loop during Set iteration
          ;; (as described in ECMA spec and Vue 3 implementation)
          (let ((effects-to-run nil))
            (dolist (effect effects)
              ;; Prevent infinite recursion: don't trigger if effect is currently running
              (unless (eq effect etaf-reactive--active-effect)
                (push effect effects-to-run)))
            ;; Run effects
            (dolist (effect (nreverse effects-to-run))
              (cond
               ;; If effect has a scheduler, use it
               ((and (listp effect) (plist-get effect :scheduler))
                (funcall (plist-get effect :scheduler) effect))
               ;; If effect has a run function, call it
               ((and (listp effect) (plist-get effect :run))
                (funcall (plist-get effect :run)))
               ;; Otherwise, treat as a plain function
               ((functionp effect)
                (funcall effect))))))))))

(defun etaf-reactive--cleanup-effect (effect)
  "Remove EFFECT from all its dependencies.
This is called before re-running an effect to clean up old dependencies,
solving the branch switching problem described in Vue 3 reactivity."
  (when (and (listp effect) (plist-get effect :deps))
    (dolist (dep-pair (plist-get effect :deps))
      (let* ((target (car dep-pair))
             (key (cdr dep-pair))
             (deps-map (gethash target etaf-reactive--bucket)))
        (when deps-map
          (let ((dep-list (gethash key deps-map)))
            (puthash key (delete effect dep-list) deps-map)))))
    ;; Clear the effect's deps list
    (plist-put effect :deps nil)))

(defun etaf-reactive-effect (fn &optional options)
  "Create a reactive effect with function FN.

OPTIONS can include:
- :scheduler - Custom scheduler function (effect) -> void
- :lazy - If t, don't run immediately (used for computed)
- :onStop - Callback when effect is stopped

Returns an effect runner function that can be called to re-run the effect.

This is the foundation of Vue 3's reactivity system. Effects automatically
track their dependencies and re-run when dependencies change."
  (let* ((scheduler (plist-get options :scheduler))
         (lazy (plist-get options :lazy))
         (on-stop (plist-get options :onStop))
         (effect (list :type 'effect
                       :fn fn
                       :scheduler scheduler
                       :lazy lazy
                       :onStop on-stop
                       :deps nil  ; list of (target . key) pairs
                       :active t))
         (runner nil))
    ;; Create the runner function
    (setq runner
          (lambda ()
            (when (plist-get effect :active)
              ;; Cleanup old dependencies before re-running
              (etaf-reactive--cleanup-effect effect)
              
              ;; Push effect onto stack for nested effect tracking
              (push effect etaf-reactive--effect-stack)
              (let ((etaf-reactive--active-effect effect))
                (unwind-protect
                    (funcall fn)
                  ;; Pop from stack when done
                  (pop etaf-reactive--effect-stack)
                  ;; Update active effect to parent if exists
                  (setq etaf-reactive--active-effect 
                        (car etaf-reactive--effect-stack)))))))
    
    ;; Store runner in effect for later use
    (plist-put effect :run runner)
    
    ;; Run immediately unless lazy
    (unless lazy
      (funcall runner))
    
    ;; Return the runner
    runner))

;;; ============================================================================
;;; Scheduler and Batching
;;; ============================================================================

(defvar etaf-reactive--queue nil
  "Queue for batched effect execution.")

(defvar etaf-reactive--is-flushing nil
  "Flag indicating if queue is currently being flushed.")

(defvar etaf-reactive--pending-flush nil
  "Flag indicating if a flush is scheduled.")

(defun etaf-reactive--queue-job (job)
  "Add JOB to the execution queue with deduplication.
Jobs are deduplicated and executed in the next microtask."
  (unless (member job etaf-reactive--queue)
    (push job etaf-reactive--queue)
    (etaf-reactive--queue-flush)))

(defun etaf-reactive--queue-flush ()
  "Schedule flushing of the job queue.
Uses run-with-idle-timer to simulate microtask behavior."
  (unless etaf-reactive--pending-flush
    (setq etaf-reactive--pending-flush t)
    (run-with-idle-timer 0 nil #'etaf-reactive--flush-jobs)))

(defun etaf-reactive--flush-jobs ()
  "Flush all pending jobs in the queue."
  (when etaf-reactive--queue
    (setq etaf-reactive--is-flushing t)
    (setq etaf-reactive--pending-flush nil)
    (let ((queue (nreverse etaf-reactive--queue)))
      (setq etaf-reactive--queue nil)
      (dolist (job queue)
        (when (functionp job)
          (funcall job))))
    (setq etaf-reactive--is-flushing nil)))

;;; ============================================================================
;;; Reactive System - Ref
;;; ============================================================================

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
                    :value value)))
    ref))

(defun etaf-ref-p (obj)
  "Check if OBJ is a ref object.
Returns t if OBJ is a ref, nil otherwise."
  (and (listp obj)
       (eq (plist-get obj :type) 'ref)))

(defun etaf-ref-get (ref)
  "Get the current value of REF.

If called within an effect (created by `etaf-reactive-effect'), the effect
is automatically registered as a dependency of this ref. When the ref
value changes, the effect will be re-run.

Returns the current value stored in the ref."
  (when (etaf-ref-p ref)
    ;; Track dependency if inside an effect
    (etaf-reactive--track ref :value)
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
        (etaf-reactive--trigger ref :value)))))

(defun etaf-ref-update (ref fn)
  "Update REF by applying FN to its current value.

FN receives the current value and should return the new value.
This is a convenience function equivalent to:
  (etaf-ref-set ref (funcall fn (etaf-ref-get ref)))

Example:

  (let ((count (etaf-ref 5)))
    (etaf-ref-update count #'1+)    ; increment by 1
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
                         :dirty t)))    ; needs recomputation
    ;; Create an effect with lazy evaluation and custom scheduler
    (let ((runner (etaf-reactive-effect
                   (lambda ()
                     (plist-put computed :value (funcall getter)))
                   (list :lazy t
                         :scheduler (lambda (_effect)
                                     ;; When dependency changes, mark as dirty
                                     (plist-put computed :dirty t)
                                     ;; Trigger effects that depend on this computed
                                     (etaf-reactive--trigger computed :value))))))
      (plist-put computed :effect runner))
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
    (etaf-reactive--track computed :value)
    
    ;; Recompute if dirty
    (when (plist-get computed :dirty)
      (plist-put computed :dirty nil)
      ;; Run the effect to recompute
      (let ((runner (plist-get computed :effect)))
        (when runner
          (funcall runner))))
    
    (plist-get computed :value)))

;;; ============================================================================
;;; Reactive System - Watch
;;; ============================================================================

(defun etaf-watch (source callback &optional options)
  "Watch SOURCE (ref, computed, or getter function) and call CALLBACK when it changes.

SOURCE can be:
- A ref created with `etaf-ref'
- A computed value created with `etaf-computed'
- A getter function that returns a value

CALLBACK is a function that receives (new-value old-value on-invalidate) as arguments
and is called whenever the source changes.

OPTIONS is an optional plist that can include:
- :immediate - If t, run callback immediately with current value
- :deep - If t, deep watch objects (not yet implemented)
- :flush - Control callback execution timing:
  * 'pre' - before DOM updates (default in Vue)
  * 'post' - after DOM updates
  * 'sync' - synchronous execution (no batching)

The on-invalidate parameter in the callback is a function that can be called
to register a cleanup function. This cleanup will run before the next callback
execution, allowing you to handle race conditions in async operations.

Returns a stop function. Call the stop function to remove the watcher:
  (funcall stop)

Example:

  (let* ((count (etaf-ref 0))
         (stop (etaf-watch
                count
                (lambda (new old on-invalidate)
                  (message \"Count changed: %s -> %s\" old new)
                  ;; Register cleanup for async operations
                  (funcall on-invalidate
                           (lambda () (message \"Cleaning up...\")))))))
    (etaf-ref-set count 1)    ; logs \"Count changed: 0 -> 1\"
    (etaf-ref-set count 2)    ; logs \"Cleaning up...\" then \"Count changed: 1 -> 2\"
    (funcall stop)            ; stop watching
    (etaf-ref-set count 3))   ; no callback

This is similar to Vue 3's watch() function with onInvalidate support."
  (let* ((immediate (plist-get options :immediate))
         (flush (or (plist-get options :flush) 'pre))
         (cleanup-fn-box (list nil))  ; Use list as mutable box
         (old-value-box (list 'etaf-watch-uninitialized))  ; Use list as mutable box
         (is-first-run-box (list t))  ; Track if this is the first run
         (effect-obj-box (list nil))  ; Store effect object for stop function
         (getter (cond
                  ((functionp source) source)
                  ((etaf-ref-p source)
                   (lambda () (etaf-ref-get source)))
                  ((etaf-computed-p source)
                   (lambda () (etaf-computed-get source)))
                  (t (error "Invalid watch source"))))
         (on-invalidate (lambda (fn)
                         "Register cleanup function for next callback."
                         (setcar cleanup-fn-box fn)))
         (job nil)
         (runner (etaf-reactive-effect
                  (lambda ()
                    ;; Capture effect object on first run
                    (unless (car effect-obj-box)
                      (setcar effect-obj-box etaf-reactive--active-effect))
                    
                    (let ((new-value (funcall getter))
                          (old-value (car old-value-box))
                          (is-first-run (car is-first-run-box)))
                      
                      ;; Handle first run - initialize old value and optionally run callback
                      (when is-first-run
                        (setcar is-first-run-box nil)
                        (setcar old-value-box new-value)
                        ;; If immediate, run callback on first run
                        (when immediate
                          (funcall callback new-value nil on-invalidate))
                        ;; Don't check for changes on first run
                        (setq is-first-run t))  ; Reuse as skip flag
                      
                      ;; Only trigger callback on changes (not on first collection)
                      (unless is-first-run  ; If we skipped above, this is nil
                        (when (not (equal new-value old-value))
                          (let ((run-callback
                                 (lambda ()
                                   ;; Run cleanup before callback
                                   (when (car cleanup-fn-box)
                                     (funcall (car cleanup-fn-box))
                                     (setcar cleanup-fn-box nil))
                                   ;; Call user callback
                                   (funcall callback new-value old-value on-invalidate)
                                   (setcar old-value-box new-value))))
                            (cond
                             ;; Sync flush - run immediately
                             ((eq flush 'sync)
                              (funcall run-callback))
                             ;; Pre/post flush - use scheduler
                             (t
                              (setq job run-callback)
                              (etaf-reactive--queue-job job))))))))
                  nil)))  ; No options - always run immediately to track dependencies
    
    ;; Return stop function
    (lambda ()
      (when (car cleanup-fn-box)
        (funcall (car cleanup-fn-box)))
      ;; Mark effect as inactive using captured effect object
      (let ((effect-obj (car effect-obj-box)))
        (when effect-obj
          (plist-put effect-obj :active nil)
          (etaf-reactive--cleanup-effect effect-obj))))))

;;; ============================================================================
;;; Reactive System - WatchEffect
;;; ============================================================================

(defun etaf-watch-effect (effect-fn &optional options)
  "Run EFFECT-FN immediately and re-run when dependencies change.

EFFECT-FN is a function that will be run immediately and then
automatically re-run whenever any reactive values it accesses change.

Dependencies are automatically tracked - you don't need to explicitly
declare them. Any ref or computed value accessed during effect execution
is automatically registered as a dependency.

OPTIONS is an optional plist that can include:
- :flush - Control effect execution timing ('pre', 'post', or 'sync')
  Default is 'sync' for immediate execution

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
  (let* ((flush (or (plist-get options :flush) 'sync))  ; Default to sync
         (scheduler (if (eq flush 'sync)
                       nil  ; No scheduler for sync (runs immediately)
                     (lambda (effect)
                       (etaf-reactive--queue-job 
                        (plist-get effect :run)))))
         (effect-obj nil)
         (runner (etaf-reactive-effect
                  effect-fn
                  (list :scheduler scheduler))))
    ;; Store effect object for stop function
    ;; We need to capture it from the effect stack after first run
    (setq effect-obj (car etaf-reactive--effect-stack))
    
    ;; Return stop function
    (lambda ()
      ;; Find the effect object - it should be in the bucket
      ;; For now, we just mark all effects on stack as inactive
      (when effect-obj
        (plist-put effect-obj :active nil)
        (etaf-reactive--cleanup-effect effect-obj)))))

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
