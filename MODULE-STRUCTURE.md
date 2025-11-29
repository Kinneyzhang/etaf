# ETAF Module Structure

This document describes the modular architecture of the ETAF (Emacs Template and Framework) project after optimization.

## Design Principles

1. **Module Cohesion**: Each module has a clear, focused purpose
2. **Minimal Interfaces**: Modules expose only necessary public functions
3. **Implementation Hiding**: Internal details are hidden within modules
4. **Dependency Clarity**: Module dependencies are explicit and minimal

## Module Organization

### Core Entry Point

#### etaf.el
- **Purpose**: Main entry point for the ETAF system
- **Dependencies**: etaf-tml, etaf-css
- **Public Interface**: (none - just aggregates sub-modules)
- **Usage**: `(require 'etaf)` loads the entire ETAF system

### Markup Processing Modules

#### etaf-tml.el
- **Purpose**: TML (Template Markup Language) to DOM conversion
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-etml-to-dom (sexp)` - Convert TML s-expression to DOM format
  - `etaf-plist-to-alist (plist)` - Convert property list to association list
- **Usage**: Convert markup from TML format `(tag :attr val child...)` to DOM format `(tag ((attr . val)) child...)`

#### etaf-dom.el
- **Purpose**: DOM manipulation, query, and traversal operations
- **Dependencies**: dom (built-in)
- **Public Interface**:
  - `etaf-dom-to-tml (sexp)` - Convert DOM to TML format
  - `etaf-alist-to-plist (alist)` - Convert association list to property list
  - `etaf-dom-tag-match-p (node tag-name)` - Check if node matches tag selector
  - `etaf-dom-class-match-p (node class-name)` - Check if node matches class selector
  - `etaf-dom-id-match-p (node id-name)` - Check if node matches ID selector
  - `etaf-dom-map (func dom)` - Traverse DOM tree and apply function to each node
  - `etaf-dom-get-previous-sibling (node dom)` - Get previous sibling element
  - `etaf-dom-get-previous-siblings (node dom)` - Get all previous siblings
  - `etaf-dom-is-descendant-of (node ancestor)` - Check descendant relationship
  - `etaf-dom-get-parent (node dom)` - Get parent node
  - `etaf-dom-get-element-children (node)` - Get element children (skip text nodes)
  - `etaf-dom-is-first-child (node)` - Check if node is first child
  - `etaf-dom-is-last-child (node)` - Check if node is last child
  - `etaf-dom-get-child-index (node)` - Get child index position
  - `etaf-dom-is-first-of-type (node)` - Check if first of its type
  - `etaf-dom-is-last-of-type (node)` - Check if last of its type
  - `etaf-dom-is-only-of-type (node)` - Check if only one of its type
  - `etaf-dom-is-empty (node)` - Check if node is empty
  - Style manipulation functions (ecss-dom-* prefix)
  - Class manipulation functions (ecss-dom-* prefix)
- **Usage**: Manipulate and query DOM structures, apply styles

### CSS Processing Modules

#### etaf-css.el
- **Purpose**: Main CSS system orchestrator and CSSOM builder
- **Dependencies**: etaf-dom, etaf-css-selector, etaf-css-media, etaf-css-parser, etaf-css-cascade, etaf-css-inheritance, etaf-css-cache, etaf-css-index
- **Public Interface**:
  - `etaf-css-build-cssom (dom &optional media-env)` - Build CSS Object Model from DOM
  - `etaf-css-get-computed-style (cssom node dom)` - Get computed styles for a node
  - `etaf-css-get-rules-for-node (cssom node dom)` - Get all applicable rules for a node
  - `etaf-css-extract-inline-styles (dom)` - Extract inline styles from DOM
  - `etaf-css-extract-style-tags (dom)` - Extract <style> tag contents
  - `etaf-css-cssom-to-string (cssom)` - Convert CSSOM to readable string
  - `etaf-css-clear-cache (cssom)` - Clear the CSSOM cache
- **CSSOM Structure**: plist with keys: :inline-rules, :style-rules, :all-rules, :rule-index, :cache, :media-env
- **Usage**: Primary interface for CSS operations - build CSSOM and query computed styles

#### etaf-css-parser.el
- **Purpose**: Parse CSS declarations, rules, and stylesheets
- **Dependencies**: etaf-css-cascade
- **Public Interface**:
  - `etaf-css-parse-declarations (css-string)` - Parse CSS declarations (supports !important)
  - `etaf-css-parse-rule (rule-string)` - Parse single CSS rule
  - `etaf-css-parse-stylesheet (css-string &optional media-query)` - Parse complete stylesheet
- **Declaration Format**: `((property value important) ...)` where important is boolean
- **Usage**: Parse CSS text into structured data

#### etaf-css-selector.el
- **Purpose**: CSS selector parsing and matching (large, complex module)
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-selector-parse (selector-string)` - Parse selector to AST
  - `etaf-css-selector-basic-match-p (node selector)` - Check if node matches selector
  - `etaf-css-selector-query (selector-string dom)` - Query DOM with selector
- **Internal Functions**: 45+ functions for tokenization, parsing, and AST manipulation
- **Usage**: Core selector engine for CSS matching

#### etaf-css-cascade.el
- **Purpose**: CSS cascade algorithm and selector specificity calculation
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-calculate-specificity (selector)` - Calculate selector specificity
  - `etaf-css-specificity> (spec1 spec2)` - Compare two specificity values
  - `etaf-css-specificity= (spec1 spec2)` - Check specificity equality
  - `etaf-css-cascade-compare-declarations (decl1 decl2)` - Compare declaration priority
  - `etaf-css-cascade-merge-rules (rules)` - Merge rules using cascade algorithm
- **Specificity Format**: `(id-count class-count type-count)`
- **Cascade Order**: (1) normal declarations → (2) !important declarations → (3) inline normal → (4) inline !important
- **Usage**: Resolve style conflicts using CSS cascade rules

#### etaf-css-inheritance.el
- **Purpose**: CSS property inheritance system
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-property-inherits-p (property)` - Check if property is inheritable
  - `etaf-css-apply-inheritance (computed-style parent-style)` - Apply inherited properties
- **Inherited Properties**: color, font-*, text-*, line-height, visibility, etc.
- **Usage**: Apply property inheritance from parent to child elements

#### etaf-css-cache.el
- **Purpose**: Computed style caching for performance optimization
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-cache-create ()` - Create new cache
  - `etaf-css-cache-get (cache node)` - Get cached style for node
  - `etaf-css-cache-set (cache node style)` - Store style in cache
  - `etaf-css-cache-clear (cache)` - Clear all cached data
  - `etaf-css-cache-remove (cache node)` - Remove specific node from cache
  - `etaf-css-cache-size (cache)` - Get number of cached entries
- **Implementation**: Hash table with node as key
- **Usage**: Avoid redundant style calculations

#### etaf-css-index.el
- **Purpose**: CSS rule indexing for performance optimization
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-index-build (rules)` - Build index from rule list
  - `etaf-css-index-query-candidates (index node)` - Get candidate rules for node
  - `etaf-css-index-extract-selector-keys (selector)` - Extract indexable keys from selector
- **Index Structure**: plist with :by-tag, :by-class, :by-id hash tables
- **Usage**: Quickly find potentially matching rules by tag/class/ID

#### etaf-css-media.el
- **Purpose**: Media query evaluation and @media rule parsing
- **Dependencies**: (none)
- **Public Interface**:
  - `etaf-css-media-match-p (query-str &optional env)` - Check if media query matches
  - `etaf-css-media-extract-at-media-blocks (css-string)` - Extract @media blocks
  - `etaf-css-media-parse-feature (feature-str)` - Parse media feature expression
  - `etaf-css-media-evaluate-feature (feature operator value &optional env)` - Evaluate feature
- **Default Environment**: `((type . screen) (width . 1024) (height . 768))`
- **Supported Features**: width, height, min-width, max-width, min-height, max-height
- **Usage**: Evaluate responsive design media queries

### Rendering Module

#### etaf-render.el
- **Purpose**: Build render tree from DOM and CSSOM
- **Dependencies**: etaf-dom, etaf-css
- **Public Interface**:
  - `etaf-render-build-tree (dom cssom)` - Build render tree
  - `etaf-render-walk (render-tree func)` - Traverse render tree
  - `etaf-render-get-style (render-node property)` - Get style property value
  - `etaf-render-find-by-tag (render-tree tag)` - Find nodes by tag
  - `etaf-render-find-by-display (render-tree display)` - Find nodes by display type
  - `etaf-render-to-string (render-tree &optional indent)` - Convert tree to string
  - `etaf-render-stats (render-tree)` - Get tree statistics
- **Render Node Structure**: plist with :node, :tag, :computed-style, :display, :children
- **Usage**: Create render tree for layout and painting (excludes display:none, <script>, etc.)

### Testing Utilities

#### etaf-ert.el
- **Purpose**: Test utilities and macros
- **Dependencies**: ert (built-in)
- **Public Interface**:
  - `should-equal (s1 s2)` - Macro for equality assertions
- **Usage**: Consistent test interface across all test files

## Module Dependency Graph

```
etaf.el
├── etaf-tml.el (no dependencies)
└── etaf-css.el
    ├── etaf-dom.el
    │   └── dom (built-in)
    ├── etaf-css-selector.el (no dependencies)
    ├── etaf-css-media.el (no dependencies)
    ├── etaf-css-parser.el
    │   └── etaf-css-cascade.el (no dependencies)
    ├── etaf-css-cascade.el (no dependencies)
    ├── etaf-css-inheritance.el (no dependencies)
    ├── etaf-css-cache.el (no dependencies)
    └── etaf-css-index.el (no dependencies)

etaf-render.el
├── etaf-dom.el
└── etaf-css.el

etaf-ert.el
└── ert (built-in)
```

## Optimization Summary

### Modules Merged

1. **etaf-utils.el** → Merged into etaf-tml.el and etaf-dom.el
   - Reason: Too simple (only 2 utility functions), 100% external usage rate
   - Impact: Functions moved to modules that actually use them

2. **etaf-css-specificity.el** → Merged into etaf-css-cascade.el
   - Reason: Tightly coupled with cascade algorithm
   - Impact: Related functionality now in single module

### Modules Kept

1. **etaf-ert.el** - Kept despite simplicity
   - Reason: Provides consistent test interface
   - Reason: Used across all test files
   - Reason: May be extended with additional test utilities

2. **etaf-css-cache.el** - Kept as separate module
   - Reason: Clear, focused caching interface
   - Reason: 67% public function ratio (4/6 functions exported)
   - Reason: May have different implementations (in-memory vs. persistent)

3. **etaf-css-index.el** - Kept as separate module
   - Reason: Clear indexing abstraction
   - Reason: 60% public function ratio (3/5 functions exported)
   - Reason: Performance optimization that can be toggled

## Usage Examples

### Basic TML to DOM Conversion
```elisp
(require 'etaf-tml)

(setq dom (etaf-etml-to-dom
           '(div :class "container"
                (h1 :id "title" "Hello World")
                (p "This is a paragraph."))))
;; => (div ((class . "container"))
;;         (h1 ((id . "title")) "Hello World")
;;         (p nil "This is a paragraph."))
```

### Building CSSOM and Querying Styles
```elisp
(require 'etaf-css)

;; Create DOM with styles
(setq dom (etaf-etml-to-dom
           '(html
              (head
                (style ".button { color: red; font-size: 14px; }"))
              (body
                (button :class "button" "Click me")))))

;; Build CSSOM
(setq cssom (etaf-css-build-cssom dom))

;; Get computed style
(setq button (dom-by-class dom "button"))
(setq styles (etaf-css-get-computed-style cssom button dom))
;; => ((color . "red") (font-size . "14px"))
```

### Building Render Tree
```elisp
(require 'etaf-render)

(setq render-tree (etaf-render-build-tree dom cssom))
(etaf-render-walk render-tree
  (lambda (node)
    (message "Tag: %s, Display: %s"
             (plist-get node :tag)
             (plist-get node :display))))
```

## Benefits of This Structure

1. **Clear Boundaries**: Each module has a well-defined purpose
2. **Minimal Coupling**: Dependencies are explicit and minimal
3. **Easy Testing**: Modules can be tested in isolation
4. **Performance**: Caching and indexing are separate, optional optimizations
5. **Maintainability**: Related functionality is grouped together
6. **Extensibility**: New features can be added without disrupting existing modules

## Future Improvements

1. Consider extracting DOM utility functions to separate module if list grows
2. May split etaf-css-selector.el if it becomes too large (currently 1132 lines)
3. Consider adding more test utilities to etaf-ert.el to justify its existence
4. May add persistent caching option in etaf-css-cache.el
