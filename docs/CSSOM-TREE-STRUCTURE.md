# CSSOM Tree Structure

## Overview

The CSSOM (CSS Object Model) has been refactored to use a tree structure similar to DOM and layout trees, allowing the use of DOM library operations for consistent tree manipulation across the entire ETAF framework.

## Tree-Based Architecture

### Before (Flat Plist Structure)
```elisp
(:inline-rules (rule1 rule2 ...)
 :style-rules (rule1 rule2 ...)
 :all-rules (all-rules...)
 :rule-index (...)
 :cache #<hash-table>
 :media-env (...))
```

### After (Tree Structure)
```elisp
(cssom ((type . root)
        (rule-index . ...)
        (cache . #<hash-table>)
        (media-env . ...))
  (stylesheet ((type . stylesheet)
               (source . ua)
               (media . "all")
               (href . nil)
               (disabled . nil))
    (rule ((type . style-rule)
           (selector . "div")
           (declarations . ((display "block" nil)))
           (specificity . (0 0 1))
           (source . ua)
           (media . nil))))
  (stylesheet ...)
  ...)
```

## Node Types

### 1. CSSOM Root Node
The root node of the CSSOM tree.

**Tag:** `cssom`

**Attributes:**
- `type`: Always `'root`
- `rule-index`: Hash tables for fast rule lookup by tag, class, and ID
- `cache`: Hash table for computed style caching
- `media-env`: Current media query environment

**Children:** Stylesheet nodes

**Example:**
```elisp
(cssom ((type . root)
        (rule-index . (:by-tag #<hash> :by-class #<hash> :by-id #<hash>))
        (cache . #<hash-table>)
        (media-env . ((type . screen) (width . 1024) ...)))
  stylesheet1
  stylesheet2
  ...)
```

### 2. Stylesheet Node
Represents a CSS stylesheet source (UA, inline, or style tag).

**Tag:** `stylesheet`

**Attributes:**
- `type`: Always `'stylesheet`
- `source`: One of `'ua`, `'inline`, or `'style-tag`
- `media`: Media query string (default: `"all"`)
- `href`: URL for external stylesheets (usually `nil`)
- `disabled`: Boolean indicating if stylesheet is disabled

**Children:** Rule nodes

**Example:**
```elisp
(stylesheet ((type . stylesheet)
             (source . style-tag)
             (media . "all")
             (href . nil)
             (disabled . nil))
  rule1
  rule2
  ...)
```

### 3. Rule Node
Represents a CSS style rule.

**Tag:** `rule`

**Attributes:**
- `type`: Always `'style-rule`
- `selector`: CSS selector string (e.g., `"div.class"`)
- `declarations`: List of declarations `((property value important) ...)`
- `specificity`: Specificity tuple `(inline id class type)`
- `source`: Origin (`'ua`, `'inline`, or `'style-tag`)
- `media`: Optional media query string
- `node`: DOM node reference (only for inline styles)

**Children:** None

**Example:**
```elisp
(rule ((type . style-rule)
       (selector . "div.button")
       (declarations . ((color "red" nil) (font-size "14px" t)))
       (specificity . (0 1 1))
       (source . style-tag)
       (media . nil)))
```

## Using DOM Operations

The tree structure enables the use of standard DOM library functions:

### Accessing Nodes
```elisp
;; Get CSSOM tag
(dom-tag cssom)  ; => 'cssom

;; Get CSSOM attributes
(dom-attr cssom 'cache)      ; => #<hash-table>
(dom-attr cssom 'media-env)  ; => ((type . screen) ...)

;; Get children (stylesheets)
(dom-children cssom)  ; => (stylesheet1 stylesheet2 ...)
```

### Traversing the Tree
```elisp
;; Map over all nodes
(etaf-dom-map
  (lambda (node)
    (when (eq (dom-tag node) 'rule)
      (message "Rule: %s" (dom-attr node 'selector))))
  cssom)

;; Get all stylesheets
(etaf-css-get-stylesheets cssom)

;; Get all rules
(etaf-css-get-all-rules cssom)
```

## API Functions

### Core Functions

#### `etaf-css-create-cssom-root (&optional media-env)`
Creates a new CSSOM root node.

```elisp
(setq cssom (etaf-css-create-cssom-root))
```

#### `etaf-css-create-stylesheet (source &optional media href)`
Creates a stylesheet node.

```elisp
(setq stylesheet (etaf-css-create-stylesheet 'style-tag "screen"))
```

#### `etaf-css-create-rule (selector declarations specificity source &optional media node)`
Creates a rule node.

```elisp
(setq rule (etaf-css-create-rule 
            "div.box"
            '((color "red" nil) (padding "10px" nil))
            '(0 1 1)
            'style-tag))
```

### Query Functions

#### `etaf-css-get-all-rules (cssom)`
Extracts all rule nodes from the CSSOM tree in priority order (UA < Author < Inline).

```elisp
(setq rules (etaf-css-get-all-rules cssom))
```

#### `etaf-css-get-stylesheets (cssom)`
Gets all stylesheet children of the CSSOM root.

```elisp
(setq stylesheets (etaf-css-get-stylesheets cssom))
```

#### `etaf-css-rule-to-plist (rule-node)`
Converts a rule node to plist format for compatibility.

```elisp
(setq rule-plist (etaf-css-rule-to-plist rule-node))
; => (:selector "div" :declarations ((color "red" nil)) ...)
```

### Backward Compatibility

#### `etaf-css-get-inline-rules (cssom)`
Gets inline style rules in plist format.

```elisp
(setq inline-rules (etaf-css-get-inline-rules cssom))
```

#### `etaf-css-get-style-rules (cssom)`
Gets stylesheet rules in plist format.

```elisp
(setq style-rules (etaf-css-get-style-rules cssom))
```

#### `etaf-css-get-all-rules-plist (cssom)`
Gets all rules in plist format.

```elisp
(setq all-rules (etaf-css-get-all-rules-plist cssom))
```

## Benefits of Tree Structure

### 1. Unified API
All ETAF data structures (DOM, CSSOM, Layout Tree) now use the same tree format and can be manipulated with DOM library functions.

```elisp
;; Same operations work on all trees
(dom-tag node)
(dom-attr node 'property)
(dom-children node)
(etaf-dom-map func tree)
```

### 2. Hierarchical Organization
Clear parent-child relationships make the structure intuitive and easier to understand.

```
CSSOM
├── UA Stylesheet
│   ├── Rule (div)
│   ├── Rule (p)
│   └── Rule (h1)
├── Author Stylesheet
│   ├── Rule (.button)
│   └── Rule (#main)
└── Inline Stylesheet
    └── Rule (div#test)
```

### 3. Extensibility
Easy to add new node types in the future:
- `@keyframes` rules
- `@font-face` rules
- `@import` rules
- CSS variables
- Nested rules

### 4. Performance
The tree structure maintains the same performance characteristics:
- Rule indexing by tag, class, and ID
- Computed style caching
- Efficient candidate rule lookup

### 5. Modularity
Clearer separation of concerns:
- Stylesheet management
- Rule organization
- Cascade computation
- Style caching

## Migration Guide

### For Existing Code

Most existing code will continue to work without changes because:

1. **Main API unchanged:** `etaf-css-build-cssom`, `etaf-css-get-computed-style`, etc.
2. **Backward compatibility functions:** `etaf-css-get-inline-rules`, `etaf-css-get-style-rules`, etc.
3. **Plist conversion:** `etaf-css-rule-to-plist` converts tree nodes to old format

### Recommended Updates

For new code, use the tree-based API:

**Old Style:**
```elisp
(let ((rules (plist-get cssom :all-rules)))
  (dolist (rule rules)
    (message "Selector: %s" (plist-get rule :selector))))
```

**New Style:**
```elisp
(let ((rules (etaf-css-get-all-rules cssom)))
  (dolist (rule rules)
    (message "Selector: %s" (dom-attr rule 'selector))))
```

**Or use tree traversal:**
```elisp
(etaf-dom-map
  (lambda (node)
    (when (eq (dom-tag node) 'rule)
      (message "Selector: %s" (dom-attr node 'selector))))
  cssom)
```

## Implementation Details

### Node Comparison for Inline Styles

Inline styles store a reference to their DOM node. When matching rules, the code handles both wrapped `((tag ...))` and unwrapped `(tag ...)` node formats:

```elisp
;; Normalize node for comparison
(let ((normalized-node (if (and (listp node)
                                (listp (car node))
                                (symbolp (car (car node))))
                           (car node)
                         node)))
  ;; Now use normalized-node for eq comparison
  (eq rule-node-ref normalized-node))
```

### Cascade Order

Stylesheets are added to the CSSOM in priority order:
1. UA stylesheet (lowest priority)
2. Author stylesheets (style tags)
3. Inline stylesheet (highest priority)

This ensures correct CSS cascade behavior when rules are collected.

### Index Building

The rule index is built from all rules and stored at the CSSOM root:

```elisp
(let* ((all-rules (etaf-css-get-all-rules cssom))
       (rules-plist (mapcar #'etaf-css-rule-to-plist all-rules))
       (rule-index (etaf-css-index-build rules-plist)))
  ;; Store in CSSOM root attributes
  (setcdr (assq 'rule-index (dom-attributes cssom)) rule-index))
```

## Testing

The refactoring includes comprehensive tests in `tests/etaf-css-tests.el`:

```bash
# Run CSS tests
cd /path/to/etaf
emacs --batch -L . -L /path/to/emacs-kp \
  -l etaf-ert.el \
  -l tests/etaf-css-tests.el \
  -f ert-run-tests-batch-and-exit
```

## Future Enhancements

The tree structure enables future improvements:

1. **CSS Modules Support:** Each stylesheet could have a namespace
2. **Source Maps:** Track original source locations
3. **Live Updates:** Listen for DOM changes and update CSSOM
4. **@rules Support:** Add @keyframes, @font-face, @import nodes
5. **CSS Variables:** Store and resolve custom properties
6. **Nested Rules:** Support CSS nesting proposal

## References

- [W3C CSSOM Specification](https://www.w3.org/TR/cssom-1/)
- [MDN: CSS Object Model](https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model)
- [ETAF Architecture](ARCHITECTURE.md)
- [ETAF Data Structures](DATA-STRUCTURES.md)
