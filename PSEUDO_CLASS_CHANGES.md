# Moving Interactive State Styles to UA Stylesheet

## Issue
Previously, interactive state styles (`:hover-style`, `:active-style`, `:disabled-style`, `:focus-style`) were defined in the tag definitions in `etaf-etml-tag.el`. This violated the principle of keeping tag definitions minimal and made the styling less maintainable.

## Solution
Moved all interactive state styles from tag definitions to the UA stylesheet using CSS pseudo-classes.

## Changes Made

### 1. UA Stylesheet (`etaf-ua-stylesheet.el`)
Added pseudo-class rules for interactive states:

```elisp
;; Links
"a{text-blue underline cursor-pointer}"
"a:hover{text-blue-700}"

;; Buttons
"button{inline}"
"button:hover{bg-gray-200}"
"button:active{bg-gray-300}"
"button:disabled{bg-gray-100 text-gray-500 cursor-not-allowed}"

;; Form elements
"input{border border-gray-300}"
"input:focus{border-blue-500}"
"input:disabled{bg-gray-100 text-gray-500}"
"textarea{border border-gray-300 font-mono}"
"textarea:focus{border-blue-500}"
```

### 2. Tag Definitions (`etaf-etml-tag.el`)
Removed state style properties, keeping only behavioral metadata:

**Before:**
```elisp
(define-etaf-etml-tag button
  :hover-style '((background-color . "#e0e0e0"))
  :active-style '((background-color . "#d0d0d0"))
  :disabled-style '((background-color . "#f5f5f5")
                    (color . "#999")
                    (cursor . "not-allowed"))
  :on-click ...)
```

**After:**
```elisp
(define-etaf-etml-tag button
  :on-click ...)
```

## Benefits

1. **True Separation of Concerns**: Tag definitions now contain only behavioral logic (event handlers, metadata), not styling
2. **Standard CSS Practice**: Uses standard CSS pseudo-classes (`:hover`, `:active`, `:focus`, `:disabled`)
3. **Centralized Styling**: All default styles in one place (UA stylesheet)
4. **Easier Customization**: Users can override pseudo-class styles via normal CSS cascade
5. **Better Alignment**: Follows browser architecture where UA stylesheets handle all default presentation

## Testing
Added test case `etaf-ecss-test-pseudo-classes-in-unified` to verify pseudo-class selectors work correctly in the unified ECSS format.

## Impact
- No breaking changes for end users
- Tag behavior unchanged
- Styles appear identical to users
- More maintainable codebase
