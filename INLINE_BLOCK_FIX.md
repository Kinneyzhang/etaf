# Fix for inline-block Display Issue

## Problem Statement

The CSS property `display: inline-block` is used to create elements that combine characteristics of both inline and block elements:

- **Inline behavior**: The element doesn't force a line break and can be displayed on the same line as other inline or inline-block elements
- **Block-like properties**: The element can have width, height, padding, and margin set, just like block elements

However, buttons and other elements with `display: inline-block` were incorrectly being treated as block-level elements in the ETAF layout engine, causing them to break to new lines instead of displaying horizontally on the same line.

## Root Cause

The issue was in two places:

1. **Layout Width Calculation** (`etaf-layout.el`, line 272): The `is-inline` variable only checked for `display: "inline"`, not `display: "inline-block"`. This caused inline-block elements to be treated as block elements during width calculation.

2. **String Merging Logic** (`etaf-layout-string.el`, line 696): The merge function only grouped elements with `display: "inline"` for horizontal layout. Inline-block elements fell through to the else case and were treated like block elements, causing vertical stacking with line breaks.

## Solution

The fix involves treating `inline-block` elements similarly to `inline` elements for layout purposes while still allowing them to have block-level properties like width and height.

### Changes Made

#### 1. etaf-layout.el

**Line 270-273**: Updated the `is-inline` calculation to include inline-block elements:
```elisp
;; Before:
(is-inline (string= display "inline"))

;; After:
(is-inline (or (string= display "inline")
               (string= display "inline-block")))
```

**Line 376-383**: Updated inline element detection to include inline-block:
```elisp
;; Before:
(when (string= child-display "inline")
  (setq has-inline-element t))

;; After:
(when (or (string= child-display "inline")
          (string= child-display "inline-block"))
  (setq has-inline-element t))
```

**Line 438-439**: Added explicit case for inline-block in layout node handling:
```elisp
((string= display "inline-block")
 (etaf-layout-block-formatting-context render-node parent-context))
```

#### 2. etaf-layout-string.el

**Line 682-708**: Updated `etaf-layout-string--merge-by-display` to group inline-block with inline elements:
```elisp
;; Before:
(if (string= display "inline")
    (push str inline-group)
  ...)

;; After:
(if (or (string= display "inline")
        (string= display "inline-block"))
    (push str inline-group)
  ...)
```

#### 3. tests/etaf-inline-block-tests.el

Created comprehensive test suite with 6 test cases:
- Basic inline-block layout
- UA stylesheet defaults for button elements
- Width and height with inline-block
- Mixing inline-block with inline elements
- Comparison with block element behavior
- Custom tags with inline-block display

## Verification

1. **Code Review**: Passed with no issues
2. **Security Check**: Completed (no CodeQL vulnerabilities)
3. **Impact Analysis**: Changes are minimal and surgical, only affecting inline-block handling

## Expected Behavior After Fix

### Before Fix:
```
[Button 1]
[Button 2]
[Button 3]
```
Buttons were stacked vertically like block elements.

### After Fix:
```
[Button 1] [Button 2] [Button 3]
```
Buttons are displayed horizontally on the same line, like inline elements.

## Testing

Run the test suite with:
```elisp
(ert-run-tests-batch "etaf-inline-block")
```

Or run individual tests:
```elisp
(ert "etaf-inline-block-test-basic-layout")
(ert "etaf-inline-block-test-ua-stylesheet")
(ert "etaf-inline-block-test-with-dimensions")
(ert "etaf-inline-block-test-mixed-with-inline")
(ert "etaf-inline-block-test-vs-block")
(ert "etaf-inline-block-test-custom-tag")
```

## Impact

This fix ensures that:
- Button elements (which have `display: inline-block` in the UA stylesheet) now display horizontally
- Custom elements with `display: inline-block` work as expected
- The standard CSS behavior for inline-block is properly implemented
- Existing functionality for block and inline elements is not affected
