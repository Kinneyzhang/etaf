# Solution Summary: Text Decoration Line Merging Fix

## Problem Statement

When using multiple Tailwind CSS text-decoration classes (like `underline`, `overline`, and `line-through`) on a single element, only the last decoration was being applied. The other decorations were being overwritten instead of combined.

### Original Issue Example
```elisp
(p :class "italic overline underline line-through"
   "Perfect for learning how the framework works.")
```

**Before Fix:** Only `line-through` would be visible
**After Fix:** All three decorations (`underline`, `overline`, `line-through`) are combined and visible

## Root Cause

The CSS cascade algorithm in `etaf-css--merge-style-alists` was treating all CSS properties the same way - when a property appeared in multiple style sources, the later one would completely overwrite the earlier one.

However, according to CSS specifications, the `text-decoration-line` property can accept multiple space-separated values:
```css
text-decoration-line: underline overline line-through;
```

## Solution

Modified the `etaf-css--merge-style-alists` function in `etaf-css.el` to handle `text-decoration-line` specially:

### Key Changes

1. **Combine Values Instead of Overwriting:**
   - When merging two style alists that both contain `text-decoration-line`
   - Split both values into word lists
   - Combine and deduplicate the words
   - Join back with spaces

2. **Special Handling for "none":**
   - `text-decoration-line: none` resets all decorations
   - If the new value is "none", it replaces everything
   - If the existing value is "none", the new value replaces it

3. **Robust String Handling:**
   - Uses `(split-string value nil t)` to properly handle multiple spaces
   - Automatically removes duplicate values with `delete-dups`

### Code Example
```elisp
;; Before: Last value wins
(etaf-css--merge-style-alists 
  '((text-decoration-line . "underline")) 
  '((text-decoration-line . "overline")))
;; => ((text-decoration-line . "overline"))  ❌

;; After: Values are combined
(etaf-css--merge-style-alists 
  '((text-decoration-line . "underline")) 
  '((text-decoration-line . "overline")))
;; => ((text-decoration-line . "underline overline"))  ✓
```

## Testing

Added comprehensive tests in `tests/etaf-css-tests.el`:

1. ✓ Basic merging: `underline` + `overline` = `underline overline`
2. ✓ Three decorations: `underline overline` + `line-through` = `underline overline line-through`
3. ✓ Reset with "none": `underline overline` + `none` = `none`
4. ✓ After "none": `none` + `underline` = `underline`
5. ✓ Duplicate removal: `underline` + `underline overline` = `underline overline`
6. ✓ Complete scenario: Tailwind classes combine correctly in real DOM

All tests pass successfully!

## Files Modified

1. **etaf-css.el** - Modified `etaf-css--merge-style-alists` function
2. **tests/etaf-css-tests.el** - Added comprehensive test cases
3. **examples/demo-text-decoration-fix.el** - Added demonstration example (Chinese + English)

## Impact

- ✅ Fixes the problem statement issue completely
- ✅ No breaking changes to existing functionality
- ✅ All other CSS properties continue to work as before
- ✅ Minimal, surgical changes to the codebase
- ✅ Well-tested with multiple scenarios
- ✅ No security vulnerabilities introduced

## Usage Examples

### Example 1: Multiple Decoration Classes
```elisp
(p :class "underline overline line-through"
   "Text with all three decorations")
;; Result: Text displays with underline, overline, AND line-through
```

### Example 2: Nested Elements
```elisp
(div :class "underline"
     (span :class "overline" "Nested text"))
;; Result: span has both underline (inherited) and overline
```

### Example 3: Reset with no-underline
```elisp
(div :class "underline overline"
     (span :class "no-underline" "No decorations"))
;; Result: span has no decorations (reset by no-underline)
```

## Technical Details

### Why Only text-decoration-line?

Other CSS properties should overwrite:
- `text-transform` (uppercase/lowercase/capitalize) - only one can apply
- `font-style` (italic/normal) - only one can apply
- `display` (block/inline/flex) - only one can apply

Only `text-decoration-line` is designed to accept multiple values according to CSS specs.

### CSS Specification Reference

From CSS Text Decoration Module Level 3:
> The text-decoration-line property specifies what line decorations, if any, 
> are added to the element. The property accepts one or more of the following values:
> none | underline | overline | line-through | blink

Multiple values are space-separated: `text-decoration-line: underline overline;`

## Verification

Run the demonstration:
```bash
cd /home/runner/work/etaf/etaf
emacs --batch -l examples/demo-text-decoration-fix.el
```

This will show all test scenarios and confirm the fix works correctly.
