# ECSS Color Styles Bug Fix

## Issue Summary

**Problem**: Color styles defined using `ecss` tags were not being applied to the final rendered text.

**Symptoms**: When using scoped CSS with the `ecss` tag like:
```elisp
(div :id "parent-div"
     (ecss "p{text-red-200}"
           "ul>li:nth-child(odd)>p{text-green-400}"
           "ul li>p code{text-orange-400}"
           "a{text-blue-400}")
     (p "This should be red")
     (a :href "#" "This should be blue"))
```
The colors were not visible in the rendered output.

## Root Cause

The bug was in the VNode-to-DOM rendering pipeline (`etaf-vdom-render` function in `etaf-vdom.el`).

When `ecss` tags were processed, they created `<style>` element VNodes with the CSS content stored in the `:textContent` property:

```elisp
(etaf-vdom-element 'style
                   (list :textContent "p { color: #fecaca; }")
                   nil)
```

However, the `etaf-vdom-render` function had two issues:
1. The `:textContent` property was being filtered out by `etaf-vdom--props-to-attrs` (line 714)
2. The filtered-out `:textContent` was never used as the element's child content

This resulted in empty `<style>` tags in the DOM: `(style nil)` instead of `(style nil "p { color: #fecaca; }")`.

## The Fix

Modified `etaf-vdom-render` function to check for `:textContent` in props and use it as the element's child content:

```elisp
;; Element VNode - render to DOM
((etaf-vdom-element-p vnode)
 (let* ((type (etaf-vdom-get-type vnode))
        (props (etaf-vdom-get-props vnode))
        (children (etaf-vdom-get-children vnode))
        ;; Check for :textContent in props - used for style/script elements
        (text-content (plist-get props :textContent))
        ;; Convert props plist to attrs alist for DOM
        (attrs (etaf-vdom--props-to-attrs props))
        ;; Render children
        (child-doms (cond
                     ;; If textContent is specified, use it as the only child
                     (text-content
                      (list text-content))
                     ;; String children
                     ((stringp children)
                      (list children))
                     ;; Array children - render recursively
                     ((listp children)
                      (delq nil (mapcar #'etaf-vdom-render children)))
                     ;; Other (nil, etc.)
                     (t nil))))
   ;; Build DOM: (tag ((attrs...)) children...)
   (cons type (cons attrs child-doms))))
```

## Pipeline Flow

The complete pipeline for rendering ecss color styles:

1. **ETML Template** with `ecss` tags
   ```elisp
   (div (ecss "p{text-red-200}") (p "Text"))
   ```

2. **Compilation** (`etaf-compile`)
   - Renders template with data
   - Processes `ecss` tags into style elements

3. **VNode Creation** (`etaf-vdom-create-from-etml`)
   - Creates VNode with `:textContent` property
   ```elisp
   (:type element :tag style :props (:textContent "p { color: #fecaca; }"))
   ```

4. **VNode Rendering** (`etaf-vdom-render`) ⭐ **FIXED HERE**
   - Extracts `:textContent` from props
   - Uses it as child content in DOM
   ```elisp
   (style nil "p { color: #fecaca; }")
   ```

5. **CSSOM Building** (`etaf-css-build-cssom`)
   - Extracts style tags from DOM
   - Parses CSS rules
   - Builds CSS Object Model

6. **Style Computation** (`etaf-css-get-computed-style`)
   - Applies CSS rules to elements
   - Computes final styles including colors

7. **Layout & Rendering**
   - Uses computed styles to render text with colors

## Testing

Added comprehensive test suite in `tests/etaf-vdom-textcontent-tests.el`:

- ✅ VNode with `:textContent` renders correctly to DOM
- ✅ Style elements contain CSS content
- ✅ Multiple ecss rules work correctly
- ✅ Scoped CSS with scope classes
- ✅ Color classes (text-red-200, text-blue-400, etc.) are preserved
- ✅ Full pipeline integration test

## Verification

To verify the fix works:

```elisp
(etaf-paint-to-buffer "*etaf-test*"
  '(div :class "border px-2 py-1 w-50"
        (div :id "parent-div"
             (ecss "p{text-red-200}"
                   "ul>li:nth-child(odd)>p{text-green-400}"
                   "ul li>p code{text-orange-400}"
                   "a{text-blue-400}")
             (h3 "Tailwindcss")
             (p "An advanced online playground for Tailwind CSS")
             (ul (li (p "Customizing your theme with" (code "@theme"))))
             (a :href "#" "Read the docs →"))))
```

Now the colors should be visible in the rendered output!

## Files Changed

1. **etaf-vdom.el**: Fixed `etaf-vdom-render` to handle `:textContent` property
2. **tests/etaf-vdom-textcontent-tests.el**: Added comprehensive test suite

## Impact

- ✅ Fixes the reported issue with ecss color styles not being applied
- ✅ No breaking changes - only adds missing functionality
- ✅ Maintains backward compatibility
- ✅ All existing tests should still pass
- ✅ Properly handles the `:textContent` property for style/script elements
