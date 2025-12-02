# Interactive State Styles Implementation

## Problem
CSS pseudo-classes like `:hover`, `:active`, `:focus`, and `:disabled` don't work automatically in Emacs text buffers. Emacs doesn't have a built-in CSS engine that evaluates these pseudo-classes dynamically.

## Solution: Dual Approach
We implement interactive state styles using a **dual approach** that combines both CSS standards and Emacs native capabilities:

### 1. UA Stylesheet (CSS Pseudo-Classes)
Defines styles using standard CSS pseudo-class syntax:

```elisp
(etaf-ecss
 "a:hover{text-blue-700}"
 "button:hover{bg-gray-200}"
 "button:active{bg-gray-300}"
 "button:disabled{bg-gray-100 text-gray-500 cursor-not-allowed}"
 "input:focus{border-blue-500}"
 "input:disabled{bg-gray-100 text-gray-500}")
```

**Purpose:**
- Semantic correctness (follows web standards)
- Used by CSS selector matching system
- Enables CSS cascade and specificity rules
- Allows customization via normal CSS mechanisms

### 2. Tag Definitions (Emacs Native Implementation)
Defines the same styles using Emacs text properties:

```elisp
(define-etaf-etml-tag button
  :hover-style '((background-color . "#e5e7eb"))  ; bg-gray-200
  :active-style '((background-color . "#d1d5db"))  ; bg-gray-300
  :disabled-style '((background-color . "#f3f4f6")
                    (color . "#9ca3af")
                    (cursor . "not-allowed"))
  :on-click ...)
```

**Purpose:**
- Actual functional implementation
- Uses Emacs `mouse-face` property for hover effects
- Integrates with `etaf-event` system for state tracking
- Provides real visual feedback to users

## How It Works

### Hover Effects
1. **Event Tracking**: `etaf-event-update-hover-state` tracks mouse position
2. **State Update**: Sets `:hover` state on elements under mouse
3. **CSS Matching**: Selector system matches `element:hover` rules
4. **Visual Feedback**: `mouse-face` property applies the hover style
5. **Style Source**: Uses `:hover-style` from tag definition

### Focus Effects
1. **Focus Tracking**: Cursor position tracked in interactive elements
2. **State Update**: Sets `:focus` state on focused element
3. **CSS Matching**: Selector system matches `element:focus` rules
4. **Visual Application**: Focus style applied via text properties

### Active Effects
1. **Mouse Events**: Mouse-down sets `:active` state
2. **State Tracking**: `etaf-event--active-element` tracks active element
3. **CSS Matching**: Selector system matches `element:active` rules
4. **Visual Application**: Active style applied

### Disabled State
1. **Attribute Check**: `disabled` attribute on element
2. **Static State**: Doesn't change dynamically
3. **CSS Matching**: Selector system matches `element:disabled` rules
4. **Style Application**: Disabled style always applied

## Why Both Are Needed

| Aspect | UA Stylesheet | Tag Definitions |
|--------|---------------|-----------------|
| **Format** | CSS pseudo-classes | Emacs properties |
| **Purpose** | Semantic/Standards | Functional |
| **Used By** | CSS selector system | Rendering system |
| **Dynamic** | Matched at runtime | Applied via text props |
| **Customizable** | Via CSS cascade | Via tag inheritance |

## Benefits

1. **Standards Compliance**: CSS pseudo-classes follow web standards
2. **Functional**: Emacs-native implementation actually works
3. **Maintainable**: Styles defined once in UA stylesheet, mirrored in tags
4. **Extensible**: Users can override via either mechanism
5. **Consistent**: Same visual results via different technical paths

## Color Mapping
To ensure consistency between UA stylesheet and tag definitions:

| Tailwind Class | Hex Color | Used For |
|----------------|-----------|----------|
| `text-blue-700` | `#1d4ed8` | a:hover |
| `bg-gray-200` | `#e5e7eb` | button:hover |
| `bg-gray-300` | `#d1d5db` | button:active |
| `bg-gray-100` | `#f3f4f6` | disabled background |
| `text-gray-500` | `#9ca3af` | disabled text |
| `border-blue-500` | `#3b82f6` | input:focus |

## Implementation Details

### Emacs Text Properties Used
- `mouse-face`: Visual hover effect (Emacs built-in)
- `keymap`: Click and keyboard event handlers
- `help-echo`: Hover state tracking function
- `pointer`: Cursor shape (hand for clickable)

### Event System Integration
- `etaf-event` module tracks element states
- `etaf-event-register-element`: Register interactive elements
- `etaf-event-update-hover-state`: Update hover state from mouse position
- `etaf-event-matches-pseudo-class-p`: Check if element matches pseudo-class

### CSS System Integration
- `etaf-css-selector-check-interactive-state`: Match pseudo-class selectors
- CSS cascade still applies with proper specificity
- Pseudo-class styles can be overridden by inline styles

## Future Improvements

Potential enhancements:
1. Auto-generate tag `:hover-style` from UA stylesheet pseudo-class rules
2. Support more pseudo-classes (`:visited`, `:checked`, etc.)
3. Optimize state tracking for better performance
4. Add visual feedback for `:focus-visible` (keyboard focus only)
5. Support CSS transitions for smooth state changes

## Conclusion

This dual approach provides the best of both worlds:
- **CSS pseudo-classes** in UA stylesheet for semantic correctness
- **Emacs text properties** in tag definitions for functional implementation

Both work together to provide a complete, standards-compliant, and functional interactive styling system in Emacs.
