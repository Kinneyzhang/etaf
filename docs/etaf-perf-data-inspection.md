# ETAF Performance Data Inspection

## Overview

The enhanced `etaf-perf` module now supports capturing and inspecting intermediate data structures generated during the rendering pipeline. This feature helps developers understand how ETAF processes templates and identify potential optimizations.

## Captured Data Structures

When data capture is enabled, the following structures are captured:

1. **Render Function** - The compiled function generated from ETML (for dynamic templates)
2. **Virtual DOM (VNode)** - The virtual DOM tree before rendering to real DOM
3. **DOM Tree** - The actual DOM structure
4. **CSSOM** - CSS Object Model with parsed styles
5. **Render Tree** - DOM + CSSOM combined with computed styles
6. **Layout Tree** - Final layout with calculated positions and dimensions

## Usage

### Basic Usage

```elisp
;; Enable performance monitoring
(etaf-perf-toggle)

;; Ensure data capture is enabled (default: t)
(setq etaf-perf-capture-data t)

;; Render something
(etaf-paint-to-buffer "*demo*"
  '(div :class "container"
     (h1 "{{ title }}")
     (ul
       (li :e-for "item in items" "{{ item }}")))
  '(:title "Demo" :items ("A" "B" "C")))

;; View captured data
(etaf-perf-show-data)
```

### Available Commands

- `etaf-perf-show-data` - Display captured rendering data in a collapsible buffer
- `etaf-perf-show-report` - Display performance metrics (timing information)
- `etaf-perf-show-combined-report` - Display both metrics and data

### Data Inspection Buffer

The data inspection buffer uses `outline-mode` for collapsible sections:

- **TAB** - Toggle current section (expand/collapse)
- **C-c C-a** - Show all sections
- **C-c C-t** - Hide all sections (show only headers)
- **q** - Quit window

Each data structure is displayed as a separate collapsible section with pretty-printed content.

## Configuration

```elisp
;; Enable/disable data capture (default: t)
(setq etaf-perf-capture-data t)

;; Maximum number of measurements to keep in history (default: 100)
(setq etaf-perf-max-history 100)
```

## Example

See `examples/etaf-perf-example.el` for a complete example:

```elisp
;; Run the data inspection example
(etaf-perf-example-7-data-inspection)
```

This example demonstrates:
- Rendering a dynamic template with data binding
- Capturing all intermediate data structures
- Displaying the data with collapsible sections

## Performance Considerations

- Data capture adds minimal overhead to rendering
- Captured data is stored in memory until cleared
- For large templates, pretty-printing may take a moment
- Use `etaf-perf-clear` to free memory when done

## Debugging Workflow

1. Enable performance monitoring: `M-x etaf-perf-toggle`
2. Render your template normally
3. Check performance: `M-x etaf-perf-show-report`
4. Inspect data structures: `M-x etaf-perf-show-data`
5. Analyze specific stages that take too long
6. Examine the corresponding data structures for issues
7. Make optimizations
8. Re-render and compare results

## Notes

- Static templates (no dynamic content) won't have VNode or render function data
- Data capture is automatic when enabled - no code changes needed
- Each render operation captures a new snapshot
- Use `(etaf-perf-show-data 1)` to view the second most recent capture, etc.
