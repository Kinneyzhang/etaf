# ETAF Table Component

**Feature-rich table component inspired by Element Plus**

## Overview

The ETAF table component provides a comprehensive solution for displaying and managing tabular data in your ETAF applications. It includes all the essential features you'd expect from a modern data table component, including sorting, filtering, pagination, row selection, and custom rendering.

## Features

### Core Features
- 📊 **Column Definitions** - Flexible column configuration with customizable properties
- 🎨 **Styling Options** - Striped rows, bordered tables, custom cell/row classes
- 📝 **Data Binding** - Reactive data binding with automatic updates
- 🔄 **Custom Formatters** - Transform data display with custom formatter functions
- 📐 **Column Widths** - Set fixed or flexible column widths

### Sorting
- ⬆️⬇️ **Column Sorting** - Click headers to sort ascending/descending
- 🔢 **Custom Sort** - Define custom sort functions for complex data types
- 🔄 **Sort Order** - Toggle between asc, desc, and no sort
- 🎯 **Default Sort** - Set initial sort column and order

### Selection
- ☑️ **Row Selection** - Single or multiple row selection
- ✅ **Select All** - Checkbox in header to select/deselect all visible rows
- 🎯 **Selection State** - Track selected rows with unique keys
- 🔄 **Selection Callbacks** - React to selection changes

### Filtering
- 🔍 **Column Filters** - Filter data by column values
- ⚡ **Custom Filters** - Define custom filter functions
- 🔄 **Multiple Filters** - Apply filters to multiple columns simultaneously

### Pagination
- 📄 **Page Navigation** - First, Previous, Next, Last buttons
- 📊 **Page Size** - Configurable rows per page
- 📈 **Page Info** - Display current page and total count
- 🎯 **Page State** - Track current page with reactive updates

### Advanced
- 📭 **Empty State** - Customizable message when no data
- ⏳ **Loading State** - Loading overlay with custom message
- 🎨 **Custom Rendering** - Full control over cell rendering
- 📇 **Index Column** - Auto-incrementing row numbers

## Basic Usage

```elisp
(require 'etaf-table)

;; Define columns (width is in character widths for text-based rendering)
(setq my-columns
  '((:prop "name" :label "Name" :width 20)
    (:prop "age" :label "Age" :width 5)
    (:prop "email" :label "Email" :width 30)))

;; Define data
(setq my-data
  '((:id 1 :name "Alice" :age 28 :email "alice@example.com")
    (:id 2 :name "Bob" :age 32 :email "bob@example.com")))

;; Render table
(etaf-paint-to-buffer "*my-table*"
  '(etaf-table :data my-data
               :columns my-columns
               :row-key "id"))
```

## Props

### Data Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:data` | list | `'()` | Array of data to display |
| `:columns` | list | `'()` | Array of column definitions |
| `:row-key` | string | `nil` | Unique key property for rows |

### Display Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:stripe` | boolean | `nil` | Enable striped rows |
| `:border` | boolean | `nil` | Enable table borders |
| `:show-header` | boolean | `t` | Show table header |
| `:show-index` | boolean | `nil` | Show index column |
| `:height` | number | `nil` | Fixed table height |
| `:max-height` | number | `nil` | Maximum table height |

### Selection Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:show-selection` | boolean | `nil` | Enable row selection |

### Sorting Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:default-sort` | plist | `nil` | Default sort (`(:prop "name" :order "asc")`) |

### Pagination Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:show-pagination` | boolean | `nil` | Enable pagination |
| `:page-size` | number | `10` | Rows per page |
| `:current-page` | number | `1` | Initial page number |

### State Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:loading` | boolean | `nil` | Show loading overlay |
| `:loading-text` | string | `"Loading..."` | Loading message |
| `:empty-text` | string | `"No data"` | Empty state message |

### Style Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `:row-class-name` | function | `nil` | Function to determine row class |
| `:cell-class-name` | function | `nil` | Function to determine cell class |
| `:header-cell-class-name` | function | `nil` | Function to determine header cell class |

## Column Definition

Each column is defined as a plist with the following properties:

### Basic Properties

| Property | Type | Description |
|----------|------|-------------|
| `:prop` | string | Property name in data object |
| `:label` | string | Column header text |
| `:width` | number | Fixed column width in character widths (cw) |
| `:type` | string | Special column type (`"selection"`, `"index"`) |

### Display Properties

| Property | Type | Description |
|----------|------|-------------|
| `:formatter` | function | Custom cell formatter `(lambda (row col value) ...)` |
| `:class-name` | string | Custom cell class name |
| `:header-class-name` | string | Custom header class name |

### Sorting Properties

| Property | Type | Description |
|----------|------|-------------|
| `:sortable` | boolean | Enable sorting for this column |
| `:sort-method` | function | Custom sort comparator `(lambda (a b) ...)` |

### Filtering Properties

| Property | Type | Description |
|----------|------|-------------|
| `:filter-method` | function | Custom filter function `(lambda (value row col) ...)` |

## Examples

### Example 1: Basic Table

```elisp
(setq columns
  '((:prop "name" :label "Name" :width 20)
    (:prop "email" :label "Email" :width 30)
    (:prop "department" :label "Department" :width 15)))

(setq data
  '((:id 1 :name "Alice" :email "alice@example.com" :department "Engineering")
    (:id 2 :name "Bob" :email "bob@example.com" :department "Marketing")))

(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data :columns columns :row-key "id"))
```

### Example 2: Sortable Columns

```elisp
(setq columns
  '((:prop "name" :label "Name" :width 20 :sortable t)
    (:prop "age" :label "Age" :width 5 :sortable t)
    (:prop "salary" :label "Salary" :width 12 :sortable t
     :formatter (lambda (row col value) (format "$%d" value)))))

(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :row-key "id"
               :default-sort (:prop "name" :order "asc")))
```

### Example 3: Row Selection

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :show-selection t
               :row-key "id"))
```

### Example 4: With Pagination

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :show-pagination t
               :page-size 10
               :current-page 1
               :row-key "id"))
```

### Example 5: Striped and Bordered

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :stripe t
               :border t
               :row-key "id"))
```

### Example 6: Custom Formatters

```elisp
(setq columns
  '((:prop "name" :label "Name" :width 20)
    (:prop "status" :label "Status" :width 12
     :formatter (lambda (row col value)
                 (if (equal value "active")
                     "✓ Active"
                   "✗ Inactive")))
    (:prop "created" :label "Created" :width 15
     :formatter (lambda (row col value)
                 (format-time-string "%Y-%m-%d" value)))))

(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data :columns columns :row-key "id"))
```

### Example 7: Index Column

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :show-index t
               :row-key "id"))
```

### Example 8: Loading State

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :loading t
               :loading-text "Loading data..."
               :row-key "id"))
```

### Example 9: Empty State

```elisp
(etaf-paint-to-buffer "*table*"
  '(etaf-table :data '()
               :columns columns
               :empty-text "No employees found"
               :border t))
```

### Example 10: Complete Feature Demo

```elisp
(setq columns
  '((:prop "name" :label "Employee" :width 20 :sortable t)
    (:prop "age" :label "Age" :width 5 :sortable t)
    (:prop "department" :label "Department" :width 15 :sortable t)
    (:prop "salary" :label "Salary" :width 12 :sortable t
     :formatter (lambda (row col value) (format "$%,d" value)))
    (:prop "status" :label "Status" :width 12
     :formatter (lambda (row col value)
                 (if (equal value "active") "✓ Active" "✗ Inactive")))))

(etaf-paint-to-buffer "*table*"
  '(etaf-table :data data
               :columns columns
               :show-selection t
               :show-index t
               :stripe t
               :border t
               :show-pagination t
               :page-size 5
               :row-key "id"
               :default-sort (:prop "name" :order "asc")))
```

## Running Examples

Load and run the example file:

```elisp
;; Load examples
(load-file "examples/etaf-table-example.el")

;; Run interactive demo
M-x etaf-table-demo

;; Or run specific examples
(etaf-table-example-1-basic)
(etaf-table-example-3-sortable)
(etaf-table-example-7-pagination)
(etaf-table-example-10-complete)
```

## Styling

The table component comes with default styles that follow Element Plus design patterns. You can customize the appearance by:

1. **Using Built-in Options**: `stripe`, `border`, etc.
2. **Custom CSS**: Override default styles in your stylesheet
3. **Class Functions**: Use `:row-class-name`, `:cell-class-name` props

### CSS Classes

- `.etaf-table-container` - Main container
- `.etaf-table` - Table element
- `.etaf-table--striped` - Striped table modifier
- `.etaf-table--border` - Bordered table modifier
- `.etaf-table__header` - Table header
- `.etaf-table__header-cell` - Header cell
- `.etaf-table__body` - Table body
- `.etaf-table__row` - Table row
- `.etaf-table__row--selected` - Selected row
- `.etaf-table__row--striped` - Striped row
- `.etaf-table__cell` - Table cell
- `.etaf-table__empty-cell` - Empty state cell
- `.etaf-table-pagination` - Pagination container
- `.etaf-table-pagination__btn` - Pagination button
- `.etaf-table-loading` - Loading overlay

## Comparison with Element Plus

This component implements the core features of [Element Plus Table](https://element-plus.org/en-US/component/table):

| Feature | Element Plus | ETAF Table | Status |
|---------|-------------|------------|--------|
| Basic Table | ✅ | ✅ | ✅ Implemented |
| Striped Rows | ✅ | ✅ | ✅ Implemented |
| Border | ✅ | ✅ | ✅ Implemented |
| Status | ✅ | ✅ | ✅ Implemented |
| Fixed Header | ✅ | ⚠️ | 🔄 Partial (via `:height`) |
| Fixed Column | ✅ | ❌ | 📋 Future |
| Fluid Height | ✅ | ⚠️ | 🔄 Partial |
| Multi-level Header | ✅ | ❌ | 📋 Future |
| Single Select | ✅ | ✅ | ✅ Implemented |
| Multiple Select | ✅ | ✅ | ✅ Implemented |
| Sorting | ✅ | ✅ | ✅ Implemented |
| Filtering | ✅ | ✅ | ✅ Implemented |
| Custom Column | ✅ | ✅ | ✅ Implemented (formatters) |
| Expandable Row | ✅ | ❌ | 📋 Future |
| Tree Data | ✅ | ❌ | 📋 Future |
| Summary Row | ✅ | ❌ | 📋 Future |
| Merge Cell | ✅ | ❌ | 📋 Future |
| Custom Index | ✅ | ✅ | ✅ Implemented |

Legend:
- ✅ Fully implemented
- ⚠️ Partially implemented
- ❌ Not yet implemented
- 📋 Planned for future release
- 🔄 Work in progress

## Best Practices

### 1. Always Use Row Keys

Use `:row-key` for proper row identification, especially with selection:

```elisp
;; Good
:row-key "id"

;; Avoid - may cause selection issues
; No row-key specified
```

### 2. Keep Data Immutable

Work with copies of data to avoid mutation issues:

```elisp
;; Good
(setq new-data (copy-sequence original-data))

;; Avoid
(setq data original-data)  ; Direct reference
```

### 3. Use Formatters for Display Logic

Keep formatting logic in formatters rather than pre-processing data:

```elisp
;; Good
:formatter (lambda (row col value) (format "$%.2f" value))

;; Avoid pre-formatting data
```

### 4. Optimize Large Datasets

Use pagination for large datasets:

```elisp
;; Good for 100+ rows
:show-pagination t
:page-size 20

;; May be slow for very large datasets without pagination
```

### 5. Meaningful Column Labels

Use clear, concise column labels:

```elisp
;; Good
:label "Employee Name"

;; Avoid
:label "n"  ; Too brief
:label "The Full Name of the Employee"  ; Too long
```

## Troubleshooting

### Table Not Rendering

**Problem**: Table doesn't appear

**Solution**: Check that you've required both modules:
```elisp
(require 'etaf)
(require 'etaf-table)
```

### Sorting Not Working

**Problem**: Clicking headers doesn't sort

**Solution**: Ensure `:sortable t` is set on columns:
```elisp
(:prop "name" :label "Name" :sortable t)
```

### Selection Not Working

**Problem**: Checkboxes don't appear or work

**Solution**: Ensure `:show-selection t` and `:row-key` are set:
```elisp
:show-selection t
:row-key "id"
```

### Pagination Not Working

**Problem**: Pagination controls don't appear

**Solution**: Ensure `:show-pagination t` is set:
```elisp
:show-pagination t
:page-size 10
```

### Formatters Not Applied

**Problem**: Custom formatters don't work

**Solution**: Ensure formatter function signature is correct:
```elisp
:formatter (lambda (row col value)
            ;; row = current row data
            ;; col = column definition
            ;; value = cell value
            (format "Formatted: %s" value))
```

## API Reference

See `etaf-table.el` for the complete API documentation.

### Component Registration

The table is automatically registered as `etaf-table` component when you load the module.

### Key Functions

- `etaf-table--sort-data` - Sort data by column
- `etaf-table--filter-data` - Filter data by conditions
- `etaf-table--paginate-data` - Paginate data
- `etaf-table--get-cell-value` - Get cell value from row
- `etaf-table--format-cell-value` - Format cell value with formatter

## License

GNU General Public License v3.0 or later.

## See Also

- [ETAF Component System](COMPONENT-SYSTEM.md)
- [ETAF Reactive System](REACTIVE-IMPLEMENTATION-SUMMARY.md)
- [Element Plus Table Documentation](https://element-plus.org/en-US/component/table)
