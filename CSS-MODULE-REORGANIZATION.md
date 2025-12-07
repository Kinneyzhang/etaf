# CSS Module Reorganization

## Overview

The ETAF CSS system was split across 11 separate files, which made it difficult to understand the overall structure and maintain the codebase. This reorganization consolidates related functionality into 5 core modules with clear responsibilities.

## Problem

The original structure had CSS functionality fragmented across:
- etaf-css-cache.el (70 lines)
- etaf-css-cascade.el (230 lines)
- etaf-css-inheritance.el (106 lines)
- etaf-css-index.el (139 lines)
- etaf-css-parse.el (157 lines)
- etaf-css-media.el (225 lines)
- etaf-css-shorthand.el (578 lines)
- etaf-css-parser.el (190 lines)
- etaf-css-selector.el (1230 lines)
- etaf-css-face.el (245 lines)
- etaf-css.el (main entry point)

This fragmentation made it difficult to:
1. Understand the CSS system architecture
2. Find related functionality
3. Maintain consistency across modules
4. Add new features

## Solution

### New Module Structure

The reorganization groups related functionality into 5 core modules:

#### 1. **etaf-css.el** (Main Entry Point)
- High-level CSSOM construction and style computation
- Orchestrates all CSS subsystems
- Public API for CSS functionality

#### 2. **etaf-css-core.el** (Core CSS Systems - 460 lines)
Consolidates core CSS algorithms and data structures:
- **Cascade Algorithm**: Implements CSS cascade rules including !important handling
- **Specificity Calculation**: Computes selector specificity for cascade ordering
- **Property Inheritance**: Manages automatic inheritance of CSS properties
- **Style Caching**: Optimizes repeated style computations with memoization
- **Rule Indexing**: Provides fast rule lookup by tag, class, and ID

#### 3. **etaf-css-parser.el** (Complete CSS Parsing - 1049 lines)
Consolidates all CSS parsing functionality:
- **CSS Parser**: Parses CSS declarations, rules, and stylesheets
- **Value Parsing**: Handles units (px, %, em, lh, cw) and value conversion
- **Media Queries**: Parses and evaluates @media rules and conditions
- **Shorthand Expansion**: Expands compound properties (border, margin, flex, etc.)

#### 4. **etaf-css-selector.el** (Selector System - 1230 lines)
Kept separate due to its size and self-contained nature:
- CSS selector parsing using AST-based approach
- Selector matching with combinator support
- Interactive pseudo-class support (:hover, :active, etc.)
- Structural pseudo-class support (:nth-child, :first-of-type, etc.)

#### 5. **etaf-css-face.el** (Emacs Face Conversion - 245 lines)
Kept separate as Emacs-specific functionality:
- Converts CSS properties to Emacs face attributes
- Handles color, font, text decoration properties
- Dual-mode style support for light/dark themes

## Migration Guide

All code, tests, and documentation have been updated to use the new module structure:

**For core CSS systems, use:**
```elisp
(require 'etaf-css-core)
```
This provides: cascade algorithm, specificity calculation, property inheritance, style caching, and rule indexing.

**For CSS parsing, use:**
```elisp
(require 'etaf-css-parser)
```
This provides: CSS parser, value parsing (px/em/lh/cw), media query evaluation, and shorthand property expansion.

## Benefits

1. **Reduced File Count**: 11 files → 5 core modules (55% reduction)
2. **Logical Grouping**: Related functionality is now co-located
3. **Clearer Architecture**: Easier to understand the CSS system structure
4. **Better Maintainability**: Less context switching between files
5. **Improved Documentation**: Each module has a clear, focused purpose
6. **Clean API**: All old module references removed, only new modules remain

## Design Principles

The reorganization follows these principles:

1. **Cohesion**: Group tightly related functionality together
2. **Separation of Concerns**: Keep distinct responsibilities in separate modules
3. **Size Balance**: Modules should be large enough to be meaningful but not overwhelming
4. **Clear Interfaces**: Each module exposes a well-defined API
5. **Clean Break**: Old module files removed, all references updated to new modules

## Module Responsibilities

### etaf-css-core.el
**Why merged?** These are all core CSS engine components that work together:
- Cascade and specificity determine which rules win
- Inheritance propagates values from parent to child
- Caching optimizes repeated computations
- Indexing speeds up rule matching

### etaf-css-parser.el  
**Why merged?** These are all parsing and preprocessing steps:
- Parser converts CSS text to structured data
- Value parsing handles unit conversions
- Media queries filter rules based on environment
- Shorthand expansion normalizes property names

### etaf-css-selector.el
**Why separate?** It's large (1230 lines) and self-contained with its own complex logic.

### etaf-css-face.el
**Why separate?** It's Emacs-specific and not part of the core CSS algorithm.

## Testing

All existing tests continue to pass:
- etaf-css-cache-tests.el
- etaf-css-cascade-tests.el (via etaf-css-specificity-tests.el)
- etaf-css-inheritance-tests.el
- etaf-css-index-tests.el
- etaf-css-parse-tests.el
- etaf-css-media-tests.el
- etaf-css-shorthand-tests.el
- etaf-css-selector-tests.el
- etaf-css-face-tests.el

Test files were updated to require the new modules, ensuring comprehensive coverage.

## Future Work

Potential improvements for the future:
1. Consider merging etaf-css-face.el into etaf-css.el (it's relatively small)
2. Document internal APIs for each module
3. Add performance benchmarks to validate caching and indexing benefits
4. Consider splitting etaf-css-selector.el if it grows larger

## Conclusion

This reorganization significantly improves the CSS module structure. The new structure is more maintainable, easier to understand, and better reflects the logical organization of CSS functionality. All old module files have been removed and all references updated to use the new consolidated modules.
