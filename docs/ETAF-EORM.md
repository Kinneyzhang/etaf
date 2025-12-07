# ETAF-EORM Documentation

ETAF-EORM is an ORM (Object-Relational Mapping) library for Emacs, inspired by [Diesel](https://github.com/diesel-rs/diesel), the popular Rust database ORM. It provides a type-safe, composable query builder interface for SQLite databases, deeply integrated with the ETAF framework.

## Table of Contents

- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Schema Definition](#schema-definition)
- [CRUD Operations](#crud-operations)
- [Query Builder](#query-builder)
- [Transactions](#transactions)
- [Integration with ETAF](#integration-with-etaf)
- [API Reference](#api-reference)
- [Examples](#examples)

## Features

- **Schema Definition**: Define table schemas with type safety
- **Composable Query Builder**: Build complex queries with a chainable API
- **CRUD Operations**: Simple and safe Create, Read, Update, Delete operations
- **Migration System**: Version-controlled schema management
- **Transaction Support**: ACID-compliant transaction handling
- **Type Safety**: Automatic type conversion between Emacs Lisp and SQLite
- **Reactive Integration**: Automatic UI updates when data changes
- **Query Logging**: Optional SQL query logging for debugging
- **Protection**: Prevents accidental mass updates/deletes

## Requirements

- Emacs 29.1 or later with SQLite support
- ETAF framework (for reactive features)

Check if your Emacs has SQLite support:

```elisp
(and (fboundp 'sqlite-available-p)
     (sqlite-available-p))
```

## Installation

Add ETAF-EORM to your load path and require it:

```elisp
(add-to-list 'load-path "/path/to/etaf")
(require 'etaf-eorm)
```

Enable query logging (optional):

```elisp
(setq etaf-eorm-enable-logging t)
```

## Quick Start

Here's a complete example showing basic usage:

```elisp
;; 1. Define a schema
(etaf-eorm-define-table users
  (id integer :primary-key t :autoincrement t)
  (name text :not-null t)
  (email text :unique t)
  (age integer)
  (created-at datetime :default current-timestamp))

;; 2. Connect to database
(setq db (etaf-eorm-connect "~/my-app.db"))

;; 3. Create tables
(etaf-eorm-migrate db)

;; 4. Insert data
(etaf-eorm-insert db 'users
  :name "Alice"
  :email "alice@example.com"
  :age 30)

;; 5. Query data
(etaf-eorm-select db 'users
  :where '(> age 25)
  :order-by 'name)

;; 6. Update data
(etaf-eorm-update db 'users
  :set '(:age 31)
  :where '(= name "Alice"))

;; 7. Delete data
(etaf-eorm-delete db 'users
  :where '(= email "alice@example.com"))

;; 8. Disconnect
(etaf-eorm-disconnect db)
```

## Schema Definition

### Defining Tables

Use `etaf-eorm-define-table` to define your database schema:

```elisp
(etaf-eorm-define-table table-name
  (column-name type &rest options)
  ...)
```

### Supported Types

- `integer` - Integer numbers
- `text` - Text strings
- `real` - Floating point numbers
- `blob` - Binary data
- `datetime` - Date and time strings
- `boolean` - Boolean values (stored as INTEGER)

### Column Options

- `:primary-key t` - Mark as primary key
- `:autoincrement t` - Auto-increment (for INTEGER PRIMARY KEY only)
- `:not-null t` - NOT NULL constraint
- `:unique t` - UNIQUE constraint
- `:default value` - Default value (use `current-timestamp` for timestamps)
- `:references (table column)` - Foreign key reference

### Example

```elisp
(etaf-eorm-define-table blog-posts
  (id integer :primary-key t :autoincrement t)
  (user-id integer :not-null t :references (users id))
  (title text :not-null t)
  (content text)
  (published boolean :default nil)
  (views integer :default 0)
  (created-at datetime :default current-timestamp)
  (updated-at datetime))
```

## CRUD Operations

### Create (Insert)

Insert a new row:

```elisp
(etaf-eorm-insert db 'users
  :name "Bob"
  :email "bob@example.com"
  :age 25)
;; Returns the ID of the inserted row
```

### Read (Select)

Select all rows:

```elisp
(etaf-eorm-select db 'users)
```

Select specific columns:

```elisp
(etaf-eorm-select db 'users
  :columns '(name email))
```

Select with conditions:

```elisp
(etaf-eorm-select db 'users
  :where '(and (> age 25) (= active t))
  :order-by '((name asc))
  :limit 10
  :offset 0)
```

### Update

Update rows (requires WHERE clause):

```elisp
(etaf-eorm-update db 'users
  :set '(:age 31 :email "new@example.com")
  :where '(= name "Alice"))
;; Returns the number of affected rows
```

### Delete

Delete rows (requires WHERE clause):

```elisp
(etaf-eorm-delete db 'users
  :where '(= email "alice@example.com"))
;; Returns the number of deleted rows
```

## Query Builder

The query builder provides a chainable interface for building queries:

```elisp
;; Basic query
(etaf-eorm-query-get
  (etaf-eorm-query db 'users))

;; Complex query
(etaf-eorm-query-get
  (etaf-eorm-query-order-by
    (etaf-eorm-query-where
      (etaf-eorm-query-select
        (etaf-eorm-query db 'users)
        'name 'email)
      '(> age 25))
    '((name asc) (age desc))))

;; Get first result
(etaf-eorm-query-first
  (etaf-eorm-query-where
    (etaf-eorm-query db 'users)
    '(= email "alice@example.com")))
```

## WHERE Clause Syntax

ETAF-EORM supports a rich WHERE clause syntax:

### Comparison Operators

```elisp
'(= column value)      ; Equal
'(!= column value)     ; Not equal
'(> column value)      ; Greater than
'(< column value)      ; Less than
'(>= column value)     ; Greater than or equal
'(<= column value)     ; Less than or equal
```

### Pattern Matching

```elisp
'(like column pattern)     ; LIKE pattern matching
'(in column (val1 val2))   ; IN list
```

### NULL Checks

```elisp
'(is-null column)          ; IS NULL
'(is-not-null column)      ; IS NOT NULL
```

### Logical Operators

```elisp
'(and expr1 expr2 ...)     ; Logical AND
'(or expr1 expr2 ...)      ; Logical OR
'(not expr)                ; Logical NOT
```

### Complex Examples

```elisp
;; Multiple conditions with AND
'(and (> age 25) (= active t) (like email "%@gmail.com"))

;; OR conditions
'(or (= role "admin") (= role "moderator"))

;; Nested conditions
'(and (> age 18)
      (or (= country "US") (= country "CA"))
      (not (= banned t)))
```

## Transactions

Execute multiple operations atomically:

```elisp
;; Using macro
(etaf-eorm-with-transaction db
  (etaf-eorm-insert db 'accounts :name "Alice" :balance 1000)
  (etaf-eorm-insert db 'accounts :name "Bob" :balance 500))

;; Using function
(etaf-eorm-transaction db
  (lambda ()
    (etaf-eorm-update db 'accounts
      :set '(:balance 900)
      :where '(= name "Alice"))
    (etaf-eorm-update db 'accounts
      :set '(:balance 600)
      :where '(= name "Bob"))))
```

If any operation fails, the entire transaction is rolled back.

## Integration with ETAF

### Reactive Queries

Create queries that automatically update when data changes:

```elisp
(require 'etaf-component)

;; Create a reactive query
(let ((users-ref (etaf-eorm-reactive-query
                   db 'users
                   (lambda (db table)
                     (etaf-eorm-select db table
                       :where '(= active t)
                       :order-by 'name)))))
  
  ;; Watch for changes
  (etaf-watch users-ref
    (lambda (new-val old-val)
      (message "Users updated: %d active users" (length new-val))))
  
  ;; Any insert/update/delete will trigger the watch
  (etaf-eorm-insert db 'users
    :name "Charlie" :active t))
```

### Display Data in ETAF UI

```elisp
(let* ((users (etaf-eorm-select db 'users :order-by 'name))
       (user-rows
        (mapcar
         (lambda (user)
           `(tr
             (td ,(plist-get user :name))
             (td ,(plist-get user :email))))
         users)))
  
  (etaf-paint-to-buffer "*Users*"
    `(table
      (thead (tr (th "Name") (th "Email")))
      (tbody ,@user-rows))))
```

## Utility Functions

### Count

Count rows matching conditions:

```elisp
(etaf-eorm-count db 'users)
;; => 10

(etaf-eorm-count db 'users :where '(> age 25))
;; => 5
```

### Exists

Check if any rows match conditions:

```elisp
(etaf-eorm-exists-p db 'users :where '(= email "alice@example.com"))
;; => t or nil
```

### Find by ID

Find a single row by ID:

```elisp
(etaf-eorm-find-by-id db 'users 42)
;; => plist of the user with id=42

;; Custom ID column
(etaf-eorm-find-by-id db 'posts 123 'post-id)
```

## Migration System

Create tables based on defined schemas:

```elisp
;; Create a single table
(etaf-eorm-create-table db 'users)

;; Drop a table
(etaf-eorm-drop-table db 'users)

;; Check if table exists
(etaf-eorm-table-exists-p db 'users)
;; => t or nil

;; Migrate all defined tables
(etaf-eorm-migrate db)
```

## API Reference

### Connection Management

- `(etaf-eorm-connect db-path)` - Connect to database
- `(etaf-eorm-disconnect db)` - Disconnect from database
- `(etaf-eorm-get-connection db-path)` - Get or create connection

### Schema Definition

- `(etaf-eorm-define-table name &rest columns)` - Define table schema
- `(etaf-eorm-get-schema table-name)` - Get schema definition

### Migrations

- `(etaf-eorm-create-table db table-name)` - Create table
- `(etaf-eorm-drop-table db table-name)` - Drop table
- `(etaf-eorm-table-exists-p db table-name)` - Check if table exists
- `(etaf-eorm-migrate db)` - Run migrations

### CRUD Operations

- `(etaf-eorm-insert db table-name &rest values)` - Insert row
- `(etaf-eorm-select db table-name &rest args)` - Select rows
- `(etaf-eorm-update db table-name &rest args)` - Update rows
- `(etaf-eorm-delete db table-name &rest args)` - Delete rows

### Query Builder

- `(etaf-eorm-query db table-name)` - Create query builder
- `(etaf-eorm-query-select query &rest columns)` - Add SELECT
- `(etaf-eorm-query-where query expr)` - Add WHERE
- `(etaf-eorm-query-order-by query &rest spec)` - Add ORDER BY
- `(etaf-eorm-query-limit query n)` - Add LIMIT
- `(etaf-eorm-query-offset query n)` - Add OFFSET
- `(etaf-eorm-query-get query)` - Execute query
- `(etaf-eorm-query-first query)` - Get first result

### Transactions

- `(etaf-eorm-transaction db func)` - Execute in transaction
- `(etaf-eorm-with-transaction db &rest body)` - Transaction macro

### Utilities

- `(etaf-eorm-count db table-name &rest args)` - Count rows
- `(etaf-eorm-exists-p db table-name &rest args)` - Check existence
- `(etaf-eorm-find-by-id db table-name id)` - Find by ID

### Reactive Integration

- `(etaf-eorm-reactive-query db table-name query-fn)` - Create reactive query

## Examples

See `examples/etaf-eorm-example.el` for comprehensive examples including:

1. Basic CRUD operations
2. Query builder usage
3. Transaction handling
4. Reactive queries
5. Integration with ETAF UI

Run all examples:

```elisp
(load-file "examples/etaf-eorm-example.el")
(etaf-eorm-run-all-examples)
```

## Best Practices

1. **Always use WHERE clauses** with UPDATE and DELETE to prevent accidental mass operations
2. **Use transactions** for operations that need to be atomic
3. **Define schemas** before creating tables
4. **Enable logging** during development for debugging
5. **Use the query builder** for complex queries
6. **Leverage reactive queries** for real-time UI updates
7. **Close connections** when done to free resources

## Inspiration from Diesel

ETAF-EORM takes inspiration from Diesel's design philosophy:

- **Type Safety**: Column and table names are checked against schemas
- **Composable Queries**: Query builder allows building queries step by step
- **Safe API**: Prevents common mistakes like mass updates/deletes
- **Migration System**: Schema versioning and management
- **Performance**: Efficient query generation and execution

## Comparison with Diesel

| Feature | Diesel (Rust) | ETAF-EORM (Emacs Lisp) |
|---------|--------------|------------------------|
| Schema Definition | Rust structs + macros | Emacs Lisp macros |
| Type Safety | Compile-time | Runtime |
| Query Builder | Yes | Yes |
| Transactions | Yes | Yes |
| Migrations | CLI tool | Programmatic |
| Reactive Integration | No | Yes (with ETAF) |
| Database Support | PostgreSQL, MySQL, SQLite | SQLite (via Emacs) |

## License

GNU General Public License v3.0 or later.

## Contributing

Contributions are welcome! Please ensure:

- Code follows Emacs Lisp conventions
- Tests are included for new features
- Documentation is updated
- Commit messages are clear and descriptive

## See Also

- [Diesel ORM](https://github.com/diesel-rs/diesel) - The inspiration for ETAF-EORM
- [ETAF Framework](https://github.com/Kinneyzhang/etaf) - The parent framework
- [SQLite Documentation](https://www.sqlite.org/docs.html) - SQLite reference
