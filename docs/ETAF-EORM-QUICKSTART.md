# Getting Started with ETAF-EORM

ETAF-EORM is now available! This quick guide will help you get started with the ORM module that supports SQLite, PostgreSQL, and MySQL databases.

## Quick Start

### 1. Check Requirements

For SQLite support, ensure you have Emacs 29.1 or later with SQLite support:

```elisp
;; Check Emacs version
M-x emacs-version

;; Check SQLite support
(and (fboundp 'sqlite-available-p)
     (sqlite-available-p))
;; Should return t
```

For PostgreSQL and MySQL, ensure the respective command-line clients are installed:
- PostgreSQL: `psql` command should be available
- MySQL: `mysql` command should be available

### 2. Load ETAF-EORM

```elisp
(require 'etaf)  ;; This now includes etaf-eorm
```

### 3. Your First Database

Create a simple todo application with SQLite:

```elisp
;; Enable logging to see SQL queries (optional)
(setq etaf-eorm-enable-logging t)

;; Define schema
(etaf-eorm-define-table todos
  (id integer :primary-key t :autoincrement t)
  (title text :not-null t)
  (completed boolean :default nil)
  (created-at datetime :default current-timestamp))

;; Connect to SQLite database
(setq db (etaf-eorm-connect :sqlite "~/todos.db"))

;; Or connect to PostgreSQL
;; (setq db (etaf-eorm-connect :postgresql
;;   :host "localhost" :port 5432
;;   :database "mydb" :user "myuser" :password "mypass"))

;; Or connect to MySQL
;; (setq db (etaf-eorm-connect :mysql
;;   :host "localhost" :port 3306
;;   :database "mydb" :user "myuser" :password "mypass"))

;; Create table (specify which tables to migrate)
(etaf-eorm-migrate db 'todos)

;; Insert some todos
(etaf-eorm-insert db 'todos
  :title "Learn ETAF-EORM"
  :completed nil)

(etaf-eorm-insert db 'todos
  :title "Build an app"
  :completed nil)

;; Query all todos
(etaf-eorm-select db 'todos)
;; => List of plists with todo data

;; Query incomplete todos
(etaf-eorm-select db 'todos
  :where '(= completed 0)
  :order-by 'created-at)

;; Complete a todo
(etaf-eorm-update db 'todos
  :set '(:completed 1)
  :where '(= title "Learn ETAF-EORM"))

;; Count completed todos
(etaf-eorm-count db 'todos :where '(= completed 1))

;; Delete completed todos
(etaf-eorm-delete db 'todos
  :where '(= completed 1))

;; Close connection when done
(etaf-eorm-disconnect db)
```

### 4. Working with Multiple Databases (Recommended Approach)

When working with multiple databases, **always specify which tables to migrate** to avoid accidentally creating all tables in all databases:

```elisp
;; Define schemas for different purposes
(etaf-eorm-define-table users
  (id integer :primary-key t :autoincrement t)
  (name text :not-null t))

(etaf-eorm-define-table posts
  (id integer :primary-key t :autoincrement t)
  (title text :not-null t))

(etaf-eorm-define-table logs
  (id integer :primary-key t :autoincrement t)
  (message text :not-null t))

;; Connect to different databases
(setq user-db (etaf-eorm-connect :sqlite "~/users.db"))
(setq content-db (etaf-eorm-connect :postgresql
  :host "localhost" :database "content" :user "admin" :password "secret"))
(setq log-db (etaf-eorm-connect :mysql
  :host "localhost" :database "logs" :user "admin" :password "secret"))

;; Migrate specific tables to specific databases (pure function approach)
(etaf-eorm-migrate user-db 'users)              ; only users in user-db
(etaf-eorm-migrate content-db 'posts)           ; only posts in content-db
(etaf-eorm-migrate log-db 'logs)                ; only logs in log-db

;; Or migrate multiple tables to one database
(etaf-eorm-migrate content-db '(users posts))   ; users and posts in content-db

;; Each database now has only its relevant tables
;; This prevents confusion and accidental data in wrong databases
```

## Examples

Run the comprehensive examples:

```elisp
;; Load examples
(load-file "examples/etaf-eorm-example.el")

;; Run all examples
(etaf-eorm-run-all-examples)

;; Or run individual examples
(etaf-eorm-example-basic)          ;; Basic CRUD
(etaf-eorm-example-query-builder)  ;; Query builder
(etaf-eorm-example-transactions)   ;; Transactions
(etaf-eorm-example-reactive)       ;; Reactive queries
(etaf-eorm-example-ui)             ;; UI integration
```

## Supported Databases

### SQLite

- **Pros**: Built-in Emacs support, no external dependencies, simple file-based storage
- **Cons**: Limited concurrency, no network access
- **Best for**: Local data, configuration storage, cache

```elisp
(setq db (etaf-eorm-connect :sqlite "~/myapp.db"))
```

### PostgreSQL

- **Pros**: Full-featured SQL database, excellent concurrency, ACID compliance
- **Cons**: Requires psql command-line tool, network setup
- **Best for**: Production applications, complex queries, multi-user systems

```elisp
(setq db (etaf-eorm-connect :postgresql
  :host "localhost"
  :port 5432
  :database "mydb"
  :user "myuser"
  :password "mypass"))
```

### MySQL

- **Pros**: Wide adoption, good performance, familiar to many developers
- **Cons**: Requires mysql command-line tool, network setup
- **Best for**: Web applications, existing MySQL infrastructure

```elisp
(setq db (etaf-eorm-connect :mysql
  :host "localhost"
  :port 3306
  :database "mydb"
  :user "myuser"
  :password "mypass"))
```

## Key Features

### Schema Definition
```elisp
(etaf-eorm-define-table users
  (id integer :primary-key t :autoincrement t)
  (name text :not-null t)
  (email text :unique t)
  (age integer)
  (created-at datetime :default current-timestamp))
```

### Query Builder
```elisp
;; Chainable queries
(etaf-eorm-query-get
  (etaf-eorm-query-order-by
    (etaf-eorm-query-where
      (etaf-eorm-query db 'users)
      '(> age 25))
    'name))
```

### Transactions
```elisp
(etaf-eorm-with-transaction db
  (etaf-eorm-insert db 'accounts :name "Alice" :balance 1000)
  (etaf-eorm-update db 'accounts 
    :set '(:balance 900) 
    :where '(= name "Alice")))
```

### Reactive Integration
```elisp
(let ((users-ref (etaf-eorm-reactive-query
                   db 'users
                   (lambda (db table)
                     (etaf-eorm-select db table)))))
  ;; Watch for changes
  (etaf-watch users-ref
    (lambda (new-val old-val)
      (message "Users updated!")))
  
  ;; Any insert/update/delete will trigger the watch
  (etaf-eorm-insert db 'users :name "New User"))
```

## Documentation

Full documentation available at:
- [Complete Guide](docs/ETAF-EORM.md) - User guide and API reference
- [Implementation Details](docs/ETAF-EORM-IMPLEMENTATION.md) - Architecture and design

## Testing

Run the test suite:

```bash
cd tests
emacs -batch -L .. -l etaf-ert.el -l etaf-eorm.el -l etaf-eorm-tests.el -f ert-run-tests-batch-and-exit
```

## Need Help?

- Check the [examples](examples/etaf-eorm-example.el)
- Read the [documentation](docs/ETAF-EORM.md)
- Review the [tests](tests/etaf-eorm-tests.el) for usage patterns

## Inspiration

ETAF-EORM is inspired by [Diesel](https://github.com/diesel-rs/diesel), the Rust ORM library, bringing similar type-safe, composable query building to Emacs Lisp.

Enjoy building database-backed applications in Emacs! 🎉
