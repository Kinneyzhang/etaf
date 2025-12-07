# Multi-Database Support in ETAF-EORM

ETAF-EORM now supports three major database systems: SQLite, PostgreSQL, and MySQL. This document provides detailed information about the multi-database architecture and usage.

## Overview

The multi-database support is implemented through a backend abstraction layer that provides a unified interface across different database systems while handling their specific requirements and syntax differences.

## Architecture

### Backend Abstraction Layer

The architecture consists of:

1. **Connection Structure**: `etaf-eorm-connection` stores connection metadata including type, handle, and parameters
2. **Backend Protocol**: A set of functions that each backend must implement
3. **Backend Implementations**: Database-specific implementations for SQLite, PostgreSQL, and MySQL

### Backend Interface

Each backend must implement the following functions:

- `connect` - Establish database connection
- `disconnect` - Close database connection
- `execute` - Execute SQL statement (INSERT, UPDATE, DELETE, etc.)
- `select` - Execute SELECT query and return results
- `table-exists-p` - Check if table exists
- `last-insert-id` - Get last inserted row ID
- `changes` - Get number of affected rows
- `begin-transaction` - Start transaction
- `commit` - Commit transaction
- `rollback` - Rollback transaction

## Supported Databases

### SQLite

**Connection Method**: Native Emacs SQLite support (requires Emacs 29.1+)

**Features**:
- File-based storage
- ACID compliance
- Zero configuration
- Built-in to Emacs

**Connection Example**:
```elisp
(setq db (etaf-eorm-connect :sqlite "~/myapp.db"))
```

**Advantages**:
- No external dependencies
- Simple setup
- Perfect for local data and configuration
- Fast for single-user applications

**Limitations**:
- Limited concurrency
- No network access
- Single connection limit

**Type Mappings**:
- `integer` → `INTEGER`
- `text` → `TEXT`
- `real` → `REAL`
- `blob` → `BLOB`
- `datetime` → `DATETIME`
- `boolean` → `INTEGER` (0/1)

**Auto-increment**: `AUTOINCREMENT` keyword with `INTEGER PRIMARY KEY`

### PostgreSQL

**Connection Method**: External `psql` command-line tool

**Features**:
- Full-featured relational database
- Excellent concurrency
- ACID compliance
- Advanced data types
- Extensibility

**Connection Example**:
```elisp
(setq db (etaf-eorm-connect :postgresql
  :host "localhost"
  :port 5432
  :database "mydb"
  :user "myuser"
  :password "mypass"))
```

**Advantages**:
- Production-ready
- Excellent performance with large datasets
- Strong consistency guarantees
- Rich feature set

**Limitations**:
- Requires PostgreSQL server and psql client
- More complex setup
- Network configuration needed

**Type Mappings**:
- `integer` → `INTEGER`
- `text` → `TEXT`
- `real` → `REAL`
- `blob` → `BYTEA`
- `datetime` → `TIMESTAMP`
- `boolean` → `BOOLEAN`

**Auto-increment**: `SERIAL` or `BIGSERIAL` type (replaces INTEGER with PRIMARY KEY)

### MySQL

**Connection Method**: External `mysql` command-line tool

**Features**:
- Wide adoption
- Good performance
- ACID compliance (with InnoDB)
- Mature ecosystem

**Connection Example**:
```elisp
(setq db (etaf-eorm-connect :mysql
  :host "localhost"
  :port 3306
  :database "mydb"
  :user "myuser"
  :password "mypass"))
```

**Advantages**:
- Familiar to many developers
- Wide tooling support
- Good documentation
- Industry standard

**Limitations**:
- Requires MySQL server and mysql client
- Network configuration needed
- Some SQL dialect differences

**Type Mappings**:
- `integer` → `INT`
- `text` → `TEXT`
- `real` → `DOUBLE`
- `blob` → `BLOB`
- `datetime` → `DATETIME`
- `boolean` → `BOOLEAN`

**Auto-increment**: `AUTO_INCREMENT` keyword with `INT PRIMARY KEY`

## Database-Specific Features

### Auto-increment Columns

Different databases handle auto-increment differently:

**SQLite**:
```sql
id INTEGER PRIMARY KEY AUTOINCREMENT
```

**PostgreSQL**:
```sql
id SERIAL PRIMARY KEY
```
(Note: SERIAL is a pseudo-type that creates an integer column with a sequence)

**MySQL**:
```sql
id INT PRIMARY KEY AUTO_INCREMENT
```

### Current Timestamp Default

All databases support `CURRENT_TIMESTAMP` for default datetime values:

```elisp
(etaf-eorm-define-table posts
  (created-at datetime :default current-timestamp))
```

Generates:
- SQLite: `created_at DATETIME DEFAULT CURRENT_TIMESTAMP`
- PostgreSQL: `created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP`
- MySQL: `created_at DATETIME DEFAULT CURRENT_TIMESTAMP`

## Usage Examples

### Basic CRUD Operations

All databases use the same API:

```elisp
;; Define schema (once, shared across all databases)
(etaf-eorm-define-table users
  (id integer :primary-key t :autoincrement t)
  (name text :not-null t)
  (email text :unique t))

;; SQLite
(setq sqlite-db (etaf-eorm-connect :sqlite "~/users.db"))
(etaf-eorm-migrate sqlite-db 'users)

;; PostgreSQL
(setq pg-db (etaf-eorm-connect :postgresql
  :host "localhost" :database "mydb" :user "admin" :password "secret"))
(etaf-eorm-migrate pg-db 'users)

;; MySQL
(setq mysql-db (etaf-eorm-connect :mysql
  :host "localhost" :database "mydb" :user "admin" :password "secret"))
(etaf-eorm-migrate mysql-db 'users)

;; Insert works the same on all databases
(etaf-eorm-insert sqlite-db 'users :name "Alice" :email "alice@example.com")
(etaf-eorm-insert pg-db 'users :name "Bob" :email "bob@example.com")
(etaf-eorm-insert mysql-db 'users :name "Charlie" :email "charlie@example.com")

;; Query works the same
(etaf-eorm-select sqlite-db 'users :where '(= name "Alice"))
(etaf-eorm-select pg-db 'users :where '(= name "Bob"))
(etaf-eorm-select mysql-db 'users :where '(= name "Charlie"))
```

### Multi-Database Application

A practical example using different databases for different purposes:

```elisp
(defvar app-config-db nil "Configuration database")
(defvar app-data-db nil "Main data database")
(defvar app-cache-db nil "Cache database")

(defun app-init-databases ()
  "Initialize application databases."
  ;; Local config in SQLite
  (setq app-config-db (etaf-eorm-connect :sqlite "~/.myapp/config.db"))
  
  ;; Main data in PostgreSQL
  (setq app-data-db (etaf-eorm-connect :postgresql
    :host (getenv "DB_HOST")
    :database (getenv "DB_NAME")
    :user (getenv "DB_USER")
    :password (getenv "DB_PASS")))
  
  ;; Fast cache in SQLite
  (setq app-cache-db (etaf-eorm-connect :sqlite "/tmp/myapp-cache.db"))
  
  ;; Migrate tables
  (etaf-eorm-migrate app-config-db 'settings)
  (etaf-eorm-migrate app-data-db '(users posts comments))
  (etaf-eorm-migrate app-cache-db 'cache-entries))

(defun app-cleanup ()
  "Cleanup database connections."
  (etaf-eorm-disconnect app-config-db)
  (etaf-eorm-disconnect app-data-db)
  (etaf-eorm-disconnect app-cache-db))
```

## Backward Compatibility

The library maintains backward compatibility with code written for the SQLite-only version:

```elisp
;; Old API (still works via etaf-eorm-get-connection)
(setq db (etaf-eorm-get-connection "~/myapp.db"))

;; New API (recommended)
(setq db (etaf-eorm-connect :sqlite "~/myapp.db"))
```

## Performance Considerations

### SQLite
- Excellent for read-heavy workloads
- Single writer limitation
- Fast for local data
- Low memory footprint

### PostgreSQL
- Best for complex queries
- Excellent multi-user concurrency
- Good for large datasets
- More memory usage

### MySQL
- Good balanced performance
- Good for web applications
- Wide hosting support
- Moderate resource usage

## Security Considerations

### Connection Security

1. **SQLite**: File permissions are the primary security mechanism
2. **PostgreSQL/MySQL**: Use secure passwords, SSL connections in production

### SQL Injection

The library uses parameterized queries where possible to prevent SQL injection. However:

- WHERE clauses are built from expressions and should not contain user input directly
- Always validate and sanitize user input before using in queries

### Password Storage

Never hardcode passwords in source code. Use environment variables:

```elisp
(setq db (etaf-eorm-connect :postgresql
  :host (getenv "DB_HOST")
  :user (getenv "DB_USER")
  :password (getenv "DB_PASSWORD")
  :database (getenv "DB_NAME")))
```

## Troubleshooting

### PostgreSQL Connection Issues

1. Ensure `psql` is installed: `which psql`
2. Test connection: `psql -h localhost -U myuser -d mydb`
3. Check PostgreSQL server is running
4. Verify firewall settings

### MySQL Connection Issues

1. Ensure `mysql` is installed: `which mysql`
2. Test connection: `mysql -h localhost -u myuser -p mydb`
3. Check MySQL server is running
4. Verify user permissions

### SQLite Issues

1. Check Emacs version: `M-x emacs-version` (needs 29.1+)
2. Verify SQLite support: `(sqlite-available-p)` should return `t`
3. Check file permissions on database directory

## Future Enhancements

Potential future improvements:

1. **Connection pooling** for PostgreSQL and MySQL
2. **Prepared statements** for better performance
3. **Native PostgreSQL library** support (pg.el)
4. **Additional database backends** (MariaDB, Oracle, etc.)
5. **Migration versioning** system
6. **Database introspection** tools

## Contributing

When adding support for new database backends:

1. Implement all required backend functions
2. Add type mappings
3. Handle auto-increment appropriately
4. Add tests
5. Update documentation

See the existing backend implementations in `etaf-eorm.el` for reference.
