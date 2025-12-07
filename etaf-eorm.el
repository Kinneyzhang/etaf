;;; etaf-eorm.el --- ETAF ORM for SQLite database -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: database, orm, sqlite
;; URL: https://github.com/Kinneyzhang/etaf

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ETAF-EORM is an ORM (Object-Relational Mapping) library for Emacs,
;; inspired by Diesel (https://github.com/diesel-rs/diesel).
;; It provides a type-safe, composable query builder interface for SQLite
;; databases, deeply integrated with the ETAF framework.
;;
;; Key Features:
;; - Schema definition with type safety
;; - Composable query builder
;; - CRUD operations
;; - Migration system
;; - Transaction support
;; - Integration with ETAF's reactive system
;;
;; Basic Usage:
;;
;;   ;; Define schemas (stored globally for convenience)
;;   (etaf-eorm-define-table users
;;     (id integer :primary-key t :autoincrement t)
;;     (name text :not-null t)
;;     (email text :unique t)
;;     (age integer)
;;     (created-at datetime :default current-timestamp))
;;
;;   (etaf-eorm-define-table posts
;;     (id integer :primary-key t :autoincrement t)
;;     (user-id integer :not-null t :references (users id))
;;     (title text :not-null t)
;;     (content text))
;;
;;   ;; Create database connections
;;   (setq db1 (etaf-eorm-connect "~/app1.db"))
;;   (setq db2 (etaf-eorm-connect "~/app2.db"))
;;
;;   ;; Migrate specific tables to specific databases (pure function approach)
;;   (etaf-eorm-migrate db1 'users)              ; only users in db1
;;   (etaf-eorm-migrate db2 '(users posts))      ; users and posts in db2
;;
;;   ;; Insert data
;;   (etaf-eorm-insert db1 'users
;;     :name "Alice"
;;     :email "alice@example.com"
;;     :age 30)
;;
;;   ;; Query data
;;   (etaf-eorm-select db1 'users
;;     :where '(> age 25)
;;     :order-by 'name)
;;
;;   ;; Update data
;;   (etaf-eorm-update db1 'users
;;     :set '(:age 31)
;;     :where '(= name "Alice"))
;;
;;   ;; Delete data
;;   (etaf-eorm-delete db1 'users
;;     :where '(= email "alice@example.com"))

;;; Code:

(require 'cl-lib)

;;; Configuration

(defgroup etaf-eorm nil
  "ETAF ORM for SQLite database."
  :group 'etaf
  :prefix "etaf-eorm-")

(defcustom etaf-eorm-default-database nil
  "Default database connection path."
  :type '(choice (const :tag "None" nil)
                 (file :tag "Database file"))
  :group 'etaf-eorm)

(defcustom etaf-eorm-enable-logging nil
  "Enable SQL query logging."
  :type 'boolean
  :group 'etaf-eorm)

(defcustom etaf-eorm-migration-directory "migrations"
  "Directory for migration files."
  :type 'string
  :group 'etaf-eorm)

;;; Internal Variables

(defvar etaf-eorm--schemas (make-hash-table :test 'eq)
  "Hash table storing table schemas.
Key: table name (symbol)
Value: schema definition plist")

(defvar etaf-eorm--connections (make-hash-table :test 'equal)
  "Hash table storing database connections.
Key: database path (string)
Value: SQLite database handle")

(defvar etaf-eorm--migrations (make-hash-table :test 'equal)
  "Hash table storing migration history.
Key: database path (string)
Value: list of applied migration timestamps")

;;; Utility Functions

(defun etaf-eorm--log (format-string &rest args)
  "Log message if logging is enabled.
FORMAT-STRING is the format string and ARGS are the arguments."
  (when etaf-eorm-enable-logging
    (apply #'message (concat "[ETAF-EORM] " format-string) args)))

(defun etaf-eorm--ensure-sqlite ()
  "Ensure SQLite support is available in Emacs."
  (unless (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))
    (error "SQLite support is not available in this Emacs build")))

(defun etaf-eorm--symbol-to-sql (symbol)
  "Convert Emacs Lisp SYMBOL to SQL identifier."
  (replace-regexp-in-string "-" "_" (symbol-name symbol)))

(defun etaf-eorm--sql-to-symbol (sql-name)
  "Convert SQL identifier SQL-NAME to Emacs Lisp symbol."
  (intern (replace-regexp-in-string "_" "-" sql-name)))

(defun etaf-eorm--plist-keys (plist)
  "Get all keys from PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun etaf-eorm--plist-values (plist)
  "Get all values from PLIST."
  (let (values)
    (while plist
      (setq plist (cdr plist))
      (push (car plist) values)
      (setq plist (cdr plist)))
    (nreverse values)))

;;; Type System

(defun etaf-eorm--type-to-sql (type)
  "Convert Emacs Lisp TYPE to SQLite type."
  (pcase type
    ('integer "INTEGER")
    ('text "TEXT")
    ('real "REAL")
    ('blob "BLOB")
    ('datetime "DATETIME")
    ('boolean "INTEGER") ; SQLite uses INTEGER for boolean
    (_ (error "Unknown type: %s" type))))

(defun etaf-eorm--sql-to-type (sql-type)
  "Convert SQLite SQL-TYPE to Emacs Lisp type."
  (pcase (upcase sql-type)
    ("INTEGER" 'integer)
    ("TEXT" 'text)
    ("REAL" 'real)
    ("BLOB" 'blob)
    ("DATETIME" 'datetime)
    (_ 'text)))

(defun etaf-eorm--validate-value (type value)
  "Validate VALUE against TYPE."
  (pcase type
    ('integer (integerp value))
    ('text (stringp value))
    ('real (numberp value))
    ('blob (stringp value))
    ('datetime (stringp value))
    ('boolean (booleanp value))
    (_ t)))

;;; Schema Definition

(defun etaf-eorm-define-table (table-name &rest columns)
  "Define a table schema.

TABLE-NAME is the name of the table (symbol).
COLUMNS is a list of column definitions.

Each column definition has the form:
  (column-name type &rest options)

Where:
  column-name - symbol representing the column name
  type - one of: integer, text, real, blob, datetime, boolean
  options - property list of constraints:
    :primary-key - mark as primary key
    :autoincrement - auto-increment (for INTEGER PRIMARY KEY only)
    :not-null - NOT NULL constraint
    :unique - UNIQUE constraint
    :default - default value
    :references - foreign key reference (table-name column-name)

Example:
  (etaf-eorm-define-table users
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (email text :unique t)
    (age integer)
    (created-at datetime :default current-timestamp))"
  (let ((schema (make-hash-table :test 'eq)))
    (dolist (column columns)
      (let* ((col-name (car column))
             (col-type (cadr column))
             (col-options (cddr column))
             (col-def (list :type col-type)))
        ;; Parse options
        (while col-options
          (let ((key (car col-options))
                (value (cadr col-options)))
            (plist-put col-def key value)
            (setq col-options (cddr col-options))))
        (puthash col-name col-def schema)))
    (puthash table-name schema etaf-eorm--schemas)
    (etaf-eorm--log "Defined table schema: %s" table-name)
    table-name))

(defmacro etaf-eorm-define-table (table-name &rest columns)
  "Define a table schema macro version.
See `etaf-eorm-define-table' for details."
  `(etaf-eorm--define-table-internal ',table-name ',columns))

(defun etaf-eorm--define-table-internal (table-name columns)
  "Internal function to define table schema.
TABLE-NAME is the table name symbol.
COLUMNS is the list of column definitions."
  (let ((schema (make-hash-table :test 'eq)))
    (dolist (column columns)
      (let* ((col-name (car column))
             (col-type (cadr column))
             (col-options (cddr column))
             (col-def (list :type col-type)))
        ;; Parse options
        (while col-options
          (let ((key (car col-options))
                (value (cadr col-options)))
            (setq col-def (plist-put col-def key value))
            (setq col-options (cddr col-options))))
        (puthash col-name col-def schema)))
    (puthash table-name schema etaf-eorm--schemas)
    (etaf-eorm--log "Defined table schema: %s" table-name)
    table-name))

(defun etaf-eorm-get-schema (table-name)
  "Get schema definition for TABLE-NAME."
  (gethash table-name etaf-eorm--schemas))

(defun etaf-eorm--column-definition-sql (column-name column-def)
  "Generate SQL column definition.
COLUMN-NAME is the column name symbol.
COLUMN-DEF is the column definition plist."
  (let* ((col-name (etaf-eorm--symbol-to-sql column-name))
         (col-type (etaf-eorm--type-to-sql (plist-get column-def :type)))
         (constraints '()))
    ;; PRIMARY KEY
    (when (plist-get column-def :primary-key)
      (push "PRIMARY KEY" constraints)
      (when (plist-get column-def :autoincrement)
        (push "AUTOINCREMENT" constraints)))
    ;; NOT NULL
    (when (plist-get column-def :not-null)
      (push "NOT NULL" constraints))
    ;; UNIQUE
    (when (plist-get column-def :unique)
      (push "UNIQUE" constraints))
    ;; DEFAULT
    (when-let ((default (plist-get column-def :default)))
      (push (format "DEFAULT %s"
                    (if (eq default 'current-timestamp)
                        "CURRENT_TIMESTAMP"
                      (etaf-eorm--quote-value default)))
            constraints))
    ;; REFERENCES (foreign key)
    (when-let ((refs (plist-get column-def :references)))
      (push (format "REFERENCES %s(%s)"
                    (etaf-eorm--symbol-to-sql (car refs))
                    (etaf-eorm--symbol-to-sql (cadr refs)))
            constraints))
    (format "%s %s%s"
            col-name
            col-type
            (if constraints
                (concat " " (string-join (nreverse constraints) " "))
              ""))))

(defun etaf-eorm--create-table-sql (table-name)
  "Generate CREATE TABLE SQL for TABLE-NAME."
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (columns '()))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    (maphash (lambda (col-name col-def)
               (push (etaf-eorm--column-definition-sql col-name col-def)
                     columns))
             schema)
    (format "CREATE TABLE IF NOT EXISTS %s (%s)"
            sql-table-name
            (string-join (nreverse columns) ", "))))

;;; Connection Management

(defun etaf-eorm-connect (db-path)
  "Connect to SQLite database at DB-PATH.
Returns a database handle."
  (etaf-eorm--ensure-sqlite)
  (let ((db (sqlite-open db-path)))
    (puthash db-path db etaf-eorm--connections)
    (etaf-eorm--log "Connected to database: %s" db-path)
    db))

(defun etaf-eorm-disconnect (db)
  "Disconnect from database DB."
  (when (sqlite-available-p)
    (sqlite-close db)
    (maphash (lambda (path handle)
               (when (eq handle db)
                 (remhash path etaf-eorm--connections)
                 (etaf-eorm--log "Disconnected from database: %s" path)))
             etaf-eorm--connections)))

(defun etaf-eorm-get-connection (db-path)
  "Get existing connection for DB-PATH or create new one."
  (or (gethash db-path etaf-eorm--connections)
      (etaf-eorm-connect db-path)))

;;; Migration System

(defun etaf-eorm-create-table (db table-name)
  "Create table TABLE-NAME in database DB based on defined schema."
  (let ((sql (etaf-eorm--create-table-sql table-name)))
    (etaf-eorm--log "Creating table: %s" sql)
    (sqlite-execute db sql)))

(defun etaf-eorm-drop-table (db table-name)
  "Drop table TABLE-NAME from database DB."
  (let ((sql (format "DROP TABLE IF EXISTS %s"
                     (etaf-eorm--symbol-to-sql table-name))))
    (etaf-eorm--log "Dropping table: %s" sql)
    (sqlite-execute db sql)))

(defun etaf-eorm-table-exists-p (db table-name)
  "Check if table TABLE-NAME exists in database DB."
  (let* ((sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (result (sqlite-select
                  db
                  "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
                  (list sql-table-name))))
    (not (null result))))

(defun etaf-eorm-migrate (db &optional table-names)
  "Run migrations for database DB.
Creates tables specified in TABLE-NAMES that don't exist yet.
If TABLE-NAMES is nil, creates all defined tables (legacy behavior).
TABLE-NAMES can be:
  - nil: migrate all tables in etaf-eorm--schemas (global)
  - a single table name symbol: migrate that table
  - a list of table name symbols: migrate those tables

Example:
  (etaf-eorm-migrate db)                    ; migrate all tables
  (etaf-eorm-migrate db 'users)             ; migrate users table
  (etaf-eorm-migrate db '(users posts))     ; migrate users and posts"
  (let ((tables-to-migrate
         (cond
          ;; No table names specified - use all defined schemas (legacy)
          ((null table-names)
           (let (tables)
             (maphash (lambda (table-name _schema)
                        (push table-name tables))
                      etaf-eorm--schemas)
             tables))
          ;; Single table name
          ((symbolp table-names)
           (list table-names))
          ;; List of table names
          ((listp table-names)
           table-names)
          (t
           (error "TABLE-NAMES must be nil, a symbol, or a list of symbols")))))
    (dolist (table-name tables-to-migrate)
      (unless (etaf-eorm-get-schema table-name)
        (error "Table schema not defined: %s" table-name))
      (unless (etaf-eorm-table-exists-p db table-name)
        (etaf-eorm-create-table db table-name)
        (etaf-eorm--log "Created table: %s" table-name)))))

;;; Query Building - Value Formatting

(defun etaf-eorm--quote-value (value)
  "Quote VALUE for SQL query."
  (cond
   ((null value) "NULL")
   ((eq value t) "1")
   ((eq value nil) "0")
   ((numberp value) (number-to-string value))
   ((stringp value) (format "'%s'" (replace-regexp-in-string "'" "''" value)))
   ((symbolp value) (format "'%s'" (symbol-name value)))
   (t (format "'%s'" (prin1-to-string value)))))

(defun etaf-eorm--build-where-clause (where-expr)
  "Build WHERE clause from WHERE-EXPR.
WHERE-EXPR can be:
  - A list like (= column value)
  - A list like (and (= col1 val1) (> col2 val2))
  - nil for no WHERE clause"
  (if (null where-expr)
      ""
    (let ((sql (etaf-eorm--where-to-sql where-expr)))
      (concat " WHERE " sql))))

(defun etaf-eorm--where-to-sql (expr)
  "Convert WHERE expression EXPR to SQL."
  (cond
   ((null expr) "1=1")
   ((not (listp expr)) (etaf-eorm--quote-value expr))
   (t
    (let ((op (car expr)))
      (pcase op
        ('and (string-join
               (mapcar #'etaf-eorm--where-to-sql (cdr expr))
               " AND "))
        ('or (string-join
              (mapcar #'etaf-eorm--where-to-sql (cdr expr))
              " OR "))
        ('not (format "NOT (%s)"
                      (etaf-eorm--where-to-sql (cadr expr))))
        ('= (format "%s = %s"
                    (etaf-eorm--symbol-to-sql (cadr expr))
                    (etaf-eorm--quote-value (caddr expr))))
        ('!= (format "%s != %s"
                     (etaf-eorm--symbol-to-sql (cadr expr))
                     (etaf-eorm--quote-value (caddr expr))))
        ('> (format "%s > %s"
                    (etaf-eorm--symbol-to-sql (cadr expr))
                    (etaf-eorm--quote-value (caddr expr))))
        ('< (format "%s < %s"
                    (etaf-eorm--symbol-to-sql (cadr expr))
                    (etaf-eorm--quote-value (caddr expr))))
        ('>= (format "%s >= %s"
                     (etaf-eorm--symbol-to-sql (cadr expr))
                     (etaf-eorm--quote-value (caddr expr))))
        ('<= (format "%s <= %s"
                     (etaf-eorm--symbol-to-sql (cadr expr))
                     (etaf-eorm--quote-value (caddr expr))))
        ('like (format "%s LIKE %s"
                       (etaf-eorm--symbol-to-sql (cadr expr))
                       (etaf-eorm--quote-value (caddr expr))))
        ('in (format "%s IN (%s)"
                     (etaf-eorm--symbol-to-sql (cadr expr))
                     (string-join
                      (mapcar #'etaf-eorm--quote-value (caddr expr))
                      ", ")))
        ('is-null (format "%s IS NULL"
                          (etaf-eorm--symbol-to-sql (cadr expr))))
        ('is-not-null (format "%s IS NOT NULL"
                              (etaf-eorm--symbol-to-sql (cadr expr))))
        (_ (error "Unknown operator in WHERE clause: %s" op)))))))

;;; CRUD Operations

(defun etaf-eorm-insert (db table-name &rest values)
  "Insert a row into TABLE-NAME in database DB.
VALUES is a plist of column names and values.

Example:
  (etaf-eorm-insert db 'users
    :name \"Alice\"
    :email \"alice@example.com\"
    :age 30)"
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (columns '())
         (placeholders '())
         (params '()))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    ;; Build column list and values
    (let ((plist values))
      (while plist
        (let* ((key (car plist))
               (value (cadr plist))
               (col-name (intern (substring (symbol-name key) 1))))
          (unless (gethash col-name schema)
            (error "Unknown column: %s" col-name))
          (push (etaf-eorm--symbol-to-sql col-name) columns)
          (push "?" placeholders)
          (push value params)
          (setq plist (cddr plist)))))
    (let ((sql (format "INSERT INTO %s (%s) VALUES (%s)"
                       sql-table-name
                       (string-join (nreverse columns) ", ")
                       (string-join (nreverse placeholders) ", "))))
      (etaf-eorm--log "INSERT: %s with params: %S" sql params)
      (sqlite-execute db sql (nreverse params))
      ;; Return the last inserted row id
      (caar (sqlite-select db "SELECT last_insert_rowid()")))))

(defun etaf-eorm-select (db table-name &rest args)
  "Select rows from TABLE-NAME in database DB.

ARGS is a plist with the following optional keys:
  :columns - list of columns to select (default: all)
  :where - WHERE clause expression
  :order-by - column name or list of (column direction) pairs
  :limit - maximum number of rows
  :offset - number of rows to skip

Returns a list of rows, where each row is a plist.

Example:
  (etaf-eorm-select db 'users
    :columns '(name email)
    :where '(> age 25)
    :order-by 'name
    :limit 10)"
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (columns (plist-get args :columns))
         (where (plist-get args :where))
         (order-by (plist-get args :order-by))
         (limit (plist-get args :limit))
         (offset (plist-get args :offset)))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    ;; Build column list
    (let* ((col-list (if columns
                         (mapcar #'etaf-eorm--symbol-to-sql columns)
                       '("*")))
           (sql (concat
                 (format "SELECT %s FROM %s"
                         (string-join col-list ", ")
                         sql-table-name)
                 (etaf-eorm--build-where-clause where)
                 (if order-by
                     (concat " ORDER BY "
                             (if (listp order-by)
                                 (string-join
                                  (mapcar (lambda (x)
                                            (if (listp x)
                                                (format "%s %s"
                                                        (etaf-eorm--symbol-to-sql (car x))
                                                        (upcase (symbol-name (cadr x))))
                                              (etaf-eorm--symbol-to-sql x)))
                                          order-by)
                                  ", ")
                               (etaf-eorm--symbol-to-sql order-by)))
                   "")
                 (if limit (format " LIMIT %d" limit) "")
                 (if offset (format " OFFSET %d" offset) ""))))
      (etaf-eorm--log "SELECT: %s" sql)
      (let ((results (sqlite-select db sql)))
        ;; Convert results to plists
        (when results
          (let* ((all-columns (or columns
                                  (let (cols)
                                    (maphash (lambda (k _v) (push k cols)) schema)
                                    (nreverse cols))))
                 (col-keywords (mapcar (lambda (c)
                                         (intern (concat ":" (symbol-name c))))
                                       all-columns)))
            (mapcar (lambda (row)
                      (let (result)
                        (cl-loop for col in col-keywords
                                 for val in row
                                 do (setq result (plist-put result col val)))
                        result))
                    results)))))))

(defun etaf-eorm-update (db table-name &rest args)
  "Update rows in TABLE-NAME in database DB.

ARGS is a plist with the following keys:
  :set - plist of columns and new values
  :where - WHERE clause expression (required to prevent accidental mass updates)

Returns the number of affected rows.

Example:
  (etaf-eorm-update db 'users
    :set '(:age 31 :email \"newemail@example.com\")
    :where '(= name \"Alice\"))"
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (set (plist-get args :set))
         (where (plist-get args :where)))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    (unless set
      (error "UPDATE requires :set parameter"))
    (unless where
      (error "UPDATE requires :where parameter to prevent accidental mass updates"))
    ;; Build SET clause
    (let* ((set-clauses '())
           (set-plist set))
      (while set-plist
        (let* ((key (car set-plist))
               (value (cadr set-plist))
               (col-name (intern (substring (symbol-name key) 1))))
          (unless (gethash col-name schema)
            (error "Unknown column: %s" col-name))
          (push (format "%s = %s"
                        (etaf-eorm--symbol-to-sql col-name)
                        (etaf-eorm--quote-value value))
                set-clauses)
          (setq set-plist (cddr set-plist))))
      (let ((sql (concat
                  (format "UPDATE %s SET %s"
                          sql-table-name
                          (string-join (nreverse set-clauses) ", "))
                  (etaf-eorm--build-where-clause where))))
        (etaf-eorm--log "UPDATE: %s" sql)
        (sqlite-execute db sql)
        (caar (sqlite-select db "SELECT changes()"))))))

(defun etaf-eorm-delete (db table-name &rest args)
  "Delete rows from TABLE-NAME in database DB.

ARGS is a plist with the following keys:
  :where - WHERE clause expression (required to prevent accidental mass deletion)

Returns the number of deleted rows.

Example:
  (etaf-eorm-delete db 'users
    :where '(= email \"alice@example.com\"))"
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (where (plist-get args :where)))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    (unless where
      (error "DELETE requires :where parameter to prevent accidental mass deletion"))
    (let ((sql (concat
                (format "DELETE FROM %s" sql-table-name)
                (etaf-eorm--build-where-clause where))))
      (etaf-eorm--log "DELETE: %s" sql)
      (sqlite-execute db sql)
      (caar (sqlite-select db "SELECT changes()")))))

;;; Transaction Support

(defun etaf-eorm-transaction (db func)
  "Execute FUNC within a transaction on database DB.
If FUNC completes successfully, commit the transaction.
If FUNC signals an error, rollback the transaction."
  (sqlite-transaction db)
  (condition-case err
      (prog1
          (funcall func)
        (sqlite-commit db))
    (error
     (sqlite-rollback db)
     (signal (car err) (cdr err)))))

(defmacro etaf-eorm-with-transaction (db &rest body)
  "Execute BODY within a transaction on database DB.
If BODY completes successfully, commit the transaction.
If BODY signals an error, rollback the transaction."
  (declare (indent 1))
  `(etaf-eorm-transaction ,db (lambda () ,@body)))

;;; Query Builder Interface (Chainable)

(cl-defstruct etaf-eorm-query
  "Query builder structure."
  db
  table
  (columns nil)
  (where-clause nil)
  (order-by-clause nil)
  (limit-value nil)
  (offset-value nil))

(defun etaf-eorm-query (db table-name)
  "Create a new query builder for TABLE-NAME on database DB."
  (make-etaf-eorm-query :db db :table table-name))

(defun etaf-eorm-query-select (query &rest columns)
  "Add SELECT columns to QUERY."
  (setf (etaf-eorm-query-columns query) columns)
  query)

(defun etaf-eorm-query-where (query where-expr)
  "Add WHERE clause to QUERY."
  (setf (etaf-eorm-query-where-clause query) where-expr)
  query)

(defun etaf-eorm-query-order-by (query &rest order-spec)
  "Add ORDER BY clause to QUERY."
  (setf (etaf-eorm-query-order-by-clause query) order-spec)
  query)

(defun etaf-eorm-query-limit (query n)
  "Add LIMIT clause to QUERY."
  (setf (etaf-eorm-query-limit-value query) n)
  query)

(defun etaf-eorm-query-offset (query n)
  "Add OFFSET clause to QUERY."
  (setf (etaf-eorm-query-offset-value query) n)
  query)

(defun etaf-eorm-query-get (query)
  "Execute QUERY and return results."
  (apply #'etaf-eorm-select
         (etaf-eorm-query-db query)
         (etaf-eorm-query-table query)
         (append
          (when (etaf-eorm-query-columns query)
            (list :columns (etaf-eorm-query-columns query)))
          (when (etaf-eorm-query-where-clause query)
            (list :where (etaf-eorm-query-where-clause query)))
          (when (etaf-eorm-query-order-by-clause query)
            (list :order-by (etaf-eorm-query-order-by-clause query)))
          (when (etaf-eorm-query-limit-value query)
            (list :limit (etaf-eorm-query-limit-value query)))
          (when (etaf-eorm-query-offset-value query)
            (list :offset (etaf-eorm-query-offset-value query))))))

(defun etaf-eorm-query-first (query)
  "Execute QUERY and return first result."
  (car (etaf-eorm-query-get (etaf-eorm-query-limit query 1))))

;;; Integration with ETAF Reactive System

(defvar etaf-eorm--reactive-queries (make-hash-table :test 'equal)
  "Hash table storing reactive query watchers.")

(defun etaf-eorm-reactive-query (db table-name query-fn)
  "Create a reactive query that automatically updates when data changes.
DB is the database connection.
TABLE-NAME is the table to watch.
QUERY-FN is a function that takes DB and TABLE-NAME and returns query results.

Returns a ref that contains the query results and updates automatically."
  (when (require 'etaf-component nil t)
    (let* ((query-key (format "%s:%s" db table-name))
           (results (funcall query-fn db table-name))
           (ref (etaf-ref results)))
      ;; Store the reactive query
      (puthash query-key (list :ref ref :query-fn query-fn)
               etaf-eorm--reactive-queries)
      ref)))

(defun etaf-eorm--trigger-reactive-update (db table-name)
  "Trigger reactive query updates for TABLE-NAME in database DB."
  (when (require 'etaf-component nil t)
    (let ((query-key (format "%s:%s" db table-name)))
      (when-let ((reactive-query (gethash query-key etaf-eorm--reactive-queries)))
        (let ((ref (plist-get reactive-query :ref))
              (query-fn (plist-get reactive-query :query-fn)))
          (etaf-ref-set ref (funcall query-fn db table-name)))))))

;; Hook into insert/update/delete to trigger reactive updates
(advice-add 'etaf-eorm-insert :after
            (lambda (db table-name &rest _)
              (etaf-eorm--trigger-reactive-update db table-name)))

(advice-add 'etaf-eorm-update :after
            (lambda (db table-name &rest _)
              (etaf-eorm--trigger-reactive-update db table-name)))

(advice-add 'etaf-eorm-delete :after
            (lambda (db table-name &rest _)
              (etaf-eorm--trigger-reactive-update db table-name)))

;;; Utility Functions

(defun etaf-eorm-count (db table-name &rest args)
  "Count rows in TABLE-NAME matching ARGS.
ARGS can include :where clause."
  (let* ((where (plist-get args :where))
         (sql (concat
               (format "SELECT COUNT(*) FROM %s"
                       (etaf-eorm--symbol-to-sql table-name))
               (etaf-eorm--build-where-clause where))))
    (etaf-eorm--log "COUNT: %s" sql)
    (caar (sqlite-select db sql))))

(defun etaf-eorm-exists-p (db table-name &rest args)
  "Check if any rows exist in TABLE-NAME matching ARGS.
ARGS can include :where clause."
  (> (apply #'etaf-eorm-count db table-name args) 0))

(defun etaf-eorm-find-by-id (db table-name id &optional id-column)
  "Find a single row by ID in TABLE-NAME.
ID-COLUMN defaults to 'id."
  (let ((id-col (or id-column 'id)))
    (etaf-eorm-query-first
     (etaf-eorm-query-where
      (etaf-eorm-query db table-name)
      (list '= id-col id)))))

;;; Provide

(provide 'etaf-eorm)

;;; etaf-eorm.el ends here
