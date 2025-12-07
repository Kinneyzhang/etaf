;;; etaf-eorm.el --- ETAF ORM for multiple databases -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: database, orm, sqlite, postgresql, mysql
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
;; It provides a type-safe, composable query builder interface for
;; SQLite, PostgreSQL, and MySQL databases, deeply integrated with the
;; ETAF framework.
;;
;; Key Features:
;; - Schema definition with type safety
;; - Composable query builder
;; - CRUD operations
;; - Migration system
;; - Transaction support
;; - Integration with ETAF's reactive system
;; - Support for SQLite, PostgreSQL, and MySQL
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
;;   ;; SQLite
;;   (setq db1 (etaf-eorm-connect :sqlite "~/app1.db"))
;;   
;;   ;; PostgreSQL
;;   (setq db2 (etaf-eorm-connect :postgresql
;;     :host "localhost" :port 5432
;;     :database "mydb" :user "myuser" :password "mypass"))
;;   
;;   ;; MySQL
;;   (setq db3 (etaf-eorm-connect :mysql
;;     :host "localhost" :port 3306
;;     :database "mydb" :user "myuser" :password "mypass"))
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
  "ETAF ORM for multiple databases (SQLite, PostgreSQL, MySQL)."
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
Key: connection identifier (string)
Value: database connection object")

(defvar etaf-eorm--migrations (make-hash-table :test 'equal)
  "Hash table storing migration history.
Key: database identifier (string)
Value: list of applied migration timestamps")

;;; Database Connection Structure

(cl-defstruct etaf-eorm-connection
  "Database connection structure."
  type          ; Database type: sqlite, postgresql, or mysql
  handle        ; Database-specific handle (sqlite db, process, etc.)
  params        ; Connection parameters (plist)
  id)           ; Unique connection identifier

;;; Database Backend Protocol

(defun etaf-eorm--get-backend (db-type)
  "Get the backend implementation for DB-TYPE."
  (pcase db-type
    ('sqlite 'etaf-eorm-sqlite-backend)
    ('postgresql 'etaf-eorm-postgresql-backend)
    ('mysql 'etaf-eorm-mysql-backend)
    (_ (error "Unknown database type: %s" db-type))))

(defun etaf-eorm--backend-call (conn method &rest args)
  "Call METHOD on connection CONN with ARGS.
Dispatches to the appropriate backend based on connection type."
  (let* ((backend (etaf-eorm--get-backend (etaf-eorm-connection-type conn)))
         (func (intern (format "%s-%s" backend method))))
    (if (fboundp func)
        (apply func conn args)
      (error "Backend method not found: %s" func))))

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

(defun etaf-eorm--type-to-sql (type db-type)
  "Convert Emacs Lisp TYPE to SQL type for DB-TYPE."
  (pcase db-type
    ('sqlite
     (pcase type
       ('integer "INTEGER")
       ('text "TEXT")
       ('real "REAL")
       ('blob "BLOB")
       ('datetime "DATETIME")
       ('boolean "INTEGER") ; SQLite uses INTEGER for boolean
       (_ (error "Unknown type: %s" type))))
    ('postgresql
     (pcase type
       ('integer "INTEGER")
       ('text "TEXT")
       ('real "REAL")
       ('blob "BYTEA")
       ('datetime "TIMESTAMP")
       ('boolean "BOOLEAN")
       (_ (error "Unknown type: %s" type))))
    ('mysql
     (pcase type
       ('integer "INT")
       ('text "TEXT")
       ('real "DOUBLE")
       ('blob "BLOB")
       ('datetime "DATETIME")
       ('boolean "BOOLEAN")
       (_ (error "Unknown type: %s" type))))
    (_ (error "Unknown database type: %s" db-type))))

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

(defun etaf-eorm--column-definition-sql (column-name column-def db-type)
  "Generate SQL column definition for DB-TYPE.
COLUMN-NAME is the column name symbol.
COLUMN-DEF is the column definition plist."
  (let* ((col-name (etaf-eorm--symbol-to-sql column-name))
         (col-type (etaf-eorm--type-to-sql (plist-get column-def :type) db-type))
         (constraints '()))
    ;; PRIMARY KEY and AUTOINCREMENT (database-specific)
    (when (plist-get column-def :primary-key)
      (push "PRIMARY KEY" constraints)
      (when (plist-get column-def :autoincrement)
        (pcase db-type
          ('sqlite (push "AUTOINCREMENT" constraints))
          ('postgresql
           ;; PostgreSQL uses SERIAL or BIGSERIAL for autoincrement
           (setq col-type (if (equal col-type "INTEGER") "SERIAL" "BIGSERIAL")))
          ('mysql
           ;; MySQL uses AUTO_INCREMENT
           (push "AUTO_INCREMENT" constraints)))))
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
                        (pcase db-type
                          ('sqlite "CURRENT_TIMESTAMP")
                          ('postgresql "CURRENT_TIMESTAMP")
                          ('mysql "CURRENT_TIMESTAMP"))
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

(defun etaf-eorm--create-table-sql (table-name db-type)
  "Generate CREATE TABLE SQL for TABLE-NAME on DB-TYPE."
  (let* ((schema (etaf-eorm-get-schema table-name))
         (sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (columns '()))
    (unless schema
      (error "Table schema not defined: %s" table-name))
    (maphash (lambda (col-name col-def)
               (push (etaf-eorm--column-definition-sql col-name col-def db-type)
                     columns))
             schema)
    (format "CREATE TABLE IF NOT EXISTS %s (%s)"
            sql-table-name
            (string-join (nreverse columns) ", "))))

;;; Connection Management

(defun etaf-eorm-connect (db-type &rest params)
  "Connect to database of DB-TYPE with PARAMS.

For SQLite:
  (etaf-eorm-connect :sqlite PATH)
  PATH - path to SQLite database file

For PostgreSQL:
  (etaf-eorm-connect :postgresql
    :host HOST :port PORT :database DB
    :user USER :password PASSWORD)

For MySQL:
  (etaf-eorm-connect :mysql
    :host HOST :port PORT :database DB
    :user USER :password PASSWORD)

Returns a database connection object."
  (let* ((db-type-sym (if (keywordp db-type)
                          (intern (substring (symbol-name db-type) 1))
                        db-type))
         (conn-id (format "%s-%s" db-type-sym (or (plist-get params :database)
                                                    (car params)
                                                    (format "%s" (random)))))
         (conn (etaf-eorm--backend-call
                (make-etaf-eorm-connection :type db-type-sym
                                           :params params
                                           :id conn-id)
                'connect)))
    (puthash conn-id conn etaf-eorm--connections)
    (etaf-eorm--log "Connected to %s database: %s" db-type-sym conn-id)
    conn))

(defun etaf-eorm-disconnect (conn)
  "Disconnect from database CONN."
  (when conn
    (etaf-eorm--backend-call conn 'disconnect)
    (let ((conn-id (etaf-eorm-connection-id conn)))
      (remhash conn-id etaf-eorm--connections)
      (etaf-eorm--log "Disconnected from database: %s" conn-id))))

;;; Database Backend Implementations

;;; SQLite Backend

(defun etaf-eorm--ensure-sqlite ()
  "Ensure SQLite support is available in Emacs."
  (unless (and (fboundp 'sqlite-available-p)
               (sqlite-available-p))
    (error "SQLite support is not available in this Emacs build")))

(defun etaf-eorm-sqlite-backend-connect (conn)
  "Connect to SQLite database using CONN parameters."
  (etaf-eorm--ensure-sqlite)
  (let* ((params (etaf-eorm-connection-params conn))
         (db-path (or (car params) (plist-get params :path)))
         (db (sqlite-open db-path)))
    (setf (etaf-eorm-connection-handle conn) db)
    conn))

(defun etaf-eorm-sqlite-backend-disconnect (conn)
  "Disconnect from SQLite database CONN."
  (when-let ((db (etaf-eorm-connection-handle conn)))
    (sqlite-close db)))

(defun etaf-eorm-sqlite-backend-execute (conn sql &optional params)
  "Execute SQL on SQLite connection CONN with PARAMS."
  (let ((db (etaf-eorm-connection-handle conn)))
    (if params
        (sqlite-execute db sql params)
      (sqlite-execute db sql))))

(defun etaf-eorm-sqlite-backend-select (conn sql &optional params)
  "Execute SELECT SQL on SQLite connection CONN with PARAMS."
  (let ((db (etaf-eorm-connection-handle conn)))
    (if params
        (sqlite-select db sql params)
      (sqlite-select db sql))))

(defun etaf-eorm-sqlite-backend-table-exists-p (conn table-name)
  "Check if TABLE-NAME exists in SQLite connection CONN."
  (let* ((sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (result (etaf-eorm-sqlite-backend-select
                  conn
                  "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
                  (list sql-table-name))))
    (not (null result))))

(defun etaf-eorm-sqlite-backend-last-insert-id (conn)
  "Get last insert ID from SQLite connection CONN."
  (caar (etaf-eorm-sqlite-backend-select conn "SELECT last_insert_rowid()")))

(defun etaf-eorm-sqlite-backend-changes (conn)
  "Get number of changed rows from SQLite connection CONN."
  (caar (etaf-eorm-sqlite-backend-select conn "SELECT changes()")))

(defun etaf-eorm-sqlite-backend-begin-transaction (conn)
  "Begin transaction on SQLite connection CONN."
  (sqlite-transaction (etaf-eorm-connection-handle conn)))

(defun etaf-eorm-sqlite-backend-commit (conn)
  "Commit transaction on SQLite connection CONN."
  (sqlite-commit (etaf-eorm-connection-handle conn)))

(defun etaf-eorm-sqlite-backend-rollback (conn)
  "Rollback transaction on SQLite connection CONN."
  (sqlite-rollback (etaf-eorm-connection-handle conn)))

;;; PostgreSQL Backend

(defun etaf-eorm-postgresql-backend-connect (conn)
  "Connect to PostgreSQL database using CONN parameters.

WARNING: This implementation passes the password via PGPASSWORD environment
variable in the command line, which may be visible in process listings.
For production use, consider:
  - Using .pgpass file for password storage
  - Using peer or ident authentication
  - Using certificate-based authentication
  - Setting up a secure connection tunnel"
  (let* ((params (etaf-eorm-connection-params conn))
         (host (or (plist-get params :host) "localhost"))
         (port (or (plist-get params :port) 5432))
         (database (plist-get params :database))
         (user (plist-get params :user))
         (password (plist-get params :password))
         (process-name (format "etaf-eorm-pg-%s" (etaf-eorm-connection-id conn)))
         (cmd (format "PGPASSWORD='%s' psql -h %s -p %d -U %s -d %s -t -A -F '|'"
                      password host port user database))
         (proc (start-process-shell-command process-name nil cmd)))
    (setf (etaf-eorm-connection-handle conn) proc)
    (set-process-query-on-exit-flag proc nil)
    conn))

(defun etaf-eorm-postgresql-backend-disconnect (conn)
  "Disconnect from PostgreSQL database CONN."
  (when-let ((proc (etaf-eorm-connection-handle conn)))
    (when (process-live-p proc)
      (process-send-string proc "\\q\n")
      (delete-process proc))))

(defun etaf-eorm-postgresql-backend-execute (conn sql &optional params)
  "Execute SQL on PostgreSQL connection CONN with PARAMS."
  (let* ((proc (etaf-eorm-connection-handle conn))
         (sql-with-params (etaf-eorm--substitute-params sql params)))
    (process-send-string proc (format "%s;\n" sql-with-params))
    (accept-process-output proc 1)
    t))

(defun etaf-eorm-postgresql-backend-select (conn sql &optional params)
  "Execute SELECT SQL on PostgreSQL connection CONN with PARAMS."
  (let* ((proc (etaf-eorm-connection-handle conn))
         (sql-with-params (etaf-eorm--substitute-params sql params))
         output)
    (process-send-string proc (format "%s;\n" sql-with-params))
    (accept-process-output proc 2)
    (with-current-buffer (process-buffer proc)
      (setq output (buffer-substring-no-properties (point-min) (point-max))))
    (etaf-eorm--parse-psql-output output)))

(defun etaf-eorm-postgresql-backend-table-exists-p (conn table-name)
  "Check if TABLE-NAME exists in PostgreSQL connection CONN."
  (let* ((sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (result (etaf-eorm-postgresql-backend-select
                  conn
                  (format "SELECT tablename FROM pg_tables WHERE tablename='%s'"
                          sql-table-name))))
    (not (null result))))

(defun etaf-eorm-postgresql-backend-last-insert-id (conn)
  "Get last insert ID from PostgreSQL connection CONN."
  ;; PostgreSQL requires explicit RETURNING clause in INSERT
  ;; This is handled at the query level
  nil)

(defun etaf-eorm-postgresql-backend-changes (conn)
  "Get number of changed rows from PostgreSQL connection CONN."
  ;; This needs to be tracked at the query level
  nil)

(defun etaf-eorm-postgresql-backend-begin-transaction (conn)
  "Begin transaction on PostgreSQL connection CONN."
  (etaf-eorm-postgresql-backend-execute conn "BEGIN"))

(defun etaf-eorm-postgresql-backend-commit (conn)
  "Commit transaction on PostgreSQL connection CONN."
  (etaf-eorm-postgresql-backend-execute conn "COMMIT"))

(defun etaf-eorm-postgresql-backend-rollback (conn)
  "Rollback transaction on PostgreSQL connection CONN."
  (etaf-eorm-postgresql-backend-execute conn "ROLLBACK"))

;;; MySQL Backend

(defun etaf-eorm-mysql-backend-connect (conn)
  "Connect to MySQL database using CONN parameters.

WARNING: This implementation passes the password via command-line argument
(-p flag), which may be visible in process listings.
For production use, consider:
  - Using ~/.my.cnf configuration file for credentials
  - Using mysql_config_editor for encrypted credential storage
  - Setting up socket-based authentication
  - Using certificate-based authentication"
  (let* ((params (etaf-eorm-connection-params conn))
         (host (or (plist-get params :host) "localhost"))
         (port (or (plist-get params :port) 3306))
         (database (plist-get params :database))
         (user (plist-get params :user))
         (password (plist-get params :password))
         (process-name (format "etaf-eorm-mysql-%s" (etaf-eorm-connection-id conn)))
         (cmd (format "mysql -h %s -P %d -u %s -p%s -D %s -s -N --default-character-set=utf8"
                      host port user password database))
         (proc (start-process-shell-command process-name nil cmd)))
    (setf (etaf-eorm-connection-handle conn) proc)
    (set-process-query-on-exit-flag proc nil)
    conn))

(defun etaf-eorm-mysql-backend-disconnect (conn)
  "Disconnect from MySQL database CONN."
  (when-let ((proc (etaf-eorm-connection-handle conn)))
    (when (process-live-p proc)
      (process-send-string proc "EXIT;\n")
      (delete-process proc))))

(defun etaf-eorm-mysql-backend-execute (conn sql &optional params)
  "Execute SQL on MySQL connection CONN with PARAMS."
  (let* ((proc (etaf-eorm-connection-handle conn))
         (sql-with-params (etaf-eorm--substitute-params sql params)))
    (process-send-string proc (format "%s;\n" sql-with-params))
    (accept-process-output proc 1)
    t))

(defun etaf-eorm-mysql-backend-select (conn sql &optional params)
  "Execute SELECT SQL on MySQL connection CONN with PARAMS."
  (let* ((proc (etaf-eorm-connection-handle conn))
         (sql-with-params (etaf-eorm--substitute-params sql params))
         output)
    (process-send-string proc (format "%s;\n" sql-with-params))
    (accept-process-output proc 2)
    (with-current-buffer (process-buffer proc)
      (setq output (buffer-substring-no-properties (point-min) (point-max))))
    (etaf-eorm--parse-mysql-output output)))

(defun etaf-eorm-mysql-backend-table-exists-p (conn table-name)
  "Check if TABLE-NAME exists in MySQL connection CONN."
  (let* ((sql-table-name (etaf-eorm--symbol-to-sql table-name))
         (result (etaf-eorm-mysql-backend-select
                  conn
                  (format "SHOW TABLES LIKE '%s'" sql-table-name))))
    (not (null result))))

(defun etaf-eorm-mysql-backend-last-insert-id (conn)
  "Get last insert ID from MySQL connection CONN."
  (caar (etaf-eorm-mysql-backend-select conn "SELECT LAST_INSERT_ID()")))

(defun etaf-eorm-mysql-backend-changes (conn)
  "Get number of changed rows from MySQL connection CONN."
  (caar (etaf-eorm-mysql-backend-select conn "SELECT ROW_COUNT()")))

(defun etaf-eorm-mysql-backend-begin-transaction (conn)
  "Begin transaction on MySQL connection CONN."
  (etaf-eorm-mysql-backend-execute conn "START TRANSACTION"))

(defun etaf-eorm-mysql-backend-commit (conn)
  "Commit transaction on MySQL connection CONN."
  (etaf-eorm-mysql-backend-execute conn "COMMIT"))

(defun etaf-eorm-mysql-backend-rollback (conn)
  "Rollback transaction on MySQL connection CONN."
  (etaf-eorm-mysql-backend-execute conn "ROLLBACK"))

;;; Helper Functions for Backends

(defun etaf-eorm--substitute-params (sql params)
  "Substitute PARAMS in SQL placeholders."
  (if (null params)
      sql
    (let ((result sql))
      (dolist (param params)
        (setq result (replace-regexp-in-string
                      "\\?" (etaf-eorm--quote-value param) result nil nil 1)))
      result)))

(defun etaf-eorm--parse-psql-output (output)
  "Parse PostgreSQL psql OUTPUT into list of lists."
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t)))
      (mapcar (lambda (line)
                (split-string line "|" t))
              lines))))

(defun etaf-eorm--parse-mysql-output (output)
  "Parse MySQL OUTPUT into list of lists."
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t)))
      (mapcar (lambda (line)
                (split-string line "\t" t))
              lines))))

(defun etaf-eorm-get-connection (db-path)
  "Get existing connection for DB-PATH or create new one.
For backward compatibility with SQLite."
  (or (gethash db-path etaf-eorm--connections)
      (etaf-eorm-connect :sqlite db-path)))

;;; Migration System

(defun etaf-eorm-create-table (conn table-name)
  "Create table TABLE-NAME in database CONN based on defined schema."
  (let* ((db-type (etaf-eorm-connection-type conn))
         (sql (etaf-eorm--create-table-sql table-name db-type)))
    (etaf-eorm--log "Creating table: %s" sql)
    (etaf-eorm--backend-call conn 'execute sql)))

(defun etaf-eorm-drop-table (conn table-name)
  "Drop table TABLE-NAME from database CONN."
  (let ((sql (format "DROP TABLE IF EXISTS %s"
                     (etaf-eorm--symbol-to-sql table-name))))
    (etaf-eorm--log "Dropping table: %s" sql)
    (etaf-eorm--backend-call conn 'execute sql)))

(defun etaf-eorm-table-exists-p (conn table-name)
  "Check if table TABLE-NAME exists in database CONN."
  (etaf-eorm--backend-call conn 'table-exists-p table-name))

(defun etaf-eorm-migrate (conn &optional table-names)
  "Run migrations for database CONN.
Creates tables specified in TABLE-NAMES that don't exist yet.
If TABLE-NAMES is nil, creates all defined tables (legacy behavior).
TABLE-NAMES can be:
  - nil: migrate all tables in etaf-eorm--schemas (global)
  - a single table name symbol: migrate that table
  - a list of table name symbols: migrate those tables

Example:
  (etaf-eorm-migrate conn)                    ; migrate all tables
  (etaf-eorm-migrate conn 'users)             ; migrate users table
  (etaf-eorm-migrate conn '(users posts))     ; migrate users and posts"
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
      (unless (etaf-eorm-table-exists-p conn table-name)
        (etaf-eorm-create-table conn table-name)
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

(defun etaf-eorm-insert (conn table-name &rest values)
  "Insert a row into TABLE-NAME in database CONN.
VALUES is a plist of column names and values.

Example:
  (etaf-eorm-insert conn 'users
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
      (etaf-eorm--backend-call conn 'execute sql (nreverse params))
      ;; Return the last inserted row id
      (etaf-eorm--backend-call conn 'last-insert-id))))

(defun etaf-eorm-select (conn table-name &rest args)
  "Select rows from TABLE-NAME in database CONN.

ARGS is a plist with the following optional keys:
  :columns - list of columns to select (default: all)
  :where - WHERE clause expression
  :order-by - column name or list of (column direction) pairs
  :limit - maximum number of rows
  :offset - number of rows to skip

Returns a list of rows, where each row is a plist.

Example:
  (etaf-eorm-select conn 'users
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
      (let ((results (etaf-eorm--backend-call conn 'select sql)))
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

(defun etaf-eorm-update (conn table-name &rest args)
  "Update rows in TABLE-NAME in database CONN.

ARGS is a plist with the following keys:
  :set - plist of columns and new values
  :where - WHERE clause expression (required to prevent accidental mass updates)

Returns the number of affected rows.

Example:
  (etaf-eorm-update conn 'users
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
        (etaf-eorm--backend-call conn 'execute sql)
        (or (etaf-eorm--backend-call conn 'changes) 0)))))

(defun etaf-eorm-delete (conn table-name &rest args)
  "Delete rows from TABLE-NAME in database CONN.

ARGS is a plist with the following keys:
  :where - WHERE clause expression (required to prevent accidental mass deletion)

Returns the number of deleted rows.

Example:
  (etaf-eorm-delete conn 'users
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
      (etaf-eorm--backend-call conn 'execute sql)
      (or (etaf-eorm--backend-call conn 'changes) 0))))

;;; Transaction Support

(defun etaf-eorm-transaction (conn func)
  "Execute FUNC within a transaction on database CONN.
If FUNC completes successfully, commit the transaction.
If FUNC signals an error, rollback the transaction."
  (etaf-eorm--backend-call conn 'begin-transaction)
  (condition-case err
      (prog1
          (funcall func)
        (etaf-eorm--backend-call conn 'commit))
    (error
     (etaf-eorm--backend-call conn 'rollback)
     (signal (car err) (cdr err)))))

(defmacro etaf-eorm-with-transaction (db &rest body)
  "Execute BODY within a transaction on database CONN.
If BODY completes successfully, commit the transaction.
If BODY signals an error, rollback the transaction."
  (declare (indent 1))
  `(etaf-eorm-transaction ,db (lambda () ,@body)))

;;; Query Builder Interface (Chainable)

(cl-defstruct etaf-eorm-query
  "Query builder structure."
  conn
  table
  (columns nil)
  (where-clause nil)
  (order-by-clause nil)
  (limit-value nil)
  (offset-value nil))

(defun etaf-eorm-query (conn table-name)
  "Create a new query builder for TABLE-NAME on database CONN."
  (make-etaf-eorm-query :conn conn :table table-name))

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
         (etaf-eorm-query-conn query)
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

(defun etaf-eorm-reactive-query (conn table-name query-fn)
  "Create a reactive query that automatically updates when data changes.
CONN is the database connection.
TABLE-NAME is the table to watch.
QUERY-FN is a function that takes CONN and TABLE-NAME and returns query results.

Returns a ref that contains the query results and updates automatically."
  (when (require 'etaf-component nil t)
    (let* ((conn-id (etaf-eorm-connection-id conn))
           (query-key (format "%s:%s" conn-id table-name))
           (results (funcall query-fn conn table-name))
           (ref (etaf-ref results)))
      ;; Store the reactive query
      (puthash query-key (list :ref ref :query-fn query-fn :conn conn)
               etaf-eorm--reactive-queries)
      ref)))

(defun etaf-eorm--trigger-reactive-update (conn table-name)
  "Trigger reactive query updates for TABLE-NAME in database CONN."
  (when (require 'etaf-component nil t)
    (let* ((conn-id (etaf-eorm-connection-id conn))
           (query-key (format "%s:%s" conn-id table-name)))
      (when-let ((reactive-query (gethash query-key etaf-eorm--reactive-queries)))
        (let ((ref (plist-get reactive-query :ref))
              (query-fn (plist-get reactive-query :query-fn))
              (stored-conn (plist-get reactive-query :conn)))
          (etaf-ref-set ref (funcall query-fn stored-conn table-name)))))))

;; Hook into insert/update/delete to trigger reactive updates
(advice-add 'etaf-eorm-insert :after
            (lambda (conn table-name &rest _)
              (etaf-eorm--trigger-reactive-update conn table-name)))

(advice-add 'etaf-eorm-update :after
            (lambda (conn table-name &rest _)
              (etaf-eorm--trigger-reactive-update conn table-name)))

(advice-add 'etaf-eorm-delete :after
            (lambda (conn table-name &rest _)
              (etaf-eorm--trigger-reactive-update conn table-name)))

;;; Utility Functions

(defun etaf-eorm-count (conn table-name &rest args)
  "Count rows in TABLE-NAME matching ARGS.
ARGS can include :where clause."
  (let* ((where (plist-get args :where))
         (sql (concat
               (format "SELECT COUNT(*) FROM %s"
                       (etaf-eorm--symbol-to-sql table-name))
               (etaf-eorm--build-where-clause where))))
    (etaf-eorm--log "COUNT: %s" sql)
    (caar (etaf-eorm--backend-call conn 'select sql))))

(defun etaf-eorm-exists-p (conn table-name &rest args)
  "Check if any rows exist in TABLE-NAME matching ARGS.
ARGS can include :where clause."
  (> (apply #'etaf-eorm-count conn table-name args) 0))

(defun etaf-eorm-find-by-id (conn table-name id &optional id-column)
  "Find a single row by ID in TABLE-NAME.
ID-COLUMN defaults to 'id."
  (let ((id-col (or id-column 'id)))
    (etaf-eorm-query-first
     (etaf-eorm-query-where
      (etaf-eorm-query conn table-name)
      (list '= id-col id)))))

;;; Provide

(provide 'etaf-eorm)

;;; etaf-eorm.el ends here
