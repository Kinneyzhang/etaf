;;; etaf-eorm-tests.el --- Tests for etaf-eorm -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;;; Commentary:

;; Tests for the ETAF-EORM module.

;;; Code:

(require 'ert)
(require 'etaf-eorm)

;;; Test Fixtures

(defvar etaf-eorm-test-db nil
  "Test database connection.")

(defvar etaf-eorm-test-db-path nil
  "Path to test database.")

(defun etaf-eorm-test-setup ()
  "Set up test database and schemas."
  ;; Create temporary database
  (setq etaf-eorm-test-db-path (make-temp-file "etaf-eorm-test-" nil ".db"))
  (setq etaf-eorm-test-db (etaf-eorm-connect etaf-eorm-test-db-path))
  
  ;; Define test schemas
  (etaf-eorm-define-table users
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (email text :unique t)
    (age integer)
    (active boolean :default t)
    (created-at datetime :default current-timestamp))
  
  (etaf-eorm-define-table posts
    (id integer :primary-key t :autoincrement t)
    (user-id integer :not-null t :references (users id))
    (title text :not-null t)
    (content text)
    (published boolean :default nil)
    (created-at datetime :default current-timestamp))
  
  ;; Create tables
  (etaf-eorm-migrate etaf-eorm-test-db))

(defun etaf-eorm-test-teardown ()
  "Clean up test database."
  (when etaf-eorm-test-db
    (etaf-eorm-disconnect etaf-eorm-test-db)
    (setq etaf-eorm-test-db nil))
  (when (and etaf-eorm-test-db-path
             (file-exists-p etaf-eorm-test-db-path))
    (delete-file etaf-eorm-test-db-path)
    (setq etaf-eorm-test-db-path nil))
  ;; Clear schemas
  (clrhash etaf-eorm--schemas))

;;; Schema Definition Tests

(ert-deftest etaf-eorm-test-define-table ()
  "Test table schema definition."
  (etaf-eorm-define-table test-table
    (id integer :primary-key t)
    (name text :not-null t))
  (let ((schema (etaf-eorm-get-schema 'test-table)))
    (should (hash-table-p schema))
    (should (= (hash-table-count schema) 2))
    (should (equal (plist-get (gethash 'id schema) :type) 'integer))
    (should (plist-get (gethash 'id schema) :primary-key))
    (should (equal (plist-get (gethash 'name schema) :type) 'text))
    (should (plist-get (gethash 'name schema) :not-null)))
  (remhash 'test-table etaf-eorm--schemas))

(ert-deftest etaf-eorm-test-create-table-sql ()
  "Test CREATE TABLE SQL generation."
  (etaf-eorm-define-table test-table
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (email text :unique t))
  (let ((sql (etaf-eorm--create-table-sql 'test-table)))
    (should (string-match-p "CREATE TABLE IF NOT EXISTS test_table" sql))
    (should (string-match-p "id INTEGER PRIMARY KEY AUTOINCREMENT" sql))
    (should (string-match-p "name TEXT NOT NULL" sql))
    (should (string-match-p "email TEXT UNIQUE" sql)))
  (remhash 'test-table etaf-eorm--schemas))

;;; Connection Tests

(ert-deftest etaf-eorm-test-sqlite-available ()
  "Test SQLite availability check."
  (if (and (fboundp 'sqlite-available-p)
           (sqlite-available-p))
      (should t)
    (should-error (etaf-eorm--ensure-sqlite))))

(ert-deftest etaf-eorm-test-connect-disconnect ()
  "Test database connection and disconnection."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (let* ((db-path (make-temp-file "etaf-eorm-test-" nil ".db"))
         (db (etaf-eorm-connect db-path)))
    (should db)
    (should (gethash db-path etaf-eorm--connections))
    (etaf-eorm-disconnect db)
    (should-not (gethash db-path etaf-eorm--connections))
    (delete-file db-path)))

;;; Migration Tests

(ert-deftest etaf-eorm-test-create-and-drop-table ()
  "Test table creation and deletion."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (should (etaf-eorm-table-exists-p etaf-eorm-test-db 'users))
        (etaf-eorm-drop-table etaf-eorm-test-db 'users)
        (should-not (etaf-eorm-table-exists-p etaf-eorm-test-db 'users))
        (etaf-eorm-create-table etaf-eorm-test-db 'users)
        (should (etaf-eorm-table-exists-p etaf-eorm-test-db 'users)))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-migrate ()
  "Test migration system."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (let* ((db-path (make-temp-file "etaf-eorm-test-" nil ".db"))
         (db (etaf-eorm-connect db-path)))
    (unwind-protect
        (progn
          (etaf-eorm-define-table migrate-test
            (id integer :primary-key t)
            (data text))
          (should-not (etaf-eorm-table-exists-p db 'migrate-test))
          (etaf-eorm-migrate db)
          (should (etaf-eorm-table-exists-p db 'migrate-test)))
      (etaf-eorm-disconnect db)
      (delete-file db-path)
      (remhash 'migrate-test etaf-eorm--schemas))))

;;; Insert Tests

(ert-deftest etaf-eorm-test-insert ()
  "Test row insertion."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (let ((id (etaf-eorm-insert etaf-eorm-test-db 'users
                  :name "Alice"
                  :email "alice@example.com"
                  :age 30)))
        (should (numberp id))
        (should (> id 0)))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-insert-multiple ()
  "Test multiple row insertions."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Charlie" :email "charlie@example.com" :age 35)
        (let ((count (etaf-eorm-count etaf-eorm-test-db 'users)))
          (should (= count 3))))
    (etaf-eorm-test-teardown)))

;;; Select Tests

(ert-deftest etaf-eorm-test-select-all ()
  "Test selecting all rows."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users)))
          (should (= (length results) 2))
          (should (plist-get (car results) :name))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-select-with-columns ()
  "Test selecting specific columns."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :columns '(name age))))
          (should (= (length results) 1))
          (let ((row (car results)))
            (should (plist-get row :name))
            (should (plist-get row :age)))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-select-with-where ()
  "Test selecting with WHERE clause."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Charlie" :email "charlie@example.com" :age 35)
        ;; Test simple WHERE
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :where '(> age 28))))
          (should (= (length results) 2)))
        ;; Test AND
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :where '(and (> age 25) (< age 35)))))
          (should (= (length results) 1)))
        ;; Test equality
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :where '(= name "Bob"))))
          (should (= (length results) 1))
          (should (string= (plist-get (car results) :name) "Bob"))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-select-with-order ()
  "Test selecting with ORDER BY."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Charlie" :email "charlie@example.com" :age 35)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        ;; Test ORDER BY ascending
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :order-by 'name)))
          (should (string= (plist-get (car results) :name) "Alice")))
        ;; Test ORDER BY with direction
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :order-by '((age desc)))))
          (should (string= (plist-get (car results) :name) "Charlie"))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-select-with-limit-offset ()
  "Test selecting with LIMIT and OFFSET."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (dotimes (i 10)
          (etaf-eorm-insert etaf-eorm-test-db 'users
            :name (format "User%d" i)
            :email (format "user%d@example.com" i)
            :age (+ 20 i)))
        ;; Test LIMIT
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :limit 5)))
          (should (= (length results) 5)))
        ;; Test LIMIT with OFFSET
        (let ((results (etaf-eorm-select etaf-eorm-test-db 'users
                         :limit 3
                         :offset 5)))
          (should (= (length results) 3))))
    (etaf-eorm-test-teardown)))

;;; Update Tests

(ert-deftest etaf-eorm-test-update ()
  "Test row update."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (let ((affected (etaf-eorm-update etaf-eorm-test-db 'users
                          :set '(:age 31 :email "alice.new@example.com")
                          :where '(= name "Alice"))))
          (should (= affected 1)))
        (let ((result (car (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(= name "Alice")))))
          (should (= (plist-get result :age) 31))
          (should (string= (plist-get result :email) "alice.new@example.com"))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-update-requires-where ()
  "Test that UPDATE requires WHERE clause."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (should-error
       (etaf-eorm-update etaf-eorm-test-db 'users
         :set '(:age 31))
       :type 'error)
    (etaf-eorm-test-teardown)))

;;; Delete Tests

(ert-deftest etaf-eorm-test-delete ()
  "Test row deletion."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (let ((deleted (etaf-eorm-delete etaf-eorm-test-db 'users
                         :where '(= name "Alice"))))
          (should (= deleted 1)))
        (let ((count (etaf-eorm-count etaf-eorm-test-db 'users)))
          (should (= count 1))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-delete-requires-where ()
  "Test that DELETE requires WHERE clause."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (should-error
       (etaf-eorm-delete etaf-eorm-test-db 'users)
       :type 'error)
    (etaf-eorm-test-teardown)))

;;; Transaction Tests

(ert-deftest etaf-eorm-test-transaction-commit ()
  "Test transaction commit."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-with-transaction etaf-eorm-test-db
          (etaf-eorm-insert etaf-eorm-test-db 'users
            :name "Alice" :email "alice@example.com" :age 30)
          (etaf-eorm-insert etaf-eorm-test-db 'users
            :name "Bob" :email "bob@example.com" :age 25))
        (let ((count (etaf-eorm-count etaf-eorm-test-db 'users)))
          (should (= count 2))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-transaction-rollback ()
  "Test transaction rollback."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (should-error
         (etaf-eorm-with-transaction etaf-eorm-test-db
           (etaf-eorm-insert etaf-eorm-test-db 'users
             :name "Alice" :email "alice@example.com" :age 30)
           (error "Intentional error"))
         :type 'error)
        (let ((count (etaf-eorm-count etaf-eorm-test-db 'users)))
          (should (= count 0))))
    (etaf-eorm-test-teardown)))

;;; Query Builder Tests

(ert-deftest etaf-eorm-test-query-builder ()
  "Test query builder interface."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Charlie" :email "charlie@example.com" :age 35)
        ;; Test chainable query builder
        (let ((results (etaf-eorm-query-get
                        (etaf-eorm-query-order-by
                         (etaf-eorm-query-where
                          (etaf-eorm-query-select
                           (etaf-eorm-query etaf-eorm-test-db 'users)
                           'name 'age)
                          '(> age 25))
                         '(age desc)))))
          (should (= (length results) 2))
          (should (string= (plist-get (car results) :name) "Charlie")))
        ;; Test query-first
        (let ((result (etaf-eorm-query-first
                       (etaf-eorm-query-where
                        (etaf-eorm-query etaf-eorm-test-db 'users)
                        '(= name "Bob")))))
          (should (string= (plist-get result :name) "Bob"))))
    (etaf-eorm-test-teardown)))

;;; Utility Function Tests

(ert-deftest etaf-eorm-test-count ()
  "Test count function."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (should (= (etaf-eorm-count etaf-eorm-test-db 'users) 2))
        (should (= (etaf-eorm-count etaf-eorm-test-db 'users
                     :where '(> age 28)) 1)))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-exists-p ()
  "Test exists-p function."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (should-not (etaf-eorm-exists-p etaf-eorm-test-db 'users))
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (should (etaf-eorm-exists-p etaf-eorm-test-db 'users))
        (should (etaf-eorm-exists-p etaf-eorm-test-db 'users
                  :where '(= name "Alice")))
        (should-not (etaf-eorm-exists-p etaf-eorm-test-db 'users
                      :where '(= name "Bob"))))
    (etaf-eorm-test-teardown)))

(ert-deftest etaf-eorm-test-find-by-id ()
  "Test find-by-id function."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (let ((id (etaf-eorm-insert etaf-eorm-test-db 'users
                    :name "Alice" :email "alice@example.com" :age 30)))
          (let ((user (etaf-eorm-find-by-id etaf-eorm-test-db 'users id)))
            (should user)
            (should (string= (plist-get user :name) "Alice")))))
    (etaf-eorm-test-teardown)))

;;; WHERE Clause Tests

(ert-deftest etaf-eorm-test-where-operators ()
  "Test various WHERE clause operators."
  (skip-unless (and (fboundp 'sqlite-available-p)
                    (sqlite-available-p)))
  (etaf-eorm-test-setup)
  (unwind-protect
      (progn
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Alice" :email "alice@example.com" :age 30)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Bob" :email "bob@example.com" :age 25)
        (etaf-eorm-insert etaf-eorm-test-db 'users
          :name "Charlie" :email "charlie@example.com" :age 35)
        ;; Test !=
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(!= name "Alice"))) 2))
        ;; Test >=
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(>= age 30))) 2))
        ;; Test <=
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(<= age 30))) 2))
        ;; Test LIKE
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(like email "%alice%"))) 1))
        ;; Test IN
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(in name ("Alice" "Bob")))) 2))
        ;; Test OR
        (should (= (length (etaf-eorm-select etaf-eorm-test-db 'users
                             :where '(or (= name "Alice") (= name "Charlie")))) 2)))
    (etaf-eorm-test-teardown)))

;;; Type Conversion Tests

(ert-deftest etaf-eorm-test-type-conversion ()
  "Test type conversion functions."
  (should (string= (etaf-eorm--type-to-sql 'integer) "INTEGER"))
  (should (string= (etaf-eorm--type-to-sql 'text) "TEXT"))
  (should (string= (etaf-eorm--type-to-sql 'real) "REAL"))
  (should (eq (etaf-eorm--sql-to-type "INTEGER") 'integer))
  (should (eq (etaf-eorm--sql-to-type "TEXT") 'text)))

(ert-deftest etaf-eorm-test-symbol-conversion ()
  "Test symbol-to-SQL conversion."
  (should (string= (etaf-eorm--symbol-to-sql 'user-id) "user_id"))
  (should (string= (etaf-eorm--symbol-to-sql 'created-at) "created_at"))
  (should (eq (etaf-eorm--sql-to-symbol "user_id") 'user-id))
  (should (eq (etaf-eorm--sql-to-symbol "created_at") 'created-at)))

(provide 'etaf-eorm-tests)

;;; etaf-eorm-tests.el ends here
