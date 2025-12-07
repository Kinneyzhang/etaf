;;; etaf-eorm-example.el --- Example usage of ETAF-EORM -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;;; Commentary:

;; This file demonstrates the usage of ETAF-EORM, showing how to:
;; - Define database schemas
;; - Perform CRUD operations
;; - Use the query builder
;; - Integrate with ETAF's reactive system

;;; Code:

(require 'etaf)
(require 'etaf-eorm)

;;; Example 1: Basic CRUD Operations

(defun etaf-eorm-example-basic ()
  "Demonstrate basic CRUD operations with ETAF-EORM."
  (interactive)
  
  ;; Enable logging to see SQL queries
  (setq etaf-eorm-enable-logging t)
  
  ;; Define a schema for a blog application
  (etaf-eorm-define-table blog-posts
    (id integer :primary-key t :autoincrement t)
    (title text :not-null t)
    (content text)
    (author text :not-null t)
    (published boolean :default nil)
    (views integer :default 0)
    (created-at datetime :default current-timestamp))
  
  ;; Connect to database
  (let ((db (etaf-eorm-connect "~/etaf-blog-example.db")))
    (unwind-protect
        (progn
          ;; Create tables
          (etaf-eorm-migrate db)
          
          ;; INSERT - Create some blog posts
          (message "\n=== INSERT Operations ===")
          (etaf-eorm-insert db 'blog-posts
            :title "Introduction to ETAF"
            :content "ETAF is a powerful framework for building text-based UIs in Emacs."
            :author "Alice"
            :published t
            :views 100)
          
          (etaf-eorm-insert db 'blog-posts
            :title "Getting Started with ETAF-EORM"
            :content "Learn how to use ETAF-EORM for database operations."
            :author "Bob"
            :published t
            :views 50)
          
          (etaf-eorm-insert db 'blog-posts
            :title "Advanced CSS in ETAF"
            :content "Deep dive into ETAF's CSS system."
            :author "Alice"
            :published nil
            :views 0)
          
          (message "Inserted 3 blog posts")
          
          ;; SELECT - Query all posts
          (message "\n=== SELECT All Posts ===")
          (let ((all-posts (etaf-eorm-select db 'blog-posts)))
            (dolist (post all-posts)
              (message "Post: %s by %s (%d views)"
                       (plist-get post :title)
                       (plist-get post :author)
                       (plist-get post :views))))
          
          ;; SELECT with WHERE - Find published posts
          (message "\n=== SELECT Published Posts ===")
          (let ((published-posts (etaf-eorm-select db 'blog-posts
                                   :where '(= published 1)
                                   :order-by '((views desc)))))
            (dolist (post published-posts)
              (message "Published: %s (%d views)"
                       (plist-get post :title)
                       (plist-get post :views))))
          
          ;; SELECT with complex WHERE - Posts by Alice with views > 0
          (message "\n=== SELECT Alice's Popular Posts ===")
          (let ((alice-posts (etaf-eorm-select db 'blog-posts
                               :where '(and (= author "Alice")
                                            (> views 0)))))
            (dolist (post alice-posts)
              (message "Alice's post: %s" (plist-get post :title))))
          
          ;; UPDATE - Increase view count
          (message "\n=== UPDATE View Count ===")
          (let ((affected (etaf-eorm-update db 'blog-posts
                            :set '(:views 150)
                            :where '(= title "Introduction to ETAF"))))
            (message "Updated %d rows" affected))
          
          ;; UPDATE - Publish a draft
          (message "\n=== UPDATE Publish Draft ===")
          (etaf-eorm-update db 'blog-posts
            :set '(:published 1)
            :where '(= title "Advanced CSS in ETAF"))
          (message "Published draft")
          
          ;; DELETE - Remove a post
          (message "\n=== DELETE Operation ===")
          (let ((deleted (etaf-eorm-delete db 'blog-posts
                           :where '(= views 0))))
            (message "Deleted %d posts with 0 views" deleted))
          
          ;; COUNT - Show remaining posts
          (message "\n=== COUNT Posts ===")
          (message "Total posts: %d" (etaf-eorm-count db 'blog-posts))
          (message "Published posts: %d"
                   (etaf-eorm-count db 'blog-posts :where '(= published 1))))
      
      ;; Cleanup
      (etaf-eorm-disconnect db))))

;;; Example 2: Query Builder

(defun etaf-eorm-example-query-builder ()
  "Demonstrate the query builder interface."
  (interactive)
  
  (etaf-eorm-define-table products
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (category text :not-null t)
    (price real :not-null t)
    (stock integer :default 0))
  
  (let ((db (etaf-eorm-connect "~/etaf-products-example.db")))
    (unwind-protect
        (progn
          (etaf-eorm-migrate db)
          
          ;; Insert sample products
          (etaf-eorm-insert db 'products
            :name "Laptop" :category "Electronics" :price 999.99 :stock 10)
          (etaf-eorm-insert db 'products
            :name "Mouse" :category "Electronics" :price 29.99 :stock 50)
          (etaf-eorm-insert db 'products
            :name "Desk" :category "Furniture" :price 299.99 :stock 5)
          (etaf-eorm-insert db 'products
            :name "Chair" :category "Furniture" :price 199.99 :stock 8)
          
          ;; Chainable query builder
          (message "\n=== Query Builder - Find expensive electronics ===")
          (let ((results (etaf-eorm-query-get
                          (etaf-eorm-query-limit
                           (etaf-eorm-query-order-by
                            (etaf-eorm-query-where
                             (etaf-eorm-query-select
                              (etaf-eorm-query db 'products)
                              'name 'price)
                             '(and (= category "Electronics")
                                   (> price 50)))
                            '((price desc)))
                           10))))
            (dolist (product results)
              (message "%s: $%.2f"
                       (plist-get product :name)
                       (plist-get product :price))))
          
          ;; Find first result
          (message "\n=== Query Builder - Find cheapest product ===")
          (let ((cheapest (etaf-eorm-query-first
                           (etaf-eorm-query-order-by
                            (etaf-eorm-query db 'products)
                            'price))))
            (message "Cheapest: %s at $%.2f"
                     (plist-get cheapest :name)
                     (plist-get cheapest :price))))
      
      (etaf-eorm-disconnect db))))

;;; Example 3: Transactions

(defun etaf-eorm-example-transactions ()
  "Demonstrate transaction support."
  (interactive)
  
  (etaf-eorm-define-table bank-accounts
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (balance real :not-null t))
  
  (let ((db (etaf-eorm-connect "~/etaf-bank-example.db")))
    (unwind-protect
        (progn
          (etaf-eorm-migrate db)
          
          ;; Create accounts
          (etaf-eorm-insert db 'bank-accounts :name "Alice" :balance 1000.0)
          (etaf-eorm-insert db 'bank-accounts :name "Bob" :balance 500.0)
          
          (message "\n=== Initial Balances ===")
          (let ((accounts (etaf-eorm-select db 'bank-accounts)))
            (dolist (account accounts)
              (message "%s: $%.2f"
                       (plist-get account :name)
                       (plist-get account :balance))))
          
          ;; Transfer money with transaction
          (message "\n=== Transfer $200 from Alice to Bob ===")
          (etaf-eorm-with-transaction db
            ;; Deduct from Alice
            (etaf-eorm-update db 'bank-accounts
              :set '(:balance 800.0)
              :where '(= name "Alice"))
            ;; Add to Bob
            (etaf-eorm-update db 'bank-accounts
              :set '(:balance 700.0)
              :where '(= name "Bob")))
          
          (message "\n=== Final Balances ===")
          (let ((accounts (etaf-eorm-select db 'bank-accounts)))
            (dolist (account accounts)
              (message "%s: $%.2f"
                       (plist-get account :name)
                       (plist-get account :balance)))))
      
      (etaf-eorm-disconnect db))))

;;; Example 4: Integration with ETAF Reactive System

(defun etaf-eorm-example-reactive ()
  "Demonstrate integration with ETAF's reactive system."
  (interactive)
  
  (when (require 'etaf-component nil t)
    (etaf-eorm-define-table tasks
      (id integer :primary-key t :autoincrement t)
      (title text :not-null t)
      (completed boolean :default nil))
    
    (let ((db (etaf-eorm-connect "~/etaf-tasks-example.db")))
      (unwind-protect
          (progn
            (etaf-eorm-migrate db)
            
            ;; Insert initial tasks
            (etaf-eorm-insert db 'tasks :title "Learn ETAF" :completed nil)
            (etaf-eorm-insert db 'tasks :title "Try ETAF-EORM" :completed nil)
            
            ;; Create a reactive query
            (let ((tasks-ref (etaf-eorm-reactive-query
                              db 'tasks
                              (lambda (db table)
                                (etaf-eorm-select db table
                                  :order-by 'id)))))
              
              ;; Watch for changes
              (etaf-watch tasks-ref
                (lambda (new-val old-val)
                  (message "\n=== Tasks Updated ===")
                  (dolist (task new-val)
                    (message "- [%s] %s"
                             (if (= (plist-get task :completed) 1) "X" " ")
                             (plist-get task :title)))))
              
              ;; Trigger initial watch
              (message "\n=== Initial Tasks ===")
              (dolist (task (etaf-ref-get tasks-ref))
                (message "- [ ] %s" (plist-get task :title)))
              
              ;; Insert a new task - will trigger reactive update
              (message "\n=== Adding New Task ===")
              (etaf-eorm-insert db 'tasks :title "Build an app" :completed nil)
              
              ;; Update a task - will trigger reactive update
              (message "\n=== Completing First Task ===")
              (etaf-eorm-update db 'tasks
                :set '(:completed 1)
                :where '(= title "Learn ETAF"))))
        
        (etaf-eorm-disconnect db)))))

;;; Example 5: Display Data in ETAF UI

(defun etaf-eorm-example-ui ()
  "Demonstrate displaying database data in ETAF UI."
  (interactive)
  
  (etaf-eorm-define-table users
    (id integer :primary-key t :autoincrement t)
    (name text :not-null t)
    (role text :not-null t)
    (active boolean :default t))
  
  (let ((db (etaf-eorm-connect "~/etaf-users-example.db")))
    (unwind-protect
        (progn
          (etaf-eorm-migrate db)
          
          ;; Insert sample users
          (etaf-eorm-insert db 'users :name "Alice" :role "Admin" :active t)
          (etaf-eorm-insert db 'users :name "Bob" :role "Editor" :active t)
          (etaf-eorm-insert db 'users :name "Charlie" :role "Viewer" :active nil)
          
          ;; Query users
          (let* ((users (etaf-eorm-select db 'users :order-by 'name))
                 (user-rows
                  (mapcar
                   (lambda (user)
                     `(tr
                       (td :class "p-1" ,(number-to-string (plist-get user :id)))
                       (td :class "p-1" ,(plist-get user :name))
                       (td :class "p-1" ,(plist-get user :role))
                       (td :class "p-1" ,(if (= (plist-get user :active) 1)
                                             "Active"
                                           "Inactive"))))
                   users)))
            
            ;; Render in ETAF
            (etaf-paint-to-buffer "*ETAF-EORM Users*"
              `(div :class "p-2"
                    (h1 :class "font-bold text-lg mb-2" "User List")
                    (table :class "w-full"
                           (thead
                            (tr :class "font-bold"
                                (th :class "p-1" "ID")
                                (th :class "p-1" "Name")
                                (th :class "p-1" "Role")
                                (th :class "p-1" "Status")))
                           (tbody
                            ,@user-rows)))
              nil
              '(".table{border-collapse: collapse}"
                "th, td{border: 1px solid gray; text-align: left}"
                "tr:hover{bg-gray-100}")
              80)))
      
      (etaf-eorm-disconnect db))))

;;; Run all examples

(defun etaf-eorm-run-all-examples ()
  "Run all ETAF-EORM examples."
  (interactive)
  (message "\n========== ETAF-EORM Examples ==========\n")
  (etaf-eorm-example-basic)
  (sit-for 1)
  (etaf-eorm-example-query-builder)
  (sit-for 1)
  (etaf-eorm-example-transactions)
  (sit-for 1)
  (when (require 'etaf-component nil t)
    (etaf-eorm-example-reactive))
  (sit-for 1)
  (etaf-eorm-example-ui)
  (message "\n========== Examples Complete ==========\n"))

(provide 'etaf-eorm-example)

;;; etaf-eorm-example.el ends here
