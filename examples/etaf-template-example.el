;;; etaf-template-example.el --- Vue.js-like template syntax example -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; This file demonstrates the Vue.js-like template syntax for TML.
;;
;; Core Features:
;; - Text interpolation: {{ expression }}
;; - Conditional rendering: v-if, v-else-if, v-else
;; - List rendering: v-for
;; - Attribute binding with interpolation
;; - v-text directive
;; - v-show directive

;;; Code:

(require 'etaf-template)
(require 'etaf-tml)

;;; Example 1: Text Interpolation

(defun etaf-template-example-interpolation ()
  "Demonstrate text interpolation with {{ }} syntax."
  (interactive)
  (message "=== Text Interpolation Example ===\n")
  
  (let* ((template '(div :class "greeting"
                      (h1 "Hello, {{ name }}!")
                      (p "You have {{ count }} messages.")))
         (data '(:name "Alice" :count 5))
         (result (etaf-template-render template data)))
    
    (message "Template: %S" template)
    (message "Data: %S" data)
    (message "Result: %S\n" result)))

;;; Example 2: Conditional Rendering (v-if / v-else)

(defun etaf-template-example-conditional ()
  "Demonstrate conditional rendering with v-if and v-else."
  (interactive)
  (message "=== Conditional Rendering Example ===\n")
  
  (let* ((template '(div
                     (p :v-if "loggedIn" "Welcome back, {{ username }}!")
                     (p :v-else "Please sign in to continue.")))
         
         ;; Test with loggedIn = true
         (data-logged-in '(:loggedIn t :username "Bob"))
         (result-logged-in (etaf-template-render template data-logged-in))
         
         ;; Test with loggedIn = false
         (data-logged-out '(:loggedIn nil :username ""))
         (result-logged-out (etaf-template-render template data-logged-out)))
    
    (message "Template: %S\n" template)
    (message "With loggedIn=true:")
    (message "  Data: %S" data-logged-in)
    (message "  Result: %S\n" result-logged-in)
    (message "With loggedIn=false:")
    (message "  Data: %S" data-logged-out)
    (message "  Result: %S\n" result-logged-out)))

;;; Example 3: Multiple Conditions (v-if / v-else-if / v-else)

(defun etaf-template-example-multiple-conditions ()
  "Demonstrate v-if / v-else-if / v-else chain."
  (interactive)
  (message "=== Multiple Conditions Example ===\n")
  
  (let* ((template '(div
                     (p :v-if "role" :class "role-{{ role }}" "Role: {{ role }}")
                     (p :v-else-if "isGuest" :class "guest" "Welcome, Guest!")
                     (p :v-else "Unknown user")))
         
         ;; Test cases
         (test-cases '((:role "admin" :isGuest nil)
                       (:role nil :isGuest t)
                       (:role nil :isGuest nil))))
    
    (message "Template: %S\n" template)
    (dolist (data test-cases)
      (let ((result (etaf-template-render template data)))
        (message "Data: %S" data)
        (message "Result: %S\n" result)))))

;;; Example 4: List Rendering (v-for)

(defun etaf-template-example-list-rendering ()
  "Demonstrate list rendering with v-for."
  (interactive)
  (message "=== List Rendering Example ===\n")
  
  ;; Simple list
  (let* ((template '(ul
                     (li :v-for "item in items" "{{ item }}")))
         (data '(:items ("Apple" "Banana" "Cherry")))
         (result (etaf-template-render template data)))
    
    (message "Simple list:")
    (message "Template: %S" template)
    (message "Data: %S" data)
    (message "Result: %S\n" result))
  
  ;; List with index
  (let* ((template '(ol
                     (li :v-for "(item, idx) in items" 
                         "{{ idx }}: {{ item }}")))
         (data '(:items ("First" "Second" "Third")))
         (result (etaf-template-render template data)))
    
    (message "List with index:")
    (message "Template: %S" template)
    (message "Data: %S" data)
    (message "Result: %S\n" result)))

;;; Example 5: v-show directive

(defun etaf-template-example-v-show ()
  "Demonstrate v-show directive for visibility control."
  (interactive)
  (message "=== v-show Example ===\n")
  
  (let* ((template '(div
                     (p :v-show "visible" "This text is visible")
                     (p :v-show "hidden" "This text is hidden")))
         (data '(:visible t :hidden nil))
         (result (etaf-template-render template data)))
    
    (message "Template: %S" template)
    (message "Data: %S" data)
    (message "Result: %S\n" result)
    (message "Note: Hidden element has 'display: none' style")))

;;; Example 6: v-text directive

(defun etaf-template-example-v-text ()
  "Demonstrate v-text directive."
  (interactive)
  (message "=== v-text Example ===\n")
  
  (let* ((template '(div
                     (span :v-text "message")))
         (data '(:message "Hello from v-text!"))
         (result (etaf-template-render template data)))
    
    (message "Template: %S" template)
    (message "Data: %S" data)
    (message "Result: %S\n" result)))

;;; Example 7: Complex Template

(defun etaf-template-example-complex ()
  "Demonstrate a complex template combining multiple features."
  (interactive)
  (message "=== Complex Template Example ===\n")
  
  (let* ((template
          '(div :class "app"
             (header
              (h1 "{{ title }}")
              (nav :v-if "navItems"
                   (ul
                    (li :v-for "item in navItems"
                        (a :href "{{ item.url }}" "{{ item.name }}")))))
             (main
              (section :v-if "articles"
                       (article :v-for "article in articles"
                                :class "article"
                                (h2 "{{ article.title }}")
                                (p "{{ article.summary }}")))
              (section :v-else
                       (p "No articles available")))
             (footer
              (p :v-show "showFooter" "© 2024 {{ siteName }}"))))
         
         (data '(:title "My Blog"
                 :navItems ((:name "Home" :url "/")
                            (:name "About" :url "/about")
                            (:name "Contact" :url "/contact"))
                 :articles ((:title "First Post" :summary "This is the first post")
                            (:title "Second Post" :summary "Another great post"))
                 :showFooter t
                 :siteName "ETAF Blog"))
         
         (result (etaf-template-render template data)))
    
    (message "Complex Template Result:")
    (message "%S\n" result)
    
    ;; Also convert to DOM format
    (let ((dom (etaf-tml-to-dom result)))
      (message "As DOM:")
      (message "%S" dom))))

;;; Example 8: Reactive Data Demo

(defun etaf-template-example-reactive ()
  "Demonstrate reactive data system."
  (interactive)
  (message "=== Reactive Data Example ===\n")
  
  (let* ((reactive (etaf-template-create-reactive '(:count 0 :name "Counter")))
         (template '(div
                     (h1 "{{ name }}")
                     (p "Count: {{ count }}"))))
    
    ;; Add a watcher
    (etaf-template-watch reactive
      (lambda (r key value)
        (message "Data changed: %S = %S" key value)))
    
    ;; Render with initial data
    (message "Initial render:")
    (message "%S\n" (etaf-template-render template (plist-get reactive :data)))
    
    ;; Update data
    (message "Updating count to 5...")
    (etaf-template-set reactive :count 5)
    
    ;; Render with updated data
    (message "\nAfter update:")
    (message "%S\n" (etaf-template-render template (plist-get reactive :data)))))

;;; Run All Examples

(defun etaf-template-run-all-examples ()
  "Run all template examples."
  (interactive)
  (etaf-template-example-interpolation)
  (etaf-template-example-conditional)
  (etaf-template-example-multiple-conditions)
  (etaf-template-example-list-rendering)
  (etaf-template-example-v-show)
  (etaf-template-example-v-text)
  (etaf-template-example-complex)
  (etaf-template-example-reactive)
  (message "\n=== All Examples Complete ==="))

;; To run all examples interactively, use M-x etaf-template-run-all-examples
;; or evaluate (etaf-template-run-all-examples) manually.

(provide 'etaf-template-example)
;;; etaf-template-example.el ends here
