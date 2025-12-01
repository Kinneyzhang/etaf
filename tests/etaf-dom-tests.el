(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

(setq-local lisp-indent-offset 2)
(setq etaf-dom-tests-dom
      (etaf-etml-to-dom
       '(div :class "rounded-xl bg-white p-10" :id "test-id"
             (div :class "space-y-6"
                  (p "An advanced online playground for Tailwind CSS, including support for things like:")
                  (ul :class "space-y-3"
                      (li :id "1"
                          (p :class "ml-3" "Customizing your theme with"
                             (code :class "text-gray-950" "@theme")))
                      (li :id "2"
                          (p :style "color:red;padding:2"
                             :class "ml-3" :id "test-p"
                             "Adding custom utilities with"
                             (code :class "text-gray-950" "@utility")))
                      (li :id "3"
                          (p :class "ml-3" "Adding custom variants with"
                             (code :class "text-gray-950" "@variant")))
                      (li :id "4" :class "flex"
                          (p :class "ml-3" "Code completion with instant preview")))
                  (p "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online."))
             (hr :class "my-6 w-full")
             (p :class "mb-3" "Want to dig deeper into Tailwind?")
             (p :class "font-semibold"
                (a :href "https://tailwindcss.com/docs"
                   :class "text-gray-950 dark:text-white" "Read the docs →")))))

;; Note: DOM-to-TML conversion preserves all attributes including default styles from etaf-etml-tag
;; The main requirement is that etaf-tag-instance is NOT embedded in the DOM (moved to virtual DOM layer)
(let ((tml (etaf-dom-to-tml etaf-dom-tests-dom)))
  ;; Check basic structure
  (should (eq (car tml) 'div))
  (should (string= (plist-get (cdr tml) :class) "rounded-xl bg-white p-10"))
  (should (string= (plist-get (cdr tml) :id) "test-id"))
  ;; Verify NO etaf-tag-instance in any node (the main requirement)
  ;; The `a` tag should NOT have :etaf-tag-instance in TML form
  (should-not (cl-some (lambda (elem)
                         (and (listp elem)
                              (eq (car elem) 'a)
                              (plist-get (cdr elem) :etaf-tag-instance)))
                       (flatten-list tml))))

(should (etaf-dom-tag-match-p etaf-dom-tests-dom "div"))

(should (etaf-dom-class-match-p etaf-dom-tests-dom "p-10"))

(should (etaf-dom-id-match-p etaf-dom-tests-dom "test-id"))

(provide 'etaf-dom-tests)
