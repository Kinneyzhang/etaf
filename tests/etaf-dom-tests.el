(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

(setq-local lisp-indent-offset 2)
(setq etaf-dom-tests-dom
  (etaf-tml-to-dom
    '(div :class "rounded-xl bg-white p-10" :id "test-id"
       (div :class "space-y-6"
         (p "An advanced online playground for Tailwind CSS, including support for things like:")
         (ul :class "space-y-3"
           (li :id "1"
             (p :class "ml-3" "Customizing your theme with"
               (code :class "text-gray-950" "@theme")))
           (li :id "2"
             (p :class "ml-3" "Adding custom utilities with"
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

(should-equal
  (etaf-dom-to-tml etaf-dom-tests-dom)
  '(div :class "rounded-xl bg-white p-10" :id "test-id" (div :class "space-y-6" (p "An advanced online playground for Tailwind CSS, including support for things like:") (ul :class "space-y-3" (li :id "1" (p :class "ml-3" "Customizing your theme with" (code :class "text-gray-950" "@theme"))) (li :id "2" (p :class "ml-3" "Adding custom utilities with" (code :class "text-gray-950" "@utility"))) (li :id "3" (p :class "ml-3" "Adding custom variants with" (code :class "text-gray-950" "@variant"))) (li :id "4" :class "flex" (p :class "ml-3" "Code completion with instant preview"))) (p "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online.")) (hr :class "my-6 w-full") (p :class "mb-3" "Want to dig deeper into Tailwind?") (p :class "font-semibold" (a :href "https://tailwindcss.com/docs" :class "text-gray-950 dark:text-white" "Read the docs →"))))

(should (etaf-dom-tag-match-p etaf-dom-tests-dom "div"))

(should (etaf-dom-class-match-p etaf-dom-tests-dom "p-10"))

(should (etaf-dom-id-match-p etaf-dom-tests-dom "test-id"))

(provide 'etaf-dom-tests)
