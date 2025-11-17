(require 'etaf-ert)

(setq-local lisp-indent-offset 2)

(should-equal
  (etaf-tml-to-dom
    '(div :class "rounded-xl bg-white p-10"
       (div :class "space-y-6"
         (p "An advanced online playground for Tailwind CSS, including support for things like:")
         (ul :class "space-y-3"
           (li (p :class "ml-3" "Customizing your theme with"
                 (code :class "text-gray-950" "@theme")))
           (li (p :class "ml-3" "Adding custom utilities with"
                 (code :class "text-gray-950" "@utility")))
           (li (p :class "ml-3" "Adding custom variants with"
                 (code :class "text-gray-950" "@variant")))
           (li :class "flex"
             (p :class "ml-3" "Code completion with instant preview")))
         (p "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online."))
       (hr :class "my-6 w-full")
       (p :class "mb-3" "Want to dig deeper into Tailwind?")
       (p :class "font-semibold"
         (a :href "https://tailwindcss.com/docs"
           :class "text-gray-950" "Read the docs →"))))
  '(div ((class . "rounded-xl bg-white p-10")) (div ((class . "space-y-6")) (p nil "An advanced online playground for Tailwind CSS, including support for things like:") (ul ((class . "space-y-3")) (li nil (p ((class . "ml-3")) "Customizing your theme with" (code ((class . "text-gray-950")) "@theme"))) (li nil (p ((class . "ml-3")) "Adding custom utilities with" (code ((class . "text-gray-950")) "@utility"))) (li nil (p ((class . "ml-3")) "Adding custom variants with" (code ((class . "text-gray-950")) "@variant"))) (li ((class . "flex")) (p ((class . "ml-3")) "Code completion with instant preview"))) (p nil "Perfect for learning how the framework works, prototyping a new idea, or creating a demo to share online.")) (hr ((class . "my-6 w-full"))) (p ((class . "mb-3")) "Want to dig deeper into Tailwind?") (p ((class . "font-semibold")) (a ((href . "https://tailwindcss.com/docs") (class . "text-gray-950")) "Read the docs →"))))

(provide 'etaf-tml-tests)
