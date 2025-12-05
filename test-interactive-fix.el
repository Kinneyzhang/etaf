;;; test-interactive-fix.el --- Test fix for interactive element click errors -*- lexical-binding: t; -*-

;; This file tests the fix for the commandp error when clicking on
;; <a> tags and <button> elements.

(require 'etaf)
(require 'etaf-etml)
(require 'etaf-vdom)
(require 'etaf-ecss)

(defun test-interactive-elements ()
  "Test the interactive elements from the problem statement."
  (interactive)
  (etaf-paint-to-buffer "*etaf-test*"
    '(div :class "border px-2 py-1 w-50"
          (div :id "parent-div"
               (ecss "p{text-red-200}"
                     "ul>li:nth-child(odd)>p{text-green-400}"
                     "ul li>p code{text-orange-400}"
                     "a{text-blue-400}")
               (h3 "Tailwindcss")
               (p "An advanced online playground for Tailwind CSS")
               (ul (li (p "Customizing your theme with" (code "@theme")))
                   (li (p "Adding custom utilities with" (code "@utility")))
                   (li (p "Adding custom variants with" (code "@variant")))
                   (li (p "Code completion with instant preview")))
               ;; line-through overline underline 只有最后样式生效了
               (span :class "italic line-through overline underline"
                     "Perfect for learning how the framework works."))
          (p :class "mt-1" "Want to dig deeper into Tailwind?")
          (div :class "flex grow justify-between"
               (a :href "https://tailwindcss.com/docs" "Read the docs →")
               (button "click me")))))

(message "Test file loaded. Run (test-interactive-elements) to test.")
(message "After rendering, try clicking on:")
(message "  1. The 'Read the docs →' link")
(message "  2. The 'click me' button")
(message "Both should work without 'wrong-type-argument commandp' errors.")

(provide 'test-interactive-fix)
;;; test-interactive-fix.el ends here
