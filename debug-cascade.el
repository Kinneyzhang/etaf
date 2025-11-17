;;; debug-cascade.el --- Debug cascade issue

(add-to-list 'load-path "/home/runner/work/ETML/ETML")
(require 'etaf-css)
(require 'etaf-tml)

;; Test case
(setq test-dom
      (etaf-tml-to-dom
       '(html
          (head
            (style "div { color: blue; } #test { color: green; } .button { color: yellow; }"))
          (body
            (div :id "test" :class "button" "Text")))))

(message "=== Building CSSOM ===")
(setq cssom (etaf-css-build-cssom test-dom))

(message "\nStyle rules:")
(dolist (rule (plist-get cssom :style-rules))
  (message "  Selector: %s, Specificity: %S, Declarations: %S"
           (plist-get rule :selector)
           (plist-get rule :specificity)
           (plist-get rule :declarations)))

(message "\n=== Finding node ===")
(setq div-node (dom-by-id test-dom "test"))
(message "Node: %S" div-node)

(message "\n=== Getting matching rules ===")
(setq matching-rules (etaf-css-get-rules-for-node cssom div-node test-dom))
(message "Found %d matching rules:" (length matching-rules))
(dolist (rule matching-rules)
  (message "  Selector: %s, Specificity: %S, Declarations: %S"
           (plist-get rule :selector)
           (plist-get rule :specificity)
           (plist-get rule :declarations)))

(message "\n=== Computing style ===")
(setq computed (etaf-css-get-computed-style cssom div-node test-dom))
(message "Computed style: %S" computed)
(message "Color value: %S" (cdr (assq 'color computed)))

(provide 'debug-cascade)
