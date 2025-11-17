;;; test-cascade-manual.el --- Manual test for cascade bug

(add-to-list 'load-path "/home/runner/work/ETML/ETML")
(require 'etaf-css)
(require 'etaf-tml)
(require 'etaf-ert)

(message "=== Manual Cascade Test ===\n")

;; Create the test DOM
(message "1. Creating test DOM...")
(setq test-dom
      (etaf-tml-to-dom
       '(html
          (head
            (style "div { color: blue; font-size: 12px; }"))
          (body
            (div :id "test" :style "color: red;" "Text")))))
(message "   DOM created: %S\n" (dom-tag test-dom))

;; Build CSSOM
(message "2. Building CSSOM...")
(setq cssom (etaf-css-build-cssom test-dom))
(message "   Inline rules count: %d" (length (plist-get cssom :inline-rules)))
(message "   Style rules count: %d" (length (plist-get cssom :style-rules)))
(message "   All rules count: %d\n" (length (plist-get cssom :all-rules)))

;; Print all rules
(message "3. All rules:")
(let ((i 1))
  (dolist (rule (plist-get cssom :all-rules))
    (message "   Rule %d: selector=%S source=%S" 
             i
             (plist-get rule :selector)
             (plist-get rule :source))
    (message "           declarations=%S"
             (plist-get rule :declarations))
    (cl-incf i)))
(message "")

;; Get div node
(message "4. Finding div#test...")
(setq div-node (dom-by-id test-dom "test"))
(message "   Found: %S\n" div-node)

;; Get matching rules
(message "5. Getting matching rules for div#test...")
(setq matching-rules (etaf-css-get-rules-for-node cssom div-node test-dom))
(message "   Matching rules count: %d" (length matching-rules))
(let ((i 1))
  (dolist (rule matching-rules)
    (message "   Match %d: selector=%S source=%S decls=%S"
             i
             (plist-get rule :selector)
             (plist-get rule :source)
             (plist-get rule :declarations))
    (cl-incf i)))
(message "")

;; Compute style
(message "6. Computing final style...")
(setq computed (etaf-css-get-computed-style cssom div-node test-dom))
(message "   Computed style: %S\n" computed)

;; Check results
(message "7. Checking results...")
(let ((color-value (cdr (assq 'color computed)))
      (font-size-value (cdr (assq 'font-size computed))))
  (message "   color: %S (expected: \"red\")" color-value)
  (message "   font-size: %S (expected: \"12px\")" font-size-value)
  (message "")
  (if (and (equal color-value "red")
           (equal font-size-value "12px"))
      (message "✓ TEST PASSED!")
    (message "✗ TEST FAILED!")))

(provide 'test-cascade-manual)
