;;; test-regex.el --- Test regex patterns

;; Test class selector pattern
(setq test-selector ".button")
(setq pattern "\\\\.[a-zA-Z][a-zA-Z0-9_-]*")

(message "Testing: %s with pattern: %s" test-selector pattern)
(setq pos 0)
(setq count 0)
(while (string-match "\\.[a-zA-Z][a-zA-Z0-9_-]*" test-selector pos)
  (message "  Match at %d: %s" pos (match-string 0 test-selector))
  (setq count (1+ count))
  (setq pos (match-end 0)))
(message "Total matches: %d" count)

;; Test type selector pattern
(message "\nTesting type selector pattern:")
(setq pos 0)
(setq count 0)
(while (string-match "\\(?:^\\|[ >+~]\\)\\([a-z][a-z0-9]*\\)\\(?=[#.:\\[]\\|[ >+~]\\|$\\)" test-selector pos)
  (message "  Match at %d: %s (captured: %s)" pos (match-string 0 test-selector) (match-string 1 test-selector))
  (setq count (1+ count))
  (setq pos (match-end 1)))
(message "Total matches: %d" count)

(provide 'test-regex)
