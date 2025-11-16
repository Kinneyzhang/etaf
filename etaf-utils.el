(defun etaf-plist-to-alist (plist)
  "Convert a plist to an alist.
Example: (:class \"foo\" :id \"bar\")
         => ((class . \"foo\") (id . \"bar\"))"
  (when plist
    (let ((result nil))
      (while plist
        (let ((key (pop plist))
              (value (pop plist)))
          (push (cons (intern (substring (symbol-name key) 1)) value)
                result)))
      (nreverse result))))

(defun etaf-alist-to-plist (alist)
  "Convert an alist to a plist.
Example: ((class . \"foo\") (id . \"bar\"))
         => (:class \"foo\" :id \"bar\")"
  (let ((result nil))
    (dolist (pair alist)
      (push (intern (concat ":" (symbol-name (car pair)))) result)
      (push (cdr pair) result))
    (nreverse result)))

(provide 'etaf-utils)
