;;; 将 etml 转为 dom

(defun etaf-tml-to-dom (sexp)
  "Convert S-expression from format 1 (plist) to format 2 (alist).
Format 1: (tag :attr1 val1 :attr2 val2 child1 child2 ...)
Format 2: (tag ((attr1 . val1) (attr2 . val2)) child1 child2 ...)"
  (cond
   ;; If it's an atom (string, number, etc.), return as is
   ((atom sexp) sexp)
   ;; If it's a list, process it
   (t (let ((tag (car sexp))
            (rest (cdr sexp))
            (attrs nil))
        (while (and rest (keywordp (car rest)))
          (push (car rest) attrs)
          (setq rest (cdr rest))
          (when rest
            (push (car rest) attrs)
            (setq rest (cdr rest))))
        (setq attrs (nreverse attrs))
        (let ((attr-alist (etaf-plist-to-alist attrs))
              (children (mapcar #'etaf-tml-to-dom rest)))
          (cons tag (cons attr-alist children)))))))

(provide 'etaf-tml)
;;; etaf-tml.el ends here
