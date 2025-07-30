(defun etml-get-ov-faces (string)
  (seq-remove
   'null
   (mapcar (lambda (data)
             (when-let* ((properties (nth 2 data))
                         (ov-face (plist-get properties 'ov-face)))
               (setf (nth 2 data) ov-face)
               data))
           (get-text-properties string))))

(defun plist->alist (plist)
  (unless (null plist)
    (append
     (list (list (pop plist) (pop plist)))
     (plist->alist plist))))

(defun alist->plist (alist)
  (mapcan (lambda (elem)
            (cons (car elem) (cdr elem)))
          alist))

(defun plist-remove-keys (plist1 keys)
  "Remove all element of PLIST2 from PLIST1."
  (let ((alist1 (plist->alist plist1)))
    (alist->plist
     (seq-remove (lambda (elem)
                   (member (car elem) keys))
                 alist1))))

(defun etml-oset (object &rest kvs)
  (let ((alist (plist->alist kvs)))
    (dolist (kv alist)
      (eval `(oset ,object ,(car kv) ',(cadr kv))))))

(defun string-to-keyword (string)
  (intern (concat ":" string)))

(defun etml-value (format)
  (pcase format
    ((and (pred listp)
          (let head (car format))
          (guard (or (functionp head)
                     ;; (eq 'lambda head)
                     )))
     (eval format)
     ;; (apply format)
     )
    ((pred symbolp) (symbol-value format))
    (_ format)))
