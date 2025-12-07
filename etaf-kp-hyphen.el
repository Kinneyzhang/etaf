;;; etaf-kp-hyphen.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar etaf-kp-hyphen--hdcache (make-hash-table :test 'equal))

(defvar etaf-kp-hyphen--languages (make-hash-table :test 'equal))

(defvar etaf-kp-hyphen--languages-lowercase (make-hash-table :test 'equal))

(defconst etaf-kp-hyphen--ignored
  '("%" "#" "LEFTHYPHENMIN" "RIGHTHYPHENMIN"
    "COMPOUNDLEFTHYPHENMIN" "COMPOUNDRIGHTHYPHENMIN"))

(cl-defstruct (etaf-kp-hyphen--datint
               (:constructor etaf-kp-hyphen--make-datint))
  value data)

(cl-defstruct (etaf-kp-hyphen--altparser
               (:constructor etaf-kp-hyphen--make-altparser))
  change index cut)

(cl-defstruct (etaf-kp-hyphen (:constructor etaf-kp-hyphen--make))
  hd left right)

(cl-defstruct (etaf-kp-hyphen--hyphdict
               (:constructor etaf-kp-hyphen--make-hyphdict))
  patterns cache maxlen)

(defun etaf-kp-hyphen--parse-hex (s)
  "Replace ^^hh with the corresponding char in S."
  (replace-regexp-in-string
   "\\^\\^\\([0-9a-fA-F][0-9a-fA-F]\\)"
   (lambda (m) (string (string-to-number (match-string 1 m) 16)))
   s))

(defun etaf-kp-hyphen--parse (pat)
  "Parse pattern string PAT to list of (digit string, non-digit string)."
  (let ((pos 0)
        (len (length pat))
        res)
    (while (< pos len)
      (let* ((digit (if (and (< pos len) (>= (aref pat pos) ?0)
                             (<= (aref pat pos) ?9))
                        (prog1 (string (aref pat pos)) (cl-incf pos))
                      ""))
             (ndigit (if (and (< pos len) (or (< (aref pat pos) ?0)
                                              (> (aref pat pos) ?9)))
                         (prog1 (string (aref pat pos)) (cl-incf pos))
                       "")))
        (push (list digit ndigit) res)))
    (nreverse res)))

(defun etaf-kp-hyphen--language-path (language)
  "Get a fallback language available in our dictionaries for string LANGUAGE."
  (let* ((parts (split-string
                 (downcase (replace-regexp-in-string "-" "_" language)) "_"))
         (found nil))
    (or (gethash language etaf-kp-hyphen--languages)
        (progn
          (while (and parts (not found))
            (let ((lang (mapconcat #'identity parts "_")))
              (setq found (gethash lang etaf-kp-hyphen--languages-lowercase))
              (pop parts)))
          found))))

(defun etaf-kp-hyphen--altparser-create (pattern alternative)
  "Create an AlternativeParser struct from PATTERN and ALTERNATIVE."
  (let* ((alt (split-string alternative ","))
         (change (nth 0 alt))
         (index (string-to-number (nth 1 alt)))
         (cut (string-to-number (nth 2 alt))))
    (if (string-prefix-p "." pattern)
        (cl-incf index))
    (etaf-kp-hyphen--make-altparser :change change :index index :cut cut)))

(defun etaf-kp-hyphen--altparser-call (altparser val)
  "Call ALTPARSER with value VAL."
  (let ((index (cl-decf (etaf-kp-hyphen--altparser-index altparser)))
        (v (string-to-number val)))
    (if (cl-oddp v)
        (etaf-kp-hyphen--make-datint
         :value v
         :data (list (etaf-kp-hyphen--altparser-change altparser)
                     index
                     (etaf-kp-hyphen--altparser-cut altparser)))
      v)))

(defun etaf-kp-hyphen--read-dic-file (path)
  "Read a .dic file from PATH. Return (encoding . lines-list)."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((encoding (buffer-substring-no-properties
                     (point) (line-end-position))))
      (forward-line 1)
      (cons encoding
            (split-string (buffer-substring-no-properties
                           (point) (point-max))
                          "\n" t)))))

(defun etaf-kp-hyphen--make-hyphdict-from-path (path)
  "Build a HyphDict structure from a .dic file at PATH."
  (let* ((file (etaf-kp-hyphen--read-dic-file path))
         (encoding (car file))
         (lines (cdr file))
         (patterns (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let* ((p (string-trim line)))
        (unless (or (string-empty-p p)
                    (cl-some (lambda (ig) (string-prefix-p ig p))
                             etaf-kp-hyphen--ignored))
          (setq p (etaf-kp-hyphen--parse-hex p))
          (let* ((factory
                  (if (and (string-match "/" p) (string-match "=" p))
                      (let* ((split (split-string p "/" t))
                             (pattern (car split))
                             (alternative (cadr split)))
                        (lambda (i)
                          (etaf-kp-hyphen--altparser-call
                           (etaf-kp-hyphen--altparser-create pattern alternative)
                           (or i "0"))))
                    #'string-to-number))
                 (pattern (if (and (string-match "/" p) (string-match "=" p))
                              (car (split-string p "/" t))
                            p))
                 (tags-values
                  (mapcar (lambda (pr) (list (cadr pr)
                                             (funcall factory (car pr))))
                          (etaf-kp-hyphen--parse pattern))))
            (let ((tags (mapcar #'car tags-values))
                  (values (mapcar #'cadr tags-values)))
              (unless (= (apply #'max (mapcar
                                       (lambda (v)
                                         (if (integerp v) v
                                           (etaf-kp-hyphen--datint-value v)))
                                       values))
                         0)
                (let ((start 0) (end (length values)))
                  (while (and (< start end) (equal (elt values start) 0))
                    (cl-incf start))
                  (while (and (> end start) (equal (elt values (1- end)) 0))
                    (cl-decf end))
                  (puthash (apply #'concat tags)
                           (cons start (cl-subseq values start end))
                           patterns))))))))
    (let* ((maxlen (apply #'max (mapcar #'length
                                        (hash-table-keys patterns)))))
      (etaf-kp-hyphen--make-hyphdict :patterns patterns
                                 :cache (make-hash-table :test 'equal)
                                 :maxlen maxlen))))

(defun etaf-kp-hyphen--hyphdict-positions (hd word)
  "Get a list of positions where WORD can be hyphenated, using HyphDict HD.
Returns a list of etaf-kp-hyphen--datint objects or ints."
  (let* ((w (downcase word))
         (cache (etaf-kp-hyphen--hyphdict-cache hd))
         (points (gethash w cache)))
    (unless points
      (let* ((pointed-word (concat "." w "."))
             (references (make-list (+ (length pointed-word) 1) 0)))
        (cl-loop
         for i from 0 below (1- (length pointed-word)) do
         (let ((stop (min (+ i (etaf-kp-hyphen--hyphdict-maxlen hd))
                          (length pointed-word))))
           (cl-loop
            for j from (1+ i) to stop do
            (let ((pattern
                   (gethash (substring pointed-word i j)
                            (etaf-kp-hyphen--hyphdict-patterns hd))))
              (when pattern
                (let* ((offset (car pattern))
                       (vals (cdr pattern))
                       (slice-start (+ i offset))
                       (slice-end (+ i offset (length vals))))
                  (cl-loop for k from slice-start below slice-end
                           for v in vals
                           do (when (and (<= 0 k)
                                         (< k (length references)))
                                (setf (nth k references)
                                      (max v (nth k references)))))))))))
        (let ((res nil))
          (cl-loop for i from 0 below (length references)
                   for reference in references
                   when (cl-oddp (if (etaf-kp-hyphen--datint-p reference)
                                     (etaf-kp-hyphen--datint-value reference)
                                   reference))
                   do (push (if (etaf-kp-hyphen--datint-p reference)
                                reference
                              (etaf-kp-hyphen--make-datint :value (- i 1)))
                            res))
          (setq points (nreverse res))
          (puthash w points cache))))
    points))

(defun etaf-kp-hyphen-load-languages (dict-dir)
  "Scan DICT-DIR for hyphenation dictionaries and populate
 `etaf-kp-hyphen--languages'."
  (dolist (file (directory-files dict-dir t "\\.dic\\'"))
    (let ((name (replace-regexp-in-string "\\(^hyph_\\|\\.dic$\\)" ""
                                          (file-name-nondirectory file))))
      (puthash name file etaf-kp-hyphen--languages)
      (let ((short (car (split-string name "_"))))
        (unless (gethash short etaf-kp-hyphen--languages-lowercase)
          (puthash short file etaf-kp-hyphen--languages-lowercase))))))

(defun etaf-kp-hyphen-create (&optional lang filename left right cache)
  "Create a etaf-kp-hyphen object. Prefer LANG, otherwise FILENAME.
LEFT and RIGHT are minimum first/last syllable chars. CACHE t/nil."
  (let* ((left (or left 2))
         (right (or right 2))
         (cache (if (null cache) t cache))
         (path (cond (lang (etaf-kp-hyphen--language-path lang))
                     (filename filename))))
    (unless path
      (error "No dictionary found for language or filename"))
    (let ((hd (or (and cache (gethash path etaf-kp-hyphen--hdcache))
                  (let ((dict (etaf-kp-hyphen--make-hyphdict-from-path path)))
                    (puthash path dict etaf-kp-hyphen--hdcache)
                    dict))))
      (etaf-kp-hyphen--make :hd hd :left left :right right))))

(defun etaf-kp-hyphen-positions (etaf-kp-hyphen word)
  "Get hyphenation positions for WORD, using EKP-HYPHEN."
  (let* ((hd (etaf-kp-hyphen-hd etaf-kp-hyphen))
         (left (etaf-kp-hyphen-left etaf-kp-hyphen))
         (right (- (length word) (etaf-kp-hyphen-right etaf-kp-hyphen))))
    (cl-remove-if-not (lambda (i)
                        (and (<= left (etaf-kp-hyphen--datint-value i))
                             (<= (etaf-kp-hyphen--datint-value i) right)))
                      (etaf-kp-hyphen--hyphdict-positions hd word))))

;; (defun etaf-kp-hyphen-inserted (etaf-kp-hyphen word &optional hyphen)
;;   "Return WORD with all possible hyphens inserted."
;;   (let ((hyphen (or hyphen "-"))
;;         (letters (string-to-list word)))
;;     (dolist (pos (reverse (etaf-kp-hyphen-positions etaf-kp-hyphen word)))
;;       (let ((idx (etaf-kp-hyphen--datint-value pos)))
;;         (if (etaf-kp-hyphen--datint-data pos)
;;             (let* ((data (etaf-kp-hyphen--datint-data pos))
;;                    (change (nth 0 data))
;;                    (index (+ (nth 1 data) idx))
;;                    (cut (nth 2 data))
;;                    (changestr (replace-regexp-in-string "=" hyphen change)))
;;               (setq letters (append (cl-subseq letters 0 index)
;;                                     (string-to-list changestr)
;;                                     (cl-subseq letters (+ index cut)))))
;;           (setq letters (append (cl-subseq letters 0 idx)
;;                                 (string-to-list hyphen)
;;                                 (cl-subseq letters idx))))))
;;     (concat "" (mapconcat #'char-to-string letters ""))))

(defun etaf-kp-hyphen-inserted (etaf-kp-hyphen word &optional hyphen)
  "Return WORD with all possible hyphens inserted, preserving
text properties."
  (let ((hyphen (or hyphen "-"))
        (result word))
    (dolist (pos (reverse (etaf-kp-hyphen-positions etaf-kp-hyphen word)))
      (let ((idx (etaf-kp-hyphen--datint-value pos)))
        (if (etaf-kp-hyphen--datint-data pos)
            (let* ((data (etaf-kp-hyphen--datint-data pos))
                   (change (nth 0 data))
                   (index (+ (nth 1 data) idx))
                   (cut (nth 2 data))
                   (changestr (replace-regexp-in-string "=" hyphen change)))
              (setq result (concat (substring result 0 index)
                                   changestr
                                   (substring result (+ index cut)))))
          (setq result (concat (substring result 0 idx)
                               hyphen
                               (substring result idx))))))
    result))

(defun etaf-kp-hyphen-boxes (etaf-kp-hyphen word)
  (split-string (etaf-kp-hyphen-inserted etaf-kp-hyphen word " ") " "))

(provide 'etaf-kp-hyphen)

;; (defun etaf-kp-hyphen-iterate (etaf-kp-hyphen word)
;;   "Yield all hyphenation possibilities for WORD, longest first."
;;   (let ((positions (reverse (etaf-kp-hyphen-positions etaf-kp-hyphen word)))
;;         res)
;;     (dolist (pos positions)
;;       (let ((idx (etaf-kp-hyphen--datint-value pos)))
;;         (if (etaf-kp-hyphen--datint-data pos)
;;             (let* ((data (etaf-kp-hyphen--datint-data pos))
;;                    (change (nth 0 data))
;;                    (index (+ (nth 1 data) idx))
;;                    (cut (nth 2 data))
;;                    (wordstr (if (string= word (upcase word))
;;                                 (upcase change)
;;                               change))
;;                    (c1 (car (split-string wordstr "=")))
;;                    (c2 (cadr (split-string wordstr "="))))
;;               (push (cons (concat (substring word 0 index) c1)
;;                           (concat c2 (substring word (+ index cut))))
;;                     res))
;;           (push (cons (substring word 0 idx)
;;                       (substring word idx))
;;                 res))))
;;     res))

;; (defun etaf-kp-hyphen-wrap (etaf-kp-hyphen word width &optional hyphen)
;;   "Return (first-part . last-part) for WORD, where first-part
;; is <= WIDTH with hyphen."
;;   (let ((hyphen (or hyphen "-"))
;;         (poss (etaf-kp-hyphen-iterate etaf-kp-hyphen word)))
;;     (setq width (- width (length hyphen)))
;;     (catch 'found
;;       (dolist (pair poss)
;;         (when (<= (length (car pair)) width)
;;           (throw 'found (cons (concat (car pair) hyphen)
;;                               (cdr pair))))))))

(provide 'etaf-kp-hyphen)
