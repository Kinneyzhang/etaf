(require 'etml-utils)
(require 'etml-face)
(require 'etml-type)

(defclass etml-element ()
  ((fmtstr :initarg :fmtstr :type string
           :documentation "format string of element")
   (data :initarg :data :initform nil
         :type (satisfies etml-type-plistp)
         :documentation "values used to format fmtstr.")
   (style :initarg :style :initform nil
          :type (satisfies etml-type-plistp)
          :documentation "")
   (separator :initarg :separator :initform ""
              :documentation "")
   (type :initarg :type :initform 'inline
         :documentation "block or inline"))
  :allow-nil-initform​​ t)

(defvar etml-element-definitions nil
  "Store definitions of all etml elememts.")

(defun etml-etml--process-face (properties)
  "Process face in PROPERTIES. Replace etml-faces with the real
 faces and set overlay face as a special text property 'ov-face."
  (when-let ((idx (seq-position properties 'face))
             (_ (not (cl-oddp idx))))
    (let* ((face-idx (1+ idx))
           (face-val (nth face-idx properties))
           ;; uniform format of face-val
           (face-val (if (symbolp face-val)
                         (list face-val)
                       face-val))
           (first-key (seq-find #'keywordp face-val))
           (face-symbols face-val)
           face-kvs property-face overlay-face)
      (when first-key
        (let ((first-key-idx (seq-position face-val first-key)))
          ;; do not process kvs in face
          (setq face-kvs (seq-subseq face-val first-key-idx))
          (setq face-symbols (seq-subseq face-val 0 first-key-idx))))
      ;; property-face
      (setq property-face
            (seq-remove
             'null (append (mapcar #'etml-property-face face-symbols)
                           face-kvs)))
      ;; FIXME: if property-face is nil, remove face property
      (setf (nth face-idx properties) property-face)
      ;; overlay-face
      (setq overlay-face
            (seq-remove 'null (mapcar #'etml-overlay-face face-symbols)))
      (when overlay-face
        (setq properties
              (append properties (list 'ov-face overlay-face)))))
    properties))

(defun etml-element-style-data (data-plist style-plist)
  ;; data-plist and style-plist are plists whose key match in :fmtstr
  ;; (elog-debug "data-plist:%S" data-plist)
  ;; (elog-debug "style-plist:%S" style-plist)
  (let (style-prop style-val)
    (while (and (setq style-prop (pop style-plist))
                (setq style-val (pop style-plist)))
      ;; text-prop and style-prop are the same
      (let* ((text-prop style-prop)
             (text-values (plist-get data-plist text-prop))
             (_ (elog-debug "style-val:%S" style-val))
             (style-val (etml-etml--process-face style-val)))
        (plist-put data-plist text-prop
                   (mapcar (lambda (value)
                             (apply #'propertize value style-val))
                           text-values))))
    data-plist))

(defun etml-element-string (elem)
  "Return formatted string of a `etml-element' object."
  (let* ((string (oref elem :fmtstr))
         (data-plist (oref elem :data))
         (style-plist (oref elem :style))
         (separator (oref elem :separator))
         (data (etml-element-style-data data-plist style-plist))
         (type (oref elem :type))
         start repl-value)
    (save-match-data
      (while (string-match "{\\(.*?\\)}" string start)
        (let* ((name (match-string 1 string))
               (prop (etml-string-to-keyword name))
               (values (etml-value (plist-get data prop)))
               ;; (values (-map-indexed
               ;;          (lambda (index value)
               ;;            (propertize value 'etml-index index))
               ;;          values))
               )
          (save-match-data
            (setq repl-value (string-join values separator)))
          (setq start
                (+ (match-end 0)
                   (- (length repl-value)
                      (length (match-string-no-properties 0 string)))))
          (setq string (replace-match repl-value nil nil string)))))
    (if (eq type 'block)
        (concat string "\n")
      string)))

(defun etml-element-function (name)
  (intern (concat "etml-element--elem-" (symbol-name name))))

(defun etml-element--set (name inner key value)
  (if (assoc name etml-element-definitions)
      (setf (plist-get (cddr (assoc name etml-element-definitions))
                       key)
            value)
    (push (list name inner key value) etml-element-definitions))
  etml-element-definitions)

;; (defun etml-element--get (name key)
;;   (plist-get (cdr (assoc name etml-element-definitions)) key))

;;;###autoload
(cl-defmacro etml-element-define (name inner keys &rest body)
  (declare (indent defun))
  (let* ((curr-func (etml-element-function name))
         docstring parent-sexp
         (plist body))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq plist (cdr body)))
    (setq parent-sexp (plist-get plist :inherit))
    ;; set etml-element-definitions
    (dolist (kv (etml-plist->alist plist))
      (etml-element--set name inner (car kv) (cadr kv)))
    ;; remove :inherit :inner in plist
    (setq plist (etml-plist-remove-keys plist '(:inherit)))
    (if parent-sexp
        (progn
          (setcar parent-sexp (etml-element-function
                               (car parent-sexp)))      
          ;; 1.with extend: define a new element function,
          ;; run parent element function first, then set new kvs.
          
          ;; eval parent element function first,
          ;; then set new kvs in curr elem
          `(cl-defun ,curr-func (,inner &key ,@keys)
             ,docstring
             ;; (etml-element-base-text :text text)
             (let ((parent-obj ,parent-sexp))
               (etml-oset parent-obj ,@plist)
               parent-obj)))
      ;; 2.without extend: define a new element function,
      ;; run etml-element with all attrs.
      `(cl-defun ,curr-func (,inner &key ,@keys)
         ,docstring
         (etml-element ,@plist)))))

;;;###autoload
(defun etml-element-render (name inner &rest kvs)
  (declare (indent defun))
  (let ((function (etml-element-function name)))
    (etml-element-string (apply function inner kvs))))

;;;###autoload
(defun etml-element-insert (name innner &rest kvs)
  "Insert etml element named NAME, KV is it's properties."
  (declare (indent defun))
  (let ((string (apply #'etml-element-render name innner kvs))
        (base-point (point)))
    (insert string)
    ;; process overlay faces if exists
    (when-let ((ov-faces (etml-get-ov-faces string)))
      (mapcar (lambda (ov-face)
                (let* ((beg (+ base-point (nth 0 ov-face)))
                       (end (+ base-point (nth 1 ov-face)))
                       (face (nth 2 ov-face))
                       (ov (make-overlay beg end)))
                  (overlay-put ov 'face face)))
              ov-faces))))

;;;###autoload
(defmacro etml-element-parse-1 (sexp)
  "Parse single sexp to etml element."
  `(let* ((lst ',sexp)
          (elem-name (car lst))
          (elem-func (etml-element-function elem-name))
          (inner (car (last lst)))
          (plist (seq-subseq lst 1 (1- (length lst)))))
     (apply 'etml-element-render elem-name inner plist)))

(defmacro etml-element-parse (sexp)
  "Parse single sexp to etml element."
  `(let* ((lst ',sexp)
          (head (car lst)))
     (if (consp head)
         (mapconcat (lambda (it)
                      (eval `(etml-element-parse-1 ,it)))
                    lst "")
       (etml-element-parse-1 ,sexp))))



;;; built-in elememts

(etml-element-define headline text (height bold)
  :fmtstr "{1}"
  :data `(:1 (,text))
  :style `(:1 (face ( :height ,height
                      ,@(when bold '(:weight bold)))))
  :type 'block)

(etml-element-define h1 text ((height 2.2) (bold t))
  :inherit (headline text :height height :bold bold))

(etml-element-define h2 text ((height 1.8) (bold t))
  :inherit (headline text :height height :bold bold))

(etml-element-define h3 text ((height 1.5) (bold t))
  :inherit (headline text :height height :bold bold))

(etml-element-define h4 text ((height 1.3) (bold t))
  :inherit (headline text :height height :bold bold))

(etml-element-define p text nil
  :fmtstr "{1}"
  :data `(:1 (,text))
  :type 'block)

(etml-element-define br num nil
  :fmtstr "{1}"
  :data `(:1 (,(make-string num ?\n))))

;; inline elememts

(etml-element-define inline text (style)
  :fmtstr "{1}"
  :data `(:1 (,text))
  :style `(:1 ,style)
  :type 'inline)

(etml-element-define span text ()
  :inherit (inline text))

(etml-element-define em text ()
  :inherit (inline text :style '(face (:slant italic))))

(etml-element-define mark text ((color "LightYellow"))
  :inherit (inline text :style
                   `(face ( :background ,color
                            :foreground "#000"))))

(etml-element-define small text ((height 0.85))
  :inherit (inline text :style `(face (:height ,height))))

(etml-element-define u text (color style)
  "Supported STYLE are: 'line, double-line, 'wave, 'dots, or 'dashes."
  :inherit (inline text :style
                   `(face (:underline
                           ,(append
                             '(:position t)
                             (cond
                              ((and color style)
                               `(:color ,color :style ,style))
                              (color `(:color ,color))
                              (style `(:style ,style))))))))

(etml-element-define b text ()
  :inherit (inline text :style `(face (:weight bold))))

(etml-element-define del text (color)
  :inherit (inline text :style
                   `(face (:strike-through ,(or color t)))))

;; input elememts

(etml-element-define base-input value ( name (size 20) max-length
                                        disabled readonly autofocus
                                        placeholder pattern required)
  "VALUE: the value of input area. SIZE: specifies the visible width\
 of input area, the default value is 20.

MAX-LENGTH: specifies the maximum number of characters\
 allowed in an input field.

DISABLED: a boolean, could't submit this field data.
READONLY: a boolean, could submit this field data but could't edit.

AUTOFOCUS: automatically focus to this. PLACEHOLDER: hint text in input area.

NAME: field name used to submit. PATTERN: specifies a regular expression that\
 the input field's value is checked against, it works with the following input types:\
 text, date, search, url, tel, email, and password.

REQUIRED: specifies that an input field must be filled out before submitting the form"
  :fmtstr "{1}"
  :data `(:1 (,(concat
                (etml-input-text (or value placeholder) size))))
  :style `(:1 (face ,(if disabled
                         'input-disabled
                       (if (and (null value) placeholder)
                           '(input-normal :foreground "grey")
                         'input-normal))
                    ,@(when (or disabled readonly)
                        '(read-only t))
                    etml-element base-input)))

(etml-element-define base-list list (prefix orderp)
  "LIST is a list of string, PREFIX is prefix of each item in LIST,
 IDX-FUNC is a funtion applied to PREFIX."
  :fmtstr "{1}"
  :separator "\n"
  :type 'block
  :data `(:1 (seq-map-indexed
              (lambda (elt idx)
                (concat (if ,orderp
                            (concat (number-to-string (1+ idx))
                                    ".")
                          ,prefix)
                        elt))
              (etml-value ',list))))

(etml-element-define ul list nil
  :inherit (base-list list :prefix "• "))

(etml-element-define ol list nil
  :inherit (base-list list :orderp t))

(etml-element-define checklist list nil
  "checklist"
  :inherit (base-list list :prefix "□ "))

(provide 'etml-element)
