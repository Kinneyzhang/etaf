;; -*- lexical-binding: t -*-

(defclass etml-element ()
  ((uuid :initarg :uuid :initform (org-id-uuid))
   (name :initarg :name :initform nil)
   (abbrev :initarg :abbrev :initform nil)
   (fmtstr :initarg :fmtstr)
   (value :initarg :value :initform nil)
   (separator :initarg :separator :initform nil)
   (style :initarg :style :initform nil)
   (watcher :initarg :watcher :initform nil))
  "")

(defvar etml-element-data nil
  "Store data of all etaf elememts.")

(defun etml--process-face (properties)
  "Process face in PROPERTIES. Replace etaf-faces with the real
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
             'null (append (mapcar #'etaf-property-face face-symbols)
                           (list face-kvs))))
      ;; FIXME: if property-face is nil, remove face property
      (setf (nth face-idx properties) property-face)
      ;; overlay-face
      (setq overlay-face
            (seq-remove 'null
                        (mapcar #'etaf-overlay-face face-symbols)))
      (when overlay-face
        (setq properties
              (append properties (list 'ov-face overlay-face)))))
    properties))

(defun etml-element--style-text (text-plist style-plist)
  ;; text-plist and style-plist are plists whose key match in :fmtstr
  (let (style-prop style-val)
    (while (and (setq style-prop (pop style-plist))
                (setq style-val (pop style-plist)))
      ;; text-prop and style-prop are the same
      (let* ((text-prop style-prop)
             (text-values (plist-get text-plist text-prop))
             (style-val (etml--process-face style-val)))
        (plist-put text-plist text-prop
                   (mapcar (lambda (value)
                             (apply #'propertize value style-val))
                           text-values))))
    text-plist))

(cl-defmethod etml-element-original-string ((elem etml-element))
  "Return formatted string of a `etml-element' object."
  (let* ((string (oref elem :fmtstr))
         (text-plist (oref elem :value))
         (style-plist (oref elem :style))
         (separator (oref elem :separator))
         (data (etml-element--style-text text-plist style-plist))
         ;; (type (oref elem :type))
         start repl-value)
    (save-match-data
      (while (string-match "{\\(.*?\\)}" string start)
        (let* ((name (match-string 1 string))
               (prop (string-to-keyword name))
               (values (etml-value (plist-get data prop))))
          (save-match-data (setq repl-value
                                 (string-join values separator)))
          (setq
           start
           (+ (match-end 0)
              (- (length repl-value)
                 (length (match-string-no-properties 0 string)))))
          (setq string (replace-match repl-value nil nil string)))))
    string))

(cl-defmethod etml-element--string ((elem etml-element))
  "Return formatted string of a `etml-element' object."
  (etml-element-original-string elem))

(defun etml-element-function (name)
  (intern (concat "etml-element-"
                  (symbol-name (etml-element-name name)))))

(defun etml-element-data-set (name key value)
  (if (assoc name etml-element-data)
      (setf (plist-get (cdr (assoc name etml-element-data))
                       key)
            value)
    (push (list name key value) etml-element-data))
  etml-element-data)

(defun etml-element-data-get (name key)
  (plist-get (cdr (assoc name etml-element-data)) key))

(defun etml-element-name-by-abbrev (abbrev)
  "Return etaf element name by it's abbrev."
  (when-let ((data (seq-find (lambda (data)
                               (string= 
                                (plist-get (cdr data) :abbrev)
                                abbrev))
                             etml-element-data)))
    (car data)))

;;;###autoload
(cl-defmacro define-etml-element (name arglist &rest body)
  (declare (indent defun))
  (let* ((curr-func (etml-element-function name))
         docstring parent-sexp (plist body))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq plist (cdr body)))
    (setq parent-sexp (plist-get plist :extend))
    ;; set etml-element-data
    (etml-element-data-set name :arglist arglist)
    (dolist (kv (plist->alist plist))
      (etml-element-data-set name (car kv) (cadr kv)))
    ;; remove :extend :inner in plist
    (setq plist (plist-remove-keys plist '(:extend :inner)))
    (if parent-sexp
        (progn
          (setcar parent-sexp (etml-element-function
                               (car parent-sexp)))      
          ;; 1.with extend: define a new element function run
          ;;  parent element function first, then set new kvs.
          ;; Eval parent element function first, then set new kvs
          ;;  in curr elem
          `(cl-defun ,curr-func (&key ,@arglist)
             ,docstring
             ;; (etml-element-base-text :text text)
             (let ((parent-obj ,parent-sexp))
               (etml-oset parent-obj ,@plist)
               parent-obj)))
      ;; 2.without extend: define a new element function,
      ;;  run etml-element with all attrs.
      `(cl-defun ,curr-func (&key ,@arglist)
         ,docstring
         (etml-element ,@plist)))))

(defun etml-element-name (name-or-abbrev)
  (if-let ((data (assoc name-or-abbrev etml-element-data)))
      (car data)
    (etml-element-name-by-abbrev name-or-abbrev)))

;;;###autoload
(defun etml-element-string (name &rest kvs)
  (let ((function (etml-element-function name)))
    (etml-element--string (apply function kvs))))

;;;###autoload
(defun etml-element-insert (name &rest kvs)
  "Insert etaf element named NAME, KV is it's properties."
  (let ((string (apply #'etml-element-string name kvs))
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

(defun etaf-make-region-editable (regions)
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (point-max) 'read-only t)
    (dolist (cons regions)
      (remove-text-properties (car cons) (cdr cons)
                              '(read-only nil)))))

(defun etml-element-parse (sexp)
  "Parse a sexp to etaf element."
  (let* ((elem-name (etml-element-name (car sexp)))
         (arglist (etml-element-data-get elem-name :arglist)))
    ;; inner args do not have to specify keyword.
    ;; if elem has only one arg, it's the inner arg in sexp
    ;; if elem has more than one arg, inner arg is specified by :inner
    (setcar sexp (etml-element-function (car sexp)))
    (let ((inner-key (symbol->keyword
                      (if (= (length arglist) 1)
                          (car arglist)
                        (etml-element-data-get elem-name :inner)))))
      ;; FIXME: support multiple inner??
      ;; FIXME: inner also support keyword
      (let ((heads (seq-drop-last sexp))
            (inner-val (car (last sexp))))
        (setq sexp (append heads (list inner-key inner-val)))))
    (etml-element--string (eval sexp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-etml-element base (text)
  "The base etaf element."
  :fmtstr "{1}"
  :value `(:1 (,text)))

(define-etml-element base-text (text)
  :abbrev "span"
  :extend (base :text text))

(define-etml-element paragraph (text)
  :abbrev "p"
  :extend (base-text :text text))

(define-etml-element headline
  (text (height 1.3) (bold t))
  :inner text
  :abbrev "h"
  :extend (base-text :text text)
  :style `(:1 (face (:weight ,(when bold 'bold) :height ,height))))

(define-etml-element base-input ( value name (size 20) maxlength
                                  disabled readonly autofocus
                                  placeholder pattern required)
  "VALUE: the value of input area.
SIZE: specifies the visible width of input area, the default value is 20.
MAXLENGTH: specifies the maximum number of characters allowed in an input field.
DISABLED: a boolean, could't submit this field data.
READONLY: a boolean, could submit this field data but could't edit.
AUTOFOCUS: automatically focus to this.
PLACEHOLDER: hint text in input area.
NAME: field name used to submit.
PATTERN: specifies a regular expression that the input field's value\
 is checked against, it works with the following input types:\
 text, date, search, url, tel, email, and password.
REQUIRED: specifies that an input field must be filled out before submitting the form"
  :fmtstr "{1}"
  :value `(:1 (,(concat
                 ;; (block-spaces 5)
                 (etaf-input-text (or value placeholder) size))))
  :style `(:1 (face ,(if disabled
                         'input-disabled
                       (if (and (null value) placeholder)
                           '(input-normal :foreground "grey")
                         'input-normal))
                    ,@(when readonly '(read-only t))
                    etaf-elem base-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-etml-element base-list
  (list (func '(lambda (_ it) (etml-value it))))
  "Apply function FUNC to each item of list LIST."
  :inner list
  :fmtstr "{1}"
  :separator "\n"
  :value `(:1 (-map-indexed (lambda (idx it)
                              (funcall ',func idx it))
                            (etml-value ',list))))

;; 考虑继承和设置父元素参数，同时设置个性化属性修改的可能性
(define-etml-element prefix-list
  (list prefix (idx-func '(lambda (idx) (ignore idx))))
  "LIST is a list of string, PREFIX is prefix of each item in LIST,
 IDX-FUNC is a funtion applied to PREFIX."
  :inner list
  :extend (base-list :list list
                     :func `(lambda (idx it)
                              (concat (funcall ',idx-func idx)
                                      ,prefix it))))

(define-etml-element plainlist (list)
  :extend (prefix-list :list list :prefix "- "))

(define-etml-element checklist (list)
  :extend (prefix-list :list list :prefix "□ "))

(define-etml-element orderlist (list)
  :extend
  (prefix-list :list list
               :idx-func (lambda (idx)
                           (concat (number-to-string (1+ idx)) "."))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-etml-element base-button ()
;;   :attrs ((text :doc "button text"))
;;   :fmtstr "{1}"
;;   :value `(:1 (,text))
;;   :abbrev button)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-etml-element newline ((n 1))
  :fmtstr "{1}"
  :value `(:1 (,(make-string n ?\n))))

(provide 'etml-element)
