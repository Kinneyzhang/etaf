;;; etml-face.el
;;; 设置亮色或暗色主体下自适应的face，支持同时定义 text properties 和 overlay
;;; 1.overlay 属性的关键词以 :ov- 为前缀
;;; 2.使用定义的 etml face 渲染文本时，会先将 text properties 设置到文本中
;;; 3.在插入文本时，再将 overlay 设置到 buffer 的文本区域

(require 'etml-utils)

(defvar etml-face-list nil)

(defun etml-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun etml-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun etml-face--function (name)
  (intern (concat "etml-" (symbol-name name) "-face")))

(defun etml-face--ov-convert (key)
  "Convert a etml ov face to the real face keyword."
  (intern (concat ":" (string-trim-left (symbol-name key) ":ov-"))))

(defun etml-face--ov-p (key)
  (string-match ":ov-" (symbol-name key)))

(defun etml-face--parse (plist)
  "Parse etml face to a plist '(:text-properties ... :overlay ...).

 The value of :text-properties will be rendered by text properties.
 The value of :overlay will be rendered by overlay."
  (seq-mapcat
   (lambda (group)
     (let ((type (car group))
           (kv-alist (cdr group)))
       (setcdr
        group
        (list (etml-alist->plist
               (if (eq :overlay-face type)
                   (mapcar
                    (lambda (kv)
                      (setcar kv (etml-face--ov-convert (car kv)))
                      kv)
                    kv-alist)
                 kv-alist))))
       group))
   (seq-group-by (lambda (lst)
                   (if (etml-face--ov-p (car lst))
                       :overlay-face
                     :property-face))
                 (etml-plist->alist plist))))

(defun etml-face-p (etml-face)
  "Judge of ETML-FACE is in etml face type."
  (and (functionp (etml-face--function etml-face))
       (member etml-face etml-face-list)))

(defun etml-property-face (etml-face)
  "Return the value of ETML-FACE as face-text-properties
 if the face is in etml type, otherwise return the symbol
 of ETML-FACE as a normal face."
  (cond
   ((etml-face-p etml-face)
    (plist-get (funcall (etml-face--function etml-face))
               :property-face))
   ((facep etml-face) etml-face)
   (t (error "no such face: %S" etml-face))))

(defun etml-overlay-face (etml-face)
  "Return the value of ETML-FACE as overlay face if the
 face is in etml type."
  (when (etml-face-p etml-face)
    (plist-get (funcall (etml-face--function etml-face))
               :overlay-face)))

(defmacro define-etml-face (name &rest kvs)
  "Define a etml face."
  (declare (indent defun))
  (let ((func-name (etml-face--function name)))
    (add-to-list 'etml-face-list name)
    `(defun ,func-name ()
       (cond
        ((etml-theme-dark-p)
         (let ((lst (plist-get ',kvs :dark)))
           (etml-face--parse lst)))
        ((etml-theme-light-p)
         (let ((lst (plist-get ',kvs :light)))
           (etml-face--parse lst)))))))



;;; built-in etml faces

(define-etml-face input-normal
  :light (:ov-box (:color "#000"))
  :dark (:ov-box (:color "#fff")))

(define-etml-face input-disabled
  :light (:foreground "#aaa" :ov-box (:color "#000"))
  :dark (:foreground "#ccc" :ov-box (:color "#fff")))

;; (:color "#fff")

;; (define-etml-face input-normal
;;   :light (:ov-box (:color "#000"))
;;   :dark (:ov-box (:color "#fff")))

;; (etml-property-face 'input-disabled)
;; (etml-overlay-face 'input-disable)

;; (etml-property-face 'input-normal)
;; (etml-overlay-face 'input-normal)

(provide 'etml-face)
