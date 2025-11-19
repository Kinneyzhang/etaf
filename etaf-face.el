;;; etaf-face.el
;;; 设置亮色或暗色主体下自适应的face，支持同时定义 text properties 和 overlay
;;; 1.overlay 属性的关键词以 :ov- 为前缀
;;; 2.使用定义的 etaf face 渲染文本时，会先将 text properties 设置到文本中
;;; 3.在插入文本时，再将 overlay 设置到 buffer 的文本区域

(require 'etaf-utils)

(defvar etaf-face-list nil)

(defun etaf-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun etaf-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun etaf-face--function (name)
  (intern (concat "etaf-" (symbol-name name) "-face")))

(defun etaf-face--ov-convert (key)
  "Convert a etaf ov face to the real face keyword."
  (intern (concat ":" (string-trim-left (symbol-name key) ":ov-"))))

(defun etaf-face--ov-p (key)
  (string-match ":ov-" (symbol-name key)))

(defun etaf-face--parse (plist)
  "Parse etaf face to a plist '(:text-properties ... :overlay ...).

 The value of :text-properties will be rendered by text properties.
 The value of :overlay will be rendered by overlay."
  (seq-mapcat
   (lambda (group)
     (let ((type (car group))
           (kv-alist (cdr group)))
       (setcdr
        group
        (list (etaf-alist->plist
               (if (eq :overlay-face type)
                   (mapcar
                    (lambda (kv)
                      (setcar kv (etaf-face--ov-convert (car kv)))
                      kv)
                    kv-alist)
                 kv-alist))))
       group))
   (seq-group-by (lambda (lst)
                   (if (etaf-face--ov-p (car lst))
                       :overlay-face
                     :property-face))
                 (etaf-plist->alist plist))))

(defun etaf-face-p (etaf-face)
  "Judge of ETAF-FACE is in etaf face type."
  (and (functionp (etaf-face--function etaf-face))
       (member etaf-face etaf-face-list)))

(defun etaf-property-face (etaf-face)
  "Return the value of ETAF-FACE as face-text-properties
 if the face is in etaf type, otherwise return the symbol
 of ETAF-FACE as a normal face."
  (cond
   ((etaf-face-p etaf-face)
    (plist-get (funcall (etaf-face--function etaf-face))
               :property-face))
   ((facep etaf-face) etaf-face)
   (t (error "no such face: %S" etaf-face))))

(defun etaf-overlay-face (etaf-face)
  "Return the value of ETAF-FACE as overlay face if the
 face is in etaf type."
  (when (etaf-face-p etaf-face)
    (plist-get (funcall (etaf-face--function etaf-face))
               :overlay-face)))

(defmacro define-etaf-face (name &rest kvs)
  "Define a etaf face."
  (declare (indent defun))
  (let ((func-name (etaf-face--function name)))
    (add-to-list 'etaf-face-list name)
    `(defun ,func-name ()
       (cond
        ((etaf-theme-dark-p)
         (let ((lst (plist-get ',kvs :dark)))
           (etaf-face--parse lst)))
        ((etaf-theme-light-p)
         (let ((lst (plist-get ',kvs :light)))
           (etaf-face--parse lst)))))))



;;; built-in etaf faces

(define-etaf-face input-normal
                  :light (:ov-box (:color "#000"))
                  :dark (:ov-box (:color "#fff")))

(define-etaf-face input-disabled
                  :light (:foreground "#aaa" :ov-box (:color "#000"))
                  :dark (:foreground "#ccc" :ov-box (:color "#fff")))

;; (:color "#fff")

;; (define-etaf-face input-normal
;;   :light (:ov-box (:color "#000"))
;;   :dark (:ov-box (:color "#fff")))

;; (etaf-property-face 'input-disabled)
;; (etaf-overlay-face 'input-disable)

;; (etaf-property-face 'input-normal)
;; (etaf-overlay-face 'input-normal)

(provide 'etaf-face)
