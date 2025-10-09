;; 对 text properties 的封装



;; elisp 原生的 text properties 函数的局限性

(defun text-props-of-string (string)
  "获取 string 的所有 text properties"
  (object-intervals string))

(defun text-props-of-region (start end &optional buffer-or-name)
  (with-current-buffer
      (get-buffer-create (or buffer-or-name
                             (current-buffer)))
    (object-intervals (buffer-substring start end))))

(defun text-props-override ())

(defun text-props-add ()
  "没有增加，有则更新"
  )

(provide 'etml-props)

;; (:window param value)

(put-text-property
 1 10 'field "1" (get-buffer "test"))

;; (put-text-property
;;  1 10
;;  'face '(:filtered
;;          (:and
;;           (:frame background-mode dark)
;;           '(:foreground "#aaccff" :background "#222233")))
;;  (get-buffer "test"))


(put-text-property
 10 20 'field "2" (get-buffer "test"))

(put-text-property
 1 10 'face '(link :foreground "red") (get-buffer "test"))

(add-face-text-property
 1 6 '(:overline "green") nil (get-buffer "test"))

(add-text-properties
 12 20 '(face (button :overline "green")) (get-buffer "test"))

(remove-text-properties
 1 10 '(face nil) (get-buffer "test"))

(set-text-properties 1 20 nil (get-buffer "test"))

(window-parameters)
