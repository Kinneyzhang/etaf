;;; etml-div 是 etml-flex etml-flex-item etml-box 的结合

;; https://github.com/radkovo/CSSBox
;; https://htmlspecs.com/dom/
;; https://lexbor.com/documentation/
;; https://lexbor.com/roadmap/

;; flex, grid, block, inline
(defclass etml-div (etml-flex etml-flex-item)
  ((display :initarg :display
            :initform 'block
            :documentation "块类型")))

(defun etml-div-string (div)
  (let ((display )))
  )

(mapcar (lambda (el)
          (aref el 1))
        (eieio-class-slots 'etml-div))
