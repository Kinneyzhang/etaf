(require 'etml-test) ;; FIXME: restore window configuration after quit window

(defun etml-flex-tests-item (num &rest kvs)
  (eval `(etml-flex-item
          :content (etml-block :content (format "etml flex test %s" num))
          :border '("#aaaaaa" . "lightGreen")
          :padding t
          ,@kvs)))

;; (ekp-clear-caches)
(etml-test-pop-buffer "*etml-flex-tests*"
  (insert
   (etml-flex-string
    :border t :padding '(4 . 1) :margin '(3 . 1)
    ;; :bgcolor '("#FFF9F0" . "#222222")
    :direction 'column
    :content
    (list (etml-flex-item :content "Test etml-flex :shrink")
          (etml-flex-item :content "")
          (etml-flex-item
           :content
           ;; FIXME: etml-flex
           (etml-flex-string
            :width '(300)
            :column-gap 8
            :content (list (etml-flex-tests-item 1 :shrink 1)
                           (etml-flex-tests-item 2 :shrink 1)
                           (etml-flex-tests-item 3 :shrink 1)
                           (etml-flex-tests-item 4 :shrink 1)
                           (etml-flex-tests-item 5 :shrink 1)))
           ;; (etml-block
           ;;  :content
           ;;  (etml-flex-string
           ;;   :width '(300)
           ;;   :column-gap 8
           ;;   :content (list (etml-flex-tests-item 1 :shrink 1)
           ;;                  (etml-flex-tests-item 2 :shrink 1)
           ;;                  (etml-flex-tests-item 3 :shrink 1)
           ;;                  (etml-flex-tests-item 4 :shrink 1)
           ;;                  (etml-flex-tests-item 5 :shrink 1))))
           )
          ))))

;; `(div :display 'flex
;;       :border t :padding '(4 . 1) :margin '(3 . 1)
;;       :direction 'column
;;       (div "Test etml-flex :shrink")
;;       (div "")
;;       (div (div :display 'flex :width '(300) :column-gap 8
;;                 ,@(mapcar
;;                    (lambda (i)
;;                      `(div :border '("#aaaaaa" . "lightGreen")
;;                            :padding t
;;                            ,(format "etml flex test %s" i)))
;;                    (number-sequence 1 5)))))

;; test shrink

;; test grow

;; test order

;; test basis

;; test cross-align

;; test direction

;; test wrap

(provide 'etml-flex-tests)
