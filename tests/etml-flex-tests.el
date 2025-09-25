(require 'etml-test) ;; FIXME: restore window configuration after quit window

(defun etml-flex-tests-item (num &rest kvs)
  (eval `(etml-flex-item
          :self (etml-block :content (format "etml flex test %s" num))
          :border '("#aaaaaa" . "lightGreen")
          :padding t
          ,@kvs)))

;; (etml-test-pop-buffer "*etml-flex-tests*"
;;   (insert
;;    (etml-flex-string
;;     :border t :padding '(4 . 1) :margin '(3 . 1)
;;     :bgcolor '("#FFF9F0" . "#222222")
;;     :items (list (etml-flex-item
;;                   :self (etml-block :content "test etml-flex shrink"))
;;                  (etml-flex-item
;;                   :self
;;                   (etml-flex
;;                    :width '(300)
;;                    :items (list (etml-flex-tests-item 1 :shrink 1)
;;                                 (etml-flex-tests-item 2 :shrink 1)
;;                                 (etml-flex-tests-item 3 :shrink 1)
;;                                 (etml-flex-tests-item 4 :shrink 1)
;;                                 (etml-flex-tests-item 5 :shrink 1))))))))

;; test shrink

;; test grow

;; test order

;; test basis

;; test cross-align

;; test direction

;; test wrap

(provide 'etml-flex-tests)
