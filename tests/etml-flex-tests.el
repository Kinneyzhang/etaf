(require 'etml-test) ;; FIXME: restore window configuration after quit window

(defun etml-flex-tests-item (num &rest kvs)
  (eval `(etml-flex-item
          :content (etml-box :content ,(format "etml flex test %s" num))
          :border-left-color "#aaaaaa"
          :padding-left-pixel 1
          ,@kvs)))

;; FIXME: etml-flex
;; (ekp-clear-caches)
(etml-test-pop-buffer "*etml-flex-tests*"
  ;; (elog-log-clear)
  ;; :border t :padding '(4 . 1) :margin '(3 . 1)
  (insert
   (etml-flex-string
    (etml-flex
     :margin-top-height 1
     :margin-left-pixel 10
     :padding-left-pixel 4
     :padding-right-pixel 4
     :border-left-pixel 1
     :border-right-pixel 1
     :border-top-p t :border-bottom-p t
     :direction 'column
     :content-justify 'center
     :height 10
     :content
     (list (etml-flex-item
            :content "Test etml-flex :shrink")
           (etml-flex-item
            :content "---------------------")
           (etml-flex-item
            :content
            ;; (etml-box :content "cwece")
            (etml-flex
             :width '(400) :column-gap 8
             :content (list (etml-flex-tests-item 1 :shrink 1 :grow 1)
                            (etml-flex-tests-item 2 :shrink 1)
                            (etml-flex-tests-item 3 :shrink 1)
                            (etml-flex-tests-item 4 :shrink 1)))
            ))))))

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
