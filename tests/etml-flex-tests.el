(require 'etml-flex)

(defun etml-flex-tests-block1 (num)
  (etml-block :content (format "etml test block etml test block etml test block etml test happy hacking emacs and vim ...%s" num)
              :width '(200)
              :padding t
              :border t))

(defun etml-flex-tests-block2 (num)
  (etml-block :content (format "test block etml test block etml test %s" num)
              :width '(200)
              :padding t
              :border t))

;; (pop-buffer-insert 15
;;   (etml-block-render (etml-flex-tests-block 1)) " "
;;   (etml-block-render (etml-flex-tests-block 2)) " "
;;   (etml-block-render (etml-flex-tests-block 3)) " "
;;   (etml-block-render (etml-flex-tests-block 4)))

(defun etml-flex-tests-item1 (num)
  (etml-flex-item
   :self (etml-flex-tests-block1 num)
   :basis 200))

(defun etml-flex-tests-item2 (num)
  (etml-flex-item
   :self (etml-flex-tests-block2 num)
   :basis 300))

(defun etml-flex-test-line-pixel ()
  (save-excursion
    (goto-char (point-min))
    (string-pixel-width
     (buffer-substring (line-beginning-position)
                       (line-end-position)))))

(defun etml-flex-test-render ()
  (progn
    (elog-log-clear)
    (pop-buffer-insert 15
      (etml-flex-render
       (etml-flex
        :width '(490)
        :wrap 'wrap
        :height 10
        :column-gap 20
        ;; :row-gap 1
        :content-justify 'center
        :items-align 'center
        :items (list (etml-flex-tests-item1 1)
                     (etml-flex-tests-item2 2)
                     (etml-flex-tests-item1 3)
                     (etml-flex-tests-item2 4)))))))

(etml-flex-test-render)

;; (let ((flex (etml-flex
;;              :width '(1200)
;;              :height 10
;;              :column-gap 20
;;              :content-justify 'space-evenly
;;              :items (list (etml-flex-tests-item 1)
;;                           (etml-flex-tests-item 2)
;;                           (etml-flex-tests-item 3)
;;                           (etml-flex-tests-item 4)))))
;;   (etml-flex-cross-units flex))
