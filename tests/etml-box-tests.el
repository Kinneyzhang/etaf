;; -*- lexical-binding: t; -*-

(require 'etml-box)

;; (string-pixel-width "happy hacking emacs and vim") ;; pixel: 243
;; (etml-box-content-pixel
;;  (etml-box
;;   :content "happy hacking emacs and vim"
;;   :width '(200)
;;   :min-width '(50)
;;   :max-width '(500)))

;; (etml-box-content-pixel
;;  (etml-box
;;   :content "happy hacking emacs and vim" ;; nchar: 27
;;   :width 20
;;   :min-width 1
;;   :max-width 50))

;; (etml-box-side-pixel
;;  (etml-box
;;   :content "happy hacking emacs and vim"
;;   :width '(200)
;;   :min-width '(50)
;;   :max-width '(500)))

(defvar etml-box-tests-box-1 nil)
(elog-log-clear etml-box-logger)
(elog-set-level etml-box-logger 'debug)
(ekp-clear-caches)
(etml-render "*etml-box-test*"
  :overflow-y 'auto
  :v-scroll-bar-direction 'left
  :v-scroll-offset 0
  :v-scroll-bar
  (etml-scroll-bar
   ;; :track-color "#999"
   :thumb-color "orange"
   :track-margin-left-pixel 0 :track-margin-right-pixel 0
   :track-padding-left-pixel 1 :track-padding-right-pixel 1
   :track-border-left-pixel 1 :track-border-right-pixel 1
   ;; :track-border-left-color "green" :track-border-right-color "green"
   :thumb-pixel 8
   :thumb-border-p nil
   :thumb-border-color "grey"
   )
  :content ;; (file-content "./text-zh-en_US.txt")
  (propertize (string-trim-right (file-content "./text-zh.txt"))
              'face 'etml-test-mono-face)
  :width 30
  :height 20
  :bgcolor nil
  :border-left-pixel 0 :border-right-pixel 1
  :border-top-p t :border-bottom-p t
  :border-bottom-style 'line
  ;; 'line 'double-line 'wave 'dots 'dashes
  :margin-left-pixel 10 :margin-right-pixel 10
  :margin-top-height 1 :margin-bottom-height 1
  :padding-left-pixel 14 :padding-right-pixel 14
  :padding-top-height 1.5 :padding-bottom-height 1.5
  )
