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

(defface etml-test-mono-face
  '((t :family "LXGW WenKai Mono"))
  "")

;; FIXME: emacs-kp
;; 1.不同字体的相同文本不应该命中缓存
;; 2.行尾个行首都不允许标点符号
;; 3.使用 nchar 也不能保持跨平台的展示完全一致

(defvar etml-box-tests-box-1 nil)
(elog-log-clear etml-box-logger)
(elog-set-level etml-box-logger 'debug)
(ekp-clear-caches)
(setq
 etml-box-tests-box-1
 (etml-box 
  :overflow-y 'auto
  :v-scroll-bar-direction 'left
  :v-scroll-offset 2
  :v-scroll-bar
  (etml-scroll-bar
   ;; :track-color "#999"
   :thumb-color "orange"
   :track-margin-left-pixel 2 :track-margin-right-pixel 2
   :track-padding-left-pixel 2 :track-padding-right-pixel 2
   :track-border-left-pixel 1 :track-border-right-pixel 1
   :track-border-top-p t  :track-border-bottom-p t
   ;; :track-border-left-color "green" :track-border-right-color "green"
   :thumb-pixel 8
   :thumb-border-p t
   :thumb-border-color "grey"
   )
  :content ;; (file-content "./text-zh-en_US.txt")
  (propertize
   (string-trim-right
    (file-content "~/IPARA/3-RESOURCES/emacs/config/github/ETML/tests/text-zh.txt"))
   'face 'etml-test-mono-face)
  :width 45
  :height 20
  :bgcolor nil
  :border-left-pixel 1 :border-right-pixel 1
  :border-top-p t :border-bottom-p t
  :border-bottom-style 'line
  ;; 'line 'double-line 'wave 'dots 'dashes
  :margin-left-pixel 10 :margin-right-pixel 10
  :margin-top-height 1 :margin-bottom-height 1
  :padding-left-pixel 14 :padding-right-pixel 14
  :padding-top-height .5 :padding-bottom-height .5
  ))

(etml-box-render "*etml-box-test*"
  etml-box-tests-box-1)
