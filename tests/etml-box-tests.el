(require 'etml-box)

(string-pixel-width "happy hacking emacs and vim") ;; pixel: 243
(etml-box-content-pixel
 (etml-box
  :content "happy hacking emacs and vim"
  :width '(200)
  :min-width '(50)
  :max-width '(500)))

(etml-box-content-pixel
 (etml-box
  :content "happy hacking emacs and vim" ;; nchar: 27
  :width 20
  :min-width 1
  :max-width 50))

(etml-box-side-pixel
 (etml-box
  :content "happy hacking emacs and vim"
  :width '(200)
  :min-width '(50)
  :max-width '(500)))

(defvar etml-box-tests-box-1
  (etml-box
   :overflow-y 'scroll
   :v-scroll-bar (etml-scroll-bar
                  :track-height 15 :track-color "#444"
                  :track-left-margin-pixel 1
                  :track-right-margin-pixel 1
                  :track-left-padding-pixel 2
                  :track-right-padding-pixel 2
                  :track-left-border-pixel 1
                  :track-right-border-pixel 1
                  :track-left-border-color "grey"
                  :track-right-border-color "grey"
                  :thumb-color "grey"
                  :thumb-pixel 10 :thumb-offset 3 :thumb-height 2
                  :thumb-border-p t :thumb-border-color "grey")
   :content (file-content "./text-zh-en_US-short.txt")
   :width '(410) :height 4 :bgcolor "#444"
   :border-left-pixel 1
   :border-right-pixel 1
   :border-top-p t :border-bottom-p t
   :padding-left-pixel 10 :padding-right-pixel 10
   ;; :padding-top-height 1
   ;; :padding-bottom-height 1
   :margin-left-pixel 10 :margin-right-pixel 10
   :margin-top-height 1 :margin-bottom-height 1
   ))

(pop-buffer-insert nil
  (etml-box-render etml-box-tests-box-1))
