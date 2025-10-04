(require 'etml-scroll-bar)

(pop-buffer-insert 30
  (etml-lines-concat
   (list (etml-scroll-bar-render
          (etml-scroll-bar
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
           :thumb-border-p t :thumb-border-color "grey"))
         "  "
         (etml-scroll-bar-render
          (etml-scroll-bar
           :track-height 15 :track-color "#888"
           :track-left-padding-pixel 1
           :track-right-padding-pixel 1
           :thumb-pixel 3 :thumb-offset 3 :thumb-height 2
           ))
         "  "
         (etml-scroll-bar-render
          (etml-scroll-bar
           :track-height 15
           :track-left-padding-pixel 0
           :track-right-padding-pixel 0
           :track-left-border-pixel 1
           :track-right-border-pixel 1
           ;; :thumb-color "orange"
           :thumb-pixel 2 :thumb-offset 3 :thumb-height 2
           ))
         "  "
         (etml-scroll-bar-render
          (etml-scroll-bar
           :track-height 15
           ;; :track-left-padding-pixel 0
           ;; :track-right-padding-pixel 0
           :track-left-border-pixel 1
           :track-right-border-pixel 1
           ;; :thumb-color "orange"
           :thumb-pixel 2 :thumb-height 2
           ))
         )))
