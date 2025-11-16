(require 'etml-element)

(pop-buffer-insert nil
  (etml-element-render 'h1 "headline")
  (etml-element-render 'h2 "headline")
  (etml-element-render 'h3 "headline")
  (etml-element-render 'h4 "headline")
  (etml-element-render 'span "|this is span1|")
  (etml-element-render 'br 1)
  (etml-element-render 'em "<em>this is span</em>")
  (etml-element-render 'br 1)
  (etml-element-render 'mark "<mark>this is span</mark>")
  (etml-element-render 'br 1)
  (etml-element-render 'small "<small>this is span</small>")
  (etml-element-render 'br 1)
  (etml-element-render 'u "happy hacking emacs~ ")
  (etml-element-render 'u "happy hacking emacs~ "
    :color "darkOrange")
  (etml-element-render 'u "happy hacking emacs~ "
    :style 'dashes
    :color "orange")
  (etml-element-render 'u "happy hacking emacs~ "
    :style 'dots
    :color "orange")
  (etml-element-render 'u "happy hacking emacs~ "
    :style 'double-line
    :color "orange")
  (etml-element-render 'u "happy hacking emacs~ "
    :style 'wave
    :color "orange")
  (etml-element-render 'br 1)
  (etml-element-render 'b "happy hacking emacs")
  (etml-element-render 'br 1)
  (etml-element-render 'del "happy hacking emacs")
  (etml-element-render 'del "happy hacking emacs"
    :color "red")
  (etml-element-render 'p "\nhappy hacking emacs and vim!")
  (etml-element-render 'ol '("emacs" "vim" "vscode"))
  (etml-element-render 'ul '("emacs" "vim" "vscode"))
  (etml-element-render 'checklist '("emacs" "vim" "vscode")))

(pop-buffer-do nil
  (insert "before: ")
  (etml-element-insert 'base-input nil
    ;; :readonly t
    ;; :disabled t
    :placeholder "input text here.."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pop-buffer-insert nil
  (etml-element-parse
   ((h1 "headline") (h2 "headline") (h3 "headline") (h4 "headline")
    (span "|this is span1|") (br 1)
    (em "<em>this is span</em>") (br 1)
    (mark "<mark>this is span</mark>") (br 1)
    (small "<small>this is span</small>") (br 1)
    (u "happy hacking emacs~ ")
    (u :color "darkOrange" "happy hacking emacs~ ")
    (u :style 'dashes :color "orange" "happy hacking emacs~ ")
    (u :style 'dots :color "orange" "happy hacking emacs~ ")
    (u :style 'double-line :color "orange" "happy hacking emacs~ ")
    (u :style 'wave :color "orange" "happy hacking emacs~ ") (br 1)
    (b "happy hacking emacs") (br 1)
    (del "happy hacking emacs")
    (del :color "red" "happy hacking emacs")
    (p "\nhappy hacking emacs and vim!")
    (ol ("emacs" "vim" "vscode"))
    (ul ("emacs" "vim" "vscode"))
    (checklist ("emacs" "vim" "vscode")))))
