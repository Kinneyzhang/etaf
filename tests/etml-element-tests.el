(pop-buffer-insert 20
  (etml-element-render 'h1 "headline")
  (etml-element-render 'h2 "headline")
  (etml-element-render 'h3 "headline")
  (etml-element-render 'h4 "headline")
  (etml-element-render 'span "|this is span1|")
  (etml-element-render 'em "<em>this is span</em>")
  (etml-element-render 'mark "<mark>this is span</mark>")
  (etml-element-render 'small "<small>this is span</small>")
  (etml-element-render 'p "happy hacking emacs and vim!")
  (etml-element-render 'ol '("emacs" "vim" "vscode"))
  (etml-element-render 'ul '("emacs" "vim" "vscode"))
  (etml-element-render 'pl '("emacs" "vim" "vscode"))
  (etml-element-render 'cl '("emacs" "vim" "vscode"))
  )

(etml-value '("emacs" "vim" "vscode"))

(pop-buffer-do 20
  (insert "before: ")
  (etml-element-insert 'base-input nil
    ;; :readonly t
    ;; :disabled t
    :placeholder "input text here.."))

;; (etml-etml--process-face
;;  '(face (link :foreground "red" :weight bold)))

;; (etml-etml--process-face
;;  '(face (:foreground "red" :weight bold)))

;; (etml-etml--process-face
;;  '(face link))
