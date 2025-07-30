(require 'ert)

(should
 (string=
  (etml-element-string 'base-text :text "happy hacking emacs!")
  "happy hacking emacs!"))

(should
 (string=
  (etml-element-string 'headline :text "happy hacking emacs!")
  "happy hacking emacs!"))

(should
 (string=
  (etml-element-string 'paragraph :text "happy hacking emacs!")
  "happy hacking emacs!"))

(should
 (string=
  (etml-element-string 'base-list :list '("emacs" "vim" "vscode"))
  "emacs
vim
vscode"))

(should
 (string=
  (let ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'base-list :list lst))
  "emacs
vim
vscode"))

(should
 (string=
  (let ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'base-list
                         :list lst
                         :func '(lambda (_ it) (upcase it))))
  "EMACS
VIM
VSCODE"))

(should
 (string=
  (let* ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'prefix-list
                         :list lst :prefix "- "))
  "- emacs
- vim
- vscode"))

(should
 (string=
  (let* ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'plainlist :list lst))
  "- emacs
- vim
- vscode"))

(should
 (string=
  (let* ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'checklist :list lst))
  "□ emacs
□ vim
□ vscode"))

(should
 (string=
  (let* ((lst '("emacs" "vim" "vscode")))
    (etml-element-string 'orderlist :list lst))
  "1.emacs
2.vim
3.vscode"))
