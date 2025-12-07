;;; etaf-playground.el --- Interactive ETAF Playground -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: etml, css, playground
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; ETAF Playground is an interactive development environment for experimenting
;; with ETAF (Emacs Text-based Application Framework).
;;
;; The interface is divided into two columns:
;; - Left side: Input panels (ETML, CSS, Elisp code)
;; - Right side: Rendered output
;;
;; Similar to MDN Playground (https://developer.mozilla.org/en-US/play),
;; users can edit ETML structure, CSS styles, and Elisp code to dynamically
;; update the UI.
;;
;; Usage:
;;   M-x etaf-playground     ; Launch the playground
;;
;; In the playground:
;;   - Edit ETML structure in the first input area
;;   - Edit CSS styles in the second input area
;;   - Edit Elisp code in the third input area (for dynamic data)
;;   - Press the "Run" button or C-c C-c to render the result

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-ecss)

;;; Customization

(defgroup etaf-playground nil
  "ETAF Playground customization."
  :group 'etaf
  :prefix "etaf-playground-")

(defcustom etaf-playground-input-width 400
  "Width for the input panel."
  :type 'integer
  :group 'etaf-playground)

(defcustom etaf-playground-output-width 500
  "Width for the output panel."
  :type 'integer
  :group 'etaf-playground)

;;; Buffer names

(defconst etaf-playground-buffer "*ETAF Playground*"
  "Playground buffer name.")

;;; Internal state

(defvar etaf-playground-etml-content nil
  "Current ETML content.")

(defvar etaf-playground-css-content nil
  "Current CSS content.")

(defvar etaf-playground-elisp-content nil
  "Current Elisp content.")

(defvar etaf-playground-output-content nil
  "Current rendered output content.")

(defvar etaf-playground-error-message nil
  "Current error message if any.")

;;; Default content

(defconst etaf-playground-etml-string
  "(div :class \"demo\"
  (p \"{{ title }}\")
  (p \"{{ message }}\")
  (ul :e-if \"items\"
    (li :e-for \"item in items\" 
        \"{{ item }}\")))"
  "Default ETML template.")

(defconst etaf-playground-css-string
  ".demo {
  padding-left: 16px;
  padding-right: 16px;
  padding-top: 1lh;
  padding-bottom: 1lh;
}
h1 {
  color: #2563eb;
  margin-bottom: 1lh;
}"
  "Default CSS styles.")

(defconst etaf-playground--default-elisp
  "'(:title \"Hello ETAF!\"
  :message \"This is a playground demo.\"
  :items (\"Item 1\" \"Item 2\" \"Item 3\"))"
  "Default Elisp data code.")

;;; ECSS styles for playground UI

;;; Helper functions

(defun etaf-playground--safe-read (str)
  "Safely read STR as Emacs Lisp expression."
  (condition-case err
      (read str)
    (error
     (setq etaf-playground-error-message
           (format "Parse error: %s" (error-message-string err)))
     nil)))

(defun etaf-playground--safe-eval (expr)
  "Safely evaluate EXPR and return result.
Note: This function evaluates arbitrary Elisp code provided by the user.
This is intentional for a playground environment where users experiment
with their own code. The evaluation is wrapped in condition-case to
catch and report errors gracefully.

The second argument `t' to `eval' enables lexical binding for the
evaluated expression."
  (condition-case err
      (eval expr t)  ; t enables lexical binding
    (error
     (setq etaf-playground-error-message
           (format "Eval error: %s" (error-message-string err)))
     nil)))

(defun etaf-playground--render-user-content ()
  "Render user's ETML with their CSS and data."
  (setq etaf-playground-error-message nil)
  (let* ((etml-sexp (etaf-playground--safe-read etaf-playground-etml-content))
         (data-expr (etaf-playground--safe-read etaf-playground-elisp-content))
         (data (when data-expr (etaf-playground--safe-eval data-expr))))
    (if etaf-playground-error-message
        (setq etaf-playground-output-content nil)
      (condition-case err
          (let* ((css etaf-playground-css-content)
                 ;; Wrap user ETML with html/head/style structure
                 (full-etml `(html
                              (head
                               (style ,css))
                              (body ,etml-sexp)))
                 (rendered (etaf-paint-string full-etml data nil
                                              etaf-playground-output-width nil)))
            (setq etaf-playground-output-content rendered))
        (error
         (setq etaf-playground-error-message
               (format "Render error: %s" (error-message-string err)))
         (setq etaf-playground-output-content nil))))))

;;; Build playground ETML

(defun etaf-playground--build-etml ()
  "Build the playground UI as ETML structure."
  ;; Note: Theme-aware Tailwind classes (with dark: variants) must be placed
  ;; in element class attributes, NOT in ECSS style rules. ECSS processes
  ;; Tailwind classes at parse time using the current theme, resulting in
  ;; theme-specific CSS that doesn't adapt when the theme changes. In contrast,
  ;; class attributes are processed by etaf-css-compute-style-for-node-dual-mode
  ;; which uses etaf-tailwind-classes-to-css-dual-mode to generate both light
  ;; and dark mode styles, allowing automatic theme switching.
  `(div
    (ecss "#pannel-input > div {border-x border-t border-gray-500}"
          "#pannel-input > div > p {pl-2 italic}"
          "#pannel-input > div > div {px-2 py-1}")
    (div :class "ml-2 mt-1"
         (div :class "flex justify-between w-50"
              (h2 :class "underline" "ETAF Playground")
              (div (button "Format") " "
                   (button "Run") " "
                   (button "Clear")))
         (div :class "flex w-200 mt-1"
              (div :id "pannel-input" :class "w-50"
                   (ecss "div:nth-child(2){color:red}")
                   (div (p :class "bg-green-700 dark:bg-gray-600 text-white dark:text-rose-400" "ETML Structure")
                        (div (p ,etaf-playground-etml-content)))
                   (div (p :class "bg-green-700 dark:bg-gray-600 text-white dark:text-rose-400" "CSS Styles")
                        (div (p ,etaf-playground-css-content)))
                   (div :class "border-b border-gray-500"
                        (p :class "bg-green-700 dark:bg-gray-600 text-white dark:text-rose-400" "Elisp Data")
                        (div (p ,etaf-playground-elisp-content))))
              (div :id "pannel-output"
                   :class "ml-2 w-100 border border-gray-500 px-1"
                   ,(if etaf-playground-error-message
                        `(div :class ""
                              (div "Error:")
                              (div ,etaf-playground-error-message))
                      `(div :class ""
                            ,(or etaf-playground-output-content
                                 "(Click Run or press C-c C-c to render)"))))))))

;;; Render playground

(defun etaf-playground--refresh ()
  "Refresh the playground display."
  (let ((buffer (get-buffer-create etaf-playground-buffer)))
    (with-current-buffer buffer
      (etaf-layout-caches-init)
      (let ((inhibit-read-only t)
            (etml (etaf-playground--build-etml)))
        (erase-buffer)
        (insert (etaf-paint-string etml)))
      (goto-char (point-min))
      (setq buffer-read-only t))
    buffer))

(defun etaf-playground--run ()
  "Run/render the current playground content."
  (interactive)
  (etaf-playground--render-user-content)
  (etaf-playground--refresh)
  (message "Playground rendered."))

(defun etaf-playground--reset ()
  "Reset playground to default content."
  (interactive)
  (setq etaf-playground-etml-content etaf-playground-etml-string)
  (setq etaf-playground-css-content etaf-playground-css-string)
  (setq etaf-playground-elisp-content etaf-playground--default-elisp)
  (setq etaf-playground-output-content nil)
  (setq etaf-playground-error-message nil)
  (etaf-playground--refresh)
  (message "Playground reset to defaults."))

(defun etaf-playground--edit-etml ()
  "Edit ETML content in a separate buffer."
  (interactive)
  (let ((edit-buffer (get-buffer-create "*ETAF Playground - Edit ETML*")))
    (with-current-buffer edit-buffer
      (erase-buffer)
      (insert etaf-playground-etml-content)
      (emacs-lisp-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (setq etaf-playground-etml-content (buffer-string))
                       (kill-buffer)
                       (etaf-playground--run))))
    (switch-to-buffer-other-window edit-buffer)
    (message "Edit ETML. Press C-c C-c to save and render.")))

(defun etaf-playground--edit-css ()
  "Edit CSS content in a separate buffer."
  (interactive)
  (let ((edit-buffer (get-buffer-create "*ETAF Playground - Edit CSS*")))
    (with-current-buffer edit-buffer
      (erase-buffer)
      (insert etaf-playground-css-content)
      (css-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (setq etaf-playground-css-content (buffer-string))
                       (kill-buffer)
                       (etaf-playground--run))))
    (switch-to-buffer-other-window edit-buffer)
    (message "Edit CSS. Press C-c C-c to save and render.")))

(defun etaf-playground--edit-elisp ()
  "Edit Elisp content in a separate buffer."
  (interactive)
  (let ((edit-buffer (get-buffer-create "*ETAF Playground - Edit Elisp*")))
    (with-current-buffer edit-buffer
      (erase-buffer)
      (insert etaf-playground-elisp-content)
      (emacs-lisp-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (setq etaf-playground-elisp-content (buffer-string))
                       (kill-buffer)
                       (etaf-playground--run))))
    (switch-to-buffer-other-window edit-buffer)
    (message "Edit Elisp data. Press C-c C-c to save and render.")))

(defun etaf-playground--edit ()
  "Open edit menu to choose what to edit."
  (interactive)
  (let ((choice (read-char-choice
                 "Edit: [1] ETML  [2] CSS  [3] Elisp  [q] Cancel: "
                 '(?1 ?2 ?3 ?q))))
    (pcase choice
      (?1 (etaf-playground--edit-etml))
      (?2 (etaf-playground--edit-css))
      (?3 (etaf-playground--edit-elisp))
      (?q (message "Cancelled.")))))

;;; Mode definition

(defvar etaf-playground-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'etaf-playground--run)
    (define-key map (kbd "C-c C-r") #'etaf-playground--reset)
    (define-key map (kbd "C-c C-e") #'etaf-playground--edit)
    (define-key map (kbd "1") #'etaf-playground--edit-etml)
    (define-key map (kbd "2") #'etaf-playground--edit-css)
    (define-key map (kbd "3") #'etaf-playground--edit-elisp)
    (define-key map (kbd "r") #'etaf-playground--run)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `etaf-playground-mode'.")

(define-derived-mode etaf-playground-mode special-mode "ETAF-Playground"
  "Major mode for ETAF Playground.

Key bindings:
\\{etaf-playground-mode-map}"
  (setq buffer-read-only t))

;;; Entry point

;;;###autoload
(defun etaf-playground ()
  "Launch the ETAF Playground.

The playground provides an interactive environment for experimenting
with ETAF's ETML templates, CSS styles, and dynamic data.

Key bindings:
  C-c C-c  Run/render the playground
  C-c C-e  Edit content (choose ETML/CSS/Elisp)
  C-c C-r  Reset to defaults
  1        Edit ETML
  2        Edit CSS
  3        Edit Elisp
  r        Run/render
  q        Quit"
  (interactive)
  ;; Initialize content
  (setq etaf-playground-etml-content etaf-playground-etml-string)
  (setq etaf-playground-css-content etaf-playground-css-string)
  (setq etaf-playground-elisp-content etaf-playground--default-elisp)
  (setq etaf-playground-output-content nil)
  (setq etaf-playground-error-message nil)
  ;; Render playground UI immediately without rendering user content
  ;; This improves startup performance - users can trigger rendering with C-c C-c
  (let ((buffer (etaf-playground--refresh)))
    (switch-to-buffer buffer)
    (etaf-playground-mode)
    (message "ETAF Playground. Press C-c C-c to render, C-c C-e to edit, q to quit.")))

(provide 'etaf-playground)
;;; etaf-playground.el ends here
