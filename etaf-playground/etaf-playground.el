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

;; Try to load etaf, gracefully handle if not available
(defvar etaf-playground--etaf-available nil
  "Whether full ETAF rendering is available.")

(condition-case nil
    (progn
      (require 'etaf)
      (require 'etaf-ecss)
      (setq etaf-playground--etaf-available t))
  (error
   (message "ETAF not fully available, playground will show structure only")))

;;; Customization

(defgroup etaf-playground nil
  "ETAF Playground customization."
  :group 'etaf
  :prefix "etaf-playground-")

(defcustom etaf-playground-default-width 600
  "Default width for rendering the ETML content."
  :type 'integer
  :group 'etaf-playground)

(defcustom etaf-playground-default-height nil
  "Default height for rendering the ETML content.
When nil, height is not constrained."
  :type '(choice (const :tag "Auto" nil)
                 (integer :tag "Fixed height"))
  :group 'etaf-playground)

;;; Buffer names

(defconst etaf-playground-main-buffer "*ETAF Playground*"
  "Main playground buffer name.")

(defconst etaf-playground-output-buffer "*ETAF Playground Output*"
  "Output buffer name for rendered content.")

;;; Default content

(defconst etaf-playground-default-etml
  "(html
  (head
    (style \"{{ css }}\"))
  (body
    (div :class \"container\"
      (h1 \"{{ title }}\")
      (p \"{{ message }}\")
      (ul :e-if \"items\"
        (li :e-for \"item in items\" \"{{ item }}\")))))"
  "Default ETML template for the playground.")

(defconst etaf-playground-default-css
  ".container {
  padding-left: 20px;
  padding-right: 20px;
  padding-top: 16px;
  padding-bottom: 16px;
  border-top-width: 1px;
  border-right-width: 1px;
  border-bottom-width: 1px;
  border-left-width: 1px;
}
h1 {
  color: #2563eb;
  margin-bottom: 12px;
}
p {
  color: #374151;
  margin-bottom: 16px;
}
ul {
  margin-top: 8px;
}
li {
  margin-bottom: 4px;
}"
  "Default CSS content for the playground.")

(defconst etaf-playground-default-elisp
  ";; Define data to use in the template
;; This code is evaluated to produce a plist
;; that will be passed as template data

'(:title \"Welcome to ETAF Playground\"
  :message \"Edit the ETML, CSS, and code below, then click Run!\"
  :items (\"Feature 1: ETML templates\"
          \"Feature 2: CSS styling\"
          \"Feature 3: Dynamic data\"))"
  "Default Elisp code for the playground.")

;;; Internal state

(defvar-local etaf-playground--etml-content nil
  "Current ETML content in the playground.")

(defvar-local etaf-playground--css-content nil
  "Current CSS content in the playground.")

(defvar-local etaf-playground--elisp-content nil
  "Current Elisp code content in the playground.")

(defvar-local etaf-playground--etml-start nil
  "Start position of ETML input area.")

(defvar-local etaf-playground--etml-end nil
  "End position of ETML input area.")

(defvar-local etaf-playground--css-start nil
  "Start position of CSS input area.")

(defvar-local etaf-playground--css-end nil
  "End position of CSS input area.")

(defvar-local etaf-playground--elisp-start nil
  "Start position of Elisp input area.")

(defvar-local etaf-playground--elisp-end nil
  "End position of Elisp input area.")

;;; Helper functions

(defun etaf-playground--make-section-header (title)
  "Create a section header with TITLE."
  (propertize (format "━━━ %s ━━━\n" title)
              'face '(:weight bold :foreground "#6366f1")
              'read-only t
              'front-sticky t
              'rear-nonsticky t))

(defun etaf-playground--make-editable-area (content id)
  "Create an editable text area with CONTENT and ID."
  (let ((start (point)))
    (insert content)
    (let ((end (point)))
      (insert "\n")
      ;; Store the marker positions
      (list (copy-marker start) (copy-marker end)))))

(defun etaf-playground--make-button (label action)
  "Create a button with LABEL that executes ACTION when clicked."
  (insert-text-button label
                      'action action
                      'follow-link t
                      'face '(:background "#2563eb"
                              :foreground "white"
                              :weight bold
                              :box (:line-width 2 :style released-button))))

(defun etaf-playground--get-area-content (start-marker end-marker)
  "Get content between START-MARKER and END-MARKER."
  (when (and start-marker end-marker
             (marker-buffer start-marker)
             (marker-buffer end-marker))
    (buffer-substring-no-properties
     (marker-position start-marker)
     (marker-position end-marker))))

(defun etaf-playground--safe-read (str)
  "Safely read STR as Emacs Lisp expression."
  (condition-case err
      (read str)
    (error
     (message "Error parsing: %s" (error-message-string err))
     nil)))

(defun etaf-playground--safe-eval (expr)
  "Safely evaluate EXPR."
  (condition-case err
      (eval expr t)
    (error
     (message "Error evaluating: %s" (error-message-string err))
     nil)))

;;; Rendering

(defun etaf-playground--render ()
  "Render the current playground content."
  (interactive)
  (with-current-buffer (get-buffer etaf-playground-main-buffer)
    (let* ((etml-str (etaf-playground--get-area-content
                      etaf-playground--etml-start
                      etaf-playground--etml-end))
           (css-str (etaf-playground--get-area-content
                     etaf-playground--css-start
                     etaf-playground--css-end))
           (elisp-str (etaf-playground--get-area-content
                       etaf-playground--elisp-start
                       etaf-playground--elisp-end))
           (etml (etaf-playground--safe-read etml-str))
           (elisp-expr (etaf-playground--safe-read elisp-str))
           (data (when elisp-expr
                   (etaf-playground--safe-eval elisp-expr)))
           ;; Add CSS to data for template interpolation
           (data-with-css (if data
                              (plist-put (copy-sequence data) :css css-str)
                            (list :css css-str))))
      (etaf-playground--show-output etml data-with-css css-str))))

(defun etaf-playground--show-output (etml data css)
  "Show rendered output for ETML with DATA and CSS."
  (let ((output-buffer (get-buffer-create etaf-playground-output-buffer)))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert (propertize "━━━ Rendered Output ━━━\n\n"
                            'face '(:weight bold :foreground "#10b981")))
        ;; Render content
        (if (and etaf-playground--etaf-available etml)
            (condition-case err
                (let ((rendered-string
                       (etaf-string etml data nil
                                    etaf-playground-default-width
                                    etaf-playground-default-height)))
                  (insert rendered-string))
              (error
               (insert (propertize "Render Error:\n"
                                   'face '(:foreground "#ef4444" :weight bold)))
               (insert (format "%s\n" (error-message-string err)))
               (insert "\n--- ETML Structure ---\n")
               (insert (pp-to-string etml))
               (when data
                 (insert "\n--- Data ---\n")
                 (insert (pp-to-string data)))))
          ;; Fallback: show structure
          (insert (propertize "(ETAF not available, showing structure)\n\n"
                              'face '(:foreground "#f59e0b")))
          (when etml
            (insert "--- ETML ---\n")
            (insert (pp-to-string etml))
            (insert "\n"))
          (when data
            (insert "--- Data ---\n")
            (insert (pp-to-string data))
            (insert "\n"))
          (when css
            (insert "--- CSS ---\n")
            (insert css)))
        ;; Footer
        (insert "\n\n")
        (insert (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
                            'face '(:foreground "#6b7280")))
        (insert (format "Rendered at: %s\n" (format-time-string "%H:%M:%S"))))
      (goto-char (point-min)))
    ;; Make sure output is visible
    (display-buffer output-buffer)))

;;; Main UI

(defun etaf-playground--create-ui ()
  "Create the playground UI in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Main title
    (insert (propertize "╔════════════════════════════════════════════════════════════╗\n"
                        'face '(:foreground "#6366f1")))
    (insert (propertize "║           🎮 ETAF Playground - Interactive Editor          ║\n"
                        'face '(:foreground "#6366f1" :weight bold)))
    (insert (propertize "╚════════════════════════════════════════════════════════════╝\n\n"
                        'face '(:foreground "#6366f1")))
    
    ;; Instructions
    (insert (propertize "Edit the ETML, CSS, and Elisp code below, then click "
                        'face '(:foreground "#6b7280")))
    (insert (propertize "[Run]" 'face '(:foreground "#2563eb" :weight bold)))
    (insert (propertize " or press "
                        'face '(:foreground "#6b7280")))
    (insert (propertize "C-c C-c"
                        'face '(:foreground "#2563eb" :weight bold)))
    (insert (propertize " to render.\n"
                        'face '(:foreground "#6b7280")))
    (insert (propertize "Press " 'face '(:foreground "#6b7280")))
    (insert (propertize "C-c C-r" 'face '(:foreground "#2563eb" :weight bold)))
    (insert (propertize " to reset to defaults.\n\n"
                        'face '(:foreground "#6b7280")))
    
    ;; Run button
    (etaf-playground--make-button "  ▶ Run  "
                                   (lambda (_)
                                     (etaf-playground--render)))
    (insert "  ")
    (etaf-playground--make-button "  ↺ Reset  "
                                   (lambda (_)
                                     (etaf-playground--reset)))
    (insert "\n\n")
    
    ;; ETML section
    (insert (etaf-playground--make-section-header "ETML Structure"))
    (let ((markers (etaf-playground--make-editable-area
                    etaf-playground-default-etml
                    'etml)))
      (setq etaf-playground--etml-start (car markers))
      (setq etaf-playground--etml-end (cadr markers)))
    (insert "\n")
    
    ;; CSS section
    (insert (etaf-playground--make-section-header "CSS Styles"))
    (let ((markers (etaf-playground--make-editable-area
                    etaf-playground-default-css
                    'css)))
      (setq etaf-playground--css-start (car markers))
      (setq etaf-playground--css-end (cadr markers)))
    (insert "\n")
    
    ;; Elisp section
    (insert (etaf-playground--make-section-header "Elisp Data Code"))
    (let ((markers (etaf-playground--make-editable-area
                    etaf-playground-default-elisp
                    'elisp)))
      (setq etaf-playground--elisp-start (car markers))
      (setq etaf-playground--elisp-end (cadr markers)))
    (insert "\n")
    
    ;; Footer
    (insert (propertize "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
                        'face '(:foreground "#6b7280")
                        'read-only t))
    (insert (propertize "Tips: The Elisp code should return a plist for template data.\n"
                        'face '(:foreground "#9ca3af")
                        'read-only t))
    (insert (propertize "      Use {{ key }} in ETML to interpolate data values.\n"
                        'face '(:foreground "#9ca3af")
                        'read-only t)))
  (goto-char (point-min)))

(defun etaf-playground--reset ()
  "Reset playground to default content."
  (interactive)
  (when (get-buffer etaf-playground-main-buffer)
    (with-current-buffer etaf-playground-main-buffer
      (etaf-playground--create-ui)
      (message "Playground reset to defaults."))))

;;; Mode definition

(defvar etaf-playground-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'etaf-playground--render)
    (define-key map (kbd "C-c C-r") #'etaf-playground--reset)
    map)
  "Keymap for `etaf-playground-mode'.")

(define-derived-mode etaf-playground-mode fundamental-mode "ETAF-Playground"
  "Major mode for ETAF Playground.

\\{etaf-playground-mode-map}"
  (setq buffer-read-only nil)
  ;; Enable font-lock for syntax highlighting in editable areas
  (setq-local font-lock-defaults nil))

;;; Entry point

;;;###autoload
(defun etaf-playground ()
  "Launch the ETAF Playground.

The playground provides an interactive environment for experimenting
with ETAF's ETML templates, CSS styles, and dynamic data.

The interface consists of:
- ETML Structure: Write your template markup
- CSS Styles: Define styles for your elements
- Elisp Data Code: Provide dynamic data for the template

Press \\[etaf-playground--render] or click [Run] to see the rendered output."
  (interactive)
  (let ((buffer (get-buffer-create etaf-playground-main-buffer)))
    (with-current-buffer buffer
      (etaf-playground-mode)
      (etaf-playground--create-ui))
    ;; Display buffers side by side
    (delete-other-windows)
    (switch-to-buffer buffer)
    ;; Create output buffer and display it on the right
    (let ((output-buffer (get-buffer-create etaf-playground-output-buffer)))
      (with-current-buffer output-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "━━━ Rendered Output ━━━\n\n"
                              'face '(:weight bold :foreground "#10b981")))
          (insert "Click [Run] or press C-c C-c to render your code.\n"))
        (setq buffer-read-only t))
      (split-window-right)
      (other-window 1)
      (switch-to-buffer output-buffer)
      (other-window 1))
    (message "ETAF Playground ready! Edit and press C-c C-c to render.")))

(provide 'etaf-playground)
;;; etaf-playground.el ends here
