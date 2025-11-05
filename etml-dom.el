(require 'dom)

(defun etml-dom-parse-html (html-string)
  "Parse HTML string into DOM tree (libxml-parse-html-region)."
  (with-temp-buffer
    (insert html-string)
    (libxml-parse-html-region (point-min) (point-max))))

(defun etml-dom-parse-sexp (sexp)
  "Parse SEXP into DOM tree."
  )

(defun etml-text-size (text face)
  "Measure width and height of TEXT in FACE."
  (save-window-excursion
    (let ((buffer (get-buffer-create "*temp-text-size*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (erase-buffer)
        (insert (propertize text 'face face))
        (let* ((width (car (window-text-pixel-size 
                            nil (point-min) (point-max))))
               (height (cdr (window-text-pixel-size 
                             nil (point-min) (point-max)))))
          (list width height))))))

(setq etml-test-node
      '(div ((width . 500) (height . 6))
            (p nil "happy hacking emacs")
            "this is text not in node"
            (div ((color . "red") (display . flex))
                 (button nil "OK")
                 (button nil "Cancel"))))
