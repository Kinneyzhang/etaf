(defun floating-scrollbar--update ()
  "Update the scrollbar's position and size."
  (when (and (frame-visible-p (selected-frame))
             (window-live-p (selected-window)))
    (let* ((window (selected-window))
           (buffer (window-buffer window))
           (window-height (window-height window))
           (window-width (window-width window))
           (total-lines (with-current-buffer buffer
                          (count-lines (point-min) (point-max))))
           (window-start-pos (window-start window))
           (window-end-pos (window-end window t))
           (visible-lines (with-current-buffer buffer
                            (condition-case nil
                                (count-lines window-start-pos window-end-pos)
                              (error window-height))))
           (start-percent (/ (float (with-current-buffer buffer
                                      (count-lines (point-min) window-start-pos)))
                             (max 1.0 total-lines)))
           (height-percent (/ (float visible-lines) (max 1.0 total-lines)))
           (thumb-start (round (* start-percent window-height)))
           (thumb-size (max 2 (round (* height-percent window-height))))
           (thumb-end (+ thumb-start thumb-size))
           (thumb-string (floating-scrollbar--create-thumb-string 
                          window-height thumb-start (min window-height thumb-end))))
      (posframe-show floating-scrollbar-buffer-name
                     :string thumb-string
                     :position (cons (- window-width 1) 0)
                     :poshandler 'posframe-poshandler-window-right
                     :width floating-scrollbar-width
                     :height window-height
                     :internal-border-width 0
                     :override-parameters '((cursor-type . nil))))))

(floating-scrollbar--update)
