(defconst etaf-log-level-list '(trace debug info warning error)
  "All levels of emacs log.")

(defconst etaf-log-default-buffer "*etaf-log*"
  "Default log buffer.")

(defconst etaf-log-default-level 'debug
  "Default log level.")

(defvar etaf-log-default-level nil
  "Current level of etaf-log, level higher than
this level will be shown in `etaf-log-default-buffer'.")

;; (defvar etaf-log-max-log-num 10000)

;;; utils

(defun format-time-millis ()
  "返回当前时间的字符串表示，精确到毫秒。"
  (let* ((now (float-time))
         (seconds (floor now))
         (millis (floor (* (- now seconds) 1000))))
    (concat (format-time-string "%H:%M:%S" seconds)
            "." (format "%03d" millis))))

;;; functions

(defun etaf-logger (&optional level buffer)
  (list :level (or level etaf-log-default-level)
        :buffer (or buffer etaf-log-default-buffer)))

(defun etaf-log-set-level (logger level)
  (setq logger (plist-put logger :level level)))

(defun etaf-log-set-buffer (logger buffer)
  (setq logger (plist-put logger :buffer buffer)))

(defun etaf-log (logger level format-string args)
  (with-current-buffer (get-buffer-create (plist-get logger :buffer))
    (local-set-key "q" 'etaf-log-quit)
    (goto-char (point-max))
    (when-let ((allowed-level (plist-get logger :level)))
      (let ((allowed-levels (member allowed-level etaf-log-level-list))
            (inhibit-read-only 1))
        (when (member level allowed-levels)
          (unless (bolp) (newline))
          (insert (format "%s| [%s] %s\n"
                          (format-time-millis)
                          (upcase (symbol-name level))
                          (apply 'format format-string args))))))
    (read-only-mode 1)))

(defun etaf-log-trace (logger format-string &rest args)
  (declare (indent defun))
  (etaf-log logger 'trace format-string args)
  t)

(defun etaf-log-debug (logger format-string &rest args)
  (declare (indent defun))
  (etaf-log logger 'debug format-string args)
  t)

(defun etaf-log-info (logger format-string &rest args)
  (declare (indent defun))
  (etaf-log logger 'info format-string args)
  t)

(defun etaf-log-warning (logger format-string &rest args)
  (declare (indent defun))
  (etaf-log logger 'warning format-string args)
  t)

(defun etaf-log-error (logger format-string &rest args)
  (declare (indent defun))
  (etaf-log logger 'error format-string args))

(defun etaf-log-quit ()
  (interactive)
  (quit-window)
  (local-unset-key "q"))

(defun etaf-log-view (logger)
  (switch-to-buffer (get-buffer-create (plist-get logger :buffer)))
  (local-set-key "q" 'etaf-log-quit))

(defun etaf-log-clear (logger)
  (with-current-buffer
      (get-buffer-create (plist-get logger :buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'etaf-log)
