;;; etaf-perf.el --- Performance monitoring for ETAF -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang
;; Keywords: performance, monitoring, profiling
;; Version: 1.0.0

;;; Commentary:

;; This module provides performance monitoring capabilities for ETAF
;; to help optimize first-screen loading time (首屏加载时间).
;;
;; It tracks timing information for each stage of the rendering pipeline:
;; - ETML parsing
;; - DOM construction
;; - CSSOM building
;; - Render tree construction
;; - Layout calculation
;; - String generation
;;
;; Usage:
;;
;;   ;; Enable performance monitoring (single command)
;;   (etaf-perf-toggle)
;;
;;   ;; Render content (timing will be collected automatically)
;;   (etaf-paint-to-buffer "*demo*"
;;     '(div :class "container" (h1 "Hello")))
;;
;;   ;; Get performance report
;;   (etaf-perf-report)
;;
;;   ;; Display detailed report in a buffer
;;   (etaf-perf-show-report)
;;
;;   ;; Clear performance data
;;   (etaf-perf-clear)
;;
;;   ;; Disable performance monitoring (toggle again)
;;   (etaf-perf-toggle)

;;; Code:

(require 'cl-lib)

;;; Configuration

(defvar etaf-perf-enabled nil
  "Whether performance monitoring is enabled.")

(defvar etaf-perf-data nil
  "Current performance data collection.
Format: (:start-time TIME :stages ((STAGE . DURATION) ...))")

(defvar etaf-perf-history nil
  "History of performance measurements.
Each entry is a plist with :timestamp, :total, and :stages.")

(defvar etaf-perf-max-history 100
  "Maximum number of performance measurements to keep in history.")

;;; Time measurement utilities

(defun etaf-perf--current-time-ms ()
  "Return current time in milliseconds."
  (let ((time (current-time)))
    (+ (* (nth 0 time) 1000.0)
       (/ (nth 1 time) 1000.0)
       (/ (nth 2 time) 1000000.0))))

(defun etaf-perf--time-diff-ms (start-time end-time)
  "Calculate time difference in milliseconds."
  (- end-time start-time))

;;; Core API

;;;###autoload
(defun etaf-perf-toggle ()
  "Toggle performance monitoring on or off.
This is the main command users should use to enable/disable monitoring.
It handles both the monitoring state and hook installation automatically."
  (interactive)
  (if etaf-perf-enabled
      (progn
        (setq etaf-perf-enabled nil)
        (advice-remove 'etaf-paint-string #'etaf-perf-wrap-paint-string)
        (message "ETAF performance monitoring disabled"))
    (setq etaf-perf-enabled t)
    (advice-add 'etaf-paint-string :around #'etaf-perf-wrap-paint-string)
    (message "ETAF performance monitoring enabled")))

(defun etaf-perf-enable ()
  "Enable performance monitoring.
Note: Use `etaf-perf-toggle' for simple on/off switching."
  (interactive)
  (setq etaf-perf-enabled t)
  (message "ETAF performance monitoring enabled"))

(defun etaf-perf-disable ()
  "Disable performance monitoring.
Note: Use `etaf-perf-toggle' for simple on/off switching."
  (interactive)
  (setq etaf-perf-enabled nil)
  (message "ETAF performance monitoring disabled"))

(defun etaf-perf-clear ()
  "Clear all performance data."
  (interactive)
  (setq etaf-perf-data nil)
  (setq etaf-perf-history nil)
  (message "ETAF performance data cleared"))

(defun etaf-perf-start ()
  "Start a new performance measurement session."
  (when etaf-perf-enabled
    (setq etaf-perf-data
          (list :start-time (etaf-perf--current-time-ms)
                :stages nil))))

(defun etaf-perf-record-stage (stage-name start-time)
  "Record the duration of a stage.
STAGE-NAME is a symbol identifying the stage.
START-TIME is the time when the stage started."
  (when (and etaf-perf-enabled etaf-perf-data)
    (let* ((end-time (etaf-perf--current-time-ms))
           (duration (etaf-perf--time-diff-ms start-time end-time))
           (stages (plist-get etaf-perf-data :stages)))
      (setq stages (append stages (list (cons stage-name duration))))
      (plist-put etaf-perf-data :stages stages))))

(defun etaf-perf-finish ()
  "Finish the current performance measurement and save to history."
  (when (and etaf-perf-enabled etaf-perf-data)
    (let* ((end-time (etaf-perf--current-time-ms))
           (start-time (plist-get etaf-perf-data :start-time))
           (total-duration (etaf-perf--time-diff-ms start-time end-time))
           (stages (plist-get etaf-perf-data :stages))
           (entry (list :timestamp (current-time)
                        :total total-duration
                        :stages stages)))
      ;; Add to history
      (push entry etaf-perf-history)
      ;; Trim history if needed
      (when (> (length etaf-perf-history) etaf-perf-max-history)
        (setq etaf-perf-history
              (cl-subseq etaf-perf-history 0 etaf-perf-max-history)))
      ;; Clear current data
      (setq etaf-perf-data nil)
      total-duration)))

;;; Macro for measuring stage execution

(defmacro etaf-perf-measure (stage-name &rest body)
  "Measure execution time of BODY and record it as STAGE-NAME.
If performance monitoring is not enabled, just execute BODY without measurement."
  (declare (indent 1))
  `(if (and (boundp 'etaf-perf-enabled) etaf-perf-enabled)
       (let ((start-time (etaf-perf--current-time-ms))
             (result (progn ,@body)))
         (etaf-perf-record-stage ,stage-name start-time)
         result)
     (progn ,@body)))

;;; Reporting

(defun etaf-perf-get-last ()
  "Get the last performance measurement."
  (car etaf-perf-history))

(defun etaf-perf-get-average (&optional n)
  "Get average performance metrics from the last N measurements.
If N is nil, use all measurements."
  (let* ((measurements (if n
                           (cl-subseq etaf-perf-history 0 (min n (length etaf-perf-history)))
                         etaf-perf-history))
         (count (length measurements)))
    (when (> count 0)
      (let ((total-sum 0.0)
            (stage-sums (make-hash-table :test 'equal)))
        ;; Sum up all measurements
        (dolist (measurement measurements)
          (setq total-sum (+ total-sum (plist-get measurement :total)))
          (dolist (stage (plist-get measurement :stages))
            (let* ((stage-name (car stage))
                   (duration (cdr stage))
                   (current (gethash stage-name stage-sums 0.0)))
              (puthash stage-name (+ current duration) stage-sums))))
        ;; Calculate averages
        (let ((avg-stages nil))
          (maphash (lambda (stage-name sum)
                     (push (cons stage-name (/ sum count)) avg-stages))
                   stage-sums)
          (list :count count
                :total (/ total-sum count)
                :stages (nreverse avg-stages)))))))

(defun etaf-perf-report (&optional n)
  "Return a formatted performance report string.
If N is provided, include average of last N measurements."
  (let ((last (etaf-perf-get-last))
        (avg (when n (etaf-perf-get-average n))))
    (with-temp-buffer
      (insert "=== ETAF Performance Report ===\n\n")
      
      ;; Last measurement
      (when last
        (insert "Last Measurement:\n")
        (insert (format "  Total Time: %.4f ms\n"
                       (plist-get last :total)))
        (insert "  Stages:\n")
        (dolist (stage (plist-get last :stages))
          (let ((name (car stage))
                (duration (cdr stage)))
            (insert (format "    %-30s: %10.4f ms (%5.1f%%)\n"
                           name
                           duration
                           (* 100.0 (/ duration (plist-get last :total)))))))
        (insert "\n"))
      
      ;; Average measurements
      (when avg
        (insert (format "Average of Last %d Measurements:\n" (plist-get avg :count)))
        (insert (format "  Total Time: %.4f ms\n"
                       (plist-get avg :total)))
        (insert "  Stages:\n")
        (dolist (stage (plist-get avg :stages))
          (let ((name (car stage))
                (duration (cdr stage)))
            (insert (format "    %-30s: %10.4f ms (%5.1f%%)\n"
                           name
                           duration
                           (* 100.0 (/ duration (plist-get avg :total)))))))
        (insert "\n"))
      
      ;; Summary
      (insert (format "Total Measurements: %d\n" (length etaf-perf-history)))
      
      (buffer-string))))

(defun etaf-perf-show-report (&optional n)
  "Display performance report in a buffer.
If N is provided, include average of last N measurements."
  (interactive "P")
  (let ((report (etaf-perf-report (when n (prefix-numeric-value n)))))
    (with-current-buffer (get-buffer-create "*ETAF Performance*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report)
        (goto-char (point-min))
        (read-only-mode 1))
      (pop-to-buffer (current-buffer)))))

;;; Optimization suggestions

(defun etaf-perf-analyze ()
  "Analyze performance data and provide optimization suggestions."
  (interactive)
  (let ((avg (etaf-perf-get-average)))
    (if (not avg)
        (message "No performance data available. Run some renders first.")
      (let* ((total (plist-get avg :total))
             (stages (plist-get avg :stages))
             (suggestions nil))
        
        ;; Find the slowest stages
        (dolist (stage stages)
          (let* ((name (car stage))
                 (duration (cdr stage))
                 (percentage (* 100.0 (/ duration total))))
            (when (> percentage 30.0)
              (push (format "- %s takes %.1f%% of total time (%.4f ms)"
                           name percentage duration)
                    suggestions))))
        
        (if suggestions
            (let ((msg (concat "Performance Bottlenecks:\n"
                              (mapconcat #'identity (nreverse suggestions) "\n"))))
              (with-current-buffer (get-buffer-create "*ETAF Performance Analysis*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert "=== ETAF Performance Analysis ===\n\n")
                  (insert msg)
                  (goto-char (point-min))
                  (read-only-mode 1))
                (display-buffer (current-buffer)))
              (message "Performance analysis complete. See *ETAF Performance Analysis* buffer."))
          (message "No significant bottlenecks detected. Performance looks good!"))))))

;;; Integration with existing code

(defun etaf-perf-wrap-paint-string (original-fn etml &optional data ecss width height)
  "Advice wrapper for `etaf-paint-string` to add performance monitoring."
  (if etaf-perf-enabled
      (progn
        (etaf-perf-start)
        (let ((result (funcall original-fn etml data ecss width height)))
          (etaf-perf-finish)
          result))
    (funcall original-fn etml data ecss width height)))

;;;###autoload
(defun etaf-perf-install-hooks ()
  "Install performance monitoring hooks into ETAF rendering pipeline."
  (interactive)
  (advice-add 'etaf-paint-string :around #'etaf-perf-wrap-paint-string)
  (message "ETAF performance monitoring hooks installed"))

;;;###autoload
(defun etaf-perf-uninstall-hooks ()
  "Remove performance monitoring hooks from ETAF rendering pipeline."
  (interactive)
  (advice-remove 'etaf-paint-string #'etaf-perf-wrap-paint-string)
  (message "ETAF performance monitoring hooks uninstalled"))

(provide 'etaf-perf)
;;; etaf-perf.el ends here
