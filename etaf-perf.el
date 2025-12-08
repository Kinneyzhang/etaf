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
;; Additionally, it can capture intermediate data structures for inspection:
;; - Render function (compiled from ETML)
;; - Virtual DOM (VNode)
;; - DOM tree
;; - CSSOM (CSS Object Model)
;; - Render tree
;; - Layout tree
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
;;   ;; Display performance metrics
;;   (etaf-perf-show-report)
;;
;;   ;; Display captured rendering data (with collapsible sections)
;;   (etaf-perf-show-data)
;;
;;   ;; Display both metrics and data
;;   (etaf-perf-show-combined-report)
;;
;;   ;; Clear performance data
;;   (etaf-perf-clear)
;;
;;   ;; Disable performance monitoring (toggle again)
;;   (etaf-perf-toggle)
;;
;; The data inspection buffer supports folding:
;; - TAB: Toggle current section
;; - C-c C-a: Show all sections
;; - C-c C-t: Hide all sections
;; - q: Quit window


;;; Code:

(require 'cl-lib)

;; Forward declarations for functions from other modules
(declare-function etaf-etml-has-dynamic-content-p "etaf-etml")
(declare-function etaf-compile "etaf-etml")
(declare-function etaf-vdom-render "etaf-vdom")
(declare-function etaf-etml-to-dom "etaf-etml")
(declare-function etaf-ecss "etaf-ecss")
(declare-function etaf-css-build-cssom "etaf-css")
(declare-function etaf-css-add-stylesheet "etaf-css")
(declare-function etaf-render-build-tree "etaf-render")
(declare-function etaf-layout-build-tree "etaf-layout")
(declare-function etaf-layout-to-string "etaf-layout")
(declare-function etaf-viewport-width "etaf")
(declare-function etaf-pop-to-buffer "etaf-utils")

;;; Configuration

(defvar etaf-perf-enabled nil
  "Whether performance monitoring is enabled.")

(defvar etaf-perf-data nil
  "Current performance data collection.
Format: (:start-time TIME :stages ((STAGE . DURATION) ...))")

(defvar etaf-perf-history nil
  "History of performance measurements.
Each entry is a plist with :timestamp, :total, :stages, and :captured-data.")

(defvar etaf-perf-max-history 100
  "Maximum number of performance measurements to keep in history.")

(defvar etaf-perf-capture-data t
  "Whether to capture intermediate rendering data structures.
When enabled, captures vdom, dom, cssom, render-tree, layout-tree,
and render-function for inspection after paint completes.")

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
                :stages nil
                :captured-data (when etaf-perf-capture-data (list))))))

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

(defun etaf-perf-capture (data-name data-value)
  "Capture intermediate data during rendering.
DATA-NAME is a symbol identifying the data (e.g., 'vdom, 'dom).
DATA-VALUE is the data structure to capture."
  (when (and etaf-perf-enabled etaf-perf-data etaf-perf-capture-data)
    (let ((captured (plist-get etaf-perf-data :captured-data)))
      (plist-put etaf-perf-data :captured-data
                 (plist-put captured data-name data-value)))))

(defun etaf-perf-finish ()
  "Finish the current performance measurement and save to history."
  (when (and etaf-perf-enabled etaf-perf-data)
    (let* ((end-time (etaf-perf--current-time-ms))
           (start-time (plist-get etaf-perf-data :start-time))
           (total-duration (etaf-perf--time-diff-ms start-time end-time))
           (stages (plist-get etaf-perf-data :stages))
           (captured-data (plist-get etaf-perf-data :captured-data))
           (entry (list :timestamp (current-time)
                        :total total-duration
                        :stages stages
                        :captured-data captured-data)))
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
  (if etaf-perf-enabled
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
                (insert
                 (format "    %-30s: %10.4f ms (%5.1f%%)\n"
                         name
                         duration
                         (* 100.0 (/ duration
                                     (plist-get last :total)))))))
            (insert "\n"))
          
          ;; Average measurements
          (when avg
            (insert (format "Average of Last %d Measurements:\n"
                            (plist-get avg :count)))
            (insert (format "  Total Time: %.4f ms\n"
                            (plist-get avg :total)))
            (insert "  Stages:\n")
            (dolist (stage (plist-get avg :stages))
              (let ((name (car stage))
                    (duration (cdr stage)))
                (insert
                 (format "    %-30s: %10.4f ms (%5.1f%%)\n"
                         name
                         duration
                         (* 100.0 (/ duration (plist-get avg :total)))))))
            (insert "\n"))
          
          ;; Summary
          (insert
           (format "Total Measurements: %d\n" (length etaf-perf-history)))
          
          (buffer-string)))
    (message "M-x `etaf-perf-toggle' to turn on etaf-perf first!")))

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
      (etaf-pop-to-buffer (current-buffer)))))

;;; Data inspection functions

(defun etaf-perf--pp-to-string (obj)
  "Pretty-print OBJ to a string."
  (with-temp-buffer
    (cl-prettyprint obj)
    (buffer-string)))

(defun etaf-perf--insert-collapsible-section (title content &optional initially-collapsed)
  "Insert a collapsible section with TITLE and CONTENT.
If INITIALLY-COLLAPSED is non-nil, the section starts collapsed."
  (let ((start (point)))
    (insert "* " title "\n")
    (when content
      (insert content)
      (unless (string-suffix-p "\n" content)
        (insert "\n")))
    (insert "\n")
    ;; Apply outline properties
    (put-text-property start (point) 'outline-level (lambda () 1))
    (when initially-collapsed
      (outline-hide-subtree))))

(defun etaf-perf-show-data (&optional entry-index)
  "Display captured rendering data in a buffer.
ENTRY-INDEX specifies which history entry to show (default: 0 = most recent)."
  (interactive "P")
  (if (not etaf-perf-enabled)
      (message "M-x `etaf-perf-toggle` to turn on etaf-perf first!")
    (let* ((index (or entry-index 0))
           (entry (nth index etaf-perf-history)))
      (if (not entry)
          (message "No performance data available at index %d" index)
        (let ((captured-data (plist-get entry :captured-data)))
          (if (not captured-data)
              (message "No captured data available. Set `etaf-perf-capture-data` to t.")
            (with-current-buffer (get-buffer-create "*ETAF Performance Data*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "=== ETAF Rendering Pipeline Data ===\n\n")
                (insert (format "Measurement timestamp: %s\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S"
                                                   (plist-get entry :timestamp))))
                (insert (format "Total render time: %.4f ms\n\n"
                               (plist-get entry :total)))
                (insert "Use TAB to expand/collapse sections, C-c C-a to show all, C-c C-t to hide all.\n\n")
                
                ;; Insert each data structure as a collapsible section
                (when-let ((render-fn (plist-get captured-data 'render-function)))
                  (etaf-perf--insert-collapsible-section
                   "Render Function (from ETML compilation)"
                   (etaf-perf--pp-to-string render-fn)
                   t))
                
                (when-let ((vdom (plist-get captured-data 'vdom)))
                  (etaf-perf--insert-collapsible-section
                   "Virtual DOM (VNode)"
                   (etaf-perf--pp-to-string vdom)
                   t))
                
                (when-let ((dom (plist-get captured-data 'dom)))
                  (etaf-perf--insert-collapsible-section
                   "DOM Tree"
                   (etaf-perf--pp-to-string dom)
                   t))
                
                (when-let ((cssom (plist-get captured-data 'cssom)))
                  (etaf-perf--insert-collapsible-section
                   "CSSOM (CSS Object Model)"
                   (etaf-perf--pp-to-string cssom)
                   t))
                
                (when-let ((render-tree (plist-get captured-data 'render-tree)))
                  (etaf-perf--insert-collapsible-section
                   "Render Tree"
                   (etaf-perf--pp-to-string render-tree)
                   t))
                
                (when-let ((layout-tree (plist-get captured-data 'layout-tree)))
                  (etaf-perf--insert-collapsible-section
                   "Layout Tree"
                   (etaf-perf--pp-to-string layout-tree)
                   t))
                
                ;; Enable outline minor mode for folding
                (outline-mode)
                (outline-hide-body)
                (goto-char (point-min))
                
                ;; Set up key bindings
                (local-set-key (kbd "TAB") 'outline-toggle-children)
                (local-set-key (kbd "<tab>") 'outline-toggle-children)
                (local-set-key (kbd "C-c C-a") 'outline-show-all)
                (local-set-key (kbd "C-c C-t") 'outline-hide-body)
                (local-set-key (kbd "q") 'quit-window)
                
                (read-only-mode 1))
              (etaf-pop-to-buffer (current-buffer)))))))))

(defun etaf-perf-show-combined-report (&optional n)
  "Display both performance metrics and captured data.
If N is provided, include average of last N measurements."
  (interactive "P")
  (etaf-perf-show-report n)
  (etaf-perf-show-data))

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
  "Advice wrapper for `etaf-paint-string` to add performance monitoring and data capture."
  (if etaf-perf-enabled
      (progn
        (etaf-perf-start)
        ;; If data capture is enabled, we need to reimplement the paint logic
        ;; to capture intermediate data structures
        (let ((result
               (if etaf-perf-capture-data
                   ;; Capture data path: manually execute stages and capture
                   (etaf-perf--paint-with-capture etml data ecss width height)
                 ;; Normal path: just measure time
                 (funcall original-fn etml data ecss width height))))
          (etaf-perf-finish)
          result))
    (funcall original-fn etml data ecss width height)))

(defun etaf-perf--paint-with-capture (etml data ecss width height)
  "Execute paint with data capture enabled.
This reimplements etaf-paint-string to capture intermediate data."
  (let* (;; Stage 1: Check if template has dynamic content
         (has-dynamic (etaf-perf-measure 'check-dynamic-content
                        (etaf-etml-has-dynamic-content-p etml)))
         ;; Capture render function for dynamic templates
         render-fn
         vnode
         ;; Stage 2: Generate DOM
         (dom (if has-dynamic
                  ;; Dynamic path: ETML → Compiler → Render Function → VNode → DOM
                  (etaf-perf-measure 'etml-compile-and-render
                    (setq render-fn (etaf-compile etml))
                    (etaf-perf-capture 'render-function render-fn)
                    (setq vnode (funcall render-fn data))
                    (etaf-perf-capture 'vdom vnode)
                    (etaf-vdom-render vnode))
                ;; Static path: ETML → DOM directly
                (etaf-perf-measure 'etml-to-dom
                  (etaf-etml-to-dom etml data))))
         _ (etaf-perf-capture 'dom dom)
         ;; Stage 3: Build stylesheet
         (stylesheet (etaf-perf-measure 'build-stylesheet
                       (if ecss (apply #'etaf-ecss ecss) "")))
         ;; Stage 4: CSSOM - Build CSS Object Model
         (cssom (etaf-perf-measure 'build-cssom
                  (etaf-css-build-cssom dom)))
         (cssom (etaf-perf-measure 'add-stylesheet
                  (etaf-css-add-stylesheet cssom stylesheet)))
         _ (etaf-perf-capture 'cssom cssom)
         ;; Stage 5: Render tree - Combine DOM and CSSOM
         (render-tree (etaf-perf-measure 'build-render-tree
                        (etaf-render-build-tree dom cssom)))
         _ (etaf-perf-capture 'render-tree render-tree)
         ;; Stage 6: Layout tree - Calculate layout
         (layout-tree (etaf-perf-measure 'build-layout-tree
                        (etaf-layout-build-tree
                         render-tree (list :width (etaf-viewport-width width)
                                           :height height))))
         _ (etaf-perf-capture 'layout-tree layout-tree))
    ;; Stage 7: Final string - Convert layout to string
    (etaf-perf-measure 'layout-to-string
      (etaf-layout-to-string layout-tree))))

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
