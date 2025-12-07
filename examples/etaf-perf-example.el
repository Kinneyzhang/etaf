;;; etaf-perf-example.el --- Performance monitoring examples -*- lexical-binding: t; -*-

;;; Commentary:
;; Examples demonstrating the ETAF performance monitoring tool.
;; This tool helps optimize first-screen loading time (首屏加载时间).

;;; Code:

(require 'etaf)
(require 'etaf-perf)

;;; Example 1: Basic performance monitoring

(defun etaf-perf-example-1-basic ()
  "Basic performance monitoring example."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Clear any existing data
  (etaf-perf-clear)
  
  ;; Render a simple template
  (message "Rendering simple template...")
  (etaf-paint-to-buffer "*perf-demo-1*"
    '(div :class "container"
       (h1 :style "color: blue; font-size: 20px" "Performance Test")
       (p "This is a simple template to test performance monitoring.")
       (ul
        (li "Item 1")
        (li "Item 2")
        (li "Item 3"))))
  
  ;; Show the performance report
  (sit-for 0.5)  ; Give buffer time to display
  (etaf-perf-show-report))

;;; Example 2: Complex template with Tailwind CSS

(defun etaf-perf-example-2-complex ()
  "Performance monitoring with complex template and Tailwind CSS."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Clear any existing data
  (etaf-perf-clear)
  
  ;; Render a complex template with Tailwind CSS
  (message "Rendering complex template with Tailwind CSS...")
  (etaf-paint-to-buffer "*perf-demo-2*"
    '(div :class "flex flex-col items-center w-800px bg-gray-100 p-4"
       (div :class "bg-white rounded-lg shadow-md p-4 mb-4"
         (h1 :class "text-2xl font-bold text-gray-900 mb-2"
             "Performance Monitoring Demo")
         (p :class "text-gray-600" 
            "Monitoring rendering pipeline performance."))
       (div :class "grid grid-cols-3 gap-4 w-full"
         (div :class "bg-blue-500 text-white p-3 rounded"
           (h3 :class "font-bold mb-1" "Stage 1")
           (p "ETML Parsing"))
         (div :class "bg-green-500 text-white p-3 rounded"
           (h3 :class "font-bold mb-1" "Stage 2")
           (p "DOM Building"))
         (div :class "bg-purple-500 text-white p-3 rounded"
           (h3 :class "font-bold mb-1" "Stage 3")
           (p "Layout")))
       (div :class "bg-yellow-100 border-l-4 border-yellow-500 p-4 mt-4"
         (p :class "text-yellow-700"
            "Check the performance report for detailed timings!"))))
  
  ;; Show the performance report
  (sit-for 0.5)
  (etaf-perf-show-report))

;;; Example 3: Dynamic template with data

(defun etaf-perf-example-3-dynamic ()
  "Performance monitoring with dynamic template."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Clear any existing data
  (etaf-perf-clear)
  
  (let ((data '(:title "Performance Test"
                :items ("Apple" "Banana" "Cherry" "Date" "Elderberry")
                :count 5)))
    
    ;; Render dynamic template
    (message "Rendering dynamic template...")
    (etaf-paint-to-buffer "*perf-demo-3*"
      '(div :class "p-4"
         (h1 "{{ title }}")
         (p "Total items: {{ count }}")
         (ul
          (li :e-for "item in items" "{{ item }}")))
      data))
  
  ;; Show the performance report
  (sit-for 0.5)
  (etaf-perf-show-report))

;;; Example 4: Multiple renders with average statistics

(defun etaf-perf-example-4-multiple ()
  "Run multiple renders and show average performance."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Clear any existing data
  (etaf-perf-clear)
  
  (message "Running 10 render cycles...")
  
  ;; Run 10 render cycles
  (dotimes (i 10)
    (etaf-paint-to-buffer "*perf-demo-4*"
      `(div :class "container p-4"
         (h1 ,(format "Render Cycle %d" (1+ i)))
         (div :class "grid grid-cols-2 gap-2"
           ,@(cl-loop for j from 1 to 20
                      collect `(div :class "bg-blue-100 p-2"
                                 ,(format "Item %d" j))))))
    (message "Completed cycle %d/10" (1+ i)))
  
  ;; Show average performance report
  (sit-for 0.5)
  (message "All cycles completed. Showing average performance...")
  (etaf-perf-show-report 10))

;;; Example 5: Performance analysis and suggestions

(defun etaf-perf-example-5-analyze ()
  "Run renders and analyze for bottlenecks."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Clear any existing data
  (etaf-perf-clear)
  
  ;; Render a very complex template that might have bottlenecks
  (message "Rendering complex template for analysis...")
  (dotimes (i 5)
    (etaf-paint-to-buffer "*perf-demo-5*"
      '(div :class "flex flex-col"
         (div :class "bg-gradient-to-r from-blue-500 to-purple-600 text-white p-8"
           (h1 :class "text-4xl font-bold mb-4" "Performance Analysis")
           (p :class "text-xl" "Analyzing rendering performance..."))
         (div :class "grid grid-cols-4 gap-4 p-4"
           ,@(cl-loop for j from 1 to 40
                      collect `(div :class "bg-white shadow-lg rounded-lg p-4 hover:shadow-xl"
                                 (div :class "text-2xl font-bold text-blue-600"
                                      ,(format "%d" j))
                                 (p :class "text-gray-600" "Sample item")
                                 (div :class "flex justify-between mt-2"
                                   (span :class "text-green-600" "Active")
                                   (span :class "text-gray-400" "•••")))))))
    (message "Completed render %d/5" (1+ i)))
  
  ;; Analyze performance
  (sit-for 0.5)
  (message "Analysis complete. Checking for bottlenecks...")
  (etaf-perf-analyze)
  (sit-for 2)
  (etaf-perf-show-report 5))

;;; Example 6: Comparison - Static vs Dynamic templates

(defun etaf-perf-example-6-comparison ()
  "Compare performance of static vs dynamic templates."
  (interactive)
  
  ;; Enable performance monitoring
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  
  ;; Test static template
  (message "Testing static template performance...")
  (etaf-perf-clear)
  (dotimes (i 5)
    (etaf-paint-to-buffer "*perf-demo-6-static*"
      '(div :class "p-4"
         (h1 "Static Template")
         (p "This is a static template.")
         (ul
          (li "Static item 1")
          (li "Static item 2")
          (li "Static item 3")))))
  
  (let ((static-avg (etaf-perf-get-average 5)))
    
    ;; Test dynamic template
    (message "Testing dynamic template performance...")
    (etaf-perf-clear)
    (dotimes (i 5)
      (etaf-paint-to-buffer "*perf-demo-6-dynamic*"
        '(div :class "p-4"
           (h1 "{{ title }}")
           (p "{{ description }}")
           (ul
            (li :e-for "item in items" "{{ item }}")))
        '(:title "Dynamic Template"
          :description "This is a dynamic template."
          :items ("Dynamic item 1" "Dynamic item 2" "Dynamic item 3"))))
    
    (let ((dynamic-avg (etaf-perf-get-average 5)))
      
      ;; Show comparison
      (with-current-buffer (get-buffer-create "*ETAF Performance Comparison*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "=== Static vs Dynamic Template Performance ===\n\n")
          (insert (format "Static Template Average:  %.2f ms\n"
                         (plist-get static-avg :total)))
          (insert (format "Dynamic Template Average: %.2f ms\n"
                         (plist-get dynamic-avg :total)))
          (insert (format "\nDynamic overhead: %.2f ms (%.1f%% slower)\n"
                         (- (plist-get dynamic-avg :total)
                            (plist-get static-avg :total))
                         (* 100.0 (/ (- (plist-get dynamic-avg :total)
                                        (plist-get static-avg :total))
                                     (plist-get static-avg :total)))))
          (insert "\n--- Static Template Stages ---\n")
          (dolist (stage (plist-get static-avg :stages))
            (insert (format "  %-30s: %6.2f ms\n"
                           (car stage) (cdr stage))))
          (insert "\n--- Dynamic Template Stages ---\n")
          (dolist (stage (plist-get dynamic-avg :stages))
            (insert (format "  %-30s: %6.2f ms\n"
                           (car stage) (cdr stage))))
          (goto-char (point-min))
          (read-only-mode 1))
        (pop-to-buffer (current-buffer))))))

;;; Interactive demo menu

;;;###autoload
(defun etaf-perf-demo ()
  "Show performance monitoring demo menu."
  (interactive)
  (with-current-buffer (get-buffer-create "*ETAF Performance Examples*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "=== ETAF Performance Monitoring Examples ===\n\n")
      (insert "Run the following commands to see different examples:\n\n")
      (insert "1. M-x etaf-perf-example-1-basic\n")
      (insert "   Basic performance monitoring\n\n")
      (insert "2. M-x etaf-perf-example-2-complex\n")
      (insert "   Complex template with Tailwind CSS\n\n")
      (insert "3. M-x etaf-perf-example-3-dynamic\n")
      (insert "   Dynamic template with data binding\n\n")
      (insert "4. M-x etaf-perf-example-4-multiple\n")
      (insert "   Multiple renders with average statistics\n\n")
      (insert "5. M-x etaf-perf-example-5-analyze\n")
      (insert "   Performance analysis and bottleneck detection\n\n")
      (insert "6. M-x etaf-perf-example-6-comparison\n")
      (insert "   Static vs Dynamic template comparison\n\n")
      (insert "Press 'q' to quit this buffer.\n")
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key "q" 'quit-window))
    (display-buffer (current-buffer))))

(provide 'etaf-perf-example)
;;; etaf-perf-example.el ends here
