;;; etaf-perf-tests.el --- Tests for etaf-perf -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for ETAF performance monitoring module

;;; Code:

(require 'ert)
(require 'etaf-perf)
(require 'etaf)

;;; Test utilities

(defun etaf-perf-tests--reset ()
  "Reset performance monitoring state for tests."
  (setq etaf-perf-enabled nil)
  (setq etaf-perf-data nil)
  (setq etaf-perf-history nil))

;;; Basic functionality tests

(ert-deftest etaf-perf-test-enable-disable ()
  "Test enabling and disabling performance monitoring."
  (etaf-perf-tests--reset)
  (should (null etaf-perf-enabled))
  (etaf-perf-enable)
  (should etaf-perf-enabled)
  (etaf-perf-disable)
  (should (null etaf-perf-enabled)))

(ert-deftest etaf-perf-test-clear ()
  "Test clearing performance data."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (etaf-perf-record-stage 'test-stage (etaf-perf--current-time-ms))
  (should etaf-perf-data)
  (etaf-perf-clear)
  (should (null etaf-perf-data))
  (should (null etaf-perf-history)))

(ert-deftest etaf-perf-test-start ()
  "Test starting a performance measurement session."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (should etaf-perf-data)
  (should (plist-get etaf-perf-data :start-time))
  (should (listp (plist-get etaf-perf-data :stages))))

(ert-deftest etaf-perf-test-record-stage ()
  "Test recording stage durations."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (let ((start-time (etaf-perf--current-time-ms)))
    ;; Small delay to ensure measurable duration
    (sit-for 0.001)
    (etaf-perf-record-stage 'test-stage start-time)
    (let ((stages (plist-get etaf-perf-data :stages)))
      (should (= (length stages) 1))
      (should (eq (caar stages) 'test-stage))
      (should (>= (cdar stages) 0)))))

(ert-deftest etaf-perf-test-finish ()
  "Test finishing a measurement session."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  ;; Small delay to ensure measurable total time
  (sit-for 0.001)
  (etaf-perf-record-stage 'test-stage (etaf-perf--current-time-ms))
  (let ((total (etaf-perf-finish)))
    (should (>= total 0))
    (should (= (length etaf-perf-history) 1))
    (should (null etaf-perf-data))))

(ert-deftest etaf-perf-test-measure-macro ()
  "Test the etaf-perf-measure macro."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (let ((result (etaf-perf-measure test-operation
                  (+ 2 2))))
    (should (= result 4))
    (let ((stages (plist-get etaf-perf-data :stages)))
      (should (> (length stages) 0))
      (should (eq (caar stages) 'test-operation)))))

(ert-deftest etaf-perf-test-measure-macro-disabled ()
  "Test that macro works when monitoring is disabled."
  (etaf-perf-tests--reset)
  (etaf-perf-disable)
  (let ((result (etaf-perf-measure test-operation
                  (* 3 4))))
    (should (= result 12))))

;;; History and averaging tests

(ert-deftest etaf-perf-test-history ()
  "Test that multiple measurements are stored in history."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (dotimes (i 3)
    (etaf-perf-start)
    (etaf-perf-record-stage 'test-stage (etaf-perf--current-time-ms))
    (etaf-perf-finish))
  (should (= (length etaf-perf-history) 3)))

(ert-deftest etaf-perf-test-get-last ()
  "Test getting the last measurement."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (etaf-perf-record-stage 'test-stage (etaf-perf--current-time-ms))
  (etaf-perf-finish)
  (let ((last (etaf-perf-get-last)))
    (should last)
    (should (plist-get last :timestamp))
    (should (plist-get last :total))
    (should (plist-get last :stages))))

(ert-deftest etaf-perf-test-get-average ()
  "Test calculating average performance."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  ;; Add 3 measurements
  (dotimes (i 3)
    (etaf-perf-start)
    (etaf-perf-record-stage 'stage-a (etaf-perf--current-time-ms))
    ;; Minimal delay for measurable time
    (sit-for 0.001)
    (etaf-perf-record-stage 'stage-b (etaf-perf--current-time-ms))
    (etaf-perf-finish))
  (let ((avg (etaf-perf-get-average)))
    (should avg)
    (should (= (plist-get avg :count) 3))
    (should (>= (plist-get avg :total) 0))
    (should (= (length (plist-get avg :stages)) 2))))

(ert-deftest etaf-perf-test-max-history ()
  "Test that history is trimmed to max size."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (let ((etaf-perf-max-history 5))
    ;; Add 10 measurements
    (dotimes (i 10)
      (etaf-perf-start)
      (etaf-perf-finish))
    ;; Should only keep 5
    (should (= (length etaf-perf-history) 5))))

;;; Reporting tests

(ert-deftest etaf-perf-test-report ()
  "Test generating a performance report."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-start)
  (etaf-perf-record-stage 'test-stage (etaf-perf--current-time-ms))
  (etaf-perf-finish)
  (let ((report (etaf-perf-report)))
    (should (stringp report))
    (should (string-match-p "Performance Report" report))
    (should (string-match-p "test-stage" report))))

(ert-deftest etaf-perf-test-report-with-average ()
  "Test generating report with averages."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  ;; Add 3 measurements
  (dotimes (i 3)
    (etaf-perf-start)
    (etaf-perf-record-stage 'stage-1 (etaf-perf--current-time-ms))
    (etaf-perf-finish))
  (let ((report (etaf-perf-report 3)))
    (should (stringp report))
    (should (string-match-p "Average of Last 3" report))))

;;; Integration tests

(ert-deftest etaf-perf-test-integration-simple ()
  "Test performance monitoring with simple render."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  (unwind-protect
      (progn
        (etaf-paint-to-buffer "*test-perf-simple*"
          '(div (h1 "Test") (p "Content")))
        (let ((last (etaf-perf-get-last)))
          (should last)
          (should (> (plist-get last :total) 0))
          (let ((stages (plist-get last :stages)))
            (should (> (length stages) 0)))))
    (etaf-perf-uninstall-hooks)
    (when (get-buffer "*test-perf-simple*")
      (kill-buffer "*test-perf-simple*"))))

(ert-deftest etaf-perf-test-integration-dynamic ()
  "Test performance monitoring with dynamic template."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  (unwind-protect
      (progn
        (etaf-paint-to-buffer "*test-perf-dynamic*"
          '(div
             (h1 "{{ title }}")
             (ul
              (li :e-for "item in items" "{{ item }}")))
          '(:title "Test"
            :items ("A" "B" "C")))
        (let ((last (etaf-perf-get-last)))
          (should last)
          (should (> (plist-get last :total) 0))
          ;; Dynamic templates should have compile stage
          (let ((stages (plist-get last :stages)))
            (should (> (length stages) 0)))))
    (etaf-perf-uninstall-hooks)
    (when (get-buffer "*test-perf-dynamic*")
      (kill-buffer "*test-perf-dynamic*"))))

(ert-deftest etaf-perf-test-integration-multiple ()
  "Test performance monitoring with multiple renders."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (etaf-perf-install-hooks)
  (unwind-protect
      (progn
        ;; Render 5 times
        (dotimes (i 5)
          (etaf-paint-to-buffer "*test-perf-multiple*"
            `(div (h1 ,(format "Test %d" i)))))
        ;; Should have 5 measurements
        (should (= (length etaf-perf-history) 5))
        ;; Get average
        (let ((avg (etaf-perf-get-average 5)))
          (should avg)
          (should (= (plist-get avg :count) 5))))
    (etaf-perf-uninstall-hooks)
    (when (get-buffer "*test-perf-multiple*")
      (kill-buffer "*test-perf-multiple*"))))

;;; Data capture tests

(ert-deftest etaf-perf-test-data-capture-enabled ()
  "Test that data capture can be enabled."
  (etaf-perf-tests--reset)
  (setq etaf-perf-capture-data t)
  (should etaf-perf-capture-data))

(ert-deftest etaf-perf-test-capture-function ()
  "Test the etaf-perf-capture function."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (setq etaf-perf-capture-data t)
  (etaf-perf-start)
  (etaf-perf-capture 'test-data '(some data))
  (let ((captured (plist-get etaf-perf-data :captured-data)))
    (should captured)
    (should (equal (plist-get captured 'test-data) '(some data)))))

(ert-deftest etaf-perf-test-data-in-history ()
  "Test that captured data is stored in history."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (setq etaf-perf-capture-data t)
  (etaf-perf-start)
  (etaf-perf-capture 'test-data "test value")
  (etaf-perf-finish)
  (let* ((entry (etaf-perf-get-last))
         (captured (plist-get entry :captured-data)))
    (should captured)
    (should (equal (plist-get captured 'test-data) "test value"))))

(ert-deftest etaf-perf-test-integration-with-capture ()
  "Test performance monitoring with data capture enabled."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (setq etaf-perf-capture-data t)
  (etaf-perf-install-hooks)
  (unwind-protect
      (progn
        (etaf-paint-to-buffer "*test-perf-capture*"
          '(div
             (h1 "{{ title }}")
             (p "{{ content }}"))
          '(:title "Test" :content "Content"))
        (let* ((last (etaf-perf-get-last))
               (captured (plist-get last :captured-data)))
          (should last)
          (should captured)
          ;; Check that we captured the expected data structures
          ;; For dynamic templates, we should have vdom, dom, cssom, etc.
          (should (plist-member captured 'vdom))
          (should (plist-member captured 'dom))
          (should (plist-member captured 'cssom))
          (should (plist-member captured 'render-tree))
          (should (plist-member captured 'layout-tree))
          (should (plist-member captured 'render-function))))
    (etaf-perf-uninstall-hooks)
    (when (get-buffer "*test-perf-capture*")
      (kill-buffer "*test-perf-capture*"))))

(ert-deftest etaf-perf-test-show-data ()
  "Test showing captured data in a buffer."
  (etaf-perf-tests--reset)
  (etaf-perf-enable)
  (setq etaf-perf-capture-data t)
  (etaf-perf-install-hooks)
  (unwind-protect
      (progn
        (etaf-paint-to-buffer "*test-perf-data*"
          '(div (h1 "Test")))
        ;; Try to show the data (should not error)
        (etaf-perf-show-data)
        (should (get-buffer "*ETAF Performance Data*"))
        (with-current-buffer "*ETAF Performance Data*"
          ;; Check that the buffer has content
          (should (> (buffer-size) 0))))
    (etaf-perf-uninstall-hooks)
    (when (get-buffer "*test-perf-data*")
      (kill-buffer "*test-perf-data*"))
    (when (get-buffer "*ETAF Performance Data*")
      (kill-buffer "*ETAF Performance Data*"))))

;;; Test runner

(defun etaf-perf-run-tests ()
  "Run all etaf-perf tests."
  (interactive)
  (ert-run-tests-interactively "^etaf-perf-test-"))

(provide 'etaf-perf-tests)
;;; etaf-perf-tests.el ends here
