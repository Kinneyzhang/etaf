;;; etaf-layout-overflow-tests.el --- Tests for vertical overflow and scrollbar -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for vertical overflow handling and scrollbar rendering in the layout system

;;; Code:

(require 'ert)
(require 'etaf-layout)
(require 'etaf-layout-box)
(require 'etaf-layout-string)
(require 'etaf-etml)
(require 'etaf-css)

;;; Test data

(defvar etaf-layout-overflow-tests-dom-visible
  (etaf-etml-to-dom
   '(html
     (head
      (style "
        body { width: 200px; }
        .container { 
          height: 3lh;
          overflow-y: visible;
        }
      "))
     (body
      (div :class "container" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))))
  "Test DOM with overflow-y: visible.")

(defvar etaf-layout-overflow-tests-dom-hidden
  (etaf-etml-to-dom
   '(html
     (head
      (style "
        body { width: 200px; }
        .container { 
          height: 3lh;
          overflow-y: hidden;
        }
      "))
     (body
      (div :class "container" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))))
  "Test DOM with overflow-y: hidden.")

(defvar etaf-layout-overflow-tests-dom-auto
  (etaf-etml-to-dom
   '(html
     (head
      (style "
        body { width: 200px; }
        .container { 
          height: 3lh;
          overflow-y: auto;
        }
      "))
     (body
      (div :class "container" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"))))
  "Test DOM with overflow-y: auto (with overflow).")

(defvar etaf-layout-overflow-tests-dom-auto-no-overflow
  (etaf-etml-to-dom
   '(html
     (head
      (style "
        body { width: 200px; }
        .container { 
          height: 5lh;
          overflow-y: auto;
        }
      "))
     (body
      (div :class "container" "Line 1\nLine 2\nLine 3"))))
  "Test DOM with overflow-y: auto (no overflow).")

(defvar etaf-layout-overflow-tests-dom-scroll
  (etaf-etml-to-dom
   '(html
     (head
      (style "
        body { width: 200px; }
        .container { 
          height: 3lh;
          overflow-y: scroll;
        }
      "))
     (body
      (div :class "container" "Line 1\nLine 2\nLine 3"))))
  "Test DOM with overflow-y: scroll (always show scrollbar).")

;;; Box model tests

(ert-deftest etaf-layout-box-overflow-y-default ()
  "Test default overflow-y value."
  (let ((box (etaf-layout-box-create)))
    (should (equal (etaf-layout-box-overflow-y box) "visible"))))

(ert-deftest etaf-layout-box-overflow-accessors ()
  "Test overflow property accessors."
  (let ((box (list :box-sizing "content-box"
                   :content '(:width 100 :height 50)
                   :padding '(:top 0 :right 0 :bottom 0 :left 0)
                   :border '(:top-width 0 :right-width 0 :bottom-width 0 :left-width 0)
                   :margin '(:top 0 :right 0 :bottom 0 :left 0)
                   :overflow '(:overflow-y "auto"
                               :v-scroll-bar-type simple
                               :v-scroll-bar-direction right
                               :scroll-thumb-color "#333"
                               :scroll-track-color "#eee"))))
    (should (equal (etaf-layout-box-overflow-y box) "auto"))
    (should (eq (etaf-layout-box-v-scroll-bar-type box) 'simple))
    (should (eq (etaf-layout-box-v-scroll-bar-direction box) 'right))
    (should (equal (etaf-layout-box-scroll-thumb-color box) "#333"))
    (should (equal (etaf-layout-box-scroll-track-color box) "#eee"))))

(ert-deftest etaf-layout-box-v-scroll-bar-direction-default ()
  "Test default scrollbar direction."
  (let ((box (etaf-layout-box-create)))
    (should (eq (etaf-layout-box-v-scroll-bar-direction box) 'right))))

;;; Layout string helper function tests

(ert-deftest etaf-layout-string--v-scroll-bar-p-visible ()
  "Test scrollbar visibility for overflow-y: visible."
  (should-not (etaf-layout-string--v-scroll-bar-p "visible" t))
  (should-not (etaf-layout-string--v-scroll-bar-p "visible" nil)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-hidden ()
  "Test scrollbar visibility for overflow-y: hidden."
  (should-not (etaf-layout-string--v-scroll-bar-p "hidden" t))
  (should-not (etaf-layout-string--v-scroll-bar-p "hidden" nil)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-auto ()
  "Test scrollbar visibility for overflow-y: auto."
  (should (eq (etaf-layout-string--v-scroll-bar-p "auto" t) 'real))
  (should-not (etaf-layout-string--v-scroll-bar-p "auto" nil)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-scroll ()
  "Test scrollbar visibility for overflow-y: scroll."
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll" t) 'real))
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll" nil) 'real)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-scroll-visible ()
  "Test scrollbar visibility for overflow-y: scroll-visible."
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll-visible" t) 'real))
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll-visible" nil) 'real)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-scroll-hidden ()
  "Test scrollbar visibility for overflow-y: scroll-hidden."
  (should-not (etaf-layout-string--v-scroll-bar-p "scroll-hidden" t))
  (should-not (etaf-layout-string--v-scroll-bar-p "scroll-hidden" nil)))

(ert-deftest etaf-layout-string--v-scroll-bar-p-scroll-auto ()
  "Test scrollbar visibility for overflow-y: scroll-auto."
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll-auto" t) 'real))
  (should (eq (etaf-layout-string--v-scroll-bar-p "scroll-auto" nil) 'blank)))

;;; Thumb height calculation tests

(ert-deftest etaf-layout-string--compute-thumb-height-no-overflow ()
  "Test thumb height when no overflow."
  ;; content-linum <= content-height: thumb = content-height
  (should (= (etaf-layout-string--compute-thumb-height 5 3) 5))
  (should (= (etaf-layout-string--compute-thumb-height 5 5) 5)))

(ert-deftest etaf-layout-string--compute-thumb-height-small-overflow ()
  "Test thumb height when overflow is smaller than content height."
  ;; overflow < content-height: thumb = content-height - overflow
  (should (= (etaf-layout-string--compute-thumb-height 5 7) 3))
  (should (= (etaf-layout-string--compute-thumb-height 5 8) 2)))

(ert-deftest etaf-layout-string--compute-thumb-height-large-overflow ()
  "Test thumb height when overflow is larger than or equal to content height."
  ;; overflow >= content-height: thumb = 1 (minimum)
  (should (= (etaf-layout-string--compute-thumb-height 5 10) 1))
  (should (= (etaf-layout-string--compute-thumb-height 5 15) 1)))

;;; Integration tests

(ert-deftest etaf-layout-overflow-y-parsing ()
  "Test that overflow-y is correctly parsed from CSS."
  (let* ((dom etaf-layout-overflow-tests-dom-auto)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 300 :height 200))))
    ;; Find the container div
    (etaf-layout-walk layout-tree
      (lambda (node)
        (when (and (dom-attr node 'class)
                   (string-match-p "container" (dom-attr node 'class)))
          (let* ((box-model (etaf-layout-get-box-model node))
                 (overflow-y (etaf-layout-box-overflow-y box-model)))
            (should (equal overflow-y "auto"))))))))

(ert-deftest etaf-layout-overflow-scroll-parsing ()
  "Test that overflow-y: scroll is correctly parsed."
  (let* ((dom etaf-layout-overflow-tests-dom-scroll)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 300 :height 200))))
    ;; Find the container div
    (etaf-layout-walk layout-tree
      (lambda (node)
        (when (and (dom-attr node 'class)
                   (string-match-p "container" (dom-attr node 'class)))
          (let* ((box-model (etaf-layout-get-box-model node))
                 (overflow-y (etaf-layout-box-overflow-y box-model)))
            (should (equal overflow-y "scroll"))))))))

(ert-deftest etaf-layout-overflow-default-visible ()
  "Test that default overflow-y is visible."
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "body { width: 200px; }"))
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 300 :height 200))))
    ;; Find any div
    (etaf-layout-walk layout-tree
      (lambda (node)
        (when (eq (dom-tag node) 'div)
          (let* ((box-model (etaf-layout-get-box-model node))
                 (overflow-y (etaf-layout-box-overflow-y box-model)))
            (should (equal overflow-y "visible"))))))))

(provide 'etaf-layout-overflow-tests)
;;; etaf-layout-overflow-tests.el ends here
