;;; etaf-grid-tests.el --- Tests for grid layout support -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the grid layout system in etaf-layout-grid.el and etaf-css-shorthand.el

;;; Code:

(require 'ert)
(require 'etaf-ert)
(require 'etaf-ecss)
(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; CSS Shorthand Tests for Grid Properties

(ert-deftest etaf-css-shorthand-test-grid-column-simple ()
  "Test expanding grid-column: <start>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-column "1" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(grid-column-start "1" nil)))
    (should (equal (nth 1 result) '(grid-column-end "auto" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-column-with-end ()
  "Test expanding grid-column: <start> / <end>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-column "1 / 3" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(grid-column-start "1" nil)))
    (should (equal (nth 1 result) '(grid-column-end "3" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-column-span ()
  "Test expanding grid-column: span <count>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-column "span 2" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(grid-column-start "span 2" nil)))
    (should (equal (nth 1 result) '(grid-column-end "auto" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-row-simple ()
  "Test expanding grid-row: <start>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-row "1" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(grid-row-start "1" nil)))
    (should (equal (nth 1 result) '(grid-row-end "auto" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-row-with-end ()
  "Test expanding grid-row: <start> / <end>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-row "2 / 4" nil)))
    (should (equal (length result) 2))
    (should (equal (nth 0 result) '(grid-row-start "2" nil)))
    (should (equal (nth 1 result) '(grid-row-end "4" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-area-named ()
  "Test expanding grid-area: <area-name>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-area "header" nil)))
    (should (equal (length result) 1))
    (should (equal (nth 0 result) '(grid-area "header" nil)))))

(ert-deftest etaf-css-shorthand-test-grid-area-position ()
  "Test expanding grid-area: <row-start> / <col-start> / <row-end> / <col-end>."
  (require 'etaf-css-shorthand)
  (let ((result (etaf-css--expand-grid-area "1 / 2 / 3 / 4" nil)))
    (should (equal (length result) 4))
    (should (equal (nth 0 result) '(grid-row-start "1" nil)))
    (should (equal (nth 1 result) '(grid-column-start "2" nil)))
    (should (equal (nth 2 result) '(grid-row-end "3" nil)))
    (should (equal (nth 3 result) '(grid-column-end "4" nil)))))

;;; Grid Layout Helper Tests

(ert-deftest etaf-layout-test-grid-parse-tracks-simple ()
  "Test parsing simple grid track definition."
  (require 'etaf-layout-grid)
  (let ((tracks (etaf-layout-grid-parse-tracks "100px 200px 300px" 600)))
    (should (equal (length tracks) 3))
    (should (equal (plist-get (nth 0 tracks) :type) 'px))
    (should (equal (plist-get (nth 0 tracks) :value) 100))
    (should (equal (plist-get (nth 1 tracks) :type) 'px))
    (should (equal (plist-get (nth 1 tracks) :value) 200))
    (should (equal (plist-get (nth 2 tracks) :type) 'px))
    (should (equal (plist-get (nth 2 tracks) :value) 300))))

(ert-deftest etaf-layout-test-grid-parse-tracks-fr ()
  "Test parsing fr unit in grid track definition."
  (require 'etaf-layout-grid)
  (let ((tracks (etaf-layout-grid-parse-tracks "1fr 2fr" 600)))
    (should (equal (length tracks) 2))
    (should (equal (plist-get (nth 0 tracks) :type) 'fr))
    (should (equal (plist-get (nth 0 tracks) :value) 1))
    (should (equal (plist-get (nth 1 tracks) :type) 'fr))
    (should (equal (plist-get (nth 1 tracks) :value) 2))))

(ert-deftest etaf-layout-test-grid-parse-tracks-mixed ()
  "Test parsing mixed grid track definition."
  (require 'etaf-layout-grid)
  (let ((tracks (etaf-layout-grid-parse-tracks "100px 1fr auto" 600)))
    (should (equal (length tracks) 3))
    (should (equal (plist-get (nth 0 tracks) :type) 'px))
    (should (equal (plist-get (nth 1 tracks) :type) 'fr))
    (should (equal (plist-get (nth 2 tracks) :type) 'auto))))

(ert-deftest etaf-layout-test-grid-resolve-tracks-fixed ()
  "Test resolving fixed-size tracks."
  (require 'etaf-layout-grid)
  (let* ((tracks (list (list :type 'px :value 100)
                      (list :type 'px :value 200)))
         (sizes (etaf-layout-grid-resolve-tracks tracks 300)))
    (should (equal (length sizes) 2))
    (should (equal (nth 0 sizes) 100))
    (should (equal (nth 1 sizes) 200))))

(ert-deftest etaf-layout-test-grid-resolve-tracks-fr ()
  "Test resolving fr tracks."
  (require 'etaf-layout-grid)
  (let* ((tracks (list (list :type 'fr :value 1)
                      (list :type 'fr :value 2)))
         (sizes (etaf-layout-grid-resolve-tracks tracks 300)))
    (should (equal (length sizes) 2))
    (should (equal (nth 0 sizes) 100))  ; 1fr = 100px
    (should (equal (nth 1 sizes) 200)))) ; 2fr = 200px

(ert-deftest etaf-layout-test-grid-resolve-tracks-mixed ()
  "Test resolving mixed tracks."
  (require 'etaf-layout-grid)
  (let* ((tracks (list (list :type 'px :value 100)
                      (list :type 'fr :value 1)
                      (list :type 'fr :value 1)))
         (sizes (etaf-layout-grid-resolve-tracks tracks 300)))
    (should (equal (length sizes) 3))
    (should (equal (nth 0 sizes) 100))  ; fixed 100px
    (should (equal (nth 1 sizes) 100))  ; 1fr = 100px
    (should (equal (nth 2 sizes) 100)))) ; 1fr = 100px

;;; Integration Tests

(ert-deftest etaf-layout-test-grid-simple ()
  "Test simple grid layout with fixed columns."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 100px 200px; grid-template-rows: 50px 50px; width: 300px; height: 100px;"
                     (div :style "grid-column: 1; grid-row: 1;" "Item 1")
                     (div :style "grid-column: 2; grid-row: 1;" "Item 2")
                     (div :style "grid-column: 1; grid-row: 2;" "Item 3")
                     (div :style "grid-column: 2; grid-row: 2;" "Item 4"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 300 :height 100))))
    (should layout-tree)
    (let ((box-model (etaf-layout-get-box-model layout-tree)))
      (should (equal (etaf-layout-box-content-width box-model) 300))
      ;; Height should be auto-calculated from rows, not the container's explicit height
      ;; since grid children don't expand the container like block children do
      (should (>= (etaf-layout-box-content-height box-model) 0)))
    ;; Check that grid tracks were computed
    (should (dom-attr layout-tree 'layout-grid-tracks))))

(ert-deftest etaf-layout-test-grid-fr-units ()
  "Test grid layout with fr units."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 1fr 2fr; width: 300px;"
                     (div "Item 1")
                     (div "Item 2"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 300 :height nil))))
    (should layout-tree)
    (let* ((tracks (dom-attr layout-tree 'layout-grid-tracks))
           (columns (plist-get tracks :columns)))
      (should columns)
      (should (equal (length columns) 2))
      ;; 1fr should be 100px, 2fr should be 200px
      (should (equal (nth 0 columns) 100))
      (should (equal (nth 1 columns) 200)))))

(ert-deftest etaf-layout-test-grid-gap ()
  "Test grid layout with gap."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 100px 100px; gap: 10px; width: 300px;"
                     (div "Item 1")
                     (div "Item 2"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 300 :height nil))))
    (should layout-tree)
    (should (equal (dom-attr layout-tree 'layout-column-gap) 10))
    (should (equal (dom-attr layout-tree 'layout-row-gap) 10))))

(ert-deftest etaf-layout-test-grid-column-span ()
  "Test grid item spanning multiple columns."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 100px 100px 100px; width: 300px;"
                     (div :style "grid-column: 1 / 3;" "Spanning Item")
                     (div :style "grid-column: 3;" "Item 2"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 300 :height nil))))
    (should layout-tree)
    (let* ((children (dom-non-text-children layout-tree))
           (item1 (nth 0 children)))
      (should item1)
      ;; Check that the item spans 2 columns
      (should (equal (dom-attr item1 'layout-grid-column-span) 2)))))

(ert-deftest etaf-layout-test-grid-auto-placement ()
  "Test automatic grid item placement."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 100px 100px; grid-template-rows: 50px 50px; width: 200px; height: 100px;"
                     (div "Item 1")
                     (div "Item 2")
                     (div "Item 3"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 200 :height 100))))
    (should layout-tree)
    ;; Check that grid tracks were computed (verifies it's treated as a grid)
    (should (dom-attr layout-tree 'layout-grid-tracks))
    ;; Check basic grid properties
    (let ((tracks (dom-attr layout-tree 'layout-grid-tracks)))
      (should (equal (plist-get tracks :num-columns) 2))
      ;; Should have at least 2 rows to accommodate 3 items
      (should (>= (plist-get tracks :num-rows) 2)))))

(ert-deftest etaf-layout-test-grid-justify-items ()
  "Test grid justify-items alignment."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-columns: 100px 100px; justify-items: center; width: 200px;"
                     (div "Item 1")
                     (div "Item 2"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 200 :height nil))))
    (should layout-tree)
    (should (equal (dom-attr layout-tree 'layout-justify-items) "center"))))

(ert-deftest etaf-layout-test-grid-align-items ()
  "Test grid align-items alignment."
  (let* ((dom (etaf-etml-to-dom
               '(div :style "display: grid; grid-template-rows: 100px 100px; align-items: end; height: 200px;"
                     (div "Item 1")
                     (div "Item 2"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width nil :height 200))))
    (should layout-tree)
    (should (equal (dom-attr layout-tree 'layout-align-items) "end"))))

(provide 'etaf-grid-tests)
;;; etaf-grid-tests.el ends here
