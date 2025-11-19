;;; etaf-layout-tests.el --- Tests for etaf-layout.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the layout system

;;; Code:

(require 'ert)
(require 'etaf-layout)
(require 'etaf-render)
(require 'etaf-css)
(require 'etaf-tml)

;;; Helper Functions

(defmacro should-equal (actual expected)
  "Assert that ACTUAL equals EXPECTED."
  `(should (equal ,actual ,expected)))

;;; Box Model Tests

(ert-deftest etaf-layout-test-box-model-create ()
  "Test box model creation."
  (let ((box-model (etaf-box-model-create)))
    (should (plist-get box-model :content))
    (should (plist-get box-model :padding))
    (should (plist-get box-model :border))
    (should (plist-get box-model :margin))
    (should-equal (etaf-box-model-content-width box-model) 0)
    (should-equal (etaf-box-model-content-height box-model) 0)))

(ert-deftest etaf-layout-test-box-model-dimensions ()
  "Test box model dimension calculations."
  (let ((box-model (list :content (list :width 200 :height 100)
                        :padding (list :top 10 :right 15 :bottom 10 :left 15)
                        :border (list :top-width 2 :right-width 2 :bottom-width 2 :left-width 2)
                        :margin (list :top 5 :right 10 :bottom 5 :left 10))))
    (should-equal (etaf-box-model-content-width box-model) 200)
    (should-equal (etaf-box-model-content-height box-model) 100)
    (should-equal (etaf-box-model-padding-width box-model) 30)
    (should-equal (etaf-box-model-padding-height box-model) 20)
    (should-equal (etaf-box-model-border-width box-model) 4)
    (should-equal (etaf-box-model-border-height box-model) 4)
    (should-equal (etaf-box-model-margin-width box-model) 20)
    (should-equal (etaf-box-model-margin-height box-model) 10)
    (should-equal (etaf-box-model-total-width box-model) 254)
    (should-equal (etaf-box-model-total-height box-model) 134)))

;;; CSS Value Parsing Tests

(ert-deftest etaf-layout-test-parse-length-px ()
  "Test parsing pixel values."
  (should-equal (etaf-layout-parse-length "10px" 1000) 10)
  (should-equal (etaf-layout-parse-length "100px" 1000) 100)
  (should-equal (etaf-layout-parse-length "0" 1000) 0))

(ert-deftest etaf-layout-test-parse-length-percent ()
  "Test parsing percentage values."
  (should-equal (etaf-layout-parse-length "50%" 1000) 500.0)
  (should-equal (etaf-layout-parse-length "25%" 800) 200.0)
  (should-equal (etaf-layout-parse-length "100%" 500) 500.0))

(ert-deftest etaf-layout-test-parse-length-auto ()
  "Test parsing auto values."
  (should-equal (etaf-layout-parse-length "auto" 1000) 'auto)
  (should-equal (etaf-layout-parse-length nil 1000) 'auto))

(ert-deftest etaf-layout-test-parse-length-em ()
  "Test parsing em values."
  (should-equal (etaf-layout-parse-length "1em" 1000) 16)
  (should-equal (etaf-layout-parse-length "2em" 1000) 32))

;;; Layout Computation Tests

(ert-deftest etaf-layout-test-simple-block-layout ()
  "Test simple block layout."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "div { width: 200px; height: 100px; padding: 10px; margin: 5px; }"))
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (should layout-tree)
    (should (plist-get layout-tree :box-model))
    (should (plist-get layout-tree :position))
    (should (plist-get layout-tree :children))))

(ert-deftest etaf-layout-test-nested-blocks ()
  "Test nested block layout."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    .outer { width: 400px; padding: 20px; }
                    .inner { width: 200px; height: 100px; margin: 10px; }
                  "))
                 (body
                  (div :class "outer"
                       (div :class "inner" "Inner 1")
                       (div :class "inner" "Inner 2"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    ;; 检查布局树结构
    (should layout-tree)
    (let* ((body-node (car (plist-get layout-tree :children)))
           (outer-node (car (plist-get body-node :children)))
           (inner-nodes (plist-get outer-node :children)))
      
      (should outer-node)
      (should-equal (length inner-nodes) 2)
      
      ;; 检查外层盒子
      (let ((outer-box (plist-get outer-node :box-model)))
        (should-equal (etaf-box-model-content-width outer-box) 400))
      
      ;; 检查内层盒子
      (let ((inner1-box (plist-get (car inner-nodes) :box-model)))
        (should-equal (etaf-box-model-content-width inner1-box) 200)
        (should-equal (etaf-box-model-content-height inner1-box) 100)))))

(ert-deftest etaf-layout-test-width-auto ()
  "Test width: auto calculation."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    .container { width: 800px; padding-left: 20px; padding-right: 20px; }
                    .box { height: 100px; margin-left: 10px; margin-right: 10px; }
                  "))
                 (body
                  (div :class "container"
                       (div :class "box" "Auto width"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    (let* ((body-node (car (plist-get layout-tree :children)))
           (container-node (car (plist-get body-node :children)))
           (box-node (car (plist-get container-node :children)))
           (box-model (plist-get box-node :box-model)))
      
      ;; 容器宽度应该是 800px
      (should-equal (etaf-box-model-content-width 
                    (plist-get container-node :box-model)) 800)
      
      ;; 盒子宽度应该自动填充容器内容宽度 - margin：800 - 10*2 (margin) = 780
      (should-equal (etaf-box-model-content-width box-model) 780))))

(ert-deftest etaf-layout-test-padding-border-margin ()
  "Test padding, border, and margin calculations."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    div { 
                      width: 200px; 
                      height: 100px;
                      padding-top: 10px;
                      padding-right: 15px;
                      padding-bottom: 10px;
                      padding-left: 15px;
                      border-top-width: 2px;
                      border-right-width: 2px;
                      border-bottom-width: 2px;
                      border-left-width: 2px;
                      margin-top: 5px;
                      margin-right: 10px;
                      margin-bottom: 5px;
                      margin-left: 10px;
                    }
                  "))
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (body-node (car (plist-get layout-tree :children)))
         (div-node (car (plist-get body-node :children)))
         (box-model (plist-get div-node :box-model))
         (padding (plist-get box-model :padding))
         (border (plist-get box-model :border))
         (margin (plist-get box-model :margin)))
    
    ;; 检查 padding
    (should-equal (plist-get padding :top) 10)
    (should-equal (plist-get padding :right) 15)
    (should-equal (plist-get padding :bottom) 10)
    (should-equal (plist-get padding :left) 15)
    
    ;; 检查 border
    (should-equal (plist-get border :top-width) 2)
    (should-equal (plist-get border :right-width) 2)
    (should-equal (plist-get border :bottom-width) 2)
    (should-equal (plist-get border :left-width) 2)
    
    ;; 检查 margin
    (should-equal (plist-get margin :top) 5)
    (should-equal (plist-get margin :right) 10)
    (should-equal (plist-get margin :bottom) 5)
    (should-equal (plist-get margin :left) 10)))

(ert-deftest etaf-layout-test-height-auto ()
  "Test height: auto calculation based on children."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    .container { width: 400px; }
                    .box { width: 100%; height: 50px; margin-bottom: 10px; }
                  "))
                 (body
                  (div :class "container"
                       (div :class "box" "Box 1")
                       (div :class "box" "Box 2")
                       (div :class "box" "Box 3"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (body-node (car (plist-get layout-tree :children)))
         (container-node (car (plist-get body-node :children)))
         (container-box (plist-get container-node :box-model)))
    
    ;; 容器高度应该是所有子元素高度之和: 3 * (50 + 10) = 180
    (should-equal (etaf-box-model-content-height container-box) 180)))

(ert-deftest etaf-layout-test-position-calculation ()
  "Test position calculation for nested elements."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    .outer { margin-left: 20px; margin-top: 20px; padding-left: 10px; padding-top: 10px; border-top-width: 2px; border-left-width: 2px; }
                    .inner { margin-left: 5px; margin-top: 5px; }
                  "))
                 (body
                  (div :class "outer"
                       (div :class "inner" "Content"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (body-node (car (plist-get layout-tree :children)))
         (outer-node (car (plist-get body-node :children)))
         (inner-node (car (plist-get outer-node :children)))
         (body-pos (plist-get body-node :position))
         (outer-pos (plist-get outer-node :position))
         (inner-pos (plist-get inner-node :position)))
    
    ;; body 位置从 (0, 0) 开始
    (should-equal (plist-get body-pos :x) 0)
    (should-equal (plist-get body-pos :y) 0)
    
    ;; 外层元素位置: body_x + outer_margin-left = 0 + 20 = 20
    (should-equal (plist-get outer-pos :x) 20)
    (should-equal (plist-get outer-pos :y) 20)
    
    ;; 内层元素位置: outer-x + outer-border + outer-padding + inner-margin
    ;; = 20 + 2 + 10 + 5 = 37
    (should-equal (plist-get inner-pos :x) 37)
    (should-equal (plist-get inner-pos :y) 37)))

;;; Layout Tree Utility Tests

(ert-deftest etaf-layout-test-layout-walk ()
  "Test layout tree traversal."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (body
                  (div (div "Nested"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (count 0))
    
    (etaf-layout-walk layout-tree
      (lambda (_node)
        (cl-incf count)))
    
    ;; 应该遍历 html, body, div, div 共 4 个节点
    (should (>= count 4))))

(ert-deftest etaf-layout-test-layout-to-string ()
  "Test layout tree string conversion."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (body
                  (div "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (layout-str (etaf-layout-to-string layout-tree)))
    
    (should (stringp layout-str))
    (should (string-match-p "<html>" layout-str))
    (should (string-match-p "<body>" layout-str))
    (should (string-match-p "<div>" layout-str))))

;;; Integration Tests

(ert-deftest etaf-layout-test-full-pipeline ()
  "Test complete pipeline from TML to layout."
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 800px; padding: 20px; }
                    .container { margin: 10px; padding: 15px; }
                    .box { width: 200px; height: 100px; }
                  "))
                 (body
                  (div :class "container"
                       (div :class "box" "Content"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    ;; 验证完整的流程
    (should dom)
    (should cssom)
    (should render-tree)
    (should layout-tree)
    
    ;; 验证布局树有子节点
    (should (plist-get layout-tree :children))
    
    ;; 验证每个节点都有必要的属性
    (etaf-layout-walk layout-tree
      (lambda (node)
        (should (plist-get node :render-node))
        (should (plist-get node :box-model))
        (should (plist-get node :position))))))

(provide 'etaf-layout-tests)
;;; etaf-layout-tests.el ends here
