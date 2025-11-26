;;; etaf-layout-buffer-string-tests.el --- Tests for layout buffer string generation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the etaf-layout-to-buffer-string function

;;; Code:

(require 'ert)
(require 'etaf-tml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; Test: Simple layout to buffer string

(ert-deftest etaf-layout-test-simple-buffer-string ()
  "测试简单布局转换为 buffer 字符串。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 100px; height: 50px; padding-left: 10px; padding-right: 10px; }
                  "))
                 (body "Content"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    ;; 验证返回的是字符串
    (should (stringp buffer-string))
    ;; 验证字符串不为空
    (should (> (length buffer-string) 0))))

;;; Test: Layout with margins

(ert-deftest etaf-layout-test-buffer-string-with-margins ()
  "测试带 margin 的布局转换为 buffer 字符串。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 100px; height: 50px; margin-left: 20px; margin-right: 20px; margin-top: 10px; margin-bottom: 10px; }
                  "))
                 (body "Test"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))))

;;; Test: Nested layout

(ert-deftest etaf-layout-test-nested-buffer-string ()
  "测试嵌套布局转换为 buffer 字符串。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 200px; padding-left: 10px; padding-right: 10px; }
                    .box { width: 180px; height: 50px; margin-bottom: 10px; }
                  "))
                 (body
                  (div :class "box" "Box 1")
                  (div :class "box" "Box 2")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证包含多行（因为有两个 box）
    (should (> (length (split-string buffer-string "\n")) 1))))

;;; Test: Layout with padding and border

(ert-deftest etaf-layout-test-buffer-string-with-padding-border ()
  "测试带 padding 和 border 的布局转换为 buffer 字符串。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { 
                      width: 100px; 
                      height: 50px; 
                      padding-left: 10px;
                      padding-right: 10px;
                      padding-top: 5px;
                      padding-bottom: 5px;
                      border-left-width: 2px;
                      border-right-width: 2px;
                    }
                  "))
                 (body "Test"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))))

(provide 'etaf-layout-buffer-string-tests)
;;; etaf-layout-buffer-string-tests.el ends here
