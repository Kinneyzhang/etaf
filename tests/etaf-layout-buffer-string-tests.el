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
  "测试简单布局转换为 buffer 字符串，验证包含文本内容。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 200px; padding-left: 10px; padding-right: 10px; }
                  "))
                 (body "Test Content"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    ;; 验证返回的是字符串
    (should (stringp buffer-string))
    ;; 验证字符串不为空
    (should (> (length buffer-string) 0))
    ;; 验证包含文本内容
    (should (string-match-p "Test Content" buffer-string))))

;;; Test: Layout with margins

(ert-deftest etaf-layout-test-buffer-string-with-margins ()
  "测试带 margin 的布局转换为 buffer 字符串，验证包含文本内容。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 200px; margin-left: 20px; margin-right: 20px; margin-top: 1px; margin-bottom: 1px; }
                  "))
                 (body "Test with margins"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证包含文本内容
    (should (string-match-p "Test with margins" buffer-string))))

;;; Test: Nested layout

(ert-deftest etaf-layout-test-nested-buffer-string ()
  "测试嵌套布局转换为 buffer 字符串，验证所有文本内容都被渲染。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 300px; padding-left: 10px; padding-right: 10px; }
                    .box { width: 280px; margin-bottom: 1px; }
                  "))
                 (body
                  (div :class "box" "First Box")
                  (div :class "box" "Second Box")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证包含多行（因为有两个 box）
    (should (> (length (split-string buffer-string "\n")) 1))
    ;; 验证包含两个 box 的文本内容
    (should (string-match-p "First Box" buffer-string))
    (should (string-match-p "Second Box" buffer-string))))

;;; Test: Layout with padding and border

(ert-deftest etaf-layout-test-buffer-string-with-padding-border ()
  "测试带 padding 和 border 的布局转换为 buffer 字符串，验证文本正确显示。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { 
                      width: 200px; 
                      padding-left: 10px;
                      padding-right: 10px;
                      padding-top: 1px;
                      padding-bottom: 1px;
                      border-left-width: 2px;
                      border-right-width: 2px;
                    }
                  "))
                 (body "Content with padding and border"))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768)))
         (buffer-string (etaf-layout-to-buffer-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证包含文本内容
    (should (string-match-p "Content with padding and border" buffer-string))))

(provide 'etaf-layout-buffer-string-tests)
;;; etaf-layout-buffer-string-tests.el ends here
