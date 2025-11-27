;;; etaf-layout-buffer-string-tests.el --- Tests for layout buffer string generation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the etaf-layout-to-string function

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
         (buffer-string (etaf-layout-to-string layout-tree)))
    
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
         (buffer-string (etaf-layout-to-string layout-tree)))
    
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
         (buffer-string (etaf-layout-to-string layout-tree)))
    
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
         (buffer-string (etaf-layout-to-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证包含文本内容
    (should (string-match-p "Content with padding and border" buffer-string))))

;;; Test: Inline elements should render on the same line

(ert-deftest etaf-layout-test-inline-elements-same-line ()
  "测试 inline 元素（如 span）应该渲染在同一行。"
  (let* ((dom (etaf-tml-to-dom
               '(div
                 (div :class "box"
                      (span "First Span") (span "Second Span")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 410)))
         (buffer-string (etaf-layout-to-string layout-tree)))
    
    (should (stringp buffer-string))
    (should (> (length buffer-string) 0))
    ;; 验证两个 span 内容都存在
    (should (string-match-p "First Span" buffer-string))
    (should (string-match-p "Second Span" buffer-string))
    ;; 验证两个 span 在同一行（它们之间没有换行符）
    ;; 通过检查 "First Span" 和 "Second Span" 之间没有换行符来验证
    (should (string-match-p "First Span.*Second Span" buffer-string))))

;;; Test: Inline styles should be applied correctly

(ert-deftest etaf-layout-test-inline-style-border-top ()
  "测试内联样式 border-top-width 应该正确应用到盒模型。"
  (let* ((dom (etaf-tml-to-dom
               '(div :style "border-top-width: 5px; border-bottom-width: 3px;"
                     "Content with top/bottom border")))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 410)))
         (box-model (etaf-layout-get-box-model layout-tree))
         (border (plist-get box-model :border)))
    
    ;; 验证 border-top-width 正确解析
    (should (= (plist-get border :top-width) 5))
    ;; 验证 border-bottom-width 正确解析
    (should (= (plist-get border :bottom-width) 3))))

;;; Test: Mixed inline and block elements

(ert-deftest etaf-layout-test-mixed-inline-block-elements ()
  "测试混合的 inline 和 block 元素布局。"
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "div { display: block; } span { display: inline; }"))
                 (body
                  (div
                   (span "Inline 1")
                   (span "Inline 2")
                   (div "Block Element")
                   (span "Inline 3"))))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 800)))
         (buffer-string (etaf-layout-to-string layout-tree)))
    
    (should (stringp buffer-string))
    ;; 验证所有内容存在
    (should (string-match-p "Inline 1" buffer-string))
    (should (string-match-p "Inline 2" buffer-string))
    (should (string-match-p "Block Element" buffer-string))
    (should (string-match-p "Inline 3" buffer-string))
    ;; 验证前两个 inline 元素在同一行
    (should (string-match-p "Inline 1.*Inline 2" buffer-string))))

(provide 'etaf-layout-buffer-string-tests)
;;; etaf-layout-buffer-string-tests.el ends here
