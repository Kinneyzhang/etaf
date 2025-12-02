;;; etaf-render-tests.el --- Tests for layout tree style handling -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for layout tree construction and style operations
;; (formerly layout tree tests - now integrated into layout)

;;; Code:

(require 'ert)
(require 'etaf-layout)
(require 'etaf-etml)
(require 'etaf-css)

;;; Test data

(defvar etaf-render-tests-dom-simple
  (etaf-etml-to-dom
   '(html
     (head
      (style "div { color: blue; display: block; }"))
     (body
      (div "Content")
      (span "More text"))))
  "Simple test DOM.")

(defvar etaf-render-tests-dom-hidden
  (etaf-etml-to-dom
   '(html
     (head
      (style "div { color: red; } .hidden { display: none; }"))
     (body
      (div "Visible")
      (div :class "hidden" "Hidden")
      (span "Visible span"))))
  "DOM with hidden elements.")

(defvar etaf-render-tests-dom-nested
  (etaf-etml-to-dom
   '(html
     (head
      (style "div { display: block; } span { display: inline; }"))
     (body
      (div
       (span "Text 1")
       (div
        (span "Text 2"))))))
  "Nested DOM structure.")

;;; Tests

(ert-deftest etaf-layout-build-tree-simple ()
  "测试简单布局树构建。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom) '(:width 1024 :height 768)))
    (should layout-tree)
    (should (eq (dom-tag layout-tree) 'html))
    ;; computed-style 可以是 nil（空列表）或包含样式的 alist
    (should (assq 'render-style (dom-attributes layout-tree)))
    (should (dom-children layout-tree))))

(ert-deftest etaf-render-node-filtering ()
  "测试布局树过滤不可见节点。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (all-tags '()))
    ;; 收集所有标签
    (etaf-layout-walk layout-tree
      (lambda (node)
        (push (dom-tag node) all-tags)))
    ;; head, script, style 等应该被过滤掉
    (should-not (memq 'head all-tags))
    (should-not (memq 'style all-tags))
    ;; body 和 div 应该存在
    (should (memq 'body all-tags))
    (should (memq 'div all-tags))))

(ert-deftest etaf-render-display-none ()
  "测试 display: none 元素被过滤。"
  (let* ((dom etaf-render-tests-dom-hidden)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (div-nodes '()))
    ;; 找到所有 div 布局节点
    (etaf-layout-walk layout-tree
      (lambda (node)
        (when (eq (dom-tag node) 'div)
          (push node div-nodes))))
    ;; 应该只有一个可见的 div（.hidden 被过滤）
    (should (= (length div-nodes) 1))
    (should-not (string= (etaf-layout-get-display (car div-nodes)) "none"))))

(ert-deftest etaf-layout-get-style ()
  "测试从布局节点获取样式。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (div-node nil))
    ;; 找到 div 节点
    (etaf-layout-walk layout-tree
                      (lambda (node)
                        (when (and (null div-node) (eq (dom-tag node) 'div))
                          (setq div-node node))))
    (should div-node)
    (should (equal (etaf-layout-get-style div-node 'color) "blue"))
    (should (equal (etaf-layout-get-style div-node 'display) "block"))))

(ert-deftest etaf-layout-find-by-tag ()
  "测试按标签查找布局节点。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (spans (etaf-layout-find-by-tag layout-tree 'span)))
    (should (= (length spans) 2))
    (should (cl-every (lambda (node)
                       (eq (dom-tag node) 'span))
                     spans))))

(ert-deftest etaf-layout-find-by-display ()
  "测试按 display 类型查找布局节点。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (block-nodes (etaf-layout-find-by-display layout-tree "block"))
         (inline-nodes (etaf-layout-find-by-display layout-tree "inline")))
    (should (> (length block-nodes) 0))
    (should (> (length inline-nodes) 0))
    (should (cl-every (lambda (node)
                       (string= (etaf-layout-get-display node) "block"))
                     block-nodes))
    (should (cl-every (lambda (node)
                       (string= (etaf-layout-get-display node) "inline"))
                     inline-nodes))))

(ert-deftest etaf-layout-walk ()
  "测试布局树遍历。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (node-count 0))
    (etaf-layout-walk layout-tree
      (lambda (_node)
        (cl-incf node-count)))
    (should (> node-count 0))))

(ert-deftest etaf-layout-stats ()
  "测试布局树统计信息。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (stats (etaf-layout-stats layout-tree)))
    (should (plist-get stats :node-count))
    (should (>= (plist-get stats :node-count) 1))
    (should (plist-get stats :max-depth))
    (should (>= (plist-get stats :max-depth) 0))
    (should (plist-get stats :display-types))
    (should (listp (plist-get stats :display-types)))))

(ert-deftest etaf-layout-to-string ()
  "测试布局树转字符串。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (str (etaf-layout-to-string layout-tree)))
    (should (stringp str))
    (should (> (length str) 0))
    (should (string-match-p "html" str))))

(ert-deftest etaf-render-computed-style-integration ()
  "测试布局节点包含正确的计算样式。"
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { color: blue; font-size: 14px; }
                            #main { color: red; }"))
                 (body
                  (div :id "main" "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (layout-tree (etaf-layout-build-tree dom cssom '(:width 1024 :height 768)))
         (main-node nil))
    ;; 找到 #main 节点 - 布局节点保留了除 class 外的原始 DOM 属性（如 id）
    (etaf-layout-walk layout-tree
                      (lambda (node)
                        (when (and (null main-node) (eq (dom-tag node) 'div))
                          (when (string= (dom-attr node 'id) "main")
                            (setq main-node node)))))
    (should main-node)
    ;; 验证层叠：#main 的 color 应该是 red（覆盖 div 的 blue）
    (should (equal (etaf-layout-get-style main-node 'color) "red"))
    ;; font-size 应该继承自 div 规则
    (should (equal (etaf-layout-get-style main-node 'font-size) "14px"))))

(provide 'etaf-render-tests)
;;; etaf-render-tests.el ends here
