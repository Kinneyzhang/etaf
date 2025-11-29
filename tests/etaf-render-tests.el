;;; etaf-render-tests.el --- Tests for etaf-render -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for render tree construction and operations

;;; Code:

(require 'ert)
(require 'etaf-render)
(require 'etaf-tml)
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

(ert-deftest etaf-render-build-tree-simple ()
  "测试简单渲染树构建。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom)))
    (should render-tree)
    (should (eq (dom-tag render-tree) 'html))
    ;; computed-style 可以是 nil（空列表）或包含样式的 alist
    (should (assq 'render-style (dom-attributes render-tree)))
    (should (dom-children render-tree))))

(ert-deftest etaf-render-node-filtering ()
  "测试渲染树过滤不可见节点。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (all-tags '()))
    ;; 收集所有标签
    (etaf-render-walk render-tree
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
         (render-tree (etaf-render-build-tree dom cssom))
         (div-nodes '()))
    ;; 找到所有 div 渲染节点
    (etaf-render-walk render-tree
      (lambda (node)
        (when (eq (dom-tag node) 'div)
          (push node div-nodes))))
    ;; 应该只有一个可见的 div（.hidden 被过滤）
    (should (= (length div-nodes) 1))
    (should-not (string= (etaf-render-get-display (car div-nodes)) "none"))))

(ert-deftest etaf-render-get-style ()
  "测试从渲染节点获取样式。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (div-node nil))
    ;; 找到 div 节点
    (etaf-render-walk render-tree
                      (lambda (node)
                        (when (and (null div-node) (eq (dom-tag node) 'div))
                          (setq div-node node))))
    (should div-node)
    (should (equal (etaf-render-get-style div-node 'color) "blue"))
    (should (equal (etaf-render-get-style div-node 'display) "block"))))

(ert-deftest etaf-render-find-by-tag ()
  "测试按标签查找渲染节点。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (spans (etaf-render-find-by-tag render-tree 'span)))
    (should (= (length spans) 2))
    (should (cl-every (lambda (node)
                       (eq (dom-tag node) 'span))
                     spans))))

(ert-deftest etaf-render-find-by-display ()
  "测试按 display 类型查找渲染节点。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (block-nodes (etaf-render-find-by-display render-tree "block"))
         (inline-nodes (etaf-render-find-by-display render-tree "inline")))
    (should (> (length block-nodes) 0))
    (should (> (length inline-nodes) 0))
    (should (cl-every (lambda (node)
                       (string= (etaf-render-get-display node) "block"))
                     block-nodes))
    (should (cl-every (lambda (node)
                       (string= (etaf-render-get-display node) "inline"))
                     inline-nodes))))

(ert-deftest etaf-render-walk ()
  "测试渲染树遍历。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (node-count 0))
    (etaf-render-walk render-tree
      (lambda (_node)
        (cl-incf node-count)))
    (should (> node-count 0))))

(ert-deftest etaf-render-stats ()
  "测试渲染树统计信息。"
  (let* ((dom etaf-render-tests-dom-nested)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (stats (etaf-render-stats render-tree)))
    (should (plist-get stats :node-count))
    (should (>= (plist-get stats :node-count) 1))
    (should (plist-get stats :max-depth))
    (should (>= (plist-get stats :max-depth) 0))
    (should (plist-get stats :display-types))
    (should (listp (plist-get stats :display-types)))))

(ert-deftest etaf-render-to-string ()
  "测试渲染树转字符串。"
  (let* ((dom etaf-render-tests-dom-simple)
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (str (etaf-render-to-string render-tree)))
    (should (stringp str))
    (should (> (length str) 0))
    (should (string-match-p "html" str))))

(ert-deftest etaf-render-computed-style-integration ()
  "测试渲染节点包含正确的计算样式。"
  (let* ((dom (etaf-etml-to-dom
               '(html
                 (head
                  (style "div { color: blue; font-size: 14px; }
                            #main { color: red; }"))
                 (body
                  (div :id "main" "Content")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (main-node nil))
    ;; 找到 #main 节点 - 渲染节点保留了原始 DOM 属性
    (etaf-render-walk render-tree
                      (lambda (node)
                        (when (and (null main-node) (eq (dom-tag node) 'div))
                          (when (string= (dom-attr node 'id) "main")
                            (setq main-node node)))))
    (should main-node)
    ;; 验证层叠：#main 的 color 应该是 red（覆盖 div 的 blue）
    (should (equal (etaf-render-get-style main-node 'color) "red"))
    ;; font-size 应该继承自 div 规则
    (should (equal (etaf-render-get-style main-node 'font-size) "14px"))))

(provide 'etaf-render-tests)
;;; etaf-render-tests.el ends here
