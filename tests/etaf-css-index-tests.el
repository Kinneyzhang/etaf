;;; etaf-css-index-tests.el --- Tests for CSS rule indexing -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-core)
(require 'etaf-css-parser)
(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试索引创建

(should
 (let ((index (etaf-css-index-create)))
   (and (plist-get index :by-tag)
        (plist-get index :by-class)
        (plist-get index :by-id))))

;;; 测试选择器键提取 - 标签

(should
 (let ((keys (etaf-css-index-extract-selector-keys "div")))
   (member 'div (plist-get keys :tags))))

;;; 测试选择器键提取 - 类

(should
 (let ((keys (etaf-css-index-extract-selector-keys ".button")))
   (member "button" (plist-get keys :classes))))

;;; 测试选择器键提取 - ID

(should
 (let ((keys (etaf-css-index-extract-selector-keys "#main")))
   (member "main" (plist-get keys :ids))))

;;; 测试选择器键提取 - 组合

(should
 (let ((keys (etaf-css-index-extract-selector-keys "div.button#main")))
   (and (member 'div (plist-get keys :tags))
        (member "button" (plist-get keys :classes))
        (member "main" (plist-get keys :ids)))))

;;; 测试规则索引构建

(should
 (let* ((rule1 (etaf-css-parse-rule "div { color: red; }"))
        (rule2 (etaf-css-parse-rule ".button { background: blue; }"))
        (rules (list rule1 rule2))
        (index (etaf-css-index-build rules)))
   (and (hash-table-p (plist-get index :by-tag))
        (hash-table-p (plist-get index :by-class))
        (hash-table-p (plist-get index :by-id)))))

;;; 测试候选规则查询

(should
 (let* ((rule1 (etaf-css-parse-rule "div { color: red; }"))
        (rule2 (etaf-css-parse-rule ".button { background: blue; }"))
        (rules (list rule1 rule2))
        (index (etaf-css-index-build rules))
        (test-node '(div ((class . "button")) "test"))
        (candidates (etaf-css-index-query-candidates index test-node)))
   (>= (length candidates) 2)))

;;; 测试 CSSOM 索引集成

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (style "div { color: red; }"))
                     (body (div "Text")))))
        (cssom (etaf-css-build-cssom test-dom))
        (index (dom-attr cssom 'cssom-rule-index)))
   (and (plist-get index :by-tag)
        (plist-get index :by-class)
        (plist-get index :by-id))))

(provide 'etaf-css-index-tests)
;;; etaf-css-index-tests.el ends here
