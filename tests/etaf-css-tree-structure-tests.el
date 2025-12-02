;;; etaf-css-tree-structure-tests.el --- Tests for CSSOM flat structure -*- lexical-binding: t; -*-

;; This file tests that CSSOM uses a flat plist structure (not a tree),
;; which is a global rule collection with indexing for efficient querying,
;; similar to Chromium browser's implementation.

(require 'etaf-css)
(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试 CSSOM 扁平结构基础

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (style "div { color: blue; }"))
                     (body (div :style "color: red;" "Text")))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该是一个 plist，不是 DOM 树
   (and (listp cssom)
        ;; plist 的第一个元素应该是 keyword
        (keywordp (car cssom)))))

;;; 测试 CSSOM 是扁平结构不是树结构

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (title "Test"))
                     (body 
                       (div :id "main" 
                         (p "Paragraph")
                         (span "Span"))))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 不应该有 DOM 标签结构
   (and (not (eq (car cssom) 'html))
        ;; CSSOM 应该是一个包含规则的 plist
        (plist-member cssom :all-rules))))

;;; 测试 CSSOM 包含所有必要属性

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html (body (div "Test")))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该包含所有必要的属性
   (and (plist-get cssom :ua-rules)
        (plist-get cssom :style-rules)
        (plist-get cssom :inline-rules)
        (plist-get cssom :all-rules)
        (plist-get cssom :rule-index)
        (plist-get cssom :cache)
        (plist-get cssom :media-env))))

;;; 测试 CSSOM 属性类型正确

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (style ".test { color: blue; }"))
                     (body (div :class "test" :style "margin: 10px;" "Text")))))
        (cssom (etaf-css-build-cssom test-dom)))
   (and (listp (plist-get cssom :ua-rules))
        (listp (plist-get cssom :style-rules))
        (listp (plist-get cssom :inline-rules))
        (listp (plist-get cssom :all-rules))
        (hash-table-p (plist-get cssom :cache))
        (listp (plist-get cssom :media-env)))))

;;; 测试 CSSOM 扁平结构不包含 DOM 树

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (p "First")
                     (p "Second")
                     (p "Third"))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 不应该包含 DOM 子节点
   ;; 它只是一个规则集合的 plist
   (not (plist-member cssom :children))))

;;; 测试 CSSOM 不修改原 DOM

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :id "test" :class "container" :data-value "123"
                         "Content")))
        (original-attrs (dom-attributes test-dom))
        (cssom (etaf-css-build-cssom test-dom))
        (dom-attrs-after (dom-attributes test-dom)))
   ;; 构建 CSSOM 不应该修改原始 DOM
   (and (equal original-attrs dom-attrs-after)
        ;; CSSOM 是独立的数据结构
        (plist-member cssom :all-rules))))

;;; 测试 CSSOM 规则查询功能

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (body
                       (div :id "main"
                         (p :class "text" "Paragraph")
                         (span :class "text" "Span"))))))
        (cssom (etaf-css-build-cssom test-dom))
        (div-node (dom-by-id test-dom "main")))
   ;; 应该可以查询节点的规则
   (listp (etaf-css-get-rules-for-node cssom div-node test-dom))))

;;; 测试 CSSOM 计算样式功能

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (p "1")
                     (p "2")
                     (p "3"))))
        (cssom (etaf-css-build-cssom test-dom))
        (first-p (car (dom-by-tag test-dom 'p))))
   ;; 应该可以计算节点的样式
   (listp (etaf-css-get-computed-style cssom first-p test-dom))))

;;; 测试 CSSOM 不深度复制 DOM

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "color: red;" "Text")))
        (original-attrs (dom-attributes test-dom))
        (cssom (etaf-css-build-cssom test-dom))
        (dom-attrs-after (dom-attributes test-dom)))
   ;; 原始 DOM 的属性不应该被修改
   (and (equal original-attrs dom-attrs-after)
        ;; CSSOM 是独立的扁平数据结构
        (plist-member cssom :cache))))

;;; 测试 CSSOM 包含正确的规则数量

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head
                       (title "Page")
                       (style "body { margin: 0; }"))
                     (body
                       (header :id "header"
                         (h1 "Title"))
                       (main :id "content"
                         (article
                           (h2 "Article")
                           (p "Text")))
                       (footer :id "footer"
                         (p "Footer"))))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该包含规则集合
   (and (listp (plist-get cssom :all-rules))
        ;; 至少应该有 UA 规则和样式表规则
        (> (length (plist-get cssom :all-rules)) 0))))

;;; 测试 CSSOM 扁平结构设计理念

;; 这个测试验证 CSSOM 是扁平的规则集合，参考 Chromium 浏览器实现：
;; 1. CSSOM 就是一个全局的规则集合
;; 2. 通过索引提供快速查询
;; 3. 不需要树形结构，避免不必要的 DOM 复制

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "display: flex;"
                         (span "Child 1")
                         (span "Child 2"))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该：
   (and 
    ;; 1. 是一个 plist，不是 DOM 树
    (keywordp (car cssom))
    ;; 2. 包含规则集合
    (plist-member cssom :all-rules)
    ;; 3. 包含索引用于快速查询
    (plist-member cssom :rule-index)
    ;; 4. 包含缓存用于性能优化
    (plist-member cssom :cache)
    ;; 5. 不包含 DOM 结构信息
    (not (plist-member cssom :tag))
    (not (plist-member cssom :children)))))

(provide 'etaf-css-tree-structure-tests)
;;; etaf-css-tree-structure-tests.el ends here
