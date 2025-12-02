;;; etaf-css-tree-structure-tests.el --- Tests for CSSOM tree structure -*- lexical-binding: t; -*-

;; This file specifically tests that CSSOM uses a tree structure similar to DOM
;; and layout tree, as specified in the design documentation.

(require 'etaf-css)
(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试 CSSOM 树结构基础

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (style "div { color: blue; }"))
                     (body (div :style "color: red;" "Text")))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该保持 DOM 树的基本结构
   (and (listp cssom)
        (symbolp (car cssom))
        (eq (car cssom) 'html))))

;;; 测试 CSSOM 保持 DOM 标签结构

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (title "Test"))
                     (body 
                       (div :id "main" 
                         (p "Paragraph")
                         (span "Span"))))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 的标签应该与 DOM 相同
   (and (eq (dom-tag cssom) 'html)
        (eq (dom-tag (nth 0 (dom-children cssom))) 'head)
        (eq (dom-tag (nth 1 (dom-children cssom))) 'body))))

;;; 测试 CSSOM 在根节点附加属性

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html (body (div "Test")))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 全局属性应该附加在根节点上
   (and (dom-attr cssom 'cssom-ua-rules)
        (dom-attr cssom 'cssom-style-rules)
        (dom-attr cssom 'cssom-inline-rules)
        (dom-attr cssom 'cssom-all-rules)
        (dom-attr cssom 'cssom-rule-index)
        (dom-attr cssom 'cssom-cache)
        (dom-attr cssom 'cssom-media-env))))

;;; 测试 CSSOM 属性类型正确

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (head (style ".test { color: blue; }"))
                     (body (div :class "test" :style "margin: 10px;" "Text")))))
        (cssom (etaf-css-build-cssom test-dom)))
   (and (listp (dom-attr cssom 'cssom-ua-rules))
        (listp (dom-attr cssom 'cssom-style-rules))
        (listp (dom-attr cssom 'cssom-inline-rules))
        (listp (dom-attr cssom 'cssom-all-rules))
        (hash-table-p (dom-attr cssom 'cssom-cache))
        (listp (dom-attr cssom 'cssom-media-env)))))

;;; 测试 CSSOM 子节点保留原始 DOM 结构

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (p "First")
                     (p "Second")
                     (p "Third"))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; 子节点数量应该与原 DOM 相同
   (= (length (dom-children cssom))
      (length (dom-children test-dom)))))

;;; 测试 CSSOM 保留节点属性

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :id "test" :class "container" :data-value "123"
                         "Content")))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; 原始 DOM 属性应该被保留（除了新增的 CSSOM 属性）
   (and (equal (dom-attr cssom 'id) "test")
        (equal (dom-attr cssom 'class) "container")
        (equal (dom-attr cssom 'data-value) "123"))))

;;; 测试可以使用 DOM 函数操作 CSSOM 树

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(html
                     (body
                       (div :id "main"
                         (p :class "text" "Paragraph")
                         (span :class "text" "Span"))))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; 应该可以使用 DOM 查询函数
   (and (dom-by-id cssom "main")
        (= (length (dom-by-class cssom "text")) 2)
        (> (length (dom-by-tag cssom 'p)) 0))))

;;; 测试 CSSOM 树可以被遍历

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (p "1")
                     (p "2")
                     (p "3"))))
        (cssom (etaf-css-build-cssom test-dom))
        (count 0))
   ;; 使用 etaf-dom-map 遍历 CSSOM 树
   (etaf-dom-map
    (lambda (node)
      (when (eq (dom-tag node) 'p)
        (setq count (1+ count))))
    cssom)
   (= count 3)))

;;; 测试 CSSOM 树深度复制（不修改原 DOM）

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "color: red;" "Text")))
        (original-attrs (dom-attributes test-dom))
        (cssom (etaf-css-build-cssom test-dom))
        (dom-attrs-after (dom-attributes test-dom)))
   ;; 原始 DOM 的属性不应该被修改
   (and (not (assq 'cssom-cache original-attrs))
        (not (assq 'cssom-cache dom-attrs-after))
        ;; 但 CSSOM 应该有这些属性
        (dom-attr cssom 'cssom-cache))))

;;; 测试 CSSOM 与 DOM 结构一致性

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
   ;; 验证 CSSOM 保持了相同的树形层次结构
   (and (eq (dom-tag cssom) (dom-tag test-dom))
        (= (length (dom-children cssom)) (length (dom-children test-dom)))
        ;; 验证可以查询到所有重要节点
        (dom-by-id cssom "header")
        (dom-by-id cssom "content")
        (dom-by-id cssom "footer")
        (dom-by-tag cssom 'h1)
        (dom-by-tag cssom 'h2)
        (dom-by-tag cssom 'article))))

;;; 测试 CSSOM 树结构与 Layout Tree 和 Render Tree 模式一致

;; 这个测试验证 CSSOM 遵循与其他树（Layout Tree、Render Tree）相同的设计模式：
;; 1. 基于原始 DOM 的标签节点结构
;; 2. 通过属性附加额外信息
;; 3. 可以用标准 DOM 函数操作

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "display: flex;"
                         (span "Child 1")
                         (span "Child 2"))))
        (cssom (etaf-css-build-cssom test-dom)))
   ;; CSSOM 应该：
   (and 
    ;; 1. 保持 DOM 结构（标签、子节点）
    (eq (dom-tag cssom) 'div)
    (= (length (dom-children cssom)) 2)
    ;; 2. 原始属性保留
    (dom-attr cssom 'style)
    ;; 3. 新增层次特定的属性（CSSOM 属性）
    (dom-attr cssom 'cssom-cache)
    (dom-attr cssom 'cssom-all-rules)
    ;; 4. 可以用统一的 DOM API 操作
    (listp (dom-children cssom))
    (listp (dom-attributes cssom)))))

(provide 'etaf-css-tree-structure-tests)
;;; etaf-css-tree-structure-tests.el ends here
