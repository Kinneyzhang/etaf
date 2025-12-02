;;; etaf-css-cache-tests.el --- Tests for CSS caching -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-cache)
(require 'etaf-etml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试缓存创建

(should
 (let ((cache (etaf-css-cache-create)))
   (hash-table-p cache)))

;;; 测试缓存设置和获取

(should
 (let* ((cache (etaf-css-cache-create))
        (test-node '(div nil "test"))
        (test-style '((color . "red") (font-size . "14px"))))
   (etaf-css-cache-set cache test-node test-style)
   (equal (etaf-css-cache-get cache test-node) test-style)))

;;; 测试缓存未命中

(should-equal
 (let ((cache (etaf-css-cache-create))
       (test-node '(div nil "test")))
   (etaf-css-cache-get cache test-node))
 nil)

;;; 测试缓存清空

(should
 (let* ((cache (etaf-css-cache-create))
        (test-node '(div nil "test"))
        (test-style '((color . "red"))))
   (etaf-css-cache-set cache test-node test-style)
   (etaf-css-cache-clear cache)
   (null (etaf-css-cache-get cache test-node))))

;;; 测试缓存大小

(should
 (let* ((cache (etaf-css-cache-create))
        (node1 '(div nil "1"))
        (node2 '(div nil "2")))
   (etaf-css-cache-set cache node1 '((color . "red")))
   (etaf-css-cache-set cache node2 '((color . "blue")))
   (= (etaf-css-cache-size cache) 2)))

;;; 测试 CSSOM 缓存集成

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "color: red;" "Text")))
        (cssom (etaf-css-build-cssom test-dom)))
   (hash-table-p (dom-attr cssom 'cssom-cache))))

;;; 测试计算样式缓存命中

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "color: red;" "Text")))
        (cssom (etaf-css-build-cssom test-dom))
        (cache (dom-attr cssom 'cssom-cache)))
   ;; 第一次计算
   (etaf-css-get-computed-style cssom test-dom test-dom)
   ;; 检查缓存命中
   (etaf-css-cache-get cache test-dom)))

;;; 测试不同标签的字体大小不会互相干扰
;;; 这个测试确保 h1, h2, h3 等标签各自保持正确的 font-size 值

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (h1 "click me")
                     (h2 "click me")
                     (h3 "click me"))))
        (cssom (etaf-css-build-cssom test-dom))
        (children (dom-children test-dom))
        (h1-node (nth 0 children))
        (h2-node (nth 1 children))
        (h3-node (nth 2 children))
        (h1-style (etaf-css-get-computed-style cssom h1-node test-dom))
        (h2-style (etaf-css-get-computed-style cssom h2-node test-dom))
        (h3-style (etaf-css-get-computed-style cssom h3-node test-dom)))
   ;; 检查每个标题有不同的 font-size
   (and (equal (cdr (assq 'font-size h1-style)) "1.6")
        (equal (cdr (assq 'font-size h2-style)) "1.4")
        (equal (cdr (assq 'font-size h3-style)) "1.3"))))

;;; 测试内联样式只匹配对应的节点
;;; 确保内联样式规则使用严格的节点身份匹配 (eq)

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div
                     (p :style "color: red" "First")
                     (p :style "color: blue" "Second"))))
        (cssom (etaf-css-build-cssom test-dom))
        (children (dom-children test-dom))
        (p1-node (nth 0 children))
        (p2-node (nth 1 children))
        (p1-style (etaf-css-get-computed-style cssom p1-node test-dom))
        (p2-style (etaf-css-get-computed-style cssom p2-node test-dom)))
   ;; 两个 p 标签应该有不同的颜色
   (and (equal (cdr (assq 'color p1-style)) "red")
        (equal (cdr (assq 'color p2-style)) "blue"))))

(provide 'etaf-css-cache-tests)
;;; etaf-css-cache-tests.el ends here
