;;; etaf-css-cache-tests.el --- Tests for CSS caching -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-cache)
(require 'etaf-tml)
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
   (hash-table-p (plist-get cssom :cache))))

;;; 测试计算样式缓存命中

(should
 (let* ((test-dom (etaf-etml-to-dom
                   '(div :style "color: red;" "Text")))
        (cssom (etaf-css-build-cssom test-dom))
        (cache (plist-get cssom :cache)))
   ;; 第一次计算
   (etaf-css-get-computed-style cssom test-dom test-dom)
   ;; 检查缓存命中
   (etaf-css-cache-get cache test-dom)))

(provide 'etaf-css-cache-tests)
;;; etaf-css-cache-tests.el ends here
