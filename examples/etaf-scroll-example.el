;;; etaf-scroll-example.el --- Example of scroll and incremental update -*- lexical-binding: t; -*-

;;; Commentary:

;; This file demonstrates:
;; 1. Interactive scrolling in rendered ETML
;; 2. Incremental update of ETML content in buffer

;;; Code:

(require 'etaf)
(require 'etaf-layout-interactive)

;;; ============================================================
;;; 1. 可滚动内容示例
;;; ============================================================

;; 创建一个带有滚动条的 ETML 元素
;; 内容超出了指定的高度（5lh），所以会出现滚动条

(defvar etaf-scroll-example-etml
  '(div :style "width: 300px; padding: 10px"
     ;; 可滚动的内容区域
     (div :style "overflow-y: auto; height: 5lh; border-width: 1px"
       "Line 1: Hello World
Line 2: This is a test
Line 3: Scrollable content
Line 4: Use n/p to scroll
Line 5: Or mouse wheel
Line 6: Content continues
Line 7: Below visible area
Line 8: Keep scrolling
Line 9: Almost there
Line 10: Last line"))
  "示例 ETML：一个可滚动的内容区域")

(defun etaf-scroll-example-render ()
  "渲染可滚动示例到 buffer 中。
渲染后可以使用:
- n 键或鼠标滚轮向下滚动
- p 键或鼠标滚轮向上滚动"
  (interactive)
  (etaf-render-to-buffer "*etaf-scroll-example*"
                         etaf-scroll-example-etml
                         400 300))

;;; ============================================================
;;; 2. 增量更新示例
;;; ============================================================

;; 增量更新允许你更新 buffer 中特定区域的内容，
;; 而不需要重新渲染整个页面。

(defun etaf-incremental-update-example ()
  "演示增量更新 API 的使用。"
  (interactive)
  (let ((buffer (get-buffer-create "*etaf-incremental-example*")))
    (with-current-buffer buffer
      ;; 初始化缓存
      (etaf-layout-caches-init)
      ;; 插入一些带有 uuid 属性的内容
      (erase-buffer)
      (insert (propertize "Original Text" 'etaf-layout-content-line "text-uuid-1"))
      (insert "\n")
      (insert (propertize "Another Text" 'etaf-layout-content-line "text-uuid-2"))
      ;; 显示 buffer
      (pop-to-buffer buffer))
    
    ;; 展示增量更新
    (message "正在更新文本...")
    (sit-for 1)
    
    ;; 更新第一个区域的文本
    (etaf-buffer-update-text buffer "text-uuid-1" "Updated Text!")
    (message "文本已更新！")
    (sit-for 1)
    
    ;; 更新第二个区域的样式
    (etaf-buffer-update-style buffer "text-uuid-2" 
                              '((color . "red") (font-weight . "bold")))
    (message "样式已更新！")))

;;; ============================================================
;;; 3. 带数据绑定的滚动示例
;;; ============================================================

(defvar etaf-scroll-data-example-template
  '(div :style "width: 400px; padding: 15px"
     (h2 "{{ title }}")
     (div :style "overflow-y: auto; height: 6lh; padding: 5px; background: #f0f0f0"
       :e-for "item in items"
       (p "{{ item }}")))
  "带数据绑定的滚动示例模板")

(defun etaf-scroll-data-example ()
  "渲染带数据的可滚动列表。"
  (interactive)
  (let ((data '(:title "可滚动列表"
                :items ("项目 1: 第一项内容"
                        "项目 2: 第二项内容"
                        "项目 3: 第三项内容"
                        "项目 4: 第四项内容"
                        "项目 5: 第五项内容"
                        "项目 6: 第六项内容"
                        "项目 7: 第七项内容"
                        "项目 8: 第八项内容"
                        "项目 9: 第九项内容"
                        "项目 10: 第十项内容"))))
    (etaf-render-to-buffer-with-data "*etaf-scroll-data-example*"
                                      etaf-scroll-data-example-template
                                      data
                                      500 400)))

;;; ============================================================
;;; 使用说明
;;; ============================================================

;; 运行以下命令来测试各个示例:
;;
;; 1. 基本滚动:
;;    M-x etaf-scroll-example-render
;;    然后将光标移动到可滚动区域，使用 n/p 键或鼠标滚轮滚动
;;
;; 2. 增量更新:
;;    M-x etaf-incremental-update-example
;;    观察文本和样式如何被增量更新
;;
;; 3. 带数据的滚动列表:
;;    M-x etaf-scroll-data-example
;;    渲染一个带有数据绑定的可滚动列表

(provide 'etaf-scroll-example)
;;; etaf-scroll-example.el ends here
