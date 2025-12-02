;;; etaf-layout-example.el --- Comprehensive layout system example -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; 这是一个完整的复杂布局示例，展示了从 TML 到最终渲染的完整流程：
;;
;; TML → DOM → CSSOM → 渲染树 → 布局树 → 绘制
;;
;; 本示例创建了一个复杂的页面布局，包括：
;; - 页面容器
;; - 头部导航栏
;; - 主内容区域（带侧边栏）
;; - 文章卡片（带图片区域）
;; - 页脚
;;
;; 每个元素都有完整的盒模型（content, padding, border, margin）

;;; Code:

(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-layout)

;;; 示例 1: 简单的两列布局

(defun etaf-layout-example-simple ()
  "简单的两列布局示例。"
  (interactive)
  (message "=== 简单两列布局示例 ===\n")
  
  ;; 1. 创建 TML 结构
  (let* ((simple-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { margin-left: 0px; margin-right: 0px; margin-top: 0px; margin-bottom: 0px; padding-left: 20px; padding-right: 20px; padding-top: 20px; padding-bottom: 20px; width: 800px; }
                .container { 
                  padding-left: 10px;
                  padding-right: 10px;
                  padding-top: 10px;
                  padding-bottom: 10px;
                  border-top-width: 2px;
                  border-bottom-width: 2px;
                  margin-bottom: 10px;
                }
                .box { 
                  width: 200px; 
                  height: 100px; 
                  padding-left: 10px;
                  padding-right: 10px;
                  padding-top: 10px;
                  padding-bottom: 10px;
                  margin-left: 5px;
                  margin-right: 5px;
                  margin-top: 5px;
                  margin-bottom: 5px;
                  border-top-width: 1px;
                }
              "))
             (body
              (div :class "container"
                   (div :class "box" "Box 1")
                   (div :class "box" "Box 2"))))))
         
         ;; 2. 构建 CSSOM
         (simple-cssom (etaf-css-build-cssom simple-dom))
         
         ;; 3. 直接构建布局树（不再需要单独的渲染树）
         (simple-layout-tree (etaf-layout-build-tree 
                              simple-dom
                              simple-cssom
                              '(:width 1024 :height 768))))
    
    ;; 5. 打印布局树结构
    (message "布局树结构：\n%s\n" (etaf-layout-to-string simple-layout-tree))
    
    ;; 6. 遍历并打印每个元素的盒模型信息
    (message "\n元素盒模型详情：")
    (etaf-layout-walk simple-layout-tree
                      (lambda (node)
                        (let* ((tag (dom-tag node))
                               (box-model (etaf-layout-get-box-model node))
                               (position (etaf-layout-get-position node))
                               (content (plist-get box-model :content))
                               (padding (plist-get box-model :padding))
                               (border (plist-get box-model :border))
                               (margin (plist-get box-model :margin)))
                          (message "\n<%s>:" tag)
                          (message "  位置: (%d, %d)" 
                                   (plist-get position :x) 
                                   (plist-get position :y))
                          (message "  内容: %dx%d" 
                                   (plist-get content :width) 
                                   (plist-get content :height))
                          (message "  内边距: %d %d %d %d" 
                                   (plist-get padding :top)
                                   (plist-get padding :right)
                                   (plist-get padding :bottom)
                                   (plist-get padding :left))
                          (message "  边框: %d %d %d %d" 
                                   (plist-get border :top-width)
                                   (plist-get border :right-width)
                                   (plist-get border :bottom-width)
                                   (plist-get border :left-width))
                          (message "  外边距: %d %d %d %d" 
                                   (plist-get margin :top)
                                   (plist-get margin :right)
                                   (plist-get margin :bottom)
                                   (plist-get margin :left))
                          (message "  总尺寸: %dx%d"
                                   (etaf-box-model-total-width box-model)
                                   (etaf-box-model-total-height box-model)))))))

;;; 示例 2: 复杂的页面布局

(defun etaf-layout-example-complex ()
  "复杂的页面布局示例，包含头部、主内容区、侧边栏和页脚。"
  (interactive)
  (message "\n\n=== 复杂页面布局示例 ===\n")
  
  ;; 1. 创建复杂的 TML 结构
  (let* ((complex-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { 
                  margin-left: 0px;
                  margin-right: 0px;
                  margin-top: 0px;
                  margin-bottom: 0px;
                  padding-left: 0px;
                  padding-right: 0px;
                  padding-top: 0px;
                  padding-bottom: 0px;
                  width: 1000px;
                }
                
                .header {
                  width: 100%;
                  height: 80px;
                  padding-left: 40px;
                  padding-right: 40px;
                  padding-top: 20px;
                  padding-bottom: 20px;
                  margin-bottom: 20px;
                  border-bottom-width: 3px;
                  border-bottom-color: navy;
                }
                
                .content-area {
                  width: 680px;
                  margin-right: 20px;
                }
                
                .article {
                  width: 100%;
                  margin-bottom: 30px;
                  padding-left: 20px;
                  padding-right: 20px;
                  padding-top: 20px;
                  padding-bottom: 20px;
                  border-top-width: 1px;
                  border-right-width: 1px;
                  border-bottom-width: 1px;
                  border-left-width: 1px;
                }
                
                .sidebar {
                  width: 260px;
                  padding-left: 20px;
                  padding-right: 20px;
                  padding-top: 20px;
                  padding-bottom: 20px;
                  border-left-width: 1px;
                }
                
                .footer {
                  width: 100%;
                  height: 100px;
                  padding-left: 40px;
                  padding-right: 40px;
                  padding-top: 30px;
                  padding-bottom: 30px;
                  margin-top: 40px;
                  border-top-width: 2px;
                }
              "))
             (body
              (div :class "header" "Header")
              (div :class "content-area"
                   (div :class "article" "Article 1")
                   (div :class "article" "Article 2"))
              (div :class "sidebar" "Sidebar")
              (div :class "footer" "Footer")))))
         
         ;; 2. 构建 CSSOM
         (complex-cssom (etaf-css-build-cssom complex-dom))
         
         ;; 3. 直接构建布局树
         (complex-layout-tree (etaf-layout-build-tree 
                              complex-dom
                              complex-cssom
                              '(:width 1024 :height 768))))
    
    ;; 5. 打印布局树结构
    (message "完整布局树：\n%s\n" (etaf-layout-to-string complex-layout-tree))
    
    ;; 6. 统计信息
    (let ((node-count 0)
          (total-width 0))
      (etaf-layout-walk complex-layout-tree
        (lambda (node)
          (cl-incf node-count)
          (let ((box-model (plist-get node :box-model)))
            (cl-incf total-width (etaf-box-model-total-width box-model)))))
      (message "\n布局统计：")
      (message "  总节点数: %d" node-count)
      (message "  累计宽度: %d" total-width))))

;;; 运行所有示例

(defun etaf-layout-run-all-examples ()
  "运行所有布局示例。"
  (interactive)
  (etaf-layout-example-simple)
  (etaf-layout-example-complex)
  (message "\n\n=== 所有示例运行完毕 ==="))

;; 如果直接加载此文件，运行示例
(when (and (boundp 'load-file-name) load-file-name)
  (etaf-layout-run-all-examples))

(provide 'etaf-layout-example)
;;; etaf-layout-example.el ends here
