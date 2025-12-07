;;; etaf-grid-example.el --- Grid layout examples -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; 这是一个完整的 Grid 布局示例，展示了从 TML 到最终渲染的完整流程。
;;
;; 本示例展示了：
;; - 基本的网格布局
;; - fr 单位的使用
;; - 网格间隙（gap）
;; - 网格项目的跨行跨列
;; - 自动放置
;; - 对齐方式

;;; Code:

(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; 示例 1: 简单的两列网格布局

(defun etaf-grid-example-simple ()
  "简单的两列网格布局示例。"
  (interactive)
  (message "=== 简单两列网格布局示例 ===\n")
  
  ;; 1. 创建 TML 结构
  (let* ((simple-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { margin: 0; padding: 20px; width: 800px; }
                .grid-container { 
                  display: grid;
                  grid-template-columns: 200px 200px;
                  grid-template-rows: 100px 100px;
                  gap: 10px;
                  padding: 10px;
                  border-top-width: 2px;
                  border-bottom-width: 2px;
                  margin-bottom: 10px;
                }
                .grid-item { 
                  padding: 10px;
                  border-top-width: 1px;
                }
              "))
             (body
              (div :class "grid-container"
                   (div :class "grid-item" "Item 1")
                   (div :class "grid-item" "Item 2")
                   (div :class "grid-item" "Item 3")
                   (div :class "grid-item" "Item 4"))))))
         
         ;; 2. 构建 CSSOM
         (simple-cssom (etaf-css-build-cssom simple-dom))
         
         ;; 3. 构建渲染树
         (simple-render-tree (etaf-render-build-tree simple-dom simple-cssom))
         
         ;; 4. 构建布局树
         (simple-layout-tree (etaf-layout-build-tree 
                             simple-render-tree 
                             '(:width 1024 :height 768))))
    
    ;; 5. 打印布局树结构
    (message "布局树结构：\n%s\n" (etaf-layout-to-string simple-layout-tree))
    
    ;; 6. 遍历并打印网格信息
    (message "\n网格布局详情：")
    (etaf-layout-walk simple-layout-tree
                      (lambda (node)
                        (when (dom-attr node 'layout-grid-tracks)
                          (let* ((tracks (dom-attr node 'layout-grid-tracks))
                                 (columns (plist-get tracks :columns))
                                 (rows (plist-get tracks :rows)))
                            (message "\n<grid-container>:")
                            (message "  列宽: %s" columns)
                            (message "  行高: %s" rows)
                            (message "  列间隙: %d" (dom-attr node 'layout-column-gap))
                            (message "  行间隙: %d" (dom-attr node 'layout-row-gap))))))))

;;; 示例 2: 使用 fr 单位的响应式网格

(defun etaf-grid-example-fr-units ()
  "使用 fr 单位的响应式网格示例。"
  (interactive)
  (message "\n\n=== 使用 fr 单位的网格布局示例 ===\n")
  
  (let* ((fr-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { margin: 0; padding: 0; width: 900px; }
                .grid-container {
                  display: grid;
                  grid-template-columns: 1fr 2fr 1fr;
                  grid-template-rows: 100px 200px;
                  gap: 15px;
                  padding: 20px;
                }
                .grid-item {
                  padding: 15px;
                  border-top-width: 1px;
                }
              "))
             (body
              (div :class "grid-container"
                   (div :class "grid-item" "Left Sidebar")
                   (div :class "grid-item" "Main Content")
                   (div :class "grid-item" "Right Sidebar")
                   (div :class "grid-item" "Footer Left")
                   (div :class "grid-item" "Footer Center")
                   (div :class "grid-item" "Footer Right"))))))
         (fr-cssom (etaf-css-build-cssom fr-dom))
         (fr-render-tree (etaf-render-build-tree fr-dom fr-cssom))
         (fr-layout-tree (etaf-layout-build-tree 
                         fr-render-tree 
                         '(:width 1024 :height 768))))
    
    (message "布局树：\n%s\n" (etaf-layout-to-string fr-layout-tree))
    
    (etaf-layout-walk fr-layout-tree
                      (lambda (node)
                        (when (dom-attr node 'layout-grid-tracks)
                          (let* ((tracks (dom-attr node 'layout-grid-tracks))
                                 (columns (plist-get tracks :columns)))
                            (message "\n列宽分配（1fr 2fr 1fr）：")
                            (message "  第1列 (1fr): %d px" (nth 0 columns))
                            (message "  第2列 (2fr): %d px" (nth 1 columns))
                            (message "  第3列 (1fr): %d px" (nth 2 columns))))))))

;;; 示例 3: 网格项目跨行跨列

(defun etaf-grid-example-spanning ()
  "网格项目跨行跨列示例。"
  (interactive)
  (message "\n\n=== 网格项目跨行跨列示例 ===\n")
  
  (let* ((span-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { margin: 0; padding: 20px; width: 640px; }
                .grid-container {
                  display: grid;
                  grid-template-columns: 200px 200px 200px;
                  grid-template-rows: 100px 100px 100px;
                  gap: 10px;
                }
                .header {
                  grid-column: 1 / 4;
                  padding: 10px;
                  border-top-width: 2px;
                }
                .sidebar {
                  grid-row: 2 / 4;
                  padding: 10px;
                  border-left-width: 2px;
                }
                .content {
                  grid-column: 2 / 4;
                  grid-row: 2 / 4;
                  padding: 10px;
                  border-top-width: 1px;
                }
              "))
             (body
              (div :class "grid-container"
                   (div :class "header" "Header (spans 3 columns)")
                   (div :class "sidebar" "Sidebar (spans 2 rows)")
                   (div :class "content" "Main Content (spans 2 columns × 2 rows)"))))))
         (span-cssom (etaf-css-build-cssom span-dom))
         (span-render-tree (etaf-render-build-tree span-dom span-cssom))
         (span-layout-tree (etaf-layout-build-tree 
                           span-render-tree 
                           '(:width 1024 :height 768))))
    
    (message "布局树：\n%s\n" (etaf-layout-to-string span-layout-tree))
    
    (let ((items (dom-non-text-children 
                 (car (dom-non-text-children 
                      (car (dom-non-text-children span-layout-tree)))))))
      (message "\n网格项目位置和跨度：")
      (dolist (item items)
        (when (dom-attr item 'layout-grid-column)
          (message "  %s:" (dom-text item))
          (message "    列: %d, 跨度: %d" 
                   (dom-attr item 'layout-grid-column)
                   (dom-attr item 'layout-grid-column-span))
          (message "    行: %d, 跨度: %d"
                   (dom-attr item 'layout-grid-row)
                   (dom-attr item 'layout-grid-row-span)))))))

;;; 示例 4: 网格对齐方式

(defun etaf-grid-example-alignment ()
  "网格对齐方式示例。"
  (interactive)
  (message "\n\n=== 网格对齐方式示例 ===\n")
  
  (let* ((align-dom
          (etaf-etml-to-dom
           '(html
             (head
              (style "
                body { margin: 0; padding: 20px; width: 700px; }
                .grid-container {
                  display: grid;
                  grid-template-columns: 200px 200px;
                  grid-template-rows: 100px 100px;
                  gap: 20px;
                  justify-items: center;
                  align-items: center;
                  padding: 20px;
                  border-top-width: 2px;
                }
                .grid-item {
                  padding: 10px;
                  border-top-width: 1px;
                }
              "))
             (body
              (div :class "grid-container"
                   (div :class "grid-item" "Centered 1")
                   (div :class "grid-item" "Centered 2")
                   (div :class "grid-item" "Centered 3")
                   (div :class "grid-item" "Centered 4"))))))
         (align-cssom (etaf-css-build-cssom align-dom))
         (align-render-tree (etaf-render-build-tree align-dom align-cssom))
         (align-layout-tree (etaf-layout-build-tree 
                            align-render-tree 
                            '(:width 1024 :height 768))))
    
    (message "布局树：\n%s\n" (etaf-layout-to-string align-layout-tree))
    
    (etaf-layout-walk align-layout-tree
                      (lambda (node)
                        (when (dom-attr node 'layout-grid-tracks)
                          (message "\n对齐方式：")
                          (message "  justify-items: %s" 
                                   (dom-attr node 'layout-justify-items))
                          (message "  align-items: %s"
                                   (dom-attr node 'layout-align-items)))))))

;;; 运行所有示例

(defun etaf-grid-run-all-examples ()
  "运行所有网格布局示例。"
  (interactive)
  (etaf-grid-example-simple)
  (etaf-grid-example-fr-units)
  (etaf-grid-example-spanning)
  (etaf-grid-example-alignment)
  (message "\n\n=== 所有示例运行完毕 ==="))

;; 如果直接加载此文件，运行示例
(when (and (boundp 'load-file-name) load-file-name)
  (etaf-grid-run-all-examples))

(provide 'etaf-grid-example)
;;; etaf-grid-example.el ends here
