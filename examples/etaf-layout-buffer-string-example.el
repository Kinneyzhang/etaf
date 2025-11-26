;;; etaf-layout-buffer-string-example.el --- Example of layout buffer string generation -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; 这个示例展示了如何使用 etaf-layout-to-buffer-string 函数
;; 将布局树转换为可直接插入 Emacs buffer 的字符串。
;;
;; 这种方式通过文本拼接来生成最终的布局，而不是使用精确的 x,y 坐标，
;; 更适合在 Emacs buffer 中进行渲染。

;;; Code:

(require 'etaf-tml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)

;;; 示例 1: 简单的布局字符串生成

(defun etaf-layout-buffer-string-example-simple ()
  "简单的布局字符串生成示例。"
  (interactive)
  (message "=== 简单布局字符串生成示例 ===\n")
  
  ;; 1. 创建 TML 结构
  (let* ((simple-dom
          (etaf-tml-to-dom
           '(html
             (head
              (style "
                body { 
                  width: 300px; 
                  height: 100px; 
                  padding-left: 20px; 
                  padding-right: 20px; 
                  padding-top: 10px; 
                  padding-bottom: 10px; 
                  margin-left: 10px;
                  margin-right: 10px;
                  border-left-width: 2px;
                  border-right-width: 2px;
                }
              "))
             (body "Hello, ETAF!"))))
         
         ;; 2. 构建 CSSOM
         (simple-cssom (etaf-css-build-cssom simple-dom))
         
         ;; 3. 构建渲染树
         (simple-render-tree (etaf-render-build-tree simple-dom simple-cssom))
         
         ;; 4. 构建布局树
         (simple-layout-tree (etaf-layout-build-tree 
                             simple-render-tree 
                             '(:width 1024 :height 768)))
         
         ;; 5. 生成 buffer 字符串
         (buffer-string (etaf-layout-to-buffer-string simple-layout-tree)))
    
    ;; 6. 在新 buffer 中显示结果
    (with-current-buffer (get-buffer-create "*ETAF Layout Example*")
      (erase-buffer)
      (insert buffer-string)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    
    (message "布局字符串已生成并显示在 *ETAF Layout Example* buffer 中")))

;;; 示例 2: 嵌套布局的字符串生成

(defun etaf-layout-buffer-string-example-nested ()
  "嵌套布局的字符串生成示例。"
  (interactive)
  (message "\n\n=== 嵌套布局字符串生成示例 ===\n")
  
  ;; 1. 创建嵌套的 TML 结构
  (let* ((nested-dom
          (etaf-tml-to-dom
           '(html
             (head
              (style "
                body { 
                  width: 400px; 
                  padding-left: 20px; 
                  padding-right: 20px; 
                  padding-top: 20px;
                  padding-bottom: 20px;
                }
                .container { 
                  width: 100%;
                  padding-left: 10px;
                  padding-right: 10px;
                  padding-top: 10px;
                  padding-bottom: 10px;
                  margin-bottom: 20px;
                  border-left-width: 1px;
                  border-right-width: 1px;
                }
                .box { 
                  width: 100%; 
                  height: 50px; 
                  padding-left: 10px;
                  padding-right: 10px;
                  margin-bottom: 10px;
                  border-left-width: 2px;
                }
              "))
             (body
              (div :class "container"
                   (div :class "box" "Box 1")
                   (div :class "box" "Box 2")
                   (div :class "box" "Box 3"))))))
         
         ;; 2. 构建 CSSOM
         (nested-cssom (etaf-css-build-cssom nested-dom))
         
         ;; 3. 构建渲染树
         (nested-render-tree (etaf-render-build-tree nested-dom nested-cssom))
         
         ;; 4. 构建布局树
         (nested-layout-tree (etaf-layout-build-tree 
                             nested-render-tree 
                             '(:width 1024 :height 768)))
         
         ;; 5. 生成 buffer 字符串
         (buffer-string (etaf-layout-to-buffer-string nested-layout-tree)))
    
    ;; 6. 在新 buffer 中显示结果
    (with-current-buffer (get-buffer-create "*ETAF Nested Layout Example*")
      (erase-buffer)
      (insert buffer-string)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    
    (message "嵌套布局字符串已生成并显示在 *ETAF Nested Layout Example* buffer 中")))

;;; 示例 3: 对比坐标方式和字符串拼接方式

(defun etaf-layout-buffer-string-example-comparison ()
  "对比坐标方式（etaf-layout-to-string）和字符串拼接方式（etaf-layout-to-buffer-string）。"
  (interactive)
  (message "\n\n=== 坐标方式 vs 字符串拼接方式 ===\n")
  
  ;; 创建相同的布局
  (let* ((dom (etaf-tml-to-dom
               '(html
                 (head
                  (style "
                    body { width: 200px; padding-left: 10px; }
                    .box { width: 100%; height: 50px; margin-bottom: 10px; }
                  "))
                 (body
                  (div :class "box" "Box 1")
                  (div :class "box" "Box 2")))))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree render-tree '(:width 1024 :height 768))))
    
    ;; 1. 坐标方式 - 显示位置信息
    (message "坐标方式（用于调试和分析）：\n%s\n" 
             (etaf-layout-to-string layout-tree))
    
    ;; 2. 字符串拼接方式 - 实际渲染
    (let ((buffer-string (etaf-layout-to-buffer-string layout-tree)))
      (message "字符串拼接方式（用于 Emacs buffer 渲染）：")
      (message "生成的字符串长度: %d 字符" (length buffer-string))
      (message "生成的字符串行数: %d 行" 
               (length (split-string buffer-string "\n")))
      
      ;; 显示在 buffer 中
      (with-current-buffer (get-buffer-create "*ETAF Comparison*")
        (erase-buffer)
        (insert "=== 使用字符串拼接方式生成的布局 ===\n\n")
        (insert buffer-string)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; 运行所有示例

(defun etaf-layout-buffer-string-run-all-examples ()
  "运行所有布局字符串生成示例。"
  (interactive)
  (etaf-layout-buffer-string-example-simple)
  (sit-for 2)
  (etaf-layout-buffer-string-example-nested)
  (sit-for 2)
  (etaf-layout-buffer-string-example-comparison)
  (message "\n\n=== 所有示例运行完毕 ==="))

;; 提供便捷的交互式命令
(defalias 'etaf-demo-buffer-string 'etaf-layout-buffer-string-example-simple)

(provide 'etaf-layout-buffer-string-example)
;;; etaf-layout-buffer-string-example.el ends here
