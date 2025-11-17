;;; etaf-css-example.el --- Usage examples for etaf-css -*- lexical-binding: t; -*-

;;; Commentary:
;; 这个文件展示了 etaf-css.el 的使用示例

;;; Code:

(require 'etaf-css)
(require 'etaf-tml)

;;; 示例 1: 解析 CSS 声明

;; 解析简单的 CSS 声明字符串
(setq example-declarations 
      (etaf-css-parse-declarations "color: red; font-size: 14px; margin: 10px;"))
;; => ((color . "red") (font-size . "14px") (margin . "10px"))

;;; 示例 2: 从 DOM 构建 CSSOM

;; 创建一个同时包含内联样式和 <style> 标签的 DOM
(setq example-dom
      (etaf-tml-to-dom
       '(html
         (head
          (style "
              .header { background: navy; color: white; padding: 20px; }
              .highlight { background: yellow; }
            "))
         (body
          (div :class "header" :style "border-bottom: 2px solid gold;"
               (h1 "欢迎"))
          (div :class "content"
               (p :class "highlight" :style "padding: 10px;" "这是高亮内容"))))))

;; 构建 CSSOM
(setq cssom (etaf-css-build-cssom example-dom))

;;; 示例 3: 查询和计算样式

;; 获取 .highlight 元素
(setq highlight-node (dom-by-class example-dom "highlight"))

;; 计算该节点的最终样式（层叠后）
(setq computed-style 
      (etaf-css-get-computed-style cssom highlight-node example-dom))
;; => 返回合并后的样式，内联样式会覆盖外部样式

(provide 'etaf-css-example)
;;; etaf-css-example.el ends here
