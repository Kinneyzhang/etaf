;;; etaf-css-example.el --- Usage examples for etaf-css -*- lexical-binding: t; -*-

;;; Commentary:
;; 这个文件展示了 etaf-css.el 的使用示例

;;; Code:

(require 'etaf-css)
(require 'etaf-tml)

;;; 示例 1: 解析 CSS 声明

;; 解析简单的 CSS 声明字符串（新格式支持 !important）
(setq example-declarations 
      (etaf-css-parse-declarations "color: red; font-size: 14px; margin: 10px;"))
;; => ((color "red" nil) (font-size "14px" nil) (margin "10px" nil))

;; 解析带 !important 的声明
(setq example-declarations-important
      (etaf-css-parse-declarations "color: red !important; font-size: 14px;"))
;; => ((color "red" t) (font-size "14px" nil))

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
;; 新版本使用增强的层叠算法，支持 !important、缓存和继承
(setq computed-style 
      (etaf-css-get-computed-style cssom highlight-node example-dom))
;; => 返回合并后的样式 ((property . value) ...)，内联样式会覆盖外部样式
;; => 第二次查询会从缓存中获取，提高性能

;;; 示例 4: 层叠和 !important

;; 创建一个展示层叠规则的 DOM
(setq cascade-dom
      (etaf-tml-to-dom
       '(html
         (head
          (style "
              div { color: blue !important; font-size: 12px; }
              .special { color: green; }
            "))
         (body
          (div :class "special" :style "color: red; font-size: 16px !important;"
               "测试层叠")))))

(setq cascade-cssom (etaf-css-build-cssom cascade-dom))
(setq div-node (dom-by-tag cascade-dom 'div))
(setq cascade-computed (etaf-css-get-computed-style 
                        cascade-cssom div-node cascade-dom))
;; color: blue (样式表的 !important 战胜内联样式的普通声明)
;; font-size: 16px (内联样式的 !important 战胜样式表的普通声明)

;;; 示例 5: 属性继承

;; 创建带继承的 DOM
(setq inherit-dom
      (etaf-tml-to-dom
       '(div :style "color: red; font-size: 14px;"
         (p "这个段落继承了父元素的 color 和 font-size"))))

(setq inherit-cssom (etaf-css-build-cssom inherit-dom))
(setq p-node (dom-by-tag inherit-dom 'p))
(setq inherit-computed (etaf-css-get-computed-style 
                        inherit-cssom p-node inherit-dom))
;; => 包含 (color . "red") 和 (font-size . "14px")，从父元素继承

;;; 示例 6: 清除缓存

;; 当 DOM 或样式发生变化时，清除缓存
(etaf-css-clear-cache cssom)

(provide 'etaf-css-example)
;;; etaf-css-example.el ends here
