;;; etaf-css.el --- Parse inline and external styles to CSSOM -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, cssom, parser
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 将 DOM 中的内联/外部样式解析为 CSSOM (CSS Object Model)
;;
;; 这是 ETAF CSS 系统的主入口点，整合了以下模块：
;; - etaf-css-parser: CSS 解析（支持 !important 和 @media）
;; - etaf-css-specificity: 选择器特异性计算
;; - etaf-css-cascade: 层叠算法（支持 !important）
;; - etaf-css-inheritance: 属性继承
;; - etaf-css-cache: 计算样式缓存
;; - etaf-css-index: 规则索引（性能优化）
;; - etaf-css-media: 媒体查询支持
;;
;; CSSOM 结构：
;; - inline-rules: 内联样式规则
;; - style-rules: 样式表规则
;; - all-rules: 所有规则（按顺序）
;; - rule-index: 规则索引（按标签、类、ID）
;; - cache: 计算样式缓存
;; - media-env: 媒体查询环境
;;
;; 使用示例：
;;
;;   ;; 从 DOM 构建 CSSOM
;;   (etaf-css-build-cssom dom)
;;   
;;   ;; 解析 CSS 声明（支持 !important）
;;   (etaf-css-parse-declarations "color: red !important; font-size: 14px;")
;;   ;; => ((color "red" t) (font-size "14px" nil))
;;
;;   ;; 查询匹配节点的样式（使用缓存和索引）
;;   (etaf-css-get-computed-style cssom node dom)

;;; Code:

(require 'cl-lib)
(require 'etaf-dom)
(require 'etaf-css-selector)
(require 'etaf-css-media)
(require 'etaf-css-parser)
(require 'etaf-css-cascade)
(require 'etaf-css-inheritance)
(require 'etaf-css-cache)
(require 'etaf-css-index)
(require 'etaf-css-shorthand)
(require 'etaf-tailwind)

;;; 从 DOM 提取样式

(defun etaf-css-extract-inline-styles (dom)
  "从 DOM 树中提取所有内联样式（style 属性）。
返回规则列表，每个规则包含节点引用和样式声明。"
  (let ((rules '()))
    (etaf-dom-map
     (lambda (node)
       (when-let* ((style-attr (dom-attr node 'style))
                   (declarations (etaf-css-parse-declarations style-attr)))
         (when declarations
           ;; 为内联样式生成唯一标识（使用节点的标签、类和ID）
           (let* ((tag (dom-tag node))
                  (id (dom-attr node 'id))
                  (class (dom-attr node 'class))
                  (selector (concat (symbol-name tag)
                                   (when id (concat "#" id))
                                   (when class 
                                     (mapconcat (lambda (c) (concat "." c))
                                               (split-string class) "")))))
             (push (list :selector selector
                        :declarations declarations
                        :specificity '(1 0 0 0)  ; 内联样式最高特异性
                        :source 'inline
                        :node node)
                   rules)))))
     dom)
    (nreverse rules)))

(defun etaf-css-extract-style-tags (dom)
  "从 DOM 树中提取 <style> 标签内的 CSS 规则。
返回规则列表。"
  (let ((rules '())
        (style-nodes (dom-search dom (lambda (node) 
                                       (eq (dom-tag node) 'style)))))
    (dolist (style-node style-nodes)
      (when-let ((css-content (dom-texts style-node))
                 (stylesheet-rules (etaf-css-parse-stylesheet css-content)))
        (setq rules (append rules stylesheet-rules))))
    rules))

;;; CSSOM 构建和查询

(defun etaf-css-build-cssom (dom &optional media-env)
  "从 DOM 树构建 CSSOM (CSS Object Model)。
DOM 是要构建 CSSOM 的 DOM 树。
MEDIA-ENV 是可选的媒体查询环境 alist。
返回包含所有 CSS 规则、索引和缓存的 CSSOM 结构。

CSSOM 结构：
- :inline-rules - 内联样式规则列表
- :style-rules - 样式表规则列表
- :all-rules - 所有规则（按顺序）
- :rule-index - 规则索引（按标签、类、ID）
- :cache - 计算样式缓存
- :media-env - 媒体查询环境"
  (let* ((inline-rules (etaf-css-extract-inline-styles dom))
         (style-rules (etaf-css-extract-style-tags dom))
         (all-rules (append style-rules inline-rules))
         (rule-index (etaf-css-index-build all-rules))
         (cache (etaf-css-cache-create))
         (env (or media-env etaf-css-media-environment)))
    (list :inline-rules inline-rules
          :style-rules style-rules
          :all-rules all-rules
          :rule-index rule-index
          :cache cache
          :media-env env)))

(defun etaf-css-get-rules-for-node (cssom node dom)
  "从 CSSOM 中获取适用于指定节点的所有规则（使用索引优化）。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。
返回适用的规则列表，会过滤掉不匹配的媒体查询规则。"
  (let ((matching-rules '())
        (rule-index (plist-get cssom :rule-index))
        (media-env (plist-get cssom :media-env))
        (etaf-dom--query-root dom))
    
    ;; 1. 首先查询索引获取候选规则（性能优化）
    (let ((candidates (if rule-index
                          (etaf-css-index-query-candidates rule-index node)
                        (plist-get cssom :all-rules))))
      
      ;; 2. 对候选规则进行匹配测试
      (dolist (rule candidates)
        ;; 2.1 检查媒体查询是否匹配
        (let ((media-query (plist-get rule :media)))
          (when (or (null media-query)
                    (etaf-css-media-match-p media-query media-env))
            ;; 媒体查询匹配，继续检查选择器
            (cond
             ;; 内联样式直接匹配节点
             ;; 使用严格的节点身份比较 (eq)，避免不同节点因属性相同而错误匹配
             ;; 这修复了当多个节点具有相同标签和属性时可能导致的样式混淆问题
             ((eq (plist-get rule :source) 'inline)
              (let ((rule-node (plist-get rule :node)))
                (when (eq rule-node node)
                  (push rule matching-rules))))
             ;; 外部样式通过选择器匹配
             ((eq (plist-get rule :source) 'style-tag)
              (condition-case nil
                  (let* ((selector (plist-get rule :selector))
                         (ast (etaf-css-selector-parse selector)))
                    (when ast
                      (let ((first-selector (car (plist-get ast :nodes))))
                        (when (and first-selector
                                  (eq (plist-get first-selector :type) 'selector))
                          ;; 使用支持组合器的匹配函数
                          (when (etaf-css-selector-node-matches-p
                                 node dom first-selector)
                            (push rule matching-rules))))))
                (error nil))))))))
    (nreverse matching-rules)))

(defun etaf-css-get-computed-style (cssom node dom)
  "计算指定节点的最终样式（使用缓存和完整层叠算法）。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。
返回合并后的样式声明列表 ((property . value) ...)。

使用完整的 CSS 层叠算法，包括：
- !important 声明处理
- 选择器特异性比较
- 内联样式优先级
- 文档顺序处理
- 计算样式缓存
- 属性继承
- Tailwind CSS 类名转换
- Tailwind CSS 复合属性展开"
  (let ((cache (plist-get cssom :cache)))
    ;; 1. 尝试从缓存获取
    (or (and cache (etaf-css-cache-get cache node))
        ;; 2. 缓存未命中，计算新样式
        (let* ((rules (etaf-css-get-rules-for-node cssom node dom))
               ;; 3. 使用层叠算法合并规则
               (computed-style (etaf-css-cascade-merge-rules rules))
               ;; 4. 处理 Tailwind CSS 类名并展开复合属性
               ;; 使用 when-let* 组合条件，避免不必要的中间变量
               ;; 使用 etaf-tailwind-classes-to-css-with-mode 以支持 dark: 变体
               (tailwind-style (when-let* ((class-attr (dom-attr node 'class))
                                           (tailwind-raw (etaf-tailwind-classes-to-css-with-mode class-attr)))
                                 (etaf-css--expand-tailwind-shorthand tailwind-raw)))
               ;; 5. 合并 Tailwind 样式到计算样式
               ;; Tailwind 类的优先级介于普通 CSS 规则和内联样式之间
               (computed-with-tailwind
                (if tailwind-style
                    (etaf-css--merge-style-alists computed-style tailwind-style)
                  computed-style))
               ;; 6. 应用属性继承（如果有父元素）
               (parent (dom-parent dom node))
               (final-style
                (if parent
                    (let ((parent-style (etaf-css-get-computed-style
                                         cssom parent dom)))
                      (etaf-css-apply-inheritance
                       computed-with-tailwind parent-style))
                  computed-with-tailwind)))
          ;; 7. 存入缓存
          (when cache
            (etaf-css-cache-set cache node final-style))
          final-style))))

;;; 辅助函数

(defun etaf-css--expand-tailwind-shorthand (style-alist)
  "展开 Tailwind CSS 样式 alist 中的复合属性。
STYLE-ALIST 是 ((property . value) ...) 格式的样式 alist。
返回展开后的样式 alist，其中复合属性（如 border-width、border-color）
会被展开为长形属性（如 border-top-width、border-right-width 等）。"
  (let ((result '()))
    (dolist (prop style-alist)
      (let* ((prop-name (car prop))
             (prop-value (cdr prop))
             ;; 尝试展开复合属性（使用 nil 作为 important 标记）
             (expanded (etaf-css-expand-shorthand prop-name prop-value nil)))
        (if expanded
            ;; 复合属性，添加展开后的声明
            ;; exp-decl 格式为 (prop-name value important)，使用 car/cadr 提取
            (dolist (exp-decl expanded)
              (push (cons (car exp-decl) (cadr exp-decl)) result))
          ;; 非复合属性，直接添加
          (push prop result))))
    (nreverse result)))

(defun etaf-css--merge-style-alists (base-style additional-style)
  "合并两个样式 alist，后者覆盖前者的同名属性。
BASE-STYLE 是基础样式 alist ((property . value) ...)。
ADDITIONAL-STYLE 是要合并的样式 alist。
返回合并后的样式 alist。"
  (let ((result (copy-alist base-style)))
    (dolist (prop additional-style)
      (let ((key (car prop)))
        ;; 删除已存在的同名属性
        (setq result (assq-delete-all key result))
        ;; 追加到末尾
        (setq result (append result (list prop)))))
    result))

(defun etaf-css-rule-to-string (rule)
  "将 CSS 规则转换为字符串形式。"
  (let ((selector (plist-get rule :selector))
        (declarations (plist-get rule :declarations)))
    (format "%s { %s }"
            selector
            (mapconcat (lambda (decl)
                        (let ((prop (nth 0 decl))
                              (value (nth 1 decl))
                              (important (nth 2 decl)))
                          (format "%s: %s%s" 
                                 prop 
                                 value
                                 (if important " !important" ""))))
                      declarations "; "))))

(defun etaf-css-cssom-to-string (cssom)
  "将 CSSOM 转换为可读的字符串形式。"
  (let ((all-rules (plist-get cssom :all-rules)))
    (mapconcat #'etaf-css-rule-to-string all-rules "\n\n")))

(defun etaf-css-clear-cache (cssom)
  "清空 CSSOM 的缓存。
在 DOM 或样式发生变化时应该调用此函数。"
  (when-let ((cache (plist-get cssom :cache)))
    (etaf-css-cache-clear cache)))

(defun etaf-css-add-stylesheet (cssom css-string)
  "向 CSSOM 添加外部 CSS 样式表。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
CSS-STRING 是 CSS 样式表字符串，如 \".box { border: 1px solid red; }\"。
返回更新后的 CSSOM。

使用示例：
  (let* ((dom (etaf-etml-to-dom '(div :class \"box\" \"hello\")))
         (cssom (etaf-css-build-cssom dom))
         (cssom (etaf-css-add-stylesheet cssom \".box { color: red; }\")))
    ;; 现在 .box 元素会有红色文字
    ...)

注意：添加样式表后会清空缓存，以确保新样式能够生效。"
  (when (and css-string (not (string-empty-p css-string)))
    (let* ((new-rules (etaf-css-parse-stylesheet css-string))
           (old-all-rules (plist-get cssom :all-rules))
           ;; 新规则添加到现有规则前面，优先级更高
           (all-rules (append new-rules old-all-rules))
           ;; 重建索引
           (rule-index (etaf-css-index-build all-rules)))
      ;; 更新 CSSOM
      (setq cssom (plist-put cssom :all-rules all-rules))
      (setq cssom (plist-put cssom :style-rules
                             (append new-rules (plist-get cssom :style-rules))))
      (setq cssom (plist-put cssom :rule-index rule-index))
      ;; 清空缓存以使新样式生效
      (etaf-css-clear-cache cssom)))
  cssom)

(provide 'etaf-css)
;;; etaf-css.el ends here
