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
;; - etaf-css-parse: CSS 值解析（单位转换：px, %, em, lh, cw）
;;
;; CSSOM 树结构（DOM 格式，可使用 DOM 库操作）：
;;
;; 根节点：
;; (cssom ((type . root)
;;         (rule-index . ...)
;;         (cache . #<hash-table>)
;;         (media-env . ...))
;;   stylesheet1 stylesheet2 ...)
;;
;; 样式表节点：
;; (stylesheet ((type . stylesheet)
;;              (source . inline|style-tag|ua)
;;              (media . "all")
;;              (href . nil)
;;              (disabled . nil))
;;   rule1 rule2 ...)
;;
;; 规则节点：
;; (rule ((type . style-rule)
;;        (selector . "div.class")
;;        (declarations . ((color "red" nil) ...))
;;        (specificity . (1 0 0 0))
;;        (source . inline|style-tag|ua)
;;        (media . nil)
;;        (node . <dom-node>)))  ; 仅内联样式
;;
;; 使用 DOM 库函数操作 CSSOM：
;; - (dom-tag cssom) => 'cssom
;; - (dom-attr cssom 'cache) => 缓存对象
;; - (dom-children cssom) => 样式表节点列表
;; - (etaf-dom-map func cssom) => 遍历 CSSOM 树
;;
;; 使用示例：
;;
;;   ;; 从 DOM 构建 CSSOM 树
;;   (etaf-css-build-cssom dom)
;;   
;;   ;; 解析 CSS 声明（支持 !important）
;;   (etaf-css-parse-declarations "color: red !important; font-size: 14px;")
;;   ;; => ((color "red" t) (font-size "14px" nil))
;;
;;   ;; 查询匹配节点的样式（使用缓存和索引）
;;   (etaf-css-get-computed-style cssom node dom)
;;
;;   ;; 遍历 CSSOM 树
;;   (etaf-dom-map
;;     (lambda (node)
;;       (when (eq (dom-tag node) 'rule)
;;         (message "Rule: %s" (dom-attr node 'selector))))
;;     cssom)

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
(require 'etaf-css-parse)
(require 'etaf-tailwind)
(require 'etaf-ua-stylesheet)

;;; CSSOM 树节点创建函数

(defun etaf-css-create-cssom-root (&optional media-env)
  "创建 CSSOM 根节点（使用 DOM 格式的树结构）。
MEDIA-ENV 是可选的媒体查询环境 alist。
返回 DOM 格式的 CSSOM 根节点。"
  (let ((cache (etaf-css-cache-create))
        (env (or media-env etaf-css-media-environment)))
    (list 'cssom (list (cons 'type 'root)
                       (cons 'rule-index nil)  ; 延迟构建索引
                       (cons 'cache cache)
                       (cons 'media-env env)))))

(defun etaf-css-create-stylesheet (source &optional media href)
  "创建样式表节点（使用 DOM 格式）。
SOURCE 是样式来源：'ua, 'inline, 或 'style-tag。
MEDIA 是可选的媒体查询字符串，默认为 \"all\"。
HREF 是可选的外部样式表 URL。
返回 DOM 格式的样式表节点。"
  (list 'stylesheet (list (cons 'type 'stylesheet)
                          (cons 'source source)
                          (cons 'media (or media "all"))
                          (cons 'href href)
                          (cons 'disabled nil))))

(defun etaf-css-create-rule (selector declarations specificity source &optional media node)
  "创建样式规则节点（使用 DOM 格式）。
SELECTOR 是 CSS 选择器字符串。
DECLARATIONS 是声明列表 ((property value important) ...)。
SPECIFICITY 是特异性元组 (inline id class type)。
SOURCE 是样式来源：'ua, 'inline, 或 'style-tag。
MEDIA 是可选的媒体查询字符串。
NODE 是可选的 DOM 节点引用（仅用于内联样式）。
返回 DOM 格式的规则节点。"
  (let ((attrs (list (cons 'type 'style-rule)
                     (cons 'selector selector)
                     (cons 'declarations declarations)
                     (cons 'specificity specificity)
                     (cons 'source source)
                     (cons 'media media))))
    (when node
      (push (cons 'node node) attrs))
    (list 'rule attrs)))

;;; 从 DOM 提取样式并构建树

(defun etaf-css-extract-inline-styles (dom)
  "从 DOM 树中提取所有内联样式，构建样式表子树。
DOM 是 DOM 树根节点。
返回内联样式表节点（stylesheet 节点）。"
  (let ((stylesheet (etaf-css-create-stylesheet 'inline))
        (rules '()))
    (etaf-dom-map
     (lambda (node)
       (when-let* ((style-attr (dom-attr node 'style))
                   (declarations (etaf-css-parse-declarations style-attr)))
         (when declarations
           ;; 为内联样式生成唯一标识
           (let* ((tag (dom-tag node))
                  (id (dom-attr node 'id))
                  (class (dom-attr node 'class))
                  (selector (concat (symbol-name tag)
                                   (when id (concat "#" id))
                                   (when class 
                                     (mapconcat (lambda (c) (concat "." c))
                                               (split-string class) "")))))
             (push (etaf-css-create-rule selector
                                         declarations
                                         '(1 0 0 0)  ; 内联样式最高特异性
                                         'inline
                                         nil
                                         node)
                   rules)))))
     dom)
    ;; 将规则添加为样式表的子节点
    (when rules
      (nconc stylesheet (nreverse rules)))
    stylesheet))

(defun etaf-css-extract-inline-styles-compat (dom)
  "从 DOM 树中提取所有内联样式（style 属性）。
返回规则列表（plist 格式），用于向后兼容。
新代码应该使用 etaf-css-extract-inline-styles。"
  (let ((stylesheet (etaf-css-extract-inline-styles dom)))
    ;; 将样式表节点的子规则转换为 plist 列表
    (mapcar #'etaf-css-rule-to-plist (dom-children stylesheet))))

(defun etaf-css-extract-style-tags (dom)
  "从 DOM 树中提取 <style> 标签内的 CSS 规则，构建样式表子树。
DOM 是 DOM 树根节点。
返回样式表节点列表。"
  (let ((stylesheets '())
        (style-nodes (dom-search dom (lambda (node) 
                                       (eq (dom-tag node) 'style)))))
    (dolist (style-node style-nodes)
      (when-let ((css-content (dom-texts style-node))
                 (parsed-rules (etaf-css-parse-stylesheet css-content)))
        ;; 为每个 <style> 标签创建一个样式表节点
        (let ((stylesheet (etaf-css-create-stylesheet 'style-tag)))
          ;; 将解析的规则转换为规则节点
          (dolist (rule-plist parsed-rules)
            (let ((rule-node (etaf-css-create-rule
                              (plist-get rule-plist :selector)
                              (plist-get rule-plist :declarations)
                              (plist-get rule-plist :specificity)
                              (plist-get rule-plist :source)
                              (plist-get rule-plist :media)
                              nil)))
              (nconc stylesheet (list rule-node))))
          (push stylesheet stylesheets))))
    (nreverse stylesheets)))

(defun etaf-css-extract-style-tags-compat (dom)
  "从 DOM 树中提取 <style> 标签内的 CSS 规则。
返回规则列表（plist 格式），用于向后兼容。
新代码应该使用 etaf-css-extract-style-tags。"
  (let ((stylesheets (etaf-css-extract-style-tags dom))
        (rules '()))
    ;; 将所有样式表节点的规则收集到一个列表
    (dolist (stylesheet stylesheets)
      (dolist (rule-node (dom-children stylesheet))
        (when (eq (dom-tag rule-node) 'rule)
          (push (etaf-css-rule-to-plist rule-node) rules))))
    (nreverse rules)))

;;; CSSOM 树辅助函数

(defun etaf-css-get-all-rules (cssom)
  "从 CSSOM 树中提取所有规则节点。
CSSOM 是 CSSOM 根节点。
返回规则节点列表（按优先级顺序：UA < Author < Inline）。"
  (let ((rules '()))
    ;; 遍历所有样式表节点（CSSOM 的子节点）
    (dolist (stylesheet (dom-children cssom))
      (when (eq (dom-tag stylesheet) 'stylesheet)
        ;; 遍历样式表中的所有规则节点
        (dolist (rule (dom-children stylesheet))
          (when (eq (dom-tag rule) 'rule)
            (push rule rules)))))
    (nreverse rules)))

(defun etaf-css-get-stylesheets (cssom)
  "从 CSSOM 树中获取所有样式表节点。
CSSOM 是 CSSOM 根节点。
返回样式表节点列表。"
  (let ((stylesheets '()))
    (dolist (child (dom-children cssom))
      (when (eq (dom-tag child) 'stylesheet)
        (push child stylesheets)))
    (nreverse stylesheets)))

(defun etaf-css-rule-to-plist (rule-node)
  "将规则节点转换为 plist 格式（用于兼容旧代码）。
RULE-NODE 是规则节点。
返回 plist 格式的规则。"
  (list :selector (dom-attr rule-node 'selector)
        :declarations (dom-attr rule-node 'declarations)
        :specificity (dom-attr rule-node 'specificity)
        :source (dom-attr rule-node 'source)
        :media (dom-attr rule-node 'media)
        :node (dom-attr rule-node 'node)))

;;; 向后兼容的 CSSOM 访问器

(defun etaf-css-get-inline-rules (cssom)
  "从 CSSOM 树中提取内联样式规则（plist 格式）。
CSSOM 是 CSSOM 根节点。
返回规则 plist 列表，用于向后兼容。"
  (let ((rules '()))
    (dolist (stylesheet (dom-children cssom))
      (when (and (eq (dom-tag stylesheet) 'stylesheet)
                 (eq (dom-attr stylesheet 'source) 'inline))
        (dolist (rule (dom-children stylesheet))
          (when (eq (dom-tag rule) 'rule)
            (push (etaf-css-rule-to-plist rule) rules)))))
    (nreverse rules)))

(defun etaf-css-get-style-rules (cssom)
  "从 CSSOM 树中提取样式表规则（plist 格式）。
CSSOM 是 CSSOM 根节点。
返回规则 plist 列表，用于向后兼容。"
  (let ((rules '()))
    (dolist (stylesheet (dom-children cssom))
      (when (and (eq (dom-tag stylesheet) 'stylesheet)
                 (memq (dom-attr stylesheet 'source) '(style-tag ua)))
        (dolist (rule (dom-children stylesheet))
          (when (eq (dom-tag rule) 'rule)
            (push (etaf-css-rule-to-plist rule) rules)))))
    (nreverse rules)))

(defun etaf-css-get-all-rules-plist (cssom)
  "从 CSSOM 树中提取所有规则（plist 格式）。
CSSOM 是 CSSOM 根节点。
返回规则 plist 列表，用于向后兼容。"
  (mapcar #'etaf-css-rule-to-plist (etaf-css-get-all-rules cssom)))

;;; CSSOM 构建和查询

(defun etaf-css-build-cssom (dom &optional media-env)
  "从 DOM 树构建 CSSOM (CSS Object Model) 树结构。
DOM 是要构建 CSSOM 的 DOM 树。
MEDIA-ENV 是可选的媒体查询环境 alist。
返回 CSSOM 根节点（使用 DOM 格式的树结构）。

CSSOM 树结构：
- 根节点 (cssom)：包含缓存、索引和环境配置
  - 样式表节点 (stylesheet)：按优先级顺序（UA < Author < Inline）
    - 规则节点 (rule)：包含选择器、声明、特异性等

可以使用 DOM 库函数操作 CSSOM 树：
- (dom-tag cssom) => 'cssom
- (dom-attr cssom 'cache) => 缓存对象
- (dom-children cssom) => 样式表节点列表
- (etaf-dom-map func cssom) => 遍历树

CSS 层叠顺序（从低到高）：
1. User Agent Stylesheet (UA rules)
2. Author Stylesheets (style tags)
3. Inline Styles (style attribute)"
  (let* ((cssom (etaf-css-create-cssom-root media-env))
         ;; 创建 UA 样式表
         (ua-stylesheet (etaf-css-create-stylesheet 'ua))
         ;; 获取 UA 规则并转换为规则节点
         (ua-rules-plist (etaf-ua-stylesheet-get-rules)))
    
    ;; 将 UA 规则添加到 UA 样式表
    (dolist (rule-plist ua-rules-plist)
      (let ((rule-node (etaf-css-create-rule
                        (plist-get rule-plist :selector)
                        (plist-get rule-plist :declarations)
                        (plist-get rule-plist :specificity)
                        'ua
                        (plist-get rule-plist :media)
                        nil)))
        (nconc ua-stylesheet (list rule-node))))
    
    ;; 提取样式表（author stylesheets）
    (let ((author-stylesheets (etaf-css-extract-style-tags dom))
          ;; 提取内联样式表
          (inline-stylesheet (etaf-css-extract-inline-styles dom)))
      
      ;; 按优先级顺序添加样式表到 CSSOM：UA < Author < Inline
      ;; 首先添加 UA 样式表
      (nconc cssom (list ua-stylesheet))
      ;; 然后添加 author 样式表
      (dolist (stylesheet author-stylesheets)
        (nconc cssom (list stylesheet)))
      ;; 最后添加内联样式表
      (when (dom-children inline-stylesheet)  ; 只有当有规则时才添加
        (nconc cssom (list inline-stylesheet)))
      
      ;; 构建规则索引
      (let* ((all-rules (etaf-css-get-all-rules cssom))
             ;; 将规则节点转换为 plist 格式用于构建索引
             (rules-for-index (mapcar (lambda (rule-node)
                                        (etaf-css-rule-to-plist rule-node))
                                      all-rules))
             (rule-index (etaf-css-index-build rules-for-index)))
        ;; 更新 CSSOM 根节点的索引属性
        (let ((attrs (dom-attributes cssom)))
          (setcdr (assq 'rule-index attrs) rule-index)))
      
      cssom)))

(defun etaf-css-get-rules-for-node (cssom node dom)
  "从 CSSOM 树中获取适用于指定节点的所有规则（使用索引优化）。
CSSOM 是由 etaf-css-build-cssom 生成的 CSSOM 根节点。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。
返回适用的规则 plist 列表，会过滤掉不匹配的媒体查询规则。"
  (let ((matching-rules '())
        (rule-index (dom-attr cssom 'rule-index))
        (media-env (dom-attr cssom 'media-env))
        (etaf-dom--query-root dom)
        ;; 标准化节点：如果是包装的节点 ((tag ...))，解包为 (tag ...)
        (normalized-node (if (and (listp node) 
                                  (listp (car node))
                                  (symbolp (car (car node))))
                             (car node)
                           node)))
    
    ;; 1. 获取所有规则节点并转换为 plist
    (let* ((all-rule-nodes (etaf-css-get-all-rules cssom))
           (all-rules-plist (mapcar #'etaf-css-rule-to-plist all-rule-nodes)))
      
      ;; 2. 如果有索引，使用索引查询候选规则
      (let ((candidates (if rule-index
                            (etaf-css-index-query-candidates rule-index normalized-node)
                          all-rules-plist)))
        
        ;; 3. 对候选规则进行匹配测试
        (dolist (rule candidates)
          ;; 3.1 检查媒体查询是否匹配
          (let ((media-query (plist-get rule :media)))
            (when (or (null media-query)
                      (etaf-css-media-match-p media-query media-env))
              ;; 媒体查询匹配，继续检查选择器
              (cond
               ;; 内联样式直接匹配节点
               ((eq (plist-get rule :source) 'inline)
                (let ((rule-node-ref (plist-get rule :node)))
                  (when (eq rule-node-ref normalized-node)
                    (push rule matching-rules))))
               ;; UA 样式和外部样式通过选择器匹配
               ((or (eq (plist-get rule :source) 'ua)
                    (eq (plist-get rule :source) 'style-tag))
                (condition-case nil
                    (let* ((selector (plist-get rule :selector))
                           (ast (etaf-css-selector-parse selector)))
                      (when ast
                        (let ((selectors (plist-get ast :nodes))
                              (matched nil))
                          (dolist (sel selectors)
                            (when (and sel
                                      (eq (plist-get sel :type) 'selector))
                              (when (etaf-css-selector-node-matches-p normalized-node dom sel)
                                (setq matched t))))
                          (when matched
                            (push rule matching-rules)))))
                  (error nil)))))))))
    (nreverse matching-rules)))

(defun etaf-css-get-computed-style (cssom node dom)
  "计算指定节点的最终样式（使用缓存和完整层叠算法）。
CSSOM 是由 etaf-css-build-cssom 生成的 CSSOM 根节点。
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
  (let ((cache (dom-attr cssom 'cache)))
    ;; 1. 尝试从缓存获取
    (or (and cache (etaf-css-cache-get cache node))
        ;; 2. 缓存未命中，计算新样式
        (let* ((rules (etaf-css-get-rules-for-node cssom node dom))
               ;; 3. 使用层叠算法合并规则
               (computed-style (etaf-css-cascade-merge-rules rules))
               ;; 4. 处理 Tailwind CSS 类名并展开复合属性
               (tailwind-style (when-let* ((class-attr (dom-attr node 'class))
                                           (tailwind-raw (etaf-tailwind-classes-to-css-with-mode class-attr)))
                                 (etaf-css--expand-tailwind-shorthand tailwind-raw)))
               ;; 5. 合并 Tailwind 样式到计算样式
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

(defun etaf-css-get-computed-style-dual-mode (cssom node dom)
  "计算指定节点的亮色和暗色两套最终样式。
CSSOM 是由 etaf-css-build-cssom 生成的 CSS 对象模型。
NODE 是要查询的 DOM 节点。
DOM 是根 DOM 节点。

返回一个 plist: (:light LIGHT-STYLE :dark DARK-STYLE)
其中 LIGHT-STYLE 是亮色模式下的计算样式，
DARK-STYLE 是暗色模式下的计算样式。

此函数用于生成支持自动切换的 Emacs face，
当背景模式改变时样式会自动更新。"
  (let* ((rules (etaf-css-get-rules-for-node cssom node dom))
         (computed-style (etaf-css-cascade-merge-rules rules))
         ;; 获取 Tailwind 双模式样式
         (class-attr (dom-attr node 'class))
         (tailwind-dual (when class-attr
                          (etaf-tailwind-classes-to-css-dual-mode class-attr)))
         (tailwind-light (when tailwind-dual
                           (etaf-css--expand-tailwind-shorthand
                            (plist-get tailwind-dual :light))))
         (tailwind-dark (when tailwind-dual
                          (etaf-css--expand-tailwind-shorthand
                           (plist-get tailwind-dual :dark))))
         ;; 合并样式
         (light-style (if tailwind-light
                          (etaf-css--merge-style-alists computed-style tailwind-light)
                        computed-style))
         (dark-style (if tailwind-dark
                         (etaf-css--merge-style-alists computed-style tailwind-dark)
                       computed-style))
         ;; 应用属性继承
         (parent (dom-parent dom node)))
    (when parent
      (let ((parent-dual (etaf-css-get-computed-style-dual-mode cssom parent dom)))
        (setq light-style (etaf-css-apply-inheritance
                           light-style (plist-get parent-dual :light)))
        (setq dark-style (etaf-css-apply-inheritance
                          dark-style (plist-get parent-dual :dark)))))
    (list :light light-style :dark dark-style)))

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
返回合并后的样式 alist。

特殊处理 text-decoration-line 属性：
- 多个值会被组合（例如：\"underline\" + \"overline\" = \"underline overline\"）
- \"none\" 值会重置所有装饰
- 重复的值会被自动去除"
  (let ((result (copy-alist base-style)))
    (dolist (prop additional-style)
      (let ((key (car prop))
            (new-value (cdr prop)))
        (if (and (eq key 'text-decoration-line)
                 (not (string= new-value "none")))
            ;; 特殊处理 text-decoration-line：组合多个值
            (let* ((existing (assq key result))
                   (existing-value (and existing (cdr existing))))
              ;; 删除现有条目
              (setq result (assq-delete-all key result))
              ;; 如果两个值都存在且都不是 "none"，则组合它们
              (let ((combined-value
                     (if (and existing-value
                              (not (string= existing-value "none")))
                         ;; 组合值并去重
                         (let* ((existing-parts (split-string existing-value nil t))
                                (new-parts (split-string new-value nil t))
                                (all-parts (append existing-parts new-parts))
                                (unique-parts (delete-dups all-parts)))
                           (mapconcat #'identity unique-parts " "))
                       ;; 没有现有值或现有值为 "none"，使用新值
                       new-value)))
                ;; 追加组合后的值
                (setq result (append result (list (cons key combined-value))))))
          ;; 标准处理：覆盖
          (progn
            ;; 删除已存在的同名属性
            (setq result (assq-delete-all key result))
            ;; 追加到末尾
            (setq result (append result (list prop)))))))
    result))

(defun etaf-css-rule-to-string (rule)
  "将 CSS 规则转换为字符串形式。
RULE 可以是规则节点或 plist 格式的规则。"
  (let* ((selector (if (listp rule)
                       (if (eq (car rule) 'rule)
                           (dom-attr rule 'selector)
                         (plist-get rule :selector))
                     nil))
         (declarations (if (listp rule)
                           (if (eq (car rule) 'rule)
                               (dom-attr rule 'declarations)
                             (plist-get rule :declarations))
                         nil)))
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
  "将 CSSOM 树转换为可读的字符串形式。
CSSOM 是 CSSOM 根节点。"
  (let ((all-rules (etaf-css-get-all-rules cssom)))
    (mapconcat (lambda (rule-node)
                 (etaf-css-rule-to-string rule-node))
               all-rules "\n\n")))

(defun etaf-css-clear-cache (cssom)
  "清空 CSSOM 的缓存。
在 DOM 或样式发生变化时应该调用此函数。
CSSOM 是 CSSOM 根节点。"
  (when-let ((cache (dom-attr cssom 'cache)))
    (etaf-css-cache-clear cache)))

(defun etaf-css-add-stylesheet (cssom css-string)
  "向 CSSOM 树添加外部 CSS 样式表。
CSSOM 是由 etaf-css-build-cssom 生成的 CSSOM 根节点。
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
    (let* ((parsed-rules (etaf-css-parse-stylesheet css-string))
           ;; 创建新的样式表节点
           (new-stylesheet (etaf-css-create-stylesheet 'style-tag)))
      
      ;; 将解析的规则添加到新样式表
      (dolist (rule-plist parsed-rules)
        (let ((rule-node (etaf-css-create-rule
                          (plist-get rule-plist :selector)
                          (plist-get rule-plist :declarations)
                          (plist-get rule-plist :specificity)
                          (plist-get rule-plist :source)
                          (plist-get rule-plist :media)
                          nil)))
          (nconc new-stylesheet (list rule-node))))
      
      ;; 将新样式表添加到 CSSOM（在内联样式之前）
      ;; 找到内联样式表的位置
      (let* ((children (dom-children cssom))
             (inline-idx (cl-position-if
                          (lambda (child)
                            (and (eq (dom-tag child) 'stylesheet)
                                 (eq (dom-attr child 'source) 'inline)))
                          children)))
        (if inline-idx
            ;; 在内联样式表之前插入
            (let ((before-inline (cl-subseq children 0 inline-idx))
                  (from-inline (cl-subseq children inline-idx)))
              ;; 重建子节点列表
              (setcdr (cdr cssom) (append before-inline (list new-stylesheet) from-inline)))
          ;; 没有内联样式表，直接添加到末尾
          (nconc cssom (list new-stylesheet))))
      
      ;; 重建规则索引
      (let* ((all-rules (etaf-css-get-all-rules cssom))
             (rules-for-index (mapcar #'etaf-css-rule-to-plist all-rules))
             (rule-index (etaf-css-index-build rules-for-index))
             (attrs (dom-attributes cssom)))
        (setcdr (assq 'rule-index attrs) rule-index))
      
      ;; 清空缓存以使新样式生效
      (etaf-css-clear-cache cssom)))
  cssom)

(provide 'etaf-css)
;;; etaf-css.el ends here
