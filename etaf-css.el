;;; 将 内联/外部 样式解析为 cssom

(require 'etaf-dom)
(require 'etaf-css-selector)

;;; style 样式应用

;; (defun ecss-dom-set-styles (node styles)
;;   "为DOM节点设置CSS样式。NODE是DOM节点，STYLES
;; 是样式列表 ((property . value) ...)。"
;;   (when (and node (listp node))
;;     (let* ((attrs (dom-attributes node))
;;            (style-attr (cdr (assq 'style attrs)))
;;            (style-map (ecss-dom-parse-style-string
;;                        (or style-attr ""))))
;;       ;; 合并新样式
;;       (dolist (style styles)
;;         (setq style-map (ecss-dom-set-style-property 
;;                          style-map (car style) (cdr style))))
;;       ;; 更新style属性
;;       (let ((new-style-string (ecss-dom-style-map-to-string style-map)))
;;         (if attrs
;;             (let ((style-assoc (assq 'style attrs)))
;;               (if style-assoc
;;                   (setcdr style-assoc new-style-string)
;;                 ;; 如果有属性列表但没有style属性，添加到属性列表
;;                 (setcdr attrs (cons (cons 'style new-style-string)
;;                                     (cdr attrs)))))
;;           ;; 如果没有属性，创建属性列表
;;           (setcar (cdr node)
;;                   (list (cons 'style new-style-string))))))))

;; (defun ecss-dom-apply-style (dom selector-string styles)
;;   "为DOM中匹配选择器的节点应用CSS样式。
;; DOM是要操作的DOM树，SELECTOR-STRING是CSS选择器字符串，
;; STYLES是要应用的样式列表，格式为 ((property . value) ...)。

;; 示例：
;;   (ecss-dom-apply-style dom \".button\"
;;     '((color . \"red\") (font-size . \"14px\")))"
;;   (let ((nodes (ecss-dom-query-selector-all dom selector-string)))
;;     (dolist (node nodes)
;;       (ecss-dom-set-styles node styles))
;;     nodes))

;; (defun ecss-dom-parse-style-string (style-string)
;;   "解析CSS style属性字符串为属性映射表。
;; 返回一个alist: ((property . value) ...)。"
;;   (let ((result '())
;;         (declarations (split-string style-string ";" t)))
;;     (dolist (decl declarations)
;;       (when (string-match "\\s-*\\([^:]+\\)\\s-*:\\s-*\\(.+\\)\\s-*" decl)
;;         (let ((prop (match-string 1 decl))
;;               (value (match-string 2 decl)))
;;           (push (cons (intern prop) value) result))))
;;     (nreverse result)))

;; (defun ecss-dom-set-style-property (style-map property value)
;;   "在样式映射表中设置或更新属性。"
;;   (let ((existing (assq property style-map)))
;;     (if existing
;;         (setcdr existing value)
;;       (setq style-map (append style-map (list (cons property value)))))
;;     style-map))

;; (defun ecss-dom-style-map-to-string (style-map)
;;   "将样式映射表转换为CSS style字符串。"
;;   (mapconcat (lambda (pair)
;;                (format "%s: %s" (car pair) (cdr pair)))
;;              style-map "; "))

;; (defun ecss-dom-get-style (node property)
;;   "获取DOM节点的指定CSS属性值。
;; NODE是DOM节点，PROPERTY是CSS属性名（symbol）。"
;;   (when (and node (listp node))
;;     (let* ((attrs (dom-attributes node))
;;            (style-attr (cdr (assq 'style attrs)))
;;            (style-map (ecss-dom-parse-style-string
;;                        (or style-attr ""))))
;;       (cdr (assq property style-map)))))

(provide 'etaf-css)
