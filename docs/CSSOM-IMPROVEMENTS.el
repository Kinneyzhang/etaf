;;; CSSOM-IMPROVEMENTS.el --- Proposed improvements to CSSOM -*- lexical-binding: t; -*-

;;; Commentary:
;; 这个文件展示了如何改进当前的 CSSOM 设计，使其更接近浏览器实现

;;; Code:

(require 'cl-lib)

;;; ============================================================================
;;; 1. 特异性计算（CSS Specificity）
;;; ============================================================================

(defun etaf-css-calculate-specificity (selector)
  "计算 CSS 选择器的特异性。
返回 (id-count class-count type-count)。

例如:
  'div'          => (0 0 1)
  '.button'      => (0 1 0)
  '#main'        => (1 0 0)
  'div.button'   => (0 1 1)
  '#main .text'  => (1 1 0)"
  (let ((id-count 0)
        (class-count 0)
        (type-count 0))
    ;; 简化实现：使用正则表达式计数
    (let ((selector-str (if (stringp selector)
                            selector
                          (plist-get selector :selector))))
      ;; 计数 ID 选择器 (#id)
      (setq id-count (length (split-string selector-str "#" t)))
      (when (> id-count 0) (cl-decf id-count))
      
      ;; 计数类选择器 (.class)
      (setq class-count (length (split-string selector-str "\\." t)))
      (when (> class-count 0) (cl-decf class-count))
      
      ;; 计数类型选择器（标签）
      ;; 简化：假设每个空格分隔的部分至少有一个标签
      (let ((parts (split-string selector-str "[ >+~]" t)))
        (dolist (part parts)
          (when (string-match "^[a-z]" part)
            (cl-incf type-count)))))
    
    (list id-count class-count type-count)))

(defun etaf-css-specificity> (spec1 spec2)
  "比较两个特异性，如果 spec1 > spec2 返回 t。
特异性格式: (id-count class-count type-count)。"
  (or (> (nth 0 spec1) (nth 0 spec2))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (> (nth 1 spec1) (nth 1 spec2)))
      (and (= (nth 0 spec1) (nth 0 spec2))
           (= (nth 1 spec1) (nth 1 spec2))
           (> (nth 2 spec1) (nth 2 spec2)))))

;;; ============================================================================
;;; 2. 改进的规则结构
;;; ============================================================================

(defun etaf-css-make-enhanced-rule (selector declarations &optional source important)
  "创建增强的 CSS 规则，包含特异性信息。"
  (list :selector selector
        :declarations declarations
        :source (or source 'style-tag)
        :specificity (etaf-css-calculate-specificity selector)
        :important important
        :node nil))

;;; ============================================================================
;;; 3. 改进的层叠算法
;;; ============================================================================

(defun etaf-css-compare-declarations (decl1 decl2)
  "比较两个声明的优先级。
每个声明格式: (property value source specificity important)。
返回 1 如果 decl1 优先, -1 如果 decl2 优先, 0 如果相同。"
  (let ((important1 (nth 4 decl1))
        (important2 (nth 4 decl2))
        (source1 (nth 2 decl1))
        (source2 (nth 2 decl2))
        (spec1 (nth 3 decl1))
        (spec2 (nth 3 decl2)))
    (cond
     ;; 1. !important 优先
     ((and important1 (not important2)) 1)
     ((and important2 (not important1)) -1)
     ;; 2. inline 样式优先于外部样式
     ((and (eq source1 'inline) (not (eq source2 'inline))) 1)
     ((and (eq source2 'inline) (not (eq source1 'inline))) -1)
     ;; 3. 比较特异性
     ((etaf-css-specificity> spec1 spec2) 1)
     ((etaf-css-specificity> spec2 spec1) -1)
     ;; 4. 特异性相同，后定义的优先（文档顺序）
     (t 0))))

(defun etaf-css-merge-declarations (declarations-list)
  "合并声明列表，应用完整的层叠算法。
DECLARATIONS-LIST 是声明列表的列表，每个声明包含属性、值、来源、特异性等。"
  (let ((property-map (make-hash-table :test 'eq)))
    ;; 收集所有属性
    (dolist (decls declarations-list)
      (dolist (decl decls)
        (let ((prop (car decl)))
          (push decl (gethash prop property-map)))))
    
    ;; 对每个属性应用层叠规则
    (let ((result '()))
      (maphash
       (lambda (prop decl-list)
         (let ((sorted (sort decl-list
                            (lambda (d1 d2)
                              (> (etaf-css-compare-declarations d1 d2) 0)))))
           ;; 使用优先级最高的
           (push (cons prop (nth 1 (car sorted))) result)))
       property-map)
      (nreverse result))))

;;; ============================================================================
;;; 4. 缓存机制
;;; ============================================================================

(defun etaf-css-build-cssom-with-cache (dom)
  "构建带缓存的 CSSOM。"
  (let ((inline-rules (etaf-css-extract-inline-styles dom))
        (style-rules (etaf-css-extract-style-tags dom))
        (cache (make-hash-table :test 'eq)))
    (list :inline-rules inline-rules
          :style-rules style-rules
          :all-rules (append style-rules inline-rules)
          :computed-cache cache)))

(defun etaf-css-get-computed-style-cached (cssom node dom)
  "获取计算样式，使用缓存。"
  (let ((cache (plist-get cssom :computed-cache)))
    (or (gethash node cache)
        (let ((computed (etaf-css-get-computed-style cssom node dom)))
          (puthash node computed cache)
          computed))))

(defun etaf-css-invalidate-cache (cssom node)
  "使特定节点的缓存失效。"
  (let ((cache (plist-get cssom :computed-cache)))
    (remhash node cache)))

;;; ============================================================================
;;; 5. 规则索引（性能优化）
;;; ============================================================================

(defun etaf-css-build-rule-index (rules)
  "为规则构建索引，提高查询性能。
返回: (:by-tag <hash> :by-class <hash> :by-id <hash>)"
  (let ((by-tag (make-hash-table :test 'eq))
        (by-class (make-hash-table :test 'equal))
        (by-id (make-hash-table :test 'equal)))
    (dolist (rule rules)
      (let ((selector (plist-get rule :selector)))
        ;; 简化：提取第一个标签/类/ID
        (cond
         ;; ID 选择器
         ((string-match "#\\([a-zA-Z0-9_-]+\\)" selector)
          (let ((id (match-string 1 selector)))
            (push rule (gethash id by-id))))
         ;; 类选择器
         ((string-match "\\.\\([a-zA-Z0-9_-]+\\)" selector)
          (let ((class (match-string 1 selector)))
            (push rule (gethash class by-class))))
         ;; 标签选择器
         ((string-match "^\\([a-z]+\\)" selector)
          (let ((tag (intern (match-string 1 selector))))
            (push rule (gethash tag by-tag)))))))
    (list :by-tag by-tag
          :by-class by-class
          :by-id by-id)))

(defun etaf-css-query-indexed-rules (index node)
  "使用索引快速查询适用于节点的规则。"
  (let ((tag (dom-tag node))
        (id (dom-attr node 'id))
        (class (dom-attr node 'class))
        (by-tag (plist-get index :by-tag))
        (by-class (plist-get index :by-class))
        (by-id (plist-get index :by-id))
        (rules '()))
    ;; 收集所有可能匹配的规则
    (when tag
      (setq rules (append rules (gethash tag by-tag))))
    (when id
      (setq rules (append rules (gethash id by-id))))
    (when class
      (dolist (c (split-string class))
        (setq rules (append rules (gethash c by-class)))))
    rules))

;;; ============================================================================
;;; 6. 使用示例
;;; ============================================================================

(defun etaf-css-improvements-example ()
  "演示改进的 CSSOM 功能。"
  (interactive)
  
  ;; 示例 1: 特异性计算
  (message "特异性计算:")
  (message "  'div': %S" (etaf-css-calculate-specificity "div"))
  (message "  '.button': %S" (etaf-css-calculate-specificity ".button"))
  (message "  '#main': %S" (etaf-css-calculate-specificity "#main"))
  (message "  'div.button': %S" (etaf-css-calculate-specificity "div.button"))
  (message "  '#main .text': %S" (etaf-css-calculate-specificity "#main .text"))
  
  ;; 示例 2: 特异性比较
  (message "\n特异性比较:")
  (let ((spec1 (etaf-css-calculate-specificity "#main"))
        (spec2 (etaf-css-calculate-specificity ".button")))
    (message "  '#main' > '.button': %S" (etaf-css-specificity> spec1 spec2)))
  
  ;; 示例 3: 创建增强规则
  (message "\n创建增强规则:")
  (let ((rule (etaf-css-make-enhanced-rule 
               "div.container"
               '((color . "red") (font-size . "14px")))))
    (message "  规则: %S" rule)
    (message "  特异性: %S" (plist-get rule :specificity))))

;;; ============================================================================
;;; 7. 性能对比测试
;;; ============================================================================

(defun etaf-css-benchmark-with-cache ()
  "测试缓存带来的性能提升。"
  (interactive)
  (let* ((dom (etaf-etml-to-dom
               '(div :id "test" :style "color: red;"
                     (p "Text 1")
                     (p "Text 2")
                     (p "Text 3"))))
         (cssom (etaf-css-build-cssom-with-cache dom))
         (node (dom-by-id dom "test"))
         (iterations 1000))
    
    ;; 无缓存
    (message "测试无缓存性能...")
    (let ((start (current-time)))
      (dotimes (_ iterations)
        (etaf-css-get-computed-style cssom node dom))
      (let ((elapsed (float-time (time-subtract (current-time) start))))
        (message "  无缓存: %.3f 秒" elapsed)))
    
    ;; 有缓存
    (message "测试有缓存性能...")
    (let ((start (current-time)))
      (dotimes (_ iterations)
        (etaf-css-get-computed-style-cached cssom node dom))
      (let ((elapsed (float-time (time-subtract (current-time) start))))
        (message "  有缓存: %.3f 秒" elapsed)))))

(provide 'CSSOM-IMPROVEMENTS)
;;; CSSOM-IMPROVEMENTS.el ends here
