;;; etaf-vue2-options-api-examples.el --- Vue 2 Options API examples -*- lexical-binding: t; -*-

;;; Commentary:

;; 本文件展示了 ETAF 组件系统对 Vue 2 选项式 API (Options API) 的支持。
;; 
;; Vue 2 Options API 风格包括：
;; - :data - 返回初始数据的函数（自动响应式）
;; - :methods - 方法对象
;; - :computed - 计算属性对象
;; - :watch - 侦听器对象
;; - :mounted/:updated/:unmounted - 生命周期钩子
;;
;; 示例目录：
;; 1. 基础计数器组件（Options API）
;; 2. 带计算属性的组件
;; 3. 带侦听器的组件
;; 4. 完整的 Todo List（Options API）
;; 5. 对比 Vue 2 Options API 和 Vue 3 Composition API

;;; Code:

(require 'etaf-component)

;;; ============================================================================
;;; 示例 1：基础计数器组件（Vue 2 Options API）
;;; ============================================================================

(defun etaf-vue2-example-1-basic-counter ()
  "示例 1：使用 Vue 2 Options API 的基础计数器组件。"
  (interactive)
  (message "=== 示例 1：Vue 2 Options API 基础计数器 ===\n")
  
  ;; 定义一个使用 Options API 的计数器组件
  (etaf-define-component vue2-counter
    :props '(:initial)
    :data (lambda ()
            ;; 返回初始数据（自动转换为响应式 refs）
            (list :count 0))
    :methods (list
              ;; 增加方法
              :increment (lambda ()
                          (let ((count-ref (plist-get this :count)))
                            (etaf-ref-update count-ref #'1+)))
              ;; 减少方法
              :decrement (lambda ()
                          (let ((count-ref (plist-get this :count)))
                            (etaf-ref-update count-ref #'1-)))
              ;; 重置方法
              :reset (lambda ()
                      (let ((count-ref (plist-get this :count)))
                        (etaf-ref-set count-ref 0))))
    :mounted (lambda ()
              ;; 组件挂载时调用
              (message "Counter component mounted"))
    :template (lambda (data)
                (let ((count-ref (plist-get data :count))
                      (increment-fn (plist-get data :increment))
                      (decrement-fn (plist-get data :decrement))
                      (reset-fn (plist-get data :reset)))
                  `(div :class "counter"
                        (button :on-click ,decrement-fn "-")
                        (span :class "count" 
                              ,(format " %d " (etaf-ref-get count-ref)))
                        (button :on-click ,increment-fn "+")
                        (button :on-click ,reset-fn "重置")))))
  
  (message "组件定义完成：vue2-counter")
  (message "可以这样使用：(vue2-counter :initial 10)\n"))

;;; ============================================================================
;;; 示例 2：带计算属性的组件（Vue 2 Options API）
;;; ============================================================================

(defun etaf-vue2-example-2-computed ()
  "示例 2：使用 Vue 2 Options API 带计算属性的组件。"
  (interactive)
  (message "=== 示例 2：Vue 2 Options API 计算属性 ===\n")
  
  ;; 定义一个带计算属性的价格计算器
  (etaf-define-component vue2-price-calculator
    :props '(:initial-price :initial-quantity)
    :data (lambda ()
            (list :price 100
                  :quantity 1
                  :tax-rate 0.1))  ; 10% 税率
    :computed (list
               ;; 计算小计
               :subtotal (lambda ()
                          (let ((price-ref (plist-get this :price))
                                (quantity-ref (plist-get this :quantity)))
                            (* (etaf-ref-get price-ref)
                               (etaf-ref-get quantity-ref))))
               ;; 计算税额
               :tax (lambda ()
                     (let ((subtotal-computed (plist-get this :subtotal))
                           (tax-rate-ref (plist-get this :tax-rate)))
                       (* (etaf-computed-get subtotal-computed)
                          (etaf-ref-get tax-rate-ref))))
               ;; 计算总计
               :total (lambda ()
                       (let ((subtotal-computed (plist-get this :subtotal))
                             (tax-computed (plist-get this :tax)))
                         (+ (etaf-computed-get subtotal-computed)
                            (etaf-computed-get tax-computed)))))
    :methods (list
              :update-price (lambda (new-price)
                             (let ((price-ref (plist-get this :price)))
                               (etaf-ref-set price-ref new-price)))
              :update-quantity (lambda (new-qty)
                                (let ((quantity-ref (plist-get this :quantity)))
                                  (etaf-ref-set quantity-ref new-qty))))
    :template (lambda (data)
                (let ((price-ref (plist-get data :price))
                      (quantity-ref (plist-get data :quantity))
                      (subtotal (plist-get data :subtotal))
                      (tax (plist-get data :tax))
                      (total (plist-get data :total)))
                  `(div :class "price-calculator"
                        (div (label "单价: ")
                             (span ,(format "%.2f" (etaf-ref-get price-ref))))
                        (div (label "数量: ")
                             (span ,(format "%d" (etaf-ref-get quantity-ref))))
                        (div :class "divider" "---")
                        (div (strong "小计: ")
                             (span ,(format "%.2f" (etaf-computed-get subtotal))))
                        (div (strong "税额: ")
                             (span ,(format "%.2f" (etaf-computed-get tax))))
                        (div (strong "总计: ")
                             (span ,(format "%.2f" (etaf-computed-get total))))))))
  
  (message "组件定义完成：vue2-price-calculator")
  (message "演示了 Options API 的计算属性功能\n"))

;;; ============================================================================
;;; 示例 3：带侦听器的组件（Vue 2 Options API）
;;; ============================================================================

(defun etaf-vue2-example-3-watchers ()
  "示例 3：使用 Vue 2 Options API 带侦听器的组件。"
  (interactive)
  (message "=== 示例 3：Vue 2 Options API 侦听器 ===\n")
  
  ;; 定义一个带侦听器的搜索框组件
  (etaf-define-component vue2-search-box
    :data (lambda ()
            (list :search-term ""
                  :search-count 0))
    :watch (list
            ;; 侦听搜索词变化
            :search-term (lambda (new-val old-val)
                          (message "搜索词从 '%s' 变为 '%s'" old-val new-val)
                          ;; 更新搜索次数
                          (let ((count-ref (plist-get this :search-count)))
                            (etaf-ref-update count-ref #'1+))))
    :methods (list
              :update-search (lambda (term)
                              (let ((term-ref (plist-get this :search-term)))
                                (etaf-ref-set term-ref term)))
              :clear-search (lambda ()
                             (let ((term-ref (plist-get this :search-term)))
                               (etaf-ref-set term-ref ""))))
    :template (lambda (data)
                (let ((term-ref (plist-get data :search-term))
                      (count-ref (plist-get data :search-count))
                      (clear-fn (plist-get data :clear-search)))
                  `(div :class "search-box"
                        (input :type "text"
                               :placeholder "输入搜索词..."
                               :value ,(etaf-ref-get term-ref))
                        (button :on-click ,clear-fn "清除")
                        (div :class "stats"
                             ,(format "搜索次数: %d" 
                                     (etaf-ref-get count-ref)))))))
  
  (message "组件定义完成：vue2-search-box")
  (message "演示了 Options API 的侦听器功能\n"))

;;; ============================================================================
;;; 示例 4：完整的 Todo List（Vue 2 Options API）
;;; ============================================================================

(defun etaf-vue2-example-4-todo-list ()
  "示例 4：使用 Vue 2 Options API 的完整 Todo List。"
  (interactive)
  (message "=== 示例 4：Vue 2 Options API Todo List ===\n")
  
  (etaf-define-component vue2-todo-list
    :props '(:initial-items)
    :data (lambda ()
            (list :items '()
                  :new-item ""
                  :filter "all"))  ; all, active, completed
    :computed (list
               ;; 已完成的项目数
               :completed-count (lambda ()
                                 (let ((items-ref (plist-get this :items)))
                                   (length (seq-filter
                                           (lambda (item)
                                             (plist-get item :done))
                                           (etaf-ref-get items-ref)))))
               ;; 未完成的项目数
               :active-count (lambda ()
                              (let ((items-ref (plist-get this :items)))
                                (length (seq-filter
                                        (lambda (item)
                                          (not (plist-get item :done)))
                                        (etaf-ref-get items-ref)))))
               ;; 过滤后的项目列表
               :filtered-items (lambda ()
                                (let ((items-ref (plist-get this :items))
                                      (filter-ref (plist-get this :filter))
                                      (filter-val (etaf-ref-get 
                                                  (plist-get this :filter))))
                                  (let ((all-items (etaf-ref-get items-ref)))
                                    (cond
                                     ((string= filter-val "active")
                                      (seq-filter (lambda (item)
                                                   (not (plist-get item :done)))
                                                 all-items))
                                     ((string= filter-val "completed")
                                      (seq-filter (lambda (item)
                                                   (plist-get item :done))
                                                 all-items))
                                     (t all-items))))))
    :methods (list
              ;; 添加新项目
              :add-item (lambda ()
                         (let ((new-item-ref (plist-get this :new-item))
                               (items-ref (plist-get this :items)))
                           (let ((text (etaf-ref-get new-item-ref)))
                             (when (not (string-empty-p text))
                               (let ((current-items (etaf-ref-get items-ref))
                                     (new-todo (list :text text :done nil)))
                                 (etaf-ref-set items-ref
                                              (append current-items (list new-todo)))
                                 (etaf-ref-set new-item-ref ""))))))
              ;; 切换项目完成状态
              :toggle-item (lambda (index)
                            (let ((items-ref (plist-get this :items)))
                              (let* ((current (etaf-ref-get items-ref))
                                     (item (nth index current))
                                     (updated (plist-put (copy-sequence item)
                                                        :done
                                                        (not (plist-get item :done)))))
                                (setf (nth index current) updated)
                                (etaf-ref-set items-ref current))))
              ;; 删除项目
              :remove-item (lambda (index)
                            (let ((items-ref (plist-get this :items)))
                              (let ((current (etaf-ref-get items-ref)))
                                (etaf-ref-set items-ref
                                             (append (seq-take current index)
                                                    (seq-drop current (1+ index)))))))
              ;; 设置过滤器
              :set-filter (lambda (filter)
                           (let ((filter-ref (plist-get this :filter)))
                             (etaf-ref-set filter-ref filter)))
              ;; 清除已完成项目
              :clear-completed (lambda ()
                                (let ((items-ref (plist-get this :items)))
                                  (let ((current (etaf-ref-get items-ref)))
                                    (etaf-ref-set items-ref
                                                 (seq-filter
                                                  (lambda (item)
                                                    (not (plist-get item :done)))
                                                  current))))))
    :mounted (lambda ()
              (message "Todo List 组件已挂载"))
    :template (lambda (data)
                (let ((filtered-items (etaf-computed-get 
                                      (plist-get data :filtered-items)))
                      (active-count (etaf-computed-get 
                                    (plist-get data :active-count)))
                      (completed-count (etaf-computed-get 
                                       (plist-get data :completed-count)))
                      (add-fn (plist-get data :add-item))
                      (toggle-fn (plist-get data :toggle-item))
                      (remove-fn (plist-get data :remove-item))
                      (set-filter-fn (plist-get data :set-filter))
                      (clear-fn (plist-get data :clear-completed)))
                  `(div :class "vue2-todo-list"
                        ;; 标题
                        (h2 "待办事项列表 (Vue 2 Options API)")
                        ;; 输入框
                        (div :class "todo-input"
                             (input :type "text"
                                    :placeholder "添加新任务...")
                             (button :on-click ,add-fn "添加"))
                        ;; 项目列表
                        (ul :class "todo-items"
                            ,@(cl-loop for item in filtered-items
                                      for index from 0
                                      collect
                                      `(li :class ,(if (plist-get item :done)
                                                      "todo-item done"
                                                    "todo-item")
                                           (input :type "checkbox"
                                                  :checked ,(plist-get item :done)
                                                  :on-change (lambda ()
                                                              (funcall ,toggle-fn ,index)))
                                           (span :class "todo-text" 
                                                 ,(plist-get item :text))
                                           (button :class "delete-btn"
                                                   :on-click (lambda ()
                                                              (funcall ,remove-fn ,index))
                                                   "删除"))))
                        ;; 底部统计和过滤器
                        (div :class "todo-footer"
                             (span :class "todo-count"
                                   ,(format "%d 项待办 / %d 项已完成" 
                                           active-count completed-count))
                             (div :class "filters"
                                  (button :on-click (lambda ()
                                                     (funcall ,set-filter-fn "all"))
                                          "全部")
                                  (button :on-click (lambda ()
                                                     (funcall ,set-filter-fn "active"))
                                          "待办")
                                  (button :on-click (lambda ()
                                                     (funcall ,set-filter-fn "completed"))
                                          "已完成"))
                             (button :on-click ,clear-fn
                                     "清除已完成"))))))
  
  (message "组件定义完成：vue2-todo-list")
  (message "这是一个完整的 Todo List，演示了 Options API 的所有特性\n"))

;;; ============================================================================
;;; 示例 5：对比 Vue 2 Options API 和 Vue 3 Composition API
;;; ============================================================================

(defun etaf-vue2-example-5-comparison ()
  "示例 5：对比 Vue 2 Options API 和 Vue 3 Composition API。"
  (interactive)
  (message "=== 示例 5：Options API vs Composition API ===\n")
  
  ;; Vue 2 Options API 风格
  (etaf-define-component counter-options
    :props '(:initial)
    :data (lambda ()
            (list :count 0))
    :computed (list
               :doubled (lambda ()
                         (* 2 (etaf-ref-get (plist-get this :count)))))
    :methods (list
              :increment (lambda ()
                          (etaf-ref-update (plist-get this :count) #'1+)))
    :template (lambda (data)
                `(div :class "counter-options"
                      (p "Count: " ,(etaf-ref-get (plist-get data :count)))
                      (p "Doubled: " ,(etaf-computed-get (plist-get data :doubled)))
                      (button :on-click ,(plist-get data :increment) "+"))))
  
  ;; Vue 3 Composition API 风格（用于对比）
  (etaf-define-component counter-composition
    :props '(:initial)
    :setup (lambda (props)
             (let* ((count (etaf-ref 0))
                    (doubled (etaf-computed
                             (lambda ()
                               (* 2 (etaf-ref-get count)))))
                    (increment (lambda ()
                                (etaf-ref-update count #'1+))))
               (list :count count
                     :doubled doubled
                     :increment increment)))
    :template (lambda (data)
                `(div :class "counter-composition"
                      (p "Count: " ,(etaf-ref-get (plist-get data :count)))
                      (p "Doubled: " ,(etaf-computed-get (plist-get data :doubled)))
                      (button :on-click ,(plist-get data :increment) "+"))))
  
  (message "定义了两个等效的计数器组件：")
  (message "  1. counter-options - 使用 Vue 2 Options API")
  (message "  2. counter-composition - 使用 Vue 3 Composition API")
  (message "\n两种 API 风格可以同时使用，选择你喜欢的即可！\n"))

;;; ============================================================================
;;; 运行所有示例
;;; ============================================================================

(defun etaf-vue2-run-all-examples ()
  "运行所有 Vue 2 Options API 示例。"
  (interactive)
  (etaf-vue2-example-1-basic-counter)
  (etaf-vue2-example-2-computed)
  (etaf-vue2-example-3-watchers)
  (etaf-vue2-example-4-todo-list)
  (etaf-vue2-example-5-comparison)
  (message "所有 Vue 2 Options API 示例已定义完成！"))

(provide 'etaf-vue2-options-api-examples)
;;; etaf-vue2-options-api-examples.el ends here
