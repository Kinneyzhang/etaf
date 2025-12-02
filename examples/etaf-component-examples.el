;;; etaf-component-examples.el --- 组件使用示例（从简单到复杂） -*- lexical-binding: t; -*-

;;; Commentary:

;; 本文件展示了 ETAF 组件系统的使用方法，包含从简单到复杂的各种示例。
;; 
;; 组件系统基于 Vue3 风格，支持：
;; - Props（属性传递）
;; - Setup 函数（组合式 API）
;; - Template（模板）
;; - 响应式系统（ref、computed、watch）
;;
;; 示例目录：
;; 1. 最简单的无状态组件
;; 2. 带 Props 的组件
;; 3. 带 Slots（子元素）的组件
;; 4. 带响应式状态的组件（ref）
;; 5. 带计算属性的组件（computed）
;; 6. 带侦听器的组件（watch）
;; 7. 复杂的交互式组件（Todo List）

;;; Code:

(require 'etaf-etml)
;; Note: etaf-css and etaf-render are optional, only needed for full rendering
;; (require 'etaf-css)
;; (require 'etaf-render)

;;; ============================================================================
;;; 示例 1：最简单的无状态组件
;;; ============================================================================
;;
;; 这是最基础的组件形式，只有一个静态模板，没有任何 props 或状态。

(defun etaf-component-example-1-basic ()
  "示例 1：最简单的无状态组件。"
  (interactive)
  (message "=== 示例 1：最简单的无状态组件 ===\n")
  
  ;; 定义一个简单的 Logo 组件
  (etaf-define-component simple-logo
    :template '(div :class "logo"
                    (span "🚀 ETAF")))
  
  ;; 定义一个简单的 Divider（分隔线）组件
  (etaf-define-component simple-divider
    :template '(hr :style "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px"))
  
  ;; 使用组件
  (let* ((template '(div
                     (simple-logo)
                     (simple-divider)
                     (p "欢迎使用 ETAF 框架！")))
         (rendered (etaf-etml-render template nil)))
    
    (message "组件定义：")
    (message "  (etaf-define-component simple-logo")
    (message "    :template '(div :class \"logo\" (span \"🚀 ETAF\")))")
    (message "")
    (message "渲染结果：%S\n" rendered)))


;;; ============================================================================
;;; 示例 2：带 Props 的组件
;;; ============================================================================
;;
;; Props 允许组件接收外部传入的数据，使组件更加灵活和可复用。

(defun etaf-component-example-2-props ()
  "示例 2：带 Props 的组件。"
  (interactive)
  (message "=== 示例 2：带 Props 的组件 ===\n")
  
  ;; 定义一个 Badge（徽章）组件
  (etaf-define-component badge
    :props '(:text :type)
    :template '(span :class "badge badge-{{ type }}"
                     :style "padding-left: 4px; padding-right: 4px; border-radius: 3px"
                     "{{ text }}"))
  
  ;; 定义一个 Alert（警告框）组件
  (etaf-define-component alert
    :props '(:message :type :title)
    :template '(div :class "alert alert-{{ type }}"
                    :style "padding-left: 10px; padding-right: 10px; padding-top: 10px; padding-bottom: 10px; border-left-width: 4px"
                    (strong :e-if "title" "{{ title }}: ")
                    (span "{{ message }}")))
  
  ;; 定义一个 UserAvatar（用户头像）组件
  (etaf-define-component user-avatar
    :props '(:name :size)
    :template (lambda (data)
                (let* ((name (or (plist-get data :name) "?"))
                       (size (or (plist-get data :size) "medium"))
                       (initial (substring name 0 1)))
                  `(div :class ,(concat "avatar avatar-" size)
                        :style "border-radius: 50%; display: inline-block; text-align: center"
                        ,initial))))
  
  ;; 使用组件
  (let* ((template1 '(badge :text "新功能" :type "primary"))
         (template2 '(badge :text "已完成" :type "success"))
         (template3 '(alert :type "warning" :title "注意" :message "这是一条警告信息"))
         (template4 '(user-avatar :name "Alice" :size "large")))
    
    (message "Badge 组件使用：")
    (message "  模板：(badge :text \"新功能\" :type \"primary\")")
    (message "  渲染：%S\n" (etaf-etml-render template1 nil))
    
    (message "Alert 组件使用：")
    (message "  模板：(alert :type \"warning\" :title \"注意\" :message \"...\")")
    (message "  渲染：%S\n" (etaf-etml-render template3 nil))
    
    (message "UserAvatar 组件使用（带函数式模板）：")
    (message "  模板：(user-avatar :name \"Alice\" :size \"large\")")
    (message "  渲染：%S\n" (etaf-etml-render template4 nil))))


;;; ============================================================================
;;; 示例 3：带 Slots（子元素）的组件
;;; ============================================================================
;;
;; Slots 允许组件接收子元素，实现内容分发，类似 Vue 的插槽机制。

(defun etaf-component-example-3-slots ()
  "示例 3：带 Slots（子元素）的组件。"
  (interactive)
  (message "=== 示例 3：带 Slots（子元素）的组件 ===\n")
  
  ;; 定义一个 Card（卡片）组件，接受子元素作为内容
  (etaf-define-component card
    :props '(:title :footer)
    :template (lambda (data)
                (let ((title (plist-get data :title))
                      (footer (plist-get data :footer))
                      (slots (plist-get data :$slots)))
                  `(div :class "card"
                        :style "border: 1px solid #ddd; border-radius: 8px; margin-bottom: 16px"
                        ,@(when title
                            `((div :class "card-header"
                                   :style "padding-left: 16px; padding-right: 16px; padding-top: 12px; padding-bottom: 12px; border-bottom: 1px solid #ddd; font-weight: bold"
                                   ,title)))
                        (div :class "card-body"
                             :style "padding-left: 16px; padding-right: 16px; padding-top: 16px; padding-bottom: 16px"
                             ,@slots)
                        ,@(when footer
                            `((div :class "card-footer"
                                   :style "padding-left: 16px; padding-right: 16px; padding-top: 12px; padding-bottom: 12px; border-top: 1px solid #ddd; color: #666"
                                   ,footer)))))))
  
  ;; 定义一个 Modal（模态框）组件
  (etaf-define-component modal
    :props '(:title :visible)
    :template (lambda (data)
                (let ((title (plist-get data :title))
                      (visible (plist-get data :visible))
                      (slots (plist-get data :$slots)))
                  ;; 只有 visible 为 true 时才渲染模态框
                  (when visible
                    `(div :class "modal-overlay"
                          :style "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: rgba(0,0,0,0.5)"
                          (div :class "modal"
                               :style "background: white; border-radius: 8px; max-width: 500px; margin: auto"
                               (div :class "modal-header"
                                    :style "padding-left: 16px; padding-right: 16px; padding-top: 12px; padding-bottom: 12px; border-bottom: 1px solid #ddd"
                                    (strong ,title))
                               (div :class "modal-body"
                                    :style "padding-left: 16px; padding-right: 16px; padding-top: 16px; padding-bottom: 16px"
                                    ,@slots)))))))
  
  ;; 定义一个 Container（容器）组件
  (etaf-define-component container
    :props '(:max-width)
    :template (lambda (data)
                (let ((max-width (or (plist-get data :max-width) "1200px"))
                      (slots (plist-get data :$slots)))
                  `(div :class "container"
                        :style ,(format "max-width: %s; margin-left: auto; margin-right: auto; padding-left: 16px; padding-right: 16px"
                                        max-width)
                        ,@slots))))
  
  ;; 使用组件
  (let* ((template '(container :max-width "800px"
                     (card :title "用户信息" :footer "最后更新：2024-01-01"
                       (p "姓名：张三")
                       (p "邮箱：zhangsan@example.com")
                       (p "角色：管理员"))))
         (rendered (etaf-etml-render template nil)))
    
    (message "Card 组件（带 slots）使用：")
    (message "  (card :title \"用户信息\" :footer \"...\"")
    (message "    (p \"姓名：张三\")")
    (message "    (p \"邮箱：...\"))")
    (message "")
    (message "渲染结果：%S\n" rendered)))


;;; ============================================================================
;;; 示例 4：带响应式状态的组件（ref）
;;; ============================================================================
;;
;; 使用 ref 创建响应式状态，当状态改变时可以触发更新。

(defun etaf-component-example-4-ref ()
  "示例 4：带响应式状态的组件（ref）。"
  (interactive)
  (message "=== 示例 4：带响应式状态的组件（ref） ===\n")
  
  ;; 演示 ref 的基本用法
  (message "--- ref 基本用法 ---")
  
  ;; 创建响应式引用
  (let ((count (etaf-ref 0)))
    (message "创建 ref: (etaf-ref 0)")
    (message "  初始值: %s" (etaf-ref-get count))
    
    ;; 修改值
    (etaf-ref-set count 5)
    (message "  设置后 (set 5): %s" (etaf-ref-get count))
    
    ;; 使用 update 更新
    (etaf-ref-update count (lambda (n) (+ n 10)))
    (message "  更新后 (+ 10): %s\n" (etaf-ref-get count)))
  
  ;; 定义一个 Counter（计数器）组件
  (etaf-define-component counter
    :props '(:initial)
    :setup (lambda (props)
             (let* ((initial (or (plist-get props :initial) 0))
                    (count (etaf-ref initial))
                    (increment (lambda ()
                                 (etaf-ref-update count #'1+)))
                    (decrement (lambda ()
                                 (etaf-ref-update count #'1-)))
                    (reset (lambda ()
                             (etaf-ref-set count initial))))
               (list :count count
                     :increment increment
                     :decrement decrement
                     :reset reset)))
    :template (lambda (data)
                (let ((count-ref (plist-get data :count)))
                  `(div :class "counter"
                        :style "display: flex; align-items: center; gap: 10px"
                        (button :on-click ,(plist-get data :decrement) "-")
                        (span :style "min-width: 50px; text-align: center"
                              ,(format "%s" (etaf-ref-get count-ref)))
                        (button :on-click ,(plist-get data :increment) "+")
                        (button :on-click ,(plist-get data :reset) "重置")))))
  
  ;; 定义一个 Toggle（开关）组件
  (etaf-define-component toggle
    :props '(:initial :label)
    :setup (lambda (props)
             (let* ((initial (plist-get props :initial))
                    (active (etaf-ref (if initial t nil)))
                    (toggle (lambda ()
                              (etaf-ref-update active #'not))))
               (list :active active
                     :toggle toggle
                     :label (plist-get props :label))))
    :template (lambda (data)
                (let* ((active-ref (plist-get data :active))
                       (is-active (etaf-ref-get active-ref))
                       (label (plist-get data :label)))
                  `(div :class "toggle"
                        :style "display: flex; align-items: center; gap: 8px"
                        (button :on-click ,(plist-get data :toggle)
                                :style ,(if is-active
                                            "background: #4CAF50; color: white"
                                          "background: #ccc; color: black")
                                ,(if is-active "ON" "OFF"))
                        (span ,label)))))
  
  (message "--- Counter 组件定义 ---")
  (message "(etaf-define-component counter")
  (message "  :props '(:initial)")
  (message "  :setup (lambda (props)")
  (message "           (let* ((count (etaf-ref initial))")
  (message "                  (increment (lambda () ...)))")
  (message "             (list :count count :increment increment)))")
  (message "  :template (lambda (data) ...))\n")
  
  ;; 演示组件定义后的状态
  (message "组件已注册: %s" (etaf-component-defined-p 'counter))
  (message "组件已注册: %s\n" (etaf-component-defined-p 'toggle)))


;;; ============================================================================
;;; 示例 5：带计算属性的组件（computed）
;;; ============================================================================
;;
;; computed 用于基于其他响应式数据派生出新的值，具有缓存特性。

(defun etaf-component-example-5-computed ()
  "示例 5：带计算属性的组件（computed）。"
  (interactive)
  (message "=== 示例 5：带计算属性的组件（computed） ===\n")
  
  ;; 演示 computed 的基本用法
  (message "--- computed 基本用法 ---")
  
  (let* ((price (etaf-ref 100))
         (quantity (etaf-ref 3))
         (total (etaf-computed
                 (lambda ()
                   (* (etaf-ref-get price)
                      (etaf-ref-get quantity)))))
         (discount-rate (etaf-ref 0.1))
         (final-price (etaf-computed
                       (lambda ()
                         (* (etaf-computed-get total)
                            (- 1 (etaf-ref-get discount-rate)))))))
    
    (message "创建 refs:")
    (message "  price = %s" (etaf-ref-get price))
    (message "  quantity = %s" (etaf-ref-get quantity))
    (message "  discount-rate = %s" (etaf-ref-get discount-rate))
    (message "")
    (message "创建 computed:")
    (message "  total = price * quantity = %s" (etaf-computed-get total))
    (message "  final-price = total * (1 - discount) = %s" (etaf-computed-get final-price))
    (message "")
    
    ;; 修改依赖值
    (etaf-ref-set quantity 5)
    (message "修改 quantity = 5 后:")
    (message "  total = %s" (etaf-computed-get total))
    (message "  final-price = %s" (etaf-computed-get final-price))
    (message ""))
  
  ;; 定义一个 PriceCalculator（价格计算器）组件
  (etaf-define-component price-calculator
    :props '(:base-price)
    :setup (lambda (props)
             (let* ((base-price (or (plist-get props :base-price) 0))
                    (quantity (etaf-ref 1))
                    (tax-rate (etaf-ref 0.08))
                    (subtotal (etaf-computed
                               (lambda ()
                                 (* base-price (etaf-ref-get quantity)))))
                    (tax (etaf-computed
                          (lambda ()
                            (* (etaf-computed-get subtotal)
                               (etaf-ref-get tax-rate)))))
                    (total (etaf-computed
                            (lambda ()
                              (+ (etaf-computed-get subtotal)
                                 (etaf-computed-get tax))))))
               (list :base-price base-price
                     :quantity quantity
                     :tax-rate tax-rate
                     :subtotal subtotal
                     :tax tax
                     :total total)))
    :template (lambda (data)
                `(div :class "price-calculator"
                      (div "单价: ¥" ,(format "%.2f" (plist-get data :base-price)))
                      (div "数量: " ,(format "%d" (etaf-ref-get (plist-get data :quantity))))
                      (div "小计: ¥" ,(format "%.2f" (etaf-computed-get (plist-get data :subtotal))))
                      (div "税费 (" ,(format "%.0f%%" (* 100 (etaf-ref-get (plist-get data :tax-rate))))
                           "): ¥" ,(format "%.2f" (etaf-computed-get (plist-get data :tax))))
                      (div :style "font-weight: bold; margin-top: 8px"
                           "总计: ¥" ,(format "%.2f" (etaf-computed-get (plist-get data :total)))))))
  
  ;; 定义一个 SearchFilter（搜索过滤器）组件
  (etaf-define-component search-filter
    :props '(:items)
    :setup (lambda (props)
             (let* ((items (or (plist-get props :items) '()))
                    (search-term (etaf-ref ""))
                    (filtered-items (etaf-computed
                                     (lambda ()
                                       (let ((term (etaf-ref-get search-term)))
                                         (if (string-empty-p term)
                                             items
                                           (seq-filter
                                            (lambda (item)
                                              (string-match-p (regexp-quote term) item))
                                            items))))))
                    (result-count (etaf-computed
                                   (lambda ()
                                     (length (etaf-computed-get filtered-items))))))
               (list :items items
                     :search-term search-term
                     :filtered-items filtered-items
                     :result-count result-count)))
    :template (lambda (data)
                `(div :class "search-filter"
                      (div "搜索词: " ,(etaf-ref-get (plist-get data :search-term)))
                      (div "匹配数: " ,(format "%d" (etaf-computed-get (plist-get data :result-count))))
                      (ul
                       ,@(mapcar (lambda (item) `(li ,item))
                                 (etaf-computed-get (plist-get data :filtered-items)))))))
  
  (message "--- PriceCalculator 组件 ---")
  (message "使用多个 computed 属性计算价格：")
  (message "  subtotal = base-price * quantity")
  (message "  tax = subtotal * tax-rate")
  (message "  total = subtotal + tax")
  (message "")
  (message "组件已注册: %s\n" (etaf-component-defined-p 'price-calculator)))


;;; ============================================================================
;;; 示例 6：带侦听器的组件（watch）
;;; ============================================================================
;;
;; watch 用于监听响应式数据的变化并执行副作用操作。

(defun etaf-component-example-6-watch ()
  "示例 6：带侦听器的组件（watch）。"
  (interactive)
  (message "=== 示例 6：带侦听器的组件（watch） ===\n")
  
  ;; 演示 watch-source 的用法
  (message "--- watch-source 基本用法 ---")
  
  (let* ((changes nil)
         (count (etaf-ref 0))
         (stop (etaf-watch
                count
                (lambda (new old)
                  (push (format "变化: %s -> %s" old new) changes)))))
    
    (message "创建 ref 和 watcher:")
    (message "  (etaf-watch count callback)")
    (message "")
    
    (etaf-ref-set count 1)
    (etaf-ref-set count 2)
    (etaf-ref-set count 3)
    
    (message "修改 count: 0 -> 1 -> 2 -> 3")
    (message "记录的变化:")
    (dolist (change (reverse changes))
      (message "  %s" change))
    (message "")
    
    ;; 停止监听
    (funcall stop)
    (etaf-ref-set count 999)
    (message "停止监听后修改 count = 999")
    (message "新的变化数: %d (应该仍是 3)\n" (length changes)))
  
  ;; 演示 watch-effect 的用法
  (message "--- watch-effect 自动依赖追踪 ---")
  
  (let* ((logs nil)
         (name (etaf-ref "Alice"))
         (age (etaf-ref 25))
         (stop (etaf-watch-effect
                (lambda ()
                  (push (format "用户: %s, 年龄: %s"
                                (etaf-ref-get name)
                                (etaf-ref-get age))
                        logs)))))
    
    (message "创建 watch-effect (自动追踪 name 和 age):")
    (message "  初始运行: %s" (car logs))
    
    (etaf-ref-set name "Bob")
    (message "  修改 name: %s" (car logs))
    
    (etaf-ref-set age 30)
    (message "  修改 age: %s" (car logs))
    
    (funcall stop)
    (message "  总运行次数: %d\n" (length logs)))
  
  ;; 定义一个 AutoSave（自动保存）组件概念
  (etaf-define-component auto-save-demo
    :props '(:initial-content)
    :setup (lambda (props)
             (let* ((content (etaf-ref (or (plist-get props :initial-content) "")))
                    (save-status (etaf-ref "已保存"))
                    (last-saved (etaf-ref nil))
                    ;; 模拟自动保存
                    (save-fn (lambda ()
                               (etaf-ref-set save-status "保存中...")
                               ;; 模拟异步保存
                               (etaf-ref-set last-saved (format-time-string "%H:%M:%S"))
                               (etaf-ref-set save-status "已保存"))))
               ;; 监听内容变化
               (etaf-watch
                content
                (lambda (new old)
                  (when (not (string= new old))
                    (funcall save-fn))))
               (list :content content
                     :save-status save-status
                     :last-saved last-saved)))
    :template (lambda (data)
                `(div :class "auto-save"
                      (div "内容: " ,(etaf-ref-get (plist-get data :content)))
                      (div :style "color: #666; font-size: 12px"
                           "状态: " ,(etaf-ref-get (plist-get data :save-status))
                           ,@(when (etaf-ref-get (plist-get data :last-saved))
                               `(" | 上次保存: " ,(etaf-ref-get (plist-get data :last-saved))))))))
  
  (message "--- AutoSave 组件概念 ---")
  (message "使用 watch-source 监听内容变化并自动保存")
  (message "组件已注册: %s\n" (etaf-component-defined-p 'auto-save-demo)))


;;; ============================================================================
;;; 示例 7：复杂的交互式组件（Todo List）
;;; ============================================================================
;;
;; 综合运用所有特性创建一个完整的 Todo List 组件。

(defun etaf-component-example-7-todo-list ()
  "示例 7：复杂的交互式组件（Todo List）。"
  (interactive)
  (message "=== 示例 7：复杂的交互式组件（Todo List） ===\n")
  
  ;; 首先定义一个 TodoItem 组件
  (etaf-define-component todo-item
    :props '(:id :text :completed :on-toggle :on-delete)
    :template (lambda (data)
                (let ((completed (plist-get data :completed))
                      (text (plist-get data :text)))
                  `(div :class "todo-item"
                        :style "display: flex; align-items: center; padding-top: 8px; padding-bottom: 8px; border-bottom: 1px solid #eee"
                        (input :type "checkbox"
                               :checked ,(if completed "checked" nil)
                               :on-change ,(plist-get data :on-toggle))
                        (span :style ,(if completed
                                          "flex: 1; margin-left: 8px; text-decoration: line-through; color: #999"
                                        "flex: 1; margin-left: 8px")
                              ,text)
                        (button :on-click ,(plist-get data :on-delete)
                                :style "color: red; border: none; background: none; cursor: pointer"
                                "✕")))))
  
  ;; 定义 TodoList 主组件
  (etaf-define-component todo-list
    :props '(:initial-items)
    :setup (lambda (props)
             (let* ((initial (or (plist-get props :initial-items) '()))
                    (items (etaf-ref initial))
                    (new-item-text (etaf-ref ""))
                    (filter-type (etaf-ref 'all)) ; 'all, 'active, 'completed
                    ;; 计算属性
                    (filtered-items (etaf-computed
                                     (lambda ()
                                       (let ((all-items (etaf-ref-get items))
                                             (filter (etaf-ref-get filter-type)))
                                         (pcase filter
                                           ('all all-items)
                                           ('active (seq-filter
                                                     (lambda (item)
                                                       (not (plist-get item :completed)))
                                                     all-items))
                                           ('completed (seq-filter
                                                        (lambda (item)
                                                          (plist-get item :completed))
                                                        all-items)))))))
                    (total-count (etaf-computed
                                  (lambda ()
                                    (length (etaf-ref-get items)))))
                    (active-count (etaf-computed
                                   (lambda ()
                                     (length (seq-filter
                                              (lambda (item)
                                                (not (plist-get item :completed)))
                                              (etaf-ref-get items))))))
                    (completed-count (etaf-computed
                                      (lambda ()
                                        (- (etaf-computed-get total-count)
                                           (etaf-computed-get active-count)))))
                    ;; 方法
                    (add-item (lambda ()
                                (let ((text (etaf-ref-get new-item-text)))
                                  (when (not (string-empty-p text))
                                    (let* ((current (etaf-ref-get items))
                                           (new-id (1+ (or (seq-max (mapcar (lambda (i) (plist-get i :id)) current)) 0)))
                                           (new-item (list :id new-id :text text :completed nil)))
                                      (etaf-ref-set items (append current (list new-item)))
                                      (etaf-ref-set new-item-text ""))))))
                    (toggle-item (lambda (id)
                                   (let ((current (etaf-ref-get items)))
                                     (etaf-ref-set
                                      items
                                      (mapcar (lambda (item)
                                                (if (eq (plist-get item :id) id)
                                                    (plist-put (copy-sequence item) :completed
                                                               (not (plist-get item :completed)))
                                                  item))
                                              current)))))
                    (delete-item (lambda (id)
                                   (let ((current (etaf-ref-get items)))
                                     (etaf-ref-set
                                      items
                                      (seq-filter (lambda (item)
                                                    (not (eq (plist-get item :id) id)))
                                                  current)))))
                    (clear-completed (lambda ()
                                       (let ((current (etaf-ref-get items)))
                                         (etaf-ref-set
                                          items
                                          (seq-filter (lambda (item)
                                                        (not (plist-get item :completed)))
                                                      current))))))
               (list :items items
                     :new-item-text new-item-text
                     :filter-type filter-type
                     :filtered-items filtered-items
                     :total-count total-count
                     :active-count active-count
                     :completed-count completed-count
                     :add-item add-item
                     :toggle-item toggle-item
                     :delete-item delete-item
                     :clear-completed clear-completed)))
    :template (lambda (data)
                (let ((filtered (etaf-computed-get (plist-get data :filtered-items)))
                      (active (etaf-computed-get (plist-get data :active-count)))
                      (completed (etaf-computed-get (plist-get data :completed-count))))
                  `(div :class "todo-app"
                        :style "max-width: 500px; margin: auto; font-family: sans-serif"
                        ;; 标题
                        (h1 :style "text-align: center; color: #333" "📝 Todo List")
                        ;; 输入区域
                        ;; 注意：在真实应用中，input 需要绑定 value 和 on-input 事件
                        ;; 这里简化展示，重点是组件结构
                        (div :style "display: flex; margin-bottom: 16px"
                             (input :type "text"
                                    :placeholder "添加新任务..."
                                    :value ,(etaf-ref-get (plist-get data :new-item-text))
                                    :style "flex: 1; padding-left: 8px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px; border: 1px solid #ddd; border-radius: 4px 0 0 4px")
                             (button :on-click ,(plist-get data :add-item)
                                     :style "padding-left: 16px; padding-right: 16px; padding-top: 8px; padding-bottom: 8px; background: #4CAF50; color: white; border: none; border-radius: 0 4px 4px 0; cursor: pointer"
                                     "添加"))
                        ;; 筛选按钮
                        ;; 注意：在真实应用中，每个按钮需要 on-click 处理器和 active 样式
                        (div :style "display: flex; gap: 8px; margin-bottom: 16px"
                             (button :style "flex: 1; padding-top: 4px; padding-bottom: 4px"
                                     :on-click (lambda () (etaf-ref-set ,(plist-get data :filter-type) 'all))
                                     "全部")
                             (button :style "flex: 1; padding-top: 4px; padding-bottom: 4px"
                                     :on-click (lambda () (etaf-ref-set ,(plist-get data :filter-type) 'active))
                                     "待完成")
                             (button :style "flex: 1; padding-top: 4px; padding-bottom: 4px"
                                     :on-click (lambda () (etaf-ref-set ,(plist-get data :filter-type) 'completed))
                                     "已完成"))
                        ;; 任务列表
                        ;; 注意：在实际应用中，这里会传入 on-toggle 和 on-delete 回调
                        ;; 由于示例限制，这里简化了实现
                        (div :class "todo-items"
                             ,@(if filtered
                                   (let ((toggle-fn (plist-get data :toggle-item))
                                         (delete-fn (plist-get data :delete-item)))
                                     (mapcar (lambda (item)
                                               (let ((id (plist-get item :id)))
                                                 `(todo-item :id ,id
                                                             :text ,(plist-get item :text)
                                                             :completed ,(plist-get item :completed)
                                                             :on-toggle (lambda () (funcall ,toggle-fn ,id))
                                                             :on-delete (lambda () (funcall ,delete-fn ,id)))))
                                             filtered))
                                 '((p :style "text-align: center; color: #999" "暂无任务"))))
                        ;; 底部统计
                        (div :style "display: flex; justify-content: space-between; margin-top: 16px; padding-top: 16px; border-top: 1px solid #eee; color: #666; font-size: 14px"
                             (span ,(format "待完成: %d" active))
                             (span ,(format "已完成: %d" completed))
                             (button :on-click ,(plist-get data :clear-completed)
                                     :style "color: #999; border: none; background: none; cursor: pointer; text-decoration: underline"
                                     "清除已完成"))))))
  
  (message "--- TodoList 组件结构 ---\n")
  (message "TodoList 组件包含：")
  (message "")
  (message "状态（ref）：")
  (message "  - items: 任务列表")
  (message "  - new-item-text: 新任务输入")
  (message "  - filter-type: 筛选类型")
  (message "")
  (message "计算属性（computed）：")
  (message "  - filtered-items: 根据筛选类型过滤后的任务")
  (message "  - total-count: 总任务数")
  (message "  - active-count: 待完成数")
  (message "  - completed-count: 已完成数")
  (message "")
  (message "方法：")
  (message "  - add-item: 添加任务")
  (message "  - toggle-item: 切换完成状态")
  (message "  - delete-item: 删除任务")
  (message "  - clear-completed: 清除已完成")
  (message "")
  (message "子组件：")
  (message "  - TodoItem: 单个任务项组件")
  (message "")
  (message "组件已注册:")
  (message "  todo-item: %s" (etaf-component-defined-p 'todo-item))
  (message "  todo-list: %s\n" (etaf-component-defined-p 'todo-list)))


;;; ============================================================================
;;; 示例 8：表单组件综合示例
;;; ============================================================================
;;
;; 展示如何创建可复用的表单组件。

(defun etaf-component-example-8-form ()
  "示例 8：表单组件综合示例。"
  (interactive)
  (message "=== 示例 8：表单组件综合示例 ===\n")
  
  ;; 定义 FormField（表单字段）组件
  ;; 注意：value 和 on-change 需要由父组件传入
  (etaf-define-component form-field
    :props '(:label :name :type :placeholder :required :error :value :on-change)
    :template (lambda (data)
                (let ((label (plist-get data :label))
                      (name (plist-get data :name))
                      (type (or (plist-get data :type) "text"))
                      (placeholder (plist-get data :placeholder))
                      (required (plist-get data :required))
                      (error (plist-get data :error))
                      (value (plist-get data :value))
                      (on-change (plist-get data :on-change)))
                  `(div :class "form-field"
                        :style "margin-bottom: 16px"
                        (label :style "display: block; margin-bottom: 4px; font-weight: 500"
                               ,label
                               ,@(when required
                                   '((span :style "color: red" " *"))))
                        (input :type ,type
                               :name ,name
                               :placeholder ,placeholder
                               :value ,(or value "")
                               ,@(when on-change
                                   `(:on-change ,on-change))
                               :style ,(concat "width: 100%; padding-left: 8px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px; border: 1px solid "
                                               (if error "#dc3545" "#ddd")
                                               "; border-radius: 4px"))
                        ,@(when error
                            `((span :style "color: #dc3545; font-size: 12px; margin-top: 4px" ,error)))))))
  
  ;; 定义 FormSelect（下拉选择）组件
  ;; 注意：value 和 on-change 需要由父组件传入
  (etaf-define-component form-select
    :props '(:label :name :options :required :value :on-change)
    :template (lambda (data)
                (let ((label (plist-get data :label))
                      (name (plist-get data :name))
                      (options (or (plist-get data :options) '()))
                      (required (plist-get data :required))
                      (value (plist-get data :value))
                      (on-change (plist-get data :on-change)))
                  `(div :class "form-field"
                        :style "margin-bottom: 16px"
                        (label :style "display: block; margin-bottom: 4px; font-weight: 500"
                               ,label
                               ,@(when required
                                   '((span :style "color: red" " *"))))
                        (select :name ,name
                                :value ,(or value "")
                                ,@(when on-change
                                    `(:on-change ,on-change))
                                :style "width: 100%; padding-left: 8px; padding-right: 8px; padding-top: 8px; padding-bottom: 8px; border: 1px solid #ddd; border-radius: 4px"
                                (option :value "" "请选择...")
                                ,@(mapcar (lambda (opt)
                                            `(option :value ,(car opt) ,(cdr opt)))
                                          options))))))
  
  ;; 定义 Form（表单）组件
  (etaf-define-component form
    :props '(:title :on-submit)
    :setup (lambda (props)
             (let* ((form-data (etaf-reactive '(:name "" :email "" :role "")))
                    (errors (etaf-ref '()))
                    (submitting (etaf-ref nil))
                    (validate (lambda ()
                                (let ((errs '()))
                                  (when (string-empty-p (etaf-reactive-get form-data :name))
                                    (push '(:name . "姓名不能为空") errs))
                                  (when (string-empty-p (etaf-reactive-get form-data :email))
                                    (push '(:email . "邮箱不能为空") errs))
                                  (etaf-ref-set errors errs)
                                  (null errs))))
                    (submit (lambda ()
                              (when (funcall validate)
                                (etaf-ref-set submitting t)
                                ;; 模拟提交
                                (message "提交表单: %S" (etaf-reactive-to-plist form-data))
                                (etaf-ref-set submitting nil)))))
               (list :title (plist-get props :title)
                     :form-data form-data
                     :errors errors
                     :submitting submitting
                     :validate validate
                     :submit submit)))
    :template (lambda (data)
                (let ((title (plist-get data :title))
                      (errors (etaf-ref-get (plist-get data :errors)))
                      (submitting (etaf-ref-get (plist-get data :submitting))))
                  `(div :class "form"
                        :style "max-width: 400px; padding-left: 24px; padding-right: 24px; padding-top: 24px; padding-bottom: 24px; border: 1px solid #ddd; border-radius: 8px"
                        (h2 :style "margin-top: 0; margin-bottom: 24px" ,title)
                        (form-field :label "姓名"
                                    :name "name"
                                    :placeholder "请输入姓名"
                                    :required t
                                    :error ,(cdr (assq :name errors)))
                        (form-field :label "邮箱"
                                    :name "email"
                                    :type "email"
                                    :placeholder "请输入邮箱"
                                    :required t
                                    :error ,(cdr (assq :email errors)))
                        (form-select :label "角色"
                                     :name "role"
                                     :options (("admin" . "管理员")
                                               ("user" . "普通用户")
                                               ("guest" . "访客")))
                        (button :type "submit"
                                :on-click ,(plist-get data :submit)
                                :style "width: 100%; padding-top: 10px; padding-bottom: 10px; background: #007bff; color: white; border: none; border-radius: 4px; cursor: pointer"
                                :disabled ,submitting
                                ,(if submitting "提交中..." "提交"))))))
  
  (message "--- 表单组件示例 ---\n")
  (message "组件结构：")
  (message "  Form (主表单组件)")
  (message "    ├── FormField (文本输入)")
  (message "    └── FormSelect (下拉选择)")
  (message "")
  (message "特性：")
  (message "  - 使用 reactive 管理表单数据")
  (message "  - 使用 ref 管理错误和提交状态")
  (message "  - 支持表单验证")
  (message "  - 支持必填项标记")
  (message "  - 支持错误信息显示")
  (message "")
  (message "组件已注册:")
  (message "  form-field: %s" (etaf-component-defined-p 'form-field))
  (message "  form-select: %s" (etaf-component-defined-p 'form-select))
  (message "  form: %s\n" (etaf-component-defined-p 'form)))


;;; ============================================================================
;;; 运行所有示例
;;; ============================================================================

(defun etaf-component-run-all-examples ()
  "运行所有组件示例。"
  (interactive)
  (etaf-component-example-1-basic)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-2-props)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-3-slots)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-4-ref)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-5-computed)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-6-watch)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-7-todo-list)
  (message "\n%s\n" (make-string 60 ?=))
  (etaf-component-example-8-form)
  (message "\n=== 所有组件示例运行完毕 ===\n")
  
  ;; 列出所有已注册的组件
  (message "已注册的组件列表:")
  (dolist (comp (etaf-component-list-all))
    (message "  - %s" comp)))

;; 使用说明
;;
;; 运行所有示例：
;;   M-x etaf-component-run-all-examples
;;
;; 运行单个示例：
;;   M-x etaf-component-example-1-basic     ; 最简单的组件
;;   M-x etaf-component-example-2-props     ; 带 props 的组件
;;   M-x etaf-component-example-3-slots     ; 带 slots 的组件
;;   M-x etaf-component-example-4-ref       ; 带响应式状态的组件
;;   M-x etaf-component-example-5-computed  ; 带计算属性的组件
;;   M-x etaf-component-example-6-watch     ; 带侦听器的组件
;;   M-x etaf-component-example-7-todo-list ; Todo List 综合示例
;;   M-x etaf-component-example-8-form      ; 表单组件示例

(provide 'etaf-component-examples)
;;; etaf-component-examples.el ends here
