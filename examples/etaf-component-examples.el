;;; etaf-component-examples.el --- Component system examples -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: components, reactive, vue3, examples
;; Version: 1.0.0

;;; Commentary:

;; This file provides comprehensive examples of ETAF's component system,
;; demonstrating both Vue 3 Composition API and Vue 2 Options API styles.
;;
;; Examples cover:
;; - Composition API: ref, computed, watch, watchEffect, reactive
;; - Options API: data, methods, computed, watch, lifecycle hooks
;; - Practical component rendering with various features
;;
;; Usage:
;;   (load-file "examples/etaf-component-examples.el")
;;   M-x etaf-component-demo

;;; Code:

(require 'etaf)
(require 'etaf-component)

;;; ============================================================================
;;; Vue 3 组合式 API 示例 (Composition API Examples)
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; 示例 1: 基础计数器 - 使用 ref
;;; Example 1: Basic Counter - Using ref
;;; ----------------------------------------------------------------------------

(etaf-define-component composition-counter
  :props '(:initial-count :step)
  :setup (lambda (props)
           "基础计数器组件，演示 ref 的使用。
Basic counter component demonstrating ref usage."
           (let* ((initial (or (plist-get props :initial-count) 0))
                  (step-val (plist-get props :step))
                  ;; Validate step parameter
                  (step (if (and step-val (numberp step-val)) step-val 1))
                  (count (etaf-ref initial))
                  (increment (lambda ()
                              (etaf-ref-update count
                                             (lambda (val) (+ val step)))))
                  (decrement (lambda ()
                              (etaf-ref-update count
                                             (lambda (val) (- val step)))))
                  (reset (lambda ()
                          (etaf-ref-set count initial))))
             (list :count count
                   :increment increment
                   :decrement decrement
                   :reset reset)))
  :template (lambda (data)
              `(div :class "counter"
                    (h3 "组合式 API - 计数器 (Composition API Counter)")
                    (p ,(format "当前值: %d" (etaf-ref-get (plist-get data :count))))
                    (div :class "buttons"
                         "[+] 增加 | [-] 减少 | [R] 重置"))))

;;; ----------------------------------------------------------------------------
;;; 示例 2: 购物车 - 使用 computed
;;; Example 2: Shopping Cart - Using computed
;;; ----------------------------------------------------------------------------

(etaf-define-component composition-cart
  :props '(:discount-rate)
  :setup (lambda (props)
           "购物车组件，演示 computed 计算属性的使用。
Shopping cart component demonstrating computed properties."
           (let* ((discount (or (plist-get props :discount-rate) 0))
                  ;; 商品列表：名称、价格、数量
                  (items (etaf-ref '((:name "苹果" :price 5.0 :quantity 2)
                                    (:name "香蕉" :price 3.0 :quantity 3)
                                    (:name "橙子" :price 4.0 :quantity 1))))
                  ;; 计算总价（自动响应 items 的变化）
                  (subtotal (etaf-computed
                            (lambda ()
                              (let ((sum 0))
                                (dolist (item (etaf-ref-get items))
                                  (let ((price (plist-get item :price))
                                        (qty (plist-get item :quantity)))
                                    (setq sum (+ sum (* price qty)))))
                                sum))))
                  ;; 计算折扣后的总价
                  (total (etaf-computed
                         (lambda ()
                           (let ((sub (etaf-computed-get subtotal)))
                             (* sub (- 1 discount))))))
                  ;; 计算商品总数
                  (item-count (etaf-computed
                              (lambda ()
                                (length (etaf-ref-get items)))))
                  ;; 增加一个商品
                  (add-item (lambda ()
                             (let* ((current (etaf-ref-get items))
                                    (new-item '(:name "新商品" :price 10.0 :quantity 1)))
                               (etaf-ref-set items (append current (list new-item)))))))
             (list :items items
                   :subtotal subtotal
                   :total total
                   :item-count item-count
                   :add-item add-item)))
  :template (lambda (data)
              `(div :class "cart"
                    (h3 "组合式 API - 购物车 (Composition API Cart)")
                    (p ,(format "商品种类: %d" (etaf-computed-get (plist-get data :item-count))))
                    (ul
                     ,@(mapcar (lambda (item)
                                `(li ,(format "%s - ¥%.2f × %d = ¥%.2f"
                                            (plist-get item :name)
                                            (plist-get item :price)
                                            (plist-get item :quantity)
                                            (* (plist-get item :price)
                                               (plist-get item :quantity)))))
                              (etaf-ref-get (plist-get data :items))))
                    (p ,(format "小计: ¥%.2f" (etaf-computed-get (plist-get data :subtotal))))
                    (p ,(format "总计: ¥%.2f" (etaf-computed-get (plist-get data :total))))
                    (p "[+] 添加商品"))))

;;; ----------------------------------------------------------------------------
;;; 示例 3: 用户资料 - 使用 watch
;;; Example 3: User Profile - Using watch
;;; ----------------------------------------------------------------------------

(etaf-define-component composition-profile
  :props '(:username)
  :setup (lambda (props)
           "用户资料组件，演示 watch 侦听器的使用。
User profile component demonstrating watch usage."
           (let* ((username (etaf-ref (or (plist-get props :username) "游客")))
                  (login-count (etaf-ref 0))
                  (status-message (etaf-ref "欢迎！"))
                  ;; 侦听用户名变化
                  (stop-watch
                   (etaf-watch
                    username
                    (lambda (new-name old-name on-invalidate)
                      (etaf-ref-set status-message
                                   (format "用户从 '%s' 变更为 '%s'" old-name new-name))
                      (etaf-ref-update login-count #'1+)
                      ;; 注册清理函数（在下次变化前执行）
                      (funcall on-invalidate
                              (lambda ()
                                (message "清理旧用户 %s 的数据..." old-name))))
                    (list :immediate t)))
                  (change-user (lambda ()
                                (let* ((names '("张三" "李四" "王五" "赵六"))
                                       (current (etaf-ref-get username))
                                       (available (cl-remove current names :test #'string=))
                                       (new-name (nth (random (length available)) available)))
                                  (etaf-ref-set username new-name)))))
             (list :username username
                   :login-count login-count
                   :status-message status-message
                   :change-user change-user)))
  :template (lambda (data)
              `(div :class "profile"
                    (h3 "组合式 API - 用户资料 (Composition API Profile)")
                    (p ,(format "当前用户: %s" (etaf-ref-get (plist-get data :username))))
                    (p ,(format "登录次数: %d" (etaf-ref-get (plist-get data :login-count))))
                    (p ,(format "状态: %s" (etaf-ref-get (plist-get data :status-message))))
                    (p "[C] 切换用户"))))

;;; ----------------------------------------------------------------------------
;;; 示例 4: 日志查看器 - 使用 watchEffect
;;; Example 4: Log Viewer - Using watchEffect
;;; ----------------------------------------------------------------------------

(etaf-define-component composition-logger
  :setup (lambda (props)
           "日志查看器组件，演示 watchEffect 的自动依赖追踪。
Log viewer component demonstrating watchEffect's automatic dependency tracking."
           (let* ((log-level (etaf-ref "INFO"))
                  (message-count (etaf-ref 0))
                  (logs (etaf-ref nil))
                  ;; watchEffect 自动追踪所有依赖
                  (stop-effect
                   (etaf-watch-effect
                    (lambda ()
                      ;; 这个函数会在任何依赖变化时自动重新执行
                      (let ((level (etaf-ref-get log-level))
                            (count (etaf-ref-get message-count)))
                        (message "[WatchEffect] 日志级别: %s, 消息数: %d" level count)
                        ;; 更新日志显示
                        (etaf-ref-set logs
                                     (format "自动监控 - 级别:%s 数量:%d" level count))))))
                  (change-level (lambda ()
                                 (let* ((levels '("DEBUG" "INFO" "WARN" "ERROR"))
                                        (current-idx (cl-position (etaf-ref-get log-level) 
                                                                 levels :test #'string=))
                                        (next-idx (mod (1+ current-idx) (length levels))))
                                   (etaf-ref-set log-level (nth next-idx levels)))))
                  (add-message (lambda ()
                                (etaf-ref-update message-count #'1+))))
             (list :log-level log-level
                   :message-count message-count
                   :logs logs
                   :change-level change-level
                   :add-message add-message)))
  :template (lambda (data)
              `(div :class "logger"
                    (h3 "组合式 API - 日志器 (Composition API Logger)")
                    (p ,(format "日志级别: %s" (etaf-ref-get (plist-get data :log-level))))
                    (p ,(format "消息数量: %d" (etaf-ref-get (plist-get data :message-count))))
                    (p ,(format "监控状态: %s" (etaf-ref-get (plist-get data :logs))))
                    (p "[L] 切换级别 | [M] 添加消息"))))

;;; ----------------------------------------------------------------------------
;;; 示例 5: 待办事项 - 使用 reactive（模拟）
;;; Example 5: Todo List - Using reactive (simulated with refs)
;;; ----------------------------------------------------------------------------

(etaf-define-component composition-todos
  :setup (lambda (props)
           "待办事项组件，演示响应式对象的使用。
Todo list component demonstrating reactive object usage."
           (let* (;; 使用 ref 包装复杂对象（模拟 reactive）
                  (todos (etaf-ref '((:id 1 :text "学习 ETAF" :done nil)
                                    (:id 2 :text "写组件示例" :done nil)
                                    (:id 3 :text "测试功能" :done nil))))
                  (filter (etaf-ref 'all)) ; all, active, completed
                  ;; 计算已完成数量
                  (completed-count (etaf-computed
                                   (lambda ()
                                     (cl-count-if (lambda (todo) (plist-get todo :done))
                                                 (etaf-ref-get todos)))))
                  ;; 计算未完成数量
                  (active-count (etaf-computed
                                (lambda ()
                                  (- (length (etaf-ref-get todos))
                                     (etaf-computed-get completed-count)))))
                  ;; 根据过滤器显示待办事项
                  (filtered-todos (etaf-computed
                                  (lambda ()
                                    (let ((all-todos (etaf-ref-get todos))
                                          (current-filter (etaf-ref-get filter)))
                                      (cond
                                       ((eq current-filter 'active)
                                        (cl-remove-if (lambda (todo) (plist-get todo :done))
                                                     all-todos))
                                       ((eq current-filter 'completed)
                                        (cl-remove-if-not (lambda (todo) (plist-get todo :done))
                                                         all-todos))
                                       (t all-todos))))))
                  (toggle-todo (lambda (id)
                                (let* ((current (etaf-ref-get todos))
                                       (updated (mapcar
                                               (lambda (todo)
                                                 (if (= (plist-get todo :id) id)
                                                     (plist-put (copy-sequence todo) :done
                                                               (not (plist-get todo :done)))
                                                   todo))
                                               current)))
                                  (etaf-ref-set todos updated))))
                  (set-filter (lambda (new-filter)
                               (etaf-ref-set filter new-filter))))
             (list :todos todos
                   :filter filter
                   :completed-count completed-count
                   :active-count active-count
                   :filtered-todos filtered-todos
                   :toggle-todo toggle-todo
                   :set-filter set-filter)))
  :template (lambda (data)
              `(div :class "todos"
                    (h3 "组合式 API - 待办事项 (Composition API Todos)")
                    (p ,(format "过滤器: %s" (etaf-ref-get (plist-get data :filter))))
                    (p ,(format "活跃: %d | 已完成: %d"
                               (etaf-computed-get (plist-get data :active-count))
                               (etaf-computed-get (plist-get data :completed-count))))
                    (ul
                     ,@(mapcar (lambda (todo)
                                `(li ,(format "[%s] %s"
                                            (if (plist-get todo :done) "✓" " ")
                                            (plist-get todo :text))))
                              (etaf-computed-get (plist-get data :filtered-todos))))
                    (p "[A] 全部 | [T] 活跃 | [D] 已完成"))))

;;; ============================================================================
;;; Vue 2 选项式 API 示例 (Options API Examples)
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; 示例 6: 选项式计数器 - 使用 data 和 methods
;;; Example 6: Options Counter - Using data and methods
;;; ----------------------------------------------------------------------------

(etaf-define-component options-counter
  :props '(:initial :max)
  :data (lambda ()
          "定义组件的响应式数据。
Define component's reactive data."
          (list :count 0
                :history nil))
  :methods (list
            :increment (lambda ()
                        "增加计数器的方法。
Method to increment counter."
                        (let* ((count-ref (plist-get this :count))
                               (history-ref (plist-get this :history))
                               (current (etaf-ref-get count-ref))
                               (max (or (plist-get this :max) 999)))
                          (when (< current max)
                            (etaf-ref-set count-ref (1+ current))
                            (etaf-ref-set history-ref
                                         (cons (format "增加到 %d" (1+ current))
                                               (etaf-ref-get history-ref))))))
            :decrement (lambda ()
                        (let ((count-ref (plist-get this :count))
                              (history-ref (plist-get this :history)))
                          (when (> (etaf-ref-get count-ref) 0)
                            (etaf-ref-update count-ref #'1-)
                            (etaf-ref-set history-ref
                                         (cons (format "减少到 %d" (etaf-ref-get count-ref))
                                               (etaf-ref-get history-ref))))))
            :reset (lambda ()
                    (let ((count-ref (plist-get this :count))
                          (history-ref (plist-get this :history)))
                      (etaf-ref-set count-ref 0)
                      (etaf-ref-set history-ref (cons "重置为 0" (etaf-ref-get history-ref))))))
  :computed (list
             :is-max (lambda ()
                      "计算属性：是否达到最大值。
Computed property: whether max value is reached."
                      (let* ((count-ref (plist-get this :count))
                             (count (etaf-ref-get count-ref))
                             (max (or (plist-get this :max) 999)))
                        (>= count max)))
             :is-min (lambda ()
                      (let ((count-ref (plist-get this :count)))
                        (<= (etaf-ref-get count-ref) 0)))
             :status (lambda ()
                      (let ((is-max (etaf-computed-get (plist-get this :is-max)))
                            (is-min (etaf-computed-get (plist-get this :is-min))))
                        (cond
                         (is-max "已达最大值")
                         (is-min "已达最小值")
                         (t "正常")))))
  :mounted (lambda ()
            "组件挂载时的生命周期钩子。
Lifecycle hook when component is mounted."
            (message "Options Counter 组件已挂载！"))
  :template (lambda (data)
              `(div :class "options-counter"
                    (h3 "选项式 API - 计数器 (Options API Counter)")
                    (p ,(format "计数: %d" (etaf-ref-get (plist-get data :count))))
                    (p ,(format "状态: %s" (etaf-computed-get (plist-get data :status))))
                    (p ,(format "历史记录: %s"
                               (let ((hist (etaf-ref-get (plist-get data :history))))
                                 (if hist
                                     (mapconcat #'identity (cl-subseq hist 0 (min 3 (length hist))) ", ")
                                   "无"))))
                    (p "[+] 增加 | [-] 减少 | [R] 重置"))))

;;; ----------------------------------------------------------------------------
;;; 示例 7: 计算器 - 使用 computed
;;; Example 7: Calculator - Using computed
;;; ----------------------------------------------------------------------------

(etaf-define-component options-calculator
  :data (lambda ()
          (list :num1 0
                :num2 0
                :operator 'add))
  :methods (list
            :set-num1 (lambda (val)
                       (etaf-ref-set (plist-get this :num1) val))
            :set-num2 (lambda (val)
                       (etaf-ref-set (plist-get this :num2) val))
            :cycle-operator (lambda ()
                             (let* ((op-ref (plist-get this :operator))
                                    (current (etaf-ref-get op-ref))
                                    (ops '(add subtract multiply divide))
                                    (current-idx (cl-position current ops))
                                    (next-idx (mod (1+ current-idx) (length ops))))
                               (etaf-ref-set op-ref (nth next-idx ops)))))
  :computed (list
             :result (lambda ()
                      "计算结果的计算属性。
Computed property for calculation result."
                      (let* ((num1 (etaf-ref-get (plist-get this :num1)))
                             (num2 (etaf-ref-get (plist-get this :num2)))
                             (op (etaf-ref-get (plist-get this :operator))))
                        (cond
                         ((eq op 'add) (+ num1 num2))
                         ((eq op 'subtract) (- num1 num2))
                         ((eq op 'multiply) (* num1 num2))
                         ((eq op 'divide)
                          (if (= num2 0) "错误：除数为零" (/ (float num1) num2)))
                         (t 0))))
             :operator-symbol (lambda ()
                               (let ((op (etaf-ref-get (plist-get this :operator))))
                                 (cond
                                  ((eq op 'add) "+")
                                  ((eq op 'subtract) "-")
                                  ((eq op 'multiply) "×")
                                  ((eq op 'divide) "÷")
                                  (t "?")))))
  :template (lambda (data)
              `(div :class "calculator"
                    (h3 "选项式 API - 计算器 (Options API Calculator)")
                    (p ,(format "%d %s %d = %s"
                               (etaf-ref-get (plist-get data :num1))
                               (etaf-computed-get (plist-get data :operator-symbol))
                               (etaf-ref-get (plist-get data :num2))
                               (let ((result (etaf-computed-get (plist-get data :result))))
                                 (if (numberp result)
                                     (format "%.2f" result)
                                   result))))
                    (p "[1] num1++ | [2] num2++ | [O] 切换运算符"))))

;;; ----------------------------------------------------------------------------
;;; 示例 8: 表单验证 - 使用 watch
;;; Example 8: Form Validation - Using watch
;;; ----------------------------------------------------------------------------

(etaf-define-component options-form
  :data (lambda ()
          (list :email ""
                :password ""
                :email-error nil
                :password-error nil
                :submit-count 0))
  :methods (list
            :set-email (lambda (val)
                        (etaf-ref-set (plist-get this :email) val))
            :set-password (lambda (val)
                           (etaf-ref-set (plist-get this :password) val))
            :submit (lambda ()
                     (let* ((email-err (etaf-ref-get (plist-get this :email-error)))
                            (pass-err (etaf-ref-get (plist-get this :password-error))))
                       (if (or email-err pass-err)
                           (message "表单有错误，无法提交")
                         (etaf-ref-update (plist-get this :submit-count) #'1+)
                         (message "表单提交成功！")))))
  :computed (list
             :is-valid (lambda ()
                        (let ((email-err (etaf-ref-get (plist-get this :email-error)))
                              (pass-err (etaf-ref-get (plist-get this :password-error))))
                          (not (or email-err pass-err)))))
  :watch (list
          :email (lambda (new-val old-val)
                  "监听邮箱变化并验证。
Watch email changes and validate."
                  (let ((error-ref (plist-get this :email-error)))
                    (if (or (string-empty-p new-val)
                            (not (string-match-p "@" new-val)))
                        (etaf-ref-set error-ref "邮箱格式无效")
                      (etaf-ref-set error-ref nil))))
          :password (lambda (new-val old-val)
                     (let ((error-ref (plist-get this :password-error)))
                       (if (< (length new-val) 6)
                           (etaf-ref-set error-ref "密码至少6个字符")
                         (etaf-ref-set error-ref nil)))))
  :template (lambda (data)
              `(div :class "form"
                    (h3 "选项式 API - 表单验证 (Options API Form)")
                    (p ,(format "邮箱: %s %s"
                               (etaf-ref-get (plist-get data :email))
                               (or (etaf-ref-get (plist-get data :email-error)) "✓")))
                    (p ,(format "密码: %s %s"
                               (let ((pwd (etaf-ref-get (plist-get data :password))))
                                 (make-string (length pwd) ?*))
                               (or (etaf-ref-get (plist-get data :password-error)) "✓")))
                    (p ,(format "状态: %s" (if (etaf-computed-get (plist-get data :is-valid))
                                             "有效 ✓" "无效 ✗")))
                    (p ,(format "提交次数: %d" (etaf-ref-get (plist-get data :submit-count))))
                    (p "[E] 设置邮箱 | [P] 设置密码 | [S] 提交"))))

;;; ----------------------------------------------------------------------------
;;; 示例 9: 仪表板 - 综合示例
;;; Example 9: Dashboard - Comprehensive Example
;;; ----------------------------------------------------------------------------

(etaf-define-component options-dashboard
  :data (lambda ()
          (list :cpu-usage 45
                :memory-usage 67
                :disk-usage 23
                :update-count 0
                :last-update nil))
  :methods (list
            :simulate-update (lambda ()
                              "模拟数据更新。
Simulate data update."
                              (etaf-ref-set (plist-get this :cpu-usage) (+ 20 (random 60)))
                              (etaf-ref-set (plist-get this :memory-usage) (+ 30 (random 50)))
                              (etaf-ref-set (plist-get this :disk-usage) (+ 10 (random 70)))
                              (etaf-ref-update (plist-get this :update-count) #'1+)
                              (etaf-ref-set (plist-get this :last-update)
                                           (format-time-string "%H:%M:%S"))))
  :computed (list
             :cpu-status (lambda ()
                          (let ((usage (etaf-ref-get (plist-get this :cpu-usage))))
                            (cond ((> usage 80) "危险")
                                  ((> usage 60) "警告")
                                  (t "正常"))))
             :memory-status (lambda ()
                             (let ((usage (etaf-ref-get (plist-get this :memory-usage))))
                               (cond ((> usage 80) "危险")
                                     ((> usage 60) "警告")
                                     (t "正常"))))
             :overall-health (lambda ()
                              (let* ((cpu-usage (etaf-ref-get (plist-get this :cpu-usage)))
                                     (mem-usage (etaf-ref-get (plist-get this :memory-usage)))
                                     (disk-usage (etaf-ref-get (plist-get this :disk-usage)))
                                     (avg (/ (+ cpu-usage mem-usage disk-usage) 3.0)))
                                (cond ((> avg 75) "不健康")
                                      ((> avg 50) "一般")
                                      (t "健康")))))
  :watch (list
          :cpu-usage (lambda (new-val old-val)
                      (when (and (> new-val 80) (<= old-val 80))
                        (message "警告：CPU 使用率过高！")))
          :memory-usage (lambda (new-val old-val)
                         (when (and (> new-val 80) (<= old-val 80))
                           (message "警告：内存使用率过高！"))))
  :mounted (lambda ()
            (message "Dashboard 组件已挂载并开始监控"))
  :template (lambda (data)
              `(div :class "dashboard"
                    (h3 "选项式 API - 系统仪表板 (Options API Dashboard)")
                    (p ,(format "CPU: %d%% [%s]"
                               (etaf-ref-get (plist-get data :cpu-usage))
                               (etaf-computed-get (plist-get data :cpu-status))))
                    (p ,(format "内存: %d%% [%s]"
                               (etaf-ref-get (plist-get data :memory-usage))
                               (etaf-computed-get (plist-get data :memory-status))))
                    (p ,(format "磁盘: %d%%"
                               (etaf-ref-get (plist-get data :disk-usage))))
                    (p ,(format "整体健康: %s"
                               (etaf-computed-get (plist-get data :overall-health))))
                    (p ,(format "更新次数: %d | 最后更新: %s"
                               (etaf-ref-get (plist-get data :update-count))
                               (or (etaf-ref-get (plist-get data :last-update)) "未更新")))
                    (p "[U] 更新数据"))))

;;; ============================================================================
;;; 交互式演示 (Interactive Demo)
;;; ============================================================================

;;;###autoload
(defun etaf-component-demo ()
  "启动组件示例的交互式演示。
Launch interactive demo of component examples."
  (interactive)
  (let ((choice (completing-read
                "选择示例 (Select example): "
                '("1. 组合式-计数器 (Composition Counter)"
                  "2. 组合式-购物车 (Composition Cart)"
                  "3. 组合式-用户资料 (Composition Profile)"
                  "4. 组合式-日志器 (Composition Logger)"
                  "5. 组合式-待办事项 (Composition Todos)"
                  "6. 选项式-计数器 (Options Counter)"
                  "7. 选项式-计算器 (Options Calculator)"
                  "8. 选项式-表单验证 (Options Form)"
                  "9. 选项式-仪表板 (Options Dashboard)"
                  "全部 (All Examples)")
                nil t)))
    (cond
     ((string-prefix-p "1." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(composition-counter :initial-count 10 :step 5)))
     
     ((string-prefix-p "2." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(composition-cart :discount-rate 0.1)))
     
     ((string-prefix-p "3." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(composition-profile :username "张三")))
     
     ((string-prefix-p "4." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(composition-logger)))
     
     ((string-prefix-p "5." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(composition-todos)))
     
     ((string-prefix-p "6." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(options-counter :initial 0 :max 100)))
     
     ((string-prefix-p "7." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(options-calculator)))
     
     ((string-prefix-p "8." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(options-form)))
     
     ((string-prefix-p "9." choice)
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(options-dashboard)))
     
     (t ; All Examples
      (etaf-paint-to-buffer "*ETAF Component Demo*"
        '(div :class "demo-container"
              (h2 "ETAF 组件系统示例 (Component System Examples)")
              (hr)
              (h3 "=== Vue 3 组合式 API ===")
              (composition-counter :initial-count 0 :step 1)
              (hr)
              (composition-cart :discount-rate 0.15)
              (hr)
              (composition-profile :username "游客")
              (hr)
              (composition-logger)
              (hr)
              (composition-todos)
              (hr)
              (h3 "=== Vue 2 选项式 API ===")
              (options-counter :initial 0 :max 50)
              (hr)
              (options-calculator)
              (hr)
              (options-form)
              (hr)
              (options-dashboard)
              (hr)
              (p "示例演示完成！"))
        nil
        ;; 添加一些样式
        '(".demo-container { padding: 2cw; }"
          "hr { margin: 1 0; }"
          "h2 { font-weight: bold; }"
          "h3 { font-weight: bold; color: blue; }"
          ".counter, .cart, .profile, .logger, .todos { margin: 1 0; padding: 1cw; }"
          ".options-counter, .calculator, .form, .dashboard { margin: 1 0; padding: 1cw; }"
          ".buttons { margin: 0.5 0; }")
        80)))))

;;;###autoload
(defun etaf-component-demo-simple (component-name)
  "渲染单个组件示例。
Render a single component example.
COMPONENT-NAME: 组件名称 (component name symbol)"
  (interactive 
   (list (intern (completing-read
                  "选择组件: "
                  '("composition-counter" "composition-cart" "composition-profile"
                    "composition-logger" "composition-todos"
                    "options-counter" "options-calculator" "options-form"
                    "options-dashboard")
                  nil t))))
  (etaf-paint-to-buffer "*ETAF Component*"
    (list component-name)
    nil
    '("div { padding: 2cw; }"
      "p { margin: 0.5 0; }")
    80))

(provide 'etaf-component-examples)
;;; etaf-component-examples.el ends here
