;;; etaf-interactive-examples.el --- 交互式组件示例（在 buffer 中展示） -*- lexical-binding: t; -*-

;;; Commentary:

;; 本文件展示了 ETAF 组件系统的交互式使用方法。
;; 所有示例都会在 buffer 中渲染和展示，支持交互操作。
;;
;; 使用方法：
;; M-x etaf-interactive-demo           ; 运行交互式演示
;; M-x etaf-demo-simple-card           ; 简单卡片示例
;; M-x etaf-demo-styled-button         ; 带样式的按钮示例
;; M-x etaf-demo-counter               ; 计数器示例（可点击交互）
;; M-x etaf-demo-todo-app              ; Todo 应用示例
;; M-x etaf-demo-user-profile          ; 用户资料卡片
;; M-x etaf-demo-layout-showcase       ; 布局展示

;;; Code:

;; 尝试加载完整的 etaf，如果失败则使用简化渲染
(defvar etaf-demo--full-render-available nil
  "是否可以使用完整的 ETAF 渲染。")

(condition-case nil
    (progn
      (require 'etaf)
      (setq etaf-demo--full-render-available t))
  (error
   (require 'etaf-etml)
   (setq etaf-demo--full-render-available nil)))

;;; ============================================================================
;;; 辅助函数：在 buffer 中展示 ETML 内容
;;; ============================================================================

(defvar etaf-demo-buffer-name "*ETAF Demo*"
  "ETAF 演示使用的 buffer 名称。")

(defun etaf-demo--render-to-buffer (etml &optional title width data)
  "将 ETML 渲染到演示 buffer 中。
ETML 是 ETML 表达式。
TITLE 是可选的标题。
WIDTH 是可选的视口宽度。
DATA 是可选的模板数据。"
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name))
         (width (or width 600))
         (rendered-string
          (if etaf-demo--full-render-available
              (condition-case err
                  (if data
                      (etaf-string-with-data etml data width nil)
                    (etaf-string etml width nil))
                (error (format "渲染错误: %S\n\n使用简化输出:\n%S" err etml)))
            ;; 简化渲染：直接输出 ETML 结构
            (format "（完整渲染需要 's' 库，显示 ETML 结构）\n\n%s"
                    (pp-to-string (if data
                                      (etaf-etml-render etml data)
                                    etml))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; 添加标题
        (when title
          (insert (propertize (concat "=== " title " ===\n\n")
                              'face '(:weight bold :height 1.2)))
          (insert "按 'q' 退出演示，按 'g' 刷新\n")
          (insert (make-string 50 ?-) "\n\n"))
        ;; 插入渲染的内容
        (insert rendered-string)
        (insert "\n\n")
        (insert (make-string 50 ?-) "\n")
        (insert "ETML 源代码:\n")
        (insert (pp-to-string etml)))
      ;; 设置只读和按键绑定
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "g") (lambda ()
                                 (interactive)
                                 (etaf-demo--render-to-buffer etml title width data)))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun etaf-demo--render-multiple (items &optional title width)
  "在 buffer 中渲染多个 ETML 表达式。
ITEMS 是 ((label . etml) ...) 的列表。
TITLE 是可选的总标题。
WIDTH 是可选的视口宽度。"
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name))
         (width (or width 600)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; 添加标题
        (when title
          (insert (propertize (concat "=== " title " ===\n\n")
                              'face '(:weight bold :height 1.2)))
          (insert "按 'q' 退出演示\n")
          (insert (make-string 60 ?-) "\n\n"))
        ;; 渲染每个 item
        (dolist (item items)
          (let* ((label (car item))
                 (etml (cdr item))
                 (rendered (condition-case err
                               (etaf-string etml width nil)
                             (error (format "渲染错误: %S" err)))))
            (insert (propertize (concat "▶ " label "\n")
                                'face '(:weight bold :foreground "blue")))
            (insert rendered)
            (insert "\n\n")))
        (insert (make-string 60 ?-) "\n"))
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;; ============================================================================
;;; 示例 1：简单卡片
;;; ============================================================================

(defun etaf-demo-simple-card ()
  "演示：简单的卡片组件。"
  (interactive)
  (let ((etml
         '(html
           (head
            (style "
              .card {
                width: 300px;
                padding-left: 16px;
                padding-right: 16px;
                padding-top: 12px;
                padding-bottom: 12px;
                border-top-width: 1px;
                border-right-width: 1px;
                border-bottom-width: 1px;
                border-left-width: 1px;
                margin-bottom: 16px;
              }
              .card-title {
                margin-bottom: 8px;
              }
              .card-body {
                color: #666;
              }
            "))
           (body
            (div :class "card"
                 (div :class "card-title" "🎉 欢迎使用 ETAF")
                 (div :class "card-body"
                      "ETAF 是一个基于 Emacs Lisp 的文本应用框架，"
                      "支持类似 HTML/CSS 的布局和样式。"))))))
    (etaf-demo--render-to-buffer etml "简单卡片示例" 400)))

;;; ============================================================================
;;; 示例 2：带样式的按钮
;;; ============================================================================

(defun etaf-demo-styled-button ()
  "演示：带样式的按钮。"
  (interactive)
  (let ((etml
         '(html
           (head
            (style "
              .btn {
                padding-left: 12px;
                padding-right: 12px;
                padding-top: 6px;
                padding-bottom: 6px;
                margin-right: 8px;
                margin-bottom: 8px;
                border-top-width: 1px;
                border-right-width: 1px;
                border-bottom-width: 1px;
                border-left-width: 1px;
              }
              .btn-primary {
                background-color: #007bff;
                color: white;
              }
              .btn-success {
                background-color: #28a745;
                color: white;
              }
              .btn-danger {
                background-color: #dc3545;
                color: white;
              }
              .btn-warning {
                background-color: #ffc107;
                color: black;
              }
              .container {
                padding-left: 16px;
                padding-top: 16px;
              }
            "))
           (body
            (div :class "container"
                 (div :style "margin-bottom: 16px"
                      (span "按钮样式演示："))
                 (div
                  (button :class "btn btn-primary" "主要按钮")
                  (button :class "btn btn-success" "成功按钮")
                  (button :class "btn btn-danger" "危险按钮")
                  (button :class "btn btn-warning" "警告按钮")))))))
    (etaf-demo--render-to-buffer etml "按钮样式示例" 500)))

;;; ============================================================================
;;; 示例 3：计数器（可交互）
;;; ============================================================================

(defvar etaf-demo--counter-value 0
  "计数器当前值。")

(defun etaf-demo--counter-increment ()
  "增加计数器值并刷新显示。"
  (interactive)
  (setq etaf-demo--counter-value (1+ etaf-demo--counter-value))
  (etaf-demo-counter))

(defun etaf-demo--counter-decrement ()
  "减少计数器值并刷新显示。"
  (interactive)
  (setq etaf-demo--counter-value (1- etaf-demo--counter-value))
  (etaf-demo-counter))

(defun etaf-demo--counter-reset ()
  "重置计数器值并刷新显示。"
  (interactive)
  (setq etaf-demo--counter-value 0)
  (etaf-demo-counter))

(defun etaf-demo-counter ()
  "演示：交互式计数器。"
  (interactive)
  (let* ((count etaf-demo--counter-value)
         (buffer (get-buffer-create etaf-demo-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "=== 交互式计数器示例 ===\n\n"
                            'face '(:weight bold :height 1.2)))
        (insert "按键说明：\n")
        (insert "  + 或 = : 增加计数\n")
        (insert "  - : 减少计数\n")
        (insert "  0 : 重置计数\n")
        (insert "  q : 退出\n\n")
        (insert (make-string 40 ?-) "\n\n")
        
        ;; 渲染计数器显示
        (insert (propertize "当前计数: " 'face '(:weight bold)))
        (insert (propertize (format "%d" count)
                            'face `(:foreground ,(cond
                                                   ((> count 0) "green")
                                                   ((< count 0) "red")
                                                   (t "black"))
                                    :weight bold
                                    :height 1.5)))
        (insert "\n\n")
        
        ;; 显示交互按钮（文本形式）
        (insert "[")
        (insert-text-button "-"
                            'action (lambda (_) (etaf-demo--counter-decrement))
                            'face '(:foreground "red" :weight bold))
        (insert "]  ")
        (insert "[")
        (insert-text-button "重置"
                            'action (lambda (_) (etaf-demo--counter-reset))
                            'face '(:foreground "gray" :weight bold))
        (insert "]  ")
        (insert "[")
        (insert-text-button "+"
                            'action (lambda (_) (etaf-demo--counter-increment))
                            'face '(:foreground "green" :weight bold))
        (insert "]\n\n")
        
        (insert (make-string 40 ?-) "\n")
        (insert "\n提示：点击上方按钮或使用快捷键操作计数器。\n"))
      
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "+") #'etaf-demo--counter-increment)
      (local-set-key (kbd "=") #'etaf-demo--counter-increment)
      (local-set-key (kbd "-") #'etaf-demo--counter-decrement)
      (local-set-key (kbd "0") #'etaf-demo--counter-reset)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;; ============================================================================
;;; 示例 4：用户资料卡片
;;; ============================================================================

(defun etaf-demo-user-profile ()
  "演示：用户资料卡片。"
  (interactive)
  (let ((etml
         '(html
           (head
            (style "
              .profile-card {
                width: 280px;
                padding-left: 20px;
                padding-right: 20px;
                padding-top: 20px;
                padding-bottom: 20px;
                border-top-width: 1px;
                border-right-width: 1px;
                border-bottom-width: 1px;
                border-left-width: 1px;
              }
              .avatar {
                width: 60px;
                height: 60px;
                background-color: #007bff;
                color: white;
                margin-bottom: 12px;
              }
              .name {
                margin-bottom: 4px;
              }
              .title {
                color: #666;
                margin-bottom: 12px;
              }
              .stats {
                margin-top: 12px;
                padding-top: 12px;
                border-top-width: 1px;
              }
              .stat-item {
                margin-right: 16px;
              }
              .stat-value {
                color: #007bff;
              }
            "))
           (body
            (div :class "profile-card"
                 (div :class "avatar" "👤")
                 (div :class "name" "张三")
                 (div :class "title" "高级软件工程师")
                 (div "📧 zhangsan@example.com")
                 (div "📍 北京市海淀区")
                 (div :class "stats"
                      (span :class "stat-item"
                            (span :class "stat-value" "128")
                            " 项目")
                      (span :class "stat-item"
                            (span :class "stat-value" "1.2k")
                            " 关注者")
                      (span :class "stat-item"
                            (span :class "stat-value" "256")
                            " 关注")))))))
    (etaf-demo--render-to-buffer etml "用户资料卡片" 400)))

;;; ============================================================================
;;; 示例 5：Todo 列表
;;; ============================================================================

(defvar etaf-demo--todo-items
  '((:id 1 :text "学习 ETAF 框架" :done t)
    (:id 2 :text "编写组件示例" :done t)
    (:id 3 :text "测试交互功能" :done nil)
    (:id 4 :text "编写文档" :done nil))
  "Todo 列表数据。")

(defun etaf-demo--toggle-todo (id)
  "切换 Todo 项的完成状态。"
  (setq etaf-demo--todo-items
        (mapcar (lambda (item)
                  (if (eq (plist-get item :id) id)
                      (plist-put (copy-sequence item) :done
                                 (not (plist-get item :done)))
                    item))
                etaf-demo--todo-items))
  (etaf-demo-todo-app))

(defun etaf-demo--add-todo (text)
  "添加新的 Todo 项。"
  (let ((new-id (1+ (apply #'max (mapcar (lambda (i) (plist-get i :id))
                                         etaf-demo--todo-items)))))
    (setq etaf-demo--todo-items
          (append etaf-demo--todo-items
                  (list (list :id new-id :text text :done nil)))))
  (etaf-demo-todo-app))

(defun etaf-demo-todo-app ()
  "演示：Todo 应用。"
  (interactive)
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name))
         (done-count (length (seq-filter (lambda (i) (plist-get i :done))
                                         etaf-demo--todo-items)))
         (total-count (length etaf-demo--todo-items)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "=== Todo 应用示例 ===\n\n"
                            'face '(:weight bold :height 1.2)))
        (insert "按键说明：\n")
        (insert "  a : 添加新任务\n")
        (insert "  1-9 : 切换对应任务的完成状态\n")
        (insert "  q : 退出\n\n")
        (insert (make-string 50 ?-) "\n\n")
        
        ;; 显示统计
        (insert (propertize (format "📋 任务列表 (%d/%d 已完成)\n\n"
                                    done-count total-count)
                            'face '(:weight bold)))
        
        ;; 渲染 Todo 列表
        (let ((index 0))
          (dolist (item etaf-demo--todo-items)
            (let* ((id (plist-get item :id))
                   (text (plist-get item :text))
                   (done (plist-get item :done))
                   (checkbox (if done "☑" "☐"))
                   (text-face (if done
                                  '(:strike-through t :foreground "gray")
                                '(:foreground "black"))))
              (setq index (1+ index))
              (insert (format "[%d] " index))
              (insert-text-button checkbox
                                  'action `(lambda (_) (etaf-demo--toggle-todo ,id))
                                  'face '(:weight bold))
              (insert " ")
              (insert (propertize text 'face text-face))
              (insert "\n"))))
        
        (insert "\n" (make-string 50 ?-) "\n")
        (insert "\n点击复选框或按数字键切换任务状态。\n"))
      
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "a") (lambda ()
                                 (interactive)
                                 (let ((text (read-string "新任务: ")))
                                   (when (> (length text) 0)
                                     (etaf-demo--add-todo text)))))
      ;; 绑定数字键
      (dotimes (i 9)
        (let ((num (1+ i)))
          (local-set-key (kbd (number-to-string num))
                         `(lambda ()
                            (interactive)
                            (when (<= ,num (length etaf-demo--todo-items))
                              (etaf-demo--toggle-todo
                               (plist-get (nth ,(1- num) etaf-demo--todo-items) :id)))))))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;; ============================================================================
;;; 示例 6：布局展示
;;; ============================================================================

(defun etaf-demo-layout-showcase ()
  "演示：布局展示。"
  (interactive)
  (let ((etml
         '(html
           (head
            (style "
              .container {
                width: 500px;
                padding-left: 16px;
                padding-right: 16px;
                padding-top: 16px;
                padding-bottom: 16px;
              }
              .section {
                margin-bottom: 20px;
              }
              .section-title {
                margin-bottom: 8px;
                padding-bottom: 4px;
                border-bottom-width: 1px;
              }
              .row {
                margin-bottom: 8px;
              }
              .box {
                padding-left: 12px;
                padding-right: 12px;
                padding-top: 8px;
                padding-bottom: 8px;
                margin-right: 8px;
                border-top-width: 1px;
                border-right-width: 1px;
                border-bottom-width: 1px;
                border-left-width: 1px;
              }
              .box-primary {
                background-color: #e3f2fd;
              }
              .box-success {
                background-color: #e8f5e9;
              }
              .box-warning {
                background-color: #fff3e0;
              }
              .alert {
                padding-left: 12px;
                padding-right: 12px;
                padding-top: 8px;
                padding-bottom: 8px;
                margin-bottom: 8px;
                border-left-width: 4px;
              }
              .alert-info {
                background-color: #e3f2fd;
                border-left-color: #2196f3;
              }
              .alert-warning {
                background-color: #fff3e0;
                border-left-color: #ff9800;
              }
              .alert-success {
                background-color: #e8f5e9;
                border-left-color: #4caf50;
              }
            "))
           (body
            (div :class "container"
                 ;; 盒子布局
                 (div :class "section"
                      (div :class "section-title" "📦 盒子布局")
                      (div :class "row"
                           (span :class "box box-primary" "主要")
                           (span :class "box box-success" "成功")
                           (span :class "box box-warning" "警告")))
                 
                 ;; 提示框
                 (div :class "section"
                      (div :class "section-title" "💡 提示框样式")
                      (div :class "alert alert-info"
                           "ℹ️ 这是一条信息提示。")
                      (div :class "alert alert-warning"
                           "⚠️ 这是一条警告提示。")
                      (div :class "alert alert-success"
                           "✅ 这是一条成功提示。"))
                 
                 ;; 嵌套结构
                 (div :class "section"
                      (div :class "section-title" "📁 嵌套结构")
                      (div :class "box"
                           (div "父级容器")
                           (div :class "box box-primary"
                                (div "子级容器 1")
                                (div :class "box box-success"
                                     "孙级容器"))
                           (div :class "box box-warning"
                                "子级容器 2"))))))))
    (etaf-demo--render-to-buffer etml "布局展示" 600)))

;;; ============================================================================
;;; 示例 7：表格展示
;;; ============================================================================

(defun etaf-demo-table ()
  "演示：表格展示。"
  (interactive)
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name))
         (data '(("姓名" "年龄" "城市" "职业")
                 ("张三" "28" "北京" "工程师")
                 ("李四" "32" "上海" "设计师")
                 ("王五" "25" "广州" "产品经理")
                 ("赵六" "35" "深圳" "项目经理"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "=== 表格展示示例 ===\n\n"
                            'face '(:weight bold :height 1.2)))
        (insert "按 'q' 退出演示\n\n")
        (insert (make-string 50 ?-) "\n\n")
        
        ;; 渲染表格
        (let* ((col-widths '(10 8 10 12))
               (separator (concat "+"
                                  (mapconcat (lambda (w)
                                               (make-string (+ w 2) ?-))
                                             col-widths "+")
                                  "+\n")))
          (insert separator)
          (dolist (row data)
            (insert "|")
            (cl-loop for cell in row
                     for width in col-widths
                     do (insert (format " %-*s |" width cell)))
            (insert "\n")
            (when (eq row (car data))  ; 表头后加分隔线
              (insert separator)))
          (insert separator))
        
        (insert "\n\n提示：这是一个简单的文本表格实现。\n"))
      
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;; ============================================================================
;;; 示例 8：进度条
;;; ============================================================================

(defvar etaf-demo--progress 0
  "当前进度值（0-100）。")

(defun etaf-demo--progress-update (delta)
  "更新进度值。"
  (setq etaf-demo--progress
        (max 0 (min 100 (+ etaf-demo--progress delta))))
  (etaf-demo-progress-bar))

(defun etaf-demo-progress-bar ()
  "演示：进度条。"
  (interactive)
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name))
         (progress etaf-demo--progress)
         (bar-width 40)
         (filled (/ (* progress bar-width) 100))
         (empty (- bar-width filled)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "=== 进度条示例 ===\n\n"
                            'face '(:weight bold :height 1.2)))
        (insert "按键说明：\n")
        (insert "  ← / h : 减少 10%\n")
        (insert "  → / l : 增加 10%\n")
        (insert "  0 : 重置为 0%\n")
        (insert "  q : 退出\n\n")
        (insert (make-string 50 ?-) "\n\n")
        
        ;; 渲染进度条
        (insert "进度: ")
        (insert (propertize (make-string filled ?█)
                            'face '(:foreground "green")))
        (insert (propertize (make-string empty ?░)
                            'face '(:foreground "gray")))
        (insert (format " %3d%%\n\n" progress))
        
        ;; 显示状态
        (insert (cond
                 ((= progress 0) "状态: 未开始")
                 ((< progress 30) "状态: 刚开始...")
                 ((< progress 70) "状态: 进行中...")
                 ((< progress 100) "状态: 即将完成!")
                 (t "状态: ✅ 已完成!")))
        (insert "\n\n")
        (insert (make-string 50 ?-) "\n"))
      
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "<left>") (lambda () (interactive) (etaf-demo--progress-update -10)))
      (local-set-key (kbd "h") (lambda () (interactive) (etaf-demo--progress-update -10)))
      (local-set-key (kbd "<right>") (lambda () (interactive) (etaf-demo--progress-update 10)))
      (local-set-key (kbd "l") (lambda () (interactive) (etaf-demo--progress-update 10)))
      (local-set-key (kbd "0") (lambda () (interactive)
                                 (setq etaf-demo--progress 0)
                                 (etaf-demo-progress-bar)))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;;; ============================================================================
;;; 主演示入口
;;; ============================================================================

(defun etaf-interactive-demo ()
  "ETAF 交互式演示主入口。"
  (interactive)
  (let* ((buffer (get-buffer-create etaf-demo-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "╔═══════════════════════════════════════════════════════╗\n"
                            'face '(:foreground "blue")))
        (insert (propertize "║       🚀 ETAF 交互式组件演示                         ║\n"
                            'face '(:foreground "blue" :weight bold)))
        (insert (propertize "╚═══════════════════════════════════════════════════════╝\n\n"
                            'face '(:foreground "blue")))
        
        (insert "欢迎使用 ETAF 框架！这里展示了各种交互式组件示例。\n")
        (insert "点击下面的链接或按对应数字键运行示例：\n\n")
        (insert (make-string 55 ?-) "\n\n")
        
        ;; 示例列表
        (insert "[")
        (insert-text-button "1"
                            'action (lambda (_) (etaf-demo-simple-card))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "简单卡片"
                            'action (lambda (_) (etaf-demo-simple-card))
                            'face '(:foreground "blue" :underline t))
        (insert " - 展示基本的卡片布局\n\n")
        
        (insert "[")
        (insert-text-button "2"
                            'action (lambda (_) (etaf-demo-styled-button))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "按钮样式"
                            'action (lambda (_) (etaf-demo-styled-button))
                            'face '(:foreground "blue" :underline t))
        (insert " - 展示各种按钮样式\n\n")
        
        (insert "[")
        (insert-text-button "3"
                            'action (lambda (_) (etaf-demo-counter))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "计数器"
                            'action (lambda (_) (etaf-demo-counter))
                            'face '(:foreground "blue" :underline t))
        (insert " - 交互式计数器（支持键盘和鼠标操作）\n\n")
        
        (insert "[")
        (insert-text-button "4"
                            'action (lambda (_) (etaf-demo-user-profile))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "用户资料"
                            'action (lambda (_) (etaf-demo-user-profile))
                            'face '(:foreground "blue" :underline t))
        (insert " - 用户资料卡片展示\n\n")
        
        (insert "[")
        (insert-text-button "5"
                            'action (lambda (_) (etaf-demo-todo-app))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "Todo 应用"
                            'action (lambda (_) (etaf-demo-todo-app))
                            'face '(:foreground "blue" :underline t))
        (insert " - 完整的 Todo 列表应用\n\n")
        
        (insert "[")
        (insert-text-button "6"
                            'action (lambda (_) (etaf-demo-layout-showcase))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "布局展示"
                            'action (lambda (_) (etaf-demo-layout-showcase))
                            'face '(:foreground "blue" :underline t))
        (insert " - 各种布局和样式组合\n\n")
        
        (insert "[")
        (insert-text-button "7"
                            'action (lambda (_) (etaf-demo-table))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "表格展示"
                            'action (lambda (_) (etaf-demo-table))
                            'face '(:foreground "blue" :underline t))
        (insert " - 文本表格演示\n\n")
        
        (insert "[")
        (insert-text-button "8"
                            'action (lambda (_) (etaf-demo-progress-bar))
                            'face '(:foreground "blue" :weight bold))
        (insert "] ")
        (insert-text-button "进度条"
                            'action (lambda (_) (etaf-demo-progress-bar))
                            'face '(:foreground "blue" :underline t))
        (insert " - 交互式进度条\n\n")
        
        (insert (make-string 55 ?-) "\n\n")
        (insert "按 'q' 退出演示\n"))
      
      (setq buffer-read-only t)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "1") #'etaf-demo-simple-card)
      (local-set-key (kbd "2") #'etaf-demo-styled-button)
      (local-set-key (kbd "3") #'etaf-demo-counter)
      (local-set-key (kbd "4") #'etaf-demo-user-profile)
      (local-set-key (kbd "5") #'etaf-demo-todo-app)
      (local-set-key (kbd "6") #'etaf-demo-layout-showcase)
      (local-set-key (kbd "7") #'etaf-demo-table)
      (local-set-key (kbd "8") #'etaf-demo-progress-bar)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(provide 'etaf-interactive-examples)
;;; etaf-interactive-examples.el ends here
