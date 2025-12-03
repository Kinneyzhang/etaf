;; -*- lexical-binding: t; -*-

(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-etml-tag)
(require 'etaf-layout-interactive)
(require 'etaf-utils)

;;;###autoload
(defun etaf-paint-string (etml &optional data ecss width height)
  "将 ETML 转换为带有样式的字符串。
WIDTH 和 HEIGHT 是可选的视口尺寸。
当为 nil 时，表示不限制根容器的该维度，使用内容的自然尺寸。"
  (let* ((dom (etaf-etml-to-dom etml data))
         (stylesheet (if ecss
                         (apply #'etaf-ecss ecss)
                       ""))
         (cssom (etaf-css-build-cssom dom))
         (cssom (etaf-css-add-stylesheet cssom stylesheet))
         ;; 使用 render 模块构建渲染树
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

;;;###autoload
(defun etaf-paint-to-buffer (buffer-or-name
                             etml &optional data ecss width height)
  "将 ETML 渲染到指定的 BUFFER-OR-NAME 中。
支持主题切换时自动重新渲染（需要先启用 `etaf-enable-theme-auto-refresh`）。
支持响应式组件的增量更新（使用 etaf-reactive-span）。"
  (declare (indent defun))
  (let ((buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      ;; 初始化缓存和响应式绑定
      (etaf-layout-caches-init)
      (setq-local etaf-reactive-buffer-bindings nil)
      ;; 渲染内容
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (etaf-paint-string etml data ecss width height)))
      ;; 设置响应式监听器
      (when etaf-reactive-buffer-bindings
        (require 'etaf-component)
        (etaf-setup-reactive-watchers buffer))
      ;; 显示 buffer
      (pop-to-buffer buffer)
      (local-set-key "q" 'etaf-window-quit)
      (read-only-mode 1))
    buffer))

(defun etaf-window-quit ()
  (interactive)
  (local-unset-key "q")
  (quit-window))

(provide 'etaf)
