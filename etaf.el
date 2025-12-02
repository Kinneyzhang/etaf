(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-etml-tag)
(require 'etaf-layout-interactive)
(require 'etaf-utils)

;;; Buffer-local 变量用于存储渲染参数，支持主题切换时自动重新渲染

(defvar-local etaf--render-params nil
  "Buffer-local 变量，存储当前 buffer 的渲染参数。
格式为 plist: (:etml ETML :data DATA :ecss ECSS :width WIDTH :height HEIGHT)
用于在主题切换时自动重新渲染。")

(defvar etaf--managed-buffers nil
  "存储所有 ETAF 管理的 buffer 列表，用于主题切换时重新渲染。")

(defun etaf-string (etml &optional data ecss width height)
  "将 ETML 转换为带有样式的字符串。
WIDTH 和 HEIGHT 是可选的视口尺寸。
当为 nil 时，表示不限制根容器的该维度，使用内容的自然尺寸。"
  (let* ((dom (etaf-etml-to-dom etml data))
         (stylesheet (if ecss
                         (apply #'etaf-ecss ecss)
                       ""))
         (cssom (etaf-css-build-cssom dom))
         (cssom (etaf-css-add-stylesheet cssom stylesheet))
         ;; 使用 ECSS 模块构建渲染树（ECSS 模块调用 CSS 模块）
         (render-tree (etaf-ecss-build-render-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

;;;###autoload
(defun etaf-render-to-buffer (buffer-or-name
                              etml &optional data ecss width height)
  "将 ETML 渲染到指定的 BUFFER-OR-NAME 中。
支持主题切换时自动重新渲染（需要先启用 `etaf-enable-theme-auto-refresh`）。"
  (declare (indent defun))
  (let ((buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      ;; 保存渲染参数，用于主题切换时重新渲染
      (setq etaf--render-params
            (list :etml etml :data data :ecss ecss
                  :width width :height height))
      ;; 将 buffer 添加到管理列表
      (unless (memq buffer etaf--managed-buffers)
        (push buffer etaf--managed-buffers))
      ;; 初始化缓存
      (etaf-layout-caches-init)
      ;; 渲染内容
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (etaf-string etml data ecss width height)))
      ;; 显示 buffer
      (pop-to-buffer buffer)
      (local-set-key "q" 'etaf-window-quit)
      (read-only-mode 1))
    buffer))

(defun etaf-window-quit ()
  (interactive)
  (local-unset-key "q")
  (quit-window))

(defun etaf-rerender-buffer (&optional buffer)
  "重新渲染 BUFFER（默认为当前 buffer）。
使用保存的渲染参数重新渲染，用于主题切换后刷新样式。"
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when etaf--render-params
          (let ((etml (plist-get etaf--render-params :etml))
                (data (plist-get etaf--render-params :data))
                (ecss (plist-get etaf--render-params :ecss))
                (width (plist-get etaf--render-params :width))
                (height (plist-get etaf--render-params :height))
                (inhibit-read-only t))
            ;; 保存当前位置
            (let ((pos (point)))
              ;; 重新渲染
              (etaf-layout-caches-init)
              (erase-buffer)
              (insert (etaf-string etml data ecss width height))
              ;; 恢复位置
              (goto-char (min pos (point-max))))))))))

(defun etaf-refresh-styles (&optional buffer)
  "增量更新 BUFFER 中的样式（默认为当前 buffer）。
只更新带有 `etaf-dual-style` 属性的文本区域的 face，不重新渲染整个 buffer。
用于主题切换后快速刷新样式。"
  (interactive)
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (is-dark (etaf-theme-dark-p))
              (pos (point-min)))
          ;; 遍历 buffer 中所有带有 etaf-dual-style 属性的区域
          (while (< pos (point-max))
            (let* ((dual-style (get-text-property pos 'etaf-dual-style))
                   (next-change
                    (or (next-single-property-change pos 'etaf-dual-style)
                        (point-max))))
              (when dual-style
                ;; 根据当前主题选择对应的 face
                (let ((new-face (if is-dark
                                    (plist-get dual-style :dark)
                                  (plist-get dual-style :light))))
                  (when new-face
                    ;; 更新 face 属性
                    (put-text-property pos next-change 'face new-face))))
              (setq pos next-change))))))))

(defun etaf--refresh-all-buffer-styles ()
  "增量更新所有 ETAF 管理的 buffer 中的样式。
用于主题切换时快速刷新所有 ETAF buffer 的样式，不重新渲染。"
  ;; 清理已关闭的 buffer
  (setq etaf--managed-buffers
        (cl-remove-if-not #'buffer-live-p etaf--managed-buffers))
  ;; 增量更新所有 buffer 的样式
  (dolist (buf etaf--managed-buffers)
    (etaf-refresh-styles buf)))

(defun etaf--rerender-all-buffers ()
  "重新渲染所有 ETAF 管理的 buffer。
用于主题切换时自动刷新所有 ETAF buffer 的样式。"
  ;; 清理已关闭的 buffer
  (setq etaf--managed-buffers
        (cl-remove-if-not #'buffer-live-p etaf--managed-buffers))
  ;; 重新渲染所有 buffer
  (dolist (buf etaf--managed-buffers)
    (etaf-rerender-buffer buf)))

(defun etaf-enable-theme-auto-refresh ()
  "启用主题切换时自动刷新 ETAF buffer 样式。
使用增量更新，只更新有双模式样式的区域，不重新渲染整个 buffer。
需要先添加 advice: (advice-add #'load-theme :around #'etaf-change-theme-background)"
  (add-hook 'etaf-theme-background-change-hook
            #'etaf--refresh-all-buffer-styles))

(defun etaf-enable-theme-auto-rerender ()
  "启用主题切换时完全重新渲染 ETAF buffer。
如果增量更新不够用（如布局也依赖主题），可以使用此函数。
需要先添加 advice: (advice-add #'load-theme :around #'etaf-change-theme-background)"
  (add-hook 'etaf-theme-background-change-hook
            #'etaf--rerender-all-buffers))

(defun etaf-disable-theme-auto-refresh ()
  "禁用主题切换时自动刷新/重新渲染。"
  (remove-hook 'etaf-theme-background-change-hook
               #'etaf--refresh-all-buffer-styles)
  (remove-hook 'etaf-theme-background-change-hook
               #'etaf--rerender-all-buffers))

;; (defun etaf-string-with-data ())

;; ;;;###autoload
;; (defun etaf-render-to-buffer-with-data (buffer-or-name etml data &optional width height)
;;   "将带数据的 ETML 渲染到指定的 BUFFER-OR-NAME 中。
;; DATA 是模板数据 plist。
;; WIDTH 和 HEIGHT 是可选的视口尺寸。
;; 当为 nil 时，表示不限制根容器的该维度，使用内容的自然尺寸。"
;;   (let ((buffer (get-buffer-create buffer-or-name)))
;;     (with-current-buffer buffer
;;       ;; 初始化缓存
;;       (etaf-layout-caches-init)
;;       ;; 渲染内容
;;       (let ((inhibit-read-only t))
;;         (erase-buffer)
;;         (insert (etaf-string-with-data etml data width height)))
;;       ;; 显示 buffer
;;       (pop-to-buffer buffer))
;;     buffer))

(provide 'etaf)
