(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-etml-tag)
(require 'etaf-layout-interactive)

(defun etaf-string (etml &optional data ecss width height)
  "将 ETML 转换为带有样式的字符串。
WIDTH 和 HEIGHT 是可选的视口尺寸。
当为 nil 时，表示不限制根容器的该维度，使用内容的自然尺寸。"
  (let* ((dom (etaf-etml-to-dom etml data))
         (stylesheet (if ecss
                         (apply #'etaf-ecss-stylesheet ecss)
                       ""))
         (cssom (etaf-css-build-cssom dom))
         (cssom (etaf-css-add-stylesheet cssom stylesheet))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

;;;###autoload
(defun etaf-render-to-buffer (buffer-or-name
                              etml &optional data ecss width height)
  (declare (indent defun))
  (let ((buffer (get-buffer-create buffer-or-name)))
    (with-current-buffer buffer
      ;; 初始化缓存
      (etaf-layout-caches-init)
      ;; 渲染内容
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (etaf-string etml data ecss width height)))
      ;; 显示 buffer
      (pop-to-buffer buffer)
      (read-only-mode 1))
    buffer))

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
