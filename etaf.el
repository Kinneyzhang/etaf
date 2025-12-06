;; -*- lexical-binding: t; -*-

(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-vdom)
(require 'etaf-layout-interactive)
(require 'etaf-utils)

(defun etaf-viewport-width (width)
  "Parse WIDTH of viewport to pixel"
  (cond
   ((numberp width) (* (frame-char-width) width))
   ((stringp width)
    (cond
     ((string-match "\\([0-9]+\\)px$" width)
      (string-to-number (match-string 1 width))) 
     ((string-match "\\([0-9]+\\)cw$" width)
      (string-to-number
       (* (frame-char-width) (match-string 1 width))))))))

;;;###autoload
(defun etaf-paint-string (etml &optional data ecss width height)
  "将 ETML 转换为带有样式的字符串。

对于静态模板（无动态内容）：
  模板(ETML) → 真实DOM → CSSOM → 渲染树 → 布局树 → 最终文本

对于动态模板（有动态内容）：
  模板(ETML) → 编译器 → 渲染函数 → 虚拟DOM → 渲染器 → 真实DOM 
  → CSSOM → 渲染树 → 布局树 → 最终文本

ETML - 模板 S-expression
DATA - 模板数据上下文
ECSS - 额外的CSS样式
WIDTH/HEIGHT - 视口尺寸

Note: The dynamic content check is performed on each call. While memoization
could be added for frequently rendered templates, the check itself is fast
(O(n) tree traversal) and memoization would add cache management complexity."
  (let* (;; Optimization: Check if template has dynamic content
         (has-dynamic (etaf-etml-has-dynamic-content-p etml))
         ;; Generate DOM: Use optimized path for static templates
         (dom (if has-dynamic
                  ;; Dynamic path: ETML → Compiler → Render Function → VNode → DOM
                  (let* ((render-fn (etaf-compile etml))
                         (vnode (funcall render-fn data)))
                    (etaf-vdom-render vnode))
                ;; Static path: ETML → DOM directly (skip VNode)
                (etaf-etml-to-dom etml data)))
         ;; Step 6: CSSOM - Build CSS Object Model
         (stylesheet (if ecss (apply #'etaf-ecss ecss) ""))
         (cssom (etaf-css-build-cssom dom))
         (cssom (etaf-css-add-stylesheet cssom stylesheet))
         ;; Step 7: 渲染树 - Combine DOM and CSSOM
         (render-tree (etaf-render-build-tree dom cssom))
         ;; Step 8: 布局树 - Calculate layout
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width (etaf-viewport-width width)
                             :height height))))
    ;; Step 9: 最终文本 - Convert layout to string
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

(defun etaf-get-dom (etml &optional data)
  (etaf-etml-to-dom etml data))

;;; FIXME: etaf-css-build-cssom supports to set extra stylesheet
;;; set extra stylesheet as the second parameter.
(defun etaf-get-cssom (etml &optional data)
  (etaf-css-build-cssom (etaf-get-dom etml data)))

(defun etaf-get-render-tree (etml &optional data)
  (etaf-render-build-tree
   (etaf-get-dom etml data)
   (etaf-get-cssom etml data)))

(defun etaf-get-layout-tree (etml &optional data width height)
  (etaf-layout-build-tree
   (etaf-get-render-tree etml data)
   (list :width (etaf-viewport-width width) :height height)))

(provide 'etaf)
