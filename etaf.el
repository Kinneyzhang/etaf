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
(require 'etaf-eorm)
(require 'etaf-perf)
(require 'etaf-log)

(defvar etaf-logger (etaf-logger))

(defun etaf-html-minify (html-string)
  "简单的 HTML 压缩：移除多余空白，保留 pre/code 标签内容。"
  (with-temp-buffer
    (insert html-string)
    (let ((case-fold-search t))  ; 忽略大小写
      ;; 先将 pre/code 内容中的换行标记
      (goto-char (point-min))
      (while (re-search-forward
              "<\\(pre\\|code\\|textarea\\)\\([^>]*\\)>\\([^<]*\\)</\\1>"
              nil t)
        (let ((content (match-string 3)))
          (save-match-data
            (setq content (replace-regexp-in-string "\n" "&#10;" content)))
          (replace-match (concat "<\\1\\2>" content "</\\1>") t)))
      ;; 压缩空白
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n\r]+" nil t)
        (replace-match " "))
      ;; 移除标签间空格
      (goto-char (point-min))
      (while (re-search-forward "> <" nil t)
        (replace-match "><"))
      (string-trim (buffer-string)))))

(defun etaf-html-to-etml (html-string)
  (with-temp-buffer
    (insert (etaf-html-minify html-string))
    (etaf-dom-to-tml
     (libxml-parse-html-region
      (point-min) (point-max)))))

(defun etaf-html-file-to-etml (html-file)
  (etaf-html-to-etml
   (etaf-get-buffer-string html-file t)))

;; Optional performance monitoring
(defvar etaf-perf-available
  (require 'etaf-perf nil t)
  "Whether etaf-perf module is available.")

;; Define stub macro if etaf-perf is not available
(unless etaf-perf-available
  (defmacro etaf-perf-measure (stage-name &rest body)
    "Stub macro when etaf-perf is not available - just execute BODY."
    (declare (indent 1))
    `(progn ,@body)))

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

Note: The dynamic content check is performed on each call via recursive tree
analysis. The check is O(n) where n is the number of nodes in the template tree.
While memoization could be added for frequently rendered templates, the check
itself is fast and memoization would add cache management complexity."
  (let* (;; Stage 1: Check if template has dynamic content
         (has-dynamic (etaf-perf-measure 'check-dynamic-content
                        (etaf-etml-has-dynamic-content-p etml)))
         ;; Stage 2: Generate DOM - Use optimized path for static templates
         (dom (if has-dynamic
                  ;; Dynamic path: ETML → Compiler → Render Function → VNode → DOM
                  (etaf-perf-measure 'etml-compile-and-render
                    (let* ((render-fn (etaf-compile etml))
                           (vnode (funcall render-fn data)))
                      (etaf-vdom-render vnode)))
                ;; Static path: ETML → DOM directly (skip VNode)
                (etaf-perf-measure 'etml-to-dom
                  (etaf-etml-to-dom etml data))))
         ;; Stage 3: Build stylesheet
         (stylesheet (etaf-perf-measure 'build-stylesheet
                       (if ecss (apply #'etaf-ecss ecss) "")))
         ;; Stage 4: CSSOM - Build CSS Object Model
         (cssom (etaf-perf-measure 'build-cssom
                  (etaf-css-build-cssom dom)))
         (cssom (etaf-perf-measure 'add-stylesheet
                  (etaf-css-add-stylesheet cssom stylesheet)))
         ;; Stage 5: 渲染树 - Combine DOM and CSSOM
         (render-tree (etaf-perf-measure 'build-render-tree
                        (etaf-render-build-tree dom cssom)))
         ;; Stage 6: 布局树 - Calculate layout
         (layout-tree (etaf-perf-measure 'build-layout-tree
                        (etaf-layout-build-tree
                         render-tree (list :width (etaf-viewport-width width)
                                           :height height)))))
    ;; Stage 7: 最终文本 - Convert layout to string
    (etaf-perf-measure 'layout-to-string
      (etaf-layout-to-string layout-tree))))

;;;###autoload
(defun etaf-paint-to-buffer (buffer-or-name
                             etml &optional data ecss width height)
  "将 ETML 渲染到指定的 BUFFER-OR-NAME 中。
支持主题切换时自动重新渲染（需要先启用 `etaf-enable-theme-auto-refresh`）。
支持响应式组件的增量更新（使用 etaf-reactive-span）。"
  (declare (indent defun))
  (let ((buffer (get-buffer-create buffer-or-name)))
    (if-let ((win (get-buffer-window buffer)))
        (select-window win)
      (etaf-switch-to-buffer buffer))
    (let* ((window (get-buffer-window buffer t))
           (width
            (or width
                (format "%spx"
                        (etaf-window-content-pixel-width window)))))
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
        (read-only-mode 1)))))

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
