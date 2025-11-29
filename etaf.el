(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-tag)

(defvar etaf-clear-external-cache-functions nil
  "清除外部缓存的函数列表。
这些函数会在 `etaf-string' 开始渲染之前被调用，
用于清除可能影响渲染结果的外部缓存（如 ekp 的字符串缓存）。

使用方法：
  (add-hook 'etaf-clear-external-cache-functions 'ekp-clear-cache)")

(defun etaf-clear-ekp-cache ()
  "清除 ekp 库的字符串缓存。
ekp 的缓存使用字符串内容和字体名称作为 key，但不考虑字体大小等文本属性。
这可能导致不同字体大小的字符串返回相同的缓存结果。
清除缓存可以确保每次渲染都重新计算字符串的像素宽度。"
  (when (boundp 'ekp-caches)
    (clrhash ekp-caches)))

(defun etaf-clear-external-caches ()
  "清除所有外部缓存。
默认会清除 ekp 的字符串缓存，也会调用 `etaf-clear-external-cache-functions' 
中注册的所有函数。"
  ;; 清除 ekp 缓存
  (etaf-clear-ekp-cache)
  ;; 调用用户注册的其他缓存清除函数
  (run-hooks 'etaf-clear-external-cache-functions))

(defun etaf-string (etml &optional width height)
  "将 ETML 转换为带有样式的字符串。
WIDTH 和 HEIGHT 是可选的视口尺寸。

在渲染之前会自动清除外部缓存（如 ekp 的字符串缓存），
以确保每次渲染都能正确应用样式（包括不同的字体大小）。"
  ;; 清除外部缓存，避免缓存导致的样式问题
  (etaf-clear-external-caches)
  (let* ((dom (etaf-etml-to-dom etml))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

(provide 'etaf)
