(require 'etaf-etml)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)
(require 'etaf-tag)

(defvar etaf-auto-clear-cache t
  "是否在渲染前自动清除外部缓存。
设为 t 时，每次调用 `etaf-string' 都会清除 ekp 等外部库的缓存，
确保不同字体大小的元素能正确渲染。
设为 nil 时，不会自动清除缓存，可以提高性能，但可能导致字体大小不正确。
如果遇到字体大小问题，可以手动调用 `etaf-clear-external-caches'。")

(defvar etaf-clear-external-cache-functions nil
  "清除外部缓存的函数列表。
这些函数会在 `etaf-clear-external-caches' 被调用时执行，
用于清除可能影响渲染结果的外部缓存。

使用方法：
  (add-hook 'etaf-clear-external-cache-functions 'my-clear-cache-func)")

(defun etaf-clear-ekp-cache ()
  "清除 ekp 库的字符串缓存。
ekp 的缓存使用字符串内容和字体名称作为 key，但不考虑字体大小等文本属性。
这可能导致不同字体大小的字符串返回相同的缓存结果。
清除缓存可以确保每次渲染都重新计算字符串的像素宽度。"
  (when (and (boundp 'ekp-caches)
             (hash-table-p ekp-caches))
    (clrhash ekp-caches)))

(defun etaf-clear-external-caches ()
  "清除所有外部缓存。
默认会清除 ekp 的字符串缓存，也会调用 `etaf-clear-external-cache-functions' 
中注册的所有函数。

可以手动调用此函数来清除缓存，也可以设置 `etaf-auto-clear-cache' 为 t 
让 `etaf-string' 自动调用此函数。"
  ;; 清除 ekp 缓存
  (etaf-clear-ekp-cache)
  ;; 调用用户注册的其他缓存清除函数
  (run-hooks 'etaf-clear-external-cache-functions))

(defun etaf-string (etml &optional width height)
  "将 ETML 转换为带有样式的字符串。
WIDTH 和 HEIGHT 是可选的视口尺寸。

当 `etaf-auto-clear-cache' 为 t（默认）时，会在渲染之前自动清除外部缓存
（如 ekp 的字符串缓存），以确保每次渲染都能正确应用样式（包括不同的字体大小）。
如果需要更高性能，可以将 `etaf-auto-clear-cache' 设为 nil，
但需要在字体样式改变时手动调用 `etaf-clear-external-caches'。"
  ;; 根据设置决定是否清除外部缓存
  (when etaf-auto-clear-cache
    (etaf-clear-external-caches))
  (let* ((dom (etaf-etml-to-dom etml))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

(provide 'etaf)
