(require 'etaf-tml)
(require 'etaf-template)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)
(require 'etaf-ecss)

(defun etaf-string (etml &optional width height)
  (let* ((dom (etaf-etml-to-dom etml))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree
          (etaf-layout-build-tree
           render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

(provide 'etaf)
