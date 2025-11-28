(require 'etaf-tml)
(require 'etaf-template)
(require 'etaf-css)
(require 'etaf-render)
(require 'etaf-layout)
(require 'etaf-tailwind)

(defun etaf-string (tml width &optional height)
  (let* ((dom (etaf-tml-to-dom tml))
         (cssom (etaf-css-build-cssom dom))
         (render-tree (etaf-render-build-tree dom cssom))
         (layout-tree (etaf-layout-build-tree
                       render-tree (list :width width :height height))))
    (etaf-layout-to-string layout-tree)))

(provide 'etaf)
