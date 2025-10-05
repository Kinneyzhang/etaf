(require 'etml-utils)
(require 'etml-box)

(defclass etml-scroll-bar ()
  ((track-height
    :initarg :track-height :initform 1
    :documentation "滚动条轨道高度，根据 content-linum，content-height 计算得到")
   (track-color
    :initarg :track-color :initform (face-attribute 'default :background)
    :documentation "滚动条轨道的颜色，仅作用于 padding 部分")
   (track-padding-left-pixel
    :initarg :track-padding-left-pixel :initform 0
    :documentation "滚动条轨道的左 padding 像素")
   (track-padding-right-pixel
    :initarg :track-padding-right-pixel :initform 0
    :documentation "滚动条轨道的右 padding 像素")
   (track-margin-left-pixel
    :initarg :track-margin-left-pixel :initform 0
    :documentation "滚动条轨道的左 margin 像素")
   (track-margin-right-pixel
    :initarg :track-margin-right-pixel :initform 0
    :documentation "滚动条轨道的右 margin 像素")
   (track-border-left-pixel
    :initarg :track-border-left-pixel :initform 0
    :documentation "滚动条轨道左边的边框像素宽度")
   (track-border-right-pixel
    :initarg :track-border-right-pixel :initform 0
    :documentation "滚动条轨道右边的边框像素宽度")
   (track-border-left-color
    :initarg :track-border-left-color :initform nil
    :documentation "滚动条轨道左边的边框颜色")
   (track-border-right-color
    :initarg :track-border-right-color :initform nil
    :documentation "滚动条轨道右边的边框颜色")
   (thumb-offset
    :initarg :thumb-offset :initform 0 :documentation "滚动条初始偏移量")
   (thumb-height
    :initarg :thumb-height :initform 1
    :documentation "滚动条高度，根据 content-linum，content-height 计算得到")
   (thumb-pixel :initarg :thumb-pixel :initform 2
                :documentation "滚动条滑块像素宽度")
   (thumb-border-p :initarg :thumb-border-p :initform nil
                   :documentation "滚动条滑块是否有边框")
   (thumb-border-color :initarg :thumb-border-color :initform nil
                       :documentation "滚动条滑块的边框，nil 或颜色")
   (thumb-color :initarg :thumb-color :initform nil
                :documentation "滚动条滑块颜色"))
  "滚动条模型")

(defun etml-scroll-bar-pixel (scroll-bar &optional border-box-p)
  "滚动栏所占的总像素宽度, border-box-p为t时表示不包含 margin"
  (let ((border-box-pixel
         (+ (oref scroll-bar track-padding-left-pixel)
            (oref scroll-bar track-padding-right-pixel)
            (oref scroll-bar track-border-left-pixel)
            (oref scroll-bar track-border-right-pixel)
            (oref scroll-bar thumb-pixel)
            (if (oref scroll-bar thumb-border-p) 2 0))))
    (if border-box-p
        border-box-pixel
      (+ border-box-pixel
         (oref scroll-bar track-margin-left-pixel)
         (oref scroll-bar track-margin-right-pixel)))))

;; 为了滚动需要给滚动条设置一些 text properties 属性
(defun etml-scroll-bar-render (scroll-bar box-uuid scroll-steps)
  ;; box-uuid 用来表示滚动条与哪个 box 关联
  ;; 设置在 thumb 的开头和结尾的中
  (let* ((track-height (oref scroll-bar track-height))
         (thumb-offset (oref scroll-bar thumb-offset))
         (thumb-height (oref scroll-bar thumb-height))
         (below-height (- track-height (+ thumb-offset thumb-height))))
    ;; track-height should >= thumb-height + thumb-offset
    (when (< below-height 0)
      (error "track-height should not less than \
(+ thumb-offset thumb-height)!"))
    (let* ((thumb-pixel (oref scroll-bar thumb-pixel))
           (thumb-border-p (oref scroll-bar thumb-border-p))
           (thumb-border-color (oref scroll-bar thumb-border-color))
           (thumb-str (etml-box-string
                       :overflow-y 'visible
                       :content (etml-pixel-border
                                 thumb-pixel thumb-height
                                 (oref scroll-bar thumb-color))
                       :border-top-p thumb-border-p
                       :border-top-color thumb-border-color
                       :border-bottom-p thumb-border-p
                       :border-bottom-color thumb-border-color
                       :border-left-pixel (or (and thumb-border-p 1) 0)
                       :border-left-color thumb-border-color
                       :border-right-pixel (or (and thumb-border-p 1) 0)
                       :border-right-color thumb-border-color))
           ;; 给滚动条首行设置 etml-v-scroll-bar-start: box-uuid
           ;; 给滚动条尾行设置 etml-v-scroll-bar-end: box-uuid
           (thumb-str (with-temp-buffer
                        (insert thumb-str)
                        (goto-char (point-min))
                        (add-text-properties
                         (line-beginning-position) (line-end-position)
                         `( etml-v-scroll-bar-start ,box-uuid
                            etml-v-scroll-steps ,scroll-steps))
                        (goto-char (point-max))
                        (add-text-properties
                         (line-beginning-position) (line-end-position)
                         `(etml-v-scroll-bar-end ,box-uuid))
                        (buffer-string)))
           ;; 一行轨道颜色的滚动条，用于生成无滚动条的轨道部分
           (track-color
            (or (oref scroll-bar track-color)
                (face-attribute 'default :background)))
           (dummy-single-thumb-str
            (etml-box-string
             :overflow-y 'visible
             :content (etml-pixel-border thumb-pixel 1 track-color)
             ;; :bgcolor track-color
             :border-left-pixel (or (and thumb-border-p 1) 0)
             :border-left-color track-color
             :border-right-pixel (or (and thumb-border-p 1) 0)
             :border-right-color track-color))
           (above-dummy-thumb-str
            (when (> thumb-offset 0)
              (etml-string-duplines dummy-single-thumb-str
                                    thumb-offset)))
           (below-dummy-thumb-str
            (when (> below-height 0)
              (etml-string-duplines dummy-single-thumb-str
                                    below-height)))
           (thumb-str (etml-lines-stack
                       (list above-dummy-thumb-str
                             thumb-str below-dummy-thumb-str)))
           (track-padding-left-pixel
            (oref scroll-bar track-padding-left-pixel))
           (track-padding-right-pixel
            (oref scroll-bar track-padding-right-pixel))
           (track-margin-left-pixel
            (oref scroll-bar track-margin-left-pixel))
           (track-margin-right-pixel
            (oref scroll-bar track-margin-right-pixel))
           (track-border-left-pixel
            (oref scroll-bar track-border-left-pixel))
           (track-border-right-pixel
            (oref scroll-bar track-border-right-pixel))
           (track-border-left-color
            (oref scroll-bar track-border-left-color))
           (track-border-right-color
            (oref scroll-bar track-border-right-color))
           (thumb-curr-str nil)
           (thumb-above-str nil)
           (thumb-below-str nil))
      (etml-box-string
       :overflow-y 'visible
       :content thumb-str
       :bgcolor (or track-color
                    (face-attribute 'default :foreground))
       :padding-left-pixel track-padding-left-pixel
       :padding-right-pixel track-padding-right-pixel
       :border-left-pixel track-border-left-pixel
       :border-right-pixel track-border-right-pixel
       :border-left-color track-border-left-color
       :border-right-color track-border-right-color
       :margin-left-pixel track-margin-left-pixel
       :margin-right-pixel track-margin-right-pixel))))

;; (defun etml-scroll-bar-y-render (box)
;;   (when-let ((y-overflow-num (etml-box-y-scroll-bar-p box))) ; 溢出的行数
;;     (let* (;; 偏移量不能大于溢出的行数
;;            (final-y-scroll-offset (min y-scroll-offset y-overflow-num))
;;            (start final-y-scroll-offset)
;;            ;; 偏移量 <= 溢出的行数
;;            (end (if (<= start y-overflow-num)
;;                     (+ start box-content-height)
;;                   ori-content-height))
;;            (scroll-bar-direction
;;             (oref block scroll-bar-direction)))
;;       ;; 修正超出范围的 offset
;;       (setq y-scroll-offset final-y-scroll-offset)
;;       (cond
;;        ((eq scroll-bar-direction 'right)
;;         (setq border-right-type 'scroll))
;;        ((eq scroll-bar-direction 'left)
;;         (setq border-left-type 'scroll))
;;        (t (error "Invalid format of scroll-bar-direction:%S"
;;                  scroll-bar-direction)))
;;       (if (> box-content-height y-overflow-num)
;;           (progn
;;             ;; 内容展示高度 > 溢出高度时，计算滚动条高度
;;             ;; 确保:一次滚动对应滚动条的一次移动
;;             (setq y-scroll-bar-height
;;                   (- box-content-height
;;                      y-overflow-num))
;;             (setq y-scroll-bar-steps
;;                   ;; 可以滚动的高度 = 展示内容高度 - 1
;;                   (make-list
;;                    (1- box-content-height) 1)))
;;         ;; 溢出高度 > 内容展示高度时，滚动条高度始终为1
;;         ;; 需要确定几次滚动对应滚动条的一次移动
;;         (setq y-scroll-bar-height 1)
;;         (setq y-scroll-bar-steps
;;               ;; FIXME: 当高度为1时，滚动条无法移动，但是需要滚动
;;               (when (> box-content-height 1)
;;                 (let ((each (/ y-overflow-num
;;                                (1- box-content-height)))
;;                       (rest
;;                        (% y-overflow-num
;;                           (1- box-content-height))))
;;                   (seq-map-indexed
;;                    (lambda (elem idx)
;;                      (if (< idx rest)
;;                          (1+ elem)
;;                        elem))
;;                    (make-list
;;                     ;; 可以滚动的高度 = 展示内容高度 - 1
;;                     (1- box-content-height) each))))))
;;       ;; 给每行文本缓存 block 相关的边界信息和文本内容，用于滚动
;;       ;; 边界信息用于在滚动时设置首行和尾行的边框:
;;       ;; 判断：无 top/bottom padding
;;       ;; 且有 top/bottom border
;;       (puthash
;;        uuid
;;        (list
;;         :lines shown-content-lines
;;         :bgcolor (etml-box-bgcolor block)
;;         :shown-lines-height box-content-height
;;         :top-border-pixel top-border-pixel
;;         :bottom-border-pixel bottom-border-pixel
;;         :top-padding top-padding
;;         :bottom-padding bottom-padding
;;         :top-border-color top-border-color
;;         :bottom-border-color bottom-border-color)
;;        etml-box-caches)
;;       (setq shown-content-lines
;;             (seq-subseq shown-content-lines start end))
;;       (setq shown-content-lines
;;             (seq-map-indexed
;;              (lambda (line idx)
;;                (propertize
;;                 line
;;                 'etml-box-line uuid
;;                 ;; etml-box-line-idx 是当前行相对于展示
;;                 ;; 的文本的偏移量。
;;                 ;; 用于判断是否为当前展示的首行或尾行
;;                 'etml-box-line-idx
;;                 (format "%s:%s" uuid idx)
;;                 ;; etml-box-y-offset 是当前行相对于
;;                 ;; 所有行文本的偏移量
;;                 'etml-box-y-offset y-scroll-offset
;;                 'etml-box-y-height box-content-height
;;                 'keymap (etml-box-scroll-map)))
;;              shown-content-lines)))

;;     )
;;   )

(provide 'etml-scroll-bar)
