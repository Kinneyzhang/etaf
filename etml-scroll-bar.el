(require 'etml-utils)
;; (require 'etml-box)

(defvar etml-scroll-bar-alist nil
  "存储不同风格滚动栏的定义的 alist，car 是滚动栏的名称，cdr 是滚动栏的kv列表")

(defclass etml-scroll-bar ()
  ((track-height
    :initarg :track-height :initform 1
    :documentation "滚动条轨道高度，根据 content-linum，content-height 计算得到")
   (track-color
    :initarg :track-color :initform (face-attribute 'default :background)
    :documentation "滚动条轨道的颜色，仅作用于 padding 部分")
   (track-margin-left-pixel
    :initarg :track-margin-left-pixel :initform 0
    :documentation "滚动条轨道的左 margin 像素")
   (track-margin-right-pixel
    :initarg :track-margin-right-pixel :initform 0
    :documentation "滚动条轨道的右 margin 像素")
   (track-padding-left-pixel
    :initarg :track-padding-left-pixel :initform 0
    :documentation "滚动条轨道的左 padding 像素")
   (track-padding-right-pixel
    :initarg :track-padding-right-pixel :initform 0
    :documentation "滚动条轨道的右 padding 像素")
   (track-padding-top-height
    :initarg :track-padding-top-height :initform 0
    :documentation "滚动条轨道的上 padding 高度")
   (track-padding-bottom-height
    :initarg :track-padding-bottom-height :initform 0
    :documentation "滚动条轨道的下 padding 高度")
   
   (track-border-left-pixel
    :initarg :track-border-left-pixel :initform 0
    :documentation "滚动条轨道左边的边框像素宽度")
   (track-border-left-color
    :initarg :track-border-left-color :initform nil
    :documentation "滚动条轨道左边的边框颜色")
   (track-border-right-pixel
    :initarg :track-border-right-pixel :initform 0
    :documentation "滚动条轨道右边的边框像素宽度")
   (track-border-right-color
    :initarg :track-border-right-color :initform nil
    :documentation "滚动条轨道右边的边框颜色")

   (track-border-top-p
    :initarg :track-border-top-p :initform nil
    :documentation "滚动条轨是否有上边框")
   (track-border-top-color
    :initarg :track-border-top-color :initform nil
    :documentation "滚动条轨上边框颜色")
   (track-border-bottom-p
    :initarg :track-border-bottom-p :initform nil
    :documentation "滚动条轨是否有下边框")
   (track-border-bottom-color
    :initarg :track-border-bottom-color :initform nil
    :documentation "滚动条轨下边框颜色")
   
   (thumb-offset :initarg :thumb-offset :initform 0
                 :documentation "滚动条初始偏移量")
   (thumb-height
    :initarg :thumb-height :initform 1
    :documentation "滚动条高度，根据 content-linum，content-height 计算得到")
   (thumb-pixel :initarg :thumb-pixel :initform 2
                :documentation "滚动条滑块像素宽度")
   (thumb-border-p :initarg :thumb-border-p :initform nil
                   :documentation "滚动条滑块是否有四个方向边框")
   (thumb-border-color :initarg :thumb-border-color
                       :initform (face-attribute 'default :foreground)
                       :documentation "滚动条滑块的边框，nil 或颜色")
   (thumb-color :initarg :thumb-color
                :initform (face-attribute 'default :foreground)
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
            ;; 由于 thumb-border 现在使用 :box 属性实现，不占用额外空间
            ;; (if (oref scroll-bar thumb-border-p) 2 0)
            )))
    (if border-box-p
        border-box-pixel
      (+ border-box-pixel
         (oref scroll-bar track-margin-left-pixel)
         (oref scroll-bar track-margin-right-pixel)))))

(defun etml-scroll-bar-track-face (scroll-bar)
  "滚动条上下的轨道部分的face"
  (let ((color (oref scroll-bar track-color)))
    `( :background ,color
       ,@(when (oref scroll-bar thumb-border-p)
           `(:box (:line-width (1 . 0) :color ,color))))))

(defun etml-scroll-bar-thumb-face (scroll-bar idx)
  "滚动条的face，type 的值可以是: all(包含上下边框), start(包含上边框),
 middle(没有上下边框), end(包含下边框)。idx是相对于滚动条首行的偏移量。"
  (let ((border-color (oref scroll-bar thumb-border-color))
        (thumb-height (oref scroll-bar thumb-height)))
    `( :background ,(oref scroll-bar thumb-color)
       ,@(when (oref scroll-bar thumb-border-p)
           (if (= 1 thumb-height)
               ;; 滚动条高度为1时有边框，直接使用 :box 属性
               `(:box (:color ,border-color))
             ;; 其余情况，分别使用 :overline 和 :underline 表示上下边框
             ;; 0 表示首行, thumb-height 表示尾行，中间的表示中间行
             `( :box (:line-width (1 . 0) :color ,border-color)
                ,@(cond
                   ((= idx 0) `(:overline ,border-color))
                   ((= idx (1- thumb-height))
                    `(:underline ( :position t
                                   :color ,border-color)))
                   ((< 0 idx (1- thumb-height)) nil)
                   (t (error "idx %s should between 0 and thumb-height"
                             idx)))))))))

(defun etml-scroll-bar-render (scroll-bar box-uuid scroll-steps)
  "渲染 SCROLL-BAR 滚动栏为字符串。BOX-UUID 用来表示滚动条与哪个 box 关联,
SCROLL-STEPS 用来表示内容滚动一次，滚动条如何移动。"
  (let* ((track-height (oref scroll-bar track-height))
         (thumb-offset (oref scroll-bar thumb-offset))
         (thumb-height (oref scroll-bar thumb-height))
         (below-height (- track-height (+ thumb-offset thumb-height))))
    ;; track-height should >= thumb-height + thumb-offset
    (when (< below-height 0)
      (error "track-height should >= thumb-height + thumb-offset"))
    ;; 滚动栏渲染的步骤：
    ;; 1. 渲染全高的滚动条：track-height, track-color, thumb-pixel
    ;;    如果 thumb-border-p 为t，还需要 track-color 颜色的左右 border
    ;; 2. 渲染实际的滚动条：将 (thumb-offset, thumb-offset+thumb-height)
    ;;    部分的滚动条的颜色标记为 thumb-color，并根据 thumb-border-p,
    ;;    thumb-border-color 设置边框。后续滚动条实时滚动，可以通过改变滚动条
    ;;    首尾行的颜色、边框和 uuid 属性来实现
    ;; 3. 给全高滚动条加上轨道的 padding, border, margin
    (let* ((track-color (oref scroll-bar track-color))
           (thumb-pixel (oref scroll-bar thumb-pixel))
           (thumb-color (oref scroll-bar thumb-color))
           (thumb-border-color (oref scroll-bar thumb-border-color))
           (basic-track-str
            (propertize
             (etml-pixel-blank thumb-pixel track-height)
             'face (etml-scroll-bar-track-face scroll-bar)))
           ;; 在 track-height 的滚动条中，标记出实际的滚动条并设置属性：
           ;; 1. 给滚动条首行设置 etml-v-scroll-bar-start: box-uuid
           ;; 2. 给滚动条尾行设置 etml-v-scroll-bar-end: box-uuid
           ;; 以上属性用于滚动条的移动 和 其关联的盒子的内容更新
           (track-height-thumb-str
            (with-temp-buffer
              (insert basic-track-str)
              (goto-char (point-min))
              (forward-line thumb-offset)
              ;; 依次设置滚动条的每一行的 face 和必要的 uuid 属性
              (dotimes (idx thumb-height)
                (add-text-properties
                 (line-beginning-position) (line-end-position)
                 `( face ,(etml-scroll-bar-thumb-face scroll-bar idx)
                    ,@(when (= idx 0)
                        `( etml-v-scroll-thumb-head ,box-uuid
                           ;; etml-v-scroll-steps ,scroll-steps
                           ))
                    ,@(when (= idx (1- thumb-height))
                        `(etml-v-scroll-thumb-tail ,box-uuid))))
                (forward-line 1))
              (buffer-string))))
      ;; 使用 etml-v-scroll-thumb-area 属性标记整个滚动条的可活动范围
      (setq track-height-thumb-str
            (propertize track-height-thumb-str
                        'etml-v-scroll-area box-uuid))
      (etml-box-string
       (etml-box
        :content track-height-thumb-str
        :bgcolor track-color 
        :padding-left-pixel (oref scroll-bar track-padding-left-pixel)
        :padding-right-pixel (oref scroll-bar track-padding-right-pixel)
        :padding-top-height (oref scroll-bar track-padding-top-height)
        :padding-bottom-height (oref scroll-bar track-padding-bottom-height)
        :border-left-pixel (oref scroll-bar track-border-left-pixel)
        :border-left-color (oref scroll-bar track-border-left-color)
        :border-right-pixel (oref scroll-bar track-border-right-pixel)
        :border-right-color (oref scroll-bar track-border-right-color)
        :border-top-p (oref scroll-bar track-border-top-p)
        :border-top-color (oref scroll-bar track-border-top-color)
        :border-bottom-p (oref scroll-bar track-border-bottom-p)
        :border-bottom-color (oref scroll-bar track-border-bottom-color)
        :margin-left-pixel (oref scroll-bar track-margin-left-pixel)
        :margin-right-pixel (oref scroll-bar track-margin-right-pixel))))))

(defmacro etml-scroll-bar-define (name &rest kvs)
  "定义不同风格滚动条"
  (declare (indent defun))
  `(etml-alist-set etml-scroll-bar-alist ',name ',kvs))

;; :track-margin-left-pixel 4 :track-margin-right-pixel 2
;; :track-padding-left-pixel 2 :track-padding-right-pixel 2

(etml-scroll-bar-define s1
  :thumb-pixel 2)

(etml-scroll-bar-define s2
  :thumb-pixel 2
  :track-border-left-pixel 1
  :track-border-left-pixel 1)

(alist-get 's2 etml-scroll-bar-alist)

(provide 'etml-scroll-bar)
