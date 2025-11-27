;; -*- lexical-binding: t; -*-
;; 最底层的基础盒模型

(require 'elog)
(require 'org-id)
(require 'etaf-utils)
(require 'etaf-scroll-bar)

(defvar etaf-box-logger (elog-logger :buffer "*etaf-box-log*"))

(defun etaf-box-log-view ()
  (interactive)
  (elog-log-view etaf-box-logger))

(defclass etaf-text-css ()
  ((color :initarg :color :initform nil :documentation "文本颜色")
   (bgcolor :initarg :bgcolor :initform nil :documentation "文本背景颜色")
   (font-family :initarg :font-family :initform nil :documentation "字体系列")
   (font-size :initarg :font-size :initform nil :documentation "字体大小")
   (font-weight :initarg :font-weight :initform nil :documentation "字体粗细")
   (text-underline :initarg :text-underline :initform nil :documentation "字体下划线")
   (text-overline :initarg :text-overline :initform nil :documentation "字体上划线")
   (text-strike :initarg :text-strike :initform nil :documentation "字体中划线")
   (text-align :initarg :text-align :initform 'left :documentation "文本水平对齐")
   (vertical-align :initarg :vertical-align :initform 'top :documentation "文本垂直对齐")
   (text-transform :initarg :text-transform :initform nil :documentation "文本转换")
   ;; (overflow-wrap :initarg :overflow-wrap :initform 'kp :documentation "文本换行方式")
   ;; (letter-spacing :initarg :letter-spacing :initform 'normal :documentation "字母间距")
   ;; (word-spacing :initarg :word-spacing :initform 'normal :documentation "单词间距")
   ;; (white-space :initarg :white-space :initform 'normal :documentation "空白处理")
   )
  "文本属性模型")

(defclass etaf-box (etaf-text-css)
  ((uuid :initarg :uuid :initform (org-id-uuid) :documentation "uuid")
   (content :initarg :content :initform " " :documentation "元素的内容区域")
   (display :initarg :display :initform 'inline :documentation "显示类型")
   (box-sizing :initarg :box-sizing :initform 'content-box :documentation "框盒计算方式")
   
   (width :initarg :width :initform nil :documentation "内容区域的宽度")
   (height :initarg :height :initform nil :documentation "内容区域的高度")
   (min-width :initarg :min-width :initform 0 :documentation "最小宽度限制")
   (min-height :initarg :min-height :initform 0 :documentation "最小高度限制")
   (max-width :initarg :max-width :initform nil :documentation "最大宽度限制")
   (max-height :initarg :max-height :initform nil :documentation "最大高度限制")

   (padding-left-pixel :initarg :padding-left-pixel :initform 0 :documentation "左内边距")
   (padding-right-pixel :initarg :padding-right-pixel :initform 0 :documentation "右内边距")
   (padding-top-height :initarg :padding-top-height :initform 0 :documentation "上内边距")
   (padding-bottom-height :initarg :padding-bottom-height :initform 0 :documentation "下内边距")

   (margin-left-pixel :initarg :margin-left-pixel :initform 0 :documentation "左外边距")
   (margin-right-pixel :initarg :margin-right-pixel :initform 0 :documentation "右外边距")
   (margin-top-height :initarg :margin-top-height :initform 0 :documentation "上外边距")
   (margin-bottom-height :initarg :margin-bottom-height :initform 0 :documentation "下外边距")

   (border-left-pixel :initarg :border-left-pixel :initform 0 :documentation "左边框宽度")
   (border-left-color :initarg :border-left-color
                      :initform (face-attribute 'default :foreground)
                      :documentation "左边框颜色")
   (border-right-pixel :initarg :border-right-pixel :initform 0 :documentation "右边框宽度")
   (border-right-color :initarg :border-right-color
                       :initform (face-attribute 'default :foreground)
                       :documentation "右边框颜色")
   
   ;; 使用 face :overline 实现上边框
   (border-top-p :initarg :border-top-p :initform nil :documentation "是否设置上边框")
   (border-top-color :initarg :border-top-color
                     :initform (face-attribute 'default :foreground)
                     :documentation "上边框颜色")
   ;; 使用 face :underline 实现下边框
   (border-bottom-p :initarg :border-bottom-p :initform nil :documentation "是否设置下边框")
   (border-bottom-color :initarg :border-bottom-color
                        :initform (face-attribute 'default :foreground)
                        :documentation "下边框颜色")
   (border-bottom-style :initarg :border-bottom-style :initform nil :documentation "下边框样式")

   ;; 滚动栏的通用设置
   (scroll-thumb-color :initarg :scroll-thumb-color
                       :initform (face-attribute 'default :foreground)
                       :documentation "滚动条颜色")
   (scroll-track-color :initarg :scroll-track-color
                       :initform (face-attribute 'default :background)
                       :documentation "滚动轨道颜色")
   
   (overflow-y :initarg :overflow-y :initform 'auto
               :documentation "垂直方向溢出处理，支持如下的值:
1.visible: 溢出内容正常显示。
2.hidden: 溢出内容会被隐藏。
3.auto: 允许滚动查看溢出内容。溢出时会显示滚动条，不溢出不显示滚动条(无滚动条空间)。
4.scroll-visible: 允许滚动查看溢出内容。无论是否溢出，始终显示滚动条。
5.scroll-hidden: 允许滚动查看溢出内容。无论是否溢出，始终不显示滚动条。
6.scroll-auto: 允许滚动查看溢出内容。无论是否溢出，始终为预留固定的滚动条空间，但是只有溢出时才显示滚动条，不溢出时用空白填充滚动条空间。")

   (v-scroll-bar-type :initarg :v-scroll-bar-type :initform nil)
   (v-scroll-offset :initarg :v-scroll-offset :initform 0
                    :documentation "文本已经滚动的偏移量，thumb-offset是滚动条的偏移量，不要混淆")
   (v-scroll-bar :initarg :v-scroll-bar :initform (etaf-scroll-bar)
                 ;; :type etaf-scroll-bar
                 :documentation "垂直方向的滚动条")
   (v-scroll-bar-direction :initarg :v-scroll-bar-direction :initform 'right
                           :documentation "垂直方向的滚动条的位置 'left or 'right")
   
   (overflow-x :initarg :overflow-x :initform 'visible :documentation "水平方向溢出处理")
   (h-scroll-bar-color :initarg :h-scroll-bar-color :initform nil :documentation "滚动条颜色")
   (h-scroll-bar-direction :initarg :h-scroll-bar-direction :initform right :documentation "滚动条方向")
   (h-scroll-bar-style :initarg :h-scroll-bar-style :initform nil :documentation "滚动条样式风格")
   
   (visibility :initarg :visibility :initform 'visible :documentation "可见性")
   (z-index :initarg :z-index :initform 'auto :documentation "堆叠顺序")
   (position :initarg :position :initform 'static :documentation "定位方式")

   ;; (float :initarg :float :initform nil :documentation "浮动方式")
   ;; (clear :initarg :clear :initform nil :documentation "清除浮动")
   ;; (box-decoration-break :initarg :box-decoration-break :initform 'slice :documentation "框装饰拆分方式")
   
   :allow-nil-initform​​ t)
  "CSS 基础框盒模型类，表示CSS视觉格式化模型中的长方形盒子。")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etaf-box-content-pixel (box)
  "根据 min-width, max-width, width, content-width
计算内容最终需要使用的像素宽度。"
  (let* ((content (oref box content))
         (original-pixel (string-pixel-width content))
         (min-pixel (etaf-width-pixel (oref box min-width) content))
         (max-pixel (etaf-width-pixel (oref box max-width) content))
         (curr-pixel (etaf-width-pixel (oref box width) content)))
    (min (or max-pixel 999999999)
         (max (or min-pixel 0)
              (or curr-pixel original-pixel)))))

(defun etaf-box-original-content (box)
  "原始文本内容"
  (etaf-lines-justify
   (oref box content)
   (etaf-box-content-pixel box)
   (oref box text-align)))

(defun etaf-box-content-linum (box)
  "原始文本的行数"
  (etaf-string-linum (etaf-box-original-content box)))

(defun etaf-box-content-height (box)
  "根据 min-height, max-height, height, content-height 计算
文本设置了宽度之后的高度。"
  (let ((content-linum (etaf-box-content-linum box))
        (min-height (oref box min-height))
        (max-height (oref box max-height))
        (curr-height (oref box height)))
    (min (or max-height 999999999)
         (max (or min-height 0)
              (or curr-height content-linum)))))

(defun etaf-v-scroll-bar (box)
  "返回 etaf-scroll-bar 对象"
  (let ((scroll-bar (or (oref box v-scroll-bar)
                        (etaf-scroll-bar))))
    (when-let* ((type (oref box v-scroll-bar-type))
                (kvs (alist-get type etaf-scroll-bar-alist)))
      (apply #'etaf-oset scroll-bar kvs))
    scroll-bar))

(defun etaf-box-side-pixel (box &optional side)
  "除了内容以外的两侧的像素宽度和。SIDE 表示指定 'left 或 'right
一侧的 border,padding,margin 的总像素宽度。"
  (let* ((left-side-pixel (+ (oref box border-left-pixel)
                             (oref box padding-left-pixel)
                             (oref box margin-left-pixel)))
         (right-side-pixel (+ (oref box border-right-pixel)
                              (oref box padding-right-pixel)
                              (oref box margin-right-pixel)))
         (v-scroll-bar-p (etaf-box-v-scroll-bar-p box))
         (v-scroll-bar-pixel (etaf-scroll-bar-pixel
                              (etaf-v-scroll-bar box)))
         (v-scroll-bar-direction (oref box v-scroll-bar-direction)))
    (pcase side
      ('left (+ left-side-pixel
                (if (and (eq 'left v-scroll-bar-direction)
                         v-scroll-bar-p)
                    v-scroll-bar-pixel
                  0)))
      ('right (+ right-side-pixel
                 (if (and (eq 'right v-scroll-bar-direction)
                          v-scroll-bar-p)
                     v-scroll-bar-pixel
                   0)))
      (_ (+ left-side-pixel right-side-pixel
            (if v-scroll-bar-p
                v-scroll-bar-pixel
              0))))))

(defun etaf-box-total-pixel (box)
  "Return the total pixel width of BLOCK."
  (+ (etaf-box-side-pixel box)
     (etaf-box-content-pixel box)))

(defun etaf-box-side-height (box)
  "内容以外，上下 padding, margin 的总高度"
  (+ (floor (oref box padding-top-height))
     (floor (oref box padding-bottom-height))
     (floor (oref box margin-top-height))
     (floor (oref box margin-bottom-height))))

(defun etaf-box-total-height (box)
  "整个 box 的总高度，包含 content, padding, margin"
  (+ (etaf-box-side-height box)
     (etaf-box-content-height box)))

(defun etaf-box-content (box)
  "设置了宽度和高度之后的文本内容"
  (let* ((lines (split-string
                 (etaf-box-original-content box) "\n" t))
         (start (oref box v-scroll-offset))
         (content-height (etaf-box-content-height box))
         (new-lines (seq-subseq lines start (+ start content-height))))
    (etaf-lines-align
     (string-join new-lines "\n")
     content-height (oref box vertical-align))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; v-scroll-bar

(defun etaf-box-v-scroll-bar-p (box)
  "是否显示垂直方向的滚动条，返回 'real 表示真实的滚动栏，
'blank 空白滚动栏（只预留滚动栏的空间）。"
  (let ((overflow-y (oref box overflow-y))
        (content-linum (etaf-box-content-linum box))
        (content-height (etaf-box-content-height box)))
    ;; 需要滚动 且 支持显示滚动条时，才显示滚动条
    (pcase overflow-y
      ('scroll-visible 'real) ;; 始终显示真实的滚动栏
      ('scroll-auto
       ;; 溢出时显示真实的滚动栏，不溢出显示空白滚动栏
       (if (> content-linum content-height)
           'real
         'blank))
      ('auto
       ;; 溢出时显示真实的滚动栏，不溢出不显示滚动栏
       (when (> content-linum content-height)
         'real))
      (_ nil))))

(defun etaf-box-v-scroll-bar-info (box)
  "计算 v-scroll-bar-thumb-height 和 v-scroll-steps"
  (let ((content-linum (etaf-box-content-linum box))
        (content-height (etaf-box-content-height box))
        ;; 滚动条高度
        v-scroll-bar-thumb-height
        ;; 滚动条每次移动对应的滚动次数列表
        v-scroll-steps)
    ;; content-linum 是原始内容的行数
    ;; content-height 是盒子内容部分展示的高度
    ;; overflow-linum 是溢出的行数
    (let ((overflow-linum (- content-linum content-height)))
      (if (> overflow-linum 0)
          (if (> content-height overflow-linum)
              ;; 盒子内容高度 > 溢出行数
              (progn
                ;; 滚动条长度为 内容高度 - 溢出行数
                (setq v-scroll-bar-thumb-height
                      (- content-height overflow-linum))
                ;; 只需滚动溢出行数次，每次滚动一行
                (setq v-scroll-steps
                      (make-list overflow-linum 1)))
            ;; 内容高度 <= 溢出高度，滚动条高度始终为1
            (setq v-scroll-bar-thumb-height 1)
            ;; overflow-linum 行溢出内容平摊到
            ;; (1- content-height) 行的滚动中
            (setq v-scroll-steps
                  ;; 当高度为1时，滚动条无法移动
                  (when (> content-height 1)
                    (etaf-split-size
                     overflow-linum (1- content-height)
                     nil nil nil t))))
        ;; 无溢出时，滚动条长度会内容高度，step 为 nil
        (setq v-scroll-bar-thumb-height content-height)))
    (cons v-scroll-bar-thumb-height v-scroll-steps)))

(defun etaf-box-v-scroll-bar-thumb-height (box)
  "滚动条高度"
  (car (etaf-box-v-scroll-bar-info box)))

(defun etaf-box-v-scroll-steps (box)
  "滚动条每次移动对应的滚动次数列表"
  (cdr (etaf-box-v-scroll-bar-info box)))

(defun etaf-box-v-scroll-bar-thumb-offset
    (scroll-steps scroll-offset)
  "根据 v-scroll-steps, v-scroll-offset 计算 thumb-offset，用于实时滚动。"
  (let ((prefixs (let ((prefix 0))
                   (mapcar (lambda (n)
                             (setq prefix (+ prefix n)))
                           scroll-steps))))
    (or (etaf--num-in-prefixs scroll-offset prefixs)
        0)))

(defun etaf-box-v-scroll-bar-string (box)
  "渲染垂直方向的滚动条。overflow-y 决定了是否需要需要滚动条和滚动条是否
用空白填充。设置 thumb-height,thumb-offset,track-height后，渲染滚动条。"
  (when-let ((v-scroll-bar-p (etaf-box-v-scroll-bar-p box)))
    (let* ((scroll-thumb-color (oref box scroll-thumb-color))
           (scroll-track-color (oref box scroll-track-color))
           (v-scroll-bar (etaf-v-scroll-bar box))
           (track-height (etaf-box-content-height box))
           (thumb-height (etaf-box-v-scroll-bar-thumb-height box))
           (scroll-steps (etaf-box-v-scroll-steps box))
           (thumb-offset (etaf-box-v-scroll-bar-thumb-offset
                          scroll-steps (oref box v-scroll-offset)))
           (box-uuid (oref box uuid))
           (track-padding-top-height
            (floor (oref box padding-top-height)))
           (track-padding-bottom-height
            (floor (oref box padding-bottom-height))))
      ;; 设置 box 计算之后用于滚动栏渲染必要的属性:
      (oset v-scroll-bar track-color scroll-track-color)
      (oset v-scroll-bar thumb-color scroll-thumb-color)
      (oset v-scroll-bar track-height track-height)
      (oset v-scroll-bar thumb-height thumb-height)
      (oset v-scroll-bar thumb-offset thumb-offset)
      ;; scroll-bar 的 上下 padding 由 box 决定，但只需要整数部分
      ;;   小数部分的 line-height 由 滚动栏所在的 box 整体渲染
      (oset v-scroll-bar track-padding-top-height
            track-padding-top-height)
      (oset v-scroll-bar track-padding-bottom-height
            track-padding-bottom-height)
      (pcase v-scroll-bar-p
        ('real (etaf-scroll-bar-render
                v-scroll-bar box-uuid scroll-steps))
        ('blank
         ;; 空白滚动栏：将滚动条的颜色与轨道颜色保持一致
         (let ((color (oref v-scroll-bar track-color)))
           (oset v-scroll-bar thumb-color color)
           (oset v-scroll-bar thumb-border-color color)
           (etaf-scroll-bar-render
            v-scroll-bar box-uuid scroll-steps)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar etaf-box-caches nil
  "Caches of etaf-box, it should be set as a buffer-local variable.")

(defun etaf-box-caches-init (&optional buffer-or-name)
  "Initialize `etaf-box-caches' in BUFFER-OR-NAME."
  (with-current-buffer (or (and buffer-or-name
                                (get-buffer-create buffer-or-name))
                           (current-buffer))
    (unless etaf-box-caches
      (setq-local
       etaf-box-caches
       (make-hash-table
        :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))))

(defun etaf-box-string (box)
  "返回 box 渲染后的文本"
  (let* (;; get margin
         (margin-top-height (oref box margin-top-height))
         (margin-bottom-height (oref box margin-bottom-height))
         (margin-left-pixel (oref box margin-left-pixel))
         (margin-right-pixel (oref box margin-right-pixel))
         ;; get padding
         ;; padding-top-height 可能为小数
         (padding-top-float (oref box padding-top-height))
         (padding-top-height (floor padding-top-float))
         (top-float (- padding-top-float padding-top-height))
         ;; 将第一个 padding 行设置 line-height 为 (1+ top-float)
         (padding-top-line-height (when (> top-float 0)
                                    (1+ top-float)))
         ;; padding-bottom-height 可能为小数
         (padding-bottom-float (oref box padding-bottom-height))
         (padding-bottom-height (floor padding-bottom-float))
         (bottom-float (- padding-bottom-float padding-bottom-height))
         ;; 将第一个 padding 行设置 line-height 为 (1+ bottom-float)
         (padding-bottom-line-height (when (> bottom-float 0)
                                       (1+ bottom-float)))
         ;; 计算边框以内的高度 inner-height
         (content-height (etaf-box-content-height box))
         (inner-height (+ content-height padding-top-height
                          padding-bottom-height))
         (padding-left-pixel (oref box padding-left-pixel))
         (padding-right-pixel (oref box padding-right-pixel))
         ;; get border
         (border-top-p (oref box border-top-p))
         (border-top-color (oref box border-top-color))
         (border-bottom-p (oref box border-bottom-p))
         (border-bottom-color (oref box border-bottom-color))
         (border-bottom-style (oref box border-bottom-style))
         (border-left-pixel (oref box border-left-pixel))
         (border-left-color (oref box border-left-color))
         (border-right-pixel (oref box border-right-pixel))
         (border-right-color (oref box border-right-color))
         (content-pixel (etaf-box-content-pixel box))
         (uuid (oref box uuid))
         (box-string (etaf-box-content box))
         (original-content-lines
          (split-string (etaf-box-original-content box) "\n" t)))
    ;; 当前buffer 为用于渲染的 buffer时，设置缓存
    (when etaf-render-buffer-p
      (puthash uuid
               (list :content-lines original-content-lines
                     :content-linum (length original-content-lines)
                     :content-height content-height
                     :v-scroll-offset (oref box v-scroll-offset)
                     :v-scroll-steps (etaf-box-v-scroll-steps box)
                     :padding-top-height padding-top-float
                     :padding-bottom-height padding-bottom-float
                     :border-top-p border-top-p
                     :border-top-color border-top-color
                     :border-bottom-p border-bottom-p
                     :border-bottom-color border-bottom-color
                     :border-bottom-style border-bottom-style
                     :v-scroll-bar (etaf-v-scroll-bar box))
               etaf-box-caches))
    ;; 设置 etaf-box-content-line 属性为 uuid
    ;; 设置滚动的 keymap
    (elog-info etaf-box-logger "set uuid to each line of box-string")
    (setq box-string
          (mapconcat (lambda (line)
                       (etaf-propertize
                        line `( etaf-box-content-line ,uuid
                                keymap ,(etaf-box-scroll-map))))
                     (split-string box-string "\n" t) "\n"))
    ;; 构建 box-string 的顺序:
    ;; 1. stack: content + top-padding(content-pixel)
    ;;           + bottom-padding(content-pixel)
    (when (or (> padding-top-height 0) (> padding-bottom-height 0))
      (elog-info etaf-box-logger
        "stack top padding %s and bottom padding %s to box-string"
        padding-top-height padding-bottom-height)
      (setq box-string
            (etaf-lines-stack
             (list (etaf-pixel-blank content-pixel padding-top-height)
                   box-string
                   (etaf-pixel-blank content-pixel padding-bottom-height)))))
    ;; 2. concat: 1 + left-padding(inner-height)
    ;;            + right-padding(inner-height)
    (when (or (> padding-left-pixel 0) (> padding-right-pixel 0))
      (elog-info etaf-box-logger
        "concat left padding %s and right padding %s to box-string"
        padding-left-pixel padding-right-pixel)
      (setq box-string
            (etaf-lines-concat
             (list (etaf-pixel-blank padding-left-pixel inner-height)
                   box-string
                   (etaf-pixel-blank padding-right-pixel inner-height)))))
    ;; 3. 设置垂直方向滚动条
    (when-let ((scroll-bar-str (etaf-box-v-scroll-bar-string box))
               (direction (oref box v-scroll-bar-direction)))
      (elog-info etaf-box-logger
        "render \"%s\" vertical scroll bar" (etaf-box-v-scroll-bar-p box))
      (setq box-string
            (etaf-lines-concat
             (pcase direction
               ('right (list box-string scroll-bar-str))
               ('left (list scroll-bar-str box-string))
               (_ (error "Invalid format of v-scroll-bar-direction: %s"
                         direction))))))
    ;; 4. set top(overline) and bottom(underline) border
    (when (or border-top-p border-bottom-p)
      (let ((lines (split-string box-string "\n" t)))
        (when border-top-p
          (elog-info etaf-box-logger "add top border to box-string")
          (setf (car lines)
                (etaf-propertize-overline (car lines) border-top-color)))
        (when border-bottom-p
          (elog-info etaf-box-logger "add bottom border to box-string")
          (setf (car (last lines))
                (etaf-propertize-underline (car (last lines))
                                           border-bottom-color
                                           border-bottom-style)))
        (setq box-string (string-join lines "\n"))))
    ;; 5. 设置背景色
    (when-let ((bgcolor (oref box bgcolor)))
      (elog-info etaf-box-logger "set bgcolor to box-string")
      (setq box-string
            (etaf-maplines
             (lambda (line)
               (etaf-propertize line `(face (:background ,bgcolor))))
             box-string 'string-join "\n")))
    ;; 6. concat: set left and right border(inner-height)
    ;;            set left and right margin(inner-height)
    (when (or (> margin-left-pixel 0) (> border-left-pixel 0)
              (> margin-right-pixel 0) (> border-right-pixel 0))
      (elog-info etaf-box-logger
        "set left margin:%s, left border:%s, right margin:%s, right border:%s"
        margin-left-pixel border-left-pixel
        margin-right-pixel border-right-pixel)
      (setq box-string
            (etaf-lines-concat
             (list
              (etaf-pixel-blank margin-left-pixel inner-height)
              (etaf-pixel-border border-left-pixel
                                 inner-height border-left-color)
              box-string
              (etaf-pixel-border border-right-pixel
                                 inner-height border-right-color)
              (etaf-pixel-blank margin-right-pixel inner-height)))))
    ;; 7. stack: set top and bottom margin(total pixel)
    ;; FIXEM: 支持浮点数的 margin，参考 padding
    (when (or (> margin-top-height 0) (> margin-bottom-height 0))
      (elog-info etaf-box-logger
        "stack top margin:%s, bottom margin:%s"
        margin-top-height margin-bottom-height)
      (let ((total-pixel (etaf-box-total-pixel box)))
        (setq box-string
              (etaf-lines-stack
               (list (etaf-pixel-blank total-pixel margin-top-height)
                     box-string
                     (etaf-pixel-blank
                      total-pixel margin-bottom-height))))))
    ;; 8. 上下 padding 为浮点数时, padding 的首尾行需要设置 line-height
    (when (or padding-top-line-height padding-bottom-line-height)
      (elog-info etaf-box-logger
        "set top padding line-height:%s, bottom padding line-height:%s"
        padding-top-line-height padding-bottom-line-height)
      (setq box-string
            (with-temp-buffer
              (insert box-string)
              (when padding-top-line-height
                (goto-char (point-min))
                ;; 到达 top padding 的首行
                (forward-line margin-top-height)
                (add-text-properties
                 (line-beginning-position) (1+ (line-end-position))
                 `(line-height ,padding-top-line-height)))
              (when padding-bottom-line-height
                (goto-char (point-max))
                ;; 没有 bottom margin 时，要换行来支持 line-height 设置
                (when (= margin-bottom-height 0)
                  (insert "\n")
                  (forward-line -1))
                ;; 到达 bottom padding 的尾行
                (forward-line (- margin-bottom-height))
                (add-text-properties
                 (line-beginning-position) (1+ (line-end-position))
                 `(line-height
                   (nil ,padding-bottom-line-height))))
              (buffer-string))))
    ;; FIXME: 应用其他字体相关的属性
    box-string))

(defvar etaf-render-buffer-p nil
  "用于表示当前 buffer 是否为 etaf 渲染的 buffer.
应该被设置为 buffer-local。")

;;;###autoload
(defun etaf-box-render (buffer-or-name box)
  "将 box 的内容渲染到 BUFFER-OR-NAME 的 buffer 中"
  (declare (indent 1))
  (let ((buffer (get-buffer-create buffer-or-name t)))
    (with-pop-buffer buffer nil
      (setq-local etaf-render-buffer-p t)
      (etaf-box-caches-init buffer)
      (insert (etaf-box-string box)))))

(defvar etaf-box-scroll-down-keys '("n" [wheel-down]))
(defvar etaf-box-scroll-up-keys '("p" [wheel-up]))

(defun etaf-box-scroll-map ()
  (let ((map (make-sparse-keymap)))
    (dolist (key etaf-box-scroll-down-keys)
      (define-key map key #'etaf-box-scroll-down))
    (dolist (key etaf-box-scroll-up-keys)
      (define-key map key #'etaf-box-scroll-up))
    map))

(defvar etaf-box-scroll-data nil
  "buffer local 的变量用于存储盒子当前的滚动情况")

(defun etaf-box-scroll (up-or-down &optional n)
  "光标位于盒子中时滚动内容"
  (when-let* ((props (text-properties-at (point)))
              (uuid (plist-get props 'etaf-box-content-line))
              (cache (gethash uuid etaf-box-caches))
              (content-lines (plist-get cache :content-lines))
              (content-linum (plist-get cache :content-linum))
              (content-height (plist-get cache :content-height))
              (overflow-linum (- content-linum content-height))
              (v-scroll-offset
               (max 0 (min (plist-get cache :v-scroll-offset)
                           overflow-linum)))
              (v-scroll-steps (plist-get cache :v-scroll-steps))
              (thumb-offset (etaf-box-v-scroll-bar-thumb-offset
                             v-scroll-steps v-scroll-offset))
              (new-v-scroll-offset
               (cond ((eq 'down up-or-down)
                      (min overflow-linum (+ v-scroll-offset (or n 1))))
                     ((eq 'up up-or-down)
                      (max 0 (- v-scroll-offset (or n 1))))))
              (new-thumb-offset (etaf-box-v-scroll-bar-thumb-offset
                                 v-scroll-steps new-v-scroll-offset))
              (box-content-lines
               (seq-subseq content-lines
                           new-v-scroll-offset
                           (+ new-v-scroll-offset content-height))))
    ;; 更新 new-v-scroll-offset 到缓存
    (plist-put cache :v-scroll-offset new-v-scroll-offset)
    ;;; 一次滚动后，更新内容和滚动条位置
    (let* ((padding-top-height (plist-get cache :padding-top-height))
           (padding-bottom-height (plist-get cache :padding-bottom-height))
           (border-top-p (plist-get cache :border-top-p))
           (border-top-color (plist-get cache :border-top-color))
           (border-bottom-p (plist-get cache :border-bottom-p)) 
           (border-bottom-color (plist-get cache :border-bottom-color))
           (border-bottom-style (plist-get cache :border-bottom-style))
           (v-scroll-bar (plist-get cache :v-scroll-bar))
           (track-border-top-p (oref v-scroll-bar track-border-top-p))
           (track-border-bottom-p (oref v-scroll-bar track-border-bottom-p))
           (thumb-height (oref v-scroll-bar thumb-height))
           (inhibit-read-only t))
      ;; 先更新滚动条位置，因为内容更新会改变滚动条的 point
      (save-excursion
        (goto-char (point-min))
        (let ((offset (abs (- new-thumb-offset thumb-offset))))
          (dotimes (_ offset)
            (let* ((cons (cond
                          ((= thumb-height 1)
                           (etaf-box-v-scroll-1-height-thumb-shift
                            up-or-down uuid))
                          ((= thumb-height 2)
                           (etaf-box-v-scroll-2-height-thumb-shift
                            up-or-down uuid)
                           )
                          ((> thumb-height 2)
                           (etaf-box-v-scroll-n-height-thumb-shift
                            up-or-down uuid))))
                   (old-region (car cons))
                   (new-region (cdr cons)))
              
              ;; FIXME: 使用 etaf-add-face-text-property 替换 add-face-text-property
              ;; 解决 face 属性重复的问题
              
              ;; scroll-down, new-thumb-offset=1, border-top-p=t, padding-top<1
              ;; 表示滚动条是从包含上边框的盒子的顶部开始向下滚动的，此时需重新补齐上边框
              (when (and (= new-thumb-offset 1)
                         (eq up-or-down 'down)
                         (or border-top-p track-border-top-p)
                         (< padding-top-height 1))
                (add-face-text-property
                 (car old-region) (cdr old-region)
                 `(:overline ,border-top-color)))
              ;; scroll-up, new-thumb-offset=(1- overflow-linum)
              ;; border-bottom-p=t, padding-bottom<1
              ;; 表示滚动条是从包含下边框的盒子的底部开始向上滚动的，此时需重新补齐下边框
              (when (and (eq up-or-down 'up)
                         (= new-thumb-offset (1- overflow-linum))
                         (or border-bottom-p track-border-bottom-p)
                         (< padding-bottom-height 1))
                (add-face-text-property
                 (car old-region) (cdr old-region)
                 `(:underline (:position t :color ,border-bottom-color))))))))
      
      ;; 后更新内容: 根据 uuid 搜索并将每一行替换为滚动后的新的文本
      (etaf-property-map-regions
       (lambda (start end idx)
         (let ((line (propertize (nth idx box-content-lines)
                                 'etaf-box-content-line uuid
                                 'keymap (etaf-box-scroll-map))))
           ;; 滚动时替换文本会丢失上下边框，必要时要加上
           (cond ((and (= idx 0)
                       (< padding-top-height 1)
                       border-top-p)
                  (setq line (etaf-propertize-overline
                              line border-top-color)))
                 ((and (= idx (1- content-height))
                       (< padding-bottom-height 1)
                       border-bottom-p)
                  (setq line (etaf-propertize-underline
                              line border-top-color
                              border-bottom-style))))
           (etaf-region-replace line start end)))
       'etaf-box-content-line uuid t))))

(defun etaf-box-v-scroll-1-height-thumb-shift (up-or-down uuid)
  "thumb-height = 1 时，滚动条如何移动一次，返回滚动前后的 region"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-head-region (etaf-property-forward-region
                               'etaf-v-scroll-thumb-head uuid t))
           (thumb-head-start (car thumb-head-region))
           (thumb-head-end (cdr thumb-head-region)))
      (cond
       ((eq 'down up-or-down) ;; 向下移动
        (let ((head-next-region
               (progn (goto-char thumb-head-end)
                      (etaf-property-forward-region
                       'etaf-v-scroll-area uuid t))))
          (etaf-region-swap thumb-head-region head-next-region)
          (cons thumb-head-region head-next-region)))
       ((eq 'up up-or-down) ;; 向上移动
        (let ((head-prev-region
               (progn (goto-char thumb-head-start)
                      (etaf-property-backward-region
                       'etaf-v-scroll-area uuid t))))
          (etaf-region-swap head-prev-region thumb-head-region)
          (cons thumb-head-region head-prev-region)))))))

(defun etaf-box-v-scroll-2-height-thumb-shift (up-or-down uuid)
  "thumb-height = 2 时，滚动条如何移动一次，返回滚动前后的 region"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-head-region (etaf-property-forward-region
                               'etaf-v-scroll-thumb-head uuid t))
           (thumb-head-start (car thumb-head-region))
           (thumb-head-end (cdr thumb-head-region))
           (thumb-tail-region (etaf-property-forward-region
                               'etaf-v-scroll-thumb-tail uuid t))
           (thumb-tail-start (car thumb-tail-region))
           (thumb-tail-end (cdr thumb-tail-region)))
      (cond
       ((eq 'down up-or-down) ;; 向下移动
        (let ((tail-next-region
               (progn (goto-char thumb-tail-end)
                      (etaf-property-forward-region
                       'etaf-v-scroll-area uuid t))))
          ;; 先交换 tail 再交换 head
          (etaf-region-swap thumb-tail-region tail-next-region)
          (etaf-region-swap thumb-head-region thumb-tail-region)
          (cons thumb-head-region thumb-tail-region)))
       ((eq 'up up-or-down) ;; 向上移动
        (let ((head-prev-region
               (progn (goto-char thumb-head-start)
                      (etaf-property-backward-region
                       'etaf-v-scroll-area uuid t))))
          ;; 先交换 head 再交换 tail
          (etaf-region-swap head-prev-region thumb-head-region)
          (etaf-region-swap thumb-head-region thumb-tail-region)
          (cons thumb-tail-region thumb-head-region)))))))

(defun etaf-box-v-scroll-n-height-thumb-shift (up-or-down uuid)
  "thumb-height > 1 时，滚动条如何移动一次，返回滚动前后的 region"
  (save-excursion
    (goto-char (point-min))
    (let* ((thumb-head-region (etaf-property-forward-region
                               'etaf-v-scroll-thumb-head uuid t))
           (thumb-head-start (car thumb-head-region))
           (thumb-head-end (cdr thumb-head-region))
           (thumb-tail-region (etaf-property-forward-region
                               'etaf-v-scroll-thumb-tail uuid t))
           (thumb-tail-start (car thumb-tail-region))
           (thumb-tail-end (cdr thumb-tail-region)))
      (cond
       ((eq 'down up-or-down) ;; 向下移动
        (let* ((head-next-region
                (progn (goto-char thumb-head-end)
                       (etaf-property-forward-region
                        'etaf-v-scroll-area uuid t)))
               (tail-next-region
                (progn (goto-char thumb-tail-end)
                       (etaf-property-forward-region
                        'etaf-v-scroll-area uuid t))))
          (etaf-region-swap thumb-head-region head-next-region)
          (etaf-region-swap thumb-tail-region tail-next-region)
          (etaf-region-swap thumb-head-region thumb-tail-region)
          (cons thumb-head-region head-next-region)))
       ((eq 'up up-or-down) ;; 向上移动
        (let* ((head-prev-region
                (progn (goto-char thumb-head-start)
                       (etaf-property-backward-region
                        'etaf-v-scroll-area uuid t)))
               (tail-prev-region
                (progn (goto-char thumb-tail-start)
                       (etaf-property-backward-region
                        'etaf-v-scroll-area uuid t))))
          (etaf-region-swap head-prev-region thumb-head-region)
          (etaf-region-swap tail-prev-region thumb-tail-region)
          (etaf-region-swap thumb-head-region thumb-tail-region)
          (cons thumb-tail-region tail-prev-region)))))))

;;;###autoload
(defun etaf-box-scroll-up ()
  (interactive)
  (etaf-box-scroll 'up))

;;;###autoload
(defun etaf-box-scroll-down ()
  (interactive)
  (etaf-box-scroll 'down))

(provide 'etaf-box)
