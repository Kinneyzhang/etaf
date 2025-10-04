(defun etml-box-y-scroll-bar-p (box)
  "Return if BLOCK should have scroll bar in vertical."
  (let* ((overflow (oref box y-overflow))
         (original-height
          (etml-string-linum (etml-box-content box)))
         (content-height (etml-box-content-height box)))
    ;; 需要滚动 且 支持显示滚动条时，才显示滚动条
    (and (eq overflow 'scroll)
         (> original-height content-height))))

(defun etml-block-bottom-border-line (width border)
  (if border
      (etml-propertize (etml-pixel-spacing width)
                       `(face (:overline ,border)))
    (etml-pixel-spacing width)))

核心属性分类
1. 基本布局属性
line-height：行高控制
text-align：文本对齐
text-indent：首行缩进
direction：文本方向
2. 断行与换行控制
overflow-wrap：溢出换行处理
word-break：单词断行规则
line-break：换行规则
text-wrap：文本换行模式
text-overflow：文本溢出处理
3. 间距与缩进
letter-spacing：字符间距
word-spacing：单词间距
text-indent-hanging：悬挂缩进
word-spacing-adjust：单词间距调整
4. 高级排版特性
hyphens：连字符控制
hanging-punctuation：标点悬挂
text-justify：两端对齐
text-align-last：最后一行对齐
5. 书写模式与方向
writing-mode：书写模式
text-orientation：文本方向（竖排）
text-combine-upright：竖排文本组合
6. 字体相关布局属性
font-family：字体系列（影响字符宽度）
font-size：字体大小（影响行高计算）
font-kerning：字距调整
font-feature-settings：OpenType特性
7. 优化与限制
text-rendering：文本渲染优化
hyphenate-limit-chars：连字符最小字符数限制
hyphenate-limit-lines：连字符行数限制
text-wrap-balance：换行平衡控制

复合属性
(margin-collapse :initarg :margin-collapse :initform nil :documentation "外边距合并行为控制")
(border :initarg :border :initform nil :documentation "所有边框的简写属性")
(border-width :initarg :border-width :initform 'medium :documentation "边框宽度")
(border-style :initarg :border-style :initform nil :documentation "边框样式")
(border-color :initarg :border-color :initform 'currentColor :documentation "边框颜色")
(border-top :initarg :border-top :initform nil :documentation "上边框简写")
(border-right :initarg :border-right :initform nil :documentation "右边框简写")
(border-bottom :initarg :border-bottom :initform nil :documentation "下边框简写")
(border-left :initarg :border-left :initform nil :documentation "左边框简写")

(defclass etml-text-css ()
  ((overflow-wrap :initarg :wrap :initform 'kp :documentation "文本换行方式")
   (color :initarg :color :initform nil :documentation "文本颜色")
   (background-color :initarg :bgcolor :initform nil :documentation "文本背景颜色")
   (font-family :initarg :family :initform nil :documentation "字体系列")
   (font-size :initarg :size :initform nil :documentation "字体大小")
   (font-weight :initarg :weight :initform nil :documentation "字体粗细")
   (text-underline :initarg :underline :initform nil :documentation "字体下划线")
   (text-overline :initarg :overline :initform nil :documentation "字体上划线")
   (text-strike :initarg :strike-through :initform nil :documentation "字体中划线")
   (text-align :initarg :align :initform 'left :documentation "文本对齐")
   (text-transform :initarg :transform :initform nil :documentation "文本转换")
   (letter-spacing :initarg :letter-spacing :initform 'normal :documentation "字母间距")
   (word-spacing :initarg :word-spacing :initform 'normal :documentation "单词间距")
   (white-space :initarg :white-space :initform 'normal :documentation "空白处理"))
  "文本属性模型")

(items-flex
 (let ((flexs (oref flex items-flex)))
   (cond
    ((symbolp flexs)
     (pcase flexs
       ('none '(0 0 auto))
       ('auto '(1 1 auto))
       (_ (error "Invalid format of items-flex!"))))
    ((vectorp flexs) flexs)
    ((consp flexs) (make-vector num flexs)))))

(blocks-widths
 ;; if flex-basis is number or cons-cell
 ;; use it to override block total width
 (seq-map-indexed
  (lambda (item-flex idx)
    (let ((basis (nth 2 item-flex))
          (block (aref blocks idx)))
      (cond
       ((eq 'auto basis)
        ;; 保持原始宽度
        (etml-block-total-pixel block))
       ((integerp basis)
        ;; 指定个数的字符宽度
        (oset block width basis)
        (etml-block-total-pixel block))
       ((consp basis)
        ;; 指定像素宽度
        (oset block width (list basis))
        basis))))
  items-flex))
