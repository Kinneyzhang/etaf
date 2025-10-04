;; -*- lexical-binding: t; -*-

(require 'etml-utils)
(require 'etml-scroll-bar)

(defclass etml-text-css ()
  ((overflow-wrap :initarg :overflow-wrap :initform 'kp :documentation "文本换行方式")
   (color :initarg :color :initform nil :documentation "文本颜色")
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
   (letter-spacing :initarg :letter-spacing :initform 'normal :documentation "字母间距")
   (word-spacing :initarg :word-spacing :initform 'normal :documentation "单词间距")
   (white-space :initarg :white-space :initform 'normal :documentation "空白处理"))
  "文本属性模型")

(defclass etml-box (etml-text-css)
  ((content :initarg :content :initform " " :documentation "元素的内容区域")
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
   (border-left-color :initarg :border-left-color :initform nil :documentation "左边框颜色")
   (border-right-pixel :initarg :border-right-pixel :initform 0 :documentation "右边框宽度")
   (border-right-color :initarg :border-right-color :initform nil :documentation "右边框颜色")
   
   ;; 使用 face :overline 实现上边框
   (border-top-p :initarg :border-top-p :initform nil :documentation "是否设置上边框")
   (border-top-color :initarg :border-top-color :initform nil :documentation "上边框颜色")
   ;; 使用 face :underline 实现下边框
   (border-bottom-p :initarg :border-bottom-p :initform nil :documentation "是否设置下边框")
   (border-bottom-color :initarg :border-bottom-color :initform nil :documentation "下边框颜色")
   (border-bottom-style :initarg :border-bottom-style :initform nil :documentation "下边框样式")
   
   (visibility :initarg :visibility :initform 'visible :documentation "可见性")
   (z-index :initarg :z-index :initform 'auto :documentation "堆叠顺序")
   (position :initarg :position :initform 'static :documentation "定位方式")
   ;; (float :initarg :float :initform nil :documentation "浮动方式")
   ;; (clear :initarg :clear :initform nil :documentation "清除浮动")
   ;; (box-decoration-break :initarg :box-decoration-break :initform 'slice :documentation "框装饰拆分方式")

   (overflow-x :initarg :overflow-x :initform 'visible :documentation "水平方向溢出处理")
   (v-scroll-bar :initarg :v-scroll-bar :initform (etml-scroll-bar)
                 :type etml-scroll-bar :documentation "垂直方向的滚动条")
   (v-scroll-bar-direction :initarg :v-scroll-bar-direction :initform 'right
                           :documentation "垂直方向的滚动条的位置 'left or 'right")
   
   (overflow-y :initarg :overflow-y :initform 'visible :documentation "垂直方向溢出处理")
   (h-scroll-bar-color :initarg :h-scroll-bar-color :initform nil :documentation "滚动条颜色")
   (h-scroll-bar-direction :initarg :h-scroll-bar-direction :initform right :documentation "滚动条方向")
   (h-scroll-bar-style :initarg :h-scroll-bar-style :initform nil :documentation "滚动条样式风格"))
  "CSS 基础框盒模型类，表示CSS视觉格式化模型中的长方形盒子。")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etml-box-content-pixel (box)
  "根据 min-width, max-width, width, content-width
计算内容最终需要使用的像素宽度。"
  (let* ((content (oref box content))
         (original-pixel (string-pixel-width content))
         (min-pixel (etml-width-pixel (oref box min-width) content))
         (max-pixel (etml-width-pixel (oref box max-width) content))
         (curr-pixel (etml-width-pixel (oref box width) content)))
    (min (or max-pixel 999999999)
         (max (or min-pixel 0)
              (or curr-pixel original-pixel)))))

(defun etml-box-side-pixel (box &optional side)
  "除了内容以外的两侧的像素宽度和。SIDE 表示指定 'left 或 'right
一侧的 border,padding,margin 的总像素宽度。"
  (let* ((left-side-pixel (+ (oref box border-left-pixel)
                             (oref box padding-left-pixel)
                             (oref box margin-left-pixel)))
         (right-side-pixel (+ (oref box border-right-pixel)
                              (oref box padding-right-pixel)
                              (oref box margin-right-pixel)))
         ;; 如果 overflow-x:scroll 会有固定的滚动条宽度
         (overflow-x (oref box overflow-x))
         (v-scroll-bar-pixel (etml-scroll-bar-pixel
                              (oref box v-scroll-bar)))
         (v-scroll-bar-direction (oref box v-scroll-bar-direction)))
    (pcase side
      ('left (+ left-side-pixel
                (if (and (eq 'scroll overflow-x)
                         (eq 'left v-scroll-bar-direction))
                    v-scroll-bar-pixel
                  0)))
      ('right (+ right-side-pixel
                 (if (and (eq 'scroll overflow-x)
                          (eq 'right v-scroll-bar-direction))
                     v-scroll-bar-pixel
                   0)))
      (_ (+ left-side-pixel right-side-pixel
            (if (eq 'scroll overflow-x)
                v-scroll-bar-pixel
              0))))))

(defun etml-box-total-pixel (box)
  "Return the total pixel width of BLOCK."
  (+ (etml-box-side-pixel box)
     (etml-box-content-pixel box)))

(defun etml-box-content-linum (box)
  "原始文本的行数"
  (etml-string-linum (etml-lines-justify
                      (oref box content)
                      (etml-box-content-pixel box)
                      (oref box text-align))))

(defun etml-box-content-height (box)
  "根据 min-height, max-height, height, content-height 计算
文本设置了宽度之后的高度。"
  (let ((content-linum (etml-box-content-linum box))
        (min-height (oref box min-height))
        (max-height (oref box max-height))
        (curr-height (oref box height)))
    (min (or max-height 999999999)
         (max (or min-height 0)
              (or curr-height content-linum)))))

(defun etml-box-side-height (box)
  "内容以外，上下 padding, margin 的总高度"
  (+ (oref box padding-top-height)
     (oref box padding-bottom-height)
     (oref box margin-top-height)
     (oref box margin-bottom-height)))

(defun etml-box-total-height (box)
  "整个 box 的总高度，包含 content, padding, margin"
  (+ (etml-box-side-height box)
     (etml-box-content-height box)))

(defun etml-box-content (box)
  "设置了宽度和高度之后的文本内容"
  (etml-lines-align (etml-lines-justify
                     (oref box content)
                     (etml-box-content-pixel box)
                     (oref box text-align))
                    (etml-box-content-height box)
                    (oref box vertical-align)))

(defun etml-box-v-scroll-bar-p (box)
  "是否显示垂直方向的滚动条"
  (let ((overflow (oref box overflow-y))
        (content-linum (etml-box-content-linum box))
        (content-height (etml-box-content-height box)))
    ;; 需要滚动 且 支持显示滚动条时，才显示滚动条
    (and (eq overflow 'scroll)
         (> content-linum content-height))))

(defun etml-box-v-scroll-bar-render (box)
  "返回垂直方向的滚动条字符串"
  (when (eq (oref box overflow-y) 'scroll)
    
    )
  )

;;; FIXME:
(defun etml-scroll-bar-info (content-linum content-height)
  "计算 v-scroll-bar-height 和 v-scroll-bar-steps"
  (let (v-scroll-bar-height ;; 滚动条高度
        ;; 滚动条每次移动对应的滚动次数列表
        v-scroll-bar-steps)
    ;; content-linum 是原始内容的行数
    ;; content-height 是盒子内容部分展示的高度
    ;; overflow-linum 是溢出的行数
    (when-let ((overflow-linum (- content-linum content-height)))
      (if (> content-height overflow-linum)
          ;; 盒子内容高度 > 溢出行数
          (progn
            (setq v-scroll-bar-height
                  (- content-height overflow-linum))
            (setq v-scroll-bar-steps
                  (make-list overflow-linum 1)))
        ;; 内容高度 <= 溢出高度，滚动条高度始终为1
        (setq v-scroll-bar-height 1)
        ;; overflow-linum 行溢出内容平摊到 (1- content-height) 行的滚动中  
        (setq v-scroll-bar-steps
              ;; 当高度为1时，滚动条无法移动
              (when (> content-height 1)
                (etml-split-size
                 overflow-linum (1- content-height) nil nil nil t)))))
    (cons v-scroll-bar-height v-scroll-bar-steps)))

(defun etml-box-v-scroll-bar-height (box)
  (let ((content-linum (etml-box-content-linum box))
        (content-height (etml-box-content-height box)))
    (car (etml-scroll-bar-info content-linum content-height))))

(defun etml-box-v-scroll-bar-steps (box)
  (let ((content-linum (etml-box-content-linum box))
        (content-height (etml-box-content-height box)))
    (cdr (etml-scroll-bar-info content-linum content-height))))

;; (defun etml-box-v-scroll-bar-render (box)
;;   ""
;;   (let* ((overflow (oref box overflow-x))
;;          (original-pixel
;;           (string-pixel-width (etml-box-content box)))
;;          (content-pixel (etml-box-content-pixel box)))
;;     ;; 需要滚动 且 支持显示滚动条时，才显示滚动条
;;     (when (and (eq overflow 'scroll)
;;                (> original-pixel content-pixel))
;;       (- original-pixel content-pixel))))

(defun etml-box-render (box)
  "返回 box 渲染后的文本"
  (let* ((v-scroll-bar-p (etml-box-v-scroll-bar-p box))
         ;; (h-scroll-bar-p (etml-box-h-scroll-bar-p box))
         ;; get margin
         (margin-top-height (oref box margin-top-height))
         (margin-bottom-height (oref box margin-bottom-height))
         (margin-left-pixel (oref box margin-left-pixel))
         (margin-right-pixel (oref box margin-right-pixel))
         ;; get padding
         (padding-top-height (oref box padding-top-height))
         (padding-bottom-height (oref box padding-bottom-height))
         (padding-left-pixel (oref box padding-left-pixel))
         (padding-right-pixel (oref box padding-right-pixel))
         (content-height (etml-box-content-height box))
         (inner-height (+ content-height padding-top-height
                          padding-bottom-height))
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
         
         (content-pixel (etml-box-content-pixel box))
         (box-string (etml-box-content box)))
    
    ;; 构建 box-string 的顺序:
    ;; 1. stack: content + top-padding(content-pixel)
    ;;           + bottom-padding(content-pixel)
    (setq box-string
          (etml-lines-stack
           (list (etml-pixel-blank content-pixel padding-top-height)
                 box-string
                 (etml-pixel-blank content-pixel padding-bottom-height))))
    ;; 2. concat: 1 + left-padding(inner-height)
    ;;            + right-padding(inner-height)
    (setq box-string
          (etml-lines-concat
           (list (etml-pixel-blank padding-left-pixel inner-height)
                 box-string
                 (etml-pixel-blank padding-right-pixel inner-height))))
    ;; 设置垂直方向滚动条
    (when v-scroll-bar-p
      (let ((v-scroll-bar (oref box v-scroll-bar))
            (thumb-height (etml-box-v-scroll-bar-height box)))
        (oset v-scroll-bar thumb-height thumb-height)
        (oset v-scroll-bar track-height content-height)
        ;; (oset v-scroll-bar track-left-border-pixel 1)
        ;; (oset v-scroll-bar track-right-border-pixel 1)
        (setq box-string
              (etml-lines-concat
               (list box-string
                     (etml-scroll-bar-render v-scroll-bar))))))
    ;; 3. set top(overline) and bottom(underline) border
    (when (or border-top-p border-bottom-p)
      (let ((lines (split-string box-string "\n" t)))
        (when border-top-p
          (setf (car lines)
                (etml-propertize
                 (car lines)
                 `(face (:overline ,(or border-top-color t))))))
        (when border-bottom-p
          (setf (car (last lines))
                (etml-propertize
                 (car (last lines))
                 `(face
                   (:underline
                    ,(let ((lst '(:position t)))
                       (when border-bottom-color
                         (setq lst
                               (append lst `(:color ,border-bottom-color))))
                       (when border-bottom-style
                         (setq lst
                               (append
                                lst `(:style ,border-bottom-style))))
                       lst))))))
        (setq box-string (string-join lines "\n"))))
    ;; 4. 设置背景色
    (when-let ((bgcolor (oref box bgcolor)))
      (setq box-string
            (etml-maplines
             (lambda (line)
               (etml-propertize line `(face (:background ,bgcolor))))
             box-string 'string-join "\n")))
    ;; 5. concat: set left and right border(inner-height)
    ;;            set left and right margin(inner-height)
    (setq box-string
          (etml-lines-concat
           (list
            (etml-pixel-blank margin-left-pixel inner-height)
            (etml-pixel-border border-left-pixel
                               inner-height border-left-color)
            box-string
            (etml-pixel-border border-right-pixel
                               inner-height border-right-color)
            (etml-pixel-blank margin-right-pixel inner-height))))
    ;; 6. stack: set top and bottom margin(total pixel)
    (let ((total-pixel (etml-box-total-pixel box)))
      (setq box-string
            (etml-lines-stack
             (list (etml-pixel-blank total-pixel margin-top-height)
                   box-string
                   (etml-pixel-blank
                    total-pixel margin-bottom-height)))))
    
    ;; FIXME: top-padding 和 bottom-padding < 1 时, 设置首尾行的 line-height
    ;; FIXME: 应用其他字体相关的属性
    box-string))

;;;###autoload
(defun etml-box-string (&rest kvs)
  (etml-box-render (apply #'etml-box kvs)))

;;;###autoload
(defun etml-box-concat (&rest blocks)
  "TEXT-ALIGN should be one of left,center,right."
  ;; ALIGN should be one of top,center,bottom.
  (etml-box :content (etml-lines-concat
                      (mapcar (lambda (block)
                                (pcase block
                                  ((pred etml-flex-p)
                                   (etml-flex-render block))
                                  ((pred etml-box-p)
                                   (etml-box-render block))))
                              blocks))))

;;;###autoload
(defun etml-box-stack (&rest blocks)
  "ALIGN used for all blocks."
  ;; TEXT-ALIGN used for text in a block.
  (etml-box :content (etml-lines-stack
                      (mapcar (lambda (block)
                                (pcase block
                                  ((pred etml-flex-p)
                                   (etml-flex-render block))
                                  ((pred etml-box-p)
                                   (etml-box-render block))))
                              blocks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; etml block scroll

(defvar etml-box-scroll-down-keys '("n" [wheel-down]))
(defvar etml-box-scroll-up-keys '("p" [wheel-up]))
;; (defvar etml-box-redraw-keys '("g"))

(defun etml-box-scroll-map ()
  (let ((map (make-sparse-keymap)))
    (dolist (key etml-box-scroll-down-keys)
      (define-key map key #'etml-box-scroll-down))
    (dolist (key etml-box-scroll-up-keys)
      (define-key map key #'etml-box-scroll-up))
    map))

(defun etml-box-scroll-bar-color (block)
  (let ((color (oref box scroll-bar-color)))
    (when (stringp color)
      (setq color (cons color color)))
    (pcase (etml-background-type)
      ('light (car color))
      ('dark (cdr color)))))

(defun etml-box-scroll (up-or-down)
  (when-let* ((props (text-properties-at (point)))
              ;; 当前光标所在行 idx，用于滚动后保持光标位置
              (line-idx (plist-get props 'etml-box-line-idx))
              (uuid (plist-get props 'etml-box-line))
              (offset (plist-get props 'etml-box-y-offset)))
    (when-let* ((inhibit-read-only t)
                (caches (gethash uuid etml-box-caches))
                (content-shown-height
                 (plist-get caches :shown-lines-height))
                (top-padding
                 (plist-get caches :top-padding))
                (bottom-padding
                 (plist-get caches :bottom-padding))
                (top-border-pixel
                 (plist-get caches :top-border-pixel))
                (bottom-border-pixel
                 (plist-get caches :bottom-border-pixel))
                (lines (plist-get caches :lines))
                (content-total-height (length lines))
                (max-offset (- content-total-height
                               content-shown-height))
                (all-content-idx offset)
                (new-offset
                 ;; ori-content-height - box-content-height
                 (cond ((eq 'down up-or-down) (1+ offset))
                       ((eq 'up up-or-down) (1- offset))))
                (shown-content-idx 0))
      (when (and (>= new-offset 0)
                 (<= new-offset max-offset))
        (let ((bgcolor (plist-get caches :bgcolor)))
          (goto-char (point-min))
          ;; 根据 uuid 属性搜索并将每一行替换为滚动后的新的文本
          (while-let ((match (text-property-search-forward
                              'etml-box-line uuid t))
                      (start (prop-match-beginning match))
                      (end (prop-match-end match))
                      (line (nth
                             (cond
                              ((eq 'down up-or-down)
                               (1+ all-content-idx))
                              ((eq 'up up-or-down)
                               (1- all-content-idx)))
                             lines)))
            (setq line (propertize
                        line
                        'etml-box-line uuid
                        'etml-box-line-idx
                        (format "%s:%s" uuid shown-content-idx)
                        'etml-box-y-offset new-offset
                        'etml-box-y-height content-shown-height
                        'keymap (etml-box-scroll-map)))
            ;; padding < 1 and has top/bottom border
            ;; should set top and bottom border
            (cond
             ((= shown-content-idx 0)
              (when (and (< top-padding 1)
                         (> top-border-pixel 0))
                (setq line
                      (etml-propertize
                       line
                       `(face (:overline
                               ,(plist-get caches :top-border-color)))))))
             ((= shown-content-idx (1- content-shown-height))
              (when (and (< bottom-padding 1)
                         (> bottom-border-pixel 0))
                (setq line
                      (etml-propertize
                       line
                       `(face
                         (:underline
                          ( :color ,(plist-get caches
                                               :top-border-color)
                            :position t))))))))
            (delete-region start end)
            (goto-char start)
            (if bgcolor
                (insert (etml-propertize
                         line `(face (:background ,bgcolor))))
              (insert line))
            (cl-incf all-content-idx 1)
            (cl-incf shown-content-idx 1)))
        ;; 根据 uuid 属性搜索并设置滚动条的首行和尾行
        (goto-char (point-min))
        (when-let*
            ((scroll-top-match
              (text-property-search-forward
               'etml-box-scroll-bar-start uuid t))
             (scroll-top-start
              (prop-match-beginning scroll-top-match))
             (scroll-top-end
              (prop-match-end scroll-top-match))
             (scroll-bar-color
              (save-excursion
                (goto-char scroll-top-start)
                (let ((plist
                       (seq-mapcat
                        (lambda (elem)
                          (if (consp elem) elem (list elem)))
                        (plist-get (text-properties-at (point))
                                   'face))))
                  (plist-get plist :foreground))))
             (prefixs
              (plist-get
               (text-properties-at scroll-top-start)
               'etml-box-scroll-prefixs))
             (prev-shown-offset (etml--num-in-prefixs
                                 offset prefixs))
             (curr-shown-offset (etml--num-in-prefixs
                                 new-offset prefixs))
             ;; shown-offset 有变化时，才移动滚动条
             ((not (= prev-shown-offset
                      curr-shown-offset)))
             ;; (one-line-scroll-bar
             ;;  (buffer-substring scroll-top-start
             ;;                    scroll-top-end))
             ;; (scroll-bar-pixel (string-pixel-width
             ;;                    one-line-scroll-bar))
             )
          (save-excursion
            (goto-char (point-min))
            (when-let* ((scroll-bottom-match
                         (text-property-search-forward
                          'etml-box-scroll-bar-end uuid t))
                        (scroll-bottom-start
                         (prop-match-beginning scroll-bottom-match))
                        (scroll-bottom-end
                         (prop-match-end scroll-bottom-match)))
              (cond
               ;; 向下滚动
               ((eq 'down up-or-down)
                ;; 首行 face 的 :inverse-video :foreground 清空
                (etml-remove-face-attributes
                 scroll-top-start scroll-top-end
                 '(:inverse-video :foreground))
                ;; 首行 etml-box-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-top-start scroll-top-end
                 '( etml-box-scroll-bar-start nil
                    etml-box-scroll-prefixs nil))
                
                ;; 如果 padding < 1 且有上边框，需要保留上边框 face
                ;; (when (and (< top-padding 1)
                ;;            (= prev-shown-offset 0)
                ;;            (> top-border-pixel 0))
                ;;   (add-text-properties
                ;;    scroll-top-start scroll-top-end
                ;;    `(face (:overline
                ;;            ,(plist-get
                ;;              caches :top-border-color)))))
                
                ;; 第二行设置为新的首行
                ;; (next-logical-line 1)
                (goto-char scroll-top-end)
                (when-let* ((match (text-property-search-forward
                                    'etml-box-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( etml-box-scroll-bar-start ,uuid
                      etml-box-scroll-prefixs ,prefixs)))
                ;; 尾行的 etml-box-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-bottom-start scroll-bottom-end
                 '(etml-box-scroll-bar-end nil))
                ;; 尾行的下一行设置为滚动条，并设置为新的尾行 uuid 属性
                (goto-char scroll-bottom-end)
                (when-let* ((match (text-property-search-forward
                                    'etml-box-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( face ( :inverse-video t
                             :foreground ,scroll-bar-color)
                      etml-box-scroll-bar-end ,uuid))))
               ;; 向上滚动
               ((eq 'up up-or-down)
                ;; 尾行的 scroll bar face 清空
                (etml-remove-face-attributes
                 scroll-bottom-start scroll-bottom-end
                 '(:inverse-video :foreground))
                ;; 尾行的 etml-box-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-bottom-start scroll-bottom-end
                 '(etml-box-scroll-bar-end nil))
                
                ;; 如果 padding < 1 且有下边框，需要保留下边框 face
                ;; (when (and (< bottom-padding 1)
                ;;            (= prev-shown-offset
                ;;               (1- content-shown-height))
                ;;            (> bottom-border-pixel 0))
                ;;   (add-text-properties
                ;;    scroll-bottom-start scroll-bottom-end
                ;;    `(face
                ;;      (:underline
                ;;       ( :position t
                ;;         :color ,(plist-get
                ;;                  caches :bottom-border-color))))))
                
                ;; 倒数第二行设置新的尾行
                (goto-char scroll-bottom-start)
                (when-let* ((match (text-property-search-backward
                                    'etml-box-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `(etml-box-scroll-bar-end ,uuid)))
                ;; 首行的 etml-box-scroll-bar-start 属性清空
                (add-text-properties
                 scroll-top-start scroll-top-end
                 '( etml-box-scroll-bar-start nil
                    etml-box-scroll-prefixs nil))
                ;; 首行的上一行设置为滚动条，并设置为新的首行
                (goto-char scroll-top-start)
                (when-let* ((match (text-property-search-backward
                                    'etml-box-scroll-bar uuid t))
                            (start (prop-match-beginning match))
                            (end (prop-match-end match)))
                  (add-text-properties
                   start end
                   `( face ( :inverse-video t
                             :foreground ,scroll-bar-color)
                      etml-box-scroll-bar-start ,uuid
                      etml-box-scroll-prefixs ,prefixs))))))))))
    (goto-char (point-min))
    ;; 滚动之后，重定向光标到当前行文本的开头
    (when-let ((match (text-property-search-forward
                       'etml-box-line-idx line-idx t)))
      (goto-char (prop-match-beginning match)))))

;;;###autoload
(defun etml-box-scroll-up ()
  (interactive)
  (etml-box-scroll 'up))

;;;###autoload
(defun etml-box-scroll-down ()
  (interactive)
  (etml-box-scroll 'down))

(provide 'etml-box)
