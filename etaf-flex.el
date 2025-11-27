;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'ekp)
(require 'etaf-box)

;;; type check functions

(defun etaf-flex-number-vector-p (value)
  "检查类型: 单个值为整数; 多个值为向量，向量中的每个元素为整数"
  (or (numberp value)
      (and (vectorp value)
           (seq-every-p #'numberp value))))

(defun etaf-flex-number-auto-vector-p (value)
  "检查类型: 单个值为整数或 'auto; 多个值为向量，向量中的每个元素为 整数或 'auto。"
  (or (or (numberp value)
          (eq 'auto value))
      (and (vectorp value)
           (seq-every-p (lambda (e)
                          (or (numberp e)
                              (eq 'auto e)))
                        value))))

(defun etaf-flex-items-flex-p (value)
  "检查 item-flex 的类型: 单个值为列表，列表形式为 '(0 1 auto)，第一第二个元素为整数，
第三个元素为 'auto 或整数；多个值为向量，向量中每个元素为列表，列表中的元素类型同上。"
  (let ((check-lst
         (lambda (lst)
           (and (integerp (nth 0 lst))
                (integerp (nth 1 lst))
                (let ((el (nth 2 lst)))
                  (or (eq 'auto el)
                      (integerp el)
                      (and (consp el)
                           (= 1 (length el))
                           (integerp (car el)))))))))
    (cond
     ((symbolp value) (or (eq 'none value)
                          (eq 'auto value)))
     ((listp value) (funcall check-lst value))
     ((vectorp value) (seq-every-p check-lst value)))))

(defun etaf-flex-items-align-p (value)
  "检查类型: 单个值为symbol; 多个值为向量，向量中的每个元素为symbol"
  (let ((check-sym
         (lambda (sym)
           (member sym '( auto flex-start flex-end
                          center baseline stretch)))))
    (cond
     ((symbolp value) (funcall check-sym value))
     ((vectorp value) (seq-every-p check-sym value)))))

;;; type check functions ends.

(defclass etaf-flex-item (etaf-box)
  ((order :initarg :order :initform 0 :type integer
          :documentation "item 在 flex 容器中顺序。")
   (basis :initarg :basis :initform 'auto :type (or integer symbol)
          :documentation "item 在​​分配空间前​​的​​初始尺寸。")
   (grow :initarg :grow :initform 0 :type integer
         :documentation "item 在​​容器有剩余空间时​​的​​增长系数​​。")
   (shrink :initarg :shrink :initform 1 :type integer
           :documentation "item 在​​容器空间不足时​​的​​缩小系数。")
   ;; 避免与 etaf-box 的 align 属性冲突
   (cross-align
    :initarg :cross-align :initform 'auto :type symbol
    :documentation "item 在交叉轴的对齐方式，覆盖容器的 items-align 属性。")))

;; horizontal unit is pixel.
;; vertical unit is line number.
;; main-axis main-start main-end main-units
;; cross-axis cross-start cross-end cross-units

(defclass etaf-flex (etaf-box)
  ((direction :initarg :direction :initform 'row :type symbol
              :documentation "主轴方向。")
   (wrap :initarg :wrap :initform 'nowrap :type symbol
         :documentation "换行方式。")
   (content-justify
    :initarg :content-justify :initform 'flex-start :type symbol
    :documentation "主轴对齐方式。")
   (content-align
    :initarg :content-align :initform 'stretch :type symbol
    :documentation "多个主轴在垂直方向的对齐方式。")
   (row-gap :initarg :row-gap :initform 0
            :type number :documentation "多行之间的 gap。")
   (column-gap :initarg :column-gap :initform 0
               :type number :documentation "多列之间的 gap。")
   (items-align
    :initarg :items-align :initform 'stretch :type symbol
    :documentation "所有 items 在交叉轴的对齐方式。"))
  "ETAF flex layout model.")

(defun etaf-flex-item-string (item)
  "item 的内容的字符串"
  (let ((content (oref item content)))
    (pcase content
      ((pred stringp) content)
      ((pred etaf-box-p) (etaf-box-string content))
      ;; FIXME: bug here
      ((pred etaf-flex-p) (etaf-flex-string content))
      (_ (error "Invalid type %s of content in item."
                (type-of content))))))

;; FIXME: item content 和 block conent 冲突
(defun etaf-flex-item-total-pixel (item)
  ;; 临时将 item content 设置为 string，使用 block 方法计算
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (pixel (etaf-box-total-pixel item)))
    ;; 还原
    (oset item content content)
    pixel))

(defun etaf-flex-item-total-height (item)
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (height (etaf-box-total-height item)))
    (oset item content content)
    height))

(defun etaf-flex-item-side-pixel (item)
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (pixel (etaf-box-side-pixel item)))
    (oset item content content)
    pixel))

(defun etaf-flex-item-side-height (item)
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (height (etaf-box-side-height item)))
    (oset item content content)
    height))

(defun etaf-flex-item-content-pixel (item)
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (pixel (etaf-box-content-pixel item)))
    (oset item content content)
    pixel))

(defun etaf-flex-item-content-height (item)
  (let* ((content (oref item content))
         (string (etaf-flex-item-string item))
         (_ (oset item content string))
         (height (etaf-box-content-height item)))
    (oset item content content)
    height))

(defun etaf-flex-parse-basis (item flex)
  ;; 对于左右方向排列的文本，宽度会影响高度，但是高度不会影响宽度
  ;; min(max)-width 优先级比 min(max)-content 高
  "返回 item 的基础宽度和最小宽度"
  (let* ((direction (oref flex direction))
         (basis (oref item basis))
         (string (etaf-flex-item-string item))
         ;; (block (oref item content))
         ;; (content (oref block content))
         ;; 最长的单词的宽度作为最小单位长度
         (block-min-pixel (seq-max (ekp-boxes-widths string)))
         (content (oref item content))
         (block-side-pixel
          (pcase content
            ((pred etaf-box-p)
             (etaf-flex-item-side-pixel content))
            ((pred stringp) 0)
            ;; FIXME: cal etaf-flex side pixel
            ((pred etaf-flex-p) 0)
            (_ (error "Invalid type %s of content in item."
                      (type-of content)))))
         curr-units min-units max-units)
    ;; basis 是 item 的初始长度，包含 margin, padding !
    (pcase direction
      ((or 'row 'row-reverse)
       (setq curr-units (etaf-flex-item-total-pixel item))
       (setq min-units
             (progn
               (oset item width (list (+ block-side-pixel
                                         block-min-pixel)))
               (etaf-flex-item-total-pixel item)))
       ;; 这里的 max-units 是 (min max-content max-width)
       ;; basis: max-content 也是 (min max-content max-width)
       (setq max-units
             (progn
               (oset item width nil
                     ;; (list (+ block-side-pixel
                     ;;          (string-pixel-width content)))
                     )
               (etaf-flex-item-total-pixel item)))
       (list (pcase basis
               ((pred numberp)
                (if (or ;; (> basis max-units)
                     (< basis min-units))
                    (error "basis %s it not between min-units %s\
 and max-units %s" basis min-units max-units)
                  basis))
               ('auto curr-units)
               ((or 'content 'max-content) max-units)
               ('min-content min-units)
               ('fit-content
                (min max-units (max min-units curr-units))))
             min-units max-units))
      ((or 'column 'column-reverse)
       (setq curr-units (etaf-flex-item-total-height item))
       (setq min-units
             (progn (oset item width (list block-min-pixel))
                    (etaf-flex-item-total-height item)))
       (setq max-units (progn (oset item width nil)
                              (etaf-flex-item-total-height item)))
       (list (pcase basis
               ((pred numberp)
                (if (or (< basis min-units)
                        ;; (> basis max-units)
                        )
                    (error "basis %s it not between min-units %s\
 and max-units %s" basis min-units max-units)
                  basis))
               ('auto curr-units)
               ((or 'content 'max-content) max-units)
               ('min-content min-units)
               ('fit-content
                (min max-units (max min-units curr-units))))
             min-units max-units)))))

(defun etaf-flex--cross-edge-units (item items-cross-max-units flex)
  "根据 align，计算需要在 item 交叉轴开头和结尾增加的单元数。
这是说的在一条轴线内部的对齐，和多条轴线间的对齐不一样。"
  (let* ((align (oref item cross-align))
         (align
          (if (or (null align) (eq 'auto align))
              (or (oref flex items-align) 'stretch)
            align))
         (direction (oref flex direction))
         ;; 交叉轴方向的总单位数
         (item-cross-units
          (pcase direction
            ((or 'row 'row-reverse)
             (etaf-flex-item-total-height item))
            ((or 'column 'column-reverse)
             (etaf-flex-item-total-pixel item)))))
    (pcase align
      ((or 'stretch 'normal)
       ;; FIXME: 这个实现是错的！
       ;; 需要调整交叉内容的长度
       (let* ((rest-units (- items-cross-max-units item-cross-units))
              (start-units (/ rest-units 2))
              (end-units (- rest-units start-units)))
         (cons start-units end-units)))
      ('center
       (let* ((rest-units (- items-cross-max-units item-cross-units))
              (start-units (/ rest-units 2))
              (end-units (- rest-units start-units)))
         (cons start-units end-units)))
      ('flex-start
       (cons 0 (- items-cross-max-units item-cross-units)))
      ('flex-end
       (cons (- items-cross-max-units item-cross-units) 0))
      ;; ('baseline
      ;;  ;; FIXME: 暂不支持，后续优化
      ;;  ;; 项目的第一行文本基线与容器中所有其他项目的基线对齐。
      ;;  ;; 若项目无文本，则以其底部边缘为基线。
      ;;  )
      )))

;; basis 决定 items 的基础宽度
;; grow or shrink

;; 使用基于 base-units 进行调整
(defun etaf-flex-items-grow (items-plists flex-units gaps-units direction)
  "按照 grow 拉伸并设置子项长度"
  (let* ((items (etaf-plists-get items-plists :item))
         (items-units-lst (etaf-plists-get items-plists :base-units))
         (items-units (apply #'+ items-units-lst))
         (rest-units (abs (- flex-units items-units gaps-units)))
         (grows (etaf-plists-get items-plists :grow))
         (grows-sum (apply '+ grows))
         (average-grow (if (> grows-sum 0)
                           (/ rest-units grows-sum)
                         0))
         (rest-num (if (> grows-sum 0)
                       (% rest-units grows-sum)
                     0))
         (rest-idx 0))
    ;; 设置 items 的主轴方向 grow 后的新的长度
    (seq-map-indexed
     (lambda (grow idx)
       (let* ((base-units (nth idx items-units-lst))
              (item (nth idx items))
              final-units)
         (if (= grow 0)
             ;; grow=0 不拉伸
             (setq final-units base-units)
           ;; grow>0 按比例拉伸
           (setq final-units
                 (+ base-units (* grow average-grow)
                    (if (< rest-idx rest-num) 1 0)))
           (cl-incf rest-idx 1))
         ;; 按照方向设置 item 的新的主轴方向长度
         (pcase direction
           ;; 关键点:
           ;; 1. basis units 是 item block 总长度
           ;; 2. item block 的 width/height 属性是内容的长度
           ((or 'row 'row-reverse)
            (oset item width
                  (list (- final-units
                           (etaf-flex-item-side-pixel item)))))
           ((or 'column 'column-reverse)
            (oset item height
                  (- final-units
                     (etaf-flex-item-side-height item)))))))
     grows)))

(defun etaf-flex-items-shrink (items-plists flex-units gaps-units direction)
  "按照 shrink 缩减并设置子项长度"
  (let* ((items (etaf-plists-get items-plists :item))
         (items-units-lst (etaf-plists-get items-plists :base-units))
         (items-units (apply #'+ items-units-lst))
         ;; shrink 后不能小于 最小宽度 或 最小高度(1)
         (rest-units (abs (- flex-units items-units gaps-units)))
         (min-units-lst (etaf-plists-get items-plists :min-units))
         (shrinks (etaf-plists-get items-plists :shrink))
         (shrinks-sum (apply '+ shrinks))
         (average-shrink (if (> shrinks-sum 0)
                             (/ rest-units shrinks-sum)
                           0))
         (rest-num (if (> shrinks-sum 0)
                       (% rest-units shrinks-sum)
                     0))
         (rest-idx 0))
    ;; (elog-debug "rest-units:%S" rest-units)
    (seq-map-indexed
     (lambda (shrink idx)
       (let* ((base-units (nth idx items-units-lst))
              (item (nth idx items))
              (min-units (nth idx min-units-lst))
              final-units)
         (if (= shrink 0)
             ;; shrink=0 不缩减
             (setq final-units base-units)
           ;; shrink>0 按比例缩减，但不能小于最小长度
           (setq final-units
                 ;; FIXME: 当缩减后小于最小长度时，
                 ;; 其余 items 重新分配这未能被缩减的长度
                 (max min-units
                      (- (- base-units (* shrink average-shrink))
                         (if (< rest-idx rest-num) 1 0))))
           (cl-incf rest-idx 1))
         ;; (elog-debug "shrink:%s" final-units)
         (pcase direction
           ;; 关键点:
           ;; 1. basis units 是 item block 总长度
           ;; 2. item block 的 width/height 属性是内容的长度
           ((or 'row 'row-reverse)
            (oset item width
                  (list (- final-units
                           (etaf-flex-item-side-pixel item)))))
           ((or 'column 'column-reverse)
            (oset item height
                  (- final-units
                     (etaf-flex-item-side-height item)))))))
     shrinks)))

(defun etaf-flex-main-gap-units (flex)
  "返回主轴方向的 gap 长度"
  (let ((direction (oref flex direction))
        (column-gap (oref flex column-gap))
        (row-gap (oref flex row-gap)))
    (pcase direction
      ((or 'row 'row-reverse) column-gap)
      ((or 'column 'column-reverse) row-gap))))

(defun etaf-flex-cross-gap-units (flex)
  "返回交叉轴方向的 gap 长度"
  (let ((direction (oref flex direction))
        (column-gap (oref flex column-gap))
        (row-gap (oref flex row-gap)))
    (pcase direction
      ((or 'row 'row-reverse) row-gap)
      ((or 'column 'column-reverse) column-gap))))

(defun etaf-flex-main-gaps-units (items-plists flex)
  "主轴方向的 gaps 的长度和，用于计算 grow 和 shrink"
  (let ((direction (oref flex direction))
        (column-gap (oref flex column-gap))
        (row-gap (oref flex row-gap)))
    (pcase direction
      ((or 'row 'row-reverse)
       (* (1- (length items-plists)) column-gap))
      ((or 'column 'column-reverse)
       (* (1- (length items-plists)) row-gap)))))

(defun etaf-flex-items-adjust (items-plists flex)
  "根据容器长度、items长度、shrink、grow 等动态调整子项长度。"
  (let* ((flex-units (etaf-flex-main-units flex))
         (direction (oref flex direction))
         (items-units
          (apply #'+ (etaf-plists-get items-plists :base-units)))
         (gaps-units (etaf-flex-main-gaps-units items-plists flex)))
    ;; (elog-debug "adjust flex-units:%S" flex-units)
    ;; (elog-debug "adjust items-units:%S" items-units)
    ;; 容器长度 >= 子项总长度: 拉伸
    (if (>= flex-units items-units)
        (etaf-flex-items-grow
         items-plists flex-units gaps-units direction)
      ;; 容器长度 < 子项总长度: 缩减
      (etaf-flex-items-shrink
       items-plists flex-units gaps-units direction))))

;; FIXME: need consider max?
(defun etaf-flex-main-units (flex)
  "Flex 容器的内容部分的主轴长度"
  (pcase (oref flex direction)
    ((or 'row 'row-reverse)
     (when-let ((width (oref flex width)))
       (etaf-width-pixel width)))
    ((or 'column 'column-reverse)
     (when-let ((height (oref flex height)))
       height))))

(defun etaf-flex-cross-units (flex)
  "Flex 容器的内容部分的交叉轴长度"
  (pcase (oref flex direction)
    ((or 'row 'row-reverse)
     (when-let ((height (oref flex height)))
       height))
    ((or 'column 'column-reverse)
     (when-let ((width (oref flex width)))
       (etaf-width-pixel width)))))

(defun etaf-flex-item-main-units (item direction)
  "获取单个 item 主轴方向的当前长度"
  (pcase direction
    ((or 'row 'row-reverse)
     (etaf-flex-item-total-pixel item))
    ((or 'column 'column-reverse)
     (etaf-flex-item-total-height item))))

(defun etaf-flex-items-main-units (items-plists flex)
  "获取当前左右子项最新的长度总和"
  (let ((direction (oref flex direction)))
    (apply #'+ (mapcar (lambda (item)
                         (etaf-flex-item-main-units item direction))
                       (etaf-plists-get items-plists :item)))))

(defun etaf-flex-items-cross-max-units (items flex)
  "交叉轴方向的子项最大长度"
  (pcase (oref flex direction)
    ((or 'row 'row-reverse)
     (seq-max (mapcar #'etaf-flex-item-total-height items)))
    ((or 'column 'column-reverse)
     (seq-max (mapcar #'etaf-flex-item-total-pixel items)))))

(defun etaf-flex-content-justify
    (items-num rest-units content-justify gap)
  (pcase content-justify
    ('flex-start (append (list 0)
                         (make-list (1- items-num) gap)
                         (list rest-units)))
    ('flex-end (append (list rest-units)
                       (make-list (1- items-num) gap)
                       (list 0)))
    ('center (append (list (/ rest-units 2))
                     (make-list (1- items-num) gap)
                     (list (+ (/ rest-units 2)
                              (% rest-units 2)))))
    ('space-between
     (append (list 0)
             (etaf-split-size rest-units (1- items-num) gap)
             (list 0)))
    ('space-around
     (let* ((around-units-lst
             (etaf-split-size rest-units (* 2 items-num)))
            (start-units (seq-first around-units-lst))
            (end-units (seq-drop around-units-lst (1- (* 2 items-num))))
            (middle-units-lst (seq-subseq around-units-lst
                                          1 (1- (* 2 items-num)))))
       (append (list start-units)
               ;; 合并中间的每2个，并加上 gap
               (mapcar (lambda (cons)
                         (+ gap (car cons) (or (cadr cons) 0)))
                       (seq-partition middle-units-lst 2))
               end-units)))
    ('space-evenly
     (etaf-split-size rest-units (1+ items-num) gap 1 items-num))))

(defun etaf-flex-content-align (items-num rest-units content-align gap)
  (pcase content-align
    ('flex-start (append (make-list items-num gap) (list rest-units)))
    ('flex-end (append (list rest-units) (make-list items-num gap)))
    ('center (append (list (/ rest-units 2))
                     (make-list (1- items-num) gap)
                     (list (+ (/ rest-units 2)
                              (% rest-units 2)))))
    ('space-between
     (append (list 0)
             (etaf-split-size rest-units (1- items-num) gap)
             (list 0)))
    ;; 区别是: 'stretch 会改变 item 在交叉轴的内容的长度，其余都不改变
    ('space-around
     (let* ((around-units-lst
             (etaf-split-size rest-units (* 2 items-num)))
            (start-units (seq-first around-units-lst))
            (end-units (seq-drop around-units-lst (1- (* 2 items-num))))
            (middle-units-lst (seq-subseq around-units-lst
                                          1 (1- (* 2 items-num)))))
       (append (list start-units)
               ;; 合并中间的每2个，并加上 gap
               (mapcar (lambda (cons)
                         (+ gap (car cons) (or (cadr cons) 0)))
                       (seq-partition middle-units-lst 2))
               end-units)))
    ('space-evenly
     (etaf-split-size rest-units (1+ items-num) gap 1 items-num))))

(defun etaf-flex-item-render (item)
  (let ((string (etaf-flex-item-string item)))
    (oset item content string)
    (etaf-box-string item)))

(defun etaf-flex-items-concat-single (items-plists
                                      main-gaps-lst
                                      cross-items-pads-lst)
  "在水平方向从左到右依次连接 items 和 gaps，同时根据交叉轴方向的对齐方式，为每个
item 补齐高度。
items-plists, main-gaps-lst 和 cross-items-pads-lst 当个主轴方向的。"
  (let* ((items (etaf-plists-get items-plists :item))
         (items-strings
          (seq-map-indexed
           ;; 处理每个 items 在 交叉轴方向的 pads
           (lambda (item idx)
             ;; (elog-debug "%S" item)
             ;; FIXME: 使用 etaf-box-string 多态派发函数
             (let* ((string (etaf-flex-item-render item))
                    (pads (nth idx cross-items-pads-lst))
                    (head-units (car pads))
                    (tail-units (cdr pads)))
               (etaf-lines-stack
                (list
                 (when (> head-units 0)
                   (etaf-string-duplines "" head-units))
                 string
                 (when (> tail-units 0)
                   (etaf-string-duplines "" tail-units))))))
           items)))
    ;; (elog-debug "main-gaps-lst:%S" main-gaps-lst)
    ;; (elog-debug "concat items-strings:%S" items-strings)
    (etaf-lines-concat
     (etaf-interleave
      (mapcar #'etaf-pixel-spacing main-gaps-lst)
      items-strings))))

(defun etaf-flex-items-stack-single (items-plists
                                     main-gaps-lst
                                     cross-items-pads-lst)
  "在垂直方向连接 items 和 gaps，同时根据交叉轴方向的对齐方式，
为每个item 补齐高度。
items-plists, main-gaps-lst 和 cross-items-pads-lst 单个主轴方向的。"
  (let* ((items (etaf-plists-get items-plists :item))
         (items-strings
          (seq-map-indexed
           ;; 处理每个 items 在 交叉轴方向的 pads
           (lambda (item idx)
             ;; FIXME: 使用 etaf-box-string 多态派发函数
             (let* ((string (etaf-flex-item-render item))
                    (pads (nth idx cross-items-pads-lst))
                    (head-units (car pads))
                    (tail-units (cdr pads)))
               (etaf-lines-concat
                (list (etaf-pixel-spacing head-units)
                      string
                      (etaf-pixel-spacing tail-units)))))
           items)))
    ;; (elog-debug "stack items-strings:%S" items-strings)
    (etaf-lines-stack
     (etaf-interleave
      (mapcar (lambda (gaps)
                (when (> gaps 0)
                  (etaf-string-duplines "" gaps)))
              main-gaps-lst)
      items-strings))))

(defun etaf-flex-rows-string (direction
                              items-plists-lst
                              cross-items-pads-lst
                              main-gaps-lst
                              cross-gaps-lst)
  "连接多个主轴方向的 items。direction 为 row 或 row-reverse 时使用。"
  (etaf-lines-stack
   (etaf-interleave
    (mapcar (lambda (gaps)
              (when (> gaps 0)
                (etaf-string-duplines "" gaps)))
            cross-gaps-lst)
    (seq-map-indexed
     (lambda (items-plists idx)
       (let ((cross-items-pads (nth idx cross-items-pads-lst))
             (main-gaps (nth idx main-gaps-lst)))
         (when (eq direction 'row-reverse)
           (setq items-plists (nreverse items-plists))
           (setq main-gaps (nreverse main-gaps))
           (setq cross-items-pads (nreverse cross-items-pads)))
         (etaf-flex-items-concat-single
          items-plists main-gaps cross-items-pads)))
     items-plists-lst))))

(defun etaf-flex-columns-string (direction
                                 items-plists-lst
                                 cross-items-pads-lst
                                 main-gaps-lst
                                 cross-gaps-lst)
  "连接多个主轴方向连的 items。direction 为 column 或 column-reverse 时使用。"
  (etaf-lines-concat
   (etaf-interleave
    (mapcar #'etaf-pixel-spacing cross-gaps-lst)
    (seq-map-indexed
     (lambda (items-plists idx)
       (let ((cross-items-pads (nth idx cross-items-pads-lst))
             (main-gaps (nth idx main-gaps-lst)))
         (when (eq direction 'column-reverse)
           (setq items-plists (nreverse items-plists))
           (setq main-gaps (nreverse main-gaps))
           (setq cross-items-pads (nreverse cross-items-pads)))
         (etaf-flex-items-stack-single
          items-plists main-gaps cross-items-pads)))
     items-plists-lst))))

(defun etaf-flex-items-plist (flex)
  (mapcar (lambda (item)
            (let ((lst (etaf-flex-parse-basis item flex)))
              (list :item item
                    :base-units (nth 0 lst)
                    :min-units (nth 1 lst)
                    :max-units (nth 2 lst)
                    :order (oref item order)
                    :grow (oref item grow)
                    :shrink (oref item shrink)
                    :align (oref item cross-align))))
          (oref flex content)))

(defun etaf-item-set-cross-units (item total-units direction)
  "设置 item 交叉轴的长度"
  (pcase direction
    ((or 'row 'row-reverse)
     (oset item height
           (- total-units (etaf-flex-item-side-height item))))
    ((or 'column 'column-reverse)
     (oset item width
           (list (- total-units (etaf-flex-item-side-pixel item)))))))

(defun etaf-flex-string (flex)
  (let* ((direction (oref flex direction))
         (items-align (oref flex items-align))
         (old-items (oref flex content))
         ;; 根据 order 重新排列 items
         (items (seq-sort (lambda (item1 item2)
                            (< (oref item1 order)
                               (oref item2 order)))
                          old-items))
         (items-plists (etaf-flex-items-plist flex))
         (items-units-lst (etaf-plists-get
                           items-plists :base-units))
         ;; (_ (elog-debug "base: %S" items-units-lst))
         ;; (_ (elog-debug "min: %S" (etaf-plists-get items-plists :min-units)))
         ;; (_ (elog-debug "max: %S" (etaf-plists-get items-plists :max-units)))
         ;; 当前使用 base-units 计算初始的总宽度
         ;; 后续动态调整中更多的使用实时计算出来的 items 总宽度
         (items-units (apply '+ items-units-lst))
         ;; 用于记录每行的 items 的数量
         (items-num (length items-units-lst))
         (wrap-lst (list items-num))
         (main-gap-units (etaf-flex-main-gap-units flex))
         (cross-gap-units (etaf-flex-cross-gap-units flex))
         ;; 设置默认值
         (items-plists-lst (list items-plists))
         (main-gaps-lst
          (list (append (list 0)
                        (make-list (1- items-num) main-gap-units)
                        (list 0))))
         (cross-gaps-lst (list 0 0))
         (cross-items-pads-lst (list (make-list items-num (cons 0 0))))
         ;; items-plists-lst
         ;; main-gaps-lst
         ;; cross-gaps-lst
         ;; cross-items-pads-lst
         cross-max-units-lst)
    (if-let*
        ;; flex 容器设置了主轴方向的长度
        ((flex-units (etaf-flex-main-units flex))
         (gaps-units (etaf-flex-main-gaps-units items-plists flex))
         (rest-units (- flex-units items-units gaps-units)))
        (progn
          ;; (elog-debug "rest-units:%S" rest-units)
          (if (>= rest-units 0)
              ;; 容器长度 >= items总长度，考虑 grow，不换行
              (etaf-flex-items-grow
               items-plists flex-units gaps-units direction)
            ;; 容器长度 < items总长度，考虑 shrink 和 换行
            (etaf-flex-items-shrink
             items-plists flex-units gaps-units direction)

            ;; 缩减之后非 nowrap，items 个数大于1且超过容器长度的
            ;; 多个 items 换行后，重新计算
            (when (and (not (eq 'nowarp (oref flex wrap)))
                       (> (length items-plists) 1))
              (let ((items-units
                     (etaf-flex-items-main-units items-plists flex)))
                (when (> items-units flex-units)
                  (setq wrap-lst (etaf-flex-line-breaks
                                  flex-units items-units-lst
                                  (etaf-flex-main-gap-units flex)))
                  ;; (elog-debug "wrap-lst:%S" wrap-lst)
                  (let ((prev 0))
                    (dolist (num wrap-lst)
                      ;; 当前主轴方向的新的 items
                      (let ((sub-items-plists
                             (seq-subseq items-plists
                                         prev (+ prev num))))
                        ;; 重新计算每行的 item 长度
                        (etaf-flex-items-adjust sub-items-plists flex)
                        (setq prev (+ prev num)))))))))
          ;; 依次处理每一条主轴上的 content-justify
          ;; 设置 main-gaps-lst 和 cross-max-units-lst
          (let ((prev 0)
                (content-justify (oref flex content-justify)))
            (setq main-gaps-lst nil)
            (dolist (num wrap-lst)
              ;; 当前主轴方向的新的 items
              (let* ((sub-items-plists
                      (seq-subseq items-plists
                                  prev (+ prev num)))
                     (items (etaf-plists-get sub-items-plists :item))
                     (items-num (length sub-items-plists))
                     (flex-units (etaf-flex-main-units flex))
                     (items-units (etaf-flex-items-main-units
                                   sub-items-plists flex))
                     (gaps-units (etaf-flex-main-gaps-units
                                  sub-items-plists flex))
                     (rest-units (- flex-units items-units gaps-units))
                     (gap-units (etaf-flex-main-gap-units flex)))
                ;; (elog-debug "flex-units:%S" flex-units)
                ;; (elog-debug "gap-units:%S" gap-units)
                ;; (elog-debug "gaps-units:%S" gaps-units)
                ;; (elog-debug "items-units:%S" items-units)
                ;; (elog-debug "rest-units:%S" rest-units)
                ;; 交叉轴方向的子项最大长度，用于后续设置 content-align
                (push (etaf-flex-items-cross-max-units items flex)
                      cross-max-units-lst)
                (if (> rest-units 0)
                    ;; 主轴方向有剩余空间时，才考虑 content-justify
                    (push (etaf-flex-content-justify
                           items-num rest-units content-justify
                           gap-units)
                          main-gaps-lst)
                  (push (append
                         '(0) (make-list (1- items-num) gap-units) '(0))
                        main-gaps-lst))
                ;; (elog-debug "main-gaps-lst:%S" main-gaps-lst)
                ;; (elog-debug "items-units:%S" items-units)
                (setq prev (+ prev num))))
            (setq main-gaps-lst (nreverse main-gaps-lst))
            (setq cross-max-units-lst (nreverse cross-max-units-lst)))
          ;; consider cross-align 和 items-align
          ;; 设置 cross-items-pads-lst 和 items-plists-lst
          ;; 重置默认值为 nil
          (setq items-plists-lst nil)
          (setq cross-items-pads-lst nil)
          (let ((prev 0) (idx 0))
            (dolist (num wrap-lst)
              (let* ((sub-items-plists
                      (seq-subseq items-plists
                                  prev (+ prev num)))
                     (items (etaf-plists-get sub-items-plists :item))
                     (items-num (length sub-items-plists))
                     ;; 单个主轴在交叉轴的最大长度，
                     ;; 用于计算 items-align, cross-align
                     (items-cross-max-units (nth idx cross-max-units-lst)))
                (push sub-items-plists items-plists-lst)
                ;; consider cross-align
                ;; 返回 cons-cell，用于 pad 在 item 开头和结尾
                (push (mapcar (lambda (item)
                                (etaf-flex--cross-edge-units
                                 item items-cross-max-units flex))
                              items)
                      cross-items-pads-lst)
                (setq prev (+ prev num))
                (cl-incf idx 1)))
            (setq items-plists-lst (nreverse items-plists-lst))
            (setq cross-items-pads-lst (nreverse cross-items-pads-lst)))
          ;; consider content-align
          (let* ((main-num (length wrap-lst))
                 ;; main-num 是主轴的个数
                 (gap-units (etaf-flex-cross-gap-units flex))
                 ;; items 和 gaps 在交叉轴的总长度
                 (cross-content-units (+ (apply '+ cross-max-units-lst)
                                         (* gap-units (1- main-num))))
                 (content-align (oref flex content-align))
                 (cross-flex-units (etaf-flex-cross-units flex)))
            ;; (elog-debug "cross-flex-units:%S" cross-flex-units)
            ;; (elog-debug "cross-content-units:%S" cross-content-units)
            ;; wrap 不是 nowrap, 子项分布在多行(列),
            ;; 且容器在交叉轴方向必须有额外空间​，才考虑 content-align

            ;; content-align 为 'stretch 时
            ;; 需要重新设置每个主轴上的所有 items 的交叉轴长度为最大长度
            (if cross-flex-units
                (if (eq content-align 'stretch)
                    (let* ((prev 0) (idx 0)
                           (rest-units
                            (- cross-flex-units cross-content-units))
                           (num (length wrap-lst))
                           (plus-lst (etaf-split-size rest-units num)))
                      (dolist (num wrap-lst)
                        (let* ((sub-items-plists
                                (seq-subseq items-plists prev (+ prev num)))
                               (items (etaf-plists-get
                                       sub-items-plists :item))
                               (cross-max-units (nth idx cross-max-units-lst))
                               (plus (nth idx plus-lst)))
                          (mapcar (lambda (item)
                                    (etaf-item-set-cross-units
                                     ;; stretch 表示加上额外的长度
                                     item (+ plus cross-max-units) direction))
                                  items)
                          (setq prev (+ prev num))
                          (cl-incf idx 1)))
                      (setq cross-gaps-lst
                            (append (list 0)
                                    (make-list (1- main-num) gap-units)
                                    (list 0))))
                  ;; 其余 content-align 值，只计算主轴之间的 gap
                  (if (and (not (eq 'nowarp (oref flex wrap)))
                           (> main-num 1)
                           (< cross-content-units cross-flex-units))
                      (setq cross-gaps-lst
                            (etaf-flex-content-align
                             main-num
                             (- cross-flex-units cross-content-units)
                             content-align gap-units))
                    ;; 只设置基础 gaps
                    (setq cross-gaps-lst
                          (append (list 0)
                                  (make-list (1- main-num) gap-units)
                                  (list 0)))))
              ;; flex 没有设置交叉轴方向的长度，不考虑 content-align，只设置基础 gaps
              (setq cross-gaps-lst
                    (append (list 0)
                            (make-list (1- main-num) gap-units)
                            (list 0))))))
      ;; else:
      ;; flex 容器未设置主轴方向的长度，items 不换行
      ;; grow=0 不拉伸；grow>0 拉伸值最大宽度 (min max-width max-content)
      (let ((grows (etaf-plists-get items-plists :grow))
            final-units)
        (seq-map-indexed
         (lambda (grow idx)
           (let ((item (nth idx items))
                 (items-plist (nth idx items-plists)))
             (if (= grow 0)
                 ;; 不拉伸，设置为 base-units
                 (setq final-units (plist-get items-plist :base-units))
               ;; 拉伸到 (min max-width max-content)
               (setq final-units (plist-get items-plist :max-units)))
             (pcase direction
               ((or 'row 'row-reverse)
                (oset item width
                      (list (- final-units
                               (etaf-flex-item-side-pixel item)))))
               ((or 'column 'column-reverse)
                (oset item height
                      (- final-units
                         (etaf-flex-item-side-height item)))))))
         grows)
        ;; 设置最后连接需要用到的变量
        (setq items-plists-lst (list items-plists))
        (setq main-gaps-lst
              (list (append (list 0)
                            (make-list (1- items-num) main-gap-units)
                            (list 0))))
        (setq cross-gaps-lst (list 0 0))
        (setq cross-items-pads-lst
              (list (make-list items-num (cons 0 0))))))
    
    ;; 最后连接 items block 时，考虑 direction, wrap 的方向
    ;; main-gaps-lst: 每个主轴方向的 items 的 gap
    ;; cross-gaps-lst: 交叉轴方向的主轴之间的 gap
    ;; cross-items-pads-lst: 每个主轴方向的 items 在交叉轴方向的 pads
    (when (eq (oref flex wrap) 'wrap-reverse)
      ;; 下面的每个都是 list 的 list，外层的 list 表示多个主轴方向的数据
      (setq items-plists-lst (nreverse items-plists-lst))
      (setq cross-items-pads-lst (nreverse cross-items-pads-lst))
      (setq main-gaps-lst (nreverse main-gaps-lst))
      (setq cross-gaps-lst (nreverse cross-gaps-lst)))
    
    ;; (elog-debug "items-plists-lst:%S" items-plists-lst)
    ;; (elog-debug "items:%S" (etaf-plists-get (nth 0 items-plists-lst)
    ;;                                         :item))
    ;; (elog-debug "cross-items-pads-lst:%S" cross-items-pads-lst)
    ;; (elog-debug "main-gaps-lst:%S" main-gaps-lst)
    ;; (elog-debug "cross-gaps-lst:%S" cross-gaps-lst)
    (let ((content
           (pcase direction
             ((or 'row 'row-reverse)
              (etaf-flex-rows-string direction items-plists-lst
                                     cross-items-pads-lst main-gaps-lst
                                     cross-gaps-lst))
             ((or 'column 'column-reverse)
              (etaf-flex-columns-string direction items-plists-lst
                                        cross-items-pads-lst main-gaps-lst
                                        cross-gaps-lst)))))
      (let ((flex-content (oref flex content))
            (string (progn (oset flex content content)
                           (etaf-box-string flex))))
        ;; 还原原有的 content
        (oset flex content flex-content)
        string))))

(provide 'etaf-flex)
