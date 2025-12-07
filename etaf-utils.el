;; -*- lexical-binding: t -*-

(require 'etaf-pixel)
(require 'cl-lib)

(defun etaf-pop-to-buffer (buffer-or-name &optional action norecord)
  (pop-to-buffer buffer-or-name action norecord)
  (local-set-key "q" (lambda ()
                       (interactive)
                       (local-unset-key "q")
                       (quit-window))))

(defun etaf-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun etaf-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defvar etaf-theme-background-change-hook nil
  "Normal hook that is run after the background of theme changed.")

(defun etaf-change-theme-background (orig-fun &rest args)
  "Advice functon when load a theme."
  (let ((before-bg (frame-parameter nil 'background-mode))
        after-bg)
    (apply orig-fun args)
    (setq after-bg (frame-parameter nil 'background-mode))
    (unless (eq before-bg after-bg)
      (run-hooks 'etaf-theme-background-change-hook))))

;;; use the following advice to detect theme background change.
;; (advice-add #'load-theme :around #'etaf-change-theme-background)

;; Local replacements for dash functions to avoid external dependency
(defun etaf-utils--interleave (list1 list2)
  "Interleave elements of LIST1 and LIST2.
Returns a list with alternating elements from LIST1 and LIST2.
The returned list has length (* 2 (min (length LIST1) (length LIST2))).
Example: (etaf-utils--interleave '(a b c) '(1 2 3)) => (a 1 b 2 c 3)"
  (let (result)
    (while (and list1 list2)
      (push (pop list1) result)
      (push (pop list2) result))
    (nreverse result)))

(defun etaf-utils--max (list)
  "Return the maximum value in LIST of numbers.
Returns nil if LIST is empty."
  (when list
    (apply #'max list)))

(defun etaf-keyword->symbol (keyword)
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defmacro etaf-alist-set (alist key value)
  `(setf (alist-get ,key ,alist) ,value))

(defun etaf-region-replace (string start end)
  "把当前 buffer start 到 end 位置的文本提环为 string"
  (goto-char start)
  (delete-region start end)
  (insert string))

(defun etaf-region-swap (region1 region2)
  "交换 region1 和 region2 的文本，
region 的格式是 cons-cell (start . end)
region1 和 region2 不允许有交叉范围 且 region1 在 region2 前面"
  (let* ((start1 (car region1))
         (end1 (cdr region1))
         (str1 (buffer-substring start1 end1))
         (start2 (car region2))
         (end2 (cdr region2))
         (str2 (buffer-substring start2 end2)))
    ;; 优先替换更靠后的 region，不会影响前面 region 的座标。
    (etaf-region-replace str1 start2 end2)
    (etaf-region-replace str2 start1 end1)))

(defun etaf-property-search-forward
    (function property &optional value predicate)
  "从当前位置向前搜索，并执行 function"
  (when-let* ((match (text-property-search-forward
                      property value predicate))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (value (prop-match-value match)))
    (funcall function start end value)))

(defun etaf-property-forward-region
    (property &optional value predicate)
  "从当前位置向前搜索，并执行 function"
  (etaf-property-search-forward
   (lambda (start end _) (cons start end))
   property value predicate))

(defun etaf-property-forward-value
    (property &optional value predicate)
  "从当前位置向前搜索，并执行 function"
  (etaf-property-search-forward
   (lambda (_ _ value) value)
   property value predicate))

(defun etaf-property-search-backward
    (function property &optional value predicate)
  "从当前位置向后搜索，并执行 function"
  (when-let* ((match (text-property-search-backward
                      property value predicate))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (value (prop-match-value match)))
    (funcall function start end value)))

(defun etaf-property-backward-region
    (property &optional value predicate)
  "从当前位置向前搜索，并执行 function"
  (etaf-property-search-backward
   (lambda (start end _) (cons start end))
   property value predicate))

(defun etaf-property-backward-value
    (property &optional value predicate)
  "从当前位置向前搜索，并执行 function"
  (etaf-property-search-backward
   (lambda (_ _ value) value)
   property value predicate))

(defun etaf-property-map-regions
    (function property &optional value predicate collect)
  "对属性匹配的开头和结尾 point 执行 function。collect 为 t 时返回结果列表"
  (save-excursion
    (goto-char (point-min))
    (let ((idx 0) lst)
      (while-let ((match (text-property-search-forward
                          property value predicate))
                  (start (prop-match-beginning match))
                  (end (prop-match-end match)))
        (let ((res (funcall function start end idx)))
          (when collect (push res lst)))
        (cl-incf idx 1))
      (nreverse lst))))

(defun etaf-property-map-strings
    (function property &optional value predicate collect)
  "对属性匹配的字符串执行 function"
  (etaf-property-map-regions
   (lambda (start end idx)
     (funcall function (buffer-substring start end) idx))
   property value predicate))

(defun etaf-symbol->keyword (symbol)
  (etaf-string-to-keyword (symbol-name symbol)))

(defun etaf-string-to-keyword (string)
  (intern (concat ":" string)))

(defun etaf-input-text (string length)
  (if (> (length string) length)
      (substring string 0 length)
    (string-pad string length)))

(defun etaf-value (format)
  (pcase format
    ((and (pred listp)
          (let head (car format))
          (guard (or (functionp head)
                     ;; (eq 'lambda head)
                     )))
     (eval format)
     ;; (apply format)
     )
    ((pred symbolp) (symbol-value format))
    (_ format)))

(defun etaf-alist->plist (alist)
  (mapcan (lambda (elem)
            (list (car elem) (cdr elem)))
          alist))

(defun etaf-plist->alist (plist)
  (unless (null plist)
    (append
     (list (cons (pop plist) (pop plist)))
     (etaf-plist->alist plist))))

(defun etaf-plist-remove-keys (plist1 keys)
  "Remove all element of PLIST2 from PLIST1."
  (let ((alist1 (etaf-plist->alist plist1)))
    (etaf-alist->plist
     (seq-remove (lambda (elem)
                   (member (car elem) keys))
                 alist1))))

(defun etaf-oset (object &rest kvs)
  (let ((alist (etaf-plist->alist kvs)))
    (dolist (kv alist)
      (eval `(oset ,object ,(car kv) ',(cdr kv))))))

(defun etaf-get-text-properties (str)
  "获取字符串STR的所有文本属性区间。
返回值为列表，每个元素为 (START END PROPERTIES)，其中
START和END为区间的起始和结束位置，PROPERTIES为该区间的属性。"
  (let ((len (length str))
        (pos 0) ranges)
    (while (< pos len)
      (let* ((props (text-properties-at pos str))
             (next-pos (next-property-change pos str len)))
        (when props
          (push (list pos next-pos props) ranges))
        (setq pos next-pos)))
    (nreverse ranges)))

(defun etaf-get-ov-faces (string)
  (seq-remove
   'null
   (mapcar (lambda (data)
             (when-let* ((properties (nth 2 data))
                         (ov-face (plist-get properties 'ov-face)))
               (setf (nth 2 data) ov-face)
               data))
           (etaf-get-text-properties string))))

(defun etaf-make-region-editable (regions)
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (point-max) 'read-only t)
    (dolist (cons regions)
      (remove-text-properties (car cons) (cdr cons)
                              '(read-only nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etaf-interleave (seq1 seq2)
  "交叉组合 seq1 和 seq2，seq1 的元素个数必须比 seq2 多一个。"
  (if (= (length seq1) (1+ (length seq2)))
      (let ((head-seq1 (seq-take seq1 (1- (length seq1))))
            (last-elem (car (last seq1))))
        (append (etaf-utils--interleave head-seq1 seq2) (list last-elem)))
    (error "(length %S) != (1+ (length %S))" seq1 seq2)))

(defun etaf-split-size (size n &optional extra start end from-tail)
  "将 SIZE 分为 N 等份，并且在 start 到 end 位置加上额外的数字 extra.
FROM-TAIL 为 t 表示优先从尾部加上多余的部分。"
  (let ((each-size (/ size n))
        (rest-num (% size n)))
    (seq-map-indexed (lambda (size idx)
                       (let ((s (if (if from-tail
                                        (>= idx (- n rest-num))
                                      (< idx rest-num))
                                    (1+ size)
                                  size))
                             (start (or start 0))
                             (end (or end n)))
                         (if (and extra (<= start idx) (> end idx))
                             (+ extra s)
                           s)))
                     (make-list n each-size))))

(defun etaf-plists-get (plist-seq prop)
  (mapcar (lambda (plst)
            (plist-get plst prop))
          plist-seq))

(defun etaf-background-type ()
  (frame-parameter nil 'background-mode))

(defun etaf-atom-consp (obj)
  "只有两个元素的 cons 类型判断"
  (and (consp obj)
       (cdr obj)
       (atom (cdr obj))
       (atom (car obj))))

(defun etaf-propertize (string properties
                               &optional start end)
  "不会覆盖原有的属性，返回新的字符串。"
  ;; 防止是 make-list 创建的元素，它们都属于同一个对象
  ;; 最好先复制一份字符串
  (let* ((string (copy-sequence string))
         (start (or start 0))
         (end (or end (length string))))
    (while properties
      (let ((prop (pop properties))
            (value (pop properties)))
        (pcase prop
          ('face (add-face-text-property
                  start end value t string))
          ('display (add-display-text-property
                     start end (car value) (cadr value)
                     string))
          (_ (put-text-property
              start end prop value string)))))
    string))

(defun etaf-propertize-bgcolor (string bgcolor)
  "Apply background color BGCOLOR to STRING, handling display properties.

This function properly applies background color to characters that have
display properties like (space :width ...) by updating the display spec
to include the face with background color.

For regular text, `add-face-text-property' is used.
For text with `(space :width ...)' display properties, the display spec
is updated to include `:face (:background BGCOLOR)'.

Example:
  Input: a string containing \" \" with display property (space :width (10))
  Output: same string with display property (space :width (10) :face (:background BGCOLOR))"
  (let* ((string (copy-sequence string))
         (len (length string))
         (pos 0))
    (while (< pos len)
      (let* ((display (get-text-property pos 'display string))
             (next-pos (or (next-single-property-change pos 'display string len) len)))
        (if (and display
                 (consp display)
                 (eq (car display) 'space))
            ;; For display property with (space ...), add :face with background
            (let* ((existing-face (plist-get (cdr display) :face))
                   (new-face (cond
                              ;; Existing face already has background, keep it
                              ((and existing-face
                                    (consp existing-face)
                                    (plist-get existing-face :background))
                               existing-face)
                              ;; Existing face is a plist, add background to it
                              ((and existing-face (consp existing-face))
                               (plist-put (copy-sequence existing-face)
                                          :background bgcolor))
                              ;; Existing face is a symbol, inherit from it
                              (existing-face
                               `(:inherit ,existing-face :background ,bgcolor))
                              ;; No existing face, create new one
                              (t
                               `(:background ,bgcolor))))
                   (new-display (append (list 'space)
                                        (plist-put (copy-sequence (cdr display))
                                                   :face new-face))))
              (put-text-property pos next-pos 'display new-display string))
          ;; For regular text, use add-face-text-property
          (add-face-text-property pos next-pos `(:background ,bgcolor) t string))
        (setq pos next-pos)))
    string))

(defun etaf-add-face-text-property (start end face)
  "已有的 face 更新，没有的 face 增加"
  ;; (add-face-text-property)
  )

(defun etaf-propertize-underline (string &optional color style)
  (etaf-propertize
   string
   `(face (:underline
           ( :position t
             :color ,(or color (face-attribute
                                'default :foreground))
             ,@(when style `(:style ,style)))))))

(defun etaf-propertize-overline (string &optional color)
  (etaf-propertize
   string `(face (:overline
                  ,(or color (face-attribute 'default :foreground))))))

(defun etaf-remove-face-attributes (start end removed-attributes)
  "将 scroll bar 的 face 清空，变为像素空格"
  ;; 局限性: 用 start 的 face 属性处理之后，渲染到所有字符上面
  ;; 因此只能来处理取消 scroll bar 的属性
  ;; 先取出 face properties
  (let ((face-properties (get-text-property start 'face))
        not-kv-faces)
    (when (consp face-properties)
      (let ((face-properties (seq-mapcat (lambda (elem)
                                           (if (consp elem)
                                               elem
                                             (list elem)))
                                         face-properties)))
        ;; face-properties 开头可能会有非 kv 形式的face，需要保留
        (catch 'break
          (while-let ((not-kv-face (car face-properties)))
            (if (not (keywordp not-kv-face))
                (progn
                  (when (facep not-kv-face)
                    (push not-kv-face not-kv-faces))
                  (pop face-properties))
              (throw 'break nil))))
        ;; 剩下的是一个 plist
        (dolist (attribute removed-attributes)
          (plist-put face-properties attribute nil))
        ;; 合并 not-kv-faces 和 kv-faces
        (add-text-properties
         start end
         `(face ,(append not-kv-faces face-properties)))))))

(defun etaf-width-pixel (width &optional content)
  "Return the pixel of etaf width. If WIDTH is a cons-cell,
 use the car of it as pixel. If WIDTH is a integer, use the
 pixel width number of chars in CONTENT as pixel. If the number
 of chars in CONTENT is less than WIDTH, use pixel of space to
 pad the rest width."
  (cond
   ((consp width) (car width))
   ((numberp width)
    (if content
        (or (etaf-string-nchar-pixel content width)
            (+ (string-pixel-width content)
               ;; 内容小于 width 个字符时，用空格补齐剩余字符计算像素
               (* (- width (length content))
                  (string-pixel-width " "))))
      (* width (string-pixel-width " "))))
   ;; (t (error "Invalid format of width %S" width))
   ))

(defun etaf-default-border-color ()
  (frame-parameter nil 'foreground-color))

(defun etaf-pixel-border (n-pixel height &optional color)
  (unless (or (= 0 n-pixel) (= 0 height))
    (let* ((color (if (eq t color)
                      (etaf-default-border-color)
                    (or color (etaf-default-border-color))))
           (string (propertize
                    " " 'face
                    `(:inverse-video t :foreground ,color)
                    'display `(space :width (,n-pixel)))))
      (etaf-string-duplines string height))))

(defun etaf--num-in-prefixs (num prefixs)
  "Find the position of the smallest number in PREFIXS
 that is greater than NUM. Position counts from 1.
If NUM is greater than all elements, return (1+ (length PREFIXS))."
  (when prefixs
    (cond
     ((= num 0) 0)
     ((> num (car (last prefixs))) (length prefixs))
     (t (let ((pos 1))
          (while (and prefixs (< (car prefixs) num))
            (setq prefixs (cdr prefixs))
            (setq pos (1+ pos)))
          pos)))))

(defun etaf--num-in-lst (num lst)
  "计算 NUM 落在 LST 中的第几个元素的前缀和区间内。"
  (let* ((prefix 0)
         ;; 计算前缀和
         (prefixs (mapcar (lambda (n)
                            (setq prefix (+ prefix n)))
                          lst)))
    (etaf--num-in-prefixs num prefixs)))

(defun etaf-flex-line-breaks (flex-units items-units-lst gap)
  "计算 FLEX 布局中每行的元素个数，考虑项目之间的固定间隙。
FLEX-UNITS 是容器最大宽度，ITEMS-UNITS-LST 是项目宽度列表，
GAP 是项目之间的固定间隙（单位与项目宽度相同）。
返回每行元素个数的列表（按行顺序排列）。"
  (let ((current-sum 0)       ; 当前行累计宽度
        (current-count 0)     ; 当前行项目计数
        (result '())           ; 结果列表（逆序存储）
        (index 0)              ; 当前处理的项目索引
        (len (length items-units-lst)))
    (while (< index len)
      (let ((current-width (nth index items-units-lst)))
        ;; 处理超大项目（宽度 >= 容器宽度）
        (if (>= current-width flex-units)
            (progn
              ;; 结束当前行（如果有项目）
              (when (> current-count 0)
                (push current-count result)
                (setq current-sum 0
                      current-count 0))
              ;; 超大项目单独成行
              (push 1 result)
              (cl-incf index))  ; 移动到下一个项目
          ;; 常规项目处理
          (let ((required-width
                 (if (> current-count 0)
                     (+ current-sum gap current-width) ; 非首项需加间隙
                   current-width))) ; 首项不加间隙
            ;; 检查是否能加入当前行
            (if (<= required-width flex-units)
                (progn
                  (setq current-sum required-width)
                  (cl-incf current-count)
                  (cl-incf index))  ; 移动到下一个项目
              ;; 无法加入当前行（除非是空行）
              (if (> current-count 0)
                  (progn
                    ;; 结束当前行（非空行）
                    (push current-count result)
                    (setq current-sum 0
                          current-count 0))
                ;; 空行特殊情况：即使单独项目也应能容纳（理论上不会触发）
                (setq current-sum current-width
                      current-count 1
                      index (1+ index))))))))
    ;; 处理最后一行（如果有未记录的项目）
    (when (> current-count 0)
      (push current-count result))
    ;; 返回正确顺序的结果（反转结果列表）
    (reverse result)))

(defun etaf-string-nchar-pixel (string n &optional from-end)
  "Return the pixel width of n char in STRING from start
by default. If FROM-END is non-nil, count from the end."
  (let ((strlen (length string)))
    (unless (< strlen n)
      (if from-end
          (string-pixel-width (substring string (- strlen n)))
        (string-pixel-width (substring string 0 n))))))

(defun etaf-pixel-blank (pixel height)
  (unless (or (= pixel 0) (= height 0))
    (etaf-string-duplines
     (etaf-pixel-spacing pixel) height)))

(defun etaf-string-linum (string &optional omit-nulls)
  (length (split-string string "\n" omit-nulls)))

(defun etaf-string-join (sequences)
  (string-join sequences "\n"))

(defun etaf-string-duplines (string length)
  (string-join (make-list length string) "\n"))

(defun etaf-lines-pad (string height &optional offset padstr)
  "Pad the string to a height of HEIGHT, with the string offset 
by OFFSET lines from the top, using PADSTR to fill blank lines."
  (let ((linum (etaf-string-linum string)))
    (if-let*
        (((>= height linum))
         (rest (- height linum))
         (top (etaf-line-offset rest (or offset 0)))
         (bottom (- rest top))
         (padstr (or padstr ""))
         (top-string
          (if (> top 0)
              (concat (etaf-string-duplines padstr top) "\n")
            ""))
         (bottom-string
          (if (> bottom 0)
              (concat "\n" (etaf-string-duplines padstr bottom))
            "")))
        (concat top-string string bottom-string)
      (error "height %s should not be less than linum of string %s"
             height linum))))

(defun etaf-string-concat (&rest strings)
  (let* ((height (etaf-utils--max (mapcar #'etaf-string-linum strings)))
         (strings (mapcar (lambda (string)
                            (etaf-lines-pad string height))
                          strings)))
    (etaf-string-join
     (apply 'cl-mapcar #'concat
            (mapcar (lambda (string)
                      (etaf-maplines 'identity string))
                    strings)))))

(defun etaf-map-collect (list map-func collect-func
                              &optional arglist)
  "Apply MAP-FUNC to LIST, then apply COLLECT-FUNC with ARGLIST
to the result of map function.

The first argument of COLLECT-FUNC function is a list and the rest
are ARGLIST."
  (apply collect-func (mapcar map-func list) (or arglist '())))

(defun etaf-maplines (function string &optional collect-func &rest arglist)
  "Apply FUNCTION to each line of STRING. If COLLECT-FUNC is non-nil,
apply this function to the list and ARGLIST are arguments of this
COLLECT-FUNC; otherwise return a list of lines."
  (let ((lines (split-string string "\n")))
    (if collect-func
        (etaf-map-collect lines function collect-func arglist)
      (mapcar function lines))))

(defun etaf-line-offset (total offset)
  "When OFFSET is a positive number, there is a offset pixel distance
from start. When OFFSET is a negative number, there is a OFFSET pixel
from the end. Return the offset from start."
  (cond ((>= offset 0) (min offset total))
        ((< offset 0) (max 0 (+ offset total)))))

(defun etaf-line-align-offset (total justify-or-align)
  "Return the offset when align with JUSTIFY-OR-ALIGN."
  (pcase justify-or-align
    ((or 'left 'top) (etaf-line-offset total 0))
    ((or 'right 'bottom) (etaf-line-offset total total))
    ('center (etaf-line-offset total (/ total 2)))
    (_ (error "Invalid format of align: %S" align))))

(defun etaf-lines-justify (string pixel &optional justify)
  "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
should be one of the symbols 'left', 'center', 'right'.
When JUSTIFY is nil, set it to 'left' by default."
  (let ((justify (or justify 'left)))
    (etaf-maplines (lambda (line)
                     (etaf-pixel-typeset line pixel justify))
                   string 'string-join "\n")))

(defun etaf-lines-align (string height &optional align)
  "垂直方向限制文本高度为 height，并根据 align 对齐"
  (let ((linum (etaf-string-linum string)))
    (if (< height linum)
        (string-join
         (seq-take (split-string string "\n" t) height)
         "\n")
      (etaf-lines-pad
       string height
       (etaf-line-align-offset
        (- height (etaf-string-linum string)) (or align 'top))
       (etaf-pixel-spacing (string-pixel-width string))))))

(defun etaf-lines-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (setq strings (delete nil strings))
  (let ((max-height (etaf-utils--max (mapcar #'etaf-string-linum strings))))
    (apply 'etaf-string-concat
           (mapcar (lambda (string)
                     (etaf-lines-justify
                      (etaf-lines-align string max-height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

(defun etaf-lines-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (setq strings (delete nil strings))
  (let ((max-width (etaf-utils--max (mapcar #'string-pixel-width strings))))
    (mapconcat (lambda (string)
                 (etaf-lines-justify
                  ;; 再 justify 整个 block
                  (etaf-lines-justify
                   ;; 先 justify 文本
                   string (string-pixel-width string) text-align)
                  max-width align))
               strings "\n")))

(defun etaf-frame-bgcolor ()
  (face-attribute 'default :background))

(provide 'etaf-utils)
