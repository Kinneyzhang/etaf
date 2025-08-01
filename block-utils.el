;; -*- lexical-binding: t -*-

(require 'block-pixel)

(defun block-background-type ()
  (frame-parameter nil 'background-mode))

(defun block-atom-consp (obj)
  "只有两个元素的 cons 类型判断"
  (and (consp obj)
       (cdr obj)
       (atom (cdr obj))))

(defun block-propertize (string properties
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

(defun block-width-pixel (width)
  (cond
   ((consp width) (car width))
   ((numberp width) (* width (string-pixel-width " ")))
   (t (error "Invalid format of width %S" width))))

(defun block-pixel-border (n-pixel height &optional color)
  (if (or (= 0 n-pixel) (= 0 height))
      ""
    (let* ((color (if (eq t color)
                      (block-default-border-color)
                    (or color (block-default-border-color))))
           (string (propertize
                    " " 'face
                    `(:inverse-video t :foreground ,color)
                    'display `(space :width (,n-pixel)))))
      (block-string-duplines string height))))

(defun block-string-nchar-pixel (string n &optional from-end)
  "Return the pixel width of n char in STRING from start
by default. If FROM-END is non-nil, count from the end."
  (let ((strlen (length string)))
    (unless (< strlen n)
      (if from-end
          (string-pixel-width (substring string (- strlen n)))
        (string-pixel-width (substring string 0 n))))))

(defun block-default-border-color ()
  (frame-parameter nil 'foreground-color))

(defun block-blank (pixel &optional height)
  (if (= pixel 0)
      ""
    (block-string-duplines
     (propertize " " 'display `(space :width (,pixel)))
     (or height 1))))

(defun block-string-linum (string &optional omit-nulls)
  (length (split-string string "\n" omit-nulls)))

(defun block-string-join (sequences)
  (string-join sequences "\n"))

(defun block-string-duplines (string length)
  (string-join (make-list length string) "\n"))

(defun block-lines-pad (string height &optional offset padstr)
  "Pad the string to a height of HEIGHT, with the string offset by OFFSET
lines from the top, using PADSTR to fill blank lines."
  (if-let* ((linum (block-string-linum string))
            ((> height linum))
            (rest (- height linum))
            (top (block-line-offset rest (or offset 0)))
            (bottom (- rest top))
            (padstr (or padstr ""))
            (top-string
             (if (> top 0)
                 (concat (block-string-duplines padstr top) "\n")
               ""))
            (bottom-string
             (if (> bottom 0)
                 (concat "\n" (block-string-duplines padstr bottom))
               "")))
      (concat top-string string bottom-string)
    string))

(defun block-string-concat (&rest strings)
  (let* ((height (-max (-map #'block-string-linum strings)))
         (strings (mapcar (lambda (string)
                            (block-lines-pad string height))
                          strings)))
    (block-string-join
     (apply 'cl-mapcar #'concat
            (mapcar (lambda (string)
                      (block-maplines 'identity string))
                    strings)))))

(defun block-map-collect (list map-func collect-func
                               &optional arglist)
  "Apply MAP-FUNC to LIST, then apply COLLECT-FUNC with ARGLIST
to the result of map function.

The first argument of COLLECT-FUNC function is a list and the rest
are ARGLIST."
  (apply collect-func (mapcar map-func list) (or arglist '())))

(defun block-maplines (function string &optional collect-func &rest arglist)
  "Apply FUNCTION to each line of STRING. If COLLECT-FUNC is non-nil,
apply this function to the list and ARGLIST are arguments of this
COLLECT-FUNC; otherwise return a list of lines."
  (let ((lines (split-string string "\n")))
    (if collect-func
        (block-map-collect lines function collect-func arglist)
      (mapcar function lines))))

(defun block-line-offset (total offset)
  "When OFFSET is a positive number, there is a offset pixel distance
from start. When OFFSET is a negative number, there is a OFFSET pixel
from the end. Return the offset from start."
  (cond ((>= offset 0) (min offset total))
        ((< offset 0) (max 0 (+ offset total)))))

(defun block-line-align-offset (total justify-or-align)
  "Return the offset when align with JUSTIFY-OR-ALIGN."
  (pcase justify-or-align
    ((or 'left 'top) (block-line-offset total 0))
    ((or 'right 'bottom) (block-line-offset total total))
    ('center (block-line-offset total (/ total 2)))
    (_ (error "Invalid format of align: %S" align))))

(defun block-lines-justify (string pixel &optional justify)
  "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
should be one of the symbols 'left', 'center', 'right'.
When JUSTIFY is nil, set it to 'left' by default."
  (let ((justify (or justify 'left)))
    (block-maplines (lambda (line)
                      (block-pixel-typeset line pixel justify))
                    string 'string-join "\n")))

;; (defun block-lines-justify (string pixel &optional justify)
;;   "Justify each line in STRING to PIXEL by JUSTIFY, JUSTIFY
;; should be one of the symbols 'left', 'center', 'right'.
;; When JUSTIFY is nil, set it to 'left' by default."
;;   (let ((justify (or justify 'left)))
;;     (block-pixel-typeset  pixel justify)))

(defun block-lines-align (string height &optional align)
  (block-lines-pad
   string height
   (block-line-align-offset
    (- height (block-string-linum string)) (or align 'top))))

(defun block-lines-concat (strings &optional align text-align)
  "TEXT-ALIGN should be one of left,center,right.
ALIGN should be one of top,center,bottom."
  (let ((height (-max (-map #'block-string-linum strings))))
    (apply 'block-string-concat
           (mapcar (lambda (string)
                     (block-lines-justify
                      (block-lines-align string height align)
                      (string-pixel-width string)
                      text-align))
                   strings))))

;; (defun block-lines-stack (strings &optional align text-align)
;;   "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
;;   (let ((max-width (-max (-map #'string-pixel-width strings))))
;;     (mapconcat (lambda (string)
;;                  (block-lines-justify
;;                   ;; 再 justify 整个 block
;;                   (block-lines-justify
;;                    ;; 先 justify 文本
;;                    string (string-pixel-width string) text-align)
;;                   max-width align))
;;                strings "\n")))

;; (defun block-has-bottom-border-p (string)
;;   (with-temp-buffer
;;     (insert (car (last (split-string string "\n"))))
;;     (goto-char (point-min))
;;     (when-let* ((match (text-property-search-forward 'face))
;;                 (_ (plist-get (prop-match-value match) :overline)))
;;       ;; buffer 的 position 比 string 的 position 多 1
;;       ;; (cons (1- (prop-match-beginning match))
;;       ;;       (1- (prop-match-end match)))
;;       )))

;; FIXME: 替换为存储在 string 中的 block 信息查找
(defun block-has-bottom-border-p (string)
  ;; 有 overline，但又不是第一行
  (let ((lines (split-string string "\n")))
    (with-temp-buffer
      (insert string)
      (goto-char (point-max))
      (catch 'break
        (while (not (bobp))
          (if-let ((match (text-property-search-backward 'face)))
              (if (plist-get (prop-match-value match) :overline)
                  (throw 'break (not (= 1 (line-number-at-pos))))
                (goto-char (line-beginning-position)))
            (throw 'break nil)))))))

(defun block-string-first-line-with-newline (string)
  ;; 带换行符，防止破坏首行的 line-height
  (save-match-data
    (if-let ((point (string-match "\n" string)))
        (substring string 0 (1+ point))
      string)))

(defun block-string-first-line (string)
  ;; 带换行符，防止破坏首行的 line-height
  (save-match-data
    (if-let ((point (string-match "\n" string)))
        (substring string 0 point)
      string)))

;;; 一下两个内置方式仅适用于 merge 底部边框行的情况
(defun block--left-margin-pixel (string)
  ;; string 是非上下 margin 部分的一行文本
  ;; 对已经渲染为字符串的 block 反向获取它的 margin 属性
  (let ((string (block-string-first-line string)))
    (if-let* ((display-value (get-text-property 0 'display string))
              (_ (and (eq 'space (car display-value))
                      (not (plist-get (get-text-property 0 'face string)
                                      :inverse-video)))))
        (car (plist-get (cdr display-value) :width))
      0)))

(defun block--right-margin-pixel (string)
  ;; string 是多行文本
  ;; 有 '(space :width ..)
  ;; 无 '(face :inverse-video t)
  (let* ((lines (split-string string "\n" t))
         (last-strs (mapcar (lambda (line)
                              (substring line -1))
                            lines))
         ;; 由于行尾可能出现直接断行，部分为像素空格
         ;; 需要判断每一行都是像素空格才能判断是 right margin
         (count (seq-count
                 (lambda (str)
                   (and (eq 'space
                            (car (get-text-property 0 'display str)))
                        (not (plist-get
                              (get-text-property 0 'face str)
                              :inverse-video))))
                 last-strs)))
    (if (= count (length last-strs))
        (car (plist-get
              (cdr (get-text-property 0 'display (car last-strs)))
              :width))
      0)))

;; FIXME: 分割一个字符串为 多个 block
;; 在 block-string 返回的字符串中存储 block 相关信息
;; 在 block-lines-concat 和 block-lines-stack 获取这些存储的信息
;; 在 block-concat 和 block--stack 再这些信息存储到新的 block 中
(defun block-lines-stack (strings &optional align text-align)
  "ALIGN used for all blocks, TEXT-ALIGN used for text in a block."
  (let ((max-width (-max (-map #'string-pixel-width strings)))
        prev-block-pixel
        prev-block-left-margin
        prev-block-right-margin
        prev-has-bottom-border-p
        result)
    (dolist (string strings)
      (let* ((curr-block-pixel (string-pixel-width string))
             (justified-str (block-lines-justify
                             ;; justify 多行文本对齐
                             string curr-block-pixel text-align))
             (curr-block-first-line
              (block-string-first-line justified-str))
             (curr-block-rest-string
              ;; 开头带了换行符
              (string-trim-left
               justified-str (regexp-quote curr-block-first-line))))
        ;; FIXME: 只要上一个 block 有底部 border，都需要 换行符 - 1
        (if-let (prev-has-bottom-border-p)
            ;; 上一个 block 有底部边框行
            ;; 需要和下一个 block 的首行 merge
            (if (string-blank-p curr-block-first-line)
                ;; 如果当前block首行是空行，直接将 overline border 行当作该行
                (setq result
                      (concat result (string-trim-left
                                      curr-block-rest-string "\n")))
              (let* ((curr-block-left-margin
                      (block--left-margin-pixel justified-str))
                     (curr-block-right-margin
                      (block--right-margin-pixel justified-str))
                     (prev-block-lines (split-string result "\n" t))
                     ;; 去除最后一行，带 overline 的空白行
                     (prev-without-last-line
                      (string-join
                       (seq-take prev-block-lines
                                 (1- (length prev-block-lines)))
                       "\n")))
                ;; 补齐左边的 overline border
                (when (> curr-block-left-margin prev-block-left-margin)
                  (let* ((left-margin-pixel prev-block-left-margin)
                         (left-overline-pixel (- curr-block-left-margin
                                                 left-margin-pixel)))
                    ;; 将 curr-block 的 left margin 拆分为两个，第二个设置 overline
                    (setq curr-block-first-line
                          (concat (block-pixel-spacing left-margin-pixel)
                                  (propertize (block-pixel-spacing left-overline-pixel)
                                              'face '(:overline t))
                                  (substring curr-block-first-line 1)))))
                ;; 补齐右边的 overline border
                (let* ((curr-pixel-to-right-border
                        (- curr-block-pixel curr-block-right-margin))
                       (prev-pixel-to-right-border
                        (- prev-block-pixel prev-block-right-margin))
                       (right-overline-pixel
                        (- prev-pixel-to-right-border curr-pixel-to-right-border))
                       (right-margin-pixel
                        (if (> (- curr-block-pixel prev-pixel-to-right-border) 0)
                            (- curr-block-pixel prev-pixel-to-right-border)
                          0)))
                  (when (> prev-pixel-to-right-border curr-pixel-to-right-border)
                    (let ((first-line
                           (if (> curr-block-right-margin 0)
                               (substring curr-block-first-line
                                          0 (1- (length curr-block-first-line)))
                             curr-block-first-line)))
                      (setq curr-block-first-line
                            (concat first-line
                                    (propertize
                                     (block-pixel-spacing right-overline-pixel)
                                     'face '(:overline t))
                                    (block-pixel-spacing right-margin-pixel))))))
                (setq result (concat prev-without-last-line
                                     "\n" curr-block-first-line curr-block-rest-string
                                     "\n"))))
          (setq result (concat result justified-str "\n"))
          (setq prev-block-left-margin
                (block--left-margin-pixel
                 (block-string-first-line justified-str)))
          (setq prev-block-right-margin
                (block--right-margin-pixel justified-str))
          (setq prev-block-pixel (string-pixel-width justified-str))
          (setq prev-has-bottom-border-p
                (block-has-bottom-border-p justified-str)))))
    result))

(provide 'block-utils)
