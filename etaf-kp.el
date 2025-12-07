;; -*- lexical-binding: t; -*-

(require 'etaf-kp-utils)
(require 'etaf-kp-hyphen)

(defconst etaf-kp-load-file-name (or load-file-name (buffer-file-name)))

(defvar etaf-kp-latin-lang "en_US")

(defvar etaf-kp-param-use-default-p t
  "Used in internal, you should not modify it!")

(defvar etaf-kp-lws-ideal-pixel nil
  "The ideal pixel of whitespace between latin words.")

(defvar etaf-kp-lws-stretch-pixel nil
  "The stretch pixel of whitespace between latin words.")

(defvar etaf-kp-lws-shrink-pixel nil
  "The shrink pixel of whitespace between latin words.")

(defvar etaf-kp-mws-ideal-pixel nil
  "The ideal pixel of whitespace between latin word and cjk char.")

(defvar etaf-kp-mws-stretch-pixel nil
  "The stretch pixel of whitespace between latin word and cjk char.")

(defvar etaf-kp-mws-shrink-pixel nil
  "The shrink pixel of whitespace between latin word and cjk char.")

(defvar etaf-kp-cws-ideal-pixel nil
  "The ideal pixel of non-whitespace, such between cjk chars.")

(defvar etaf-kp-cws-stretch-pixel nil
  "The stretch pixel of non-whitespace, such between cjk chars.")

(defvar etaf-kp-cws-shrink-pixel nil
  "The shrink pixel of non-whitespace, such between cjk chars.")

(defvar etaf-kp-lws-max-pixel nil)

(defvar etaf-kp-lws-min-pixel nil)

(defvar etaf-kp-mws-max-pixel nil)

(defvar etaf-kp-mws-min-pixel nil)

(defvar etaf-kp-cws-max-pixel nil)

(defvar etaf-kp-cws-min-pixel nil)

(defvar etaf-kp-caches
  (make-hash-table
   :test 'equal :size 100 :rehash-size 1.5 :weakness nil)
  "Key of etaf-kp-caches is the hash of string.")

(defun etaf-kp-root-dir ()
  (when etaf-kp-load-file-name
    (file-name-directory etaf-kp-load-file-name)))

(defun etaf-kp-load-dicts ()
  (etaf-kp-hyphen-load-languages
   (expand-file-name "./dictionaries" (etaf-kp-root-dir))))

(etaf-kp-load-dicts)

(defun etaf-kp-param-check ()
  "Check whether all params are set."
  (and etaf-kp-lws-ideal-pixel etaf-kp-lws-stretch-pixel etaf-kp-lws-shrink-pixel
       etaf-kp-mws-ideal-pixel etaf-kp-mws-stretch-pixel etaf-kp-mws-shrink-pixel
       etaf-kp-cws-ideal-pixel etaf-kp-cws-stretch-pixel etaf-kp-cws-shrink-pixel))

(defun etaf-kp-param-set-default (string)
  (let* ((lws-pixel (etaf-kp-word-spacing-pixel string))
         (mws-pixel (- lws-pixel 2)))
    (etaf-kp-param-set
     lws-pixel (round (* lws-pixel 0.5)) (round (* lws-pixel 0.333))
     mws-pixel (round (* mws-pixel 0.5)) (round (* mws-pixel 0.333))
     0 2 0)))

(defun etaf-kp-param-set ( lws-ideal lws-stretch lws-shrink
                       mws-ideal mws-stretch mws-shrink
                       cws-ideal cws-stretch cws-shrink)
  (setq etaf-kp-lws-ideal-pixel lws-ideal)
  (setq etaf-kp-lws-stretch-pixel lws-stretch)
  (setq etaf-kp-lws-shrink-pixel lws-shrink)
  (setq etaf-kp-mws-ideal-pixel mws-ideal)
  (setq etaf-kp-mws-stretch-pixel mws-stretch)
  (setq etaf-kp-mws-shrink-pixel mws-shrink)
  (setq etaf-kp-cws-ideal-pixel cws-ideal)
  (setq etaf-kp-cws-stretch-pixel cws-stretch)
  (setq etaf-kp-cws-shrink-pixel cws-shrink)
  (unless (etaf-kp-param-check)
    (error "all pixel args should not be nil!"))
  (setq etaf-kp-lws-max-pixel (+ etaf-kp-lws-ideal-pixel etaf-kp-lws-stretch-pixel))
  (setq etaf-kp-lws-min-pixel (- etaf-kp-lws-ideal-pixel etaf-kp-lws-shrink-pixel))
  (setq etaf-kp-mws-max-pixel (+ etaf-kp-mws-ideal-pixel etaf-kp-mws-stretch-pixel))
  (setq etaf-kp-mws-min-pixel (- etaf-kp-mws-ideal-pixel etaf-kp-mws-shrink-pixel))
  (setq etaf-kp-cws-max-pixel (+ etaf-kp-cws-ideal-pixel etaf-kp-cws-stretch-pixel))
  (setq etaf-kp-cws-min-pixel (- etaf-kp-cws-ideal-pixel etaf-kp-cws-shrink-pixel))
  (setq etaf-kp-param-use-default-p nil))

(defun etaf-kp-param-fmtstr ()
  (format "%s-%s-%s-%s-%s-%s-%s-%s-%s"
          etaf-kp-lws-ideal-pixel etaf-kp-lws-stretch-pixel etaf-kp-lws-shrink-pixel
          etaf-kp-mws-ideal-pixel etaf-kp-mws-stretch-pixel etaf-kp-mws-shrink-pixel
          etaf-kp-cws-ideal-pixel etaf-kp-cws-stretch-pixel etaf-kp-cws-shrink-pixel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 正确版本：包含完整字符集和组合标记
(defvar etaf-kp-latin-regexp
  (concat
   "["                               ; 开始字符集
   "A-Za-z'-"                          ; 基础拉丁字母
   "\300-\326\330-\366\370-\417"     ; ISO-8859-1补充
   "\u00C0-\u00D6\u00D8-\u00F6"      ; Unicode基本补充
   "\u00F8-\u00FF\u0100-\u024F"      ; 扩展A/B
   "\u1E00-\u1EFF"                   ; 扩展附加
   "\uA780-\uA7F9"                   ; 拉丁扩展-D
   "]"                               ; 闭合字符集
   )
  "正则表达式匹配所有拉丁字符及其变体，包括组合变音符")

(defun etaf-kp-split-string (string)
  ;; return (boxes-vector . hyphen-positions-vector)
  (let* ((boxes (etaf-kp-split-to-boxes string))
         (idx 0)
         new-boxes idxs)
    (dolist (box (append boxes nil))
      (save-match-data
        (if (string-match
             (format
              "^\\([[{<„‚¿¡*@\"']*\\)\\(%s+\\)\\([]}>.,*?\"']*\\)$"
              etaf-kp-latin-regexp)
             box)
            (let* ((pure-word (match-string 2 box))
                   (left-punct (match-string 1 box))
                   (right-punct (match-string 3 box))
                   (word-lst (etaf-kp-hyphen-boxes
                              (etaf-kp-hyphen-create etaf-kp-latin-lang)
                              pure-word))
                   (num (length word-lst)))
              (when left-punct
                (setf (car word-lst) (concat left-punct
                                             (car word-lst))))
              (when right-punct
                (setf (car (last word-lst))
                      (concat (car (last word-lst))
                              right-punct)))
              (push word-lst new-boxes)
              (dotimes (i num)
                (when (< i (1- num))
                  (push idx idxs))
                (cl-incf idx)))
          (push (list box) new-boxes)
          (cl-incf idx))))
    (let ((boxes-lst (apply #'append (nreverse new-boxes))))
      (cons (vconcat boxes-lst)
            (vconcat (nreverse idxs))))))

(defun etaf-kp-str-type (str)
  "STR should be single letter string."
  (cond
   ;; a half-width cjk punct
   ((or (string= "“" str) (string= "”" str)) 'cjk)
   ((= (string-width str) 1) 'latin)
   ((= (string-width str) 2)
    (if (etaf-kp-cjk-fw-punct-p str)
        'cjk-punct
      'cjk))
   (t (error "Abnormal string width %s for %s"
             (string-width str) str))))

(defun etaf-kp-box-type (box)
  (unless (or (null box) (string-empty-p box))
    (cons (etaf-kp-str-type (substring box 0 1))
          (etaf-kp-str-type (substring box -1)))))

(defun etaf-kp-glue-type (prev-box-type curr-box-type)
  "Lws means whitespace between latin words; cws means
whitespace between cjk words; mws means whitespace between
cjk and latin words; nws means no whitespace."
  (let ((before (cdr prev-box-type))
        (after (car curr-box-type)))
    (if before
        (cond
         ((and (eq before 'latin) (eq after 'latin)) 'lws)
         ((and (eq before 'cjk) (eq after 'cjk)) 'cws)
         ((or (and (eq before 'cjk) (eq after 'latin))
              (and (eq before 'latin) (eq after 'cjk)))
          'mws)
         ((or (eq before 'cjk-punct) (eq after 'cjk-punct)) 'cws))
      'nws)))

(defun etaf-kp--glues-types (boxes boxes-types hyphen_positions)
  "Set type of all glue in boxes using `etaf-kp-glue-type',
set type to 'nws for each glue after position in hyphen_positions."
  ;; IMPORTANT!
  (let* ((num (length boxes))
         (glues-types (make-vector num nil))
         prev-box-type curr-box-type)
    ;; set hyphen position to 'nws
    (dolist (i (append hyphen_positions nil))
      (aset glues-types (1+ i) 'nws))
    (dotimes (i num)
      (unless (aref glues-types i)
        (let ((curr-box-type (aref boxes-types i)))
          (aset glues-types
                i (etaf-kp-glue-type prev-box-type curr-box-type))
          (setq prev-box-type curr-box-type))))
    glues-types))

(defun etaf-kp-glue-ideal-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) etaf-kp-lws-ideal-pixel)
        ((eq 'mws type) etaf-kp-mws-ideal-pixel)
        ((eq 'cws type) etaf-kp-cws-ideal-pixel)))

(defun etaf-kp-glue-min-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) etaf-kp-lws-min-pixel)
        ((eq 'mws type) etaf-kp-mws-min-pixel)
        ((eq 'cws type) etaf-kp-cws-min-pixel)))

(defun etaf-kp-glue-max-pixel (type)
  (cond ((or (null type) (eq 'nws type)) 0)
        ((eq 'lws type) etaf-kp-lws-max-pixel)
        ((eq 'mws type) etaf-kp-mws-max-pixel)
        ((eq 'cws type) etaf-kp-cws-max-pixel)))

(defun etaf-kp-text-hash (string)
  (let ((latin-font (etaf-kp-latin-font string))
        (cjk-font (etaf-kp-cjk-font string))
        (print-text-properties t))
    (secure-hash
     'md5 (format "%s|%s|%s" latin-font cjk-font (prin1-to-string string)))))

(defun etaf-kp-text-cache (string)
  ;; consider font
  ;; (text-data . param-cache(param-data . dp-cache(dp-data)))
  "Return the text cache of STRING. Text cache consists of
(data . param-cache). Data is a plist (:boxes boxes :boxes-widths
boxes-widths :glues-types glues-types)."
  (let ((text-hash (etaf-kp-text-hash string)))
    (if-let ((_ etaf-kp-caches)
             (cache (gethash text-hash etaf-kp-caches)))
        cache
      (when (or etaf-kp-param-use-default-p
                (null (etaf-kp-param-check)))
        (etaf-kp-param-set-default string))
      (setq etaf-kp-param-use-default-p t)
      (let* ((cjk-font (etaf-kp-cjk-font string))
             (latin-font (etaf-kp-latin-font string))
             (cons (etaf-kp-split-string string))
             (boxes (car cons))
             (hyphen_after_positions (cdr cons))
             (boxes-widths (vconcat
                            (mapcar #'string-pixel-width boxes)))
             (boxes-types (vconcat (mapcar #'etaf-kp-box-type boxes)))
             (glues-types (etaf-kp--glues-types boxes boxes-types
                                            hyphen_after_positions))
             (plist (list :boxes boxes
                          :latin-font latin-font
                          :cjk-font cjk-font
                          :boxes-widths boxes-widths
                          :boxes-types boxes-types
                          :glues-types glues-types))
             (cache (cons plist nil)))
        (unless etaf-kp-caches
          (setq etaf-kp-caches (make-hash-table
                            :test 'equal :size 100
                            :rehash-size 1.5 :weakness nil)))
        (puthash text-hash cache etaf-kp-caches)
        cache))))

(defun etaf-kp-text-data (string &optional key)
  "Return the data plist of text cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (car (etaf-kp-text-cache string))))
    (if key
        (plist-get data key)
      data)))

(defun etaf-kp-boxes (string)
  (etaf-kp-text-data string :boxes))

(defun etaf-kp-hyphen-str (string)
  "-"
  ;; (propertize
  ;;  "-"
  ;;  'face `(:family ,(etaf-kp-text-data string :latin-font)))
  )

(defun etaf-kp-hyphen-pixel (string)
  (string-pixel-width (etaf-kp-hyphen-str string)))

(defun etaf-kp-boxes-widths (string)
  (etaf-kp-text-data string :boxes-widths))

(defun etaf-kp-boxes-types (string)
  (etaf-kp-text-data string :boxes-types))

(defun etaf-kp-glues-types (string)
  (etaf-kp-text-data string :glues-types))

(defun etaf-kp-param-cache (string)
  ;; (param-data . dp-cache(dp-data))
  "Return a plist of ideal-prefixs, min-prefixs and max-prefixs
by caculating with string and other params."
  (if-let* ((param-record (cdr (etaf-kp-text-cache string)))
            (param-cache (gethash (etaf-kp-param-fmtstr) param-record)))
      param-cache
    (let* ((boxes-num (length (etaf-kp-boxes string)))
           (boxes-widths (etaf-kp-boxes-widths string))
           (glues-types (etaf-kp-glues-types string))
           (ideal-prefixs (make-vector (1+ boxes-num) 0))
           (min-prefixs (make-vector (1+ boxes-num) 0))
           (max-prefixs (make-vector (1+ boxes-num) 0)))
      (dotimes (i boxes-num)
        (aset ideal-prefixs (1+ i)
              (+ (aref ideal-prefixs i) (aref boxes-widths i)
                 (etaf-kp-glue-ideal-pixel (aref glues-types i))))
        (aset min-prefixs (1+ i)
              (+ (aref min-prefixs i) (aref boxes-widths i)
                 (etaf-kp-glue-min-pixel (aref glues-types i))))
        (aset max-prefixs (1+ i)
              (+ (aref max-prefixs i) (aref boxes-widths i)
                 (etaf-kp-glue-max-pixel (aref glues-types i)))))
      (let* ((param-data (list :ideal-prefixs ideal-prefixs
                               :min-prefixs min-prefixs
                               :max-prefixs max-prefixs))
             (param-cache (cons param-data nil)))
        (if-let ((param-record (cdr (etaf-kp-text-cache string))))
            (puthash (etaf-kp-param-fmtstr) param-cache param-record)
          (let ((param-record (make-hash-table
                               :test 'equal :size 100
                               :rehash-size 1.5 :weakness nil)))
            (puthash (etaf-kp-param-fmtstr) param-cache param-record)
            (puthash (etaf-kp-text-hash string)
                     (cons (etaf-kp-text-data string) param-record)
                     etaf-kp-caches)))
        param-cache))))

(defun etaf-kp-param-data (string &optional key)
  "Return the data plist of param cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (car (etaf-kp-param-cache string))))
    (if key
        (plist-get data key)
      data)))

(defun etaf-kp-ideal-prefixs (string)
  (etaf-kp-param-data string :ideal-prefixs))

(defun etaf-kp-min-prefixs (string)
  (etaf-kp-param-data string :min-prefixs))

(defun etaf-kp-max-prefixs (string)
  (etaf-kp-param-data string :max-prefixs))

(defun etaf-kp--gaps-list (glues-types)
  (list (seq-count (lambda (it) (eq 'lws it)) glues-types)
        (seq-count (lambda (it) (eq 'mws it)) glues-types)
        (seq-count (lambda (it) (eq 'cws it)) glues-types)))

(defun etaf-kp--line-cost-and-gaps (ideal-pixel line-pixel glues-types)
  "Return the cost ratio of WORDS limited to LINE-PIXEL."
  (let* ((glues-types (seq-drop glues-types 1))
         (gaps-list (etaf-kp--gaps-list glues-types))
         (latin-gaps (nth 0 gaps-list))
         (mix-gaps (nth 1 gaps-list))
         (cjk-gaps (nth 2 gaps-list))
         (rest-pixel (- line-pixel ideal-pixel))
         ratio)
    (if (> rest-pixel 0)
        ;; should stretch
        (setq ratio
              (/ rest-pixel
                 (float (+ (* latin-gaps etaf-kp-lws-stretch-pixel)
                           (* cjk-gaps etaf-kp-cws-stretch-pixel)
                           (* mix-gaps etaf-kp-mws-stretch-pixel)))))
      ;; should shrink
      (setq ratio
            (/ rest-pixel
               (float (+ (* latin-gaps etaf-kp-lws-shrink-pixel)
                         (* mix-gaps etaf-kp-mws-shrink-pixel))))))
    (list :cost (* 100 (expt ratio 3)) :gaps gaps-list)))

(defun etaf-kp-hyphenate-p (glues-types n)
  (and (< n (length glues-types))
       (eq 'nws (aref glues-types n))))

(defun etaf-kp-dp-cache (string line-pixel)
  (if-let* ((dp-record (cdr (etaf-kp-param-cache string)))
            (dp-cache (gethash line-pixel dp-record)))
      dp-cache
    (let* ((glues-types (etaf-kp-glues-types string))
           (boxes (etaf-kp-boxes string))
           (hyphen-pixel (etaf-kp-hyphen-pixel string))
           (n (length boxes))
           (ideal-prefixs (etaf-kp-ideal-prefixs string))
           (min-prefixs (etaf-kp-min-prefixs string))
           (max-prefixs (etaf-kp-max-prefixs string))
           (backptrs (make-vector (1+ n) nil))
           (costs (make-vector (1+ n) nil))
           ;; rest pixel = line-pixel - ideal-pixel
           (rests (make-vector (1+ n) nil))
           (gaps (make-vector (1+ n) nil))
           ;; 连续行 hyphen 结尾计数
           (hyphen-line-count 0))
      (dotimes (i (1+ n))
        (aset costs i (if (= i 0) 0.0 nil)))
      (dotimes (i (1+ n))
        (when (aref costs i)
          (setq hyphen-line-count 0)
          (catch 'break
            (dotimes (j (- n i))
              (let* ((k (+ i j 1)) ;; k: end word index (exclusive)
                     (is-last (= k n))
                     (end-with-hyphenp (etaf-kp-hyphenate-p glues-types k))
                     (ideal-pixel (- (aref ideal-prefixs k)
                                     (aref ideal-prefixs i)
                                     (etaf-kp-glue-ideal-pixel
                                      (aref glues-types i))))
                     (max-pixel (- (aref max-prefixs k)
                                   (aref max-prefixs i)
                                   (etaf-kp-glue-max-pixel
                                    (aref glues-types i))))
                     (min-pixel (- (aref min-prefixs k)
                                   (aref min-prefixs i)
                                   (etaf-kp-glue-min-pixel
                                    (aref glues-types i)))))

                ;; ends with hyphen, plus the pixel of hyphen
                (when end-with-hyphenp
                  (cl-incf ideal-pixel hyphen-pixel)
                  (cl-incf max-pixel hyphen-pixel)
                  (cl-incf min-pixel hyphen-pixel))

                ;; back to last word
                (when (or (> min-pixel line-pixel)
                          (and is-last (> ideal-pixel line-pixel)))
                  (when (null (aref costs (1- k)))
                    ;; can not find a proper line break,
                    ;; break line at prev box
                    (let* ((hyphenate-p (etaf-kp-hyphenate-p glues-types (1- k)))
                           (ideal-pixel (- (aref ideal-prefixs (1- k))
                                           (aref ideal-prefixs i)
                                           (etaf-kp-glue-ideal-pixel
                                            (aref glues-types i))))
                           (rest-pixel (- line-pixel ideal-pixel)))
                      (when hyphenate-p (cl-incf ideal-pixel hyphen-pixel))
                      (aset costs (1- k) (+ 100 (expt rest-pixel 3)))
                      (aset rests (1- k) rest-pixel)
                      (aset backptrs (1- k) i)
                      (aset gaps (1- k)
                            (etaf-kp--gaps-list
                             (seq-drop (cl-subseq glues-types i (1- k)) 1)))
                      ;; (elog-debug "1-k:%s; rests:%S" (1- k)
                      ;;             (aref rests (1- k)))
                      ))
                  (throw 'break nil))
                
                (when (or (<= min-pixel line-pixel max-pixel)
                          (and is-last (<= ideal-pixel line-pixel)))
                  (let* ((line-gaps)
                         (line-cost
                          (cond
                           ;; only has one word
                           ((= j 0)
                            (expt (- ideal-pixel line-pixel) 3))
                           (is-last 0.0)
                           ;; has more than one word
                           (t (let* ((cost-and-gaps
                                      (etaf-kp--line-cost-and-gaps
                                       ideal-pixel line-pixel
                                       (seq-subseq glues-types i k)))
                                     (cost (plist-get cost-and-gaps :cost))
                                     (gaps (plist-get cost-and-gaps :gaps)))
                                (setq line-gaps gaps)
                                (if end-with-hyphenp
                                    (progn
                                      ;; add extra cost of hypen
                                      (cl-incf hyphen-line-count)
                                      (+ cost (* 1000 hyphen-line-count)))
                                  (setq hyphen-line-count 0)
                                  cost)))))
                         (total-cost (+ (aref costs i) line-cost)))
                    ;; (message "total cost:%S" total-cost)
                    ;; (message "hyphen-line-count:%S" hyphen-line-count)
                    (when (or (null (aref costs k))
                              (< (abs total-cost) (abs (aref costs k))))
                      ;; set all for cost is smaller!
                      (aset rests k (- line-pixel ideal-pixel))
                      (aset gaps k line-gaps)
                      (aset costs k total-cost)
                      ;; 断点设置为 当前行的起点 = 上一行的和结束点
                      (aset backptrs k i)))))))))
      (let ((breaks (list n))
            (index n))
        (while (> index 0)
          (let ((prev (aref backptrs index)))
            (if prev (progn (push prev breaks)
                            (setq index prev))
              (setq index (1- index)))))
        (let* ((breaks (cdr breaks))
               lines-rests lines-gaps dp-cache)
          (dolist (i breaks)
            (push (aref rests i) lines-rests)
            (push (aref gaps i) lines-gaps))
          ;; set dp cache
          (setq dp-cache (list :rests (nreverse lines-rests)
                               :gaps (nreverse lines-gaps)
                               :breaks breaks
                               :cost (aref costs (length boxes))))
          ;; update param cache
          (if-let ((dp-record (cdr (etaf-kp-param-cache string))))
              (puthash line-pixel dp-cache dp-record)
            (let ((dp-record (make-hash-table
                              :test 'equal :size 100
                              :rehash-size 1.5 :weakness nil)))
              (puthash line-pixel dp-cache dp-record)
              (puthash (etaf-kp-param-fmtstr)
                       (cons (etaf-kp-param-data string) dp-record)
                       (cdr (etaf-kp-text-cache string)))))
          dp-cache)))))

(defun etaf-kp-dp-data (string line-pixel &optional key)
  "Return the data plist of dp cache. If KEY is non-nil,
return the value of KEY in plist."
  (let ((data (etaf-kp-dp-cache string line-pixel)))
    (if key
        (plist-get data key)
      data)))

(defun etaf-kp-total-cost (string line-pixel)
  "Return the COST of kp algorithm."
  (etaf-kp-dp-data string line-pixel :cost))

(defun etaf-kp-line-breaks (string line-pixel)
  "Return the break points of kp algorithm."
  (etaf-kp-dp-data string line-pixel :breaks))

(defun etaf-kp-line-glues (string line-pixel)
  "Line glues include glues before first box and after last box.
So the length of line glues is: line-boxes-num + 1"
  (let* ((boxes-widths (etaf-kp-boxes-widths string))
         (boxes (etaf-kp-boxes string))
         (boxes-num (length boxes))
         (glues-types (etaf-kp-glues-types string))
         (ideal-prefixs (etaf-kp-ideal-prefixs string))
         (max-prefixs (etaf-kp-max-prefixs string))
         (breaks (etaf-kp-line-breaks string line-pixel))
         (line-glues (make-vector (length breaks) nil))
         (start 0))
    (dotimes (i (length breaks))
      (let* ((end (nth i breaks))
             (line-boxes-widths (cl-subseq boxes-widths start end))
             (line-glues-types
              ;; exclude glue before word at the start of line
              (seq-drop (cl-subseq glues-types start end) 1))
             (is-last (>= end boxes-num))
             line-glue)
        (setq line-glue
              (cond
               ((= 1 (length line-boxes-widths))
                (if (etaf-kp-hyphenate-p glues-types end)
                    ;; ends with hyphen, minus hyphen-pixel
                    (list 0 (- line-pixel
                               (etaf-kp-hyphen-pixel string)
                               (aref line-boxes-widths 0)))
                  (list 0 (- line-pixel (aref line-boxes-widths 0)))))
               (is-last
                (append '(0)
                        (mapcar #'etaf-kp-glue-ideal-pixel line-glues-types)
                        (list
                         (- line-pixel (- (aref ideal-prefixs end)
                                          (aref ideal-prefixs start)
                                          (etaf-kp-glue-ideal-pixel
                                           (aref glues-types start)))))))
               (t
                ;; (elog-debug "-----------------")
                ;; (elog-debug "glues-types:%s" line-glues-types)
                (let ((max-pixel (- (aref max-prefixs end)
                                    (aref max-prefixs start)
                                    (etaf-kp-glue-max-pixel
                                     (aref glues-types start)))))
                  ;; ends with hyphen
                  (when (etaf-kp-hyphenate-p glues-types end)
                    (cl-incf max-pixel (etaf-kp-hyphen-pixel string)))
                  ;; (elog-debug "start:%s; end:%s; max:%s" start end max-pixel)
                  (if (< max-pixel line-pixel)
                      (progn
                        ;; 行尾直接断行的情况
                        ;; (elog-debug "暴力断行 i:%s pixel:%s"
                        ;;             i (- line-pixel max-pixel))
                        (append '(0)
                                (mapcar #'etaf-kp-glue-max-pixel line-glues-types)
                                (list (- line-pixel max-pixel))))
                    ;; 正常情况
                    (let ((ideal-pixel (- (aref ideal-prefixs end)
                                          (aref ideal-prefixs start)
                                          (etaf-kp-glue-ideal-pixel
                                           (aref glues-types start)))))
                      ;; ideal-pixel 包含第一个box之前的glue
                      ;; (elog-debug "boxes:%S" (cl-subseq boxes start end))
                      ;; (elog-debug "line:%s; ideal:%s; rest:%s; stored-rest:%s"
                      ;;             line-pixel ideal-pixel (- line-pixel ideal-pixel)
                      ;;             (nth i (etaf-kp-dp-data string line-pixel :rests)))
                      )
                    (let* ((lines-rests (etaf-kp-dp-data string line-pixel :rests))
                           (curr-rest-pixel (nth i lines-rests)))
                      ;; (elog-debug "curr-rest-pixel:%s" curr-rest-pixel)
                      (cond
                       ((= curr-rest-pixel 0)
                        (append '(0) (mapcar #'etaf-kp-glue-ideal-pixel
                                             line-glues-types)
                                '(0)))
                       (t
                        (let* ((lines-gaps (etaf-kp-dp-data string line-pixel :gaps))
                               (line-gaps (nth i lines-gaps))
                               (latin-gaps (nth 0 line-gaps))
                               (mix-gaps (nth 1 line-gaps))
                               (cjk-gaps (nth 2 line-gaps))
                               (latin-gap-pixel 0) (latin-extra-gaps 0)
                               (mix-gap-pixel 0) (mix-extra-gaps 0)
                               (cjk-gap-pixel 0) (cjk-extra-gaps 0)
                               (line-rest-pixel (abs curr-rest-pixel)))
                          ;; max-latin-change include stretch or shrink
                          (let* ((latin-gap-change (if (> curr-rest-pixel 0)
                                                       etaf-kp-lws-stretch-pixel
                                                     etaf-kp-lws-shrink-pixel))
                                 (latin-max-pixel (* latin-gaps latin-gap-change)))
                            (if (< (- line-rest-pixel latin-max-pixel) 0)
                                (when (> latin-gaps 0)
                                  ;; only stretch latin glues
                                  ;; (elog-debug "latin-gap-pixel:%s"
                                  ;;             (/ line-rest-pixel latin-gaps))
                                  ;; (elog-debug "latin-extra-gaps:%s"
                                  ;;             (% line-rest-pixel latin-gaps))
                                  (setq latin-gap-pixel (/ line-rest-pixel latin-gaps))
                                  (setq latin-extra-gaps (% line-rest-pixel latin-gaps)))
                              ;; stretch latin glues max and continue to stretch mix glues
                              (setq latin-gap-pixel latin-gap-change)
                              (setq line-rest-pixel (- line-rest-pixel latin-max-pixel))
                              (let* ((mix-gap-change (if (> curr-rest-pixel 0)
                                                         etaf-kp-mws-stretch-pixel
                                                       etaf-kp-mws-shrink-pixel))
                                     (mix-max-pixel (* mix-gaps mix-gap-change)))
                                (if (< (- line-rest-pixel mix-max-pixel) 0)
                                    (when (> mix-gaps 0)
                                      ;; only stretch mix glues
                                      (setq mix-gap-pixel (/ line-rest-pixel mix-gaps))
                                      (setq mix-extra-gaps (% line-rest-pixel mix-gaps)))
                                  ;; stretch mix glues max and continue to stretch cjk glues
                                  (setq mix-gap-pixel mix-gap-change)
                                  (setq line-rest-pixel (- line-rest-pixel mix-max-pixel))
                                  (when (> cjk-gaps 0)
                                    (setq cjk-gap-pixel (/ line-rest-pixel cjk-gaps))
                                    (setq cjk-extra-gaps (% line-rest-pixel cjk-gaps)))))))
                          (let* ((latin-extra-index -1)
                                 (mix-extra-index -1)
                                 (cjk-extra-index -1)
                                 (glue-pixel-lst
                                  (mapcar
                                   (lambda (type)
                                     (let ((pixel (cond
                                                   ((eq type 'lws)
                                                    (cl-incf latin-extra-index)
                                                    (if (< latin-extra-index latin-extra-gaps)
                                                        (1+ latin-gap-pixel)
                                                      latin-gap-pixel))
                                                   ((eq type 'mws)
                                                    (cl-incf mix-extra-index)
                                                    (if (< mix-extra-index mix-extra-gaps)
                                                        (1+ mix-gap-pixel)
                                                      mix-gap-pixel))
                                                   ((eq type 'cws)
                                                    (cl-incf cjk-extra-index)
                                                    (if (< cjk-extra-index cjk-extra-gaps)
                                                        (1+ cjk-gap-pixel)
                                                      cjk-gap-pixel))
                                                   ((eq type 'nws) 0))))
                                       (if (> curr-rest-pixel 0)
                                           ;; stretch
                                           (+ (etaf-kp-glue-ideal-pixel type) pixel)
                                         ;; shrink
                                         (- (etaf-kp-glue-ideal-pixel type) pixel))))
                                   line-glues-types)))
                            ;; (elog-debug "glue-pixel-lst:%S" glue-pixel-lst)
                            ;; (elog-debug "--------------")
                            (append '(0) glue-pixel-lst '(0))))))))))))
        (aset line-glues i (vconcat line-glue nil))
        (setq start end)))
    line-glues))

(defun etaf-kp-combine-glues-and-boxes (glues boxes)
  (let* ((glues (append glues nil))
         (last-glue (car (last glues)))
         (glues (-drop-last 1 glues))
         (boxes (append boxes nil)))
    (if (= (length glues) (length boxes))
        (string-join (append (-interleave glues boxes)
                             (list last-glue)))
      (error "(length glues) + 1 != (length boxes)"))))

(defun etaf-kp--pixel-justify (string line-pixel)
  "Justify single STRING to LINE-PIXEL."
  (let* ((boxes (etaf-kp-boxes string))
         (hyphen (etaf-kp-hyphen-str string))
         (breaks (etaf-kp-line-breaks string line-pixel))
         (num (length breaks))
         (lines-glues (etaf-kp-line-glues string line-pixel))
         (glues-types (etaf-kp-glues-types string))
         (start 0) strings)
    (dotimes (i num)
      (let* ((end (nth i breaks))
             (line-boxes (cl-subseq boxes start end))
             (line-glues (mapcar #'etaf-kp-pixel-spacing
                                 (aref lines-glues i))))
        ;; not last line and glue is 'nws, should add hyphen
        (when (etaf-kp-hyphenate-p glues-types end)
          (setf (aref line-boxes (- end start 1))
                (concat (aref line-boxes (- end start 1)) hyphen)))
        (push (etaf-kp-combine-glues-and-boxes line-glues line-boxes)
              strings)
        (setq start end)))
    (mapconcat 'identity (nreverse strings) "\n")))

(defun etaf-kp-pixel-justify (string line-pixel &optional use-cache)
  "Justify multiline STRING to LINE-PIXEL.
When USE-CACHE is non-nil, use the cache for performance.
Default is nil, meaning cache is not used."
  (let ((etaf-kp-caches (if use-cache
                        etaf-kp-caches
                      (make-hash-table
                       :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))
        (strs (split-string string "\n")))
    (mapconcat (lambda (str)
                 (if (string-blank-p str)
                     ""
                   (etaf-kp--pixel-justify str line-pixel)))
               strs "\n")))

(defun etaf-kp-pixel-range-justify (string min-pixel max-pixel &optional use-cache)
  "Find the optimal breakpoint for STRING typesetting between
a MIN-PIXEL and MAX-PIXEL width and return a cons-cell. The car
of it is the ​​typeset tex and cdr is the best pixel.
When USE-CACHE is non-nil, use the cache for performance.
Default is nil, meaning cache is not used."
  (let* ((etaf-kp-caches (if use-cache
                         etaf-kp-caches
                       (make-hash-table
                        :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))
         (strings (split-string string "\n"))
         (best-pixel max-pixel)
         (best-cost-lst
          (mapcar (lambda (string)
                    (if (string-blank-p string)
                        0
                      (abs (etaf-kp-total-cost string max-pixel))))
                  strings))
         ;; get average cost of all lines' costs
         (best-cost (/ (float (apply #'+ best-cost-lst))
                       (length best-cost-lst)))
         (curr-pixel max-pixel))
    (while (>= curr-pixel min-pixel)
      (let* ((cost-lst
              (mapcar (lambda (string)
                        (if (string-blank-p string)
                            0
                          (abs (etaf-kp-total-cost string curr-pixel))))
                      strings))
             (curr-cost (/ (float (apply #'+ cost-lst))
                           (length cost-lst))))
        (when (< curr-cost best-cost)
          (progn
            (setq best-cost curr-cost)
            (setq best-pixel curr-pixel)))
        (cl-decf curr-pixel 1)))
    (cons (etaf-kp-pixel-justify string best-pixel use-cache) best-pixel)))

(provide 'etaf-kp)
