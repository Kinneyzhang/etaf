;; -*- lexical-binding: t; -*-

;;; related to font

;; (defun etaf-kp-cjk-char-p (char)
;;   "Return if char CHAR is cjk."
;;   (or
;;    ;; CJK统一表意文字（基本区）
;;    (<= #x4E00 char #x9FFF)
;;    ;; CJK扩展A区
;;    (<= #x3400 char #x4DBF)
;;    ;; CJK扩展B区（注意：超出16位范围）
;;    (and (<= #x20000 char) (<= char #x2A6DF))
;;    ;; CJK兼容/部首扩展等
;;    ;; CJK符号和标点
;;    (<= #x3000 char #x303F)
;;    ;; 日文假名
;;    (<= #x3040 char #x30FF)
;;    ;; 韩文谚文
;;    (<= #xAC00 char #xD7AF)
;;    ;; CJK兼容表意文字
;;    (<= #xF900 char #xFAFF)))

(defsubst etaf-kp-cjk-char-p (char)
  "Return non-nil if CHAR is a CJK character."
  (let ((entry (aref (category-table) char)))
    ;; Use ‘describe-categories’ for a full list of categories.
    ;; Another way is to use ‘char-script-table’ (see
    ;; ‘script-representative-chars’ for possible scripts), which is
    ;; not as convenient.
    (or (aref entry ?c) ; Chinese
        (aref entry ?h) ; Korean
        (aref entry ?j) ; Japanese
        )))

(defun etaf-kp-font-family (string &optional position)
  (format "%s" (font-get (font-at (or position 0) nil string) :family)))

(defun etaf-kp-font-monospace-p (font-family)
  (let* ((font (find-font (font-spec :family font-family)))
         (font-name (font-xlfd-name font))
         (type (nth 10 (split-string font-name "-" t))))
    ;; 'c' used in terminal
    (or (or (string= "m" type) (string= "c" type))
        (let ((info (font-info font-name)))
          (and info (> (length info) 4) 
               ;; 等宽字体的核心标志: 最大宽度等于平均宽度
               (= (aref info 7) (aref info 11)))))))

(defun etaf-kp-get-latin-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let ((char (char-after)))
                  (not (or (and (>= char ?a) (<= char ?z))
                           (and (>= char ?A) (<= char ?Z))))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun etaf-kp-get-cjk-letter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (< (point) (point-max))
                (let* ((char (char-after))
                       (width (char-width char)))
                  (or (or (= 1 width) (= 0 width))
                      (not (etaf-kp-cjk-char-p char)))))
      (forward-char 1))
    (unless (eobp)
      (buffer-substring (point) (1+ (point))))))

(defun etaf-kp-monospace-p (string)
  "判断字符串中的拉丁字母的字体是否等宽，返回字体名称"
  (if-let* ((letter (etaf-kp-get-latin-letter string))
            (font-family (etaf-kp-font-family letter)))
      ;; return monospace font family
      (when (etaf-kp-font-monospace-p font-family)
        font-family)
    ;; no latin letter in string, use default
    (face-attribute 'default :family)))

(defun etaf-kp-word-spacing-pixel (string)
  ;; font is monospace, use the pixel of blank
  ;; as word spacing pixel
  (if-let ((font-family (etaf-kp-monospace-p string)))
      (string-pixel-width
       (propertize " " 'face `(:family ,font-family)))
    (let* ((letter (etaf-kp-get-latin-letter string))
           (font-family (etaf-kp-font-family letter)))
      (string-pixel-width
       (propertize
        " " 'face `(:family ,font-family))))))

(defun etaf-kp-latin-font (string)
  (if-let ((letter (etaf-kp-get-latin-letter string)))
      (etaf-kp-font-family letter)
    (face-attribute 'default :family)))

(defun etaf-kp-cjk-font (string)
  (if-let ((letter (etaf-kp-get-cjk-letter string)))
      (etaf-kp-font-family letter)
    (etaf-kp-font-family "牛")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etaf-kp-start-process-with-callback
    (process-name command-args callback
                  &optional output-buffer)
  "执行命令（带参数）并在完成后调用回调"
  (let* ((buffer-name (generate-new-buffer-name
                       (or output-buffer "*EKP Process Output*")))
         (process (apply #'start-process process-name
                         buffer-name command-args)))
    (set-process-sentinel
     process
     `(lambda (proc event)
        (if (string-match-p "finished" event)
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  (funcall ',callback proc (process-buffer proc))
                (when (buffer-live-p (process-buffer proc))
                  (kill-buffer (process-buffer proc)))))
          (message "%s, please check %s" (string-trim event)
                   ,buffer-name))))
    process))

(defun etaf-kp-rust-module-reload (module)
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module))))
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(defun etaf-kp-module-dir ()
  (when-let ((root-dir (etaf-kp-root-dir)))
    (expand-file-name "ekp_rust" root-dir)))

(defun etaf-kp-module-file ()
  (when-let* ((module-dir (etaf-kp-module-dir))
              (filename (cond ((eq system-type 'darwin) "libekp.dylib")
                              ((eq system-type 'windows-nt) "ekp.dll")
                              (t "libekp.so"))))
    (expand-file-name (concat "target/release/" filename) module-dir)))

(defun etaf-kp-module-load ()
  "Load rust module of ekp."
  (if (executable-find "cargo")
      (let ((file (etaf-kp-module-file)))
        (if file
            (etaf-kp-rust-module-reload file)
          (etaf-kp-module-build)))
    (error "Please install cargo and add it to executable path!")))

(defun etaf-kp-module-build ()
  "Reload ekp rust module."
  (interactive)
  (if (executable-find "cargo")
      (etaf-kp-start-process-with-callback
       "etaf-kp-build"
       (cond
        ((eq system-type 'windows-nt)
         `("cmd.exe" "/c" ,(format "cd %s && cargo build -r"
                                   (etaf-kp-module-dir))))
        (t `("zsh" "-c" ,(format "cd %s && cargo build -r"
                                 (etaf-kp-module-dir)))))
       (lambda (proc buffer)
         (etaf-kp-rust-module-reload (etaf-kp-module-file))
         (message "ekp rust module reload success!")))
    (error "Please install cargo and add it to executable path!")))

(defmacro etaf-kp-setq (sym val)
  "Set the value of symbol SYM to VAL. If VAL is nil, set to
the value of [SYM]-default."
  `(setq ,sym (or ,val ,(intern (concat (symbol-name sym)
                                        "-default")))))

(defun etaf-kp-pixel-spacing (pixel)
  "Return a pixel spacing with a PIXEL pixel width."
  (if (= pixel 0)
      ""
    (propertize " " 'display `(space :width (,pixel)))))

(defun etaf-kp-cjk-fw-punct-p (str)
  "Return if CHAR is CJK full-width punctuation."
  (let ((char (seq-first str)))
    (or (equal (char-syntax char) ?.)
        (and (>= char #x3000) (<= char #x303F))
        (and (>= char #xFF00) (<= char #xFF60)))))

(defun etaf-kp-split-to-boxes (string)
  (if (string-blank-p string)
      (vector string)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (let ((state (char-width (seq-first string)))
            curr-str prev-str boxes)
        (while (not (eobp))
          (let* ((str (buffer-substring (point) (1+ (point)))))
            (if (or (string-blank-p str)
                    ;; 零宽 unicode
                    (= 0 (string-width str)))
                (when curr-str
                  (push curr-str boxes)
                  (setq curr-str nil))
              (if (= state 1)
                  (cond
                   ((= 1 (string-width str))
                    (setq curr-str (concat curr-str str)))
                   ((= 2 (string-width str))
                    (when curr-str
                      (push curr-str boxes)
                      (setq curr-str nil))
                    ;; switch to state 2
                    (push str boxes)
                    (setq state 2)))
                (cond
                 ((= 2 (string-width str))
                  (if (etaf-kp-cjk-fw-punct-p str)
                      ;; cjk punct 连在前一个字符后面
                      (progn
                        (push (concat prev-str str) boxes)
                        (setq prev-str nil))
                    (when prev-str (push prev-str boxes))
                    (setq prev-str str)))
                 ((= 1 (string-width str))
                  (when prev-str
                    (push prev-str boxes)
                    (setq prev-str nil))
                  ;; switch to state 1
                  (setq curr-str (concat curr-str str))
                  (setq state 1))))))
          (forward-char 1))
        ;; push CJK char at the end of buffer to boxes
        (when prev-str
          (push prev-str boxes))
        ;; push latin word at the end of buffer to boxes
        (when curr-str
          (push curr-str boxes))
        (vconcat (nreverse boxes))))))

(defun etaf-kp-clear-caches ()
  (interactive)
  (setq etaf-kp-caches
        (make-hash-table
         :test 'equal :size 100 :rehash-size 1.5 :weakness nil)))

;; (defun etaf-kp-split-to-boxes (string)
;;   "Split STRING into a vector: English by word, Chinese
;; by character, punctuation attached to previous unit."
;;   (with-temp-buffer
;;     (insert string)
;;     (goto-char (point-min))
;;     (let (words (index 0))
;;       (while (not (eobp))
;;         ;; skip whitespace
;;         (skip-syntax-forward "-")
;;         (unless (eobp)
;;           (let ((start (point)))
;;             (if (etaf-kp-cjk-char-p (char-after))
;;                 (forward-char 1)
;;               (forward-word 1)
;;               ;; process float number
;;               (when-let* ((char1 (char-after (point)))
;;                           (char2 (char-after (1+ (point))))
;;                           (_ (and (eq char1 ?.) (<= ?0 char2 ?9))))
;;                 (forward-word 1))
;;               ;; process hyphen and contraction
;;               (while (or (eq (char-after (point)) ?-)
;;                          (eq (char-after (point)) ?')
;;                          (eq (char-after (point)) ?/))
;;                 (forward-word 1)))
;;             ;; attach punctuation
;;             (while (and (not (eobp))
;;                         (with-syntax-table (standard-syntax-table)
;;                           (eq (char-syntax (char-after)) ?.)))
;;               ;; (message "(char-after):%s" (char-after))
;;               (forward-char 1))
;;             (push (buffer-substring start (point)) words))))
;;       (vconcat (nreverse words)))))

;; (defvar etaf-kp-par-num 8)
;; (defun etaf-kp-strings (string n)
;;   "Split STRING to average N parts but don't split in a word."
;;   ;; used for rust
;;   (let* ((size (length string))
;;          (each-size (/ size n))
;;          (str-ends (--map (if (= it n) size (* it each-size))
;;                           (number-sequence 1 n)))
;;          regions)
;;     (with-temp-buffer
;;       (insert string)
;;       (goto-char (point-min))
;;       (let ((str-start 0)
;;             (prev-end 0))
;;         (dolist (str-end str-ends)
;;           (when (> str-end prev-end)
;;             (goto-char (1+ str-end))
;;             (while (and (not (eobp))
;;                         (not (eq ?  (char-after)))
;;                         (< (char-width (char-after)) 2))
;;               (forward-char 1))
;;             (setq str-end (1- (point)))
;;             (push (cons str-start str-end) regions)
;;             (setq prev-end str-end)
;;             (setq str-start str-end)))))
;;     (vconcat (--map
;;               (substring string (car it) (cdr it))
;;               (nreverse regions)))))

(provide 'etaf-kp-utils)
