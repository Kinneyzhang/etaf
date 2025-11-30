(require 'etaf-ert)
(require 'etaf-layout-interactive)

;;; ============================================================
;;; 缓存管理测试
;;; ============================================================

;;; Test etaf-layout-caches-init
(let ((test-buffer (generate-new-buffer "*test-cache*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (should etaf-layout-caches)
        (should (hash-table-p etaf-layout-caches)))
    (kill-buffer test-buffer)))

;;; Test etaf-layout-cache-get/set
(let ((test-buffer (generate-new-buffer "*test-cache-get-set*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (etaf-layout-cache-set "test-uuid" :content-lines '("line1" "line2"))
        (should-equal (etaf-layout-cache-get "test-uuid" :content-lines)
                      '("line1" "line2")))
    (kill-buffer test-buffer)))

;;; Test etaf-layout-cache-put
(let ((test-buffer (generate-new-buffer "*test-cache-put*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (etaf-layout-cache-put "uuid-1"
                               '(:content-lines ("a" "b" "c")
                                 :content-linum 3
                                 :content-height 2))
        (should-equal (etaf-layout-cache-get "uuid-1" :content-linum) 3)
        (should-equal (etaf-layout-cache-get "uuid-1" :content-height) 2))
    (kill-buffer test-buffer)))

;;; ============================================================
;;; 滚动信息计算测试
;;; ============================================================

;;; Test etaf-layout-v-scroll-steps - no overflow
(should-equal (etaf-layout-v-scroll-steps 5 5) nil)

;;; Test etaf-layout-v-scroll-steps - small overflow
(should-equal (etaf-layout-v-scroll-steps 7 5) '(1 1))

;;; Test etaf-layout-v-scroll-steps - large overflow
(let ((steps (etaf-layout-v-scroll-steps 20 5)))
  (should steps)
  (should (= (length steps) 4))
  (should (= (apply #'+ steps) 15)))

;;; Test etaf-layout-v-scroll-thumb-height - no overflow
(should-equal (etaf-layout-v-scroll-thumb-height 5 5) 5)

;;; Test etaf-layout-v-scroll-thumb-height - small overflow
(should-equal (etaf-layout-v-scroll-thumb-height 7 5) 3)

;;; Test etaf-layout-v-scroll-thumb-height - large overflow
(should-equal (etaf-layout-v-scroll-thumb-height 20 5) 1)

;;; Test etaf-layout-v-scroll-thumb-offset
(should-equal (etaf-layout-v-scroll-thumb-offset '(1 1) 0) 0)
(should-equal (etaf-layout-v-scroll-thumb-offset '(1 1) 1) 1)
(should-equal (etaf-layout-v-scroll-thumb-offset '(1 1) 2) 2)

;;; ============================================================
;;; Keymap 测试
;;; ============================================================

;;; Test etaf-layout-scroll-map creation
(let ((map (etaf-layout-scroll-map)))
  (should (keymapp map))
  ;; Check that scroll keys are defined
  (should (lookup-key map "n"))
  (should (lookup-key map "p")))

;;; ============================================================
;;; 增量更新 API 测试
;;; ============================================================

;;; Test etaf-buffer--style-to-face
(should-equal (etaf-buffer--style-to-face '((color . "red")))
              '(:foreground "red"))

(should-equal (etaf-buffer--style-to-face '((background-color . "blue")))
              '(:background "blue"))

(should-equal (etaf-buffer--style-to-face '((font-weight . "bold")))
              '(:weight bold))

(should-equal (etaf-buffer--style-to-face '((font-style . "italic")))
              '(:slant italic))

(should-equal (etaf-buffer--style-to-face '((text-decoration . "underline")))
              '(:underline t))

;;; Test etaf-buffer--style-to-face with multiple properties
(let ((face (etaf-buffer--style-to-face '((color . "red")
                                           (background . "yellow")
                                           (font-weight . "bold")))))
  (should-equal (plist-get face :foreground) "red")
  (should-equal (plist-get face :background) "yellow")
  (should-equal (plist-get face :weight) 'bold))

;;; Test etaf-buffer-update-text
(let ((test-buffer (generate-new-buffer "*test-update-text*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (insert (propertize "old text" 'etaf-layout-content-line "test-uuid"))
        (etaf-buffer-update-text test-buffer "test-uuid" "new text")
        (should (string-match "new text" (buffer-string))))
    (kill-buffer test-buffer)))

;;; Test etaf-buffer-update-style
(let ((test-buffer (generate-new-buffer "*test-update-style*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (insert (propertize "test text" 'etaf-layout-content-line "style-uuid"))
        (etaf-buffer-update-style test-buffer "style-uuid" '((color . "red")))
        (let ((face (get-text-property 1 'face)))
          (should face)))
    (kill-buffer test-buffer)))

;;; Test etaf-buffer-update-attribute
(let ((test-buffer (generate-new-buffer "*test-update-attr*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (insert (propertize "test" 'etaf-layout-content-line "attr-uuid"))
        (etaf-buffer-update-attribute test-buffer "attr-uuid" 'custom-prop "value")
        (should-equal (get-text-property 1 'custom-prop) "value"))
    (kill-buffer test-buffer)))

;;; Test etaf-buffer-get-region-text
(let ((test-buffer (generate-new-buffer "*test-get-text*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (insert (propertize "hello world" 'etaf-layout-content-line "text-uuid"))
        (should-equal (etaf-buffer-get-region-text test-buffer "text-uuid")
                      "hello world"))
    (kill-buffer test-buffer)))

;;; Test etaf-buffer-get-region-properties
(let ((test-buffer (generate-new-buffer "*test-get-props*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (etaf-layout-caches-init)
        (insert (propertize "test" 
                            'etaf-layout-content-line "props-uuid"
                            'custom-attr "custom-value"))
        (let ((props (etaf-buffer-get-region-properties test-buffer "props-uuid")))
          (should (plist-get props 'etaf-layout-content-line))
          (should-equal (plist-get props 'custom-attr) "custom-value")))
    (kill-buffer test-buffer)))

;;; ============================================================
;;; etaf-layout-setup-scroll-region 测试
;;; ============================================================

;;; Test etaf-layout-setup-scroll-region
(let ((test-buffer (generate-new-buffer "*test-setup-scroll*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (let* ((content-lines '("line1" "line2" "line3" "line4" "line5"))
               (result (etaf-layout-setup-scroll-region
                        "line1\nline2\nline3"
                        "scroll-uuid"
                        content-lines
                        3)))
          ;; Check that keymap is set
          (should (get-text-property 0 'keymap result))
          ;; Check that uuid is set
          (should-equal (get-text-property 0 'etaf-layout-content-line result)
                        "scroll-uuid")
          ;; Check cache was created
          (should-equal (etaf-layout-cache-get "scroll-uuid" :content-linum) 5)
          (should-equal (etaf-layout-cache-get "scroll-uuid" :content-height) 3)))
    (kill-buffer test-buffer)))

;;; Test etaf-buffer-find-region-by-uuid
(let ((test-buffer (generate-new-buffer "*test-find-region*")))
  (unwind-protect
      (with-current-buffer test-buffer
        (insert "before ")
        (insert (propertize "target text" 'etaf-layout-content-line "find-uuid"))
        (insert " after")
        (let ((region (etaf-buffer-find-region-by-uuid "find-uuid")))
          (should region)
          (should (= (car region) 8))
          (should (= (cdr region) 19))))
    (kill-buffer test-buffer)))

(provide 'etaf-layout-interactive-tests)
