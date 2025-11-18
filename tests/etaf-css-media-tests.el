;;; etaf-css-media-tests.el --- Tests for etaf-css-media -*- lexical-binding: t; -*-

(require 'etaf-css)
(require 'etaf-css-media)
(require 'etaf-tml)
(require 'etaf-dom)
(require 'etaf-ert)

;;; 测试媒体类型匹配

(should-equal
 (etaf-css-media-match-type-p "all" '((type . screen)))
 t)

(should-equal
 (etaf-css-media-match-type-p "screen" '((type . screen)))
 t)

(should-equal
 (etaf-css-media-match-type-p "print" '((type . screen)))
 nil)

(should-equal
 (etaf-css-media-match-type-p "screen" '((type . print)))
 nil)

;;; 测试媒体特性解析

(should-equal
 (etaf-css-media-parse-feature "min-width: 768px")
 '(width min 768))

(should-equal
 (etaf-css-media-parse-feature "max-width: 1024px")
 '(width max 1024))

(should-equal
 (etaf-css-media-parse-feature "width: 800px")
 '(width equal 800))

;;; 测试媒体特性评估

(should-equal
 (etaf-css-media-evaluate-feature 'width 'min 768 '((width . 1024)))
 t)

(should-equal
 (etaf-css-media-evaluate-feature 'width 'min 768 '((width . 500)))
 nil)

(should-equal
 (etaf-css-media-evaluate-feature 'width 'max 1024 '((width . 800)))
 t)

(should-equal
 (etaf-css-media-evaluate-feature 'width 'max 1024 '((width . 1200)))
 nil)

(should-equal
 (etaf-css-media-evaluate-feature 'width 'equal 800 '((width . 800)))
 t)

;;; 测试完整媒体查询匹配

(should-equal
 (etaf-css-media-match-p "screen" '((type . screen) (width . 1024)))
 t)

(should-equal
 (etaf-css-media-match-p "print" '((type . screen) (width . 1024)))
 nil)

(should-equal
 (etaf-css-media-match-p "screen and (min-width: 768px)" '((type . screen) (width . 1024)))
 t)

(should-equal
 (etaf-css-media-match-p "screen and (min-width: 768px)" '((type . screen) (width . 500)))
 nil)

(should-equal
 (etaf-css-media-match-p "screen and (max-width: 1024px)" '((type . screen) (width . 800)))
 t)

(should-equal
 (etaf-css-media-match-p "screen and (min-width: 768px) and (max-width: 1024px)" 
                         '((type . screen) (width . 900)))
 t)

(should-equal
 (etaf-css-media-match-p "screen and (min-width: 768px) and (max-width: 1024px)" 
                         '((type . screen) (width . 500)))
 nil)

;;; 测试 @media 块提取

(should-equal
 (length (etaf-css-media-extract-at-media-blocks 
          "@media screen { .test { color: red; } }"))
 1)

(should-equal
 (nth 0 (car (etaf-css-media-extract-at-media-blocks 
              "@media screen { .test { color: red; } }")))
 "screen")

(should-equal
 (length (etaf-css-media-extract-at-media-blocks 
          "@media screen { .a { color: red; } }
           @media print { .b { color: blue; } }"))
 2)

;;; 测试带 @media 的 CSSOM 构建

(setq test-dom
      (etaf-tml-to-dom
       '(html
         (head
          (style "
            .desktop { display: block; }
            @media screen and (min-width: 768px) {
              .mobile { display: none; }
              .desktop { font-size: 18px; }
            }
            @media screen and (max-width: 767px) {
              .mobile { display: block; }
              .desktop { display: none; }
            }
          "))
         (body
          (div :class "mobile" "Mobile content")
          (div :class "desktop" "Desktop content")))))

;; 测试桌面环境（宽度 >= 768px）
(setq desktop-cssom (etaf-css-build-cssom test-dom '((type . screen) (width . 1024) (height . 768))))
(setq desktop-node (dom-by-class test-dom "desktop"))
(setq desktop-style (etaf-css-get-computed-style desktop-cssom desktop-node test-dom))

;; desktop 应该有 display: block 和 font-size: 18px
(should
 (assq 'display desktop-style))

(should
 (assq 'font-size desktop-style))

;; 测试移动环境（宽度 < 768px）
(setq mobile-cssom (etaf-css-build-cssom test-dom '((type . screen) (width . 375) (height . 667))))
(setq mobile-node (dom-by-class test-dom "mobile"))
(setq mobile-style (etaf-css-get-computed-style mobile-cssom mobile-node test-dom))

;; mobile 应该有 display: block
(should
 (assq 'display mobile-style))

;;; 测试空媒体查询（匹配所有）

(should-equal
 (etaf-css-media-match-p "" '((type . screen)))
 t)

(should-equal
 (etaf-css-media-match-p nil '((type . screen)))
 t)

;;; 测试媒体环境的完整性

;; 测试只提供 type 的环境
(should-equal
 (etaf-css-media-match-p "screen" '((type . screen)))
 t)

;; 测试包含完整属性的环境（type, width, height）
(should-equal
 (etaf-css-media-match-p "screen and (min-width: 768px)" 
                         '((type . screen) (width . 1024) (height . 768)))
 t)

;; 测试高度媒体查询
(should-equal
 (etaf-css-media-match-p "screen and (min-height: 600px)" 
                         '((type . screen) (width . 1024) (height . 768)))
 t)

(should-equal
 (etaf-css-media-match-p "screen and (max-height: 500px)" 
                         '((type . screen) (width . 1024) (height . 768)))
 nil)

;;; 测试 CSSOM 构建的幂等性

;; 确保多次使用相同 DOM 构建不同环境的 CSSOM 不会相互干扰
(setq test-dom-2
      (etaf-tml-to-dom
       '(html
         (head
          (style "
            .box { padding: 10px; }
            @media screen and (min-width: 768px) {
              .box { padding: 20px; }
            }
          "))
         (body
          (div :class "box" "Content")))))

;; 构建桌面环境 CSSOM
(setq cssom-desktop-2 (etaf-css-build-cssom test-dom-2 '((type . screen) (width . 1024) (height . 768))))
(setq box-desktop-2 (etaf-css-get-computed-style 
                     cssom-desktop-2 
                     (dom-by-class test-dom-2 "box") 
                     test-dom-2))

;; 构建移动环境 CSSOM
(setq cssom-mobile-2 (etaf-css-build-cssom test-dom-2 '((type . screen) (width . 375) (height . 667))))
(setq box-mobile-2 (etaf-css-get-computed-style 
                    cssom-mobile-2 
                    (dom-by-class test-dom-2 "box") 
                    test-dom-2))

;; 桌面环境应该有 padding: 20px（来自媒体查询）
(should
 (equal (cdr (assq 'padding box-desktop-2)) "20px"))

;; 移动环境应该有 padding: 10px（基础样式）
(should
 (equal (cdr (assq 'padding box-mobile-2)) "10px"))

;; 再次检查桌面环境（确保没有被移动环境影响）
(setq box-desktop-2-recheck (etaf-css-get-computed-style 
                             cssom-desktop-2 
                             (dom-by-class test-dom-2 "box") 
                             test-dom-2))
(should
 (equal (cdr (assq 'padding box-desktop-2-recheck)) "20px"))

(message "All etaf-css-media tests passed!")

(provide 'etaf-css-media-tests)
;;; etaf-css-media-tests.el ends here
