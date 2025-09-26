;; -*- lexical-binding: t -*-

(require 'etml-block)
(require 'etml-flex)

;; (defclass etml-node (etml-block)
;;   ((blocks :initarg :blocks :type (list-of etml-block))
;;    (display :initarg :display)))

'(div :display 'flex :direction 'column
      :border t :padding '(4 . 1) :margin '(3 . 1)
      (div "Test etml-flex :shrink")
      (div "")
      (div (div :display 'flex :width '(300) :column-gap 8
                (div :border '("#aaaaaa" . "lightGreen") :padding t
                     "etml flex test 1")
                (div :border '("#aaaaaa" . "lightGreen") :padding t
                     "etml flex test 2")
                (div :border '("#aaaaaa" . "lightGreen") :padding t
                     "etml flex test 3")
                (div :border '("#aaaaaa" . "lightGreen") :padding t
                     "etml flex test 4")
                (div :border '("#aaaaaa" . "lightGreen") :padding t
                     "etml flex test 5"))))

;; 写 emacs-lisp 代码解析下面的 sexp。每个sexp的第一个元素是为 div 时表示普通的块组件，可以被解析为 etml-flex, etml-flex-item, etml-block 对象其中的一种。从第二个元素开始是一个 plist 表示对象的各种属性，最后的一个或多个元素表示对象的内容。

;; 具体来说，当对象有 :display 'flex 属性时，div被解析为 etml-flex，其余属性原样传入到 etml-flex 属性中，最后的内容部分放在 :content 属性中，如果多个内容则形成列表。

;; etml-flex 下的子元素 div 全部解析为 etml-flex-item 对象，同样的，属性元素照搬，内容部分放在 :content 中

(defun etml-parse (sexp)
  
  )

(provide 'etml)
