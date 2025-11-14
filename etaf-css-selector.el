;;; ecss-selector.el --- ECSS选择器解析器 -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Based on postecss-selector-parser
;; Keywords: ecss, parser, selector
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这是一个ECSS选择器解析器的Emacs Lisp实现，基于postecss-selector-parser项目。
;; 它将ECSS选择器字符串解析为抽象语法树（AST），支持各种ECSS选择器语法。
;;
;; 主要功能：
;; - 词法分析（Tokenization）
;; - 语法分析（Parsing）
;; - AST构建和遍历
;; - 选择器修改和序列化
;;
;; 使用示例：
;;
;;   (ecss-selector-parse "div.class#id > span:hover")
;;   ;; => AST树结构
;;
;;   (ecss-selector-walk
;;    (ecss-selector-parse "div.class")
;;    (lambda (node) (message "Node: %s" (plist-get node :type))))

;;; Code:

(require 'cl-lib)

(defconst ecss-token-types
  '((ampersand . 38)          ; &
    (asterisk . 42)           ; *
    (comma . 44)              ; ,
    (colon . 58)              ; :
    (semicolon . 59)          ; ;
    (open-paren . 40)         ; (
    (close-paren . 41)        ; )
    (open-square . 91)        ; [
    (close-square . 93)       ; ]
    (dollar . 36)             ; $
    (tilde . 126)             ; ~
    (caret . 94)              ; ^
    (plus . 43)               ; +
    (equals . 61)             ; =
    (pipe . 124)              ; |
    (greater-than . 62)       ; >
    (space . 32)              ; 空格
    (single-quote . 39)       ; '
    (double-quote . 34)       ; "
    (slash . 47)              ; /
    (bang . 33)               ; !
    (backslash . 92)          ; \
    (cr . 13)                 ; \r
    (feed . 12)               ; \f
    (newline . 10)            ; \n
    (tab . 9)                 ; \t
    ;; 特殊token类型
    (str . -1)                ; 字符串
    (comment . -2)            ; 注释
    (word . -3)               ; 单词
    (combinator . -4))        ; 组合器
  "ECSS token类型映射表。")

(defconst ecss-word-delimiters
  (let ((delims (make-hash-table)))
    (dolist (type '(space tab newline cr feed
                    ampersand asterisk bang comma colon semicolon
                    open-paren close-paren open-square close-square
                    single-quote double-quote plus pipe tilde
                    greater-than equals dollar caret slash))
      (puthash (cdr (assq type ecss-token-types)) t delims))
    delims)
  "单词分隔符集合。")

(defconst ecss-whitespace-tokens
  (let ((tokens (make-hash-table)))
    (dolist (type '(space tab newline cr feed))
      (puthash (cdr (assq type ecss-token-types)) t tokens))
    tokens)
  "空白字符token集合。")

(defconst ecss-hex-chars
  (let ((hex (make-hash-table)))
    (dolist (char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                    ?a ?b ?c ?d ?e ?f ?A ?B ?C ?D ?E ?F))
      (puthash char t hex))
    hex)
  "十六进制字符集合。")

;;; 词法分析器（Tokenizer）

(defun ecss-consume-escape (css start)
  "从CSS字符串中消费一个转义序列。
CSS是输入字符串，START是反斜杠的位置。
返回转义序列的结束位置。"
  (let ((next start)
        (code (and (< (1+ start) (length css))
                   (aref css (1+ start)))))
    (when code
      (if (gethash code ecss-hex-chars)
          ;; 十六进制转义
          (let ((hex-digits 0))
            (while (and (< hex-digits 6)
                        (< (1+ next) (length css))
                        (gethash (aref css (1+ next)) ecss-hex-chars))
              (cl-incf next)
              (cl-incf hex-digits))
            ;; 如果少于6个十六进制字符，空格结束转义
            (when (and (< hex-digits 6)
                       (< (1+ next) (length css))
                       (= (aref css (1+ next))
                          (cdr (assq 'space ecss-token-types))))
              (cl-incf next)))
        ;; 单字符转义
        (cl-incf next)))
    next))

(defun ecss-consume-word (css start)
  "从CSS字符串中消费一个单词。
CSS是输入字符串，START是单词的起始位置。
返回单词的结束位置。"
  (cl-block nil
    (let ((next start)
          (code nil))
      (while (< next (length css))
        (setq code (aref css next))
        (cond
         ((gethash code ecss-word-delimiters)
          (cl-return (1- next)))
         ((= code (cdr (assq 'backslash ecss-token-types)))
          (setq next (1+ (ecss-consume-escape css next))))
         (t
          (cl-incf next))))
      (1- next))))

(defun ecss-tokenize (css)
  "对ECSS选择器字符串进行词法分析。
返回token列表，每个token是一个向量：
[TYPE START-LINE START-COL END-LINE END-COL START-POS END-POS]"
  (let* ((length (length css))
         (tokens '()) (offset -1)
         (line 1) (start 0) (end 0)
         code content end-column end-line
         escaped escape-pos last lines
         next next-line next-offset quote token-type)
    (while (< start length)
      (setq code (aref css start))
      
      ;; 更新行号
      (when (= code (cdr (assq 'newline ecss-token-types)))
        (setq offset start
              line (1+ line)))
      (cond
       ;; 空白字符
       ((memq code (mapcar (lambda (type)
                             (cdr (assq type ecss-token-types)))
                           '(space tab newline cr feed)))
        (setq next start)
        (while (and (< (cl-incf next) length)
                    (memq (aref css next)
                          (mapcar (lambda (type)
                                    (cdr (assq type ecss-token-types)))
                                  '(space tab newline cr feed))))
          (when (= (aref css next) (cdr (assq 'newline ecss-token-types)))
            (setq offset next
                  line (1+ line))))
        (setq token-type (cdr (assq 'space ecss-token-types))
              end-line line
              end-column (- next offset 1)
              end next))
       ;; 组合器: + > ~ |
       ((memq code (mapcar (lambda (type)
                             (cdr (assq type ecss-token-types)))
                           '(plus greater-than tilde pipe)))
        (setq next start)
        (while (and (< (cl-incf next) length)
                    (memq (aref css next)
                          (mapcar (lambda (type)
                                    (cdr (assq type ecss-token-types)))
                                  '(plus greater-than tilde pipe)))))
        (setq token-type (cdr (assq 'combinator ecss-token-types))
              end-line line
              end-column (- start offset)
              end next))
       ;; 单字符token
       ((memq code
              (mapcar (lambda (type)
                        (cdr (assq type ecss-token-types)))
                      '(asterisk ampersand bang comma equals dollar caret
                                 open-square close-square colon semicolon
                                 open-paren close-paren)))
        (setq next start
              token-type code
              end-line line
              end-column (- start offset)
              end (1+ next)))
       ;; 字符串
       ((memq code (mapcar (lambda (type)
                             (cdr (assq type ecss-token-types)))
                           '(single-quote double-quote)))
        (setq quote
              (if (= code (cdr (assq 'single-quote ecss-token-types)))
                  "'" "\"")
              next start
              escaped nil)
        (catch 'done
          (while t
            (setq next (string-match-p (regexp-quote quote)
                                       css (1+ next)))
            (unless next
              (error "Unclosed quote"))
            (setq escape-pos next
                  escaped nil)
            (while (and (> escape-pos 0)
                        (= (aref css (1- escape-pos))
                           (cdr (assq 'backslash ecss-token-types))))
              (cl-decf escape-pos)
              (setq escaped (not escaped)))
            (unless escaped
              (throw 'done nil))))
        (setq token-type (cdr (assq 'str ecss-token-types))
              end-line line
              end-column (- start offset)
              end (1+ next)))
       ;; 注释或斜杠
       ((= code (cdr (assq 'slash ecss-token-types)))
        (if (and (< (1+ start) length)
                 (= (aref css (1+ start))
                    (cdr (assq 'asterisk ecss-token-types))))
            ;; 注释
            (progn
              (setq next (string-match-p "\\*/" css (+ start 2)))
              (unless next
                (error "Unclosed comment"))
              (setq content (substring css start (+ next 2))
                    lines (split-string content "\n" t)
                    last (1- (length lines)))
              (if (> last 0)
                  (setq next-line (+ line last)
                        next-offset (- (+ next 1)
                                       (length (nth last lines))))
                (setq next-line line
                      next-offset offset))
              (setq token-type (cdr (assq 'comment ecss-token-types))
                    line next-line
                    end-line next-line
                    end-column (- (1+ next) next-offset)
                    end (+ next 2)))
          ;; 单独的斜杠
          (setq next start
                token-type code
                end-line line
                end-column (- start offset)
                end (1+ next))))
       ;; 单词
       (t (setq next (ecss-consume-word css start)
                token-type (cdr (assq 'word ecss-token-types))
                end-line line
                end-column (- next offset)
                end (1+ next))))
      ;; 添加token
      (push (vector token-type line (- start offset)
                    end-line end-column start end)
            tokens)
      ;; 重置offset
      (when next-offset
        (setq offset next-offset
              next-offset nil))
      (setq start end))
    (nreverse tokens)))

;;; 节点构造函数

(defconst ecss-node-types
  '(root selector tag class id attribute pseudo
         universal combinator nesting comment string)
  "ECSS AST节点类型列表。")

(defun ecss-make-node (type &rest props)
  "创建一个ECSS AST节点。TYPE是节点类型，PROPS是属性列表。"
  (if (member type ecss-node-types)
      (let ((node (list :type type)))
        (while props
          (setq node (plist-put node (car props) (cadr props))
                props (cddr props)))
        (unless (plist-get node :spaces)
          (setq node (plist-put node :spaces (list :before "" :after ""))))
        node)
    (error "Type %S is not a valid ecss node type!" type)))

(defun ecss-make-root (&rest props)
  "创建根节点。"
  (apply #'ecss-make-node 'root :nodes '() props))

(defun ecss-make-selector (&rest props)
  "创建选择器节点。"
  (apply #'ecss-make-node 'selector :nodes '() props))

(defun ecss-make-tag (value &rest props)
  "创建标签选择器节点。"
  (apply #'ecss-make-node 'tag :value value props))

(defun ecss-make-class (value &rest props)
  "创建类选择器节点。"
  (apply #'ecss-make-node 'class :value value props))

(defun ecss-make-id (value &rest props)
  "创建ID选择器节点。"
  (apply #'ecss-make-node 'id :value value props))

(defun ecss-make-attribute (&rest props)
  "创建属性选择器节点。"
  (apply #'ecss-make-node 'attribute props))

(defun ecss-make-pseudo (value &rest props)
  "创建伪类/伪元素节点。"
  (apply #'ecss-make-node 'pseudo :value value props))

(defun ecss-make-universal (&rest props)
  "创建通配符选择器节点。"
  (apply #'ecss-make-node 'universal :value "*" props))

(defun ecss-make-combinator (value &rest props)
  "创建组合器节点。"
  (apply #'ecss-make-node 'combinator :value value props))

(defun ecss-make-nesting (&rest props)
  "创建嵌套选择器节点。"
  (apply #'ecss-make-node 'nesting :value "&" props))

(defun ecss-make-comment (value &rest props)
  "创建注释节点。"
  (apply #'ecss-make-node 'comment :value value props))

;;; 节点操作函数

(defun ecss-node-append (container node)
  "将节点NODE添加到容器CONTAINER的子节点列表中。"
  (let ((nodes (plist-get container :nodes)))
    (plist-put container :nodes (append nodes (list node)))))

(defun ecss-node-to-string (node)
  "将节点转换为字符串。"
  (let ((type (plist-get node :type))
        (value (plist-get node :value))
        (spaces (plist-get node :spaces))
        (nodes (plist-get node :nodes)))
    (concat
     (or (plist-get spaces :before) "")
     (cond
      ((eq type 'root)
       (mapconcat #'ecss-node-to-string nodes ","))
      ((eq type 'selector)
       (mapconcat #'ecss-node-to-string nodes ""))
      ((eq type 'tag) value)
      ((eq type 'class) (concat "." value))
      ((eq type 'id) (concat "#" value))
      ((eq type 'universal) "*")
      ((eq type 'combinator) value)
      ((eq type 'pseudo) value)
      ((eq type 'nesting) "&")
      ((eq type 'comment) value)
      ((eq type 'attribute)
       (let ((attr (plist-get node :attribute))
             (op (plist-get node :operator))
             (val (plist-get node :value))
             (quote (plist-get node :quote-mark)))
         (concat "["
                 (or attr "")
                 (or op "")
                 (when val
                   (if quote
                       (concat quote val quote)
                     val))
                 "]")))
      (t (or value "")))
     (or (plist-get spaces :after) ""))))

;;; 语法分析器（Parser）

(cl-defstruct ecss-parser
  "ECSS解析器结构。"
  css             ; 输入ECSS字符串
  tokens          ; token数组
  position        ; 当前位置
  root            ; 根节点
  current         ; 当前选择器节点
  spaces-before)  ; 累积的前置空白

(defun ecss-parser-curr-token (parser)
  "获取解析器的当前token。"
  (let ((pos (ecss-parser-position parser)))
    (when (< pos (length (ecss-parser-tokens parser)))
      (aref (ecss-parser-tokens parser) pos))))

(defun ecss-parser-next-token (parser)
  "获取解析器的下一个token。"
  (let ((pos (1+ (ecss-parser-position parser))))
    (when (< pos (length (ecss-parser-tokens parser)))
      (aref (ecss-parser-tokens parser) pos))))

(defun ecss-parser-content (parser &optional token)
  "获取token的内容字符串。"
  (let ((tok (or token (ecss-parser-curr-token parser))))
    (when tok
      (substring (ecss-parser-css parser)
                 (aref tok 5)  ; START-POS
                 (aref tok 6)))))  ; END-POS

(defun ecss-parser-new-node (parser node)
  "添加新节点到当前选择器。"
  ;; 将累积的空白附加到节点的 :before
  (when (ecss-parser-spaces-before parser)
    (let ((spaces (plist-get node :spaces)))
      (plist-put spaces :before (ecss-parser-spaces-before parser)))
    (setf (ecss-parser-spaces-before parser) ""))
  (ecss-node-append (ecss-parser-current parser) node)
  node)

(defun ecss-parser-space (parser)
  "处理空白字符。"
  ;; 累积空白字符，稍后附加到下一个节点
  (let ((space-content (ecss-parser-content parser)))
    (setf (ecss-parser-spaces-before parser)
          (concat (or (ecss-parser-spaces-before parser) "")
                  space-content)))
  (cl-incf (ecss-parser-position parser)))

(defun ecss-parser-comment (parser)
  "处理注释。"
  (let* ((token (ecss-parser-curr-token parser))
         (content (ecss-parser-content parser token)))
    (ecss-parser-new-node
     parser
     (ecss-make-comment content))
    (cl-incf (ecss-parser-position parser))))

(defun ecss-parser-comma (parser)
  "处理逗号（新选择器）。"
  (let ((selector (ecss-make-selector)))
    (ecss-node-append (ecss-parser-root parser) selector)
    (setf (ecss-parser-current parser) selector))
  (cl-incf (ecss-parser-position parser)))

(defun ecss-parser-word (parser)
  "处理单词token。"
  (let* ((content (ecss-parser-content parser))
         (i 0)
         (len (length content))
         nodes
         (first-node t))
    ;; 分割单词为多个节点
    (while (< i len)
      (let ((ch (aref content i)))
        (cond
         ((= ch ?.)  ; 类选择器
          (let ((start (cl-incf i))
                (end i))
            (while (and (< end len)
                        (not (memq (aref content end) '(?. ?# ?\[))))
              (cl-incf end))
            (push (ecss-make-class (substring content start end)) nodes)
            (setq i end)))
         
         ((= ch ?#)  ; ID选择器
          (let ((start (cl-incf i))
                (end i))
            (while (and (< end len)
                        (not (memq (aref content end) '(?. ?# ?\[))))
              (cl-incf end))
            (push (ecss-make-id (substring content start end)) nodes)
            (setq i end)))
         
         (t  ; 标签选择器
          (let ((start i)
                (end i))
            (while (and (< end len)
                        (not (memq (aref content end) '(?. ?# ?\[))))
              (cl-incf end))
            (when (> end start)
              (push (ecss-make-tag (substring content start end)) nodes))
            (setq i end))))))
    
    ;; 添加节点 - 只有第一个节点获得累积的空白
    (dolist (node (nreverse nodes))
      (if first-node
          (progn
            (ecss-parser-new-node parser node)
            (setq first-node nil))
        ;; 后续节点直接添加，不获得累积的空白
        (ecss-node-append (ecss-parser-current parser) node)))
    (cl-incf (ecss-parser-position parser))))

(defun ecss-parser-universal (parser)
  "处理通配符选择器。"
  (ecss-parser-new-node parser (ecss-make-universal))
  (cl-incf (ecss-parser-position parser)))

(defun ecss-parser-pseudo (parser)
  "处理伪类/伪元素。"
  (let ((content (ecss-parser-content parser))
        (next (ecss-parser-next-token parser)))
    
    ;; 移动到冒号之后
    (cl-incf (ecss-parser-position parser))
    
    ;; 检查是否为双冒号
    (when (and (ecss-parser-curr-token parser)
               (= (aref (ecss-parser-curr-token parser) 0) 
                  (cdr (assq 'colon ecss-token-types))))
      (setq content (concat content (ecss-parser-content parser)))
      (cl-incf (ecss-parser-position parser)))
    
    ;; 消费伪类/伪元素名称（如果当前是word token）
    (when (and (ecss-parser-curr-token parser)
               (= (aref (ecss-parser-curr-token parser) 0) 
                  (cdr (assq 'word ecss-token-types))))
      (setq content (concat content (ecss-parser-content parser)))
      (cl-incf (ecss-parser-position parser)))
    
    ;; 处理带参数的伪类（如 :nth-child(2)）
    (when (and (ecss-parser-curr-token parser)
               (= (aref (ecss-parser-curr-token parser) 0) 
                  (cdr (assq 'open-paren ecss-token-types))))
      ;; 消费开括号和所有内容直到闭括号
      (catch 'done
        (while (< (ecss-parser-position parser)
                  (length (ecss-parser-tokens parser)))
          (let ((token (ecss-parser-curr-token parser)))
            (setq content (concat content (ecss-parser-content parser)))
            (cl-incf (ecss-parser-position parser))
            (when (= (aref token 0)
                     (cdr (assq 'close-paren ecss-token-types)))
              (throw 'done nil))))))
    
    (ecss-parser-new-node parser (ecss-make-pseudo content))))

(defun ecss-parser-combinator (parser)
  "处理组合器。"
  (let ((content (ecss-parser-content parser)))
    (ecss-parser-new-node
     parser
     (ecss-make-combinator
      (if (string-match-p "^[ \t\n\r\f]+$" content)
          " "
        content)))
    (cl-incf (ecss-parser-position parser))))

(defun ecss-parser-nesting (parser)
  "处理嵌套选择器。"
  (ecss-parser-new-node parser (ecss-make-nesting))
  (cl-incf (ecss-parser-position parser)))

(defun ecss-parser-attribute (parser)
  "处理属性选择器。"
  (let ((attr-tokens '())
        (start-token (ecss-parser-curr-token parser)))
    (cl-incf (ecss-parser-position parser))
    
    ;; 收集方括号内的token
    (while (and (< (ecss-parser-position parser)
                   (length (ecss-parser-tokens parser)))
                (not (= (aref (ecss-parser-curr-token parser) 0)
                        (cdr (assq 'close-square ecss-token-types)))))
      (push (ecss-parser-curr-token parser) attr-tokens)
      (cl-incf (ecss-parser-position parser)))
    
    (setq attr-tokens (nreverse attr-tokens))
    
    ;; 简化实现：只处理基本属性选择器 [attr] 和 [attr="value"]
    (let ((node (ecss-make-attribute))
          (pos 0)
          (len (length attr-tokens)))
      
      (when (> len 0)
        ;; 第一个应该是属性名
        (let ((token (nth pos attr-tokens)))
          (when (= (aref token 0) (cdr (assq 'word ecss-token-types)))
            (plist-put node :attribute (ecss-parser-content parser token))
            (cl-incf pos)))
        
        ;; 检查操作符
        (when (< pos len)
          (let ((token (nth pos attr-tokens)))
            (when (memq (aref token 0)
                        (mapcar (lambda (type)
                                  (cdr (assq type ecss-token-types)))
                                '(equals caret dollar asterisk tilde pipe)))
              (let ((op-content (ecss-parser-content parser token)))
                ;; 检查是否有后续的等号
                (when (and (< (1+ pos) len)
                           (= (aref (nth (1+ pos) attr-tokens) 0)
                              (cdr (assq 'equals ecss-token-types))))
                  (setq op-content (concat op-content "="))
                  (cl-incf pos))
                (plist-put node :operator op-content)
                (cl-incf pos)))))
        
        ;; 检查值
        (when (< pos len)
          (let ((token (nth pos attr-tokens)))
            (cond
             ((= (aref token 0) (cdr (assq 'str ecss-token-types)))
              (let ((content (ecss-parser-content parser token)))
                (plist-put node :value (substring
                                        content 1 (1- (length content))))
                (plist-put node :quote-mark (substring content 0 1))))
             ((= (aref token 0) (cdr (assq 'word ecss-token-types)))
              (plist-put node :value (ecss-parser-content parser token)))))))
      
      (ecss-parser-new-node parser node))
    (cl-incf (ecss-parser-position parser))))

(defun ecss-parser-parse (parser)
  "解析当前token。"
  (let ((token (ecss-parser-curr-token parser)))
    (when token
      (let ((type (aref token 0)))
        (cond
         ((gethash type ecss-whitespace-tokens)
          (ecss-parser-space parser))
         ((= type (cdr (assq 'comment ecss-token-types)))
          (ecss-parser-comment parser))
         ((= type (cdr (assq 'comma ecss-token-types)))
          (ecss-parser-comma parser))
         ((= type (cdr (assq 'word ecss-token-types)))
          (ecss-parser-word parser))
         ((= type (cdr (assq 'asterisk ecss-token-types)))
          (ecss-parser-universal parser))
         ((= type (cdr (assq 'colon ecss-token-types)))
          (ecss-parser-pseudo parser))
         ((= type (cdr (assq 'combinator ecss-token-types)))
          (ecss-parser-combinator parser))
         ((= type (cdr (assq 'ampersand ecss-token-types)))
          (ecss-parser-nesting parser))
         ((= type (cdr (assq 'open-square ecss-token-types)))
          (ecss-parser-attribute parser))
         (t
          (cl-incf (ecss-parser-position parser))))))))

(defun ecss-parser-loop (parser)
  "主解析循环。"
  (while (< (ecss-parser-position parser)
            (length (ecss-parser-tokens parser)))
    (ecss-parser-parse parser))
  ;; 处理剩余的尾随空白 - 附加到最后一个节点的 :after
  (when (and (ecss-parser-spaces-before parser)
             (not (string= (ecss-parser-spaces-before parser) "")))
    (let* ((current-selector (ecss-parser-current parser))
           (nodes (plist-get current-selector :nodes)))
      (when nodes
        (let* ((last-node (car (last nodes)))
               (spaces (plist-get last-node :spaces)))
          (when spaces
            (plist-put spaces :after
                       (ecss-parser-spaces-before parser)))))))
  (ecss-parser-root parser))

;;; 公共API

(defun ecss-selector-parse (selector-string)
  "解析ECSS选择器字符串，返回AST。
SELECTOR-STRING是要解析的ECSS选择器字符串。

示例：
  (ecss-selector-parse \"div.class#id\")
  ;; => AST树结构"
  (let* ((tokens (ecss-tokenize selector-string))
         (root (ecss-make-root))
         (selector (ecss-make-selector))
         (parser nil))
    (ecss-node-append root selector)
    (setq parser (make-ecss-parser
                  :css selector-string
                  :tokens (vconcat tokens)
                  :position 0
                  :root root
                  :current selector
                  :spaces-before ""))
    (ecss-parser-loop parser)))

(defun ecss-selector-walk (ast func)
  "遍历AST树的所有节点，对每个节点调用FUNC。
AST是要遍历的抽象语法树，FUNC是对每个节点调用的函数。

示例：
  (ecss-selector-walk ast
    (lambda (node)
      (message \"Node type: %s\" (plist-get node :type))))"
  (funcall func ast)
  (let ((nodes (plist-get ast :nodes)))
    (when nodes
      (dolist (node nodes)
        (ecss-selector-walk node func)))))

(defun ecss-selector-walk-type (ast node-type func)
  "遍历AST树中指定类型的节点，对每个节点调用FUNC。
AST是要遍历的抽象语法树，NODE-TYPE是要匹配的节点类型，
FUNC是对匹配节点调用的函数。

示例：
  (ecss-selector-walk-type ast 'class
    (lambda (node)
      (message \"Class: %s\" (plist-get node :value))))"
  (ecss-selector-walk
   ast (lambda (node)
         (when (eq (plist-get node :type) node-type)
           (funcall func node)))))

(defun ecss-selector-stringify (ast)
  "将AST转换回ECSS选择器字符串。
AST是要转换的抽象语法树。

示例：
  (ecss-selector-stringify (ecss-selector-parse \"div.class\"))
  ;; => \"div.class\""
  (ecss-node-to-string ast))

(defun ecss-selector-walk-tags (ast func)
  "遍历AST中的所有标签选择器。"
  (ecss-selector-walk-type ast 'tag func))

(defun ecss-selector-walk-classes (ast func)
  "遍历AST中的所有类选择器。"
  (ecss-selector-walk-type ast 'class func))

(defun ecss-selector-walk-ids (ast func)
  "遍历AST中的所有ID选择器。"
  (ecss-selector-walk-type ast 'id func))

(defun ecss-selector-walk-pseudos (ast func)
  "遍历AST中的所有伪类/伪元素。"
  (ecss-selector-walk-type ast 'pseudo func))

(defun ecss-selector-walk-attributes (ast func)
  "遍历AST中的所有属性选择器。"
  (ecss-selector-walk-type ast 'attribute func))

(provide 'ecss-selector)

;;; ecss-selector.el ends here
