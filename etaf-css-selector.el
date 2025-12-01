;;; etaf-css-selector.el --- ECSS选择器解析器 -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Based on postetaf-css-selector-parser
;; Keywords: ecss, parser, selector
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; 这是一个ECSS选择器解析器的Emacs Lisp实现，基于postetaf-css-selector-parser项目。
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
;;   (etaf-css-selector-parse "div.class#id > span:hover")
;;   ;; => AST树结构
;;
;;   (etaf-css-selector-walk
;;    (etaf-css-selector-parse "div.class")
;;    (lambda (node) (message "Node: %s" (plist-get node :type))))

;;; Code:

(require 'cl-lib)
;; Optional: Load etaf-event for interactive pseudo-class support
;; The selector matching will still work without it, but interactive
;; pseudo-classes (:hover, :active, :focus) will not match.
;; This is intentional to keep the CSS selector module independent.
(require 'etaf-event nil t)

(defconst etaf-css-selector-token-types
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

(defconst etaf-css-selector-word-delimiters
  (let ((delims (make-hash-table)))
    (dolist (type '(space tab newline cr feed
                          ampersand asterisk bang comma colon semicolon
                          open-paren close-paren open-square close-square
                          single-quote double-quote plus pipe tilde
                          greater-than equals dollar caret slash))
      (puthash (cdr (assq type etaf-css-selector-token-types)) t delims))
    delims)
  "单词分隔符集合。")

(defconst etaf-css-selector-whitespace-tokens
  (let ((tokens (make-hash-table)))
    (dolist (type '(space tab newline cr feed))
      (puthash (cdr (assq type etaf-css-selector-token-types)) t tokens))
    tokens)
  "空白字符token集合。")

(defconst etaf-css-selector-hex-chars
  (let ((hex (make-hash-table)))
    (dolist (char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                       ?a ?b ?c ?d ?e ?f ?A ?B ?C ?D ?E ?F))
      (puthash char t hex))
    hex)
  "十六进制字符集合。")

;;; 词法分析器（Tokenizer）

(defun etaf-css-selector-consume-escape (css start)
  "从CSS字符串中消费一个转义序列。
CSS是输入字符串，START是反斜杠的位置。
返回转义序列的结束位置。"
  (let ((next start)
        (code (and (< (1+ start) (length css))
                   (aref css (1+ start)))))
    (when code
      (if (gethash code etaf-css-selector-hex-chars)
          ;; 十六进制转义
          (let ((hex-digits 0))
            (while (and (< hex-digits 6)
                        (< (1+ next) (length css))
                        (gethash (aref css (1+ next))
                                 etaf-css-selector-hex-chars))
              (cl-incf next)
              (cl-incf hex-digits))
            ;; 如果少于6个十六进制字符，空格结束转义
            (when (and (< hex-digits 6)
                       (< (1+ next) (length css))
                       (= (aref css (1+ next))
                          (cdr (assq 'space etaf-css-selector-token-types))))
              (cl-incf next)))
        ;; 单字符转义
        (cl-incf next)))
    next))

(defun etaf-css-selector-consume-word (css start)
  "从CSS字符串中消费一个单词。
CSS是输入字符串，START是单词的起始位置。返回单词的结束位置。"
  (cl-block nil
    (let ((next start)
          (code nil))
      (while (< next (length css))
        (setq code (aref css next))
        (cond
         ((gethash code etaf-css-selector-word-delimiters)
          (cl-return (1- next)))
         ((= code (cdr (assq 'backslash etaf-css-selector-token-types)))
          (setq next (1+ (etaf-css-selector-consume-escape css next))))
         (t
          (cl-incf next))))
      (1- next))))

(defun etaf-css-selector-tokenize (css)
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
      (when (= code (cdr (assq 'newline etaf-css-selector-token-types)))
        (setq offset start
              line (1+ line)))
      (cond
       ;; 空白字符
       ((memq code (mapcar (lambda (type)
                             (cdr (assq type etaf-css-selector-token-types)))
                           '(space tab newline cr feed)))
        (setq next start)
        (while (and (< (cl-incf next) length)
                    (memq (aref css next)
                          (mapcar
                           (lambda (type)
                             (cdr (assq type etaf-css-selector-token-types)))
                           '(space tab newline cr feed))))
          (when (= (aref css next)
                   (cdr (assq 'newline etaf-css-selector-token-types)))
            (setq offset next
                  line (1+ line))))
        (setq token-type (cdr (assq 'space etaf-css-selector-token-types))
              end-line line
              end-column (- next offset 1)
              end next))
       ;; 组合器: + > ~ |
       ((memq code (mapcar (lambda (type)
                             (cdr (assq type etaf-css-selector-token-types)))
                           '(plus greater-than tilde pipe)))
        (setq next start)
        (while (and (< (cl-incf next) length)
                    (memq (aref css next)
                          (mapcar
                           (lambda (type)
                             (cdr (assq type etaf-css-selector-token-types)))
                           '(plus greater-than tilde pipe)))))
        (setq token-type (cdr (assq 'combinator etaf-css-selector-token-types))
              end-line line
              end-column (- start offset)
              end next))
       ;; 单字符token
       ((memq code
              (mapcar (lambda (type)
                        (cdr (assq type etaf-css-selector-token-types)))
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
                             (cdr (assq type etaf-css-selector-token-types)))
                           '(single-quote double-quote)))
        (setq quote
              (if (= code (cdr (assq 'single-quote
                                     etaf-css-selector-token-types)))
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
                           (cdr (assq 'backslash
                                      etaf-css-selector-token-types))))
              (cl-decf escape-pos)
              (setq escaped (not escaped)))
            (unless escaped
              (throw 'done nil))))
        (setq token-type (cdr (assq 'str etaf-css-selector-token-types))
              end-line line
              end-column (- start offset)
              end (1+ next)))
       ;; 注释或斜杠
       ((= code (cdr (assq 'slash etaf-css-selector-token-types)))
        (if (and (< (1+ start) length)
                 (= (aref css (1+ start))
                    (cdr (assq 'asterisk etaf-css-selector-token-types))))
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
              (setq token-type (cdr (assq 'comment
                                          etaf-css-selector-token-types))
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
       (t (setq next (etaf-css-selector-consume-word css start)
                token-type (cdr (assq
                                 'word etaf-css-selector-token-types))
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

(defconst etaf-css-selector-node-types
  '(root selector tag class id attribute pseudo
         universal combinator nesting comment string)
  "ECSS AST节点类型列表。")

(defun etaf-css-selector-make-node (type &rest props)
  "创建一个ECSS AST节点。TYPE是节点类型，PROPS是属性列表。"
  (if (member type etaf-css-selector-node-types)
      (let ((node (list :type type)))
        (while props
          (setq node (plist-put node (car props) (cadr props))
                props (cddr props)))
        (unless (plist-get node :spaces)
          (setq node (plist-put node :spaces (list :before "" :after ""))))
        node)
    (error "Type %S is not a valid ecss node type!" type)))

(defun etaf-css-selector-make-root (&rest props)
  "创建根节点。"
  (apply #'etaf-css-selector-make-node 'root :nodes '() props))

(defun etaf-css-selector-make-selector (&rest props)
  "创建选择器节点。"
  (apply #'etaf-css-selector-make-node 'selector :nodes '() props))

(defun etaf-css-selector-make-tag (value &rest props)
  "创建标签选择器节点。"
  (apply #'etaf-css-selector-make-node 'tag :value value props))

(defun etaf-css-selector-make-class (value &rest props)
  "创建类选择器节点。"
  (apply #'etaf-css-selector-make-node 'class :value value props))

(defun etaf-css-selector-make-id (value &rest props)
  "创建ID选择器节点。"
  (apply #'etaf-css-selector-make-node 'id :value value props))

(defun etaf-css-selector-make-attribute (&rest props)
  "创建属性选择器节点。"
  (apply #'etaf-css-selector-make-node 'attribute props))

(defun etaf-css-selector-make-pseudo (value &rest props)
  "创建伪类/伪元素节点。"
  (apply #'etaf-css-selector-make-node 'pseudo :value value props))

(defun etaf-css-selector-make-universal (&rest props)
  "创建通配符选择器节点。"
  (apply #'etaf-css-selector-make-node 'universal :value "*" props))

(defun etaf-css-selector-make-combinator (value &rest props)
  "创建组合器节点。"
  (apply #'etaf-css-selector-make-node 'combinator :value value props))

(defun etaf-css-selector-make-nesting (&rest props)
  "创建嵌套选择器节点。"
  (apply #'etaf-css-selector-make-node 'nesting :value "&" props))

(defun etaf-css-selector-make-comment (value &rest props)
  "创建注释节点。"
  (apply #'etaf-css-selector-make-node 'comment :value value props))

;;; 节点操作函数

(defun etaf-css-selector-node-append (container node)
  "将节点NODE添加到容器CONTAINER的子节点列表中。"
  (let ((nodes (plist-get container :nodes)))
    (plist-put container :nodes (append nodes (list node)))))

(defun etaf-css-selector-node-to-string (node)
  "将节点转换为字符串。"
  (let ((type (plist-get node :type))
        (value (plist-get node :value))
        (spaces (plist-get node :spaces))
        (nodes (plist-get node :nodes)))
    (concat
     (or (plist-get spaces :before) "")
     (cond
      ((eq type 'root)
       (mapconcat #'etaf-css-selector-node-to-string nodes ","))
      ((eq type 'selector)
       (mapconcat #'etaf-css-selector-node-to-string nodes ""))
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

(cl-defstruct etaf-css-selector-parser
  "ECSS解析器结构。"
  css             ; 输入ECSS字符串
  tokens          ; token数组
  position        ; 当前位置
  root            ; 根节点
  current         ; 当前选择器节点
  spaces-before)  ; 累积的前置空白

(defun etaf-css-selector-parser-curr-token (parser)
  "获取解析器的当前token。"
  (let ((pos (etaf-css-selector-parser-position parser)))
    (when (< pos (length (etaf-css-selector-parser-tokens parser)))
      (aref (etaf-css-selector-parser-tokens parser) pos))))

(defun etaf-css-selector-parser-next-token (parser)
  "获取解析器的下一个token。"
  (let ((pos (1+ (etaf-css-selector-parser-position parser))))
    (when (< pos (length (etaf-css-selector-parser-tokens parser)))
      (aref (etaf-css-selector-parser-tokens parser) pos))))

(defun etaf-css-selector-parser-content (parser &optional token)
  "获取token的内容字符串。"
  (let ((tok (or token (etaf-css-selector-parser-curr-token parser))))
    (when tok
      (substring (etaf-css-selector-parser-css parser)
                 (aref tok 5)  ; START-POS
                 (aref tok 6)))))  ; END-POS

(defun etaf-css-selector-parser-new-node (parser node)
  "添加新节点到当前选择器。"
  ;; 将累积的空白附加到节点的 :before
  (when (etaf-css-selector-parser-spaces-before parser)
    (let ((spaces (plist-get node :spaces)))
      (plist-put spaces
                 :before (etaf-css-selector-parser-spaces-before parser)))
    (setf (etaf-css-selector-parser-spaces-before parser) ""))
  (etaf-css-selector-node-append
   (etaf-css-selector-parser-current parser) node)
  node)

(defun etaf-css-selector-parser-space (parser)
  "处理空白字符。"
  ;; 累积空白字符，稍后附加到下一个节点
  (let ((space-content (etaf-css-selector-parser-content parser)))
    (setf (etaf-css-selector-parser-spaces-before parser)
          (concat (or (etaf-css-selector-parser-spaces-before parser) "")
                  space-content)))
  (cl-incf (etaf-css-selector-parser-position parser)))

(defun etaf-css-selector-parser-comment (parser)
  "处理注释。"
  (let* ((token (etaf-css-selector-parser-curr-token parser))
         (content (etaf-css-selector-parser-content parser token)))
    (etaf-css-selector-parser-new-node
     parser
     (etaf-css-selector-make-comment content))
    (cl-incf (etaf-css-selector-parser-position parser))))

(defun etaf-css-selector-parser-comma (parser)
  "处理逗号（新选择器）。"
  (let ((selector (etaf-css-selector-make-selector)))
    (etaf-css-selector-node-append
     (etaf-css-selector-parser-root parser) selector)
    (setf (etaf-css-selector-parser-current parser) selector))
  (cl-incf (etaf-css-selector-parser-position parser)))

(defun etaf-css-selector-parser-word (parser)
  "处理单词token。"
  (let* ((content (etaf-css-selector-parser-content parser))
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
            (push (etaf-css-selector-make-class
                   (substring content start end))
                  nodes)
            (setq i end)))
         
         ((= ch ?#)  ; ID选择器
          (let ((start (cl-incf i))
                (end i))
            (while (and (< end len)
                        (not (memq (aref content end) '(?. ?# ?\[))))
              (cl-incf end))
            (push (etaf-css-selector-make-id
                   (substring content start end))
                  nodes)
            (setq i end)))
         
         (t  ; 标签选择器
          (let ((start i)
                (end i))
            (while (and (< end len)
                        (not (memq (aref content end) '(?. ?# ?\[))))
              (cl-incf end))
            (when (> end start)
              (push (etaf-css-selector-make-tag
                     (substring content start end))
                    nodes))
            (setq i end))))))
    
    ;; 添加节点 - 只有第一个节点获得累积的空白
    (dolist (node (nreverse nodes))
      (if first-node
          (progn
            (etaf-css-selector-parser-new-node parser node)
            (setq first-node nil))
        ;; 后续节点直接添加，不获得累积的空白
        (etaf-css-selector-node-append
         (etaf-css-selector-parser-current parser) node)))
    (cl-incf (etaf-css-selector-parser-position parser))))

(defun etaf-css-selector-parser-universal (parser)
  "处理通配符选择器。"
  (etaf-css-selector-parser-new-node parser (etaf-css-selector-make-universal))
  (cl-incf (etaf-css-selector-parser-position parser)))

(defun etaf-css-selector-parser-pseudo (parser)
  "处理伪类/伪元素。"
  (let ((content (etaf-css-selector-parser-content parser))
        (next (etaf-css-selector-parser-next-token parser)))
    
    ;; 移动到冒号之后
    (cl-incf (etaf-css-selector-parser-position parser))
    
    ;; 检查是否为双冒号
    (when (and (etaf-css-selector-parser-curr-token parser)
               (= (aref (etaf-css-selector-parser-curr-token parser) 0) 
                  (cdr (assq 'colon etaf-css-selector-token-types))))
      (setq content (concat content (etaf-css-selector-parser-content parser)))
      (cl-incf (etaf-css-selector-parser-position parser)))
    
    ;; 消费伪类/伪元素名称（如果当前是word token）
    (when (and (etaf-css-selector-parser-curr-token parser)
               (= (aref (etaf-css-selector-parser-curr-token parser) 0) 
                  (cdr (assq 'word etaf-css-selector-token-types))))
      (setq content (concat content (etaf-css-selector-parser-content parser)))
      (cl-incf (etaf-css-selector-parser-position parser)))
    
    ;; 处理带参数的伪类（如 :nth-child(2)）
    (when (and (etaf-css-selector-parser-curr-token parser)
               (= (aref (etaf-css-selector-parser-curr-token parser) 0) 
                  (cdr (assq 'open-paren etaf-css-selector-token-types))))
      ;; 消费开括号和所有内容直到闭括号
      (catch 'done
        (while (< (etaf-css-selector-parser-position parser)
                  (length (etaf-css-selector-parser-tokens parser)))
          (let ((token (etaf-css-selector-parser-curr-token parser)))
            (setq content (concat content
                                  (etaf-css-selector-parser-content parser)))
            (cl-incf (etaf-css-selector-parser-position parser))
            (when (= (aref token 0)
                     (cdr (assq 'close-paren etaf-css-selector-token-types)))
              (throw 'done nil))))))
    
    (etaf-css-selector-parser-new-node
     parser (etaf-css-selector-make-pseudo content))))

(defun etaf-css-selector-parser-combinator (parser)
  "处理组合器。"
  (let ((content (etaf-css-selector-parser-content parser)))
    (etaf-css-selector-parser-new-node
     parser
     (etaf-css-selector-make-combinator
      (if (string-match-p "^[ \t\n\r\f]+$" content)
          " "
        content)))
    (cl-incf (etaf-css-selector-parser-position parser))))

(defun etaf-css-selector-parser-nesting (parser)
  "处理嵌套选择器。"
  (etaf-css-selector-parser-new-node
   parser (etaf-css-selector-make-nesting))
  (cl-incf (etaf-css-selector-parser-position parser)))

(defun etaf-css-selector-parser-attribute (parser)
  "处理属性选择器。"
  (let ((attr-tokens '())
        (start-token (etaf-css-selector-parser-curr-token parser)))
    (cl-incf (etaf-css-selector-parser-position parser))
    
    ;; 收集方括号内的token
    (while (and (< (etaf-css-selector-parser-position parser)
                   (length (etaf-css-selector-parser-tokens parser)))
                (not (= (aref (etaf-css-selector-parser-curr-token parser) 0)
                        (cdr (assq 'close-square
                                   etaf-css-selector-token-types)))))
      (push (etaf-css-selector-parser-curr-token parser) attr-tokens)
      (cl-incf (etaf-css-selector-parser-position parser)))
    
    (setq attr-tokens (nreverse attr-tokens))
    
    ;; 简化实现：只处理基本属性选择器 [attr] 和 [attr="value"]
    (let ((node (etaf-css-selector-make-attribute))
          (pos 0)
          (len (length attr-tokens)))
      
      (when (> len 0)
        ;; 第一个应该是属性名
        (let ((token (nth pos attr-tokens)))
          (when (= (aref token 0)
                   (cdr (assq 'word etaf-css-selector-token-types)))
            (plist-put node :attribute
                       (etaf-css-selector-parser-content parser token))
            (cl-incf pos)))
        
        ;; 检查操作符
        (when (< pos len)
          (let ((token (nth pos attr-tokens)))
            (when (or
                   (memq (aref token 0)
                         (mapcar
                          (lambda (type)
                            (cdr (assq type etaf-css-selector-token-types)))
                          '(equals caret dollar asterisk tilde pipe)))
                   ;; 也检查combinator类型，因为~和|可能被 tokenize 为 combinator
                   (= (aref token 0)
                      (cdr (assq 'combinator etaf-css-selector-token-types))))
              (let ((op-content (etaf-css-selector-parser-content
                                 parser token)))
                ;; 检查是否有后续的等号
                (when (and (< (1+ pos) len)
                           (= (aref (nth (1+ pos) attr-tokens) 0)
                              (cdr (assq 'equals
                                         etaf-css-selector-token-types))))
                  (setq op-content (concat op-content "="))
                  (cl-incf pos))
                (plist-put node :operator op-content)
                (cl-incf pos)))))
        
        ;; 检查值
        (when (< pos len)
          (let ((token (nth pos attr-tokens)))
            (cond
             ((= (aref token 0)
                 (cdr (assq 'str etaf-css-selector-token-types)))
              (let ((content (etaf-css-selector-parser-content parser token)))
                (plist-put node :value (substring
                                        content 1 (1- (length content))))
                (plist-put node :quote-mark (substring content 0 1))))
             ((= (aref token 0)
                 (cdr (assq 'word etaf-css-selector-token-types)))
              (plist-put
               node :value (etaf-css-selector-parser-content
                            parser token)))))))
      
      (etaf-css-selector-parser-new-node parser node))
    (cl-incf (etaf-css-selector-parser-position parser))))

(defun etaf-css-selector-parser-parse (parser)
  "解析当前token。"
  (let ((token (etaf-css-selector-parser-curr-token parser)))
    (when token
      (let ((type (aref token 0)))
        (cond
         ((gethash type etaf-css-selector-whitespace-tokens)
          (etaf-css-selector-parser-space parser))
         ((= type (cdr (assq 'comment etaf-css-selector-token-types)))
          (etaf-css-selector-parser-comment parser))
         ((= type (cdr (assq 'comma etaf-css-selector-token-types)))
          (etaf-css-selector-parser-comma parser))
         ((= type (cdr (assq 'word etaf-css-selector-token-types)))
          (etaf-css-selector-parser-word parser))
         ((= type (cdr (assq 'asterisk etaf-css-selector-token-types)))
          (etaf-css-selector-parser-universal parser))
         ((= type (cdr (assq 'colon etaf-css-selector-token-types)))
          (etaf-css-selector-parser-pseudo parser))
         ((= type (cdr (assq 'combinator etaf-css-selector-token-types)))
          (etaf-css-selector-parser-combinator parser))
         ((= type (cdr (assq 'ampersand etaf-css-selector-token-types)))
          (etaf-css-selector-parser-nesting parser))
         ((= type (cdr (assq 'open-square etaf-css-selector-token-types)))
          (etaf-css-selector-parser-attribute parser))
         (t
          (cl-incf (etaf-css-selector-parser-position parser))))))))

(defun etaf-css-selector-parser-loop (parser)
  "主解析循环。"
  (while (< (etaf-css-selector-parser-position parser)
            (length (etaf-css-selector-parser-tokens parser)))
    (etaf-css-selector-parser-parse parser))
  ;; 处理剩余的尾随空白 - 附加到最后一个节点的 :after
  (when (and (etaf-css-selector-parser-spaces-before parser)
             (not (string= (etaf-css-selector-parser-spaces-before parser)
                           "")))
    (let* ((current-selector (etaf-css-selector-parser-current parser))
           (nodes (plist-get current-selector :nodes)))
      (when nodes
        (let* ((last-node (car (last nodes)))
               (spaces (plist-get last-node :spaces)))
          (when spaces
            (plist-put spaces :after
                       (etaf-css-selector-parser-spaces-before parser)))))))
  (etaf-css-selector-parser-root parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etaf-css-selector-parse (string)
  "解析CSS选择器字符串 SELECTOR，返回AST。

示例：
  (etaf-css-selector-parse \"div.class#id\")
  ;; => '(:type root :nodes ((:type selector :nodes ((:type tag :value \"div\" :spaces (:before \"\" :after \"\")) (:type class :value \"class\" :spaces (:before \"\" :after \"\")) (:type id :value \"id\" :spaces (:before \"\" :after \"\"))) :spaces (:before \"\" :after \"\"))) :spaces (:before \"\" :after \"\"))"
  (let* ((tokens (etaf-css-selector-tokenize string))
         (root (etaf-css-selector-make-root))
         (selector (etaf-css-selector-make-selector))
         (parser nil))
    (etaf-css-selector-node-append root selector)
    (setq parser (make-etaf-css-selector-parser
                  :css string
                  :tokens (vconcat tokens)
                  :position 0
                  :root root
                  :current selector
                  :spaces-before ""))
    (etaf-css-selector-parser-loop parser)))

(defun etaf-css-selector-walk (func ast)
  "遍历AST树的所有节点，对每个节点调用FUNC。
  AST是要遍历的抽象语法树，FUNC是对每个节点调用的函数。

  示例：
(etaf-css-selector-walk
 (lambda (node)
   (message \"Node type: %s\" (plist-get node :type)))
 ast)"
  (funcall func ast)
  (when-let ((nodes (plist-get ast :nodes)))
    (dolist (node nodes)
      (etaf-css-selector-walk func node))))

(defun etaf-css-selector-attribute-match-p (node ast-node)
  "检查DOM节点NODE是否匹配属性选择器AST-NODE。"
  ;; (etaf-css-selector-parse "a[href=\"https://..\"]")
  ;; (:type attribute :spaces (:before "" :after "") :attribute "href" :operator "=" :value "https://.." :quote-mark "\"")
  (when (and node (listp node))
    (let* ((attr-name (intern (plist-get ast-node :attribute)))
           (operator (plist-get ast-node :operator))
           (expected-value (plist-get ast-node :value))
           (actual-value (dom-attr node attr-name)))
      (cond
       ;; 仅检查属性存在
       ((null operator) (not (null actual-value)))
       ;; 属性值完全匹配
       ((string= operator "=")
        (and actual-value (string= actual-value expected-value)))
       ;; 属性值前缀匹配
       ((string= operator "^=")
        (and actual-value (string-prefix-p expected-value actual-value)))
       ;; 属性值后缀匹配
       ((string= operator "$=")
        (and actual-value (string-suffix-p expected-value actual-value)))
       ;; 属性值包含子串
       ((string= operator "*=")
        (and actual-value (string-match-p
                           (regexp-quote expected-value) actual-value)))
       ;; 属性值包含空格分隔的单词
       ((string= operator "~=")
        (and actual-value 
             (member expected-value (split-string actual-value " "))))
       ;; 属性值等于或以其开头后跟连字符
       ((string= operator "|=")
        (and actual-value
             (or (string= actual-value expected-value)
                 (string-prefix-p (concat expected-value "-")
                                  actual-value))))
       (t nil)))))

(defun etaf-css-selector-parse-nth-expression (expr)
  "解析nth表达式，如 '2n+1', 'odd', 'even', '3'。
返回 (a . b)，表示 an+b 的形式。"
  (cond
   ((string= expr "odd") '(2 . 1))
   ((string= expr "even") '(2 . 0))
   ((string-match "^\\([+-]?[0-9]*\\)n\\([+-][0-9]+\\)?$" expr)
    (let ((a-str (match-string 1 expr))
          (b-str (match-string 2 expr)))
      (cons (if (or (string= a-str "") (string= a-str "+"))
                1
              (if (string= a-str "-")
                  -1
                (string-to-number a-str)))
            (if b-str (string-to-number b-str) 0))))
   ((string-match "^\\([+-]?[0-9]+\\)$" expr)
    (cons 0 (string-to-number expr)))
   (t nil)))

(defun etaf-css-selector-nth-match-p (index a b)
  "检查索引INDEX是否匹配 an+b 模式。INDEX从1开始。"
  (if (= a 0)
      (= index b)
    (and (>= index 1)
         (>= (- index b) 0)
         (= (mod (- index b) a) 0)
         ;; 确保 n 是非负整数
         (>= (/ (- index b) a) 0))))

(defun etaf-css-selector-basic-match-p (node selector-ast)
  "检查 DOM 节点 NODE 是否匹配基础选择器 SELECTOR-AST。
基础选择器是没有组合器的选择器序列，如 'div.class#id'。"
  (when (and node (listp node))
    (let ((matches t))
      ;; 遍历选择器的所有部分
      (etaf-css-selector-walk
       (lambda (ast-node)
         (let ((type (plist-get ast-node :type)))
           (cond
            ((eq type 'tag)
             (setq matches
                   (and matches (etaf-dom-tag-match-p
                                 node (plist-get ast-node :value)))))
            ((eq type 'class)
             (setq matches
                   (and matches (etaf-dom-class-match-p
                                 node (plist-get ast-node :value)))))
            ((eq type 'id)
             (setq matches
                   (and matches (etaf-dom-id-match-p
                                 node (plist-get ast-node :value)))))
            ((eq type 'universal) t) ;; 通配符总是匹配
            ((eq type 'attribute)
             (setq matches
                   (and matches (etaf-css-selector-attribute-match-p
                                 node ast-node))))
            ((eq type 'pseudo)
             (setq matches
                   (and matches (etaf-css-selector-pseudo-match-p
                                 node ast-node)))))))
       selector-ast)
      matches)))

(defun etaf-css-selector-pseudo-match-p (node pseudo-node)
  "检查DOM节点NODE是否匹配伪类选择器PSEUDO-NODE。
支持结构伪类和交互伪类（:hover, :active, :focus等）。
交互伪类需要通过 etaf-event 模块跟踪元素状态。"
  (when (and node (listp node))
    (let ((pseudo-value (plist-get pseudo-node :value)))
      (cond
       ;; Interactive pseudo-classes (require event model)
       ;; :hover - Element is being hovered by mouse
       ((string= pseudo-value ":hover")
        (etaf-css-selector-check-interactive-state node :hover))
       ;; :active - Element is being activated (mouse pressed)
       ((string= pseudo-value ":active")
        (etaf-css-selector-check-interactive-state node :active))
       ;; :focus - Element has focus
       ((string= pseudo-value ":focus")
        (etaf-css-selector-check-interactive-state node :focus))
       ;; :disabled - Element is disabled
       ((string= pseudo-value ":disabled")
        (etaf-css-selector-check-interactive-state node :disabled))
       ;; :enabled - Element is enabled
       ((string= pseudo-value ":enabled")
        (etaf-css-selector-check-interactive-state node :enabled))
       
       ;; Structural pseudo-classes (existing implementation)
       ;; :first-child
       ((string= pseudo-value ":first-child")
        (etaf-dom-is-first-child node))
       ;; :last-child
       ((string= pseudo-value ":last-child")
        (etaf-dom-is-last-child node))
       ;; :only-child
       ((string= pseudo-value ":only-child")
        (and (etaf-dom-is-first-child node)
             (etaf-dom-is-last-child node)))
       ;; :first-of-type
       ((string= pseudo-value ":first-of-type")
        (etaf-dom-is-first-of-type node))
       ;; :last-of-type
       ((string= pseudo-value ":last-of-type")
        (etaf-dom-is-last-of-type node))
       ;; :only-of-type
       ((string= pseudo-value ":only-of-type")
        (etaf-dom-is-only-of-type node))
       ;; :empty
       ((string= pseudo-value ":empty")
        (etaf-dom-is-empty node))
       ;; :nth-child(an+b)
       ((string-match "^:nth-child(\\([^)]+\\))$" pseudo-value)
        (let* ((expr (match-string 1 pseudo-value))
               (parsed (etaf-css-selector-parse-nth-expression expr))
               (index (etaf-dom-get-child-index node)))
          (and parsed index
               (etaf-css-selector-nth-match-p
                index (car parsed) (cdr parsed)))))
       ;; :nth-last-child(an+b)
       ((string-match "^:nth-last-child(\\([^)]+\\))$" pseudo-value)
        (let* ((expr (match-string 1 pseudo-value))
               (parsed (etaf-css-selector-parse-nth-expression expr)))
          (when (and parsed etaf-dom--query-root)
            (let* ((parent (etaf-dom-get-parent
                            node etaf-dom--query-root))
                   (element-children
                    (when parent
                      (etaf-dom-get-element-children parent)))
                   (total (length element-children))
                   (index (etaf-dom-get-child-index node)))
              (and index
                   (etaf-css-selector-nth-match-p
                    (- total index -1) (car parsed) (cdr parsed)))))))
       ;; :nth-of-type(an+b)
       ((string-match "^:nth-of-type(\\([^)]+\\))$" pseudo-value)
        (let* ((expr (match-string 1 pseudo-value))
               (parsed (etaf-css-selector-parse-nth-expression expr)))
          (when (and parsed etaf-dom--query-root)
            (let* ((parent (etaf-dom-get-parent
                            node etaf-dom--query-root))
                   (node-tag (dom-tag node)))
              (when parent
                (let* ((element-children
                        (etaf-dom-get-element-children parent))
                       (same-type-children (cl-remove-if-not
                                            (lambda (child)
                                              (eq (dom-tag child) node-tag))
                                            element-children))
                       (index 0))
                  (catch 'found
                    (dolist (child same-type-children)
                      (cl-incf index)
                      (when (eq child node)
                        (throw 'found
                               (etaf-css-selector-nth-match-p
                                index (car parsed) (cdr parsed)))))
                    nil)))))))
       ;; :nth-last-of-type(an+b)
       ((string-match "^:nth-last-of-type(\\([^)]+\\))$" pseudo-value)
        (let* ((expr (match-string 1 pseudo-value))
               (parsed (etaf-css-selector-parse-nth-expression expr)))
          (when (and parsed etaf-dom--query-root)
            (let* ((parent (etaf-dom-get-parent
                            node etaf-dom--query-root))
                   (node-tag (dom-tag node)))
              (when parent
                (let* ((element-children
                        (etaf-dom-get-element-children parent))
                       (same-type-children (cl-remove-if-not
                                            (lambda (child)
                                              (eq (dom-tag child) node-tag))
                                            element-children))
                       (total (length same-type-children))
                       (index 0))
                  (catch 'found
                    (dolist (child same-type-children)
                      (cl-incf index)
                      (when (eq child node)
                        (throw 'found
                               (etaf-css-selector-nth-match-p
                                (- total index -1)
                                (car parsed)
                                (cdr parsed)))))
                    nil)))))))
       ;; :not(selector) - 简化实现，只支持简单选择器
       ((string-match "^:not(\\([^)]+\\))$" pseudo-value)
        (let* ((inner-selector (match-string 1 pseudo-value))
               (inner-ast (etaf-css-selector-parse inner-selector)))
          ;; 检查节点是否不匹配内部选择器
          (when inner-ast
            (let ((first-selector (car (plist-get inner-ast :nodes))))
              (when (and first-selector
                         (eq (plist-get first-selector :type) 'selector))
                (not (etaf-css-selector-basic-match-p
                      node first-selector)))))))
       ;; 其他伪类暂不支持，返回t以避免过滤
       (t t)))))

(defun etaf-css-selector-check-interactive-state (node state-key)
  "Check if NODE matches the interactive STATE-KEY (:hover, :active, etc.).
This function integrates with the etaf-event module to check element state.
If the event module is not loaded or the element is not registered,
returns nil (element doesn't match the pseudo-class)."
  (when (and node (listp node))
    ;; Check if etaf-event module is available
    (when (featurep 'etaf-event)
      ;; Try to get the element's UUID from its attributes
      (let ((uuid (dom-attr node 'uuid)))
        (when uuid
          ;; Query the event system for this element's state
          (etaf-event-matches-pseudo-class-p uuid state-key))))))

(defun etaf-css-selector-part-match-p (node selector-nodes)
  "检查DOM节点是否匹配选择器节点列表 SELECTOR-NODES。"
  (let ((matches t)
        (mock-selector (etaf-css-selector-make-selector)))
    ;; 创建一个临时选择器节点来包含这些节点
    (dolist (ast-node selector-nodes)
      (etaf-css-selector-node-append mock-selector ast-node))
    (etaf-css-selector-basic-match-p node mock-selector)))

;;; 组合器匹配

(defun etaf-css-selector-descendant-match-p (node ancestor-nodes dom)
  "检查节点NODE是否有祖先匹配ANCESTOR-NODES（后代组合器）。
返回匹配的祖先节点，如果没有匹配则返回nil。"
  (catch 'found
    (dom-search
     dom
     (lambda (candidate)
       (when (and (etaf-css-selector-part-match-p
                   candidate ancestor-nodes)
                  (etaf-dom-is-descendant-of node candidate))
         (throw 'found candidate))))
    nil))

(defun etaf-css-selector-child-match-p (node parent-nodes dom)
  "检查节点NODE的直接父节点是否匹配PARENT-NODES（子组合器）。
返回匹配的父节点，如果没有匹配则返回nil。"
  ;; 正确实现：获取node的直接父节点，检查它是否匹配parent-nodes
  (let ((parent (etaf-dom-get-parent node dom)))
    (when (and parent (etaf-css-selector-part-match-p parent parent-nodes))
      parent)))

(defun etaf-css-selector-adjacent-sibling-match-p
    (node prev-sibling-nodes dom)
  "检查节点NODE的前一个兄弟节点是否匹配PREV-SIBLING-NODES（相邻兄弟组合器）。"
  (let ((prev-sibling (etaf-dom-get-previous-sibling node dom)))
    (and prev-sibling
         (etaf-css-selector-part-match-p
          prev-sibling prev-sibling-nodes))))

(defun etaf-css-selector-general-sibling-match-p
    (node sibling-nodes dom)
  "检查节点NODE之前的兄弟节点中是否有匹配SIBLING-NODES（通用兄弟组合器）。"
  (let ((prev-siblings (etaf-dom-get-previous-siblings node dom))
        (found nil))
    (dolist (sibling prev-siblings)
      (when (etaf-css-selector-part-match-p sibling sibling-nodes)
        (setq found t)))
    found))

(defun etaf-css-selector-combinator-match-p
    (node parts rightmost-combinator dom)
  "检查节点是否满足组合器链的所有条件，从右到左处理PARTS。
RIGHTMOST-COMBINATOR是连接node到前一个部分的组合器。"
  (if (null parts)
      t
    (let* ((current-part (car (last parts)))
           (remaining-parts (butlast parts))
           (combinator (or rightmost-combinator (cdr current-part)))
           (selector-nodes (car current-part))
           ;; next-combinator 是当前部分存储的组合器，它连接当前部分到其左侧邻居
           ;; 这将在递归调用时使用
           (next-combinator (cdr current-part)))
      (cond
       ;; 后代组合器（空格）
       ((or (null combinator) (string= combinator " "))
        (let ((matching-ancestor (etaf-css-selector-descendant-match-p
                                  node selector-nodes dom)))
          (and matching-ancestor
               (if remaining-parts
                   ;; 递归检查剩余部分，从匹配的祖先继续
                   (etaf-css-selector-combinator-match-p
                    matching-ancestor remaining-parts next-combinator dom)
                 t))))
       ;; 子组合器 (>)
       ((string= combinator ">")
        (let ((matching-parent (etaf-css-selector-child-match-p
                                node selector-nodes dom)))
          (and matching-parent
               (if remaining-parts
                   ;; 递归检查剩余部分，从匹配的父节点继续
                   (etaf-css-selector-combinator-match-p
                    matching-parent remaining-parts next-combinator dom)
                 t))))
       ;; 相邻兄弟组合器 (+)
       ((string= combinator "+")
        (and (etaf-css-selector-adjacent-sibling-match-p
              node selector-nodes dom)
             (if remaining-parts t t)))
       ;; 通用兄弟组合器 (~)
       ((string= combinator "~")
        (and (etaf-css-selector-general-sibling-match-p
              node selector-nodes dom)
             (if remaining-parts t t)))
       (t t)))))

(defun etaf-css-selector-split-by-combinators (selector-ast)
  "将选择器AST按组合器分割成多个部分。返回一个列表，每个元素是
(selector-nodes . combinator)。"
  (let ((parts '())
        (current-nodes '())
        (current-combinator nil)
        (last-was-combinator nil))
    (dolist (node (plist-get selector-ast :nodes))
      (if (eq (plist-get node :type) 'combinator)
          (progn
            ;; 保存当前累积的节点和组合器
            (when current-nodes
              (push (cons (nreverse current-nodes) current-combinator)
                    parts))
            ;; 设置新的组合器
            (setq current-combinator (plist-get node :value))
            (setq current-nodes '())
            (setq last-was-combinator t))
        ;; 检查是否有隐式的后代组合器（空格）
        ;; 只在上一个节点不是组合器时检查
        (unless last-was-combinator
          (let* ((spaces (plist-get node :spaces))
                 (before-space (and spaces (plist-get spaces :before))))
            (when (and before-space (not (string-empty-p before-space)))
              ;; 有前导空格，表示这是一个新的选择器部分
              (when current-nodes
                (push (cons (nreverse current-nodes) current-combinator)
                      parts))
              ;; 设置后代组合器
              (setq current-combinator " ")
              (setq current-nodes '()))))
        ;; 累积非组合器节点
        (push node current-nodes)
        (setq last-was-combinator nil)))
    ;; 添加最后一组节点
    (when current-nodes
      (push (cons (nreverse current-nodes) current-combinator) parts))
    (nreverse parts)))

(defun etaf-css-selector-node-matches-p (node dom selector-ast)
  "检查节点 NODE 是否匹配选择器 SELECTOR-AST（支持组合器）。
NODE 是要检查的 DOM 节点。
DOM 是完整的 DOM 树根节点，用于查找父节点和兄弟节点。
SELECTOR-AST 是通过 etaf-css-selector-parse 解析得到的选择器 AST。
返回 t 如果节点匹配选择器，否则返回 nil。"
  (let ((parts (etaf-css-selector-split-by-combinators selector-ast)))
    (if (= (length parts) 1)
        ;; 简单选择器，无组合器
        (etaf-css-selector-part-match-p node (caar parts))
      ;; 复杂选择器，有组合器
      ;; 从右到左匹配
      (let* ((rightmost-part (car (last parts)))
             (preceding-parts (butlast parts))
             (rightmost-combinator (cdr rightmost-part)))
        ;; 首先检查节点是否匹配最右侧选择器
        (and (etaf-css-selector-part-match-p node (car rightmost-part))
             ;; 检查是否满足所有组合器关系
             (etaf-css-selector-combinator-match-p
              node preceding-parts rightmost-combinator dom))))))

(defun etaf-css-selector-query-by-ast (dom selector-ast)
  "使用复杂选择器（包含组合器）查询DOM，返回所有匹配的节点列表。"
  (let ((parts (etaf-css-selector-split-by-combinators selector-ast))
        (results '()))
    (if (= (length parts) 1)
        ;; 简单选择器，无组合器
        (dom-search dom
                    (lambda (node)
                      (etaf-css-selector-part-match-p
                       node (caar parts))))
      ;; 复杂选择器，有组合器
      ;; 从右到左匹配
      (let* ((rightmost-part (car (last parts)))
             (preceding-parts (butlast parts))
             (rightmost-combinator (cdr rightmost-part)))  
        ;; 首先找到匹配最右侧选择器的节点
        (dom-search
         dom
         (lambda (node)
           (and (etaf-css-selector-part-match-p
                 node (car rightmost-part))
                ;; 检查是否满足所有组合器关系
                (etaf-css-selector-combinator-match-p
                 node preceding-parts rightmost-combinator dom))))))))

(defun etaf-css-selector-query (dom selector-string)
  "在DOM树中查询所有匹配CSS选择器的节点。DOM是要查询的DOM树，
SELECTOR-STRING是CSS选择器字符串。返回匹配节点的列表。

示例：(etaf-css-selector-query dom \"div.container p.text\")"
  (let* ((etaf-dom--query-root dom) ; Set query root for pseudo-class checks
         (ast (etaf-css-selector-parse selector-string))
         (root (plist-get ast :type))
         (results '()))
    ;; 处理根节点中的所有选择器（逗号分隔）
    (dolist (selector (plist-get ast :nodes))
      (when (eq (plist-get selector :type) 'selector)
        (let ((matches (etaf-css-selector-query-by-ast dom selector)))
          (setq results (append results matches)))))
    ;; 去重
    (cl-remove-duplicates results :test #'equal)))

(provide 'etaf-css-selector)
;;; etaf-css-selector.el ends here
