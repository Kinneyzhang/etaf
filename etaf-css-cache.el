;;; etaf-css-cache.el --- CSS Computed Style Cache -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ETAF Contributors
;; Keywords: css, cache, performance
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 计算样式缓存系统
;;
;; 为了提高性能，缓存每个节点的计算样式。
;; 避免重复计算相同节点的样式。
;;
;; 使用示例：
;;
;;   ;; 创建缓存
;;   (setq cache (etaf-css-cache-create))
;;   
;;   ;; 查询缓存
;;   (etaf-css-cache-get cache node)
;;   
;;   ;; 设置缓存
;;   (etaf-css-cache-set cache node style)

;;; Code:

(require 'cl-lib)

(defun etaf-css-cache-create ()
  "创建一个新的计算样式缓存。
返回一个哈希表，用于存储节点到样式的映射。"
  (make-hash-table :test 'eq :size 100))

(defun etaf-css-cache-get (cache node)
  "从缓存中获取节点的计算样式。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。
返回缓存的样式，如果不存在则返回 nil。"
  (gethash node cache))

(defun etaf-css-cache-set (cache node style)
  "将节点的计算样式存入缓存。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。
STYLE 是计算后的样式。"
  (puthash node style cache))

(defun etaf-css-cache-clear (cache)
  "清空缓存中的所有数据。
CACHE 是缓存哈希表。"
  (clrhash cache))

(defun etaf-css-cache-remove (cache node)
  "从缓存中移除指定节点的样式。
CACHE 是缓存哈希表。
NODE 是 DOM 节点。"
  (remhash node cache))

(defun etaf-css-cache-size (cache)
  "获取缓存中的条目数量。
CACHE 是缓存哈希表。"
  (hash-table-count cache))

(provide 'etaf-css-cache)
;;; etaf-css-cache.el ends here
