;;; etaf-layout-box.el --- Box model for layout computation -*- lexical-binding: t; -*-

;; Copyright (C) 2024 ETAF Contributors

;; Author: ETAF Contributors
;; Keywords: layout, box-model
;; Version: 1.0.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; CSS 盒模型模块
;;
;; 本模块提供盒模型的数据结构和操作函数。
;; 所有函数使用 `etaf-layout-box-' 前缀。
;;
;; 盒模型数据结构:
;;   (:box-sizing "content-box"|"border-box"
;;    :content (:width <number> :height <number>)
;;    :padding (:top <n> :right <n> :bottom <n> :left <n>)
;;    :border (:top-width <n> :right-width <n> :bottom-width <n> :left-width <n>
;;             :top-color <color> :right-color <color> :bottom-color <color> :left-color <color>)
;;    :margin (:top <n> :right <n> :bottom <n> :left <n>))
;;
;; 公共接口：
;; - `etaf-layout-box-create' - 创建空的盒模型结构
;; - `etaf-layout-box-content-width' - 获取内容宽度
;; - `etaf-layout-box-content-height' - 获取内容高度
;; - `etaf-layout-box-padding-width' - 获取左右内边距之和
;; - `etaf-layout-box-padding-height' - 获取上下内边距之和
;; - `etaf-layout-box-border-width' - 获取左右边框之和
;; - `etaf-layout-box-border-height' - 获取上下边框之和
;; - `etaf-layout-box-margin-width' - 获取左右外边距之和
;; - `etaf-layout-box-margin-height' - 获取上下外边距之和
;; - `etaf-layout-box-total-width' - 计算总宽度
;; - `etaf-layout-box-total-height' - 计算总高度

;;; Code:

(require 'cl-lib)

;;; ============================================================
;;; 创建函数
;;; ============================================================

(defun etaf-layout-box-create ()
  "创建空的盒模型结构。

返回一个 plist，包含以下键：
- :box-sizing - 盒模型计算方式
- :content - 内容区域尺寸
- :padding - 内边距
- :border - 边框（宽度和颜色）
- :margin - 外边距"
  (list :box-sizing "content-box"
        :content (list :width 0 :height 0)
        :padding (list :top 0 :right 0 :bottom 0 :left 0)
        :border (list :top-width 0 :right-width 0 :bottom-width 0 :left-width 0
                      :top-color (face-attribute 'default :foreground)
                      :right-color (face-attribute 'default :foreground)
                      :bottom-color (face-attribute 'default :foreground)
                      :left-color (face-attribute 'default :foreground))
        :margin (list :top 0 :right 0 :bottom 0 :left 0)))

;;; ============================================================
;;; 内容区域访问器
;;; ============================================================

(defun etaf-layout-box-content-width (box-model)
  "获取盒模型的内容宽度。
BOX-MODEL 是盒模型 plist。"
  (plist-get (plist-get box-model :content) :width))

(defun etaf-layout-box-content-height (box-model)
  "获取盒模型的内容高度。
BOX-MODEL 是盒模型 plist。"
  (plist-get (plist-get box-model :content) :height))

;;; ============================================================
;;; 内边距访问器
;;; ============================================================

(defun etaf-layout-box-padding-width (box-model)
  "获取盒模型的左右内边距之和。
BOX-MODEL 是盒模型 plist。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :left)
       (plist-get padding :right))))

(defun etaf-layout-box-padding-height (box-model)
  "获取盒模型的上下内边距之和。
BOX-MODEL 是盒模型 plist。"
  (let ((padding (plist-get box-model :padding)))
    (+ (plist-get padding :top)
       (plist-get padding :bottom))))

;;; ============================================================
;;; 边框访问器
;;; ============================================================

(defun etaf-layout-box-border-width (box-model)
  "获取盒模型的左右边框之和。
BOX-MODEL 是盒模型 plist。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :left-width)
       (plist-get border :right-width))))

(defun etaf-layout-box-border-height (box-model)
  "获取盒模型的上下边框之和。
BOX-MODEL 是盒模型 plist。"
  (let ((border (plist-get box-model :border)))
    (+ (plist-get border :top-width)
       (plist-get border :bottom-width))))

;;; ============================================================
;;; 外边距访问器
;;; ============================================================

(defun etaf-layout-box-margin-width (box-model)
  "获取盒模型的左右外边距之和。
BOX-MODEL 是盒模型 plist。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :left)
       (plist-get margin :right))))

(defun etaf-layout-box-margin-height (box-model)
  "获取盒模型的上下外边距之和。
BOX-MODEL 是盒模型 plist。"
  (let ((margin (plist-get box-model :margin)))
    (+ (plist-get margin :top)
       (plist-get margin :bottom))))

;;; ============================================================
;;; 总尺寸计算
;;; ============================================================

(defun etaf-layout-box-total-width (box-model)
  "计算盒模型的总宽度（content + padding + border + margin）。
BOX-MODEL 是盒模型 plist。"
  (+ (etaf-layout-box-content-width box-model)
     (etaf-layout-box-padding-width box-model)
     (etaf-layout-box-border-width box-model)
     (etaf-layout-box-margin-width box-model)))

(defun etaf-layout-box-total-height (box-model)
  "计算盒模型的总高度（content + padding + border + margin）。
BOX-MODEL 是盒模型 plist。"
  (+ (etaf-layout-box-content-height box-model)
     (etaf-layout-box-padding-height box-model)
     (etaf-layout-box-border-height box-model)
     (etaf-layout-box-margin-height box-model)))

(provide 'etaf-layout-box)
;;; etaf-layout-box.el ends here
