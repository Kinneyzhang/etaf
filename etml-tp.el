;;; tp layers

(require 'dash)

(defun etml-tp-top-layer-props (properties)
  "获取最上面的属性层，也就是实际要渲染的层"
  (if-let ((idx (-elem-index 'etml-tp-layers properties)))
      (-remove-at-indices (list idx (1+ idx)) properties)
    properties))

(defun etml-tp-below-layers-props (properties)
  "获取最上层以下的属性层列表"
  (plist-get properties 'etml-tp-layers))

(defun etml-tp-all (start end &optional object)
  "获取 OBJECT 的 start 到 end 范围内文本的所有 text properties。
OBJECT 可以为 buffer 或 string，nil 默认为当前 buffer。
point 从 0 开始。"
  (let ((object (or object (current-buffer))))
    (cond
     ((stringp object)
      (object-intervals (substring object start end)))
     ((bufferp object)
      (with-current-buffer (get-buffer-create object)
        (object-intervals (buffer-substring start end))))
     (t (error "Invalid format of object: %S"
               (type-of object))))))

(defun etml-tp-empty-p (object)
  (null (object-intervals object)))

(defun etml-tp-intervals-map (function start end &optional object)
  "处理 start，end 之间的所有 intervals 的文本属性
function 的四个参数分别为:区间开始位置，区间结束位置，顶层属性和下层属性列表"
  (remove
   nil
   (mapcar
    (lambda (tp)
      (let* ((interval-start (nth 0 tp)) ;; start from 0
             (interval-end (nth 1 tp))
             (interval-props (nth 2 tp))
             (top-props (etml-tp-top-layer-props interval-props))
             (below-props-lst (etml-tp-below-layers-props interval-props)))
        (funcall function
                 interval-start interval-end
                 top-props below-props-lst)))
    (etml-tp-all start end object))))

(defun etml-tp-layer-props (name start end &optional object)
  "返回 object 的 start 和 end 之间文本属性层为 name 的属性列表"
  (etml-tp-intervals-map
   (lambda (i-start i-end top belows)
     (when-let ((props (seq-find
                        (lambda (props)
                          (equal name (plist-get props 'etml-tp-name)))
                        (append (list top) belows))))
       (list (+ start i-start) (+ start i-end) props)))
   start end object))

;; (defun etml-tp-layers-names (start end &optional object)
;;   "返回当前所有 layers 的名称"
;;   (seq-uniq
;;    (apply 'append
;;           (etml-tp-intervals-map
;;            (lambda (i-start i-end top belows)
;;              (remove nil (mapcar (lambda (props)
;;                                    (plist-get props 'etml-tp-name))
;;                                  (append (list top) belows))))
;;            start end object))))

(defun etml-tp-layer-set (name start end &optional object)
  "将 object 的 start 和 end 之间的文本当前的属性层命名为 name"
  (if (etml-tp-empty-p object)
      (add-text-properties
       start end (list 'etml-tp-name name) end object)
    (etml-tp-intervals-map
     (lambda (i-start i-end top belows)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (plist-put top 'etml-tp-name name)
                (list 'etml-tp-layers belows))
        object))
     start end object))
  object)

(defun etml-tp-layer-push (name start end properties &optional object)
  "设置 properties 为最上面的 prop 层，name 是 layer 的名字"
  ;; 当前顶层放到 etml-tp-layers 开头，properties 设置为当前顶层。
  ;; FIXME: 需要检查 name 是否已经存在，存在则报错
  (declare (indent defun))
  (when (etml-tp-layer-props name start end object)
    (error "Already exist layer named %S" name))
  (if (etml-tp-empty-p object)
      (set-text-properties
       start end (append properties (list 'etml-tp-name name))
       object)
    (etml-tp-intervals-map
     (lambda (i-start i-end top belows)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (append properties (list 'etml-tp-name name))
                (list 'etml-tp-layers (append (list top) belows)))
        object))
     start end object))
  object)

(defun etml-tp-layer-delete (name start end &optional object)
  "移除 object 的 start 和 end 之间文本的名称为 name 的层"
  ;; 当前顶层放到 etml-tp-layers 开头，properties 设置为当前顶层。
  ;; FIXME: 需要检查 name 是否已经存在，存在则报错
  (declare (indent defun))
  (etml-tp-intervals-map
   (lambda (i-start i-end top belows)
     (set-text-properties
      (+ start i-start) (+ start i-end)
      ;; name 是顶层，删除该层后，下一层上移
      (if (equal name (plist-get top 'etml-tp-name))
          (append (nth 0 belows)
                  (list 'etml-tp-layers (seq-drop belows 1)))
        ;; name 不是顶层，直接删除
        (append top
                (list 'etml-tp-layers
                      (-remove (lambda (props)
                                 (equal name (plist-get
                                              props 'etml-tp-name)))
                               belows))))
      object))
   start end object)
  nil)

(defun etml-tp-layer-rotate (start end &optional object)
  "将 start 和 end 之间的顶层文本属性移到最后一层，相当于循环显示不同层。"
  (etml-tp-intervals-map
   (lambda (i-start i-end top belows)
     (set-text-properties
      (+ start i-start) (+ start i-end)
      (append (nth 0 belows)
              (list 'etml-tp-layers
                    (append (seq-drop belows 1)
                            (list top))))
      object))
   start end object)
  nil)

(defun etml-tp-layer-pin (name start end &optional object)
  "将 start 和 end 之间名称为 name 的层移动最上面。"
  (unless (etml-tp-layer-props name start end object)
    (error "Doesn't exist a layer named %S" name))
  (etml-tp-intervals-map
   (lambda (i-start i-end top belows)
     ;; name layer 本身就位于最上层时，无需操作
     (unless (equal (plist-get top 'etml-tp-name) name)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (let ((new-top
               ;; 获取新的置顶层
               (seq-find (lambda (props)
                           (equal (plist-get props 'etml-tp-name)
                                  name))
                         belows))
              ;; 移除掉被置顶的层
              (rest-belows
               (-remove (lambda (props)
                          (equal (plist-get props 'etml-tp-name)
                                 name))
                        belows)))
          (append new-top
                  (list 'etml-tp-layers
                        (append (list top) rest-belows))))
        object)))
   start end object)
  nil)

;;; propertize string

(defvar etml-tp-layer-alist nil
  "Alist 的每个元素是单个 layer")

(defvar etml-tp-layer-groups nil
  "group 的每个元素是 layer 组，组中存储的是多个 layer")

(defun etml-tp-layer-define (name properties)
  "定义一个名称为 name 的文本属性层，数据存放在 etml-tp-layer-alist 中"
  (declare (indent defun))
  (if (assoc name etml-tp-layer-alist)
      (setf (cdr (assoc name etml-tp-layer-alist)) properties)
    (push (cons name properties) etml-tp-layer-alist))
  (assoc name etml-tp-layer-alist))

(defun etml-tp-layer-group-define (name &rest layers)
  "每个属性层在 etml-tp-layer-alist 中，属性组指在
etml-tp-layer-groups 中存储属性层的名称层级关系与layers定义顺序一致。
最上面的定义表示顶层，渲染时会显示出来。"
  (declare (indent defun))
  (let ((layer-names
         (nreverse
          (mapcar (lambda (layer)
                    (let ((layer-name (car layer)))
                      (etml-tp-layer-define layer-name (cdr layer))
                      layer-name))
                  layers))))
    (if (assoc name etml-tp-layer-groups)
        (setf (cdr (assoc name etml-tp-layer-groups)) layer-names)
      (push (cons name layer-names) etml-tp-layer-groups))))

(defun etml-tp-propertize (string properties &optional layer)
  (declare (indent defun))
  (let ((layer (or layer (org-id-uuid))))
    (etml-tp-layer-push layer
      0 (length string) properties string)
    string))

(defun etml-tp-layer-propertize (string layer)
  (if-let ((layer-info (assoc layer etml-tp-layer-alist)))
      (etml-tp-propertize string
        (cdr layer-info) (car layer-info))
    (error "layer %S doesn't exist!" layer)))

(defun etml-tp-layer-group-propertize (string layer-group)
  (if-let* ((group-info (assoc layer-group etml-tp-layer-groups))
            (layers (cdr group-info)))
      (progn
        (dolist (layer layers)
          (setq string (etml-tp-layer-propertize string layer)))
        string)
    (error "layer group %S doesn't exist!" layer-group)))

(provide 'etml-tp)
