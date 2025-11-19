;; -*- lexical-binding: t -*-
;;; String operation related to pixel

(require 's)
(require 'ekp)

(defun etaf-pixel-spacing (pixel)
  "Return a pixel spacing with a PIXEL pixel width."
  (if (= pixel 0)
      ""
    (propertize " " 'display `(space :width (,pixel)))))

(defun etaf-pixel-pad (s prefix-pixel &optional suffix-pixel)
  "Pad the start of string S with PREFIX-PIXEL pixel width of
pixel spacing, and the end of string S with SUFFIX-PIXEL pixel width."
  (s-wrap s (etaf-pixel-spacing prefix-pixel)
          (etaf-pixel-spacing (or suffix-pixel 0))))

(defun etaf-pixel--smart-offset (s total-pixel offset-pixel)
  "When OFFSET is a positive number, there is a offset pixel distance
from start. When OFFSET is a negative number, there is a OFFSET pixel
from the end. Return the offset from start."
  (let ((str-pixel (string-pixel-width s)))
    (when (> str-pixel total-pixel)
      (error "pixel width of string is %s, it should not\
 more than total-pixel %s" str-pixel total-pixel))
    (let ((rest-pixel (- total-pixel str-pixel)))
      (cond ((>= offset-pixel 0) (min offset-pixel rest-pixel))
            ((< offset-pixel 0)
             (max 0 (+ offset-pixel rest-pixel)))))))

(defun etaf-pixel--align-offset (s total align)
  "Return the offset from start in TOTAL pixel after
setting ALIGN type."
  (pcase align
    ('left (etaf-pixel--smart-offset s total 0))
    ('right (etaf-pixel--smart-offset
             s total (- total (string-pixel-width s))))
    ('center (etaf-pixel--smart-offset
              s total (/ (- total (string-pixel-width s)) 2)))
    (_ (error "Invalid value of ALIGN: %S" align))))

(defun etaf-pixel-reach (s total-pixel &optional offset side)
  "Make the pixel width of string S reach to TOTAL-PIXEL. If OFFSET
is non-nil, make OFFSET pixel width of offset from start. If FROM-END
is non-nil, offset from the end.

TOTAL-PIXEL must be equal for more than the pixel width of string S.

SIDE should be 'left or 'right. OFFSET could either be a positive
or negative number. If they are not set, SIDE will be set to 'left
defaultly and offset will be set to 0 defaultly.

When OFFSET is positive, offset from SIDE side of S.
When OFFSET is negative, offset from the reverse SIDE side of S."
  (let* ((side (or side 'left))
         (offset (etaf-pixel--smart-offset s total-pixel
                                                 (or offset 0)))
         (rest-pixel (- total-pixel (string-pixel-width s)))
         left-pixel)
    (pcase side
      ('left (setq left-pixel offset))
      ('right (setq left-pixel (- rest-pixel offset)))
      (_ (error "Invalid value of SIDE: %S" side)))
    (etaf-pixel-pad s left-pixel (- rest-pixel left-pixel))))

(defun etaf-pixel-align (s total-pixel &optional align)
  "Make the pixel width of string S reach to TOTAL-PIXEL and then
align it by ALIGN type.

ALIGN should be one of 'left, 'center, 'right.
TOTAL-PIXEL must be equal or more than the pixel width of string S."
  (let ((offset (etaf-pixel--align-offset
                 s total-pixel (or align 'left))))
    (etaf-pixel-reach s total-pixel offset)))

(defun etaf-pixel-center (s total-pixel)
  "Make the pixel width of string S reach to TOTAL-PIXEL and then
make it at the center of TOTAL-PIXEL width."
  (etaf-pixel-align s total-pixel 'center))

(defun etaf-pixel-left (s total-pixel)
  "Make the pixel width of string S reach to TOTAL-PIXEL and then
make it at the left of TOTAL-PIXEL width."
  (etaf-pixel-align s total-pixel 'left))

(defun etaf-pixel-right (s total-pixel)
  "Make the pixel width of string S reach to TOTAL-PIXEL and then
make it at the right of TOTAL-PIXEL width."
  (etaf-pixel-align s total-pixel 'right))

;; keep words readable
(defun etaf-pixel-wrap (s pixel)
  "Wrap string s to make each line up to PIXEL width."
  (ekp-pixel-justify s pixel))

(defun etaf-pixel-typeset (s pixel &optional align)
  (let ((str-pixel (string-pixel-width s)))
    (if (> str-pixel pixel)
        (etaf-pixel-wrap s pixel) 
      (etaf-pixel-align s pixel align))))

;;; pixel cut

;; do not keep words readable
(defun etaf-pixel--floor-string (s pixel)
  "Return the part in string S and the pixel
width of it is not more than PIXEL pixel."
  (if (<= (string-pixel-width s) pixel)
      s
    (let (new-s (curr-pixel 0))
      (catch 'break
        (dolist (char (split-string s "" t))
          (let ((char-pixel (string-pixel-width char)))
            (if (> char-pixel pixel)
                (throw 'break "")
              (if (> (+ curr-pixel char-pixel) pixel)
                  (throw 'break new-s)
                (setq curr-pixel (+ curr-pixel char-pixel))
                (setq new-s (concat new-s char)))))))
      new-s)))

(defun etaf-pixel-keep-left (s pixel)
  (let* ((part-s (etaf-pixel--floor-string s pixel))
         (part-pixel (string-pixel-width part-s)))
    (concat part-s (etaf-pixel-spacing (- pixel part-pixel)))))

(defun etaf-pixel-keep-right (s pixel)
  (let* ((part-s (etaf-pixel--floor-string (reverse s) pixel))
         (part-s (reverse part-s))
         (part-pixel (string-pixel-width part-s)))
    (concat (etaf-pixel-spacing (- pixel part-pixel)) part-s)))

(defun etaf-pixel-chop-left (s pixel)
  (let ((right-pixel (- (string-pixel-width s)
                        (min (string-pixel-width s) pixel))))
    (etaf-pixel-keep-right s right-pixel)))

(defun etaf-pixel-chop-right (s pixel)
  (let ((left-pixel (- (string-pixel-width s)
                       (min (string-pixel-width s) pixel))))
    (etaf-pixel-keep-left s left-pixel)))

(provide 'etaf-pixel)
