;;; Locate A Window  -*- lexical-binding: t; -*-
(defun  loc (&optional corners)
  "describe location"
(or corners (setq corners (window-edges)))
  (let*
      ((fw (frame-width))
       (fh (1- (frame-height)))
       (tr 0)
       (mr (/ fh 2))
       (br fh)
       (lc 0)
       (mc (/ fw 2))
       (rc fw)
       (count (length (window-list))))
    (pcase  corners
           (`(0 0 99 61) `left-half)
(`(99 0 199 61) `right-half)
      )))
