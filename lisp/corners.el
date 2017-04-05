;;; Locate A Window -*- lexical-binding: t; -*-
(defun loc (&optional corners)
  "describe location"
  (interactive "P")
  (or corners (setq corners (window-edges)))
  (let*
      ((fw (frame-width))
       (fh (1- (frame-height)))
       (tr 0)
       (mr (/ (frame-height) 2))
       (br fh)
       (lc 0)
       (mc (/ (frame-width) 2))
       (rc fw)
       (count (length (window-list))))
    (print
    (cond
     ((equal corners `(,lc ,tr ,mc ,br)) 'left-half)
     ((equal corners `(,mc ,tr ,rc, br)) 'right-half)
     ((equal corners `(,lc ,tr ,rc ,mr)) 'top-half)
     ((equal corners `(,lc ,mr ,rc ,br)) 'bottom-half)
     ((equal corners `(,lc ,tr ,mc ,mr)) 'top-left)
     ((equal corners `(,mc ,tr ,rc ,mr)) 'top-right)
     ((equal corners `(,lc ,mr ,mc ,br)) 'bottom-left)
     ((equal corners `(,mc ,mr ,rc ,br)) 'bottom-right)
(t corners))))


)
