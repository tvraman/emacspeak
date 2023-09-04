;; -*- lexical-binding: nil; -*-


(load-library "dismal-mode-defaults")
;;{{{ fix for emacs 21.3.5 (CVS)
;;; Need the following wrapper advice to make dismal run under
;;; emacs 21.3.5

(defun my-wrap-broken-function (f)
  "Wrap an around advice to catch errors in broken function f."
  (eval
   `(defadvice ,f (around fix-bug pre act comp)
      (condition-case nil
          ad-do-it
        (error nil))
      ad-return-value)))
(mapc 'my-wrap-broken-function
'(dismal-visit-cell dismal-goto-cell
dismal-draw-row-label dismal-draw-column-label
dismal-redraw-cell dismal-redraw-row dismal-redraw-column))


 ;;}}}
