;;; Taken from endless' Blog:  -*- lexical-binding: t; -*-

(define-key ctl-x-map "\C-i" #'auto-correct-update)
;;; shift-space is too  easy to trigger accidentally.
;(global-set-key  (kbd "S-SPC") 'auto-correct-update)
(defun auto-correct-update (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) "")))
        (aft nil))
    (call-interactively #'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (or (string= aft bef)
                (string= aft "")
                (string= bef ""))
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)
