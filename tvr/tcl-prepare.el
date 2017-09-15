;; -*- lexical-binding: nil; -*-

;; If you plan to use the interface to the TclX help files, you must
;; set the variable tcl-help-directory-list to point to the topmost
;; directories containing the TclX help files.  Eg:
;;

(eval-after-load "tcl"
  (progn
    (add-hook 'tcl-mode-hook 'tcl-auto-fill-mode)
    (add-hook 'tcl-mode-hook 'tcl-guess-application)
    (setq tcl-prompt-regexp "[-_a-zA-Z]+> \| [=>]+")
    (add-hook
     'inferior-tcl-mode-hook
     #'(lambda  ()
         (define-key  inferior-tcl-mode-map "\C-c\C-p" 'comint-previous-prompt )))
    (setq tcl-help-directory-list '("/usr/lib/tclX8.6/help/tcl"))

    (setq tcl-default-application "tcl")
    ))
