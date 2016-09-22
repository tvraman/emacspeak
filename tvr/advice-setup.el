;;; load advice on startup:  -*- lexical-binding: t; -*-
(require 'advice)
;;{{{  rcs advice: 

(defadvice vc-register (before make-rcs-dir activate)
  "Automatically create an RCS directory before registering a file."
  (if (not (file-exists-p "RCS"))
      (make-directory "RCS")))

;;}}}
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
