;;; load advice on startup:
(require 'advice)
;;{{{  rcs advice: 

(defadvice vc-register (before make-rcs-dir activate)
  "Automatically create an RCS directory before registering a file."
  (if (not (file-exists-p "RCS"))
      (make-directory "RCS")))

;;}}}
;;{{{   save .bbdb when quitting vm

(defadvice vm-quit (after save-bbdb-after-vm activate)
  "Save .bbdb when vm is quit. "
  (if (get-buffer ".bbdb")
      (set-buffer ".bbdb")
    (save-buffer)))

;;}}}
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
