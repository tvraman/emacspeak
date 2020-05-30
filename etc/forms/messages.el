;;; emacspeak/forms.el --- Speech friendly interface to /var/log/messages  -*- lexical-binding: t; -*-
;;;$Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable sql-mode
;;; Keywords: forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  setup forms

(setq forms-read-only t)
(setq forms-file
      (read-file-name  "Messages file: "
                       "/var/log/"
                       "/var/log/messages"))

(setq forms-read-only nil)
(setq forms-field-sep
      (substring (system-name)
                 0
                 (string-match "\\." (system-name))))
(setq forms-number-of-fields 2)

(setq forms-format-list
      (list
       "Message: "2
       "\n"
       "Date: "1
       "\n"))

;;}}}

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
