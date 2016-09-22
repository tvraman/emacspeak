;;; Original Author: Tim Cross  -*- lexical-binding: t; -*-
;;; Updated for inclusion in the Emacspeak contrib section.
;;      Filename: 10speech.el
;; Creation Date: Sunday, 16 January 2005 01:38 PM EST
;; Last Modified: Thursday, 03 June 2010 10:04 AM EST
;;        Author: Tim Cross <tcross@une.edu.au>
;;   Description: Script to load emacspeak when you run emacs
;;
;;; Set environment variable RUN_EMACSPEAK to activate.
;;; Set EMACSPEAK_DIR to custom emacspeak location.
;;; Defaults to /usr/share/emacs/site-lisp/emacspeak
(when (getenv "RUN_EMACSPEAK")
  (unless (or   noninteractive (featurep 'emacspeak))
    (let ((emacspeak-location
           (or (getenv "EMACSPEAK_DIR")
               "/usr/share/emacs/site-lisp/emacspeak")))
      (add-to-list 'load-path (expand-file-name "lisp"emacspeak-location))
      (load-library "emacspeak-setup"))


;;; This ensures that any speech rate settings take effect on startup:
    (add-hook
     'after-init-hook
     #'(lambda ()
         (when (file-exists-p custom-file) (load-file custom-file))
         (emacspeak-tts-startup-hook)
         (message "Successfully initialized Emacs")))
    ))


