;;; stumpwmrc -- Attach Speech actions to StumpWM hooks
;;; $Id: stumpwmrc 7078 2011-06-29 22:07:46Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:   Interface Attach TTS  actions to Window hooks
;;; Keywords: StumpWM, Emacspeak, Audio Desktop
;;; {TTS

;;(use-package :cl-user)
;; (load "/home/raman/emacs/lisp/emacspeak/stumpwm/tts.lisp")
;; (setq *tts-engine* *tts-32-outloud*)
;; (tts-say "TTS: Ready to talk! ")

;;; }
;;; {Speak Actions:

;; (defun speak-window (window)
;;   "Speak  window  information."
;; (tts-speak (stumpwm::window-name window)))


;; (defun speak-current-window ()
;;   "Speak current window  information."
;;   (tts-speak (stumpwm::window-name (stumpwm::current-window))))

;; (defun speak-messages (&rest messages)
;;   "Speak messages, a list of lines."
;;   (tts-speak-list  messages))

;;; }
;;; {Attach Handlers:

;;(setq  stumpwm:*new-window-hook* (list 'speak-current-window))
;(stumpwm:add-hook 'stumpwm:*new-window-hook* 'speak-current-window)
;*destroy-window-hook*
;; (setq  stumpwm:*focus-window-hook* (list 'speak-current-window))
;(stumpwm:add-hook 'stumpwm:*focus-window-hook* 'speak-current-window)
;*focus-frame-hook*
;*new-frame-hook*
;*message-hook*
;*message-hook*
;*focus-group-hook*
;*urgent-window-hook*

;;; }

;;; { end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;; }
