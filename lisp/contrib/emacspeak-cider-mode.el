;;; emacspeak-cider-mode.el --- Speech-Enable cider-mode  -*- lexical-binding: t; -*- 
;;; $Author: marshall.flax@gmail.com, but under GPL 2+ and copyright assigned to T.V. Raman, and/or the emacspeak project $
;;; Description: speech-enable cider-mode
;;; Keywords: Emacspeak, Cider, Clojure
;;; 

;;}}}
;;{{{ Introduction: Cider mode is for programming Clojure.
;;; cider-eval-defun-at-point works wonderfully evaluating an expression and displaying inline the result.
;;; But when there's an error, it silently switches to a stacktrace buffer -- so a blind user can't tell the difference between (1) a computation that takes a long time and (2) a computation that threw a stack trace.
;;; So we add one piece of advice to announce the buffer change (i.e. when it calls cider-popup-buffer-display)

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice interactive commands:

(defadvice cider-popup-buffer-display (after emacspeak pre act comp)
  “So we know that focus switched to stacktrace buffer”
     (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-cider-mode)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end: 

;;}}}
