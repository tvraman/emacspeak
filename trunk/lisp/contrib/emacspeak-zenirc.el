;;; emacspeak-zenirc.el --- Speech-enabling extension for the Emacs IRC Client Zenirc.
;;{{{ Copyright

;;; Copyright (C) 2000 Ben van Poppel

;; Author Ben van Poppel <benny@netspace.net.au>
;; Created: 9 Jul. 2000.

;; Emacspeak is copyright (C) 1995--2000 by T. V. Raman, 1994--95 by 
;; Digital Equipment Corporation. All rights reserved.
;; emacspeak-zenirc.el is free software; you are free to copy/modify it 
;; under the terms of the GNU General Public License as published by the 
;; Free Software Foundation Inc.; version 2, or, at your option, any
;; later version. 

;; Emacspeak-zenirc.el is distributed in the hope that it will be
;; useful, but withou any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details.

;;}}}
;;{{{ Introduction

;;; Commentary:

;; Zenirc is an IRC client written in Emacs Lisp. Now you can waste
;; time on IRC from the comfort of the world's greatest text editor!
;; This is an Emacspeak extension for Zenirc which ensures that blind
;; people, too, can waste time on IRC from within Emacs. 

;; Much of this code was directly pinched from the comint sections of
;; emacspeak-speak.el and emacspeak-advice.el and fixed to make Zenirc
;; talk.
;;; zenirc is found at 
;;; /anonymous@ftp.cis.ohio-state.edu:/pub/emacs-lisp/incoming/zenirc-2.112.tar.gz

;;; Code:

;;}}}
;;{{{ required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'emacspeak-keymap)
(require 'voice-lock)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)

;;}}}
;;{{{ zenirc autospeak 

;; Autospeak and output monitors:

(defvar emacspeak-zenirc-autospeak t
  "With non-nil value, speak Zenirc output.
Do not set this by hand, use command
`emacspeak-zenirc-toggle-autospeak' bound to 
\\[emacspeak-zenirc-toggle-autospeak]")

(defun emacspeak-zenirc-toggle-autospeak (&optional prefix)
  "Toggle state of `emacspeak-zenirc-autospeak`.
When on, messages output to the Zenirc buffer are automatically
spoken. Interactive prefix argument means toggle the global default
value, then set the current local value to the result."
  (interactive  "P")
  (declare  (special  emacspeak-zenirc-autospeak ))
  (cond
   (prefix
    (setq-default  emacspeak-zenirc-autospeak
                   (not  (default-value 'emacspeak-zenirc-autospeak )))
    (setq emacspeak-zenirc-autospeak (default-value 'emacspeak-zenirc-autospeak )))
   (t (make-local-variable 'emacspeak-zenirc-autospeak)
      (setq emacspeak-zenirc-autospeak
	    (not emacspeak-zenirc-autospeak ))))
  (emacspeak-auditory-icon
   (if emacspeak-zenirc-autospeak 'on 'off))
  (message "Turned %s zenirc autospeak %s "
           (if emacspeak-zenirc-autospeak "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{ output monitor 

(defvar emacspeak-zenirc-output-monitor nil
  "If non-nil, speak Zenirc output regardless of current buffer.")

(make-variable-buffer-local
 'emacspeak-zenirc-output-monitor)

(defun emacspeak-zenirc-toggle-output-monitor (&optional prefix)
"Toggle state of Zenirc output monitor.
When on, messages output to the Zenirc buffer are spoken regardless of
the current buffer or window. Interactive prefix argument means toggle
the global default value, then set the current local value to the
result."
  (interactive  "P")
  (declare  (special  emacspeak-zenirc-output-monitor ))
  (cond
   (prefix
    (setq-default  emacspeak-zenirc-output-monitor
                   (not  (default-value 'emacspeak-zenirc-output-monitor )))
    (setq emacspeak-zenirc-output-monitor (default-value 'emacspeak-zenirc-output-monitor )))
   (t (make-local-variable 'emacspeak-zenirc-output-monitor)
      (setq emacspeak-zenirc-output-monitor
	    (not emacspeak-zenirc-output-monitor ))))
  (emacspeak-auditory-icon
   (if emacspeak-zenirc-output-monitor 'on 'off))
  (message "Turned %s zenirc monitor %s "
           (if emacspeak-zenirc-output-monitor "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{ advice zenirc messages 

;; Advise zenirc-message to speak output to the Zenirc buffer:

(defadvice zenirc-message (around emacspeak pre act comp)
  "Speak any output to the Zenirc buffer."
  (declare (special emacspeak-zenirc-autospeak
		    emacspeak-zenirc-output-monitor))
  (catch 'out
    (save-excursion
      (let ((buffer (ad-get-arg 0)))
	(cond
	 ((bufferp buffer)
	  nil)
	 ((processp buffer)
	  (setq buffer (process-buffer buffer)))
	 (t
	  ad-do-it
	  (throw 'out t)))
	(set-buffer buffer)
	(let 
	    ((start (marker-position zenirc-process-mark))
	     (monitor emacspeak-zenirc-output-monitor)
	     (dtk-stop-immediately nil))
	  ad-do-it
	  (when (and emacspeak-zenirc-autospeak
		     (or monitor
			 (eq (selected-window)
			     (get-buffer-window buffer))))
	    (emacspeak-speak-region start (marker-position zenirc-process-mark))
	    ad-return-value))))))

;;}}}
;;{{{ voice lock 

;; Voice locking:

;; Zenirc voices:

(defvar emacspeak-zenirc-info-voice 'betty)
(defvar emacspeak-zenirc-action-voice 'paul-animated)
(defvar emacspeak-zenirc-query-voice 'paul-monotone)
(defvar emacspeak-zenirc-nick-voice 'frank)
(defvar emacspeak-zenirc-userhost-voice 'harry)
(defvar emacspeak-zenirc-pubmsg-voice 'paul)
(defvar emacspeak-zenirc-primsg-voice 'wendy)

(defvar emacspeak-zenirc-voice-lock-messages nil
"Set this to T if you want messages automatically voice
locked.")

(defvar zenirc-voice-lock-keywords nil
  "Keywords to highlight in Zenirc")

(setq zenirc-voice-lock-keywords
      (append zenirc-voice-lock-keywords
	      '(("^\\[info\\]\\(.*\\)$" 1 emacspeak-zenirc-info-voice)
		("^\\[action\\]\\(.*\\)$" 1 emacspeak-zenirc-action-voice)
		("^\\[query\\]\\(.*\\)$" 1 emacspeak-zenirc-query-voice)
		("^[<*]\\(\\([^>*]*!\\)\\|\\([^!]*[>*]\\)\\)" 1 emacspeak-zenirc-nick-voice)
		("^[<*][^>*]*!\\(.*@.*[>*]\\)" 1 emacspeak-zenirc-userhost-voice)
		("^<.*>\\(.*\\)$" 1 emacspeak-zenirc-pubmsg-voice)
		("^\\*.*\\*\\(.*\\)$" 1 emacspeak-zenirc-primsg-voice))))

(voice-lock-set-major-mode-keywords 'zenirc-mode 'zenirc-voice-lock-keywords)


(add-hook 'zenirc-startup-hook
          (function
           (lambda ()
             (setq dtk-punctuation-mode "some")
             (when dtk-allcaps-beep
               (dtk-toggle-allcaps-beep))
             (emacspeak-dtk-sync)
             (when emacspeak-zenirc-voice-lock-messages
               (condition-case nil
                   (voice-lock-mode 1)
                 (error nil ))))))

;;}}}
;;{{{ keymap

;; Keymap:

(defvar emacspeak-zenirc-map nil
  "Sub-map containing speech-related Zenirc commands.")

(unless emacspeak-zenirc-map
  (setq emacspeak-zenirc-map (make-sparse-keymap))
  (define-key emacspeak-keymap "z" emacspeak-zenirc-map)
  (define-key emacspeak-zenirc-map "a"
    'emacspeak-zenirc-toggle-autospeak)
  (define-key emacspeak-zenirc-map "o"
    'emacspeak-zenirc-toggle-output-monitor)

(declaim (special zenirc-mode-map ))
  (define-key zenirc-mode-map [27 1] 'emacspeak-zenirc-toggle-autospeak)
  (define-key zenirc-mode-map [27 15]
    'emacspeak-zenirc-toggle-output-monitor))

;;}}}

(provide 'emacspeak-zenirc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
