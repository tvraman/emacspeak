;;; emacspeak-jss.el --- Speech-enable JSS
;;; $Id: emacspeak-jss.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable JSS An Emacs Interface to Browser Debuggers 
;;; Keywords: Emacspeak,  Audio Desktop jss
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNJSS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:
;;; jss connects Emacs to browsers that support the webkit debugging protocol.
;;; You can use this to connect to a running Chrome or Firefox.
;;; Make sure to start chrome with the correct command-line flag: e.g., on Linux:
;;; google-chrome --remote-debugging-port=9222
;;; You can get  Emacs package jss from here:
;;; url = git://github.com/segv/jss.git
;;; Make sure to first install the websocket package from elpa.
;;; This package speech-enables jss for Emacspeak users.
;;; This is what I use alongside package Kite at present when developing/debugging ChromeVox.
;;; ChromeVox == http://google-axs-chrome.googlecode.com
;;; http://chromevox.com

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Customizations:

(defgroup emacspeak-jss nil
  "Speech-enable JSS interaction."
  :group 'emacspeak)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
   (jss-console-debug-message  voice-smoothen)
   (jss-console-log-message    voice-monotone)
   (jss-console-warn-message   voice-animate)
   (jss-console-error-message  voice-animate-extra)
   (jss-http-repl-meta-data-face voice-monotone)
   (jss-http-repl-submitted-face voice-annotate)
   (jss-script-line-marker-face  voice-monotone)
   (jss-button-face  voice-bolden)
   ))

;;}}}
;;{{{ ToDos: Fix Interactive Commands:
                                        ; jss-console-toggle-timing-data
                                        ; jss-debugger-set-resume-point-here
                                        ; jss-debugger-stepper-frame-restart
                                        ; jss-debugger-stepper-resume
                                        ; jss-expand-nearest-remote-value
                                        ; jss-http-repl
                                        ; jss-http-repl-after-change-function
                                        ; jss-http-repl-choose-user-agent
                                        ; jss-http-repl-ensure-header
                                        ; jss-http-repl-read-authorization
                                        ; jss-http-repl-read-cache-control
                                        ; jss-http-repl-read-content-length
                                        ; jss-http-repl-read-content-type
                                        ; jss-http-repl-send-request
                                        ; jss-invoke-primary-action
                                        ; jss-invoke-secondary-action
                                        ; jss-io-clone-into-http-repl
                                        ; jss-prompt-beginning-of-line
                                        ; jss-prompt-eval-or-newline
                                        ; jss-prompt-insert-next-input
                                        ; jss-prompt-insert-previous-input
                                        ; jss-remote-value-expand-at-point
                                        ; jss-set-debugger-sensitivity
                                        ; jss-tab-goto-console
                                        ; jss-toggle-network-monitor
                                        ; jss-toggle-text-visibility
                                        ; 
;;}}}
;;{{{ Advice interactive commands:

;;; Advice Killing Commands:
(loop for f in
      '(jss-script-kill
        jss-debugger-kill
        jss-console-kill
        jss-browser-kill-buffer)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory icon."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-mode-line)))))

        ;;; Setup JSS buffers in programming mode:
(add-hook 'jss-super-mode-hook 'emacspeak-setup-programming-mode
          )
(add-hook
 'jss-super-mode-hook
 #'(lambda ()
     (emacspeak-auditory-icon 'open-object)
     (emacspeak-speak-mode-line)))

;;; Cue task completion

(loop for f in
      '(jss-browser-mode-refresh
        jss-connect
        jss-console-clear-buffer
        jss-console-ensure-connection
        jss-console-reload-page)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'task-done)
            (emacspeak-speak-mode-line)))))

;;; Navigators:

(loop for f in
      '(
        jss-previous-button
        jss-next-button
        jss-frame-previous
        jss-frame-next
        jss-frame-goto-source
        jss-frame-goto-exception
        jss-debugger-stepper-step-into
        jss-debugger-stepper-step-out
        jss-debugger-stepper-step-over
        jss-debugger-frame-goto-prompt
        )
      do
      (eval
       `(defadvice,f (after emacspeak pre act comp)
          "provide auditory feedback."
          (when  (ems-interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-jss)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
