;;; tts.jl -- Sawfish interface  to Emacspeak speech servers 
;;; $Id$
;;; $Author$
;;; Description:   Interface REP/Sawfish to Emacspeak TTS servers
;;; Keywords: Sawfish, Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C)  2000 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;;; Interface REP/Sawfish to Emacspeak TTS servers

;;}}}
;; Customization  options.

(defgroup tts "Speech synthesis")

(defvar emacspeak "/home/raman/emacs/lisp/emacspeak"
"Root of Emacspeak installation.")

(defvar tts-process nil
"Handle to tts server connection.")



;;; Servers

(defvar tts-dtk
  (expand-file-name "servers/dtk-exp" emacspeak)
"DTK tcl server")

(defvar tts-outloud
  (expand-file-name "servers/outloud" emacspeak)
  "Outloud tcl server")

(defvar tts-32-outloud
  (expand-file-name "servers/32-outloud" emacspeak)
  "Outloud tcl server")
(defvar tts-engine tts-dtk
"Default TTS  engine. User settable.")

(defun tts-open ()
  "Open a TTS session."
  (interactive)
  (setq tts-process (make-process))
  (start-process tts-process  tts-engine))

(defun tts-close ()
  "Close a TTS session."
  (interactive)
  (when(and  (processp tts-process)
             (process-running-p tts-process))
    (kill-process tts-process))
  (setq tts-process nil))

(defun tts-running-p ()
  "Is there a tts process up and running?"
  (and (processp tts-process) (process-running-p
                               tts-process)))

(defvar tts-stop-immediately t
  "Non nil means previous speech is flushed immediately.")

(defun tts-say (text)
  "Say some text."
  (unless (and  tts-process
 (process-running-p tts-process))
      (tts-open))
  (when tts-stop-immediately
    (format tts-process "s\n"))
  (format tts-process "q {%s}; d\n" text))

(defun tts-say-workspace ()
  "Say the name of the current workspace."
  (interactive)
  (tts-say (or (nth current-workspace workspace-names)
                    (format nil "Workspace %d"
                            current-workspace))))

(defvar tts-say-window-details-p nil 
"Non-nil means we also speak the window's position and dimensions.")

(defun tts-say-window (window)
  "Say the name of window W."
  (interactive "%W")
  (when window 
  (let ((title (window-name window))
        (position (window-position window))
        (dimensions (window-dimensions window)))
    (if tts-say-window-details-p
        (tts-say
         (format nil "%s at %s with dimensions %s"
                 title position dimensions))
      (tts-say title)))))

(defun tts-say-current-window ()
  "Say the name of the current window."
  (interactive)
  (tts-say-window (input-focus)))

(defun tts-say-workspace-on-change (enable)
  "Enable/disable the reading of a workspace's name when you change to it."
  (if enable
      (unless (in-hook-p 'enter-workspace-hook tts-say-workspace)
        (add-hook 'enter-workspace-hook tts-say-workspace))
    (remove-hook 'enter-workspace-hook tts-say-workspace)))

(defun tts-say-window-on-focus (enable)
  "Enable/disable the reading of a window's name when it receives focus."
  (if enable
      (unless (in-hook-p 'focus-in-hook tts-say-window)
        (add-hook 'focus-in-hook tts-say-window))
    (remove-hook 'focus-in-hook tts-say-window)))
(defun tts-describe-key ()
  "Speak the output of `describe-key'."
  (interactive)
  (tts-say
   (let ((standard-output (make-string-output-stream)))
     (describe-key)
     (get-output-stream-string standard-output))))

(provide 'tts)
(message "Loaded tts.jl")

;;; tts.jl ends here
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
