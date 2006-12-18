;;; emacspeak-zinf.el --- Control zinf from Emacs
;;; $Id$
;;; $Author$
;;; Description: Controlling zinf from emacs 
;;; Keywords: Emacspeak, zinf
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

;;; Copyright (c) 1995 -- 2006, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction:

;;; Commentary:

;;; Defines a simple derived mode for interacting with
;;; zinf.
;;; zinf == zinf is not freeamp
;;; zinf navigation commands then work via single keystrokes.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{ define a derived mode for zinf interaction 

(defvar emacspeak-zinf-process nil
  "Process handle to zinf." )

;;;###autoload
(define-prefix-command 'emacspeak-zinf-prefix-command
  'emacspeak-zinf-mode-map)

(define-derived-mode emacspeak-zinf-mode fundamental-mode 
  "Zinf Interaction"
  "Major mode for zinf interaction. \n\n
\\{emacspeak-zinf-mode-map}"
  (setq emacspeak-zinf-process (get-buffer-process (current-buffer))))

(declaim (special emacspeak-zinf-mode-map))

(defvar emacspeak-zinf-zinf-keys
  (list ?p ?+ ?-  ?f ?b ?s ?= ?q
        ?F ?B ?j ?J ??)
  "Keys accepted by zinf.")

;;;###autoload
(defun emacspeak-zinf-zinf-command (char)
  "Execute Zinf command."
  (interactive "cZinf Command:")
  (declare (special emacspeak-zinf-process))
  (let*  ((buffer (process-buffer emacspeak-zinf-process))
          (mark nil))
    (save-excursion
      (set-buffer buffer)
      (setq mark (point-max))
      (process-send-string
       emacspeak-zinf-process
       (format "%c" char))
      (accept-process-output emacspeak-zinf-process 1)
      (message "%s"
               (buffer-substring mark (point-max))))))
;;;###autoload
(defun emacspeak-zinf-zinf-call-command ()
  "Call appropriate zinf command."
  (interactive)
  (emacspeak-zinf-zinf-command last-input-char)
  (when (char-equal last-input-char ?q)
    (emacspeak-aumix-reset)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(define-key emacspeak-zinf-mode-map  "z" 'emacspeak-zinf)
(loop for c in emacspeak-zinf-zinf-keys
      do
      (define-key emacspeak-zinf-mode-map   (format
                                                "%c" c)
        'emacspeak-zinf-zinf-call-command))
(define-key emacspeak-zinf-mode-map [left]
  'emacspeak-aumix-wave-decrease)
(define-key emacspeak-zinf-mode-map [right] 'emacspeak-aumix-wave-increase)

;;}}}
;;{{{ emacspeak-zinf

;;;###autoload
(defun emacspeak-zinf (resource)
  "Play specified resource using zinf.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-zinf-mode."
  (interactive
   (list
    (read-file-name "MP3 Resource: "
                    (when (eq major-mode 'dired-mode)
                      (dired-get-filename)))))
  (declare (special emacspeak-zinf-process))
  (when (and emacspeak-zinf-process
             (eq 'run (process-status
                       emacspeak-zinf-process))
             (y-or-n-p "Stop currently playing music? "))
    (kill-buffer (process-buffer emacspeak-zinf-process))
    (setq emacspeak-zinf-process nil))
  (let ((process-connection-type nil))
    (setq emacspeak-zinf-process
          (start-process
           "zinf" "zinf""aoss"
           "zinf"
           "-ui" "cmdline.ui"
           (expand-file-name resource)))
    (switch-to-buffer (process-buffer
                       emacspeak-zinf-process))
    (emacspeak-zinf-mode)))

;;}}}
(provide 'emacspeak-zinf)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
