;;; emacspeak-freeamp.el --- Control freeamp from Emacs
;;; $Id$
;;; $Author$
;;; Description: Controlling freeamp from emacs 
;;; Keywords: Emacspeak, freeamp
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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
;;; freeamp.
;;; If you use freeamp  as your mp3 player from w3 for
;;; example,
;;; put the buffer containing freeamp in freeamp-mode.
;;; freeamp navigation commands then work via single keystrokes.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{ define a derived mode for freeamp interaction 
(defvar emacspeak-freeamp-process nil
  "Process handle to freeamp." )

;;;###autoload
(define-prefix-command 'emacspeak-freeamp-prefix-command
  'emacspeak-freeamp-mode-map)

(define-derived-mode emacspeak-freeamp-mode fundamental-mode 
  "Freeamp Interaction"
  "Major mode for freeamp interaction. \n\n
\\{emacspeak-freeamp-mode-map}"
  (setq emacspeak-freeamp-process (get-buffer-process (current-buffer))))

(declaim (special emacspeak-freeamp-mode-map))

(defvar emacspeak-freeamp-freeamp-keys
  (list ?p ?+ ?-  ?f ?b ?s ?= ?q)
  "Keys accepted by freeamp.")
;;;###autoload
(defun emacspeak-freeamp-freeamp-command (char)
  "Execute FreeAmp command."
  (interactive "cFreeamp Command:")
  (declare (special emacspeak-freeamp-process))
  (let*  ((buffer (process-buffer emacspeak-freeamp-process))
          (mark nil))
    (save-excursion
      (set-buffer buffer)
      (setq mark (point-max))
      (process-send-string
       emacspeak-freeamp-process
       (format "%c" char))
      (accept-process-output emacspeak-freeamp-process 1)
      (message "%s"
	       (buffer-substring mark (point-max))))))
;;;###autoload
(defun emacspeak-freeamp-freeamp-call-command ()
  "Call appropriate freeamp command."
  (interactive)
  (emacspeak-freeamp-freeamp-command last-input-char)
  (when (char-equal last-input-char ?q)
    (emacspeak-aumix-reset)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(define-key emacspeak-freeamp-mode-map  "o" 'emacspeak-freeamp)
(loop for c in emacspeak-freeamp-freeamp-keys
      do
      (define-key emacspeak-freeamp-mode-map   (format
						"%c" c)
        'emacspeak-freeamp-freeamp-call-command))
(define-key emacspeak-freeamp-mode-map [left]
  'emacspeak-aumix-wave-decrease)
(define-key emacspeak-freeamp-mode-map [right] 'emacspeak-aumix-wave-increase)

;;}}}
;;{{{ emacspeak-freeamp
;;;###autoload
(defun emacspeak-freeamp (resource)
  "Play specified resource using freeamp.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-freeamp-mode."
  (interactive
   (list
    (read-file-name "MP3 Resource: "
                    (when (eq major-mode 'dired-mode)
		      (dired-get-filename)))))
  (declare (special emacspeak-freeamp-process))
  (when (and emacspeak-freeamp-process
             (eq 'run (process-status
                       emacspeak-freeamp-process))
             (y-or-n-p "Stop currently playing music? "))
    (kill-buffer (process-buffer emacspeak-freeamp-process))
    (setq emacspeak-freeamp-process nil))
  (let ((process-connection-type nil))
    (setq emacspeak-freeamp-process
          (start-process
           "freeamp" "freeamp" "freeamp"
           (expand-file-name resource)))
    (switch-to-buffer (process-buffer
                       emacspeak-freeamp-process))
    (emacspeak-freeamp-mode)))

;;}}}
(provide 'emacspeak-freeamp)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
