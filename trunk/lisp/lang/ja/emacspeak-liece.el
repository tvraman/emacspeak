;;; emacspeak-liece.el --- Speech enable liece -- Fluent spoken access to IRC
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)

(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar emacspeak-liece-stop-immediately t)
(defadvice liece (after emacspeak pre act )
  "read the mode line after liece starts."
  (emacspeak-speak-mode-line))

(defadvice liece-command-quit (after emacspeak pre act )
  "announces after liece quit."
  (emacspeak-speak-mode-line))

(defadvice liece-command-toggle-channel-buffer-mode (after emacspeak pre act )
  "read the mode line after liece-command-toggle-channel-buffer-mode."
  (emacspeak-speak-mode-line))


(defadvice liece-command-toggle-nick-buffer-mode (after emacspeak pre act )
  "read the mode line after liece-command-toggle-nick-buffer-mode."
  (emacspeak-speak-mode-line))

(defadvice liece-freeze (after  emacspeak pre act )
  "Announce freeze status" 
  (with-current-buffer buffer
    (if (string= liece-freeze-indicator "F")
	(dtk-speak "freeze on")
      (dtk-speak "freeze off"))))

(defadvice liece-own-freeze (after emacspeak pre act )
  "Announce own-freeze status" 
  (with-current-buffer buffer
    (if (string= liece-own-freeze-indicator "M")
	(dtk-speak "own-freeze on")
      (dtk-speak "own-freeze off"))))

(defadvice liece-set-beep (after  emacspeak pre act )
  "Announce beep status" 
  (with-current-buffer buffer
    (if (string= liece-beep-indicator "B")
	(dtk-speak "beep on")
      (dtk-speak "beep off"))))

(defadvice liece-switch-to-channel (after emacspeak pre act )
  "read the channel name after liece-switch-to-channel."
  (dtk-speak chnl))

(defadvice liece-switch-to-channel-no (after emacspeak pre act )
  "read the channel number after liece-switch-to-channel-no."
  (dtk-speak num))

(defadvice liece-insert-internal (after emacspeak pre act )
  "speak liece-insert-internal"
  (if (bufferp buffer)
      (if (string-match "\\*Channel:" (buffer-name buffer))
	  (if (liece-get-buffer-window buffer)
	      (let ((dtk-stop-immediately emacspeak-liece-stop-immediately)
		    (dtk-stop-immediately-while-typing emacspeak-liece-stop-immediately))
		(dtk-speak string))))))

(defun emacspeak-liece-toggle-stop ()
  "Stop speak message if next message come."
  (interactive)
  (if emacspeak-liece-stop-immediately
      (progn (setq emacspeak-liece-stop-immediately nil)
	     (dtk-speak "stop off"))
    (progn (setq emacspeak-liece-stop-immediately t)
	   (dtk-speak "stop on"))
    )
  )


(provide 'emacspeak-liece)

