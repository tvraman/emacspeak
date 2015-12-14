;;; soundscape.el --- Soundscapes for The Emacspeak Desktop
;;; Description:  Soundscapes Using Boodler
;;; Keywords: Emacspeak,  Audio Desktop Soundscapes
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
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; http://boodler.org is a Python-based SoundScape generator.
;;; This module defines Emacspeak conveniences for running  Soundscapes.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'subr-x)

;;}}}
;;{{{ Configuration:

(defconst soundscape-player (executable-find "boodler")
  "Soundscape player. Looks for installed boodler.")

(defconst soundscape-mgr (executable-find "boodle-mgr")
  "Soundscape manager. Looks for installed boodler.")

(defconst soundscape-list (expand-file-name "soundscapes"  "~/.boodler")
  "Soundscape player. Looks for installed boodler.")

(defvar soundscape--catalog nil
  "Catalog of installed soundscapes keyed by agent name.")

;;}}}
;;{{{ Catalog:

(defun soundscape-catalog (&optional refresh)
  "Return catalog of installed Soundscapes, initialize if necessary."
  (declare (special soundscape--catalog soundscape-list))
  (cond
   ((and soundscape--catalog (null refresh)) soundscape--catalog)
   ((null (file-exists-p soundscape-list))
    (error "Soundscape catalog not initialized."))
   (t
    (let ((name nil)
          (path nil))
      (with-temp-buffer
        (insert-file-contents soundscape-list)
        (goto-char (point-min)) (while
            (not (eobp))
          (setq path
                (buffer-substring (line-beginning-position) (line-end-position)))
          (setq name (second (split-string path "/")))
          (when (and name path)
            (push (cons name path) soundscape--catalog))
          (forward-line 1))))
    soundscape--catalog)))

(defsubst soundscape-lookup (name)
  "Return package/agent for this name."
  (cdr (assoc name (soundscape-catalog))))

(defsubst soundscape-reverse-lookup (path)
  "Return name for this package/agent."
  (cdr (rassocassoc path (soundscape-catalog))))


(defun soundscape-read ()
  "Read name of Soundscape with completion."
  (let ((completion-ignore-case t))
    (soundscape-lookup
     (completing-read "Soundscape: " (soundscape-catalog)))))

;;}}}
;;{{{ Running:

(defvar soundscape-processes (make-hash-table :test #'equal)
  "Hash table of running Soundscapes.")

;;;###autoload
(defun soundscape (scape)
  "Play soundscape."
  (interactive (list (soundscape-read)))
  (declare (special soundscape-processes))
  (let ((proc
         (start-process "SoundScape" nil soundscape-player "-o" "alsa" scape))
        (name (second (split-string scape "/"))))
    (when (process-live-p proc)
      (puthash name proc soundscape-processes)
      (message "Started %s" scape))))

(defun soundscape-stop (name)
  "Stop running Soundscape."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Stop Soundscape:"
                       (hash-table-keys soundscape-processes)))))
  (declare (special soundscape-processes))
  (delete-process (gethash name soundscape-processes))
  (remhash  name soundscape-processes)
  (message "Stopped soundscape %s" name))

(defun soundscape-kill ()
  "Stop all running soundscapes."
  (interactive)
  (declare (special soundscape-processes))
  (mapcar  #'soundscape-stop (hash-table-keys soundscape-processes))
  (message "Stopped all soundscapes."))

(defsubst soundscape-running-p (name)
  "Predicate to check if soundscape is running."
  (declare (special soundscape-processes))
  (gethash  name soundscape-processes))

;;}}}
;;{{{ Modes->SoundScapes:

(defvar soundscape-mode-table (make-hash-table :test #'eq)
  "Maps mode-names to associated Soundscapes.")

(defsubst  soundscape-for-mode (mode)
  "Return associated soundscape for this mode if any."
  (declare (special soundscape-mode-table))
  (let ((result nil))
    (while mode 
    (pushnew (gethash mode soundscape-mode-table) result)
    (setq mode (get mode 'derived-mode-parent)))
    (delq nil result)))

(defsubst  soundscape-map-mode (mode scape)
  "Associate soundscape for this mode."
  (declare (special soundscape-mode-table))
  (puthash mode scape soundscape-mode-table))

;;; Add some mappings
(soundscape-map-mode 'prog-mode(soundscape-lookup "Cavern"))
(soundscape-map-mode 'eww-mode (soundscape-lookup "BackgroundWaves"))
(soundscape-map-mode 'text-mode (soundscape-lookup "Still"))
;;; Gnus, VM, Mail, Jabber (communication)
(loop
 for m in
 '(
   gnus-summary-mode gnus-article-mode gnus-group-mode
   vm-presentation-mode vm-mode mail-mode
   jabber-roster-mode jabber-chat-mode erc-mode)
 do
 (soundscape-map-mode m (soundscape-lookup"Drip" )))

;; help, man, references 
(loop
 for m in
 '(
   info-mode  help-mode  Man-mode
              Custom-mode messages-buffer-mode)
 do
 (soundscape-map-mode m (soundscape-lookup"Cavern" )))

;;}}}
;;{{{ Automatic soundscapes:

;;;###autoload 
(defcustom soundscape-auto nil
  "Turn on automatic soundscapes."
  :type 'boolean
  :group 'soundscape)
(defun soundscape-activate (mode)
  "Activate and deactivate Soundscapes for  this mode."
  (declare (special soundscape-auto))
  (let ((scapes (soundscape-for-mode mode)))
    (when scapes
        (loop
         for scape in scapes
         unless (gethash scape soundscape-processes)
         do (soundscape scape)))
    (unless (eq mode 'shell-mode)
      (loop
         for name  being the hash-keys of soundscape-processes
         unless (member (soundscape-lookup name)  scapes)
         do (soundscape-stop name)))))
   
(defadvice emacspeak-speak-mode-line (after soundscape pre act comp)
  "Switch soundscape if soundscape-auto is on."
  (and soundscape-auto (soundscape-activate major-mode)))

;;}}}
(provide 'soundscape)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
