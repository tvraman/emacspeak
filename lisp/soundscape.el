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
;;; Main Entry Points:
;;;@itemize
;;; @item M-x soundscape --- runs a named SoundScape
;;; @item M-x soundscape-toggle --- Enables or disables automatic SoundScapes.
;;; @end itemize
;;; When automatic Soundscapes are enabled, SoundScapes are automatically started and stopped based on the current major mode.
;;; Thus, SoundScapes can be thought of as reflecting the @emph{mood} of the current @emph{mode}.

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

;;; This file is generated via a shell-hack for now.

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
   ((null (file-exists-p soundscape-list)) (error "Catalog not initialized."))
   (t
    (let ((name nil)
          (scape nil))
      (with-temp-buffer
        (insert-file-contents soundscape-list)
        (goto-char (point-min))
        (while (not (eobp))
          (setq scape
                (buffer-substring (line-beginning-position) (line-end-position)))
          (setq name (second (split-string scape "/")))
          (when (and name scape)
            (push (cons name scape) soundscape--catalog))
          (forward-line 1))))
    soundscape--catalog)))

(defsubst soundscape-lookup-name (name)
  "Return package/agent for this name."
  (cdr (assoc name (soundscape-catalog))))

(defsubst soundscape-lookup-scape (scape)
  "Return name for this package/agent."
  (car (rassoc scape (soundscape-catalog))))

(defun soundscape-read ()
  "Read name of Soundscape with completion."
  (let ((completion-ignore-case t))
    (soundscape-lookup-name
     (completing-read "Soundscape: " (soundscape-catalog)))))

;;}}}
;;{{{ Running:

(defvar soundscape-processes (make-hash-table :test #'equal)
  "Hash table of running Soundscapes indexed by Soundscape path.")

;;;###autoload
(defun soundscape (scape)
  "Play soundscape."
  (interactive (list (soundscape-read)))
  (declare (special soundscape-processes))
  (let ((proc (gethash scape soundscape-processes)))
    (unless (process-live-p proc)
      (message "Starting %s" scape)
      (setq proc
            (start-process "Boodler" nil soundscape-player "-o" "alsa" scape))
      (when (process-live-p proc) (puthash scape proc soundscape-processes))
      (message "Soundscape %s is running. " scape))))

(defun soundscape-stop (scape)
  "Stop running Soundscape."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Stop: " (hash-table-keys soundscape-processes)))))
  (declare (special soundscape-processes))
  (let ((proc (gethash scape soundscape-processes)))
    (when (process-live-p proc)
      (delete-process proc)
      (remhash  scape soundscape-processes)
      (message "Stopped soundscape %s" scape))))

(defun soundscape-kill ()
  "Stop all running soundscapes."
  (interactive)
  (declare (special soundscape-processes))
  (mapcar  #'soundscape-stop (hash-table-keys soundscape-processes))
  (message "Stopped all soundscapes."))

(defsubst soundscape-running-p (scape)
  "Predicate to check if soundscape is running."
  (declare (special soundscape-processes))
  (process-live-p (gethash  scape soundscape-processes)))

(defun soundscape-display ()
  "Display names of running scapes."
  (interactive)
  (message "%s"
           (mapconcat
            #'soundscape-lookup-scape
            (hash-table-keys soundscape-processes)
           " ")))
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
(soundscape-map-mode 'special-mode (soundscape-lookup-name "BlopSpace"))
(soundscape-map-mode 'prog-mode(soundscape-lookup-name "Cavern"))
(soundscape-map-mode 'eww-mode (soundscape-lookup-name "BackgroundWaves"))
(soundscape-map-mode 'text-mode (soundscape-lookup-name "Still"))
;;; Gnus, VM, Mail, Jabber (communication)
(loop
 for m in
 '(
   message-mode gnus-summary-mode gnus-article-mode gnus-group-mode
                vm-presentation-mode vm-mode mail-mode
                jabber-roster-mode jabber-chat-mode erc-mode)
 do
 (soundscape-map-mode m (soundscape-lookup-name"Drip" )))

;; help, man, references
(loop
 for m in
 '(
   Info-mode  help-mode  Man-mode
              Custom-mode messages-buffer-mode)
 do
 (soundscape-map-mode m (soundscape-lookup-name "RainForever" )))

;;}}}
;;{{{ Automatic soundscapes:

;;;###autoload
(defvar soundscape-auto nil
  "Turn on automatic soundscapes.
Do not set this by hand, use command \\[soundscape-toggle].")



(defun soundscape-activate (mode)
  "Activate and deactivate Soundscapes for  this mode."
  (let ((scapes (soundscape-for-mode mode)))
    (when scapes
      (loop
       for scape in scapes
       unless (gethash scape soundscape-processes)
       do (soundscape scape))
      (loop
       for scape  being the hash-keys of soundscape-processes
       unless (member scape  scapes)
       do (soundscape-stop scape)))))

(defadvice emacspeak-speak-mode-line (after soundscape pre act comp)
  "Switch soundscape if soundscape-auto is on."
  (and soundscape-auto (soundscape-activate major-mode)))

;;}}}
;;{{{ SoundScape Toggle:

;;;###autoload
(defun soundscape-toggle ()
  "Toggle automatic SoundScapes.
When turned on, Soundscapes are automatically run based on current major mode."
  (interactive)
  (declare (special soundscape-auto))
  (cond
   (soundscape-auto
    (setq soundscape-auto nil)
    (soundscape-kill))
   (t (setq soundscape-auto t)))
  (message "Automatic Soundscapes are now %s"
           (if soundscape-auto "on" "off")))

;;}}}
(provide 'soundscape)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
