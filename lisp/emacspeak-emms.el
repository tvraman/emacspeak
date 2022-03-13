;;; emacspeak-emms.el --- Speech-enable EMMS Multimedia UI  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech-enable EMMS
;;; Keywords: Emacspeak, Multimedia
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-27 17:54:54 -0700 (Mon, 27 Aug 2007) $ |
;;;  $Revision: 4150 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction

;;; Commentary:
;;;Speech-enables EMMS --- the Emacs equivalent of XMMS
;;; available from  the Emacs package archive.
;;; http://savannah.gnu.org/project/emms
;;; EMMS is under active development,
;;; to get the current CVS version, use Emacspeak command
;;; M-x emacspeak-cvs-gnu-get-project-snapshot RET emms RET
;;;
;;; Code:

;;}}}
;;{{{ required modules
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(declare-function emms-playlist-current-selected-track "emacspeak-emms" t)
(declare-function emms-player-pause "emacspeak-emms" t)

;;}}}
;;{{{ module emms:

(defun emacspeak-emms-speak-current-track ()
  "Speak current track."
  (interactive)
  (message
   (cdr (assq 'name (emms-playlist-current-selected-track)))))

(cl-loop for f in
         '(emms-next emms-next-noerror emms-previous)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Speak track name."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'select-object)))))

;;; these commands should not be made to talk since that would  interferes
;;; with real work.
(cl-loop for f in
         '(emms-start emms-stop emms-sort
                      emms-shuffle emms-random)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "Provide auditory icon."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'select-object)))))

(cl-loop for f in
         '(emms-playlist-first emms-playlist-last
                               emms-playlist-mode-first emms-playlist-mode-last)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'large-movement)
               (emacspeak-speak-line)))))
(cl-loop for f in
         '(emms-browser emms-browser-next-filter
                        emms-browser-previous-filter)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-mode-line)
               (emacspeak-auditory-icon 'open-object)))))

(defadvice emms-browser-bury-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ Module emms-streaming:
(cl-declaim (special emms-stream-mode-map))
(defadvice emms-stream-mode (after emacspeak pre act comp)
  "Update keymaps."
  (define-key emms-stream-mode-map "\C-e"
    'emacspeak-prefix-command))

(defadvice emms-stream-delete-bookmark (after emacspeak pre act
                                              comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice emms-stream-save-bookmarks-file (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (message "Saved stream bookmarks.")))

(cl-loop for f in
         '(emms-streams emms-stream-quit
                        emms-stream-popup emms-stream-popup-revert
                        emms-playlist-mode-go
                        )
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-mode-line)))))

(cl-loop for f in
         '(emms-stream-next-line emms-stream-previous-line)
         do
         (eval
          `(defadvice ,f (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-line)))))
(defadvice emms-playlist-mode-bury-buffer (after emacspeak pre act comp)
  "Announce the buffer that becomes current."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ silence chatter from info

(defadvice emms-info-really-initialize-track (around emacspeak
                                                     pre act
                                                     comp)
  "Silence messages."
  (ems-with-messages-silenced
   ad-do-it))

;;}}}
;;{{{ pause/resume if needed


(defun emacspeak-emms-pause-or-resume ()
  "Pause/resume if emms is running. For use  in
emacspeak-silence-hook."
  (cl-declare (special emms-player-playing-p))
  (when (and (boundp 'emms-player-playing-p)
             (not (null emms-player-playing-p)))
    (emms-player-pause)))

(add-hook 'emacspeak-silence-hook 'emacspeak-emms-pause-or-resume)

;;}}}
(provide 'emacspeak-emms)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
