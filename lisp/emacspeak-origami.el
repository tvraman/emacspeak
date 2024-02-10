;;; emacspeak-origami.el --- Speech-enable ORIGAMI  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable ORIGAMI An Emacs Interface to origami
;; Keywords: Emacspeak,  Audio Desktop origami
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

;;;   Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNORIGAMI FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; ORIGAMI ==  One More Flexible Folding Mechanism
;; This module speech-enables origami-mode.
;;; Code:

;;;   Required modules
(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Map Faces:

(voice-setup-add-map
 '(
   (origami-fold-fringe-face voice-monotone-extra)
   (origami-fold-header-face voice-bolden-medium)
   (origami-fold-replacement-face voice-smoothen)))

;;;  Advice low-level internals: hide/show overlay
(defadvice origami-hide-overlay (after emacspeak pre act comp)
  "Attach auditory icon at front."
  (let ((s
         (save-excursion
           (goto-char (overlay-start (ad-get-arg 0)))
           (line-beginning-position)))
        (e (overlay-end (ad-get-arg 0))))
    (put-text-property s e 'auditory-icon 'ellipses)))

(defadvice origami-show-overlay (after emacspeak pre act comp)
  "Remove auditory icon at front."
  (let ((s
         (save-excursion
           (goto-char (overlay-start (ad-get-arg 0)))
           (line-beginning-position)))
        (e (overlay-end (ad-get-arg 0))))
    (put-text-property s e 'auditory-icon nil)))

;;;  Interactive Commands:
(defvar origami-mode)

(defadvice origami-mode (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon (if origami-mode 'on 'off))
    (message "Turned %s origami mode." (if origami-mode 'on 'off))))

(cl-loop
 for f in
 '(
   origami-previous-fold origami-next-fold
   origami-forward-fold-same-level origami-backward-fold-same-level
   origami-forward-fold)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'large-movement)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(origami-close-node-recursively origami-close-node origami-close-all-nodes)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'close-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(
   origami-show-only-node  origami-show-node
   origami-open-node-recursively origami-open-node origami-open-all-nodes)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)
       (emacspeak-speak-line)))))

(defun emacspeak-origami-invisible-p ()
  "Check if point  is on  a closed or open node."
  (condition-case nil
      (overlay-get
       (overlay-get (cl-first (overlays-at (point))) 'fold-overlay) 'invisible)
    (error nil)))

(cl-loop
 for f in
 '(
   origami-forward-toggle-node origami-recursively-toggle-node
   origami-toggle-all-nodes origami-toggle-node)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (let ((flag (if  (emacspeak-origami-invisible-p) 'on 'off)))
         (emacspeak-icon flag)
         (message "%s nodes." (if flag "Expanded " "Collapsed "))
         (emacspeak-speak-line))))))

(provide 'emacspeak-origami)
;;;  end of file

