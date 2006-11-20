;;; emacspeak-message.el --- Speech enable Message -- Used to compose news postings and replies
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extensions for posting
;;; Keywords:emacspeak, audio interface to emacs posting messages
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
;;;Copyright (C) 1995 -- 2006, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman  
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

;;{{{  Introduction

;;; advice for posting message commands

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{ voice mapping

(voice-setup-add-map
 '(
   (message-cited-text-face voice-bolden)  ;; pre-emacs22
   (message-header-cc-face voice-bolden)  ;; pre-emacs22
   (message-header-name-face voice-animate)  ;; pre-emacs22
   (message-header-newsgroups-face voice-bolden)  ;; pre-emacs22
   (message-header-other-face voice-bolden)  ;; pre-emacs22
   (message-header-subject-face voice-bolden)  ;; pre-emacs22
   (message-header-to-face voice-bolden)  ;; pre-emacs22
   (message-header-xheader-face voice-bolden)  ;; pre-emacs22
   (message-mml-face voice-brighten)  ;; pre-emacs22
   (message-separator-face voice-bolden-extra)  ;; pre-emacs22

   (message-cited-text voice-bolden)
   (message-header-cc voice-bolden)
   (message-header-name voice-animate)
   (message-header-newsgroups voice-bolden)
   (message-header-other voice-bolden)
   (message-header-subject voice-bolden)
   (message-header-to voice-bolden)
   (message-header-xheader voice-bolden)
   (message-mml voice-brighten)
   (message-separator voice-bolden-extra)
   ))

;;}}}
;;{{{  advice interactive commands

(defadvice message-goto-to (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-summary (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-subject (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-cc (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-bcc (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-fcc (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-keywords (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-newsgroups (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-followup-to (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-reply-to (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-body (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (message "Beginning of message body")))

(defadvice message-goto-signature (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-distribution (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-insert-citation-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-insert-to (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-insert-signature (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Signed the article.")))

(defadvice message-insert-newsgroups (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-insert-courtesy-copy (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-beginning-of-line (before emacspeak pre act)
  "Stop speech first."
  (when (interactive-p) (dtk-stop )
        (emacspeak-auditory-icon 'select-object)
	(dtk-speak "beginning of line")))

(defadvice message-goto-from (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-goto-mail-followup-to (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice message-newline-and-reformat (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "newline and reformat")))




(add-hook 'message-mode-hook
          (lambda ()
            (emacspeak-auditory-icon 'open-object)
            (message "Starting message %s ... done"
                     (buffer-name))))

;;}}}
(provide  'emacspeak-message)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
