;;; emacspeak-custom.el --- Speech enable interactive Emacs customization  -*- lexical-binding: t; -*- 
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Auditory interface to custom
;;; Keywords: Emacspeak, Speak, Spoken Output, custom
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2018, T. V. Raman
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

;;{{{  Required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'cus-edit)
;;}}}
;;{{{  Introduction
;;; Commentary:
;;;Advise custom to speak.
;;; most of the work is actually done by emacspeak-widget.el
;;; which speech-enables the widget libraries.
;;; Code:
;;}}}

;;{{{ advice

(defadvice Custom-reset-current (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'item)
    (dtk-speak "Reset current")))

(defadvice Custom-reset-saved(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'unmodified-object)
    (dtk-speak "Reset to saved")))

(defadvice Custom-reset-standard (after emacspeak act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (dtk-speak "Erase customization")))

(defadvice Custom-set (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (dtk-speak "Set for current session")))

(defadvice Custom-save (around emacspeak pre act comp)
  "Silence messages and produce auditory feedback."
  ad-do-it
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (dtk-speak "Set and saved"))
  ad-return-value)

(defadvice Custom-buffer-done (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice customize-save-customized (after emacspeak pre act comp)
  "Provide auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (message "Saved customizations.")))

(defadvice custom-save-all (after emacspeak pre act comp)
  "Provide auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'save-object)
    (message "Saved customizations.")))

(defadvice customize-save-customized (around quiet pre act comp)
  "Silence speech."
  (let ((dtk-quiet t)) ad-do-it))

(defadvice custom-set (after emacspeak pre act comp)
  "Provide auditory feedback. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Set all updates.")))

(defadvice customize (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-custom-goto-group)
    (emacspeak-speak-line)))

(defadvice customize-group (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-custom-goto-group)
    (emacspeak-speak-line)))

(defadvice customize-browse (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice customize-option (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (let ((symbol (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (search-forward (custom-unlispify-tag-name symbol))
      (forward-line 0)
      (emacspeak-speak-line))))

(defadvice customize-apropos (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (forward-line 0)
    (emacspeak-speak-line)))

(defadvice customize-variable (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (let ((symbol (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (search-forward (custom-unlispify-tag-name symbol))
      (forward-line 0)
      (emacspeak-speak-line))))

(defadvice Custom-goto-parent (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice Custom-newline (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ custom hook

(add-hook 'Custom-mode-hook
          (function
           (lambda nil
             (emacspeak-pronounce-refresh-pronunciations))))

;;}}}
;;{{{ define voices
(voice-setup-add-map
 '(
   (custom-variable-obsolete voice-smoothen-extra)
   (custom-group-subtitle  voice-smoothen)
   (custom-themed voice-brighten)
   (custom-visibility voice-annotate)
   (custom-button voice-bolden)
   (custom-button-pressed voice-bolden-extra)
   (custom-button-pressed-unraised voice-bolden-extra)
   (custom-button-mouse voice-bolden-medium)
   (custom-button-unraised voice-smoothen)
   (custom-changed voice-smoothen)
   (custom-comment voice-monotone-medium)
   (custom-comment-tag voice-monotone)
   (custom-documentation voice-brighten-medium)
   (custom-face-tag voice-lighten)
   (custom-group-tag voice-bolden)
   (custom-group-tag-1 voice-lighten-medium)
   (custom-invalid voice-animate-extra)
   (custom-link voice-bolden)
   (custom-modified voice-lighten-medium)
   (custom-rogue voice-bolden-and-animate)
   (custom-modified voice-lighten-medium)
   (custom-saved voice-smoothen-extra)
   (custom-set voice-smoothen-medium)
   (custom-state voice-smoothen)
   (custom-variable-button voice-animate)
   (custom-variable-tag voice-bolden-medium)))

;;}}}
;;{{{  custom navigation

(defvar emacspeak-custom-group-regexp
  "^/-"
  "Pattern identifying start of custom group.")


(defun emacspeak-custom-goto-group ()
  "Jump to custom group when in a customization buffer."
  (interactive)
  (cl-declare (special emacspeak-custom-group-regexp))
  (when (eq major-mode 'custom-mode)
    (goto-char (point-min))
    (re-search-forward emacspeak-custom-group-regexp
                       nil t)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defvar emacspeak-custom-toolbar-regexp
  "^Operate on everything in this buffer:"
  "Pattern that identifies toolbar section.")

(defun emacspeak-custom-goto-toolbar ()
  "Jump to custom toolbar when in a customization buffer."
  (interactive)
  (cl-declare (special emacspeak-custom-toolbar-regexp))
  (when (eq major-mode 'custom-mode)
    (goto-char (point-min))
    (re-search-forward emacspeak-custom-toolbar-regexp nil
                       t)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;}}}
;;{{{  bind emacspeak commands 

(cl-declaim (special custom-mode-map))
(define-key custom-mode-map "E" 'Custom-reset-standard)
(define-key custom-mode-map "r" 'Custom-reset-current)
(define-key custom-mode-map "R" 'Custom-reset-saved)
(define-key custom-mode-map "s" 'Custom-set)
(define-key  custom-mode-map "S" 'Custom-save)

(define-key custom-mode-map "," 'backward-paragraph)
(define-key custom-mode-map "." 'forward-paragraph)
(define-key custom-mode-map  "\M-t" 'emacspeak-custom-goto-toolbar)
(define-key custom-mode-map  "\M-g"
  'emacspeak-custom-goto-group)

;;}}}
;;{{{ augment custom widgets

;;}}}
(provide 'emacspeak-custom)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
