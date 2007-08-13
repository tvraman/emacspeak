;;; emacspeak-ido.el --- speech-enable ido
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:   extension to speech enable ido
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-13 19:23:07 -0700 (Sun, 13 May 2007) $ |
;;;  $Revision: 4555 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2007, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{  Introduction:

;;; Commentary:

;;; speech-enable ido.el
;;; This is an interesting task since most of the value-add
;;; provided by package ido.el  is visual feedback.
;;; Speech UI Challenge: What  is the most efficient means of
;;; conveying a dynamically updating set of choices?
;;; current strategy is to walk the list using c-s and c-r as
;;; provided by ido
;;; Set number matches shown to 3 using Custom so you dont hear
;;; the entire list.

;;; Code:

;;}}}

;;{{{ required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ speech-enable feedback routines

(defvar emacspeak-ido-cache-current-directory nil
  "Cached value of ido-current-directory.")

(defadvice ido-set-current-directory (before emacspeak pre act
                                             comp)
  "Cache previous value of ido-current-directory."
  (setq emacspeak-ido-cache-current-directory
        ido-current-directory))

(defadvice ido-exhibit (after emacspeak pre act comp)
  "Speak first of the displayed matches."
  (when (and ido-matches
             (sit-for 0.5))
    (emacspeak-auditory-icon 'progress))
  (dtk-speak
   (concat 
    (car ido-matches)
    (format " %d choices: " (length ido-matches))
    (minibuffer-contents)
    (if(or (null ido-current-directory)
           (string-equal ido-current-directory emacspeak-ido-cache-current-directory))
        " "
      (format "In directory: %s"
              ido-current-directory)))))

;;}}}
;;{{{ speech-enable interactive commands:

(defadvice ido-mode (after emacspeak pre act comp)
  "Provide auditory feedback.
Tip: Use M-x customize to set ido-max-prospects to a small value
  when using Emacspeak --- I set it to 3.
The default value of 12 is too high for using ido effectively with speech. "
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-mode 'on 'off))
    (dtk-speak
     (format "IDo set to %s"
             ido-mode))))

(defadvice ido-everywhere (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-everywhere 'on 'off))
    (dtk-speak
     (format "Turned %s IDo everywhere."
             (if ido-everywhere " on " " off ")))))

 

(defadvice ido-toggle-case (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-case-fold 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if ido-case-fold 'on 'off)))))

(defadvice ido-toggle-regexp (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-enable-regexp 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if ido-enable-regexp 'on 'off)))))
(defadvice ido-toggle-prefix (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-enable-prefix 'on 'off))
    (dtk-speak
     (format "Prefix %s"
             (if ido-enable-prefix 'on 'off)))))

(defadvice ido-toggle-ignore (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if ido-process-ignore-lists 'on 'off))
    (dtk-speak
     (format "Case %s"
             (if ido-process-ignore-lists
                 'on 'off)))))

(defadvice ido-complete (after emacspeak pre act comp)
  "Speak completion at the head of the list."
  (when (interactive-p)
    (dtk-speak (car ido-matches))))

(loop for f in
      '(ido-find-file ido-find-file-other-frame
                      ido-find-file-other-window
                      ido-find-alternate-file
                      ido-find-file-read-only
                      ido-find-file-read-only-other-window ido-find-file-read-only-other-frame)
      do
      (eval
       (`
        (defadvice   (, f)(around emacspeak pre act comp)
          "Provide auditory feedback."
          (cond
           ((interactive-p)
            (let ((emacspeak-minibuffer-enter-auditory-icon nil))
              (emacspeak-auditory-icon 'open-object)
              ad-do-it
              (emacspeak-auditory-icon 'open-object)
              (emacspeak-speak-mode-line)))
           (t ad-do-it))
          ad-return-value))))

(loop for f in
      '(ido-switch-buffer ido-switch-buffer-other-window
                          ido-switch-buffer-other-frame
                          ido-display-buffer)
      do
      (eval
       (`
        (defadvice   (, f)(around emacspeak pre act comp)
          "Provide auditory feedback."
          (cond
           ((interactive-p)
            (let ((emacspeak-minibuffer-enter-auditory-icon nil))
              (emacspeak-auditory-icon 'open-object)
              ad-do-it
              (emacspeak-auditory-icon 'select-object)
              (emacspeak-speak-mode-line)))
           (t ad-do-it))
          ad-return-value))))

;;; note that though these are after advice fragments,
;;; ido-matches does not reflect the change at the time we
;;; get called.
;;; hence the off-by-one hack

(defadvice ido-next-match (after emacspeak pre act comp)
  "Speak match at the front of the list."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (second ido-matches))))

(defadvice ido-prev-match (after emacspeak pre act comp)
  "Speak match at the front of the list."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (car (last ido-matches)))))

(defadvice ido-kill-buffer-at-head (after emacspeak pre act comp)
  "Provide auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice ido-kill-buffer (after emacspeak pre act comp)
  "Provide auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ define personalities 

(defgroup emacspeak-ido nil
  "Emacspeak ido customizations."
  :group  'emacspeak
  )
(voice-setup-add-map
 '(
   (ido-first-match voice-brighten-extra)
   (ido-only-match voice-bolden)
   (ido-subdir voice-lighten-extra)
   (ido-indicator voice-smoothen)
   (ido-incomplete-regexp voice-monotone)
   ))

;;}}}
;;{{{ Additional keybindings 

(defun emacspeak-ido-keys ()
  "Setup additional  keybindings within ido."
  (declare (special ido-completion-map))
  (define-key ido-completion-map "\C-f" 'ido-enter-find-file)
  (define-key ido-completion-map "^" 'ido-up-directory))

;;}}}
(provide 'emacspeak-ido)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
