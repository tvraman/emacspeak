;;; emacspeak-ido.el --- speech-enable ido
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable ido
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2003, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ speech-enable feedback routines

(defadvice ido-exhibit (after emacspeak pre act comp)
  "Speak first of the displayed matches."
  (dtk-speak
   (format
    "%s %d Choices: %s"
    (car ido-matches)
    (length ido-matches)
    (or ido-text ""))))

;;}}}
;;{{{ speech-enable interactive commands:

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
(defadvice  ido-find-file (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice  ido-switch-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

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
    (emacspeak-auditory-icon 'close-object)))

;;}}}
(provide 'emacspeak-ido)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
