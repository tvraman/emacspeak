;;; emacspeak-mct.el --- Speech-enable EMACSPEAK-MCT  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EMACSPEAK-MCT An Emacs Interface to emacspeak-mct
;;; Keywords: Emacspeak,  Audio Desktop emacspeak-mct
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;; EMACSPEAK-MCT ==  Speech-Enable mct Integrated Completions

;;; Code:

;;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'mct "mct" 'no-error))

(voice-setup-add-map 
'(
(mct-highlight-candidate voice-bolden)))

;;;  Interactive Commands:

'(
  
  mct-choose-completion-dwim
  mct-choose-completion-exit
  mct-choose-completion-no-exit
  mct-complete-and-exit
  mct-edit-completion
  mct-keyboard-quit-dwim
  mct-list-completions-toggle
  mct-minibuffer-mode
  mct-mode
  mct-next-completion-group
  mct-next-completion-or-mini
  mct-previous-completion-group
  mct-previous-completion-or-mini
  mct-switch-to-completions-top
  )


(cl-loop
 for f in 
 '(mct-switch-to-completions-bottom mct-beginning-of-buffer)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(provide 'emacspeak-emacspeak-mct)
;;;  end of file





