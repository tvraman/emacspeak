;;; emacspeak-winring.el --- Speech enable WinRing -- Manage multiple Emacs window configurations
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to winring
;;; Keywords: Emacspeak, Speak, Spoken Output, winring
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

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction

;;; window configurations in emacs are very useful 
;;; you can display the same file in different windows,
;;;and have diffrent portions of the file displayed.
;;; winring allows you to manage window configurations,
;;; and this module speech-enables it.

;;}}}
;;{{{ Advice commands

(defadvice winring-jump-to-configuration(after emacspeak pre
                                               act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-tapestry-describe-tapestry winring-name)))

(defadvice winring-next-configuration(after emacspeak pre
                                            act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-tapestry-describe-tapestry winring-name)))

(defadvice winring-prev-configuration(after emacspeak pre
                                            act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-tapestry-describe-tapestry winring-name)))

(defadvice winring-new-configuration(after emacspeak pre
                                           act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))
(defadvice winring-delete-configuration(after emacspeak pre
                                              act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-mode-line)))

;;}}}
(provide 'emacspeak-winring)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
