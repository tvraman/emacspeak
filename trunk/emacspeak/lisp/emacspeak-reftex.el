;;; emacspeak-reftex.el --- speech enable reftex
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable
;;; reftex 
;;; Keywords: Emacspeak, reftex
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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'thingatpt)
(require 'voice-lock)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables refteex --
;;; reftex is a minor mode that makes navigation of TeX
;;; documents  possible via a table of contents buffer.

;;; Code:

;;}}}
;;{{{ advice interactive commands

(defadvice reftex-select-previous-heading (after emacspeak pre act
                                      comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-select-next-heading (after emacspeak pre act
                                  comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-toc-previous (after emacspeak pre act
                                      comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-toc-next (after emacspeak pre act
                                  comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-toc-goto-line (after emacspeak pre act comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (recenter 0)
    (cond
     (outline-minor-mode
      (emacspeak-outline-speak-this-heading))
     (t
      (emacspeak-speak-predefined-window 1)))))
      

(defadvice reftex-toc-goto-line-and-hide (after emacspeak pre act comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if outline-minor-mode
        (emacspeak-outline-speak-this-heading)
      (emacspeak-speak-line))))

(defadvice reftex-toc-view-line (after emacspeak pre act comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
      (other-window 1)
    (recenter 0)
    (other-window 1)
    (emacspeak-speak-predefined-window 2)))

(defadvice reftex-select-previous (after emacspeak pre act comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-select-next (after emacspeak pre act
                                  comp)
  "Speech enable  by speaking toc entry."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice reftex-select-accept (after emacspeak pre act
                                       comp)
  "Speak line where we inserted the reference."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice reftex-toc-toggle-follow (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if reftex-toc-follow-mode
         'on
       'off))
    (message "Turned %s follow mode. "
             (if reftex-toc-follow-mode 'on 'off))))
(defadvice reftex-toc-toggle-labels (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if reftex-toc-include-labels
         'on
       'off))
    (message "Turned %s labels. "
             (if reftex-toc-include-labels 'on 'off))))

(defadvice reftex-toc-toggle-file-boundary (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if reftex-toc-include-file-boundaries
         'on
       'off))
    (message "Turned %s file boundary markers. "
             (if reftex-toc-include-file-boundaries 'on 'off))))


(defadvice reftex-toc-toggle-context (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if reftex-toc-include-context
         'on
       'off))
    (message "Turned %s context markers. "
             (if reftex-toc-include-context 'on 'off))))



;;}}}
(provide 'emacspeak-reftex)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
