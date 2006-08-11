;;; emacspeak-sql.el --- Speech enable sql-mode
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable sql-mode
;;; Keywords: Emacspeak, database interaction
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables sql-mode--
;;; available from
;;; http://paddington.ic.uva.nl/public/sql-modes.zip
;;; sql-mode.el implemented by the above package
;;;sets up an Emacs to SQL interface where you can
;;;interactively evaluate SQL expressions.
;;; Code:

;;}}}
;;{{{ advice

(defadvice sqlplus-execute-command (after emacspeak pre act comp)
  "Provide auditory feedback and place point at the start of the output."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (sqlplus-back-command 2)
    (forward-line 1)
    (emacspeak-speak-line)))

(defadvice sqlplus-back-command (after emacspeak pre act
                                       comp)"Move prompt appropriately,  and speak the line."
                                       (when (interactive-p)
                                         (emacspeak-auditory-icon 'large-movement)
                                         (forward-line 1)
                                         (emacspeak-speak-line)))

(defadvice sqlplus-forward-command (after emacspeak pre act
                                          comp)
  "Move prompt appropriately,  and speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (forward-line 1)
    (emacspeak-speak-line)))

(defadvice sqlplus-next-command (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice sqlplus-previous-command (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice sql-send-region (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    ad-do-it
    (emacspeak-auditory-icon 'mark-object))
   (t ad-do-it))
  ad-return-value)

(defadvice sql-send-buffer (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    ad-do-it
    (emacspeak-auditory-icon 'mark-object))
   (t ad-do-it))
  ad-return-value)

;;}}}
(provide 'emacspeak-sql)

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
