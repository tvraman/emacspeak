;;; emacspeak-flycheck.el --- Speech-enable FLYCHECK  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-flycheck.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable FLYCHECK An Emacs Interface to flycheck
;;; Keywords: Emacspeak,  Audio Desktop flycheck
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNFLYCHECK FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; FLYCHECK == On-the-fly checking.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map faces

(voice-setup-add-map
 '(
   (flycheck-warning voice-animate)
   (flycheck-error voice-bolden)
   (flycheck-info voice-monotone-extra)
   (flycheck-error-list-highlight-at-point voice-bolden-extra)
   (flycheck-error-list-highlight voice-bolden-medium)
   (flycheck-error-list-highlightflycheck-error-list-column-number voice-lighten)
   (flycheck-error-list-line-number voice-lighten)
   (flycheck-error-list-info voice-monotone-extra)
   (flycheck-error-list-warning voice-animate)
   (flycheck-error-list-error voice-bolden)))

;;}}}
;;{{{ Advice interactive commands.

(cl-loop
 for  f in
 '(flycheck-next-error flycheck-previous-error flycheck-first-error)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice flycheck-list-errors (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Displayed error listing in other window.")))

(defadvice flycheck-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Checking buffer.")))

(defadvice flycheck-clear (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Cleared errors")))

(defadvice flycheck-compile (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Compiling buffer")))

(defadvice flycheck-error-list-refresh (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Refreshed errors")))

;;}}}
(provide 'emacspeak-flycheck)
;;; emacspeak-flycheck ends here
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
