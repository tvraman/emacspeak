;;; emacspeak-gtags.el --- Speech-enable GTAGS
;;; $Id: emacspeak-gtags.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable GTAGS An Emacs Interface to gtags
;;; Keywords: Emacspeak,  Audio Desktop gtags
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNGTAGS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; GTAGS ==  Emacs support for GNU global.
;;; GNU  global implements  a modern tags solution
;;; Package gtags interfaces Emacs to this tool.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice interactive functions:

;;; Jumpers: Move to tags by various means
(loop for f in
      '(
        gtags-find-with-grep
        gtags-find-with-idutils
        gtags-make-complete-list
        gtags-select-tag
        gtags-select-mode
        gtags-select-tag-by-event
        gtags-find-symbol
        gtags-find-file
        gtags-find-pattern
        gtags-find-tag
        gtags-display-browser
        gtags-find-tag-by-event
        gtags-find-rtag
        gtags-find-tag-from-here
        )
        do
        (eval
         `(defadvice ,f (after emacspeak pre act comp)
            "Provide auditory feedback."
            (when (ems-interactive-p)
              (emacspeak-auditory-icon 'large-movement)
              (emacspeak-speak-line)))))

(defadvice gtags-pop-stack (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gtags-select-mode (after emacspeak pre act comp)
  "Provide  auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-gtags)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
