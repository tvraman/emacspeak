;;; emacspeak-transient.el --- Speech-enable TRANSIENT  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable TRANSIENT An Emacs Interface to transient
;;; Keywords: Emacspeak,  Audio Desktop transient
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
;;; MERCHANTABILITY or FITNTRANSIENT FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; TRANSIENT ==  Transient commands --- used by magit and friends.
;;; This module speech-enables transient.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(defadvice transient-toggle-common (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cl-declare (special transient-show-common-commands))
  (when (ems-interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon
     (if transient-show-common-commands 'on 'off))))

(defadvice transient-resume (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'open-object)))

(cl-loop
 for f in
 '(transient-quit-all transient-quit-one transient-quit-seq transient-suspend)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (dtk-stop)
       (emacspeak-auditory-icon 'close-object)))))

(cl-loop
 for f in
 '(transient-save transient-set)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'save-object)
       (dtk-stop)))))

(cl-loop
 for f in
 '(transient-history-next
   transient-history-prev)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (dtk-stop)
       (emacspeak-auditory-icon 'select-object)))))

;;}}}
(provide 'emacspeak-transient)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
