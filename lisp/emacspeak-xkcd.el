;;; emacspeak-xkcd.el --- Speech-enable XKCD
;;; $Id: emacspeak-xkcd.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable XKCD An Emacs Interface to xkcd
;;; Keywords: Emacspeak,  Audio Desktop xkcd
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
;;; MERCHANTABILITY or FITNXKCD FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; XKCD ==  emacs-xkcd
;;; View XKCD comics in Emacs.
;;; Speech enables package emacs-xkcd
;;; Augments it by displaying the alt text and the transcript.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;; Eventually move this to the emacs-xkcd package if possible.

(defvar xkcd-transcript nil
  "Cache current transcript.")
;;; Cache transcript.
;;; Content downloaded by the time this is called.
(defsubst emacspeak-xkcd-get-current-transcript ()
  "Cache current transcript."
  (setq 
   xkcd-transcript 
   (cdr 
    (assoc 'transcript (json-read-from-string (xkcd-get-json "" xkcd-cur))))))
(defadvice xkcd-get (after emacspeak first pre act comp)
  "Insert cached transcript in xkcd-transcript."
  (let ((inhibit-read-only t))
    (emacspeak-xkcd-get-current-transcript)
    (goto-char (point-max))
    (insert xkcd-alt)
    (insert "\n")
    (insert 
     (format "Transcript: %s" 
             (if (zerop (length xkcd-transcript))
                 "Not available yet."
               xkcd-transcript)))
    (goto-char (point-min))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))

(provide 'emacspeak-xkcd)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
