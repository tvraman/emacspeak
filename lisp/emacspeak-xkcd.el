;;; emacspeak-xkcd.el --- Speech-enable XKCD  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-xkcd.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable XKCD An Emacs Interface to xkcd
;;; Keywords: Emacspeak,  Audio Desktop xkcd
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
;;; XKCD ==  XKCD In Emacs
;;; View XKCD comics in Emacs.
;;; Speech enables package xkcd
;;; Augments it by displaying the alt text and the transcript.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'json)
(require 'xkcd "xkcd" 'no-error)
;;}}}
;;{{{ Fix error when loading images on the console:

(defadvice xkcd-insert-image (around emacspeak pre act comp)
  "no-Op on console"
  (cond
   ((not window-system) t)
   (t ad-do-it)))

;;}}}

(defadvice xkcd-kill-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defvar xkcd-transcript nil
  "Cache current transcript.")
;;; Cache transcript.
;;; Content downloaded by the time this is called.
(defun emacspeak-xkcd-get-current-transcript ()
  "Cache current transcript."
  (cl-declare (special xkcd-cur))
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
;;{{{ Advice browse-url-default-browser:

(defadvice browse-url-default-browser (around emacspeak pre act comp)
  "Use Emacs browser --- rather than an external browser."
  (when nil ad-do-it) ; to silence byte-compiler 
  (eww-browse-url (ad-get-arg 0)))

;;}}}
(defun emacspeak-xkcd-open-explanation-browser ()
  "Open explanation of current xkcd in default browser"
  (interactive)
  (cl-declare (special xkcd-cur))
  (browse-url (concat "http://www.explainxkcd.com/wiki/index.php/"
                      (number-to-string xkcd-cur))))
(when (boundp 'xkcd-mode-map)
  (define-key xkcd-mode-map "e" 'emacspeak-xkcd-open-explanation-browser))
(provide 'emacspeak-xkcd)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
