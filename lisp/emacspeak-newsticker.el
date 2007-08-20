;;; emacspeak-newsticker.el --- Speech-enable newsticker
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak front-end for NEWSTICKER 
;;; Keywords: Emacspeak, newsticker 
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2006-08-19 10:48:45 -0700 (Sat, 19 Aug 2006) $ |
;;;  $Revision: 4074 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;; Commentary:
;;{{{  Introduction:

;;; Newsticker provides a continuously updating newsticker using
;;; RSS
;;; Provides functionality similar to amphetadesk --but in pure elisp

;;}}}
;;{{{ required modules

;;; Code:
(require 'emacspeak-preamble)
(require 'backquote)
;;}}}
;;{{{ define personalities 
(voice-setup-add-map
 '(
   (newsticker-new-item-face voice-brighten)
   (newsticker-old-item-face voice-monotone)
   (newsticker-feed-face voice-animate)
   ))
;;}}}
;;{{{ advice functions

(defadvice newsticker--cache-remove (around emacspeak pre act
                                            comp)
  "Silence messages temporarily to avoid chatter."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

(defadvice newsticker-callback-enter (around emacspeak pre act
                                             comp)
  "Silence messages temporarily to avoid chatter."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))
(defadvice newsticker-retrieval-tick (around emacspeak pre act comp)
  "Silence messages temporarily to avoid chatter."
  (let ((emacspeak-speak-messages nil))
    ad-do-it
    ad-return-value))

;;}}}
;;{{{ advice interactive commands

(defun emacspeak-newsticker-summarize-item ()
  "Summarize current item."
  (emacspeak-speak-line))

(loop for f in
      '(newsticker-next-item newsticker-previous-item
                             newsticker-next-new-item
      newsticker-previous-new-item
      newsticker-previous-feed newsticker-next-feed
                             )
      do
      (eval
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-newsticker-summarize-item))))))

;;}}}
;;{{{  silence auto activity

(loop for f in
      '(newsticker-get-news-with-delay
        newsticker-get-news
        newsticker--cache-save)
      do
      (eval
       `(defadvice  ,f (around emacspeak pre act comp)
          "Silence messages."
          (let ((emacspeak-speak-messages nil))
            ad-do-it))))
;;}}}
(provide 'emacspeak-newsticker)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
