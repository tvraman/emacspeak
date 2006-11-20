;;; emacspeak-semi.el --- Speech enable semi -- Fluent spoken access to MIME User Interface on emacsen
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable semi
;;; Keywords: Emacspeak, semi, mime, mail, News, Advice, Spoken Output
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 1.1 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)

(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Advise top-level semi command
;;; for mime-view
(defadvice mime-preview-move-to-previous (after emacspeak pre act)
  "speak the previous line for mime-preview"
  (emacspeak-speak-line))

(defadvice mime-preview-move-to-next (after emacspeak pre act)
  "speak the next line of mime-preview"
  (emacspeak-speak-line))

(defadvice mime-preview-scroll-up-entity (after emacspeak pre act)
  "speak the scrool up mime entity"
  (emacspeak-speak-line))

(defadvice mime-preview-scroll-down-entity (after emacspeak pre act)
  "speak the scrool down mime entity"
  (emacspeak-speak-line))

(defadvice mime-preview-next-line-entity (after emacspeak pre act)
  "speak the next-line-entity for mime-view"
  (emacspeak-speak-line))

(defadvice mime-preview-next-line-entity (after emacspeak pre act)
  "speak the previous-line-entity for mime-view"
  (emacspeak-speak-line))

(defadvice mime-preview-quit (after emacspeak pre act)
  "announce the mime-view quit"
  (emacspeak-speak-mode-line))
(defadvice mime-edit-insert-message (after emacspeak pre act)
  "announce the message insert draft"
  (emacspeak-speak-line))
(defadvice mime-edit-insert-signature (after emacspeak pre act)
  "announce insert signature."
  (dtk-speak "inserted signature"))
(defadvice mime-edit-enclose-alternative-region (after emacspeak pre act)
  " speak enclose as multipart alternative region"
(emacspeak-speak-line))
(defadvice mime-edit-enclose-parallel-region (after emacspeak pre act)
  "speak enclose multipart parallel region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-mixed-region (after emacspeak pre act)
  "speak enclose multipart mixed region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-digest-region (after emacspeak pre act)
  "speak enclose multipart digest region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-pgp-signed-region (after emacspeak pre act)
  "speak enclose pgp-sign region"
  (emacspeak-speak-line))
(defadvice mime-edit-enclose-pgp-encrypted-region (after emacspeak pre act)
  "speak enclose pgp-encrypted region"
  (emacspeak-speak-line))
(defadvice mime-edit-preview-message (after emacspeak pre act)
  "speak the preview mode line"
  (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-semi)

