;;; emacspeak-kkc.el -- voicifying KKC(Kana Kanji Conversion)
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable mew
;;; Keywords: Emacspeak, Mew, IM, mail, Advice, Spoken Output
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
(eval-when-compile
  (require 'cl)
  (require 'emacspeak-m17n-ja))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice kkc-update-conversion (after emacspeak pre act)
  "Speaks when conversion candidate is updated"
  (let ((kkc-cand (buffer-substring
		   (overlay-start kkc-overlay-head)
		   (overlay-end kkc-overlay-head))))
    (dtk-speak (emacspeak-ja-convert-string-to-phonetic kkc-cand) nil 'ja)
    )
)

(provide 'emacspeak-kkc)
