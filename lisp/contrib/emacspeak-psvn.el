;;; emacspeak-psvn.el --- Speech enable Emacs interface for working with svn
;;; $Author: LukasLoehrer $ 
;;; DescriptionEmacspeak extensions psvn
;;; Keywords:emacspeak, audio interface to emacs, version control 
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2006/02/16 14:23:57 $ |
;;;  $Revision: 23.5001 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright: Lukas Loehrer
;;; Copyright (c) 1995 by .
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

;;{{{  required 

(require 'emacspeak-preamble)
(require 'psvn)
;;}}}
;;{{{Advice for svn-status-mode

(defadvice svn-status-next-line (after emacspeak pre act)
  "Speak the line we just moved to."
  (when (interactive-p)
	(emacspeak-speak-line)))

(defadvice svn-status-previous-line (after emacspeak pre act)
  "Speak the line we just moved to."
  (when (interactive-p)
	(emacspeak-speak-line)))

;;}}}


;;{{{ mapping font faces to personalities 
(voice-setup-add-map
 '(
   (svn-status-marked-face voice-lighten)
   ))

;;}}}

(provide  'emacspeak-psvn)

;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
