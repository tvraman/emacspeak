;;; emacspeak-dmacro.el --- Speech enable DMacro -- Dynamic  Macros 
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for dmacro
;;; Keywords:emacspeak, audio interface to emacs dmacro
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 24.0 $ | 
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

;;{{{  Required libraries

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Provide additional advice to dmacro.
;;; dmacro is used to write code rapidly and consistently. 

;;}}}

;;{{{  advice:

;;; A simple minded thing to do is to at least speak the line that point is on
(defadvice insert-dmacro (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-line )))

;;}}}
;;{{{  fix interactive commands

;;}}}
(provide  'emacspeak-dmacro)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
