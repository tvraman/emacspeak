;;; outloud-css-speech.el --- CSS -- Cascaded Speech Style Interface
;;; $Id$
;;; $Author$
;;; Description: Outloud Interface for Cascaded Speech Style Sheets
;;; Keywords:emacspeak, audio interface to emacs CSS
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

;;;Copyright (C) 1995 -- 2003, T. V. Raman 
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


;;; Commentary:
;;{{{  Introduction:

;;; see module dtk-css-speech for details.

;;}}}
;;{{{ required modules

;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'outloud-voices)

;;}}}
;;{{{  A speech style structure

;; (defstruct  outloud-speech-style        ;;
;;   family                                ;;
;;   gain left-volume right-volume         ;;
;;   average-pitch                         ;;
;;   pitch-range                           ;;
;;   stress                                ;;
;;   richness                              ;;
;;   )                                     ;;

;;}}}

(provide  'outloud-css-speech)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
