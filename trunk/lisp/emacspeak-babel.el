;;; emacspeak-babel.el --- Speech-enable BabelFish
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to use BabelFish
;;; Keywords: Emacspeak, WWW interaction
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

;;;Copyright (C) 1995 -- 2006, T. V. Raman 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:
;;;Speech-enable Babelfish translator
;;; Package babel can be obtained from:
;;;     <URL:http://www.chez.com/emarsden/downloads/babel.el>

;;}}}
;;{{{ speech-enable commands

(defun emacspeak-babel-done ()
  "Announce  completion of the translation request."
  (message "Displayed translation in other window")
  (emacspeak-auditory-icon 'task-done))

(add-hook 'babel-mode-hook 'emacspeak-babel-done)
;;}}}
(provide 'emacspeak-babel)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
