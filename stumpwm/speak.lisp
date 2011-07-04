;;; speak.lisp -- Attach Speech actions to StumpWM hooks
;;; $Id: speak.lisp 7078 2011-06-29 22:07:46Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:   Interface Attach TTS  actions to Window hooks
;;; Keywords: StumpWM, Emacspeak, Audio Desktop
;;; {  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2011-06-29 15:07:46 -0700 (Wed, 29 Jun 2011) $ |
;;;  $Revision: 7078 $ |
;;; Location undetermined
;;;

;;; }
;;; {  Copyright:

;;; Copyright (C)  2011, T. V. Raman<raman@cs.cornell.edu>
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

;;; }
;;; { Introduction:

;;; Commentary:
;;; Interface StumpWM to TTS

;;; }
(require 'tts)
(use-package :stumpwm)
;;; {Speak Actions:

(defun speak-window (window)
  "Speak  window  information."
  (tts-speak (window-name window)))

(defun speak-current-window ()
  "Speak current window  information."
  (tts-speak (window-name (current-window))))
(defun speak-messages (&rest messages)
  "Speak messages, a list of lines."
  (tts-speak-list  messages))

;;; }
;;; {Attach Handlers:

(stumpwm:add-hook 'stumpwm:*new-window-hook* 'speak-current-window)
;*destroy-window-hook*
(stumpwm:add-hook 'stumpwm:*focus-window-hook* 'speak-current-window)
;*focus-frame-hook*
;*new-frame-hook*
;*message-hook*
;*message-hook*
;*focus-group-hook*
;*urgent-window-hook*

;;; }


(provide 'speak)

;;; { end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;; }
