;;; tts.lisp -- Common Lisp interface  to Emacspeak speech servers 
;;; $Id: tts.lisp 7078 2011-06-29 22:07:46Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:   Interface REP/Sawfish to Emacspeak TTS servers
;;; Keywords: Sawfish, Emacspeak, Audio Desktop
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
;;; Interface Common  Lisp  to Emacspeak TTS servers

;;; }

;;; { Settings

(defvar emacspeak "/home/raman/emacs/lisp/emacspeak"
"Root of Emacspeak installation.")

(defvar tts-process nil
"Handle to tts server connection.")

(defvar tts-dtk
  (concatenate 'string   emacspeak "servers/dtk-exp")
"DTK tcl server")



(defvar tts-outloud
  (concatenate 'string   emacspeak "servers/outloud")
  "Outloud tcl server")

(defvar tts-32-outloud
  (concatenate 'string   emacspeak "servers/32-outloud")
  "Outloud tcl server")

(defvar tts-engine tts-dtk
"Default TTS  engine. User settable.")

;;; }
;;; {Commands

(defun tts-open ()
  "Open a TTS session."
  (interactive)
  (setq tts-process (sb-ext:run-program tts-engine '() :wait nil
                                      :output :stream :input :stream))
  
  (start-process tts-process  tts-engine))

(defun tts-close ()
  "Close a TTS session."
  (interactive)
  (when(and  (processp tts-process)
             (process-running-p tts-process))
    (kill-process tts-process))
  (setq tts-process nil))

(defun tts-running-p ()
  "Is there a tts process up and running?"
  (and (processp tts-process) (process-running-p
                               tts-process)))


(defvar tts-stop-immediately t
  "Stop speech immediately.")

(defun tts-say (text)
  "Say some text."
  (unless (and  tts-process
 (process-running-p tts-process))
      (tts-open))
  (when tts-stop-immediately
    (format tts-process  "s\n"))
  (format tts-process "q {%s}; d\n" text))

;;; }

(provide 'tts)


;;; { end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;; }
