;;;$Id$
;;; tools.jl --- Emacs tool for sawfish
;;; $Author$
;;; Description:   Commands for launching or switching to
;;; a running Emacs
;;; Keywords: Sawfish, Emacspeak, Audio Desktop
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

;;; Copyright (C)  2000 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{ Introduction:

;;; Commentary:
;;; Tool for launching Emacs.

;;}}}
(require 'tts)
;;; Set this to the executable you wish to run  via command `emacs'
(defcustom emacs-program
 "/usr/bin/emacs -i &"
  "Emacs executable to run.")
(defcustom xemacs-program
 "/usr/bin/xemacs -i &"
  "Emacs executable to run.")


;;; Interactive command to start emacs or switch to an
;;; existing session.

(defun emacs  ()
  "Switch to a running emacs or start one if necessary."
  (interactive)
  (let ((w (car
            (delete-if-not
             (lambda (x)
               (string= (window-class x) "Emacs"))
             (managed-windows)))))
    (if w
	(display-window w)
      (system emacs-program))
    (and (tts-running-p) (tts-say-current-window))))


(defun xemacs  ()
  "Switch to a running xemacs or start one if necessary."
  (interactive)
  (let ((w (car
            (delete-if-not
             (lambda (x)
               (string= (window-class x) "xEmacs"))
             (managed-windows)))))
    (if w
	(display-window w)
      (system xemacs-program))
    (and (tts-running-p) (tts-say-current-window))))
(defun switch-to-emacs  ()
  "Switch to a running emacs "
  (interactive)
  (let ((w (car
            (delete-if-not
             (lambda (x)
               (string= (window-class x) "Emacs"))
             (managed-windows)))))
    (if w
	(display-window w))
    (and (tts-running-p) (tts-say-current-window))))

(defun delete-this-window-safely ()
  "Delete current window safely."
  (interactive)
  (delete-window-safely (car (managed-windows))))

(defun launch  (program)
  "Launch specified program or switch to it if it is already running."
  (interactive "sRun program:")
  (let ((w (car
            (delete-if-not
             (lambda (x)
               (string= (window-class x) program))
             (managed-windows)))))
    (if w
	(display-window w)
      (system program))
    (and (tts-running-p) (tts-say-current-window))))

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

