;;; emacspeak-tapestry.el --- Speak information about current layout of windows
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak module to speak window tapestries
;;; Keywords:emacspeak, audio interface to emacs tapestry
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
;;;Copyright (C) 1995, 1996, 1997, 1998, 1999   T. V. Raman  
;;; Copyright (c) 1995 by T. V. Raman  
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
(eval-when (compile) (require 'tapestry))
 (condition-case nil 
     (require 'tapestry)
   (error "Could not locate tapestry.el"))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'emacspeak-fix-interactive)
;;{{{  Introduction

;;; emacspeak extensions to speak window widnow layouts 

;;}}}
;;{{{  Interactive defun 

(defun emacspeak-tapestry-describe-tapestry (&optional name)
  "Describe the current layout of visible buffers in current frame."
  (interactive)
  (let* ((buffer-map (tapestry-buffer-map ))
         (count (length buffer-map))
         (window-list  (tapestry-window-list))
         (description
          (format "%s displays %s buffer%s  "
                  (or name "Current frame ")
                  count 
                  (if (> count 1) "s" " "))))
    (put-text-property 0 (length description)
                       'personality  'annotation-voice description)
    (loop for buffer in buffer-map
          and window in window-list
          do
          (setq description
                (concat description
                        (format "%s
with window coordinates %s"
                                (second buffer)
                                (window-edges window)))))
    (dtk-speak description)))

;;}}}
(provide  'emacspeak-tapestry)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
