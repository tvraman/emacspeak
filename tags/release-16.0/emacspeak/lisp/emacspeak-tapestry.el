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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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

(eval-when-compile (require 'cl))
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

(defun emacspeak-tapestry-describe-tapestry ()
  "Describe the current layout of visible buffers in current frame."
  (interactive)
  (let* ((buffer-map (tapestry-buffer-map ))
         (count (length buffer-map))
         (window-list  (tapestry-window-list))
         (windows nil)
         (description
          (format "Frame displays %s buffer%s "
                  count 
                  (if (> count 1) "s" ""))))
    (put-text-property 0 (length description)
                       'personality  'annotation-voice
                       description)
    (setq windows 
          (loop for buffer in buffer-map
                and window in window-list
                collect
                (let ((w (format "%s "  (second buffer)))
                      (corners  (window-edges window))
                      (tl nil )
                      (br nil))
                  (put-text-property 0 (length w)
                                     'personality
                                     'paul-animated w)
                  (setq tl
                        (format  " %d %d "
                                 (first corners) (second corners))
                        br  (format " %d %d "
                                    (third corners) (fourth corners)))
                  (put-text-property 0 (length tl)
                                     'personality 'harry tl)
                  (put-text-property 0 (length br)
                                     'personality 'harry br)
                        (concat w
                                " with top left "
                                tl
                                " and bottom right "
                                br))))
    (tts-with-punctuations "some"
                           (dtk-speak
                            (concat description
                                    (apply 'concat windows ))))))


;;}}}
(provide  'emacspeak-tapestry)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
