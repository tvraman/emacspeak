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
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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


;;{{{  Introduction

;;; emacspeak extensions to speak window widnow layouts 

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)
(require 'tapestry)

;;}}}
;;{{{  Interactive defun 
;;;###autoload
(defun emacspeak-tapestry-describe-tapestry (&optional details)
  "Describe the current layout of visible buffers in current frame.
Use interactive prefix arg to get coordinate positions of the
displayed buffers."
  (interactive "P")
  (declare (special voice-animate voice-bolden))
  (let* ((buffer-map (tapestry-buffer-map ))
         (count (length buffer-map))
         (window-list  (tapestry-window-list))
         (windows nil)
         (description
          (format "Frame displays %s buffer%s "
                  count 
                  (if (> count 1) "s" ""))))
    (put-text-property 0 (length description)
                       'personality  voice-annotate
                       description)
    (setq windows 
          (cond
           (details 
            (loop for buffer in buffer-map
                  and window in window-list
                  collect
                  (let ((w (format "%s "  (second buffer)))
                        (corners  (window-edges window))
                        (tl nil )
                        (br nil))
                    (put-text-property 0 (length w)
                                       'personality
                                       voice-animate w)
                    (setq tl
                          (format  " %d %d "
                                   (first corners) (second corners))
                          br  (format " %d %d "
                                      (third corners) (fourth corners)))
                    (put-text-property 0 (length tl)
                                       'personality voice-bolden tl)
                    (put-text-property 0 (length br)
                                       'personality voice-bolden br)
                    (concat w
                            " with top left "
                            tl
                            " and bottom right "
                            br))))
           (t
            (loop for buffer in buffer-map
                  collect
                  (second buffer)))))
    (tts-with-punctuations 'all
                           (dtk-speak
                            (concat description
                                    (mapconcat #'identity
                                               windows
                                               " "))))))

;;;###autoload
(defun emacspeak-tapestry-select-window-by-name (buffer-name)
  "Select window by the name of the buffer it displays.
This is useful when using modes like ECB or the new GDB UI where
  you want to preserve the window layout 
but quickly switch to a window by name."
  (interactive
   (list
    (completing-read 
     "Select window: "
     (mapcar 
      #'(lambda (w)
          (list (buffer-name (window-buffer w))))
      (tapestry-window-list))
     nil 'must-match)))
  (pop-to-buffer buffer-name)
  (emacspeak-speak-line))

;;}}}
(provide  'emacspeak-tapestry)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
