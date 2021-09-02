;;; emacspeak-tapestry.el --- Speak information about current layout of windows  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description: Emacspeak module to speak window tapestries
;;; Keywords:emacspeak, audio interface to emacs tapestry
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman 
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}


;;{{{  Introduction
;;; Commentary:
;;; emacspeak extensions to speak window widnow layouts 
;;; Code:
;;}}}
;;{{{ requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
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
  (cl-declare (special voice-animate voice-bolden))
  (let* ((buffer-map (tapestry-buffer-map))
         (count (length buffer-map))
         (window-list  (tapestry-window-list))
         (windows nil)
         (description
          (format
           "Displaying %s window%s "
           count 
           (if (> count 1) "s" ""))))
    (put-text-property
     0 (length description) 'personality  voice-annotate description)
    (setq
     windows 
     (cond
      (details 
       (cl-loop
        for buffer in buffer-map
        and window in window-list
        collect
        (let ((w (format "%s "  (cl-second buffer)))
              (corners  (window-edges window))
              (tl nil)
              (br nil))
          (put-text-property
           0 (length w)
           'personality voice-animate w)
          (setq
           tl (format  " %d %d " (cl-second corners) (cl-first corners))
           br  (format " %d %d " (cl-fourth corners) (cl-third corners)))
          (put-text-property 0 (length tl) 'personality voice-bolden tl)
          (put-text-property 0 (length br) 'personality voice-bolden br)
          (concat w " with top left " tl " and bottom right " br))))
      (t (mapcar #'cl-second buffer-map))))
    (emacspeak--sox-multiwindow (window-edges))
    (tts-with-punctuations
     'all
     (dtk-speak (concat description (mapconcat #'identity windows " "))))))

(cl--defalias 'emacspeak-speak-window-layout 'emacspeak-tapestry-describe-tapestry)
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
      (window-list))
     nil 'must-match)))
  (pop-to-buffer buffer-name)
  (emacspeak-speak-line))

;;}}}
(provide  'emacspeak-tapestry)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
