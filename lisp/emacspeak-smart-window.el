;;; emacspeak-smart-window.el --- SMART-WINDOW  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable SMART-WINDOW An Emacs Interface to smart-window
;; Keywords: Emacspeak,  Audio Desktop smart-window
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNSMART-WINDOW FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; SMART-WINDOW ==  Smart Window switching for Emacs

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'sox-gen)

;;}}}

;;{{{ Interactive Commands:
(cl-loop
 for f in 
 '(smart-window-buffer-split smart-window-file-split
                             smart-window-move smart-window-rotate)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     (when (ems-interactive-p)
       (let ((dtk-stop-immediately nil))
         (sox-multiwindow 1 2)
         (dtk-speak (buffer-name (current-buffer)))
         (emacspeak-describe-tapestry 'full))))))

;;}}}
(provide 'emacspeak-smart-window)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
