;;; xbacklight.el --- Control Display Brightness From Emacs
;;;$Id$
;;;Emacs front-end to XBacklight
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2015, T. V. Raman<raman@cs.cornell.edu>
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
;;{{{ introduction

;;; Commentary:
;;; Provide an emacs front-end to xbacklight.
;;; This is a tool that controls the brightness on laptops.
;;; To install xbacklight, 
;;; sudo apt-get install xbacklight

;;; This module  is most   easily used in conjunction with  hydra:
;;;M-x package-install hydra

;;; Code:
;;}}}
;;{{{ required packages

(eval-when-compile(require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'hydra nil 'no-error)

;;}}}
;;{{{  Variables:

(defvar xbacklight-cmd
  (executable-find "xbacklight")
  "Location of xbacklight executable.")

;;}}}
;;{{{ Commands:
(defun xbacklight-get ()
  "Get current brightness level."
  (interactive)
  (message "Brightness is %s"
           (shell-command-to-string (format "%s -get " xbacklight-cmd ))))

(defun xbacklight-set (brightness)
  "Set brightness to  specified level.
`brightness' is a percentage value."
  (interactive "nBrightness: ")
  (shell-command (format "%s -set %s"
                         xbacklight-cmd brightness))
  (xbacklight-get))

(defun xbacklight-0 ()
  "Set brightness to 0"
  (interactive)
  (xbacklight-set 0))

(defun xbacklight-1 ()
  "Set brightness to 100"
  (interactive)
  (xbacklight-set 100))
(defgroup xbacklight nil
  "Control XBacklight from Emacs."
  :group 'emacspeak
  :group 'applications)

(defcustom xbacklight-step  10
  "Step-size used when incrementing and decrementing brightness."
  :type 'integer
  :group  'xbacklight)
  
(defun xbacklight-increment ()
  "Increase brightness by  by one step."
  (interactive)
  (shell-command
   (format "%s -inc %s"
           xbacklight-cmd xbacklight-step))
  (xbacklight-get))

(defun xbacklight-decrement ()
  "Decrease brightness by  by one step."
  (interactive)
  (shell-command
   (format "%s -dec %s"
           xbacklight-cmd xbacklight-step))
  (xbacklight-get))   

  ;;}}}
;;{{{ Hydra:

(when (featurep 'hydra)
  (defhydra hydra-brightness (global-map "<print>")
    "Brightness"
    ("i" xbacklight-increment "brighter")
    ("SPC" xbacklight-increment "brighter")
    ("d" xbacklight-decrement "dimmer")
    ("d" xbacklight-decrement "dimmer")
    ("g" xbacklight-get "Get")
    ("s" xbacklight-set "set")
    ("0" xbacklight-0 "black")
    ("<print>" xbacklight-0 "black")
    ("1" xbacklight-1 "white")))

;;}}}
(provide 'xbacklight)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
