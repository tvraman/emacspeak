;;; light.el --- Control Display Brightness From Emacs  -*- lexical-binding: t; -*-
;;
;; Emacs front-end to Light
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman<tv.raman.tv@gmail.com>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ introduction

;;; Commentary:
;; Provide an emacs front-end to light.
;; This is a tool that controls the brightness on laptops.
;; To install light,
;; sudo apt-get install light
;; Install light setuid (sudo chmod u+s /usr/bin/light)
;; This module  is most   easily used in conjunction with  hydra:
;; M-x package-install hydra

;;; Code:
;;}}}
;;{{{ required packages

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{  Variables:

(defvar light-cmd
  (executable-find "light")
  "light executable.")

;;}}}
;;{{{ Commands:

;;;###autoload
(defun light-get ()
  "Get  brightness level."
  (interactive)
  (cl-declare (special light-cmd))
  (when light-cmd
    (let ((value (read (shell-command-to-string (format "%s " light-cmd)))))
      (cond
       ((numberp value)
        (when (called-interactively-p 'interactive)
          (message "Brightness is %d" (round  value)))
        value)))))

;;;###autoload
(defun light-set (brightness)
  "Set brightness."
  (interactive "sBrightness: ")
  (cl-declare (special light-cmd))
  (when light-cmd
    (start-process "Light" nil light-cmd "-S" brightness)
    (when (called-interactively-p 'interactive)
      (message "Set brightness to %s" brightness))))

(defvar light-step  "10"
  "Step-size used when incrementing and decrementing brightness.")

;;;###autoload
(defun light-increment ()
  "Increase brightness."
  (interactive)
  (start-process "Light" nil  light-cmd "-A" light-step)
  (when (called-interactively-p 'interactive)
    (message "Increased brightness")))

;;;###autoload
(defun light-decrement ()
  "Decrease brightness."
  (interactive)
  (start-process "Light" nil light-cmd  "-U" light-step)
  (when (called-interactively-p 'interactive)
    (message "decreased brightness")))

;;;###autoload
(defun light-black ()
  "Black screen."
  (interactive)
  (light-set "0")
  (when (called-interactively-p 'interactive)
    (message "Turned screen black")))

;;;###autoload
(defun light-white ()
  "White screen."
  (interactive)
  (light-set "100")
  (when (called-interactively-p 'interactive)
    (message "Full brightness")))

;;}}}
(provide 'light)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
