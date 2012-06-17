;;; emacspeak-congrats.el --- Data Sonification, Graphics To Sound for emacspeak
;;; $Id: emacspeak-congrats.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Data Sonification, Convert Graphics to Sound
;;; Keywords: Emacspeak,  Audio Desktop CONGRATS
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;; MERCHANTABILITY or FITNCONGRATS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; CONGRATS ==  Convert Graphics To Sound
;;; I first implemented this idea in late   1988 for my final year project at IIT Bombay.
;;; A scaled-down version of this project was submitted to the John Hopkins  National Search  in 1991
;;; See  http://www.cs.cornell.edu/home/raman/publications/ieee-congrats.ps
;;; Congrats was originally implemented on a BBC Micro with 32K of memory.
;;; This module provides data sonification services for the Emacspeak Audio Desktop
;;; in the spirit of Congrats --- it uses package siggen --- and specifically, the tones utility from that package for generating the auditory output.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Helpers:

;;; Set up AOSS:
(defvar emacspeak-congrats-libaoss
  (if (file-exists-p "/proc/asound")
      "/usr/lib/libaoss.so"
    nil)
  "Location of libaoss.so")

(defun emacspeak-congrats-configure-alsa ()
  "Update LD_PRELOAD to include libaoss.so."
  (declare (special emacspeak-congrats-libaoss))
  (unless emacspeak-congrats-libaoss (error "Alsa not available."))
  (let ((ld (getenv "LD_PRELOAD")))
    (unless
        (and emacspeak-congrats-libaoss
             ld (string-match "/usr/lib/libaoss.so" ld))
      (setq ld (if ld (format ":%s" ld) ""))
      (setenv "LD_PRELOAD" (format "%s%s" "/usr/lib/libaoss.so" ld)))))

;;}}}
;;{{{ Sonifiers:

;;; Take a vector of numbers and sonify it:

(defun emacspeak-congrats-data-to-tones (data &optional duration)
  "Takes  an array or list of numbers and produces a tone. 
Argument duration --- default is 1ms --- specifies duration of each step."
  (or duration (setq duration 1))
  (setq duration  (number-to-string duration))
  (when (arrayp  data) (setq data (append data nil)))
  (setq data (mapcar #'number-to-string data))
  (apply 'call-process
         "tones"
         nil t nil
         duration data))

;;}}}
;;{{{ Sample Tests:
(defvar emacspeak-congrats-test nil
  "Tests evaluated if set to T.")

(when emacspeak-congrats-test

;;; Constant:
  (emacspeak-congrats-data-to-tones (loop for i from 200 to 1200 collect 440))
  (emacspeak-congrats-data-to-tones (loop for i from 200 to 1200 collect 660))
  (emacspeak-congrats-data-to-tones (loop for i from 200 to 1200 collect 880))

;;; linear Change
  (emacspeak-congrats-data-to-tones (loop for i from 0 to 1200 by 1 collect i))
  (emacspeak-congrats-data-to-tones (loop for i downfrom 1200 to 0 by 1 collect i))
  (emacspeak-congrats-data-to-tones
   (append
    (loop for i downfrom 1200 to 0 by 1 collect i)
    (loop for i from 0 to 1200 by 1 collect i)))

;;; Circle: Radius 100: First Quadrant 
  (emacspeak-congrats-data-to-tones
   (loop for i from  0 to 1000
         collect
         (floor                         ; tones wants integers 
          (* 12                         ; scale up freq range
             (sqrt
              (- 10000.0                   ; r^2: r=100
                 (* (/ i 10.0) (/ i 10.0)) ; steps of .1
                 ))))))



  (emacspeak-congrats-data-to-tones
   (append                             ; 2quadrants of the circle
    (loop for i from  0 to 1000
          collect
          (floor                        ; tones wants integers 
           (* 12                        ; scale up freq range
              (sqrt
               (- 10000.0                   ; r^2: r=100
                  (* (/ i 10.0) (/ i 10.0)) ; steps of .1
                  )))))
    (loop for i downfrom 1000 to 0
          collect
          (floor                        ; tones wants integers 
           (* 12                        ; scale up freq range
              (sqrt
               (- 10000.0                   ; r^2: r=100
                  (* (/ i 10.0) (/ i 10.0)) ; steps of .1
                  )))))))


  )

;;}}}
(provide 'emacspeak-congrats)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
