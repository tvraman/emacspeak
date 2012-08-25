;;; congrats.el --- Data Sonification, Graphics To Sound for emacspeak
;;; $Id: congrats.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
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
;;; Note that  the original version of CONGRATS  enabled  multiple types of "scans"
;;; you could listen to a curve in terms of Cartesian or Polar coordinates.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Helpers:

;;; Set up AOSS:
(defvar congrats-libaoss
  (if (file-exists-p "/proc/asound")
      "/usr/lib/libaoss.so"
    nil)
  "Location of libaoss.so")

(defvar congrats-libaoss-configured-p nil
  "Record that we have configured libaoss.")

(defun congrats-configure-alsa ()
  "Update LD_PRELOAD to include libaoss.so."
  (declare (special congrats-libaoss congrats-libaoss-configured-p))
  (unless congrats-libaoss (error "Alsa not available."))
  (let ((ld (getenv "LD_PRELOAD")))
    (unless
        (and congrats-libaoss
             (null congrats-libaoss-configured-p)
             ld (string-match "/usr/lib/libaoss.so" ld))
      (setq ld (if ld (format ":%s" ld) ""))
      (setenv "LD_PRELOAD" (format "%s%s" "/usr/lib/libaoss.so" ld))
      (and ld (setq congrats-libaoss-configured-p t)))))

;;}}}
;;{{{ Sonifiers:

;;; Take a vector of numbers and sonify it:

(defun congrats-data-to-tones (data &optional duration)
  "Takes  an array or list of numbers and produces a tone. 
Argument duration --- default is 1ms --- specifies duration of each step."
  (or duration (setq duration 1))
  (setq duration  (number-to-string duration))
  (when (arrayp  data) (setq data (append data nil)))
  (setq data (mapcar #'number-to-string data))
  (apply 'call-process "tones" nil t nil duration data))

;;}}}
;;{{{ Sample Tests:
(defvar congrats-test nil
  "Tests evaluated if set to T.")

(when congrats-test

  ;;{{{ Constant:

;;; 200hz is X=0
  (congrats-data-to-tones (loop for i from 200 to 1200 collect 200))
  (congrats-data-to-tones (loop for i from 200 to 1200 collect 440))
  (congrats-data-to-tones (loop for i from 200 to 1200 collect 660))
  (congrats-data-to-tones (loop for i from 200 to 1200 collect 880))

  ;;}}}
  ;;{{{  linear Change:

;;; x=y for x in [-2, 2] stepsize 1/1000 

  (congrats-data-to-tones
   (loop for i from  -2000  to 2000 collect (+ 200 (abs  i))))
  
;;; Contrast with circle:
;;; x in [-1, 1] stepsize 1/1000 
;;; y = 1+x    x < 0; y = 1-x x >0
;;; i.e. y= 1 -abs(x)
  
  (congrats-data-to-tones
   (loop for i from -1000 to 1000
         collect
         (+ 200                         ; translate X axis
            (- 1000 (abs i)))))

  ;;}}}
  ;;{{{ Circle:

;;; Circle: Radius 100: 
;;; Note: We translate the circle by 200hz which is X=0
;;; Unit Circle: x^2 + y^2 =1  
;;; Here is y for x  in [-1, 1] stepsize = 1/1000

  (congrats-data-to-tones
   (loop for i from -1000 to 1000
         collect
         (+ 200                         ; translating X axis
            (round
             (* 1000
                (sqrt (- 1 (/ (* i i ) 1000000.0))))))))  

  ;;}}}
  ;;{{{ Ellipse :

;;; Note: We translate the ellipse by 200hz which is X=0
;;;  x^2/a^2 +y^2/b^2 =1 a=1, b=3/4
;;; Here is y for x  in [-4, 4] stepsize = 1/1000

  (congrats-data-to-tones
   (loop for i from -1000 to 1000
         collect
         (+ 200                         ; translating X axis
            (round
             (* 1000
                0.75 ; b/a
                (sqrt (- 1(/ (* i i ) 1000000.0))))))))

;;; the same ellipse with major and minor axies flipped:
  (congrats-data-to-tones
   (loop for i from -1000 to 1000
         collect
         (+ 200                         ; translating X axis
            (round
             (* 1000
                (/ 4.0 3.0) ; b/a
                (sqrt (- 1(/ (* i i ) 1000000.0))))))))

  ;;}}}
  ;;{{{ Parabola: y=x^2 x in [-2, 2] stepsize 2/1000
  (congrats-data-to-tones
   (loop for i from -2000 to 2000 by 2
         collect
         (+ 200 ; translate X axis
            (round (* 1000 (/ (* i i) 1000000.0))))))
  

  ;;{{{ Sine And Cosine 
;;; Axis is not translated here:

;;; y = sin(x) x in [-Pi, Pi] stepsize 1/1000 
  (congrats-data-to-tones
   (loop for i from -3141 to 3141
         collect 
         (abs  (round (* 1000 (sin ( / i 1000.0)))))))

;;; Cosine in the same range:

  (congrats-data-to-tones
   (loop for i from -3141 to 3141
         collect 
         (abs  (round (* 1000 (cos ( / i 1000.0)))))))

  ;;}}}
  ;;}}}

  )

;;}}}
(provide 'congrats)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
