;;; dtk-css-speech.el --- CSS -- Cascaded Speech Style Interface
;;; $Id$
;;; $Author$
;;; Description: DTK Interface for Cascaded Speech Style Sheets
;;; Keywords:emacspeak, audio interface to emacs CSS
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
;;; Copyright (c) 1996 by T. V. Raman
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


;;; Commentary:
;;{{{  Introduction:

;;; The CSS Speech Style Sheet specification defines a number of
;;; abstract device independent voice properties.
;;; A setting conforming to the CSS speech specification can be
;;; represented in elisp as a structure.

;;; We will refer to this structure as a "speech style".
;;; This structure needs to be mapped to device dependent codes to
;;; produce the desired effect.
;;; This module forms a bridge between User Agents e.g. Emacs-w3 that
;;; wish to implement a speech style sheet
;;; and Emacspeak's dtk-voices module.
;;; Emacspeak produces voice change effects by examining the value of
;;; text-property 'personality.
;;; Think of a buffer of formatted text along with the text-property
;;; 'personality appropriately set as a "aural display list".
;;; Applications like W3 that produce such formatted buffers  call function
;;; dtk-personality-from-speech-style  with a  "speech-style"
;;; --a structure as defined in this module and get back a symbol that
;;; they then assign to the value of property 'personality.
;;;Emacspeak's rendering engine then does the needful at the time
;;;speech is produced.
;;; Function dtk-personality-from-speech-style does the following:
;;; Takes as input a "speech style"
;;;(1)  Computes a symbol that will be used henceforth to refer to this
;;; specific speech style.
;;; (2) Examines emacspeak's internal voice table to see if this
;;; speech style has a voice already defined.
;;; If so it returns immediately.
;;; Otherwise, it does the additional work of defining a dtk-voice for
;;; future use.
;;; See module dtk-voices.el to see how voices are defined.

;;}}}
;; 
;;; Code:
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'dtk-voices)
;;{{{  A speech style structure

(defstruct  dtk-speech-style
  family
  gain left-volume right-volume
  average-pitch
  pitch-range
  stress
  richness
  )

;;}}}
;;{{{  Mapping css parameters to dtk codes

;;{{{ voice family codes

(defvar dtk-family-table nil
  "Association list of dtk voice names and control codes.")

(defsubst dtk-set-family-code (name code)
  "Set control code for voice family NAME  to CODE."
  (declare (special dtk-family-table))
  (when (stringp name)
    (setq name (intern name)))
  (setq dtk-family-table
        (cons (list name code )
              dtk-family-table)))

(defsubst dtk-get-family-code (name)
  "Get control code for voice family NAME."
  (declare (special dtk-family-table ))
  (when (stringp name)
    (setq name (intern name )))
  (or (cadr (assq  name dtk-family-table))
      ""))

(dtk-set-family-code 'paul ":np")
(dtk-set-family-code 'harry ":nh")
(dtk-set-family-code 'dennis ":nd")
(dtk-set-family-code 'frank ":nf")
(dtk-set-family-code 'betty ":nb")
(dtk-set-family-code 'ursula ":nu")
(dtk-set-family-code 'wendy ":nw")
(dtk-set-family-code 'rita ":nr")
(dtk-set-family-code 'kid ":nk")

;;}}}
;;{{{  hash table for mapping families to their dimensions

(defvar dtk-css-code-tables (make-hash-table)
  "Hash table holding vectors of dtk codes.
Keys are symbols of the form <FamilyName-Dimension>.
Values are vectors holding the control codes for the 10 settings.")

(defsubst dtk-css-set-code-table (family dimension table)
  "Set up voice FAMILY.
Argument DIMENSION is the dimension being set,
and TABLE gives the values along that dimension."
  (declare (special dtk-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (setf  (gethash key dtk-css-code-tables) table )))

(defsubst dtk-css-get-code-table (family dimension)
  "Retrieve table of values for specified FAMILY and DIMENSION."
  (declare (special dtk-css-code-tables))
  (let ((key (intern (format "%s-%s" family dimension))))
    (gethash key dtk-css-code-tables)))

;;}}}
;;{{{ volume

;;; Note:volume settings not implemented for Dectalks.
(defvar dtk-gain-table (make-vector  10 "")
  "Maps CSS volume settings to actual synthesizer codes.")

;;}}}
;;{{{  average pitch

;;; Average pitch for standard male voice is 122hz --this is mapped to
;;; a setting of 5.
;;; Average pitch varies inversely with speaker head size --a child
;;; has a small head and a higher pitched voice.
;;; We change parameter head-size in conjunction with average pitch to
;;; produce a more natural change on the Dectalk.

;;{{{  paul average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " ap %s hs % s"
		    (second setting)
		    (third setting)))))
   '(
     (0 96 115)
     (1 101 112)
     (2 108 109)
     (3 112 106)
     (4 118 103 )
     (5 122  100)
     (6 128 98)
     (7 134 96)
     (8 140 94)
     (9 147 91)
     ))
  (dtk-css-set-code-table 'paul 'average-pitch table ))

;;}}}
;;{{{  harry average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " ap %s hs % s"
		    (second setting)
		    (third setting)))))
   '(
     (0 50 125)
     (1 59 123)
     (2 68 121)
     (3 77 120)
     (4 83  118 )
     (5 89 115)
     (6 95 112)
     (7 110 105)
     (8 125 100)
     (9 140 95)
     ))
  (dtk-css-set-code-table 'harry 'average-pitch table ))

;;}}}
;;{{{  betty average pitch

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " ap %s hs % s"
		    (second setting)
		    (third setting)))))
   '(
     (0 160 115)
     (1 170 112)
     (2 181 109)
     (3 192 106)
     (4 200 103 )
     (5 208  100)
     (6 219 98)
     (7 225  96)
     (8 240 94)
     (9 260  91)
     ))
  (dtk-css-set-code-table 'betty 'average-pitch table ))

;;}}}

(defsubst dtk-get-average-pitch-code (value family)
  "Get  AVERAGE-PITCH for specified VALUE and  FAMILY."
  (or family (setq family 'paul))
  (aref (dtk-css-get-code-table family 'average-pitch)
	value))

;;}}}
;;{{{  pitch range

;;;  Standard pitch range is 100 and is  mapped to
;;; a setting of 5.
;;; A value of 0 produces a flat monotone voice --maximum value of 250
;;; produces a highly animated voice.
;;; Additionally, we also set the assertiveness of the voice so the
;;; voice is less assertive at lower pitch ranges.
;;{{{  paul pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " pr %s as %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 0)
     (1 20 10)
     (2 40 20)
     (3 60 30)
     (4 80 40 )
     (5 100 50 )
     (6 137 60)
     (7 174 70)
     (8 211 80)
     (9 250 100)
     ))
  (dtk-css-set-code-table 'paul 'pitch-range table ))

;;}}}
;;{{{  harry pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " pr %s as %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 0)
     (1 16 20)
     (2 32 40)
     (3 48 60)
     (4 64 80 )
     (5 80 100 )
     (6 137 100)
     (7 174 100)
     (8 211 100)
     (9 250 100)
     ))
  (dtk-css-set-code-table 'harry 'pitch-range table ))

;;}}}
;;{{{  betty pitch range

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " pr %s as %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 0)
     (1 50 10)
     (2 80 20)
     (3 100 25)
     (4 110 30 )
     (5 140 35)
     (6 165 57)
     (7 190 75)
     (8 220 87)
     (9 250 100)
     ))
  (dtk-css-set-code-table 'betty 'pitch-range table ))

;;}}}
(defsubst dtk-get-pitch-range-code (value family)
  "Get pitch-range code for specified VALUE and FAMILY."
  (or family (setq family 'paul))
  (aref (dtk-css-get-code-table family 'pitch-range)
	value))

;;}}}
;;{{{  stress

;;; On the Dectalk we vary four parameters
;;; The hat rise which controls the overall shape of the F0 contour
;;; for sentence level intonation and stress,
;;; The stress rise that controls the level of stress on stressed
;;; syllables,
;;; the baseline fall for paragraph level intonation
;;; and the quickness --a parameter that controls whether the final
;;; frequency targets are completely achieved in the phonetic
;;; transitions.
;;{{{  paul stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " hr %s sr %s qu %s bf %s "
		    (second setting)
		    (third setting)
		    (fourth setting)
		    (fifth setting)))))
   '(
     (0  0 0 0 0)
     (1 3 6  20 3)
     (2 6 12  40 6)
     (3 9 18  60 9 )
     (4 12 24 80 14)
     (5 18 32  100 18)
     (6 34 50 100 20)
     (7 48  65 100 35)
     (8 63 82 100 60)
     (9 80  90 100  40)
     ))
  (dtk-css-set-code-table 'paul 'stress table))

;;}}}
;;{{{  harry stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " hr %s sr %s qu %s bf %s "
		    (second setting)
		    (third setting)
		    (fourth setting)
		    (fifth setting)))))
   '(
     (0  0 0 0 0)
     (1 4 6 2 2 )
     (2 8 12 4 4 )
     (3 12 18 6 6 )
     (4 16 24 8 8 )
     (5 20 30 10 9)
     (6 40  48 32 16)
     (7 60 66 54 22)
     (8 80 78 77 34)
     (9 100 100 100 40)
     ))
  (dtk-css-set-code-table 'harry 'stress table))

;;}}}
;;{{{  betty stress

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table
	    (first setting)
	    (format " hr %s sr %s qu %s bf %s "
		    (second setting)
		    (third setting)
		    (fourth setting)
		    (fifth setting)))))
   '(
     (0  1 1 0 0)
     (1 3 4 11 0)
     (2 5 8 22 0)
     (3 8 12 33 0 )
     (4 11  16 44 0)
     (5 14 20 55 0)
     (6 35 40 65 10)
     (7 56 80 75 20)
     (8 77 90 85 30)
     (9 100 100 100 40)
     ))
  (dtk-css-set-code-table 'betty 'stress table))

;;}}}
(defsubst dtk-get-stress-code (value family)
  (or family (setq family 'paul ))
  (aref (dtk-css-get-code-table family 'stress)
        value))

;;}}}
;;{{{  richness

;;; Smoothness and richness vary inversely.
;;{{{  paul richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
	    (format " ri %s sm %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 100)
     (1 14 80)
     (2 28 60)
     (3 42 40 )
     (4 56 20)
     (5 70  3 )
     (6 60 24 )
     (7 70 16)
     (8 80 8 20)
     (9 100  0)
     ))
  (dtk-css-set-code-table 'paul 'richness table))

;;}}}
;;{{{  harry richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
	    (format " ri %s sm %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 100 0)
     (1 96 3)
     (2 93 6)
     (3 90 9)
     (4 88 11)
     (5 86 12)
     (6 60 24 )
     (7 40 44)
     (8 20 65)
     (9 0 70)
     ))
  (dtk-css-set-code-table 'harry 'richness table))

;;}}}
;;{{{  betty richness

(let ((table (make-vector 10 "")))
  (mapcar
   (function
    (lambda (setting)
      (aset table (first setting)
	    (format " ri %s sm %s "
		    (second setting)
		    (third setting)))))
   '(
     (0 0 100)
     (1 8 76)
     (2 16 52)
     (3 24  28)
     (4 32 10)
     (5 40 4)
     (6 50 3 )
     (7 65 3)
     (8 80 8 2)
     (9 100  0)
     ))
  (dtk-css-set-code-table 'betty 'richness table))

;;}}}

(defsubst dtk-get-richness-code (value family)
  (or family (setq family 'paul))
  (aref (dtk-css-get-code-table family 'richness)
        value))

;;}}}

;;}}}
;;{{{  dtk-define-voice-from-speech-style

(defun dtk-define-voice-from-speech-style (name style)
  "Define NAME to be a dtk voice as specified by settings in STYLE."
  (let* ((family(dtk-speech-style-family style))
	 (command
	  (concat "["
		  (dtk-get-family-code family)
		  " :dv "
		  (dtk-get-average-pitch-code (dtk-speech-style-average-pitch style) family)
		  (dtk-get-pitch-range-code (dtk-speech-style-pitch-range style) family)
		  (dtk-get-stress-code (dtk-speech-style-stress style ) family)
		  (dtk-get-richness-code (dtk-speech-style-richness style) family)
		  "]")))
    (dtk-define-voice name command)))

;;}}}
;;{{{  dtk-personality-from-speech-style

(defun dectalk-personality-from-speech-style (style)
  "First compute a symbol that will be name for this STYLE.
Then see if a voice defined for it.
Finally return the symbol"
  (cond
   ((= 0 (dtk-speech-style-gain style))
    'inaudible)
   (t
    (let ((name (intern
                 (format "%s-%s%s%s%s"
                         (dtk-speech-style-family style)
                         (dtk-speech-style-average-pitch style)
                         (dtk-speech-style-pitch-range style)
                         (dtk-speech-style-stress style)
                         (dtk-speech-style-richness style)))))
      (unless (dtk-voice-defined-p name)
        (dtk-define-voice-from-speech-style name style))
      name))))

;;}}}
(provide  'dtk-css-speech)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
