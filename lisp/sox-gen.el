;;; sox-gen.el: -*- lexical-binding: t -*-
;;; $Author: tv.raman.tv $
;;; Description:  collection of SoX  sound generators
;;; Keywords: Emacspeak,  Audio Desktop sox
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
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Contains a collection of functions that generate sound using SoX.
;;; These functions are primarily for use from other Emacs/Emacspeak modules.
;;; This module can be used independent of Emacspeak.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'sox)

;;}}}
;;{{{ Generator:

(defun sox-gen-cmd (cmd)
  "Play specified command."
  (declare (special sox-play))
  (apply #'start-process "SoX" nil sox-play  (split-string cmd)))

;;}}}
;;{{{ synth:

(defconst sox-synth-cmd
  "-q -n synth %s "
  "Invoke synth generation.")

(defun sox-synth (length  &rest args)
  "Call synth with length and args."
  (declare (special sox-synth-cmd))
  (sox-gen-cmd
   (concat
    (format sox-synth-cmd length)
    (mapconcat #'identity args " "))))

;;}}}
;;{{{ Sin:

(defconst sox-sin-cmd
  "-q -n synth %s sin %s "
  "Command-line that produces a simple sine wave.")

(defun sox-sin (length freq &rest args)
  "Play sine wave specified by length and freq.
Freq can be specified as a frequency, note (%nn) or frequency range.
Remaining args specify additional commandline args."
  (declare (special sox-sin-cmd))
  (sox-gen-cmd
   (concat
    (format sox-sin-cmd length freq)
    (mapconcat #'identity args " "))))

;;}}}
;;{{{ Binaural Audio:

(defconst sox-binaural-cmd
  "-q -n synth %s sin %s sin %s gain %s channels 2 "
  "Command-line that produces a binaural beat.")

;;;###autoload
(defun sox-tone-binaural (length freq beat gain)
  "Play binaural audio with carrier frequency `freq', beat `beat',  and gain `gain'."
  (interactive
   (list
    (read-number "Duration in seconds: " 60)
    (read-number "Carrier Frequency [50 -- 800]: " 100)
    (read-number "Beat Frequency [0.5 -- 40]: " 4.5)
    (read-number "Gain [Use negative values]: " -10)))
  (declare (special sox-binaural-cmd))
  (sox-gen-cmd (format sox-binaural-cmd length freq (+ freq beat) gain)))

(defconst sox-beats-binaural-cmd
  "-q -n synth %s %s gain %s channels 2 "
  "Command-line that produces multiple  binaural beats.")

(defsubst sox-read-binaural-beats ()
  "Read and return a list of binaural beat-spec tupples."
  (let ((specs nil)
        (this-freq 0)
        (this-beat nil))
    (while  this-freq
      (setq this-freq  (read-number "Carrier Frequency [50-800]: " 0))
      (when (zerop this-freq) (setq this-freq nil))
      (when this-freq
        (setq this-beat (read-number "Beat Frequency [0.5 -- 40]: " 4.5))
        (push (list this-freq this-beat) specs)))
    (nreverse specs)))

;;;###autoload
(defun sox-beats-binaural (length beat-spec-list  gain)
  "Play binaural audio with beat-spec specifying the various tones.
Param `beat-spec' is a list of `(carrier beat) tupples."
  (interactive
   (list
    (read-number "Duration in seconds: " 60)
    (sox-read-binaural-beats)
    (read-number "Gain [Use negative values]: " -10)))
  (declare (special sox-beats-binaural-cmd))
  (unless beat-spec-list (error "No beats specified. "))
  (sox-gen-cmd
   (format sox-beats-binaural-cmd
           length
           (mapconcat
            #'(lambda (spec)
                (format "sin %s sin %s"
                        (first spec)
                        (+ (first spec) (second spec))))
            beat-spec-list " ")
           gain)))

(defstruct sox--binaural
  beats ; list of beat-specs
  gain ; overall gain
  )

;;; Helper:

(defun sox--binaural-play  (length binaural)
  "Plays an instance of sox-binaural."
  (sox-beats-binaural  length
                       (sox--binaural-beats  binaural)
                       (sox--binaural-gain binaural)))

(defvar sox-binaural-effects-table (make-hash-table :test #'equal)
  "Hash table mapping binaural effect names to effect structures.")

(defun sox-define-binaural-effect   (name effect)
  "Setup mapping  from name to binaural effect."
  (declare (special sox-binaural-effects-table))
  (puthash name effect sox-binaural-effects-table))

;;;###autoload
(defun sox-binaural (name duration)
  "Play specified binaural effect."
  (interactive
   (list
    (completing-read "Binaural Effect: " sox-binaural-effects-table nil 'must-match)
    (read-number "Duration: " 600)))
  (declare (special sox-binaural-effects-table))
  (sox--binaural-play duration
                      (gethash name sox-binaural-effects-table)))
;;{{{  Define Effects:

;;; delta, theta, alpha, beta
;;; sleep, dream, think, act

(sox-define-binaural-effect
 "sleep" ; delta
 (make-sox--binaural
  :beats '((100 0.5) (200 1.5) (250 2.5) (400 3.75))
  :gain -14))

(sox-define-binaural-effect
 "dream" ; theta
 (make-sox--binaural
  :beats '((100 4.5) (200 5.0) (250 7.0) (400 7.34))
  :gain -14))

(sox-define-binaural-effect
 "think" ;alpha
 (make-sox--binaural
  :beats '((150 8.5) (225 8.8) (300 10.0) (400 12.0))
  :gain -14))

(sox-define-binaural-effect
 "act" ; beta
 (make-sox--binaural
  :beats '((100 13.5) (200 18.0) (300 23.0) (400 40.0))
  :gain -14))
;;; Chakras: Carrier frequencies taken from  the Web.
;;; https://sourceforge.net/p/sbagen/mailman/message/3047882/

;;; root:         256 Hz
;;; navel:        288 Hz
;;; solar plexus: 320 Hz
;;; heart:        341.3 Hz
;;; throat:       384 Hz
;;; 3rd eye:      426.7 Hz
;;; crown:        480 Hz

;;; Use theta (4.5 --7 as the beat frequency)
(defconst sox--chakra-settings
  '(
    ("root" 256 7.0)
    ("navel" 288 6.5)
    ("solar-plexus" 320 6.0)
    ("heart" 341.3 5.5)
    ("throat" 384 5.0)
    ("3rd-eye" 426.7 5.2)
    ("crown" 480 4.75)
    )
  "Frequency settings.")

(cl-loop
 for s in sox--chakra-settings do
 (sox-define-binaural-effect
  (first s)
  (make-sox--binaural
   :beats `(,(cdr s))
   :gain -20)))

;;}}}

;;}}}
;;{{{ Pluck:

(defconst sox-pluck-cmd
  "-q -n synth %s pluck %s channels 2 "
  "Command-line that produces a simple plucke.")

(defun sox-pluck (length freq &rest args)
  "Play plucke  specified by length and freq.
Freq can be specified as a frequency, note (%nn) or frequency range."
  (declare (special sox-pluck-cmd))
  (sox-gen-cmd
   (concat
    (format sox-pluck-cmd length freq)
    (mapconcat #'identity args " "))))

;;}}}
;;{{{ Chime:

(defconst sox-chime-cmd
  "-q -n synth -j 3 sin %3 sin %-2 sin %-5 sin %-9 \
                   sin %-14 sin %-21 fade h .01 2 1.5 delay \
                   1.3 1 .76 .54 .27 remix - fade h 0 2.7 2.5 norm -1 channels 2"
  "Command-line that produces a simple chime.")

;;;###autoload
(defun sox-chime (&optional tempo speed)
  "Play chime --- optional args tempo and speed default to 1."
  (declare (special sox-chime-cmd))
  (sox-gen-cmd
   (concat
    sox-chime-cmd
    (when tempo (format " tempo %s" tempo))
    (when speed (format " speed %s" speed)))))

;;}}}
;;{{{ Guitar Chord:

(defconst sox-guitar-chord-cmd
  "-q -n synth pl G2 pl B2 pl D3 pl G3 pl D4 pl G4 \
                   delay 0 .05 .1 .15 .2 .25 remix - fade 0 4 .1 norm -1 channels 2"
  "Play a guitar chord.")

(defun sox-guitar-chord (&optional tempo speed)
  "Play a guitar chord"
  (declare (special sox-guitar-chord-cmd))
  (sox-gen-cmd
   (concat
    sox-guitar-chord-cmd
    (when tempo (format " tempo %s" tempo))
    (when speed (format " speed %s" speed)))))

;;}}}
(provide 'sox-gen)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
