;;; sox-gen.el --- Binaural Beats And More Using SoX -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  collection of SoX  sound generators including Binaural Beats
;;; Keywords: Emacspeak,  Audio Desktop sox
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; Provides binaural audio along with pre-defined themes.
;;; This module can be used independent of Emacspeak.

;;; @subsection Binaural Beats  Using SoX
;;;
;;; A binaural beat is an auditory illusion perceived when two different
;;; pure-tone sine waves, both with frequencies lower than 1500 Hz, with
;;; less than a 40 Hz difference between them, are presented to a listener
;;; dichotically (one through each ear). For example, if a 530 Hz pure tone
;;; is presented to a subject's right ear, while a 520 Hz pure tone is
;;; presented to the subject's left ear, the listener will perceive the
;;; auditory illusion of a third tone, in addition to the two pure-tones
;;; presented to each ear. The third sound is called a binaural beat, and in
;;; this example would have a perceived pitch correlating to a frequency of
;;; 10 Hz, that being the difference between the 530 Hz and 520 Hz pure
;;; tones presented to each ear. For more details, see
;;; @url{https://en.wikipedia.org/wiki/Binaural_beats}.
;;;
;;; This module implements a set of user-facing commands for generating
;;; binaural beats. The commands are organized from high-level commands that
;;; play predefined binaural beats to lower-level commands that can be
;;; used to create new effect sequences.
;;;
;;; All binaural beat sequences are played with a relatively low gain
;;; --- they are designed to be heard in the background and when
;;; effective blend fully into the background. You can increase the
;;; overall volume of all binaural beat sequences by customizing
;;; @defvar {User Option} sox-binaural-gain-offset to a positive value
;;; @end defvar
;;; --- default is 0.
;;;
;;; @subsubsection High-Level Commands For Pre-Defined Binaural Beats
;;;
;;; These commands can be called directly to play one of the predefined
;;; binaural beats.
;;;
;;; @itemize
;;; @item @command{sox-rev-up}: A set of binaural beats designed for  use
;;; at the start of the day. Transitions from @emph{Dream} -> @emph{Think}
;;; ->@emph{Act} -> @emph{Focus}.
;;; @item @command{sox-wind-down}: A  set of binaural beats for winding down
;;; at the end of the day. This can be thought of as the reverse of
;;; @command{sox-rev-up} and the sequence transitions from @emph{Act} ->
;;; @emph{Think} -> @emph{Dream} ->@emph{Sleep}.
;;; @item @command{sox-turn-down}: Designed for falling asleep.
;;; This sequence starts with a short period of @emph{Dream} before moving
;;; to @emph{Sleep}.
;;; @item @command{sox-relax}:  A variant of the previous sequence,
;;; @code{sox-relax} spends equal time in @emph{Dream} and @emph{Sleep}.
;;; @item @command{sox-binaural}: Provide a completion-based front-end to
;;; playing any one of the predefined binaural effects (@emph{Delta},
;;; @emph{Theta}, @emph{Alpha}, @emph{Beta}, or @emph{Gamma}. The previously
;;; defined sequences are built up using these effects.
;;; @item @command{sox-beats-binaural}: Plays a collection of binaural
;;; beats, prompting for carrier and beat frequencies for each tone. The
;;; predefined sequences listed earlier were created after first generating
;;; experimental beat-sequences using this command.
;;; @item @command{sox-slide-binaural}: Prompts for two binaural effects
;;; (see above) and generates a binaural beat that @emph{slides} from the
;;; first effect to the second over a specified duration.
;;; @item @command{sox-chakras}: Pick  amongst one of a predefined set of
;;; sequences designed for @emph{Chakra} meditation.
;;; @item @command{sox-tone-binaural}: Generate a simple binaural beat
;;; with a single carrier frequency.
;;; @item @command{sox-tone-slide-binaural}: Generate a  tone that slides
;;; from one binaural beat to another.
;;; @end itemize
;;;
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))


;;}}}
;;{{{ sox-gen-p:

(defvar sox-gen-p
  (eval-when-compile (executable-find "sox"))
  "Is sox available?")

;;}}}
;;{{{ SoX Command Generator:
(defvar sox-gen-play (executable-find "play")
  "Location of play from SoX.")

(defun sox-gen-cmd (cmd)
  "Play specified command."
  (cl-declare (special sox-gen-play sox-gen-p))
  (when sox-gen-p
    (apply #'start-process "SoX" nil sox-gen-play  (split-string cmd))))

;;}}}
;;{{{ Binaural Audio:
(defcustom sox-binaural-gain-offset 0
  "User specified offset that is added to default gain when generating
tones using SoX, e.g., for binaural beats."
  :type 'number
  :group 'sox)

(defconst sox-binaural-cmd
  "-q -n synth %s sin %s sin %s gain %s channels 2 "
  "Command-line that produces a binaural beat.")


(defun sox-tone-binaural (length freq beat gain)
  "Play binaural audio with carrier frequency `freq', beat `beat', and
gain `gain'."
  (interactive
   (list
    (timer-duration(read-from-minibuffer "Duration: "))
    (read-number "Carrier Frequency [50 -- 800]: " 100)
    (read-number "Beat Frequency [0.5 -- 40]: " 4.5)
    (read-number "Gain [Use negative values]: " -18)))
  (cl-declare (special sox-binaural-cmd sox-binaural-gain-offset))
  (sox-gen-cmd
   (format
    sox-binaural-cmd length
    freq (+ freq beat)
    (+ gain sox-binaural-gain-offset))))


(defun sox-tone-slide-binaural (length freq beat-start beat-end  gain)
  "Play binaural audio with carrier frequency `freq', beat
`beat-start' -> `beat-end', and gain `gain'."
  (interactive
   (list
    (timer-duration(read-from-minibuffer "Duration: "))
    (read-number "Carrier Frequency [50 -- 800]: " 100)
    (read-number "Start Beat Frequency [0.5 -- 40]: " 4.5)
    (read-number "End Beat Frequency [0.5 -- 40]: " 0.5)
    (read-number "Gain [Use negative values]: " -18)))
  (cl-declare (special sox-binaural-cmd sox-binaural-gain-offset))
  (sox-gen-cmd
   (format
    sox-binaural-cmd
    length freq
    (format "%s:%s" (+ freq beat-start) (+ freq beat-end))
    (+ gain sox-binaural-gain-offset))))

(defconst sox-beats-binaural-cmd
  "-q -n synth %s %s gain %s channels 2 "
  "Command-line that produces multiple  binaural beats.")

(defun sox-read-binaural-beats ()
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


(defun sox-beats-binaural (length beat-spec-list  gain)
  "Play binaural audio with beat-spec specifying the various tones.
Param `beat-spec-list' is a list of `(carrier beat) tupples."
  (interactive
   (list
    (timer-duration(read-from-minibuffer "Duration: "))
    (sox-read-binaural-beats)
    (read-number "Gain [Use negative values]: " -18)))
  (cl-declare (special sox-beats-binaural-cmd sox-binaural-gain-offset))
  (unless beat-spec-list (error "No beats specified. "))
  (sox-gen-cmd
   (format
    sox-beats-binaural-cmd
    length
    (mapconcat
     #'(lambda (spec)
         (let ((f (cl-first spec))
               (b (cl-second spec)))
           (cond
            ((numberp  b)
             (format "sin %s sin %s" f (+ f b)))
            ((and (listp b) (numberp (cl-first b)) (numberp (cl-second b)))
             (format "sin %s sin %s"
                     f
                     (format "%s:%s"    ;slide
                             (+ f (cl-first b)) (+ f (cl-second b))))))))
     beat-spec-list " ")
    (+ gain sox-binaural-gain-offset))))

(cl-defstruct sox--binaural
  beats ; list of beat-specs
  gain ; overall gain
  )

;;; Helper:

(defun sox--binaural-play  (length binaural)
  "Plays an instance of sox-binaural."
  (sox-beats-binaural
   length
   (sox--binaural-beats  binaural)
   (+ (sox--binaural-gain binaural) sox-binaural-gain-offset)))

(defvar sox-binaural-effects-table (make-hash-table :test #'equal)
  "Hash table mapping binaural effect names to effect structures.")

(defun sox-define-binaural-effect   (name effect)
  "Setup mapping  from name to binaural effect."
  (cl-declare (special sox-binaural-effects-table))
  (puthash name effect sox-binaural-effects-table))

(defun sox-binaural-get-effect (name)
  "Return predefined effect."
  (cl-declare (special sox-binaural-effects-table))
  (or (gethash name sox-binaural-effects-table)
      (error "Effect not defined.")))
;;{{{  Define Effects:

;;; delta, theta, alpha, beta, gamma
;;; sleep, dream, think, act, focus

(sox-define-binaural-effect
 "sleep" ; delta
 (make-sox--binaural
  :beats '((75 0.5) (150 1.0) (225 2.0) (300 4.0))
  :gain -20))

(sox-define-binaural-effect
 "dream" ; theta
 (make-sox--binaural
  :beats '((75 4.5) (150 5.3) (225 6.3)  (300 7.83))
  :gain -18))

(sox-define-binaural-effect
 "think" ;alpha
 (make-sox--binaural
  :beats '((75 8.3) (150 9.0) (225 10.0) (300 12.0))
  :gain -10))

(sox-define-binaural-effect
 "act" ; beta
 (make-sox--binaural
  :beats '((75 13.5) (150 18.0) (225 23.0) (300 40.0))
  :gain -10))

(sox-define-binaural-effect
 "focus" ; gamma
 (make-sox--binaural
  :beats '((75 40.0) (150 40.0) (225 40.0) (300 40.0))
  :gain -20))

;; }}}

(defun sox--format-seconds (secs)
  "Audio-format seconds."
  (format-seconds "%H %M and%z %S" secs))

(defun sox-binaural (name duration)
  "Play specified binaural effect."
  (interactive
   (list
    (completing-read "Binaural Effect: " sox-binaural-effects-table
                     nil 'must-match)
    (timer-duration (read-from-minibuffer "Duration: "))))
  (sox--binaural-play duration (sox-binaural-get-effect name))
  (dtk-notify-say
   (format "%s: %s" name (sox--format-seconds duration))))


(defun sox-slide-binaural (name-1 name-2 duration)
  "Play specified binaural slide from `name-1' to `name-2'."
  (interactive
   (list
    (completing-read "Binaural Effect: " sox-binaural-effects-table nil 'match)
    (completing-read "Binaural Effect: " sox-binaural-effects-table nil 'match)
    (timer-duration (read-from-minibuffer "Duration: "))))
  (cl-declare (special sox-binaural-slider-scale))
  (let ((slide (sox--gen-slide-a->b name-1 name-2))
        (slope (/ duration  sox-binaural-slider-scale))
        (dur (* 2 (/ duration  sox-binaural-slider-scale))))
    (sox-binaural name-1 dur)
    (run-with-timer
     dur nil
     #'(lambda (n1 n2  d)
         (dtk-notify-say
          (format "%s  to %s %s" n1 n2 (sox--format-seconds d)))
         (sox--binaural-play  d slide))
     name-1 name-2 slope)
    (run-with-timer
     (+ dur slope) nil
     #'(lambda (n2 s)
         (sox-binaural n2 s))
     name-2 dur)))

(defun sox--gen-slide-a->b (a b)
  "Return a binaural  structure that slides from a to be."
  (let* ((a-effect (sox-binaural-get-effect a))
         (b-effect (sox-binaural-get-effect b))
         (a-beats (sox--binaural-beats a-effect))
         (b-beats (sox--binaural-beats b-effect)))
    (cl-assert (= (length a-beats) (length b-beats))
               t "Effects have different lengths. " a b)
    (make-sox--binaural
     :gain  (/ (+ (sox--binaural-gain a-effect)
                  sox-binaural-gain-offset
                  (sox--binaural-gain b-effect))
               2)
     :beats
     (cl-loop
      for i from 0 to (length a-effect)
      collect
      (let ((a-i (elt a-beats i))
            (b-i (elt b-beats i)))
        (list
         (/ (+ (cl-first a-i) (cl-first b-i)) 2) ; carrier frequency
         (list (cl-second a-i) (cl-second b-i))))))))

;;}}}

;;}}}
;;{{{ Defined Binaural Themes (Sequences):

(defconst sox-rev-up-beats
  '(("dream" 1) ("think"  4) ("act" 2) ("focus" 1))
  "List of  beats to use for rev-up in the morning.")

(defconst sox-wind-down-beats
  '(("think"3)("dream" 4) ("sleep" 1))
  "List of  beats to use for wind-down in the evening.")

(defconst sox-turn-down-beats
  '(("dream" 2) ("sleep" 6))
  "List of  beats to use for turn-down at  night.")

(defconst sox-relax-beats
  '(("dream" 4) ("sleep" 4))
  "List of  beats to use for relaxing.")

;;; Theme Helper:
(defcustom sox-binaural-slider-scale 5.0
  "Scale factor used to compute slide duration when moving from one
binaural beat to another."
  :type 'float
  :set #'(lambda (sym val)

           (set-default sym (float val)))
  :group 'sox)

(defun sox--theme-compute-length (theme scale)
  "Return  how long  this theme  invocation will run in seconds."
  (let  ((intervals (mapcar #'(lambda (th) (* scale (cl-second th))) theme))
         (result 0))
    (cl-loop for i in intervals do
             (cl-incf result i)
             (unless (eq  i (car (last intervals)))
               (cl-incf  result (/ i  sox-binaural-slider-scale))))
    result))

(defun
    sox--theme-duration-scale (theme duration)
  "Given a theme and a desired overall duration, compute duration scale."
  (cl-declare (special sox-binaural-slider-scale))
  (let ((steps (mapcar #'cl-second theme)))
    (/
     (timer-duration duration)
     (+ (apply #'+ steps)
        (* (/ 1   sox-binaural-slider-scale)
           (apply #'+ (butlast steps)))))))

(defun sox--theme-play (theme duration)
  "Play  set of  binaural beats specified in theme."
  (cl-declare (special sox-binaural-slider-scale))
  (let ((start 0)
        (dur-scale (sox--theme-duration-scale theme duration)))
    (dtk-notify-say
     (sox--format-seconds (sox--theme-compute-length theme dur-scale)))
    (cl-loop
     for beat in theme
     and i from 0 do
     (let* ((b (cl-first beat))
            (next (cl-first (elt theme (+ 1 i))))
            (end (* dur-scale  (cl-second beat)))
            (slider-start (+ start end))
            (slider-len (/ end sox-binaural-slider-scale)))
       (run-with-timer                  ; start now
        start nil                       ; no repeat
        #'(lambda (this then)
            (sox-binaural this then))
        b end)
       (setq start (+ start end))
;;; slider
       (when (and next (not (zerop slider-len)))
         (run-with-timer                ; start  at slider-start
          slider-start nil              ; no repeat
          #'(lambda (this that len)
              (dtk-notify-say
               (format "%s to %s %s"
                       this that (sox--format-seconds len)))
              (sox--binaural-play
               len
               (sox--gen-slide-a->b this that)))
          b next slider-len)
         (setq start (+ start slider-len)))))))

(defun sox-rev-up (length)
  "Play rev-up set of  binaural beats for `length' seconds. "
  (interactive "sDuration: ")
  (cl-declare (special sox-rev-up-beats))
  (sox--theme-play sox-rev-up-beats length))

(defun sox-turn-down (length)
  "Play turn-down set of  binaural beats for `length' seconds. "
  (interactive "sDuration: ")
  (cl-declare (special sox-turn-down-beats))
  (sox--theme-play sox-turn-down-beats length))

(defun sox-wind-down (length)
  "Play wind-down set of  binaural beats for `length' seconds."
  (interactive "sDuration: ")
  (cl-declare (special sox-wind-down-beats))
  (sox--theme-play sox-wind-down-beats length))

(defun sox-relax (length)
  "Play relax set of  binaural beats for `length' seconds."
  (interactive "sDuration: ")
  (cl-declare (special sox-relax-beats))
  (sox--theme-play sox-relax-beats length))

;;}}}
;;{{{ Chakra Themes:

;;; Default Theme For Chakras:
;;; From: https://www.youtube.com/watch?v=ARoih8HTPGw

(defconst sox--chakra-settings-0
  '(
    ("root" 228 8.0)
    ("navel" 303 9.0)
    ("solar-plexus" 182 10.0)
    ("heart" 128.3 10.5)
    ("throat" 192 12.0)
    ("3rd-eye" 144 13)
    ("crown" 216 15)
    )
  "Frequency settings.")

(cl-loop
 for s in sox--chakra-settings-0 do
 (sox-define-binaural-effect
  (cl-first s)
  (make-sox--binaural
   :beats `(,(cdr s))
   :gain -20)))

;;; Chakras: Set 1:Carrier frequencies taken from  the Web.
;;; https://sourceforge.net/p/sbagen/mailman/message/3047882/

(defconst sox--chakra-settings-1
  '(
    ("root" 256 7.83)
    ("navel" 288 7.83)
    ("solar-plexus" 320 7.83)
    ("heart" 341.3 7.83)
    ("throat" 384 7.83)
    ("3rd-eye" 426.7 7.83)
    ("crown" 480 7.83)
    )
  "Frequency settings.")

(cl-loop
 for s in sox--chakra-settings-1 do
 (sox-define-binaural-effect
  (cl-first s)
  (make-sox--binaural
   :beats `(,(cdr s))
   :gain -20)))

(defun sox-chakras (theme duration)
  "Play each chakra for specified duration.
Parameter `theme' specifies variant."
  (interactive
   (list
    (intern
     (completing-read  "Chakra Theme Variant: "
                       '("sox--chakra-settings-0" "sox--chakra-settings-1")
                       nil 'must-match
                       "sox--chakra-settings-"))
    (timer-duration (read-from-minibuffer "Chakra Duration: "))))
  (let ((names  (mapcar #'car (symbol-value theme)))
        (start 0))
    (cl-loop
     for name in names do
     (run-with-timer start nil #'(lambda (n) (sox-binaural n  duration)) name)
     (setq start (+ start duration)))))

;;}}}
;;{{{ synth:

(defconst sox-synth-cmd
  "-q -n synth %s "
  "Invoke synth generation.")

(defun sox-synth (length  &rest args)
  "Call synth with length and args."
  (cl-declare (special sox-synth-cmd))
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
  (cl-declare (special sox-sin-cmd))
  (sox-gen-cmd
   (concat
    (format sox-sin-cmd length freq)
    (mapconcat #'identity args " "))))

;;}}}
;;{{{ Pluck:

(defconst sox-pluck-cmd
  "-q -n synth %s pluck %s channels 2 "
  "Command-line that produces a simple plucke.")

(defun sox-pluck (length freq &rest args)
  "Play plucke  specified by length and freq.
Freq can be specified as a frequency, note (%nn) or frequency range."
  (cl-declare (special sox-pluck-cmd))
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


(defun sox-chime (&optional tempo speed)
  "Play chime --- optional args tempo and speed default to 1."
  (cl-declare (special sox-chime-cmd))
  (sox-gen-cmd
   (concat
    sox-chime-cmd
    (when tempo (format " tempo %s" tempo))
    (when speed (format " speed %s" speed)))))

;;}}}
;;{{{ Multiwindow:

(defconst sox-multiwindow-cmd
  "-q -n synth -j 3 \
%s %%-5 %s %%-2 \
fade h 0.2 0.7 0.5 \
delay 0.1 0.5 \
echo .9 .5 40 0.5 60 0.3 5 0.2 \
channels 2 tempo 1.3   gain -10"
  "Chime used to indicate multiple windows.
%s is an op --- either sin or pluck.")

(defun sox-multiwindow (&optional swap  speed op)
  "Produce a short note used to cue multiwindow."
  (cl-declare (special sox-multiwindow-cmd))
  (or op (setq op "sin"))
  (sox-gen-cmd
   (concat
    (format  sox-multiwindow-cmd op op )
    (when swap " swap ")
    (when speed (format " speed %s" speed)))))

;;}}}
;;{{{ scroll:

(defconst sox-do-scroll-up-cmd
  "-q -n synth pinknoise brownnoise \
fade 0.1 0.6 0.4 \
delay 0.1 0.5 \
tempo 2 channels 2"
  "A quick scroll sound.")

(defun sox-do-scroll-up (&optional  speed)
  "Produce a short do-scroll-up."
  (cl-declare (special sox-do-scroll-up-cmd))
  (sox-gen-cmd
   (concat
    sox-do-scroll-up-cmd
    (when speed (format " speed %s" speed)))))

(defconst sox-do-scroll-down-cmd
  "-q -n synth brownnoise pinknoise \
fade 0.1 0.6 0.4 \
delay 0.5 0.1 \
tempo 2 channels 2   "
  "A quick scroll sound.")

(defun sox-do-scroll-down (&optional speed)
  "Produce a short do-scroll-down."
  (cl-declare (special sox-do-scroll-down-cmd))
  (sox-gen-cmd
   (concat
    sox-do-scroll-down-cmd
    (when speed (format " speed %s" speed)))))

;;}}}
;;{{{ tone:

(defconst sox-tones-cmd
  "-q -n synth -j 3 \
sin %3 sin %-2 sin %-5 sin %-9 sin %-14 sin %-21 \
fade h .01 2 1.5 \
delay  1.3 1 .76 .54 .27 \
  norm -1 channels 2"
  "Command-line that produces a sequence of  tones.")

(defun sox-tones (&optional tempo speed)
  "Play sequence of tones --- optional args tempo and speed default to 1."
  (cl-declare (special sox-tones-cmd))
  (sox-gen-cmd
   (concat
    sox-tones-cmd
    (when tempo (format " tempo %s" tempo))
    (when speed (format " speed %s" speed)))))

;;}}}
;;{{{ Guitar Chord:

(defconst sox-guitar-chord-cmd
  "-q -n synth pl G2 pl B2 pl D3 pl G3 pl D4 pl G4 delay 0 .05 .1 .15 .2 .25 \
remix - fade 0 4 .1 norm -1 channels 2"
  "Play a guitar chord.")

(defun sox-guitar-chord (&optional tempo speed)
  "Play a guitar chord"
  (cl-declare (special sox-guitar-chord-cmd))
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
;;; end:

;;}}}
