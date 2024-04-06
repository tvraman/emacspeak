;;; emacspeak-empv.el --- Speech-enable EMPV  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $s-mo
;; Description:  Speech-enable EMPV An Emacs Interface to empv
;; Keywords: Emacspeak,  Audio Desktop empv
;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; Location https://github.com/tvraman/emacspeak

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



;;; Commentary:
;; EMPV ==  Emacs Front-End To mpv --- the GNU media player
;; Provides better Youtube integration
;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'empv nil t)
(require 'iimage nil t)
(declare-function emacspeak-google-canonicalize-result-url
                  "emacspeak-google" (url))
(declare-function emacspeak-google-result-url-prefix "emacspeak-google" nil)

;;; Interactive Commands:

(cl-loop
 for f in
 '(
   aempv-current-loop-off empv-current-loop-on
   empv-toggle empv-pause
   empv-file-loop-off empv-file-loop-on
   empv-playlist-loop-off empv-playlist-loop-on) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (dtk-stop 'all)
       (emacspeak-icon 'button)))))

(defadvice empv-youtube-results-play-current (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p) (emacspeak-icon 'button)))

(defadvice empv-youtube-results-inspect (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (emacspeak-speak-mode-line)
    ))

(defadvice empv-youtube-tabulated (before emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'button)
    ))

(defadvice empv-exit (after emacspeak pre act comp)
  "Icon."
  (repeat-exit)
  (when (ems-interactive-p)
    (dtk-stop 'all)
    (emacspeak-icon 'close-object)))

;;; Additional Commands:

(defvar emacspeak-empv-history nil
  "Youtube history for EMpv.")

(defvar emacspeak-empv-history-max 16
  "Max number of history to preserve.")

;;;###autoload
(defun emacspeak-empv-play-url (url)
  "Play URL using mpv. "
  (interactive (list (ems--read-url 'emacspeak-empv-history)))
  (cl-declare (special emacspeak-empv-history-max
                       emacspeak-empv-history))
  (when
      (and url (stringp url)
           (string-prefix-p (emacspeak-google-result-url-prefix) url))
    (setq url  (emacspeak-google-canonicalize-result-url url)))
  (add-to-history 'emacspeak-empv-history url emacspeak-empv-history-max)
  (empv-play url))

(defadvice empv-play (before emacspeak pre act comp)
  "Record history."
  (cl-declare (special emacspeak-empv-history-max
                       emacspeak-empv-history))
  (let ((url (ad-get-arg 0)))
    (when
        (and url (stringp url)
             (string-prefix-p (emacspeak-google-result-url-prefix) url))
      (setq url  (emacspeak-google-canonicalize-result-url url)))
    (add-to-history 'emacspeak-empv-history url emacspeak-empv-history-max)))

(defun emacspeak-empv-play-last ()
  "Play most recently played URL."
  (interactive )
  (cl-declare (special emacspeak-empv-history))
  (emacspeak-empv-play-url (cl-first emacspeak-empv-history)))

(declare-function emacspeak-media-local-resource "emacspeak-empv" t)
(declare-function emacspeak-media-read-resource
                  "emacspeak-m-player" (&optional prefix))

;;;###autoload
(defun emacspeak-empv-play-file (file &optional _prefix)
  "Play file using mpv.
Interactive prefix arg plays directory.
If already playing, then read an empv key and invoke its command."
  (interactive
   (list
    (unless (and empv--process (process-live-p empv--process))
      (emacspeak-media-read-resource current-prefix-arg))
    current-prefix-arg))
  (cl-declare (special  empv--process))
  (cond
   ((null file)                         ; we're already playing
    (call-interactively
     (lookup-key  empv-map  (read-key-sequence "EMpv Key:"))))
   (t (dtk-notify-speak (file-name-base file))
      (empv-play file))))

(defun emacspeak-empv-radio ()
  "Play Internet stream"
  (interactive)
  (emacspeak-empv-play-file
   (let ((default-directory emacspeak-media-shortcuts))
     (emacspeak-media-read-resource))))
(defun emacspeak-empv-accumulate-to-register ()
  "Accumulate media links to register u"
  (interactive)
  (emacspeak-accumulate-to-register ?u
                                    'empv-youtube-results--current-video-url))
(declare-function emacspeak-eww-yt-dl "emacspeak-eww" (url))

;;; Seekers:
(defun emacspeak-empv-time-pos ()
  "Speak time and percent position."
  (interactive)
  (empv--let-properties '(time-pos percent-pos)
    (message "%s.  %.2d%%"
             (ems--format-clock (or .time-pos 0))
             (or .percent-pos 0))))

(defsubst emacspeak-empv-post-nav ()
  "Post nav action"
  (when (called-interactively-p 'interactive)
    (call-interactively 'emacspeak-empv-time-pos)
    (emacspeak-icon 'tick-tick)))

(defun emacspeak-empv-relative-seek (target)
  "Relative seek in seconds,see `empv-seek'"
  (interactive (list (read-number "Target:" 30 )))
  (empv-seek target)
  (emacspeak-empv-post-nav))

(defun emacspeak-empv-absolute-seek (target)
  "Absolute seek in seconds,see `empv-seek'"
  (interactive "nTarget:")
  (empv-seek target '("absolute"))
  (emacspeak-empv-post-nav))

(defun emacspeak-empv-backward-10-seconds (&optional count)
  "Move back  count  slices of 10 seconds."
  (interactive "n")
  (or count (setq count 1))
  (empv-seek (* count -10))
  (emacspeak-empv-post-nav))

(defun emacspeak-empv-forward-10-seconds (&optional count)
  "Move forward count  chunks of 10 seconds."
  (interactive "p")
  (or count (setq count 1))
  (empv-seek (* count 10))
  (emacspeak-empv-post-nav))

;; Generate other navigators:

(defun emacspeak-empv-backward-minute (&optional count)
  "Move back  count  minutes."
  (interactive "p")
  (or count (setq count 1))
  (empv-seek (* count -60))
  (emacspeak-empv-post-nav))

(defun emacspeak-empv-forward-minute (&optional count)
  "Move forward count  minutes."
  (interactive "p")
  (or count (setq count 1))
  (empv-seek (* count 60))
  (emacspeak-empv-post-nav))

;; Generate other navigators:

(defun ems--empv-gen-nav (duration)
  "Generate time navigator."
  (eval
   `(defun ,(intern  (format "emacspeak-empv-forward-%s-minutes" duration)) ()
      ,(format "Move forward by %s minutes" duration )
      (interactive )
      (funcall-interactively 'emacspeak-empv-forward-minute ,duration)))
  (eval
   `(defun ,(intern  (format "emacspeak-empv-backward-%s-minutes" duration)) ()
      ,(format "Move backward by %s minutes" duration )
      (interactive )
      (funcall-interactively 'emacspeak-empv-backward-minute ,duration))))

;; Use it:
(mapc #'ems--empv-gen-nav '(5 10 30))

(defun emacspeak-empv-percentage-seek (target)
  "Percentage seek in seconds,see `empv-seek'"
  (interactive "nTarget:")
  (empv-seek target '("absolute-percent"))
  (when (called-interactively-p 'interactive)
    (call-interactively 'emacspeak-empv-time-pos)
    (emacspeak-icon 'button)))

;;; Setup:
(add-hook
 'empv-youtube-results-mode-hook
 #'(lambda nil
     (emacspeak-icon 'open-object)
     (emacspeak-pronounce-refresh-pronunciations)
     (dtk-notify-speak
      (format "%s results" (length empv--last-youtube-candidates)))))

(defun emacspeak-empv-setup ()
  "Emacspeak setup for empv."
  (cl-declare (special empv-map
                       empv-youtube-results-mode-map))
  (define-key empv-youtube-results-mode-map
              "o" 'empv-youtube-results-play-current)
  (cl-loop
   for b in
   '(
     ("%" emacspeak-empv-percentage-seek)
     ("'" empv-current-loop-on)
     ("," emacspeak-empv-toggle-left)
     ("." emacspeak-empv-toggle-right)
     ("\\" emacspeak-empv-toggle-custom)
     ("<" emacspeak-empv-backward-10-seconds)
     (">" emacspeak-empv-forward-10-seconds)
     ("0" empv-volume-up)
     ("9" empv-volume-down)
     (";" emacspeak-empv-toggle-filter)
     ("<down>" emacspeak-empv-forward-10-minutes)
     ("<left>" emacspeak-empv-backward-5-minutes)
     ("<next>" emacspeak-empv-forward-30-minutes)
     ("<prior>" emacspeak-empv-backward-30-minutes)
     ("<right>" emacspeak-empv-forward-5-minutes)
     ("<up>" emacspeak-empv-backward-10-minutes)
     ("=" emacspeak-empv-time-pos)
     ("DEL" emacspeak-empv-clear-filter)
     ("M" emacspeak-empv-backward-minute)
     ("SPC" empv-toggle)
     ("b" emacspeak-empv-toggle-balance)
     ("l" empv-lyrics-current)
     ("m" emacspeak-empv-forward-minute)
     ("r" emacspeak-empv-relative-seek)
     ("s" emacspeak-empv-absolute-seek)
     ("v" empv-set-volume)
     ("y" emacspeak-empv-yt-download))
   do
   (emacspeak-keymap-update empv-map b)))

(emacspeak-empv-setup)

;; Repeat:
(mapc
 #'(lambda (c) (put c 'repeat-map 'empv-map))
 '(
   empv-youtube-results-play-current
   empv-set-volume empv-display-current  empv-toggle
   emacspeak-empv-play-last emacspeak-empv-play-url
   emacspeak-empv-radio emacspeak-empv-play-file
   emacspeak-empv-play-local
   emacspeak-empv-backward-10-seconds emacspeak-empv-forward-10-seconds
   emacspeak-empv-forward-minute emacspeak-empv-backward-minute
   emacspeak-empv-forward-5-minutes emacspeak-empv-backward-5-minutes
   emacspeak-empv-forward-10-minutes emacspeak-empv-backward-10-minutes
   emacspeak-empv-forward-15-minutes emacspeak-empv-backward-15-minutes
   emacspeak-empv-forward-30-minutes emacspeak-empv-backward-30-minutes
   emacspeak-empv-time-pos emacspeak-empv-clear-filter
   emacspeak-empv-toggle-custom emacspeak-empv-toggle-filter
   emacspeak-empv-toggle-left emacspeak-empv-toggle-right
   emacspeak-empv-absolute-seek  emacspeak-empv-percentage-seek
   emacspeak-empv-relative-seek))


(defvar emacspeak-empv-filter-history nil
  "History of filters used.")
(defconst emacspeak-empv-filters
  '(
    "asubboost" "bs2b" "bs2b=cmoy" "bs2b=jmeier"
    "extrastereo" "extrastereo=1.5" "haas" "headphone"
    "stereowiden=4.25:.1:735:.8"
    "stereotools=mutel=true"
    "stereotools=muter=true"
    "surround=7.1" "virtualbass"
    )
  "Table of MPV filters.")

;;;  Toggling Filters
(defun emacspeak-empv-toggle-filter (filter)
  "Toggle Filter.
Filter is of the  form name=arg-1:arg-2:..."
  (interactive
   (list
    (completing-read   "Filter:"
                       emacspeak-empv-filters nil nil nil
                       'emacspeak-empv-filter-history)))
  (cl-declare (special emacspeak-empv-filter-history))
  (cl-pushnew filter emacspeak-empv-filter-history :test #'string=)
  (empv--send-command (list "af" "toggle" filter)))

(defun emacspeak-empv-toggle-balance (value)
  "Set balance to value --- range is -1.0..1.0 "
  (interactive (list (read-minibuffer "Balance: ")))
  (funcall-interactively #'emacspeak-empv-toggle-filter
                         (format "stereotools=balance_out=%f" value)))

(defun emacspeak-empv-clear-filter ()
  "Clear all filters. "
  (interactive)
  (empv--send-command (list "af" "clr" "" ))
  (message "Cleared filters")
  (emacspeak-icon 'delete-object))

(defcustom emacspeak-empv-custom-filters
  '("extrastereo" "stereowiden=4.25:.1:735:.8")
  "List of custom filters to turn on/off at one shot
The default value is suitable for classical instrumental music."
  :type '(repeat  :tag "Filters" (string :tag "Filter"))
  :group 'emacspeak-empv)

(defun emacspeak-empv-toggle-custom ()
  "Toggle our custom filters."
  (interactive)
  (cl-declare (special emacspeak-empv-custom-filters))
  (when emacspeak-empv-custom-filters
    (mapc
     #'(lambda (filter) (empv--send-command (list "af" "toggle" filter)))
     emacspeak-empv-custom-filters)
    (emacspeak-icon 'button)
    (message "Toggled custom filters")))

(defun emacspeak-empv-toggle-left ()
  "Toggle output to being just on the left."
  (interactive)
  (empv--send-command (list "af" "toggle" "stereotools=muter=true"))
  (emacspeak-icon 'button)
  (message "Toggled output left"))

(defun emacspeak-empv-toggle-right ()
  "Toggle output to being just on the right."
  (interactive)
  (empv--send-command (list "af" "toggle" "stereotools=mutel=true"))
  (emacspeak-icon 'button)
  (message "Toggled output right"))

(provide 'emacspeak-empv)
;;;  end of file

                                        ;
                                        ;
                                        ;
