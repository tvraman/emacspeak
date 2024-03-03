;;; emacspeak-empv.el --- Speech-enable EMPV  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EMPV An Emacs Interface to empv
;;; Keywords: Emacspeak,  Audio Desktop empv
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
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

(defadvice empv-exit (after emacspeak pre act comp)
  "Icon."
  (when (ems-interactive-p)
    (dtk-stop 'all)
    (emacspeak-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice empv-youtube-tabulated (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)))

;;; Additional Commands:

(defvar emacspeak-empv-history nil
  "Youtube history for EMpv.")

(defvar emacspeak-empv-history-max 16
  "Max number of history to preserve.")

;;;###autoload
(defun emacspeak-empv-play-url (url &optional left)
  "Play URL using mpv.
Interactive prefix arg plays on left ear. "
  (interactive (list (emacspeak-eww-read-url 'emacspeak-empv-history)
                     current-prefix-arg))
  (cl-declare (special emacspeak-empv-history-max
                       emacspeak-empv-history empv-mpv-args))
  (when
      (and url (stringp url)
           (string-prefix-p (emacspeak-google-result-url-prefix) url))
    (setq url  (emacspeak-google-canonicalize-result-url url)))
  (add-to-history 'emacspeak-empv-history url emacspeak-empv-history-max)
  (let* ((args (copy-sequence empv-mpv-args))
         (empv-mpv-args args))
    (when left (push "--audio-channels=fl" empv-mpv-args))
    (empv-play url)))

(defun emacspeak-empv-play-last (&optional left)
  "Play most recently played URL."
  (interactive "P")
  (cl-declare (special emacspeak-empv-history))
  (emacspeak-empv-play-url (cl-first emacspeak-empv-history) left))

(declare-function emacspeak-media-local-resource "emacspeak-empv" t)
(declare-function emacspeak-media-read-resource
                  "emacspeak-m-player" (&optional prefix))

;;;###autoload
(defun emacspeak-empv-play-file (file &optional left)
  "Play file using mpv.
Interactive prefix arg plays on left ear using alsa."
  (interactive(list (emacspeak-media-read-resource)
                    current-prefix-arg))
  (cl-declare (special empv-mpv-args))
  (let* ((args (copy-sequence empv-mpv-args))
         (empv-mpv-args args))
    (when left (push "--audio-channels=fl" empv-mpv-args))
    (empv-play file)))

(put 'emacspeak-empv-play-file 'repeat-map 'empv-map)
(put 'emacspeak-empv-play-url 'repeat-map 'empv-map)
(put 'emacspeak-empv-play-last 'repeat-map 'empv-map)

(defsubst emacspeak-empv-local-file ()
  "Return local media filename read with completion."
  (let (( default-directory empv-audio-dir))
    (emacspeak-media-local-resource nil)))

(defun emacspeak-empv-play-local (file )
  "Play a local resource  using mpv."
  (interactive (list (emacspeak-empv-local-file)))
  (empv-play file))

(put 'emacspeak-empv-play-local 'repeat-map 'empv-map)

(defun emacspeak-empv-accumulate-to-register ()
  "Accumulate media links to register u"
  (interactive)
  (emacspeak-accumulate-to-register ?u
                                    'empv-youtube-results--current-video-url))
(declare-function emacspeak-eww-yt-dl "emacspeak-eww" (url))

;;;###autoload
(defun emacspeak-empv-yt-download ()
  "Download Youtube result."
  (interactive)
  (emacspeak-eww-yt-dl (empv-youtube-results--current-video-url)))

;;; Seekers:
(defun emacspeak-empv-time-pos ()
  "Speak time and percent position."
  (interactive)
  (empv--let-properties '(time-pos percent-pos)
    (message "%s %.2d%%"
             (ems--format-clock (or .time-pos 0))
              (or .percent-pos 0))))

(defsubst emacspeak-empv-post-nav ()
  "Post nav action"
  (when (called-interactively-p 'interactive)
    (call-interactively 'emacspeak-empv-time-pos)
    (emacspeak-icon 'tick-tick)))

(defun emacspeak-empv-relative-seek (target)
  "Relative seek in seconds,see `empv-seek'"
  (interactive "nTarget:")
  (empv-seek target)
  (emacspeak-empv-post-nav))

(defun emacspeak-empv-absolute-seek (target)
  "Absolute seek in seconds,see `empv-seek'"
  (interactive "nTarget:")
  (empv-seek target '("absolute"))
  (emacspeak-empv-post-nav))

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
      (funcall-interactively 'emacspeak-empv-backward-minute
                             ,duration))))


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

(defun emacspeak-empv-setup ()
  "Emacspeak setup for empv."
  (cl-declare (special empv-map))
  (global-set-key (kbd "s-SPC") empv-map)
  (cl-loop
   for b in
   '(
     ("%" emacspeak-empv-percentage-seek)
     ("'" empv-current-loop-on)
     ("." emacspeak-empv-toggle-custom)
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
     ("C-j" empv-youtube-results-play-current)
     ("DEL" emacspeak-empv-clear-filter)
     ("M" emacspeak-empv-backward-minute)
     ("RET" empv-youtube-tabulated)
     ("SPC" empv-toggle)
     ("m" emacspeak-empv-forward-minute)
     ("r" emacspeak-empv-relative-seek)
     ("s" emacspeak-empv-absolute-seek)
     ("u" emacspeak-empv-accumulate-to-register)
     ("v" empv-set-volume)
     ("x" empv-exit)
     ("y" emacspeak-empv-yt-download))
   do
   (emacspeak-keymap-update empv-map b))
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'empv-map)))
   empv-map))

(emacspeak-empv-setup)

(defvar emacspeak-empv-filter-history nil
  "History of filters used.")
(defconst emacspeak-empv-filters
  '(
    "asubboost" "bs2b" "bs2b=cmoy" "bs2b=jmeier"
    "extrastereo" "extrastereo=1.5" "haas" "headphone"
    "stereowiden=4.25:.1:735:.8" "surround=7.1" "virtualbass"
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

(provide 'emacspeak-empv)
;;;  end of file

                                        ;
                                        ;
                                        ;
