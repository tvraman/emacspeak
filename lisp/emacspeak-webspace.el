;;; emacspeak-webspace.el --- Webspaces In Emacspeak
;;; $Id: emacspeak-webspace.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  WebSpace provides smart updates from the Web.
;;; Keywords: Emacspeak,  Audio Desktop webspace
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNWEBSPACE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; WEBSPACE ==  Smart Web Gadgets For The Emacspeak Desktop

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-webutils)
(require 'emacspeak-we)

;;}}}
;;{{{ WebSpace Speaker:
(defun emacspeak-webspace-display (infolet)
  "Displays specified infolet.
Infolets use the same structure as mode-line-format and header-line-format.
Display includes auditory and visual display."
  (declare (special header-line-format))
  (setq header-line-format infolet)
  (dtk-speak (format-mode-line header-line-format))
(emacspeak-auditory-icon 'progress))

(declaim (special emacspeak-webspace-keymap))
(loop for k in
      '(
        ("w" emacspeak-webspace-weather)
        ("h" emacspeak-webspace-headlines)
        )
      do
      (define-key emacspeak-webspace-keymap (first k) (second k)))

;;}}}
;;{{{ CNN Headlines:

(defvar emacspeak-webspace-cnn-us-headlines-command-template
  (when (executable-find "xmlstarlet")
    "xmlstarlet sel --net -t -m //item/title  -v . --nl \
http://rss.cnn.com/rss/cnn_us.rss"
    )
  "Command line that gives us CNN US news headlines.")

(defvar emacspeak-webspace-cnn-us-headlines (make-ring 10)
  "Ring of CNN USheadlines.")

(defun emacspeak-webspace-cnn-us-headlines-get ()
  "Populate a ring of CNN US headlines."
  (declare (special emacspeak-webspace-cnn-us-headlines-command-template
                    emacspeak-webspace-cnn-us-headlines))
  (when emacspeak-webspace-cnn-us-headlines-command-template
    (mapc
     #'(lambda (h)
         (ring-insert emacspeak-webspace-cnn-us-headlines
                      h))
    (split-string
     (shell-command-to-string emacspeak-webspace-cnn-us-headlines-command-template)
     "\n"))))

(defvar emacspeak-webspace-cnn-us-headlines-timer nil
  "Timer holding our CNN headlines update timer.")

(defun emacspeak-webspace-update-cnn-us-headlines (period)
  "Setup periodic CNN updates.
Period is specified as documented in function run-at-time.
Updated headlines  found in ring  `emacspeak-webspace-cnn-us-headlines"
  (interactive "sUpdate Frequencey: ")
  (declare (special emacspeak-webspace-cnn-us-headlines
                    emacspeak-webspace-cnn-us-headlines-timer ))
  (emacspeak-webspace-cnn-us-headlines-get)
  (setq emacspeak-webspace-cnn-us-headlines-timer
        (run-at-time  period nil
                      'emacspeak-webspace-update-cnn-us-headlines)))

(defun  emacspeak-webspace-headlines ()
  "Speak current headline."
  (interactive)
  (declare (special emacspeak-webspace-cnn-us-headlines))
  (emacspeak-webspace-display (ring-remove emacspeak-webspace-cnn-us-headlines)))

;;}}}
;;{{{ Weather Wizard:

(defvar emacspeak-webspace-weather-command-template
  (when (executable-find "xmlstarlet")
    "xmlstarlet sel --net -t -v '//item[1]/title' \
http://www.wunderground.com/auto/rss_full/%s.xml")
  "Command line that gives us weather conditions as a short string.")

(defun emacspeak-webspace-weather-conditions  ()
  "Return weather conditions for `emacspeak-url-template-weather-city-state'."
  (declare (special emacspeak-url-template-weather-city-state
                    emacspeak-webspace-weather-command-template))
  (when (and emacspeak-webspace-weather-command-template
             emacspeak-url-template-weather-city-state)
    (substring
     (shell-command-to-string
      (format emacspeak-webspace-weather-command-template
              emacspeak-url-template-weather-city-state))
     0 -1)))

(defvar emacspeak-webspace-current-weather nil
  "Holds cached value of current weather conditions.")

(defvar emacspeak-webspace-weather-timer nil
  "Timer holding our weather update timer.")

(defun emacspeak-webspace-update-weather (period)
  "Setup periodic weather updates.
Period is specified as documented in function run-at-time.
Updated weather is found in `emacspeak-webspace-current-weather'."
  (interactive "sUpdate Frequencey: ")
  (declare (special emacspeak-webspace-current-weather
                    emacspeak-webspace-weather-timer ))
  (unless emacspeak-url-template-weather-city-state
    (error
     "First set option emacspeak-url-template-weather-city-state to your city/state."))
  (setq emacspeak-webspace-current-weather
        (emacspeak-webspace-weather-conditions))
  (setq emacspeak-webspace-weather-timer
        (run-at-time  period nil
                      'emacspeak-webspace-update-weather)))

(defun  emacspeak-webspace-weather ()
  "Speak current weather."
  (interactive)
  (declare (special emacspeak-webspace-current-weather))
  (emacspeak-webspace-display 'emacspeak-webspace-current-weather))

;;}}}
(provide 'emacspeak-webspace)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
