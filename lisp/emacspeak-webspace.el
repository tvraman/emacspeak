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
(defvar emacspeak-webspace-weather-updates-timer nil
  "Timer holding our weather update timer.")

(defun emacspeak-webspace-setup-weather-updates ()
  "Setup periodic weather updates.
Updated weather is found in `emacspeak-webspace-current-weather'."
  (interactive)
  (declare (special emacspeak-webspace-current-weather
                    emacspeak-webspace-weather-updates-timer ))
  (unless emacspeak-url-template-weather-city-state
    (error
     "First set option emacspeak-url-template-weather-city-state to your city/state."))
  (setq emacspeak-webspace-current-weather
        (emacspeak-webspace-weather-conditions))
  (setq emacspeak-webspace-weather-updates-timer
        (run-at-time  "1 hour" nil
                      'emacspeak-webspace-setup-weather-updates)))

;;}}}
(provide 'emacspeak-webspace)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
