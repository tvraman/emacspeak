;;; emacspeak-apt-utils.el --- speech-enable APT interface
;;; Description:  Emacspeak extension to speech-enable APT utilities
;;; Keywords: Emacspeak, apt, Debian Package Manager
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:

;;; Initial version: Author: Igor B. Poretsky <master@goga.energo.ru>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables apt-utils.el
;;; that is included in the debian-el package
;;; and provides a nice interface to searching and browsing
;;; Debian packages.

;;}}}
;;{{{

;;; Code:

(defsubst emacspeak-apt-utils-speak-package-name ()
  "Speak package name at point."
  (let ((package (apt-utils-package-at)))
    (put-text-property 0 (length package)
		       'personality (get-text-property (point) 'personality)
		       package)
    (dtk-speak package)))

(defadvice apt-utils-mode (after emacspeak pre act comp)
  "Setup Emacspeak extensions"
  (voice-lock-mode 1)
  (dtk-set-punctuations "all"))

;;}}}
;;{{{ Advice interactive commands to speak.

(defadvice apt-utils-show-package (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-choose-package-link (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-previous-package (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice apt-utils-search (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)))

(defadvice apt-utils-search-names-only (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)))

(defadvice apt-utils-search-grep-dctrl (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)))

(defadvice apt-utils-search-file-names (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)))

(defadvice apt-utils-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice apt-utils-kill-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice apt-utils-rebuild-package-lists (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice apt-utils-toggle-package-info (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))

(defadvice apt-utils-view-copyright (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-readme (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-debian-readme (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-news (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-debian-news (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-changelog (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-view-debian-changelog (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-follow-link (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice apt-utils-previous-package (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-apt-utils-speak-package-name)))

(defadvice apt-utils-next-package (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-apt-utils-speak-package-name)))

;;}}}
;;{{{ mapping font faces to personalities 

(def-voice-font emacspeak-apt-normal-package-personality voice-bolden
  'apt-utils-normal-package-face
  "Personality for apt-utils-normal-package-face")

(def-voice-font emacspeak-apt-utils-virtual-package-personality voice-animate
  'apt-utils-virtual-package-face
  "Personality for apt-utils-virtual-package-face")

(def-voice-font emacspeak-apt-utils-field-keyword-personality voice-animate-extra
  'apt-utils-field-keyword-face
  "Personality for apt-utils-field-keyword-face")

(def-voice-font emacspeak-apt-utils-field-contents-personality voice-lighten-extra
  'apt-utils-field-contents-face
  "Personality for apt-utils-field-contents-face")

(def-voice-font emacspeak-apt-utils-description-personality voice-smoothen-extra
  'apt-utils-description-face
  "Personality for apt-utils-description-face")

(def-voice-font emacspeak-apt-utils-version-personality voice-lighten
  'apt-utils-version-face
  "Personality for apt-utils-version-face")

(def-voice-font emacspeak-apt-utils-broken-personality voice-bolden-and-animate
  'apt-utils-broken-face
  "Personality for apt-utils-broken-face")

;;}}}
(provide 'emacspeak-apt-utils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
