;;; emacspeak-dictionary.el --- speech-enable dictionaries  -*- lexical-binding: t; -*- 
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:   Speech enable dictionary mode
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2021, T. V. Raman<tv.raman.tv@gmail.com>
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:
;;; Commentary:
;;; Speech-enables emacs client for accessing dictionary
;;; server at dict.org:2628
;;; Code:
;;}}}
;;{{{ Advice interactive commands to speak.
(defadvice dictionary (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
(defadvice dictionary-close (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice dictionary-select-dictionary (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Selected dictionary")))
(defadvice dictionary-select-strategy (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Selected strategy")))

(defadvice dictionary-search (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-speak-line)))
(defadvice dictionary-lookup-definition (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-speak-line)))

(defadvice dictionary-match-words (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-speak-line)))

(defadvice dictionary-previous (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))
(defadvice dictionary-prev-link (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-range 'link-function)))

(defadvice dictionary-next-link (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-range 'link-function)))

;;}}}
(provide 'emacspeak-dictionary)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
