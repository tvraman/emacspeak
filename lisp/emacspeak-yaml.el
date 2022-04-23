;;; emacspeak-yaml.el --- Speech-enable YAML  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable YAML An Emacs Interface to yaml
;; Keywords: Emacspeak,  Audio Desktop yaml
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNYAML FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; YAML == Yet Another Markup Language
;; This module speech-enables yaml-mode.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:

'(
  yaml-electric-backspace
  yaml-electric-bar-and-angle

  yaml-narrow-to-block-literal
  )

(defadvice yaml-indent-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice yaml-mode (after emacspeak pre act comp)
  "speak."
  (unless emacspeak-audio-indentation
    (emacspeak-toggle-audio-indentation)))

(defadvice yaml-fill-paragraph (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'fill-object)))

(defadvice yaml-electric-backspace (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (dtk-tone-deletion)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice yaml-electric-bar-and-angle (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice yaml-electric-dash-and-dot (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

;;}}}
(provide 'emacspeak-yaml)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
