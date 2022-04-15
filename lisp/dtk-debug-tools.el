;;; emacspeak-dtk-debug-tools.el ---  DTK-DEBUG-TOOLS  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable DTK-DEBUG-TOOLS An Emacs Interface to dtk-debug-tools
;;; Keywords: Emacspeak,  Audio Desktop dtk-debug-tools
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
;;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNDTK-DEBUG-TOOLS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; DTK-DEBUG-TOOLS ==  Debug tools for Emacspeak's TTS layer

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{dtk-debug-speak-buffer

(defun dtk--debug-speak-buffer (text)
  "Return the buffer that dtk-speak would have created as its scratch buffer."
  (cl-declare (special 
               tts-strip-octals
               emacspeak-use-auditory-icons
               dtk-speech-rate dtk-speak-nonprinting-chars
               dtk-speak-treat-embedded-punctuations-specially
               dtk-quiet dtk-chunk-separator-syntax
               voice-lock-mode dtk-punctuation-mode
               dtk-split-caps dtk-caps
               emacspeak-pronounce-pronunciation-table
               selective-display))
;;; ensure text is a  string
  (unless (stringp text) (setq text (format "%s" text)))
;;; If you dont want me to talk,or my server is not running,
;;; I will remain silent.
;;; I also do nothing if text is nil or ""
  (unless
      (or dtk-quiet (not dtk-speak-server-initialized)
          (null text) (zerop (length text)))
    (when selective-display
      (let ((ctrl-m (string-match "\015" text)))
        (and ctrl-m (setq text (substring text 0 ctrl-m)))))
    (let ((inhibit-point-motion-hooks t) ;snapshot relevant state
          (inhibit-read-only t)
          (inhibit-modification-hooks t)
          (deactivate-mark nil)
          (invisibility-spec buffer-invisibility-spec)
          (syntax-table (syntax-table))
          (pronunciation-table emacspeak-pronounce-pronunciation-table)
          (inherit-chunk-separator-syntax dtk-chunk-separator-syntax)
          (inherit-speak-nonprinting-chars dtk-speak-nonprinting-chars)
          (inherit-strip-octals tts-strip-octals)
          (complement-separator (dtk-complement-chunk-separator-syntax))
          (caps dtk-caps)
          (split-caps dtk-split-caps)
          (inherit-enable-multibyte-characters enable-multibyte-characters)
          (tts-scratch-buffer (get-buffer-create "*TTS-Debug*"))
          (start 1)
          (end nil)
          (mode dtk-punctuation-mode)
          (voice-lock voice-lock-mode))
      (with-current-buffer tts-scratch-buffer
        (setq buffer-undo-list t)
        (erase-buffer)
;;; inherit environment
        (setq
         buffer-invisibility-spec invisibility-spec
         dtk-chunk-separator-syntax inherit-chunk-separator-syntax
         dtk-punctuation-mode mode
         dtk-split-caps split-caps
         dtk-caps caps
         
         dtk-speak-nonprinting-chars inherit-speak-nonprinting-chars
         tts-strip-octals inherit-strip-octals
         voice-lock-mode voice-lock)
        (set-syntax-table syntax-table)
        (set-buffer-multibyte inherit-enable-multibyte-characters)
        (insert-for-yank text)
        (dtk--delete-invisible-text)
        
        
        
        
        (goto-char (point-min))
        (skip-syntax-forward " ")       ;skip leading whitespace
        tts-scratch-buffer))))

;;}}}

(provide 'emacspeak-dtk-debug-tools)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
