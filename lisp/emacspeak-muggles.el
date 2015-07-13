;;; emacspeak-muggles.el --- Convenience Hydras For The Emacspeak Desktop
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MUGGLES An Emacs Interface to muggles
;;; Keywords: Emacspeak,  Audio Desktop muggles
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNMUGGLES FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; MUGGLES ==  Keybinding Conveniences For The Emacspeak Desktop.

;;; This module implements no new functionality --- contrast with emacspeak-wizards.
;;; Instead, it uses package hydra  to provide convenience key-bindings that access  existing Emacspeak functionality.

;;; Usage: M-x load-library emacspeak-muggles.
;;; You need to install package Hydra first:
;;; M-x package-install  hydra.

;;; Implemented Hydras:

;;; Brightness: <print> Control display brightness using xbacklight.

;;;View-Mode: <C-z> Temporarily behave like view-mode.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(when (locate-library "package")
  (unless (locate-library "hydra") (package-install 'hydra)))
(require 'hydra)

;;}}}
;;{{{ Emacspeak Helpers:

(defun emacspeak-muggles-pre ()
  "Provide auditory icon"
  (emacspeak-auditory-icon 'open-object))

(defun emacspeak-muggles-post ()
  "Provide auditory icon"
  (emacspeak-auditory-icon 'close-object))

;;}}}
;;{{{ Brightness:

(defhydra
  emacspeak-muggles-brightness
  (global-map "<print>"
              :pre emacspeak-muggles-pre
              :post emacspeak-muggles-post)
  "Brightness"
  ("i" xbacklight-increment "brighter")
  ("SPC" xbacklight-increment "brighter")
  ("d" xbacklight-decrement "dimmer")
  ("g" xbacklight-get "Get")
  ("s" xbacklight-set "set")
  ("0" (xbacklight-set 0) "black")
  ("<print>" (xbacklight-set 0) "black")
  ("1" (xbacklight-set 100) "white"))

;;}}}
;;{{{  Biew Mode:

(defhydra
  emacspeak-muggles-view
  (
   global-map "C-z"
   :pre emacspeak-muggles-pre :post emacspeak-muggles-post :color amaranth)
  "View Mode"
  ("l" forward-char)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("m" set-mark-command "mark")
  ("a" move-beginning-of-line "beg")
  ("e" move-end-of-line "end")
  ("y" kill-ring-save "yank" :color blue)
  ("p" View-search-last-regexp-backward)
  ("n" View-search-last-regexp-forward)
  ("\\" View-search-regexp-backward)
  ("/" View-search-regexp-forward)
  ("r" isearch-backward)
  ("s" isearch-forward)
  ("m" point-to-register)
  ("'" register-to-point)
  ("x" exchange-point-and-mark)
  ("@" View-back-to-mark)
  ("." set-mark-command)
  ("g" View-goto-line)
  ("=" what-line)
  ("u" View-scroll-half-page-backward)
  ("d" View-scroll-half-page-forward)
  ("SPC" View-scroll-page-forward)
  ("o" View-scroll-to-buffer-end)
  (">" end-of-buffer)
  ("<" beginning-of-buffer)
  ("[" previous-page)
  ("]" next-page)
  ("{" backward-paragraph)
  ("}" forward-paragraph)
  ("(" backward-sexp)
  (")" forward-sexp)

  ("q" nil "quit"))

;;}}}
;;{{{ Org-Mode Table Navigation:

;;}}}
(provide 'emacspeak-muggles)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
