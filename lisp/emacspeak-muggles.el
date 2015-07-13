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

;;; org-mode tables: <C-c t> Table UI for org-mode tables.

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
  ("'" register-to-point)
  ("(" backward-sexp)
  (")" forward-sexp)
  ("." set-mark-command)
  ("/" View-search-regexp-forward)
  ("<" beginning-of-buffer)
  ("=" what-line)
  (">" end-of-buffer)
  ("@" View-back-to-mark)
  ("SPC" View-scroll-page-forward)
  ("[" previous-page)
  ("\\" View-search-regexp-backward)
  ("]" next-page)
  ("a" move-beginning-of-line "beg")
  ("d" View-scroll-half-page-forward)
  ("e" move-end-of-line "end")
  ("g" View-goto-line)
  ("h" backward-char)
  ("j" next-line)
  ("k" previous-line)
  ("l" forward-char)
  ("m" point-to-register)
  ("n" View-search-last-regexp-forward)
  ("p" View-search-last-regexp-backward)
  ("q" nil "quit")
  ("r" isearch-backward)
  ("s" isearch-forward)
  ("u" View-scroll-half-page-backward)
  ("x" exchange-point-and-mark)
  ("y" kill-ring-save "yank" :color blue)
  ("{" backward-paragraph)
  ("}" forward-paragraph)
  )

;;}}}
;;{{{ Org-Mode Table Navigation:

(defhydra
  emacspeak-muggles-org-table
  (
   org-mode-map "C-c t"
   :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
  "Org Table UI"
  ("j" org-table-next-row)
  ("k" org-table-previous-row)
  ("h" org-table-previous-field)
  ("l" org-table-next-field)
  ("SPC"emacspeak-org-table-speak-current-element)
  ("."emacspeak-org-table-speak-coordinates)
  ("b"emacspeak-org-table-speak-both-headers-and-element)
  ("r"emacspeak-org-table-speak-row-header-and-element)
  ("c"emacspeak-org-table-speak-column-header-and-element))

;;}}}
(provide 'emacspeak-muggles)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
