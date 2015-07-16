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
;;; Note that on newer versions of Emacs, loading this module will attempt to automatically install package hydra if it is not found.

;;; Implemented Hydras:

;;;@itemize
;;; @item Brightness: <print> Control display brightness using xbacklight.
;;; @item View-Mode: <C-z> Temporarily behave like view-mode.
;;;@item  org-mode tables: <C-c t> Table UI for org-mode tables.
;;;@item m-player: Super-M Emacspeak-M-Player Commands
;;; @item hideshow C-c h hideshow
;;; @item toggle-option <C-c o> Toggle options
;;;@end itemize

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(when (locate-library "package")
  (unless (locate-library "hydra") (package-install 'hydra)))
(require 'hydra)
(require 'xbacklight)
(require 'view)
(require 'org)
(require 'emacspeak-m-player)

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

(defhydra emacspeak-muggles-brightness
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
;;{{{  View Mode:

(defhydra emacspeak-muggles-view
  (global-map "C-z"
              :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
  "View Mode"
  ("$" set-selective-display)
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
  ("A"beginning-of-defun )
  ("E"end-of-defun )
  ("a" move-beginning-of-line "beg")
  ("b" backward-word)
  ("c" emacspeak-speak-char)
  ("d" View-scroll-half-page-forward)
  ("e" move-end-of-line "end")
  ("f" forward-word)
  ("g" View-goto-line)
  ("h" backward-char)
  ("i" emacspeak-speak-mode-line)
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
  ("w"kill-ring-save)
  ("x" exchange-point-and-mark)
  ("y" kill-ring-save "yank")
  ("{" backward-paragraph)
  ("}" forward-paragraph)
  )

;;}}}
;;{{{ Org-Mode Table Navigation:

(defhydra emacspeak-muggles-org-table
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
;;{{{ Media Player:

(defhydra emacspeak-muggles-m-player
  (emacspeak-super-keymap "m"
                          :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
  (";" emacspeak-m-player)
  ("+" emacspeak-m-player-volume-up)
  ("," emacspeak-m-player-backward-10s)
  ("-" emacspeak-m-player-volume-down)
  ("." emacspeak-m-player-forward-10s)
  ("<" emacspeak-m-player-backward-1min)
  ("<down>" emacspeak-m-player-forward-1min)
  ("<end>" emacspeak-m-player-end-of-track)
  ("<home>" emacspeak-m-player-beginning-of-track)
  ("<left>" emacspeak-m-player-backward-10s)
  ("<next>" emacspeak-m-player-forward-10min)
  ("<prior>" emacspeak-m-player-backward-10min)
  ("<right>" emacspeak-m-player-forward-10s)
  ("<up>" emacspeak-m-player-backward-1min)
  ("=" emacspeak-m-player-volume-up)
  (">" emacspeak-m-player-forward-1min)
  ("?" emacspeak-m-player-display-position)
  ("C" emacspeak-m-player-clear-filters)
  ("C-m" emacspeak-m-player-load)
  ("DEL" emacspeak-m-player-reset-speed)
  ("L" emacspeak-m-player-load-file)
  ("M-l" emacspeak-m-player-load-playlist)
  ("O" emacspeak-m-player-reset-options)
  ("P" emacspeak-m-player-apply-reverb-preset)
  ("Q" emacspeak-m-player-quit)
  ("R" emacspeak-m-player-edit-reverb)
  ("S" emacspeak-amark-save)
  ("SPC" emacspeak-m-player-pause)
  ("[" emacspeak-m-player-slower)
  ("]" emacspeak-m-player-faster)
  ("a" emacspeak-m-player-amark-add)
  ("b" emacspeak-m-player-balance)
  ("c" emacspeak-m-player-slave-command)
  ("d" emacspeak-m-player-delete-filter)
  ("e" emacspeak-m-player-add-equalizer)
  ("f" emacspeak-m-player-add-filter)
  ("g" emacspeak-m-player-seek-absolute)
  ("j" emacspeak-m-player-amark-jump)
  ("l" emacspeak-m-player-get-length)
  ("m" emacspeak-m-player-speak-mode-line)
  ("n" emacspeak-m-player-next-track)
  ("o" emacspeak-m-player-customize-options)
  ("p" emacspeak-m-player-previous-track)
  ("q" bury-buffer)
  ("r" emacspeak-m-player-seek-relative)
  ("s" emacspeak-m-player-scale-speed)
  ("t" emacspeak-m-player-play-tracks-jump)
  ("u" emacspeak-m-player-url)
  ("v" emacspeak-m-player-volume-change)
  ("(" emacspeak-m-player-left-channel)
  (")" emacspeak-m-player-right-channel)
  ("{" emacspeak-m-player-half-speed)
  ("}" emacspeak-m-player-double-speed)
  )

;;}}}
;;{{{ HideShow:

(defhydra  emacspeak-muggles-hideshow
  (global-map "C-c h"
              :pre emacspeak-muggles-pre :post emacspeak-muggles-post :color blue)
  "Hideshow"
  ("h" hs-hide-block)
  ("s" hs-show-block)
  ("H" hs-hide-all)
  ("S" hs-show-all)
  ("i" hs-hide-initial-comment-block))

;;}}}
;;{{{ Option Toggle

;;; Cloned from hydra-examples.el and modified to tase.

(defhydra emacspeak-muggles-toggle-option
  (global-map "C-c o" :color blue)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_g_ debug-on-quit:    %`debug-on-quit
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("g"  toggle-debug-on-quit  nil)
  ("t" toggle-truncate-lines nil)
  ("w" whitespace-mode nil)
  ("q" nil "quit"))

;;}}}
(provide 'emacspeak-muggles)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
