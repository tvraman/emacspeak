;;; emacspeak-ivy.el --- Speech-enable IVY  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable IVY An Emacs Interface to ivy
;;; Keywords: Emacspeak,  Audio Desktop ivy
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
;;; MERCHANTABILITY or FITNIVY FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; IVY ==  One More Smart Completion Technique 
;;; Speech-enable ivy-style completion.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
(ivy-action voice-animate)
(ivy-confirm-face voice-bolden)
(ivy-current-match voice-lighten)
(ivy-cursor voice-smoothen)
(ivy-match-required-face voice-bolden-extra)
(ivy-minibuffer-match-face-1 voice-monotone)
(ivy-minibuffer-match-face-2 voice-monotone-medium)
(ivy-minibuffer-match-face-3 voice-monotone-light)
(ivy-minibuffer-match-face-4 voice-monotone)
(ivy-modified-buffer voice-bolden-and-animate)
(ivy-remote voice-lighten)
(ivy-subdir voice-smoothen)
(ivy-virtual voice-animate)))

;;}}}
;;{{{ Interactive Commands:

'(
ivy-alt-done
ivy-avy
ivy-backward-delete-char
ivy-backward-kill-word
ivy-beginning-of-buffer
ivy-bibtex
ivy-bibtex-with-local-bibliography
ivy-call
ivy-delete-char
ivy-dispatching-call
ivy-dispatching-done
ivy-dispatching-done-hydra
ivy-done
ivy-end-of-buffer
ivy-forward-char
ivy-help
ivy-historian-mode
ivy-imenu-anywhere
ivy-immediate-done
ivy-insert-current
ivy-kill-line
ivy-kill-ring-save
ivy-kill-word
ivy-minibuffer-grow
ivy-minibuffer-shrink
ivy-mode
ivy-next-action
ivy-next-history-element
ivy-next-line
ivy-next-line-and-call
ivy-next-line-or-history
ivy-occur
ivy-occur-click
ivy-occur-dispatch
ivy-occur-grep-mode
ivy-occur-mode
ivy-occur-next-line
ivy-occur-press
ivy-occur-press-and-switch
ivy-occur-previous-line
ivy-occur-read-action
ivy-occur-revert-buffer
ivy-occur-toggle-calling
ivy-pages
ivy-partial
ivy-partial-or-done
ivy-pop-view
ivy-prev-action
ivy-previous-history-element
ivy-previous-line
ivy-previous-line-and-call
ivy-previous-line-or-history
ivy-push-view
ivy-read-action
ivy-recentf
ivy-restrict-to-matches
ivy-resume
ivy-reverse-i-search
ivy-rotate-sort
ivy-scroll-down-command
ivy-scroll-up-command
ivy-switch-buffer
ivy-switch-buffer-other-window
ivy-toggle-calling
ivy-toggle-case-fold
ivy-toggle-fuzzy
ivy-toggle-ignore
ivy-toggle-regexp-quote
ivy-wgrep-change-to-wgrep-mode
ivy-xcdoc-search-api
ivy-xcdoc-search-api-at-point
ivy-yank-word
ivy-youtube
)

;;}}}
(provide 'emacspeak-ivy)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
