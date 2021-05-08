;;; emacspeak-eaf.el --- Speech-enable EAF  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EAF An Emacs Interface to eaf
;;; Keywords: Emacspeak,  Audio Desktop eaf
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
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNEAF FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EAF ==  Emacs Application Framework
;;; https://github.com/manateelazycat/emacs-application-framework.git 
;;; This module speech-enables the elisp side of EAF.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:
'(
  eaf--kill-python-process
eaf-browser-restore-buffers
eaf-browser-send-esc-or-exit-fullscreen
eaf-create-mindmap
eaf-describe-bindings
eaf-edit-buffer-cancel
eaf-edit-buffer-confirm
eaf-edit-buffer-switch-to-org-mode
eaf-edit-mode
eaf-elfeed-open-url
eaf-file-browser-qrcode
eaf-file-sender-qrcode
eaf-file-sender-qrcode-in-dired
eaf-generate-keymap-doc
eaf-get-path-or-url
eaf-goto-left-tab
eaf-goto-right-tab
eaf-import-chrome-bookmarks
eaf-import-firefox-bookmarks
eaf-install-dependencies
eaf-interleave-add-file-url
eaf-interleave-add-note
eaf-interleave-app-mode
eaf-interleave-mode
eaf-interleave-open-notes-file
eaf-interleave-quit
eaf-interleave-sync-current-note
eaf-interleave-sync-next-note
eaf-interleave-sync-pdf-page-current
eaf-interleave-sync-previous-note
eaf-keyboard-quit
eaf-kill-process
eaf-mode
eaf-open
eaf-open-airshare
eaf-open-bookmark
eaf-open-browser
eaf-open-browser-with-history
eaf-open-camera
eaf-open-demo
eaf-open-external
eaf-open-ipython
eaf-open-jupyter
eaf-open-mail-as-html
eaf-open-mindmap
eaf-open-music
eaf-open-office
eaf-open-terminal
eaf-open-this-buffer
eaf-open-url-at-point
eaf-open-vue-demo
eaf-pdf-outline
eaf-pdf-outline-jump
eaf-pdf-outline-mode
eaf-restart-process
eaf-search-it
eaf-send-alt-backspace-sequence
eaf-send-ctrl-return-sequence
eaf-send-down-key
eaf-send-key
eaf-send-key-sequence
eaf-send-left-key
eaf-send-return-key
eaf-send-right-key
eaf-send-second-key-sequence
eaf-send-shift-return-sequence
eaf-send-up-key
eaf-share-path-or-url
eaf-stop-process
eaf-toggle-fullscreen
)


;;}}}
(provide 'emacspeak-eaf)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
