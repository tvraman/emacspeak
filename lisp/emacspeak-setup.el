;;; emacspeak-setup.el --- Setup Emacspeak environment --loaded to start Emacspeak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  File for setting up and starting Emacspeak
;;; Keywords: Emacspeak, Setup, Spoken Output
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-06-06 19:00:23 -0700 (Fri, 06 Jun 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;{{{ Introduction

;;; Commentary:
;;; Entry point for Emacspeak.
;;; The simplest and most basic way to start emacspeak is:
;;; emacs -q -l <emacspeak-dir>/lisp/emacspeak-setup.el
;;; The above starts a vanilla Emacs with just Emacspeak loaded.
;;; Once the above has been verified to work,
;;; You can  add
;;; (load-library "emacspeak-setup")
;;; To your .emacs file.
;;; See tvr/emacs-startup.el in the Emacspeak Git repository for  my setup.

;;; Code:

;;}}}
;;{{{Load-path:

(push
 (expand-file-name "../lisp" (file-name-directory load-file-name))
 load-path)
(require 'emacspeak-preamble)
(unless noninteractive
  (let ((file-name-handler-alist nil)
        (load-source-file-function nil))
    (load  "emacspeak-loaddefs")))

;;}}}
;;; Start emacspeak if emacs   is interactive:
(unless noninteractive (emacspeak))
(provide 'emacspeak-setup)
;;{{{  emacs local variables

;;; local variables:
;;; mode: emacs-lisp
;;; folded-file: t
;;; end:

;;}}}
