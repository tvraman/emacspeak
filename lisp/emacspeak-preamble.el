;;; emacspeak-preamble.el --- standard  include for Emacspeak modules  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; DescriptionEmacspeak extensions for auctex-mode
;;; Keywords:emacspeak, audio interface to emacs AUCTEX
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Required modules 

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
;(require 'auth-source)
;(require 'url-parse)
(require 'advice)
(setq ad-redefinition-action 'accept)
(require 'emacspeak-load-path)
(require 'voice-setup)
(require 'dtk-speak)
(require 'emacspeak-pronounce)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-keymap)
(eval-when-compile
  (condition-case nil
      (package-initialize)
    (error "Check your elpa packages --- something is broken")))

;;}}}

(provide  'emacspeak-preamble)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
