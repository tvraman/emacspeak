;;; emacspeak-daisy.el --- daisy Front-end for emacspeak desktop
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for DAISY Talking Books
;;; Keywords: Emacspeak, daisy Digital Talking Books
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; Commentary:

;;; Daisy Digital Talking Book Reader

;;}}}
;;{{{ required modules

;;; Code:

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)
(require 'derived)
(require 'xml-parse)
(eval-when-compile (require 'emacspeak-keymap))

;;}}}
;;{{{  Customization variables

(defgroup emacspeak-daisy nil
  "Daisy Digital Talking Books  for the Emacspeak desktop."
  :group 'emacspeak)

;;}}}
;;{{{  helpers

;;}}}
;;{{{  emacspeak-daisy mode

(declaim (special emacspeak-daisy-mode-map))

(define-derived-mode emacspeak-daisy-mode text-mode 
  "Major mode for Daisy Digital Talking Books.\n"
  " An DAISY front-end for the Emacspeak desktop.

Pre-requisites:

0) mpg123 for playing mp3 files
1) libxml and libxslt packages 
2) xml-parse.el for parsing XML in Emacs Lisp.


The Emacspeak DAISY front-end is launched by command
emacspeak-daisy bound to \\[emacspeak-daisy].  

This command switches to a special buffer that has DAISY
commands bounds to single keystrokes-- see the ke-binding
list at the end of this description.  Use Emacs online help
facility to look up help on these commands.

emacspeak-daisy-mode provides the necessary functionality to
navigate and listen to Daisy talking books. 

Here is a list of all emacspeak DAISY commands along with their key-bindings:

\\{emacspeak-daisy-mode-map}"
  (progn
    (emacspeak-keymap-remove-emacspeak-edit-commands emacspeak-daisy-mode-map)))

(define-key emacspeak-daisy-mode-map "?" 'describe-mode)

;;}}}
;;{{{ interactive commands

;;}}}
(provide 'emacspeak-daisy)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
