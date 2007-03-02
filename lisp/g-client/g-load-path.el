;;; g-load-path.el -- Setup Emacs load-path for compiling grep
;;; $Id: g-load-path.el 4158 2006-08-31 03:16:54Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Sets up load-path for g-client compilation
;;; Keywords: g-client, Google services  for Emacs
;;{{{  LCD Archive entry:
;;; LCD Archive Entry:
;;; g-client| T. V. Raman |raman@cs.cornell.edu
;;; An Emacs client for Google services
;;; $Date: 2006-08-30 20:16:54 -0700 (Wed, 30 Aug 2006) $ |
;;;  $Revision$ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 2006--2007, T. V. Raman
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
(defvar g-directory
  (and load-file-name
       (file-name-directory load-file-name))
  "Directory where g-client  is built. ")

(unless (member g-directory load-path )
  (setq load-path
        (cons g-directory load-path )))

(setq byte-compile-warnings
      '(redefine callargs free-vars
                 unresolved obsolete))

(provide 'g-load-path)
