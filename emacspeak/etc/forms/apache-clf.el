;;; emacspeak/forms/apache-clf.el --- Speech friendly
;;; interface to apache logs (CLF)
;;;$Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable  forms
;;; Keywords: forms
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
;;;Copyright (C) 1995, 1996, 1997, 1998   T. V. Raman  
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  setup forms

(setq forms-read-only t)
(setq forms-file
      (read-file-name  "Log file: "
                       "/var/httpd/logs/"
                       "/etc/httpd/logs/access_log"))


(setq forms-read-file-filter 'emacspeak-forms-flush-unwanted-records)

(setq forms-field-sep " ")
(setq forms-number-of-fields 15)
;;; CLF: 
;;; host ident authuser date request status bytes
;;; plus referer plus agent == combined 
(setq forms-format-list
      (list
 "Request: "6 7 8 
"\n"
 "From: "1
"\n"
 "Date: "4 5
"\n"
"Referer: " 11
"\t"
"Agent: " 12 13 14 
"status: " 9
"bytes: " 10
"\n"
"ident: " 2
"authuser: " 3
       ))

;;}}}

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
