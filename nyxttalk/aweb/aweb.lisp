;;;   -*- Syntax: Common-Lisp; Package: tts ; Mode: LISP -*-    ;;;
;;; aweb.lisp -- Speech-Enable Nyxt Browser
;;; $Author: tv.raman.tv $
;;; Description: Speech-enable nyxt via  Common Lisp bridge to Emacspeak TTS servers
;;; Keywords: Next, Emacspeak, Audio Desktop
;;{{{ Copyright:

;;; Copyright (C) -2020, T. V. Raman<tv.raman.tv@gmail.com>
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Introduction:

;;; Commentary:
;;; Speech-enable nyxt.

;;}}}
;;{{{Package:
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))
(in-package :cl-user)

(defpackage :aweb (:use :common-lisp :tts)
            )

(in-package :aweb)

;;}}}
;;{{{Grab Document Body:

(in-package :nyxt)
(defun  write-html-to-temp ()
  "dump html to /tmp"
  (within-renderer-thread 
    (with-result
        (body (with-current-buffer (current-buffer) (document-get-body)))
      (with-open-file  (out "/tmp/dump.html" :direction :output) 
        (write-string body out)))))

;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
