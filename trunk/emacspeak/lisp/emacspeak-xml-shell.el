;;; emacspeak-xml-shell.el --- Implements a simple XML browser
;;; $Id$
;;; $Author$
;;; Description:  Contains  xml-shell
;;; Keywords: Emacspeak,  Audio Desktop Xml-Shell
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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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

;;{{{  introduction

;;; Use xmllint from package libxml2 to implement 
;;; an XML browser.
;;; Uses the interactive shell provided by xmllint to do the hard
;;; work.
;;; Results of traversal are transformed  using xsltproc from libxslt 

;;}}}
;;{{{  Required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'custom)
(require 'comint)
(require 'voice-lock)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-wizards)
(require 'derived)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-xml-shell nil
  "XML browser for the Emacspeak desktop.")

(defcustom emacspeak-xml-shell-command "xmllint"
  "Executable that provides the XML browser shell."
  :type 'string
  :group 'emacspeak-xml-shell)

(defcustom emacspeak-xml-shell-options
   (list "--shell"
        "--format"
        "--noent")
  "Command-line options for XML browse command."
  :type  '(repeat string)
:group 'emacspeak-xml-shell)

;;}}}
;;{{{ xml browse mode

(define-derived-mode emacspeak-xml-shell-mode comint-mode 
  "Browsing XML documents."
  XML Browse\n\n
\\{emacspeak-xml-shell-mode-map}")

;;}}}
;;{{{ Create and launch XML Browser

(defvar emacspeak-xml-shell-process nil
  "Handle to running XML browser process.")

(defvar emacspeak-xml-shell-hooks nil
  "Start up hooks run after XML browser  process is started.")

(defun emacspeak-xml-shell-start-process (system-id)
  "Launch Xml-Shell process."
  (declare (special emacspeak-xml-shell-process
                    emacspeak-xml-shell-hooks
                    emacspeak-xml-shell-command
                    emacspeak-xml-shell-options))
  (let ((buffer (apply 'make-comint "Xml-Shell"
                             emacspeak-xml-shell-command
                             nil
                             (append emacspeak-xml-shell-options
                                      (list system-id)))))
    (save-excursion
      (set-buffer buffer)
      (emacspeak-xml-shell-mode)
      (run-hooks 'emacspeak-xml-shell-hooks)
      (setq emacspeak-xml-shell-process
            (get-buffer-process buffer)))))

(defun emacspeak-xml-shell (system-id)
  "Start Xml-Shell on contents of system-id."
  (interactive
   (list
    (read-from-minibuffer
     "Browse XML: ")))
  (declare (special emacspeak-xml-shell-process))
  (unless
      (and (processp emacspeak-xml-shell-process)
      (eq 'run 
      (process-status  emacspeak-xml-shell-process)))
  (emacspeak-xml-shell-start-process system-id))
  (emacspeak-auditory-icon 'open-object)
  (switch-to-buffer (process-buffer
                     emacspeak-xml-shell-process))
  (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-xml-shell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
