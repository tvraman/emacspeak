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
  "XML Shell "
  "XML Shell\n\n
Interactive XML browser.
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
(defvar emacspeak-xml-shell-document
  nil
  "System ID of document being browsed.")

(defun emacspeak-xml-shell-process-sentinel  (proc status)
  "Process sentinel for XML shell."
  (declare (special emacspeak-xml-shell-document))
  (setq emacspeak-xml-shell-document nil))

(defun emacspeak-xml-shell (system-id)
  "Start Xml-Shell on contents of system-id."
  (interactive
   (list
    (if (and (processp emacspeak-xml-shell-process)
      (eq 'run 
      (process-status  emacspeak-xml-shell-process)))
emacspeak-xml-shell-document
    (read-file-name
     "Browse XML: "))))
  (declare (special emacspeak-xml-shell-process
emacspeak-xml-shell-document))
  (unless (string-match "^http:" system-id)
    (setq system-id (expand-file-name system-id)))
  (unless
      (and (processp emacspeak-xml-shell-process)
      (eq 'run 
      (process-status  emacspeak-xml-shell-process)))
  (emacspeak-xml-shell-start-process system-id)
  (setq emacspeak-xml-shell-document system-id))
  (set-process-sentinel emacspeak-xml-shell-process 'emacspeak-xml-shell-process-sentinel)
  (emacspeak-auditory-icon 'open-object)
  (switch-to-buffer (process-buffer
                     emacspeak-xml-shell-process))
  (emacspeak-speak-mode-line))
  
   
;;}}}
;;{{{ Navigate the tree

(defun emacspeak-xml-shell-navigate (xpath)
  "Navigate to the node specified by xpath."
  (declare (special emacspeak-xml-shell-process))
  (save-excursion
    (set-buffer (process-buffer emacspeak-xml-shell-process))
    (goto-char (point-max))
    (insert
   (format "cd %s"
           xpath))
    (comint-send-input))
  (when (eq (current-buffer)
             (process-buffer emacspeak-xml-shell-process))
    (goto-char (process-mark emacspeak-xml-shell-process))))

(defun emacspeak-xml-shell-goto-children()
  "Navigate down to the children of current node."
  (interactive)
  (emacspeak-xml-shell-navigate "./*[1]"))

(defun emacspeak-xml-shell-goto-parent()
  "Navigate up to the parent of current node."
  (interactive)
  (emacspeak-xml-shell-navigate ".."))

(defun emacspeak-xml-shell-goto-next-child()
  "Navigate forward  to the next child  of current node."
  (interactive)
  (emacspeak-xml-shell-navigate "(following-sibling::*)[1]"))
(defun emacspeak-xml-shell-goto-previous-child()
  "Navigate backward  to the previous child  of current node."
  (interactive)
  (emacspeak-xml-shell-navigate "(preceding-sibling::*)[1]"))

;;}}}
;;{{{ showing nodes

(defun emacspeak-xml-shell-process-node ( xpath display-function)
  "Apply display-function to the contents of node specified by xpath.
Display function accepts two arguments, start and end that specify the
region of text to process."
  (declare (special emacspeak-xml-shell-process))
  (let ((start nil)
        (end nil))
    (save-excursion
      (set-buffer (process-buffer emacspeak-xml-shell-process))
      (goto-char (point-max))
      (insert (format "cat %s" xpath))
      (comint-send-input)
      (accept-process-output  emacspeak-xml-shell-process)
      (sit-for 1)
      (goto-char (marker-position comint-last-input-end))
      (forward-line 1)
      (setq start (point))
      (goto-char (marker-position (process-mark emacspeak-xml-shell-process)))
      (setq end (point))
      (funcall display-function start end)
      (set-buffer (process-buffer emacspeak-xml-shell-process))
      (delete-region
       (max (point-min)start)
       (min (point-max) end)))))


(defvar emacspeak-xml-shell-xslt nil
  "XSL transform to apply to processed node.")

(defun emacspeak-xml-shell-transform-current (start end)
  "Display current node."
  (declare (special emacspeak-xml-shell-xslt))
  (let ((output-buffer (get-buffer-create  "*XML Shell Output*")))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (insert-buffer-substring 
       (process-buffer emacspeak-xml-shell-process)
                               start end)
      (emacspeak-xslt-region emacspeak-xml-shell-xslt
                             (point-min) (point-max)))
    (switch-to-buffer output-buffer)))

(defun emacspeak-xml-shell-current ()
  "Display current node."
  (interactive)
  (emacspeak-xml-shell-process-node "."
                                    'emacspeak-xml-shell-display-as-html))

;;}}}
;;{{{ keybindings

(declaim (special emacspeak-xml-shell-mode-map))
(define-key emacspeak-xml-shell-mode-map [left] 
'emacspeak-xml-shell-goto-previous-child)
(define-key emacspeak-xml-shell-mode-map [right] 
'emacspeak-xml-shell-goto-next-child)
(define-key emacspeak-xml-shell-mode-map [up] 
'emacspeak-xml-shell-goto-parent)
(define-key emacspeak-xml-shell-mode-map [down] 
'emacspeak-xml-shell-goto-children)

;;}}}
(provide 'emacspeak-xml-shell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
