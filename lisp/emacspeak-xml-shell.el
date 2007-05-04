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
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
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

(require 'emacspeak-preamble)
(require 'comint)
;;}}}
;;{{{ Customizations
;;;###autoload
(defgroup emacspeak-xml-shell nil
  "XML browser for the Emacspeak desktop."
  :group 'emacspeak)

(defcustom emacspeak-xml-shell-command "xmllint"
  "Executable that provides the XML browser shell.
Default is xmllint.
If you want an XML Shell on steroids get XSH and use emacs custom to
customize the default to be xsh."
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

(defcustom emacspeak-xml-shell-hooks nil
  "Start up hooks run after XML browser  process is started."
  :type 'hook
  :group 'emacspeak-xml-shell)

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
                               (if (string-match ".html?$" system-id)
                                   (list "--html")
                                 nil )
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
;;;###autoload
(defun emacspeak-xml-shell (system-id)
  "Start Xml-Shell on contents of system-id."
  (interactive
   (list
    (if (and (processp emacspeak-xml-shell-process)
             (eq 'run 
                 (process-status  emacspeak-xml-shell-process)))
        emacspeak-xml-shell-document
      (read-from-minibuffer
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

(defvar emacspeak-xml-shell-display-buffer nil
  "Buffer that displays processed output.")

(defun emacspeak-xml-shell-create-accumulator (accumulate terminator post-processor)
  "Create a function that is suitable for use as a filter function for
the XML shell process. The returned function will accumulate process
output into `accumulate' --a stream or buffer. Accumulation stops when
the accumulator encounters value specified by terminator.
When accumulation is done, post-processor is called to process the
content.
Post-processor accepts a region of text to process specified by start
and end."
  (declare (special emacspeak-xml-shell-display-buffer))
  (`
   (lambda (process output)
     (let ((stream  (, accumulate))
           (processor (function (, post-processor)))
           (done  (, terminator)))
       (save-excursion
         (set-buffer stream)
         (goto-char (point-max))
         (cond
          ((string-match done output)
           (insert output)
           (goto-char (point-min))
           (kill-line)
           (goto-char (point-max))
           (beginning-of-line)
           (kill-line)
           (set-process-filter process 'comint-output-filter)
           (funcall processor
                    (point-min) (point-max))
           (kill-buffer stream)
           (save-excursion
             (set-buffer (process-buffer process))
             (goto-char (point-max))
             (comint-send-input)))
          (t (insert output))))))))

(defvar emacspeak-xml-shell-cat
  (cond
   ((string-equal "xmllint" emacspeak-xml-shell-command)
    "cat")
   ((string-equal emacspeak-xml-shell-command "xsh")
    "ls")
   (t ""))
  "Set according to the XML Shell in use.")

(defun emacspeak-xml-shell-process-node ( xpath display-function)
  "Apply display-function to the contents of node specified by xpath.
Display function accepts two arguments, start and end that specify the
region of text to process."
  (declare (special emacspeak-xml-shell-process
                    emacspeak-xml-shell-cat))
  (let ((accumulator nil)
        (terminator nil)
        (accumulate (get-buffer-create "*xml-shell-accumulator*")))
    (save-excursion
      (set-buffer accumulate)
      (erase-buffer))
    (save-excursion
      (set-buffer (process-buffer emacspeak-xml-shell-process))
      (goto-char (process-mark emacspeak-xml-shell-process))
      (setq terminator (thing-at-point 'line))
      (setq accumulator (emacspeak-xml-shell-create-accumulator
                         accumulate  terminator 
                         display-function))
      (set-process-filter emacspeak-xml-shell-process accumulator)
      (insert (format "%s %s"
                      emacspeak-xml-shell-cat
                      xpath))
      (comint-send-input))))

(defcustom emacspeak-xml-shell-xslt nil
  "XSL transform to apply when displaying current node."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Command"))
  :group 'emacspeak-xml-shell)

(defsubst emacspeak-xml-shell-setup-html-base (base)
  "Locate HTML head in current buffer and add document base.  Creates
HTML head if none found."
  (goto-char (point-min))
  (let ((head   (search-forward "<head>" nil t)))
    (or head
        (insert "<head>\n"))
    (insert
     (format "<base href=\"%s\">\n" base))
    (or head (insert "</head>\n"))))

(defun emacspeak-xml-shell-display-as-html (start end)
  "Suitable for use in displaying current node as HTML."
  (declare (special emacspeak-xml-shell-xslt))
  (when emacspeak-xml-shell-xslt
    (emacspeak-xslt-region
     emacspeak-xml-shell-xslt
     start end))
  (emacspeak-xml-shell-setup-html-base
   emacspeak-xml-shell-document)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-------" nil t)
      (replace-match "<br>")))
  (browse-url-of-buffer)
  (setq emacspeak-xml-shell-display-buffer (current-buffer)))

(defun emacspeak-xml-shell-browse-current ()
  "Display current node."
  (interactive)
  (declare (special emacspeak-xml-shell-display-buffer))
  (emacspeak-xml-shell-process-node "."
                                    'emacspeak-xml-shell-display-as-html)
  (switch-to-buffer emacspeak-xml-shell-display-buffer))

(defun emacspeak-xml-shell-browse-result (xpath)
  "Display XPath  and display its result using W3."
  (interactive "sXPath:")
  (declare (special emacspeak-xml-shell-display-buffer))
  (emacspeak-xml-shell-process-node xpath 
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
(define-key emacspeak-xml-shell-mode-map "\C-c\C-v"
  'emacspeak-xml-shell-browse-result)
(define-key emacspeak-xml-shell-mode-map "\C-cv" 'emacspeak-xml-shell-browse-current)

;;}}}
(provide 'emacspeak-xml-shell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
