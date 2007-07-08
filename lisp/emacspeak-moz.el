;;; emacspeak-moz.el.el --- Talk to Firefox via MozRepl
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Control Firefox from Emacs
;;; Keywords: Emacspeak,  Audio Desktop Firefox
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

;;; Commentary:

;;; MozRepl provides a read-eval-print loop into Firefox
;;; This module provides convenient functions for driving MozRepl
;;; See http://repo.hyperstruct.net/mozlab

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'browse-url)
(require 'emacspeak-webutils)

;;}}}
;;{{{ Customizations

(defgroup emacspeak-moz nil
  "Control Firefox from Emacs."
  :group 'emacspeak)

(define-prefix-command 'emacspeak-moz-prefix-command 'emacspeak-moz-keymap )
(global-set-key "\C-x@hf" 'emacspeak-moz-prefix-command)
(loop for k in
      '(
        (" " emacspeak-moz-browse-current)
        ([up] emacspeak-moz-visit-previous-and-browse)
        ([down] emacspeak-moz-visit-next-and-browse)
        ("b" emacspeak-moz-eval-expression-and-browse)
        ("c" emacspeak-moz-close-tab-or-browser)
        ("e" emacspeak-moz-eval-expression-and-go)
        ("i" emacspeak-moz-inspect)
        ("j" emacspeak-moz-jump)
        ("\;" inferior-moz-switch-to-mozilla)
        ("," emacspeak-moz-browser-back)
        ("<" emacspeak-moz-browser-back)
        ("." emacspeak-moz-browser-forward)
        (">" emacspeak-moz-browser-forward)
        ("F" browse-url-firefox)
        ("g" emacspeak-moz-goto-url)
        ("u" emacspeak-moz-goto-url-at-point)
        ("s" emacspeak-moz-search)
        ("r" emacspeak-moz-refresh)
        )
      do
      (emacspeak-keymap-update  emacspeak-moz-keymap k))

;;}}}
;;{{{ Interactive commands:

;;;###autoload
(defun emacspeak-moz-eval-expression-and-go (exp)
  "Send expression to Moz and switch to it."
  (interactive "sJSEval: ")
  (comint-send-string (inferior-moz-process) exp)
  (switch-to-buffer (process-buffer (inferior-moz-process)))
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))


(defvar emacspeak-moz-output-buffer " *moz output*"
  "Buffer where we accumulate moz output.")

(defun emacspeak-moz-accumulate-output(output)
  "Accumulate output into our Moz output buffer."
  (declare (special emacspeak-moz-output-buffer))
  (save-excursion
    (set-buffer emacspeak-moz-output-buffer)
    (goto-char (point-max))
    (insert output)
    output))
;;;###autoload
(defun emacspeak-moz-eval-expression-and-browse (exp)
  "Send expression to Moz, get output, and browse it in Emacs."
  (interactive "sJSEval: ")
  (declare (special emacspeak-moz-output-buffer))
  (let ((comint-preoutput-filter-functions
         (list 'emacspeak-moz-accumulate-output)))
    (save-excursion
      (set-buffer (get-buffer-create emacspeak-moz-output-buffer))
      (erase-buffer)
      (setq buffer-undo-list t))
    (comint-send-string (inferior-moz-process) exp)
    (save-excursion
      (set-buffer emacspeak-moz-output-buffer)
      (accept-process-output (inferior-moz-process))
      (goto-char (point-max))
      (delete-region (line-beginning-position)
                     (line-end-position))
      (when (eq browse-url-browser-function
                'browse-url-w3)
        (add-hook 'emacspeak-w3-post-process-hook
                  'emacspeak-speak-buffer))
      (browse-url-of-buffer ))))
;;;###autoload
(defun emacspeak-moz-close-tab-or-browser ()
  "Close tab, or browser when one tab left."
  (interactive)
  (emacspeak-moz-eval-expression-and-go
   "BrowserCloseTabOrWindow()\n"))
;;;###autoload
(defun emacspeak-moz-goto-url(url)
  "Make Firefox used by our repl Go to the specified URL."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (or
                           (browse-url-url-at-point)
                           "http://"))))
  (emacspeak-moz-eval-expression-and-go
   (format "content.location.href='%s';repl.updateADom()\n"
           url)))
;;;###autoload
(defun emacspeak-moz-goto-url-at-point()
  "Make Firefox used by our repl Go to url under point."
  (interactive)
  (declare (special emacspeak-webutils-url-at-point))
  (unless emacspeak-webutils-url-at-point
    (error "Not in a browser buffer."))
  (let ((url (funcall emacspeak-webutils-url-at-point)))
    (cond
     (url
      (emacspeak-moz-eval-expression-and-go
       (format "content.location.href='%s';repl.updateADom()\n"
               url))
      (message "Sent url at point to firefox."))
     (t (error "No url under point.")))))

;;;###autoload
(defun emacspeak-moz-browser-forward ()
  "Move forward in history."
  (interactive)
  (emacspeak-moz-eval-expression-and-go
   "BrowserForward(); repl.updateADom();repl.print(\"\\n\"+title)\n")
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object))
  (emacspeak-speak-line))
;;;###autoload
(defun emacspeak-moz-browser-back ()
  "Move back in history."
  (interactive)
  (emacspeak-moz-eval-expression-and-go
   "BrowserBack(); repl.updateADom(); repl.print(\"\\n\"+title)\n")
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object))
  (emacspeak-speak-line))
;;;###autoload
(defun emacspeak-moz-jump (index)
  "Jump to specified index in history."
  (interactive "nHistory Index: ")
  (emacspeak-moz-eval-expression-and-go
   (format
    "getWebNavigation().gotoIndex(%d);repl.print(\"\\n\" +
title)\n"
    index))
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))
;;;###autoload
(defun emacspeak-moz-inspect (what)
  "Inspect specified object."
  (interactive "sInspect: ")
  (emacspeak-moz-eval-expression-and-go
   (format "repl.inspect(%s)\n" what))
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))
;;;###autoload
(defun emacspeak-moz-search (pattern)
  "Search for pattern in current context."
  (interactive "sPattern: ")
  (emacspeak-moz-eval-expression-and-go
   (format "repl.search(/%s/i)\n" pattern))
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))
;;;###autoload
(defun emacspeak-moz-refresh ()
  "Reload document."
  (interactive)
  (emacspeak-moz-eval-expression-and-go
   (format "BrowserReload();repl.updateADom();repl.print(\"\\n\"+title)\n"))
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;;###autoload
(defun emacspeak-moz-visit-next-and-browse ()
  "Asks visitor to go  forward and browses the result."
  (interactive)
  (emacspeak-moz-eval-expression-and-browse
   "repl.adom.visit(); repl.adom.html()"))
;;;###autoload
(defun emacspeak-moz-browse-current ()
  "Browse curent node."
  (interactive)
  (emacspeak-moz-eval-expression-and-browse
   " repl.adom.html()"))
;;;###autoload
(defun emacspeak-moz-visit-previous-and-browse ()
  "Asks visitor to go  backward and browses the result."
  (interactive)
  (emacspeak-moz-eval-expression-and-browse
   "repl.adom.visit(-1); repl.adom.html()"))

;;}}}
;;{{{ Advice interactive commands:

(defadvice inferior-moz-switch-to-mozilla (after emacspeak pre
                                                 act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ setup minor mode hook to load in our files.

(defvar emacspeak-moz-js-directory
  (expand-file-name "js" emacspeak-directory)
  "Directory where we keep js files.")

(defun emacspeak-moz-load-js-files (directory)
  "Load all js files from specified directory."
  (declare (special moz-repl-name))
  (comint-send-string
   (inferior-moz-process)
   (mapconcat
    #'(lambda (file)
        (format "%s.load('file://localhost%s')"
                moz-repl-name file))
    (directory-files directory  'full "js$")
    ";")))

(add-hook 'inferior-moz-mode-hook
          #'(lambda ()
              (emacspeak-moz-load-js-files emacspeak-moz-js-directory)))
;;}}}
(provide 'emacspeak-moz)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
