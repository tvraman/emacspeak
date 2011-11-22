;;; emacspeak-shr.el --- Speech-enable SHR
;;; $Id: emacspeak-shr.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SHR An Emacs Interface to shr
;;; Keywords: Emacspeak,  Audio Desktop shr
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNSHR FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; SHR ==  Simple HTML  Reader
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'shr)
(require 'org)

;;}}}
;;{{{ Enhanced shr:
(defvar shr-url-dom nil
  "Buffer local value of DOM.")
(make-variable-buffer-local 'shr-url-dom)

(defun shr-url-callback (args)
  "Callback for url-retrieve."
  (goto-char (point-min))
  (let* ((inhibit-read-only t)
         (start (re-search-forward "^$"))
         (dom (libxml-parse-html-region start(point-max)))
         (buffer
          (get-buffer-create
           (or 
            (shr-get-title-from-dom dom)
            "Untitled"))))
    (with-current-buffer buffer
      (erase-buffer)
      (shr-insert-document dom)
      (setq shr-url-dom dom)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)))    

(defsubst shr-get-title-from-dom (dom)
  "Return Title."
  (let ((content dom)
        (title nil))
    (while
        (and content
             (listp content)
             (not
              (setq title
                    (find-if
                     #'(lambda (e) (and (listp e)(eq 'title (car
                                                             e))))
                     (third content)))))
      (setq content (third content)))
    (when title (third title))))

;;;###autoload
(defun shr-url (url)
  "Display web page."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (get-text-property (point) 'shr-url))))
  (url-retrieve url 'shr-url-callback))

;;}}}
;;{{{ Speech-enable:

;;}}}
(provide 'emacspeak-shr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
