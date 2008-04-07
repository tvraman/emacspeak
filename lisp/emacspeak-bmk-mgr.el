;;; emacspeak-bmk-mgr.el --- speech-enables bmk-mgr.el
;;;$Id: emacspeak-bmk-mgr.el 
;;4332 2007-01-21 14:14:20Z tv.raman.tv $
;; Original Author: rdc1x@comcast.net (Robert D Crawford)
;;{{{ Copyright

;;; This file is not part of Emacs, but the same terms and
;;; conditions apply.
;; Copyright (C) 2001,2002  Dimitri V. Paduchih

;; Initial version: Author: Dimitri Paduchih <paduch@imm.uran.ru>
;;;author: T. V. Raman (integration with Emacspeak, and sections marked TVR)
;; Keywords: emacspeak, bmk-mgr

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; bmk-mgr is a bookmark manager for emacs written by Jose Antonio 
;; Ortega Ruiz <jao@gnu.org>.

;;}}}

;;{{{ Introduction

;;; Commentary:
;;; defines a simple bookmark manager for use with emacs Web browsers.
;;}}}
;;; Code:
;;{{{  required modules

(require 'emacspeak-preamble)
(eval-when-compile
  (condition-case nil
      (require 'bmk-mgr)
    (error nil)))

;;}}}
;;{{{ keybindings 

(declaim (special bmk-mgr-mode-map
                  emacspeak-prefix))
(define-key bmk-mgr-mode-map emacspeak-prefix 'emacspeak-prefix-command)

;;}}}
;;{{{ helper functions

(defun emacspeak-bmk-mgr-speak-node-info ()
  "Speak info about current bookmark or folder."
  (interactive)
  (let ((node (bmk-mgr-get-node-at-point)))
    (if node
        (if (bmk-mgr-node-url-p node)
            (let ((url (bmk-mgr-node-url node))
                  (name (bmk-mgr-node-name node)))
              (and url 
                   ;; is there some way to insert pauses and/or
                   ;; voicification to dtk-speak?
                   (dtk-speak (concat name ", " url))))
          (let ((children (bmk-mgr-node-child-folders node))
                (name (bmk-mgr-node-name node)))
            (if children
                (dtk-speak (concat name ", " "Subfolders: " 
                                   (mapconcat 'bmk-mgr-node-name children ", ")))))))))

;;}}}
;;{{{  advice interactive commands.

(defadvice bmk-mgr-next-line (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-speak-line))))

(defadvice bmk-mgr-previous-line (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-speak-line))))

(defadvice bmk-mgr-kill-bookmark (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'delete-object)
      (dtk-speak "Killed bookmark."))))

(defadvice bmk-mgr-next-folder (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line))))

(defadvice bmk-mgr-previous-folder (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-line))))

(defadvice bmk-mgr-toggle-folder(after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (if (bmk-mgr-node-closed-p (bmk-mgr-get-node-at-point))
        (emacspeak-auditory-icon 'close-object)
      (emacspeak-auditory-icon 'open-object))
    (emacspeak-speak-line)))

(defadvice bmk-mgr-bookmark-info (before emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (emacspeak-auditory-icon 'item)))

(defadvice bmk-mgr-yank-bookmark (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'yank-object)
      (dtk-speak "Yanked bookmark."))))

(defadvice bmk-mgr-find-folder (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (bmk-mgr-bookmark-info)))

(defadvice bmk-mgr-move-bookmark-up (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (dtk-speak "Moved bookmark up")
    (emacspeak-auditory-icon 'modified-object)))

(defadvice bmk-mgr-move-bookmark-down (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (dtk-speak "Moved bookmark down")
    (emacspeak-auditory-icon 'modified-object)))

(defadvice bmk-mgr-edit-bookmark (around emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (emacspeak-auditory-icon 'ask-short-question)
    ad-do-it
    (dtk-speak "Edited bookmark.")))

(defadvice bmk-mgr-add-folder (around emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (emacspeak-auditory-icon 'ask-short-question)
    ad-do-it
    (dtk-speak "Folder added.")))

;; for some reason the following advice messes up functions that call the function 
;; (defadvice bmk-mgr-add-bookmark (around emacspeak pre act comp)
;;   "Speech enable bmk-mgr."
;;   (when (interactive-p)
;;     (emacspeak-auditory-icon 'ask-short-question)
;;     (message "in add-bookmark advice before ad-do-it")
;;     ad-do-it
;;     (message "in add-bookmark advice after ad-do-it")
;;     (dtk-speak "Bookmark added.")))

(defadvice bmk-mgr-add-bookmark (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (dtk-speak "Bookmark added.")))

(defadvice bmk-mgr-refresh (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'modified-object)
      (dtk-speak "Refreshed bookmark list."))))

(defadvice bmk-mgr-browse-url (after emacspeak pre act comp)
  "Speech enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (if (and 
           (bmk-mgr-node-folder-p (bmk-mgr-get-node-at-point))
           (bmk-mgr-node-closed-p (bmk-mgr-get-node-at-point)))
          (progn
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-line))
        (progn
          (emacspeak-auditory-icon 'open-object)
          (emacspeak-speak-line)))
      (if (not (bmk-mgr-node-folder-p (bmk-mgr-get-node-at-point)))
          (emacspeak-auditory-icon 'open-object)))))
        
(defadvice bmk-mgr-add-current-page (after emacspeak pre act comp)
  "Speach enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'save-object)
      (dtk-speak "Added current page."))))

(defadvice bmk-mgr-add-url-at-point (after emacspeak pre act comp)
  "Speach enable bmk-mgr."
  (when (interactive-p)
    (let ((emacspeak-speak-messages nil))
      (emacspeak-auditory-icon 'save-object)
      (dtk-speak "Added url at point."))))

;;}}}
;;{{{ mapping font faces to personalities 

(voice-setup-add-map
 '(
   (bmk-mgr-sel-folder-face voice-bolden-medium)
   ))

(defadvice bmk-mgr-mode (after emacspeak pre act comp)
  "Set punctuation mode and refresh punctuations."
  (declare (special dtk-punctuation-mode))
  (setq dtk-punctuation-mode 'some)
  (emacspeak-pronounce-refresh-pronunciations)
  (define-key bmk-mgr-mode-map emacspeak-prefix 'emacspeak-prefix-command))

;;}}}
(provide 'emacspeak-bmk-mgr)
;;{{{ end of file 

;;; emacspeak-bmk-mgr.el ends here

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}

