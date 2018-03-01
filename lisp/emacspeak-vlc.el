;;; emacspeak-vlc.el --- Speech-enable VLC  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable VLC An Emacs Interface to vlc
;;; Keywords: Emacspeak,  Audio Desktop vlc
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
;;; MERCHANTABILITY or FITNVLC FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; An Emacspeak Front-End For VLC Interaction.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'derived)

;;}}}
;;{{{ define a derived mode for VLC interaction

(defvar emacspeak-vlc-process nil
  "Process handle to vlc.")

(defun emacspeak-vlc-dispatch (command)
  "Dispatch command to vlc."
  (cl-declare (special emacspeak-vlc-process))
  (with-current-buffer (process-buffer emacspeak-vlc-process)
    (erase-buffer)
    (process-send-string
     emacspeak-vlc-process
     (format "%s\n" command))
    (accept-process-output emacspeak-vlc-process 0.1)))

(defvar emacspeak-vlc-current-directory nil
  "Records current directory of media being played.
This is set to nil when playing Internet  streams.")

(define-derived-mode emacspeak-vlc-mode comint-mode
  "Vlc Interaction"
  "Major mode for vlc interaction. \n\n
\\{emacspeak-vlc-mode-map}"
  (progn
    (setq buffer-undo-list t)
    (setq emacspeak-vlc-process (get-buffer-process (current-buffer)))))

;;}}}
;;{{{ emacspeak-vlc

(defgroup emacspeak-vlc nil
  "Emacspeak VLC Interaction."
  :group 'emacspeak)

(defcustom emacspeak-vlc-program
  (or (executable-find "cvlc")
      (executable-find "vlc"))
  "VLC player program."
  :type 'string
  :group 'emacspeak-vlc)



(defvar emacspeak-vlc-default-options
  (list "-I" "rc")
  "Default options for VLC.")

(defcustom emacspeak-vlc-options
  (copy-sequence emacspeak-vlc-default-options)
  "Options passed to VLC."
  :type  '(repeat
           (string :tag "option"))
  :group 'emacspeak-vlc)





;;;###autoload
(defun emacspeak-vlc  ()
  "Start or control Emacspeak VLC player.

Uses current context to prompt for media to play.
Controls media playback when already playing a stream.

\\{emacspeak-vlc-mode-map}."
  (interactive)
  (cl-declare (special emacspeak-vlc-process))
  (cond
   ((and emacspeak-vlc-process
         (eq 'run (process-status emacspeak-vlc-process))
         (buffer-live-p (process-buffer emacspeak-vlc-process)))
    (with-current-buffer (process-buffer emacspeak-vlc-process)
      (call-interactively #'emacspeak-vlc-command)))
   (t
    (call-interactively #'emacspeak-vlc-player))))

;;;###autoload
(defun emacspeak-vlc-pop-to-player ()
  "Pop to vlc buffer."
  (interactive)
  (cl-declare (special emacspeak-vlc-process))
  (unless (process-live-p emacspeak-vlc-process)
    (emacspeak-vlc-player))
  (pop-to-buffer (process-buffer emacspeak-vlc-process))
  (emacspeak-speak-mode-line))

(defun emacspeak-vlc-command (key)
  "Invoke VLC commands."
  (interactive (list (read-key-sequence "MPlayer Key: ")))
  (unless (eq 'run (process-status emacspeak-vlc-process))
    (emacspeak-vlc-player))
  (call-interactively
   (or (lookup-key emacspeak-vlc-mode-map key) 'undefined)))
;;;###autoload
(defun emacspeak-vlc-url (url )
  "Call emacspeak-vlc with specified URL."
  (interactive (list (car (browse-url-interactive-arg "Media URL: "))))
  (ems-with-messages-silenced
   (emacspeak-vlc url )))





(defun emacspeak-vlc-directory-files (directory)
  "Return media files in directory.
Searches recursively if `directory-files-recursively' is available (Emacs 25)."
  (cl-declare (special emacspeak-media-extensions))
  (cond
   ((fboundp 'directory-files-recursively)
    (directory-files-recursively directory emacspeak-media-extensions))
   (t (directory-files  directory 'full emacspeak-media-extensions))))



(defun emacspeak-vlc-read-resource ()
  "Read resource from minibuffer with contextual smarts."
  (cl-declare (special ido-work-directory-list ))
  (let ((completion-ignore-case t)
        (read-file-name-function
         (if (eq major-mode 'locate-mode)
             #'read-file-name-default
           #'ido-read-file-name))
        (read-file-name-completion-ignore-case t)
        (default
          (when (or (eq major-mode 'dired-mode) (eq major-mode 'locate-mode))
            (dired-get-filename nil 'no-error)))
        (ido-work-directory-list
         (cl-loop
          for d in ido-work-directory-list
          when (string-match  emacspeak-media-directory-regexp  d) collect d))
        (result nil))
    (setq result
          (read-file-name
           "Media Resource: "
           default-directory
           default 'must-match default))
    result))






;;;###autoload
(defun emacspeak-vlc-player (resource)
  "Play specified resource using vlc.
Resource is a media resource or playlist containing media resources.
The player is placed in a buffer in emacspeak-vlc-mode."
  (interactive
   (list
    (emacspeak-vlc-read-resource)))
  (cl-declare (special
            emacspeak-vlc-file-list emacspeak-vlc-current-directory
            ido-work-directory-list emacspeak-media-directory-regexp
            emacspeak-media-shortcuts-directory emacspeak-vlc-process
            emacspeak-vlc-program emacspeak-vlc-options))
  (when (and emacspeak-vlc-process
             (eq 'run (process-status emacspeak-vlc-process))
             (y-or-n-p "Stop currently playing music? "))
    (emacspeak-vlc-quit)
    (setq emacspeak-vlc-process nil))
  (let ((buffer (get-buffer-create "*Vlc*"))
        (process-connection-type nil)
        (options (copy-sequence emacspeak-vlc-options))
        (file-list nil))
    (unless (string-match "^[a-z]+:"  resource) ; not a URL
      (setq resource (expand-file-name resource))
      (setq emacspeak-vlc-current-directory
            (file-name-directory resource)))
    (if (file-directory-p resource)
        (setq file-list (emacspeak-vlc-directory-files resource))
      (setq file-list (list resource)))
    (setq options
          (cond
           (file-list (nconc options file-list))
           (t
            (nconc options (list resource)))))
    (with-current-buffer buffer
      (setq buffer-undo-list t)
      (setq emacspeak-vlc-process
            (apply 'start-process "VLC" buffer
                   emacspeak-vlc-program options))
      (when emacspeak-vlc-current-directory
        (cd emacspeak-vlc-current-directory))
      (emacspeak-vlc-mode)
      (setq  emacspeak-vlc-file-list file-list)
      (message "VLC opened  %s"
               (abbreviate-file-name resource)))))

;;}}}
;;{{{ Interactive Commands:

;;}}}
(provide 'emacspeak-vlc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
