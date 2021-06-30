;;; emacspeak-bbc.el --- Light-Weight BBC Client   -*- lexical-binding: t; -*-
;;; $Id: emacspeak-bbc.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Light-weight  BBC 
;;; Keywords: Emacspeak,  Audio Desktop bbc
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNBBC FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; BBC developer API (backstage) is now history.
;;; that implementation is in obsolete/emacspeak-bbc-backstage.el
;;; This module contains a light-weight client.

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-forms)

;;}}}
;;{{{ Stream using get_iplayer:

(defvar emacspeak-bbc-iplayer-handle
  (expand-file-name  "iplayer-stream.mp3" temporary-file-directory)
  "Location of named pipe used for streaming.")

(defvar emacspeak-bbc-get-iplayer
  (eval-when-compile  (executable-find "get_iplayer"))
  "Name of get_iplayer executable.")

(defun emacspeak-bbc-get-iplayer-stream-url (url)
  "Stream using get_iplayer."
  (interactive "sURL: ")
  (cl-declare (special emacspeak-bbc-get-iplayer emacspeak-bbc-iplayer-handle))
  (let
      ((command
        (format
         "%s --stream --url='%s' --modes=flashaaclow,hlsaaclow --type=radio > %s &"
         emacspeak-bbc-get-iplayer url emacspeak-bbc-iplayer-handle)))
    (unless (file-exists-p emacspeak-bbc-iplayer-handle)
      (shell-command (format "mknod %s p" emacspeak-bbc-iplayer-handle)))
    (dtk-speak-and-echo "Initialized stream, please wait.")
    (shell-command  command " *get-iplayer*")
    (sit-for 1)
    (emacspeak-m-player emacspeak-bbc-iplayer-handle)))

(defun emacspeak-bbc-get-iplayer-stream-pid (pid)
  "Stream using get_iplayer."
  (interactive "sPid ")
  (cl-declare (special emacspeak-bbc-get-iplayer emacspeak-bbc-iplayer-handle))
  (let
      ((command
        (format
         "%s --stream --pid='%s' --modes=flashaaclow,hlsaaclow --type=radio > %s &"
         emacspeak-bbc-get-iplayer pid emacspeak-bbc-iplayer-handle)))
    (unless (file-exists-p emacspeak-bbc-iplayer-handle)
      (shell-command (format "mknod %s p" emacspeak-bbc-iplayer-handle)))
    (dtk-speak-and-echo "Initialized stream, please wait.")
    (shell-command  command " *get-iplayer*")
    (sit-for 1)
    (emacspeak-m-player emacspeak-bbc-iplayer-handle)))

;;}}}
;;{{{get_iplayer catalog 

;;; Run get_iplayer regularly to refresh ~/.get_iplayer/radio.cache.
;;; This command then helps you view that listing using emacs
;;forms-mode.
;;;###autoload
(defun emacspeak-bbc-schedule ()
  "Browse BBC Schedule from get_iplayer radio cache"
  (interactive)
  (funcall-interactively #'emacspeak-forms-find-file (expand-file-name "forms/get-iplayer.el" emacspeak-etc-directory)))

;;}}}
(provide 'emacspeak-bbc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
