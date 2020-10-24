;;; emacspeak-remote.el --- Enables running remote Emacspeak sessions  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface to remote speech server
;;; Keywords: Emacspeak, Speak, Spoken Output, remote server
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-25 16:05:19 -0700 (Fri, 25 Jul 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2018, T. V. Raman
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

;;{{{  Required modules
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction
;;; Commentary:
;;; This module is mostly obsolete.
;;; Code:
;;}}}
;;{{{  User customizations

(defgroup emacspeak-remote nil
  "Emacspeak remote group."
  :group 'emacspeak-remote)

;;}}}
;;{{{ Helper for guessing host where we came from:

;;; see etc/last-log.pl

;;;Remote hostname guessing
;;;
(cl-declaim (special emacspeak-user-directory))

(defvar emacspeak-remote-hostname
  (expand-file-name  ".current-remote-hostname"
                     emacspeak-user-directory)
  "Filename containing the name of the host we connected from")


(defun emacspeak-remote-edit-current-remote-hostname  ()
  "Interactively set up where we came from.
Value is persisted for use with ssh servers."
  (interactive)
  (cl-declare (special emacspeak-remote-hostname))
  (when (file-exists-p   emacspeak-remote-hostname)
    (find-file emacspeak-remote-hostname)))
;;; Todo: parse out hostname if the file has user@host:port
(defun emacspeak-remote-get-current-remote-hostname  ()
  "Return the name of the remote hostname from where we connected if known"
  (cl-declare (special emacspeak-remote-hostname))
  (when (file-exists-p   emacspeak-remote-hostname)
    (let ((buffer (find-file-noselect emacspeak-remote-hostname))
          (result nil))
      (with-current-buffer buffer
        (setq result (buffer-substring (point-min) (1- (point-max)))))
      (kill-buffer buffer)
      result)))

;;}}}
;;{{{  Connect to  remote server
(defun emacspeak-remote-auto-connect-to-server ()
  "Invoked via gnudoit --typically from a login sequence--
to have the running emacspeak connect to a server running on
the host we just logged in from."
  (let ((host (emacspeak-remote-get-current-remote-hostname)))
    (when (and  (> (length host) 0)
                (not (eq host (system-name))))
      (emacspeak-remote-quick-connect-to-server))))

(defvar emacspeak-remote-default-port-to-connect
  "2222"
  "Default used when prompting for a port to connect to.")

;;;###autoload
(defun emacspeak-remote-quick-connect-to-server()
  "Connect to remote server.
Does not prompt for host or port, but quietly uses the guesses
that appear as defaults when prompting. Use this once you are
sure the guesses are usually correct."
  (interactive)
  (emacspeak-remote-connect-to-server
   (emacspeak-remote-get-current-remote-hostname)
   (string-to-number  emacspeak-remote-default-port-to-connect)))
(declare-function ssh "ssh" (input-args &optional buffer))
;;;###autoload
(defun emacspeak-remote-home()
  "Open ssh session to where we came from.
Uses value returned by `emacspeak-remote-get-current-remote-hostname'."
  (interactive)
  (unless (require 'ssh)
    (error "You do not have module ssh.el installed."))
  (let
      ((spec (emacspeak-remote-get-current-remote-hostname))
       fields host port user)
    (setq fields (split-string spec "[@:]"))
    (setq
     user  (cl-first fields)
     host (cl-second fields)
     port (cl-third fields))
    (ssh
     (format "%s -p %s -l %s"
             host port user)
     (format "Remote-%s"
             host))))

;;;###autoload
(defun emacspeak-remote-ssh-to-server(login host port)
  "Open ssh session to where we came from."
  (interactive
   (list
    (read-from-minibuffer "Login: "
                          (user-login-name))
    (read-from-minibuffer "Host: ")
    (read-from-minibuffer "Port: "
                          "22")))
  (unless (require 'ssh)
    (error "You do not have module ssh.el installed."))
  (ssh
   (format "%s -p %s -l %s"
           host port login)
   "remote-ssh"))

(defcustom emacspeak-remote-default-ssh-server
  nil
  "Default ssh server to use for remote speech server."
  :type '(choice
          (const  :tag "Ignore" nil)
          (string  :tag "SSH Server"))
  :group 'emacspeak-remote)

;;;###autoload
(defun emacspeak-remote-quick-connect-via-ssh ()
  "Connect via ssh to remote Emacspeak server.
Server is specified via custom option `emacspeak-remote-default-ssh-server'."
  (interactive)
  (cl-declare (special emacspeak-remote-default-ssh-server))
  (when emacspeak-remote-default-ssh-server
    (setq dtk-program emacspeak-remote-default-ssh-server)
    (dtk-select-server emacspeak-remote-default-ssh-server)
    (dtk-initialize)))
(declare-function emacspeak-eterm-cache-remote-host "emacspeak-eterm" (host))

(defvar emacspeak-eterm-remote-hosts-table
  (make-vector 127 0)
  "obarray used for completing hostnames when prompting for a remote
host. Hosts are added whenever a new hostname is encountered. ")

;;;###autoload
(defun  emacspeak-remote-connect-to-server (host port)
  "Connect to and start using remote speech server running on
host host and listening on port port. Host is the hostname of the
remote server, typically the desktop machine. Port is the tcp
port that that host is listening on for speech requests."
  (interactive
   (list
    (completing-read "Remote host: "
                     emacspeak-remote-hosts-table ;completion table
                     nil                          ;predicate
                     nil                          ;must-match
                     (emacspeak-remote-get-current-remote-hostname) ;initial input
                     )
    (read-from-minibuffer "Remote port:" dtk-local-server-port)))
  (cl-declare (special dtk-speaker-process dtk-program 
                       dtk-local-server-port
                       dtk-local-engine emacspeak-remote-hosts-table))
  (let* ((dtk-program dtk-local-engine)
         (process-connection-type nil)  ;dont waste a pty
         (old-process dtk-speaker-process)
         (new-process
          (open-network-stream "remote-speaker" nil host port)))
    (unless (intern-soft host emacspeak-remote-hosts-table)
      (emacspeak-eterm-cache-remote-host host))
    (accept-process-output)
    (cond
     ((or (eq 'run (process-status new-process))
          (eq 'open (process-status new-process)))
      (setq dtk-speaker-process new-process)
      (setq emacspeak-remote-default-port-to-connect (format "%s" port))
      (delete-process old-process)
      (run-hooks 'emacspeak-remote-hooks)
      (message "Connecting to server on host %s  port %s" host port))
     (t (error "Failed to connect to speech server on host %s port %s" host port)))))

;;}}}
(provide 'emacspeak-remote)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
