;;; emacspeak-remote.el --- Enables running remote Emacspeak sessions
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to remote speech server
;;; Keywords: Emacspeak, Speak, Spoken Output, remote server
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction

;;; In a running emacspeak session,
;;;nuke the running server and start talking to a remote speech server,
;;;after prompting for host and port

;;}}}
;;{{{  User customizations
;;;###autoload
(defgroup emacspeak-remote nil
  "Emacspeak remote group."
  :group 'emacspeak-remote)

(defcustom emacspeak-remote-hooks nil
  "List of hook functions that are run after
emacspeak is set to run as a remote application.
Use this to add actions you typically perform after you enter remote
mode."
  :type 'hook
  :group 'emacspeak-remote)

;;; Here is what I currently use:
;;; It switches to using C-r as the emacspeak prefix key
;;; if emacspeak-remote-update-keymap is set to t
(defvar emacspeak-remote-update-keymap nil
  "*Set this to T if you want the default remote startup hook
to update your keymap.
This is useful if you run remote emacspeak sessions within
a local  Emacspeak terminal buffer.")

(defun emacspeak-remote-default-hook ()
  "Function run by default  when we launch a remote session"
  (declare (special emacspeak-remote-update-keymap
                    emacspeak-auditory-icon-function))
  (when emacspeak-remote-update-keymap
    (emacspeak-keymap-choose-new-emacspeak-prefix
     (format "%c" 18)))
  (setq emacspeak-auditory-icon-function
        'emacspeak-serve-auditory-icon))

(add-hook 'emacspeak-remote-hooks 'emacspeak-remote-default-hook)
;;}}}
;;{{{ Helper for guessing host where we came from:

;;; To get this to work,
;;; put the following into your .login (csh)
;;; or translate it to bash syntax and place it in your
;;; .profile:

                                        ;/bin/rm -f  ~/.emacspeak/.current-remote-hostname
                                        ;set remote=`who am i`
                                        ;if ( $;remote == 6 ) then
                                        ;eval   set remote=$remote[6]
                                        ;echo -n  "$remote" > ~/.emacspeak/.current-remote-hostname
                                        ;endif

;;;Remote hostname guessing
;;;
(declaim (special emacspeak-resource-directory))

(defvar emacspeak-remote-hostname
  (concat emacspeak-resource-directory
          "/"
          ".current-remote-hostname")
  "Filename containing the name of the host we connected from")

(defun emacspeak-remote-get-current-remote-hostname  ()
  "Return the name of the remote hostname from where we connected if known"
  (declare (special emacspeak-remote-hostname))
  (when (file-exists-p   emacspeak-remote-hostname )
    (let ((buffer (find-file-noselect
                   emacspeak-remote-hostname))
          (result nil))
      (save-excursion
        (set-buffer buffer)
        (setq result (buffer-substring
                      (point-min)
                      (1- (point-max)))))
      (kill-buffer buffer )
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

(defvar emacspeak-remote-use-telnet-to-connect nil
  "*If set to t, then use a telnet subprocess
to connect to the remote host that is running the speech
server. Default is to use Emacs' built-in open-network-stream.")

;;;###autoload
(defcustom emacspeak-remote-use-ssh nil
  "Set to T to use SSH remote servers."
  :type 'boolean
  :group 'emacspeak-remote)

  
;;;###autoload
(defun emacspeak-remote-quick-connect-to-server()
  "Connect to remote server.
Does not prompt for host or port, but quietly uses the
guesses that appear as defaults when prompting.
Use this once you are sure the guesses are usually correct."
  (interactive)
  (declare (special emacspeak-remote-use-ssh))
  (cond
   (emacspeak-remote-use-ssh (emacspeak-ssh-tts-restart))
   (t (emacspeak-remote-connect-to-server
       (emacspeak-remote-get-current-remote-hostname)
       (string-to-number  emacspeak-remote-default-port-to-connect)))))

;;;###autoload
(defun emacspeak-remote-ssh-to-server(login)
  "Open ssh session to where we came from."
  (interactive
   (list
    (read-from-minibuffer "Login: "
                          (user-login-name))))
  (unless (require 'ssh)
    (error "You do not have module ssh.el installed."))
  (ssh  
   (format "%s -l %s"
           (emacspeak-remote-get-current-remote-hostname)
           login)
   "remote-ssh"))
           
   
;;;###autoload
(defun  emacspeak-remote-connect-to-server (host port)
  "Connect to and start using remote speech server running on host host
and listening on port port.  Host is the hostname of the remote
server, typically the desktop machine.  Port is the tcp port that that
host is listening on for speech requests."
  (interactive
   (progn (tts-restart)
          (list
           (completing-read "Remote host: "
                            emacspeak-eterm-remote-hosts-table ;completion table
                            nil                         ;predicate
                            nil                         ;must-match
                            (emacspeak-remote-get-current-remote-hostname) ;initial input
                            ))
          (read-minibuffer "Remote port: "
                           emacspeak-remote-default-port-to-connect)))
  (declare (special dtk-speaker-process
                    emacspeak-remote-use-telnet-to-connect
                    emacspeak-remote-default-port-to-connect
                    emacspeak-eterm-remote-hosts-table))
  (let* ((process-connection-type nil)  ;dont waste a pty
         (old-process dtk-speaker-process)
         (new-process
          (if emacspeak-remote-use-telnet-to-connect
              (start-process  "remote-speaker" nil
                              "telnet"
                              host port)
            (open-network-stream "remote-speaker" nil
                                 host port))))
    (unless (intern-soft host emacspeak-eterm-remote-hosts-table)
      (emacspeak-eterm-cache-remote-host host))
    (cond
     ((or (eq 'run (process-status new-process))
          (eq 'open (process-status new-process)))
      (setq dtk-speaker-process new-process)
      (setq emacspeak-remote-default-port-to-connect
            (format "%s" port ))
      (delete-process old-process)
      (run-hooks 'emacspeak-remote-hooks)
      (sit-for 5)
      (message "Connecting to server on host %s  port %s"
               host port )
      (sit-for 5))
     (t (error "Failed to connect to speech server on host %s port %s"
               host port )))))

(emacspeak-fix-interactive-command-if-necessary 'emacspeak-remote-connect-to-server)

;;}}}
(provide 'emacspeak-remote )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
