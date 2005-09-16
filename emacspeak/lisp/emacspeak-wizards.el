;;; emacspeak-wizards.el --- Implements Emacspeak  convenience wizards
;;; $Id$
;;; $Author$
;;; Description:  Contains convenience wizards
;;; Keywords: Emacspeak,  Audio Desktop Wizards
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;; Contains various wizards for the Emacspeak desktop.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'lisp-mnt)
(require 'desktop)
(require 'dired)
(require 'derived)
(require 'eldoc)
(require 'find-dired)
(require 'emacspeak-preamble)
(require 'emacspeak-table-ui)
(require 'shell)
(require 'texinfo)
(require 'term)
(require 'cus-edit)
(require 'emacspeak-xslt)
(eval-when-compile
  (condition-case nil
      (require 'emacspeak-w3)
    (error nil)))

;;}}}
;;{{{ custom

(defgroup emacspeak-wizards nil
  "Wizards for the Emacspeak desktop."
  :group 'emacspeak
  :prefix "emacspeak-wizards-")

;;}}}
;;{{{  Emacspeak News and Documentation

(defun emacspeak-view-emacspeak-news ()
  "Display info on recent change to Emacspeak."
  (interactive)
  (declare (special emacspeak-etc-directory
                    emacspeak-version))
  (find-file-read-only (expand-file-name "NEWS"
                                         emacspeak-etc-directory))
  (emacspeak-auditory-icon 'news)
  (view-mode t)
  (voice-lock-mode t)
  (let
      ((p (where-is-internal
           'outline-previous-visible-heading nil 'ascii))
       (n (where-is-internal
           'outline-next-visible-heading nil 'ascii))
       (keys nil))
    (when   (and n p)
      (setq keys
            (format "%s and %s"
                    (key-description p)
                    (key-description n))))
    (dtk-speak
     (format "Welcome to  Emacspeak %s news. Use %s to
navigate this document."
             emacspeak-version
             (or keys "outline mode features")))))
;;;###autoload
(defun emacspeak-view-emacspeak-doc ()
  "Display a summary of all Emacspeak commands."
  (interactive)
  (declare (special emacspeak-etc-directory))
  (find-file-read-only (expand-file-name "DOC"
                                         emacspeak-etc-directory))
  (emacspeak-auditory-icon 'help)
  (view-mode t)
  (dtk-speak
   (format "Welcome to a summary of Emacspeak commands")))
;;;###autoload
(defun emacspeak-view-emacspeak-tips ()
  "Browse  Emacspeak productivity tips."
  (interactive)
  (declare (special emacspeak-etc-directory))
  (emacspeak-w3-without-xsl
   (browse-url
    (format "file:///%stips.html"
	    emacspeak-etc-directory)))
  (emacspeak-auditory-icon 'help)
  (emacspeak-speak-mode-line))

;;;###autoload
(defun emacspeak-view-emacspeak-faq ()
  "Browse the Emacspeak FAQ."
  (interactive)
  (declare (special emacspeak-etc-directory))
  (find-file-read-only (expand-file-name "FAQ"
                                         emacspeak-etc-directory))
  (emacspeak-auditory-icon 'help)
  (view-mode t)
  (voice-lock-mode t)
  (let
      ((p (where-is-internal
           'outline-previous-visible-heading nil 'ascii))
       (n (where-is-internal
           'outline-next-visible-heading nil 'ascii))
       (keys nil))
    (when   (and n p)
      (setq keys
            (format "%s and %s"
                    (key-description p)
                    (key-description n))))
    (dtk-speak
     (format "Welcome to the Emacspeak FAQ List. Use %s to
navigate this document."
             (or keys "outline mode features")))))

;;}}}
;;{{{ utility function to copy documents:

(defvar emacspeak-copy-file-location-history nil
  "History list for prompting for a copy location.")

(defvar emacspeak-copy-associated-location nil
  "Buffer local variable that records where we copied this document last.")

(make-variable-buffer-local
 'emacspeak-copy-associated-location)
;;;###autoload
(defun emacspeak-copy-current-file ()
  "Copy file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when copying.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Asks for confirmation if the copy will result in an
  existing file being overwritten."
  (interactive)
  (declare (special emacspeak-copy-file-location-history
                    emacspeak-copy-associated-location))
  (let ((file (or (buffer-file-name)
                  (error "Current buffer is not visiting any file")))
        (location (read-file-name
                   "Copy current file to location: "
                   emacspeak-copy-associated-location ;default
                   (car
                    emacspeak-copy-file-location-history)))
        (minibuffer-history (or
                             emacspeak-copy-file-location-history
                             minibuffer-history)))
    (setq emacspeak-copy-associated-location location)
    (when (file-directory-p location)
      (unless (string-equal location (car emacspeak-copy-file-location-history))
        (push location emacspeak-copy-file-location-history))
      (setq location
            (expand-file-name
             (file-name-nondirectory file)
             location)))
    (copy-file
     file location
     1                                  ;prompt before overwriting
     t                                  ;preserve
                                        ;modification time
     )
    (emacspeak-auditory-icon 'select-object)
    (message "Copied current document to %s" location)))
;;;###autoload
(defun emacspeak-link-current-file ()
  "Link (hard link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists."
  (interactive)
  (declare (special emacspeak-copy-file-location-history
                    emacspeak-copy-associated-location))
  (let ((file (or (buffer-file-name)
                  (error "Current buffer is not visiting any file")))
        (location (read-file-name
                   "Link current file to location: "
                   emacspeak-copy-associated-location ;default
                   (car
                    emacspeak-copy-file-location-history)))
        (minibuffer-history (or
                             emacspeak-copy-file-location-history
                             minibuffer-history)))
    (setq emacspeak-copy-associated-location location)
    (when (file-directory-p location)
      (unless (string-equal location (car emacspeak-copy-file-location-history))
        (push location emacspeak-copy-file-location-history))
      (setq location
            (expand-file-name
             (file-name-nondirectory file)
             location)))
    (add-name-to-file
     file location)
    (emacspeak-auditory-icon 'select-object)
    (message "Linked current document to %s" location)))
;;;###autoload
(defun emacspeak-symlink-current-file ()
  "Link (symbolic link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists."
  (interactive)
  (declare (special emacspeak-copy-file-location-history
                    emacspeak-copy-associated-location))
  (let ((file (or (buffer-file-name)
                  (error "Current buffer is not visiting any file")))
        (location (read-file-name
                   "Symlink current file to location: "
                   emacspeak-copy-associated-location ;default
                   (car
                    emacspeak-copy-file-location-history)))
        (minibuffer-history (or
                             emacspeak-copy-file-location-history
                             minibuffer-history)))
    (setq emacspeak-copy-associated-location location)
    (when (file-directory-p location)
      (unless (string-equal location (car emacspeak-copy-file-location-history))
        (push location emacspeak-copy-file-location-history))
      (setq location
            (expand-file-name
             (file-name-nondirectory file)
             location)))
    (make-symbolic-link
     file location)
    (emacspeak-auditory-icon 'select-object)
    (message "Symlinked  current doc>ument to %s" location)))

;;}}}
;;{{{ Utility command to run and tabulate shell output

(defvar emacspeak-speak-run-shell-command-history nil
  "Records history of commands used so far.")
;;;###autoload
(defun emacspeak-speak-run-shell-command (command &optional as-root)
  "Invoke shell COMMAND and display its output as a table.  The results
are placed in a buffer in Emacspeak's table browsing mode.  Optional
interactive prefix arg as-root runs the command as root (not yet
implemented).  Use this for running shell commands that produce
tabulated output.  This command should be used for shell commands that
produce tabulated output that works with Emacspeak's table recognizer.
Verify this first by running the command in a shell and executing
command `emacspeak-table-display-table-in-region' normally bound to
\\[emacspeak-table-display-table-in-region]."
  (interactive
   (list
    (read-from-minibuffer "Shell command: "
                          nil           ;initial input
                          nil           ; keymap
                          nil           ;read
                          'emacspeak-speak-run-shell-command-history)
    current-prefix-arg))
  (let ((buffer-name (format "*%s-output*" command))
        (start nil)
        (end nil))
    (shell-command command buffer-name)
    (pushnew   command
               emacspeak-speak-run-shell-command-history
               :test 'string-equal)
    (save-excursion
      (set-buffer buffer-name)
      (untabify (point-min) (point-max))
      (setq start (point-min)
            end (1- (point-max)))
      (condition-case nil
          (emacspeak-table-display-table-in-region  start end )
        (error
         (progn
           (message "Output could not be tabulated correctly")
           (switch-to-buffer buffer-name)))))))

;;}}}
;;{{{ linux howtos

(defcustom emacspeak-wizards-linux-howto-directory
  (cond
   ((file-exists-p "/usr/doc/HOWTO/" )
    "/usr/doc/HOWTO/")
   ((file-exists-p "/usr/doc/howto/" )
    "/usr/doc/howto/")
   ((file-exists-p
     "/usr/share/doc/howto/")
    "/usr/share/doc/howto/")
   (t nil))
  "Root  of Linux Howtos."
  :type '(choice :tag "Howto Root"
		 (const nil :tag "None")
		 (directory :tag "Directory"))
  :group 'emacspeak-wizards)

;;;###autoload
(defun emacspeak-speak-browse-linux-howto (howto)
  "Browse a Linux Howto file.
We cleanup underlining, and set up outline mode correctly."
  (interactive 
   (list
    (read-file-name "Howto file: "
                    emacspeak-wizards-linux-howto-directory
                    nil
                    t)))
  (declare (special view-exit-action))
  (let ((buffer (find-file-noselect  howto))
        (output (format "*%s*"
                        (file-name-nondirectory howto))))
    (unless buffer (error "Cannot find howto %s" howto))
    (save-excursion
      (set-buffer buffer)
      (shell-command-on-region  (point-min)
                                (point-max)
                                "ul -t dumb"
                                output)
      (set-buffer output)
      (view-mode 1)
      (outline-minor-mode t)
      (setq outline-regexp 
            "^ *[1-9][0-9.]* ")
      (goto-char (point-min)))
    (kill-buffer buffer)
    (switch-to-buffer output)
    (set-buffer-modified-p nil)
    (setq view-exit-action 'kill-buffer)
    (emacspeak-auditory-icon 'open-object)
    (message "You can use outline commands to browse this
howto document.")))

;;}}}
;;{{{ pop up messages buffer 

                                        ; Internal variable to memoize window configuration

(defvar emacspeak-popup-messages-config-0 nil
  "Memoizes window configuration.")
;;;###autoload
(defun emacspeak-speak-popup-messages ()
  "Pop up messages buffer.
If it is already selected then hide it and try to restore
previous window configuration."
  (interactive)
  (cond
                                        ; First check if Messages buffer is already selected
   ((string-equal (buffer-name (window-buffer (selected-window)))
		  "*Messages*")
    (when (window-configuration-p emacspeak-popup-messages-config-0)
      (set-window-configuration emacspeak-popup-messages-config-0))
    (setq emacspeak-popup-messages-config-0 nil)
    (bury-buffer "*Messages*")
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line))
                                        ; popup Messages buffer
   (t
                                        ; Memoize current window configuration only if buffer isn't yet visible
    (setq emacspeak-popup-messages-config-0
	  (and (not (get-buffer-window "*Messages*"))
	       (current-window-configuration)))
    (pop-to-buffer "*Messages*" nil t)
                                        ; position cursor on the last message
    (goto-char (point-max))
    (beginning-of-line  (and (bolp) 0))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line))))

;;}}}
;;{{{ Show active network interfaces
;;;###autoload
(defun emacspeak-speak-hostname ()
  "Speak host name."
  (interactive)
  (message (system-name)))

(defcustom emacspeak-speak-show-active-network-interfaces-command
  "echo `/sbin/ifconfig | grep -v '^lo' | grep '^[a-z]' | awk '{print $1}'`"
  "Command that displays names of active network interfaces."
  :type 'string
  :group 'emacspeak-wizards)
					;"echo `/sbin/ifconfig %s | grep 'inet addr' | awk '{print $2}'| sed
					;'s/addr://'`"

(defcustom emacspeak-speak-show-active-network-interfaces-addresses
  "ifconfig %s |grep inet |cut -d : -f 2 |cut -d \\  -f 1"
  "Command that displays address of  a specific interface."
  :type 'string
  :group 'emacspeak-wizards
  )

(defvar emacspeak-speak-network-interfaces-list
  (list  "eth0" "ppp0" "eth1" "ppp1" "tr0" "tr1")
  "Used whne prompting for an interface to query.")
;;;###autoload
(defun emacspeak-speak-show-active-network-interfaces
  (&optional address)
  "Shows all active network interfaces in the echo area.
With interactive prefix argument ADDRESS it prompts for a
specific interface and shows its address. The address is
also copied to the kill ring for convenient yanking."
  (interactive "P")
  (declare (special emacspeak-speak-network-interfaces
                    emacspeak-speak-message-again-should-copy-to-kill-ring
                    emacspeak-last-message
                    emacspeak-speak-show-active-network-interfaces-command
                    emacspeak-speak-show-active-network-interfaces-addresses))
  (let ((command nil))
    (cond 
     (address  (setq command
                     (format
                      emacspeak-speak-show-active-network-interfaces-addresses
                      (read-from-minibuffer
                       "Specify interface: "
                       nil nil nil 
                       'emacspeak-speak-network-interfaces-list  ))))
     (t (setq command 
              emacspeak-speak-show-active-network-interfaces-command)))
    (shell-command command )
    (when (and address
               (not emacspeak-speak-message-again-should-copy-to-kill-ring))
      (kill-new emacspeak-last-message))))

;;}}}
;;{{{  simple phone book 
(defcustom emacspeak-speak-telephone-directory
  (expand-file-name "tel-dir" emacspeak-resource-directory)
  "File holding telephone directory.
This is just a text file, and we use grep to search it."
  :group 'emacspeak-speak
  :type 'string)

(defcustom emacspeak-speak-telephone-directory-command
  "grep -i "
  "Command used to look up names in the telephone
directory."
  :group 'emacspeak-speak
  :type 'string)
;;;###autoload
(defun emacspeak-speak-telephone-directory (&optional edit)
  "Lookup and display a phone number.
With prefix arg, opens the phone book for editting."
  (interactive "P")
  (cond
   (edit
    (find-file emacspeak-speak-telephone-directory)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object))
   ((file-exists-p emacspeak-speak-telephone-directory)
    (shell-command
     (format "%s %s %s"
             emacspeak-speak-telephone-directory-command
             (read-from-minibuffer "Lookup number for: ")
             emacspeak-speak-telephone-directory)))
   (t (error "First create your phone directory in %s"
             emacspeak-speak-telephone-directory))))

;;}}}
;;{{{  launch a root shell 

(require 'comint)

;;; convenience to launch a root shell.

(defvar emacspeak-wizards-root-buffer 
  "*root*"
  "Name of buffer where we run as root.")
;;;###autoload
(defun emacspeak-root (&optional cd)
  "Start a root shell or switch to one that already exists.
Optional interactive prefix arg `cd' executes cd
default-directory after switching."
  (interactive "P")
  (declare (special explicit-shell-file-name
                    emacspeak-wizards-root-buffer
                    default-directory))
  (let ((dir (expand-file-name default-directory)))
    (cond
     ((comint-check-proc emacspeak-wizards-root-buffer)
      (pop-to-buffer emacspeak-wizards-root-buffer)
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-mode-line))
     (t
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))		     
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args")))
             shell-buffer)
        (save-excursion
          (set-buffer (apply 'make-comint "root" prog
                             (if (file-exists-p startfile) startfile)
                             (if (and xargs-name (boundp xargs-name))
                                 (symbol-value xargs-name)
                               '("-i"))))
          (setq shell-buffer (current-buffer))
          (shell-mode)
          (switch-to-buffer shell-buffer)
          (process-send-string
           (get-buffer-process shell-buffer)
           "su -l\n")))
      (when (featurep 'emacspeak)
        (dtk-speak "Enter root password: "))))
    (when cd
      (unless (string-equal dir
                            (expand-file-name default-directory))
        (goto-char (point-max))
        (insert (format "pushd %s" dir))
        (comint-send-input)
        (shell-process-cd dir)))))
;;;###autoload
(defun emacspeak-sudo (command)
  "SUDo command --run command as super user."
  (interactive
   (list
    (read-from-minibuffer "SUDO Command: ")))
  (let* ((name  (car (split-string command)))
         (buffer (format "*sudo-%s*" name)))
    (shell-command
     (format "sudo %s" command)
     buffer)
    (cond
     ((> 1
         (save-excursion
           (set-buffer buffer)
           (count-lines (point-min) (point-max))))
      (pop-to-buffer buffer)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'open-object))
     (t (save-excursion
          (set-buffer buffer)
          (message (buffer-string)))))))
;;;###autoload
(defun emacspeak-wizards-tpctl-display-status ()
  "Show display status on thinkpads using tpctl."
  (interactive)
  (emacspeak-sudo "tpctl --dull --sdi | tail -1"))

;;{{{ ppp

(defvar emacspeak-wizards-ppp-status-command
  "/sbin/ifconfig | grep ^ppp"
  "Command to obtain ppp status.")

(defun emacspeak-wizards-ppp-status ()
  "Return ppp status."
  (declare (special emacspeak-wizards-ppp-status-command))
  (zerop (shell-command emacspeak-wizards-ppp-status-command)))

(defun emacspeak-wizards-ppp-toggle ()
  "Bring up or bring down ppp."
  (interactive)
  (if (emacspeak-wizards-ppp-status)
      (emacspeak-sudo "ifdown ppp0  1>&- 2>&- &")
    (emacspeak-sudo "ifup ppp0  1>&- 2>&- &")))

;;}}}
;;{{{ vpn
(defvar emacspeak-wizards-vpn-status-command
  "/sbin/ifconfig | grep ^ipsec"
  "Command to obtain vpn status.")

(defcustom emacspeak-wizards-vpn-start-command nil
  "Command that brings up a VPN connection."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Command"))
  :group 'emacspeak-wizards)

(defcustom emacspeak-wizards-vpn-end-command nil
  "Command that brings down a   VPN connection."
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Command"))
  :group 'emacspeak-wizards)

(defun emacspeak-wizards-vpn-status ()
  "Return vpn status."
  (zerop (shell-command emacspeak-wizards-vpn-status-command)))

;;;###autoload
(defun emacspeak-wizards-vpn-toggle ()
  "Bring up or bring down vpn."
  (interactive)
  (declare (special emacspeak-wizards-vpn-start-command
                    emacspeak-wizards-vpn-end-command))
  (if (emacspeak-wizards-vpn-status)
      (shell-command
       (format "%s &"
	       emacspeak-wizards-vpn-end-command))
    (shell-command 
     (format  "%s&"
	      emacspeak-wizards-vpn-start-command))))

;;}}}
;;;###autoload 
(defun emacspeak-wizards-edit-file-as-root (filename)
  "Edit file as root using sudo vi.
See /etc/sudoers for how to set up sudo."
  (interactive
   (list
    (read-file-name "Edit file as root: ")))
  (require 'term)
  (delete-other-windows)
  (switch-to-buffer
   (term-ansi-make-term
    (generate-new-buffer-name
     (format "vi-%s"
             (file-name-nondirectory filename)))
    "sudo" 
    nil
    "vi" 
    filename))
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ edit file as root using sudo vi 
(defun emacspeak-wizards-vi-as-su-file (file)
  "Launch sudo vi on specified file in a terminal."
  (interactive
   (list
    (expand-file-name
     (read-file-name "SU Edit File: "))))
  (require 'term)
  (delete-other-windows)
  (switch-to-buffer
   (term-ansi-make-term
    (generate-new-buffer-name
     (format "vi-%s"
             (file-name-nondirectory file)))
    "sudo"
    nil
    "vi"
    file))
  (emacspeak-eterm-record-window   1 
                                   (cons 0 1)
                                   (cons 79 20)
                                   'right-stretch 'left-stretch)
  (emacspeak-eterm-set-filter-window 1)
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-line))

;;}}}
;;{{{ setup CVS access to sourceforge 

(defcustom emacspeak-cvs-local-directory
  (expand-file-name "~/sourceforge/cvs-emacspeak")
  "Directory where we download the snapshot."
  :type 'directory
  :group 'emacspeak-wizards)

(defun emacspeak-cvs-done-alert (process state)
  "Alert user of cvs status."
  (message "Done getting CVS snapshot.")
  (emacspeak-auditory-icon 'task-done))

(defcustom emacspeak-cvs-anonymous-cvsroot
  ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/emacspeak"
  "CVSROOT for emacspeak CVS repository at sourceforge."
  :type 'string
  :group 'emacspeak-wizards)
;;;###autoload
(defun emacspeak-cvs-get-anonymous  ()
  "Get latest cvs snapshot of emacspeak."
  (interactive)
  (declare (special emacspeak-cvs-local-directory
                    emacspeak-cvs-anonymous-cvsroot))
  (unless (file-exists-p emacspeak-cvs-local-directory)
    (make-directory emacspeak-cvs-local-directory 'parents))
  (cd emacspeak-cvs-local-directory)
  (let ((cvs-process nil))
    (setq cvs-process
          (start-process "cvs" "*cvs-emacspeak*" "cvs"
                         (format "-d%s"
                                 emacspeak-cvs-anonymous-cvsroot)
                         "login"))
    (process-send-string cvs-process "\n\n\n")
    (cond
     ((file-exists-p
       (expand-file-name "emacspeak/CVS"
                         emacspeak-cvs-local-directory))
      (cd (expand-file-name "emacspeak"
                            emacspeak-cvs-local-directory))
      (setq cvs-process
            (start-process "cvs" "*cvs-emacspeak*" "cvs"
                           (format "-d%s"
                                   emacspeak-cvs-anonymous-cvsroot)
                           "-z3 -q"
                           "update"
                           "-d")))
     (t
      (setq cvs-process
            (start-process "cvs" "*cvs-emacspeak*" "cvs"
                           (format "-d%s"
                                   emacspeak-cvs-anonymous-cvsroot)
                           "-z3"
                           "co"
                           "emacspeak"))))
    (set-process-sentinel cvs-process
                          'emacspeak-cvs-done-alert)))

(defvar emacspeak-cvs-sf-anonymous-cvsroot-pattern
  ":pserver:anonymous@cvs.sourceforge.net:/cvsroot/%s"
  "CVSROOT pattern for project CVS repository at
sourceforge.
Typically %s is replaced by project name.")

(defvar emacspeak-cvs-gnu-anonymous-cvsroot-pattern
  ":pserver:anoncvs@subversions.gnu.org:/cvsroot/%s"
  "CVSROOT pattern for project CVS repository at
GNU.
Typically %s is replaced by project name.")

(defcustom emacspeak-cvs-local-directory-pattern
  "~/sourceforge/cvs-%s"
  "Pattern from which name of local download directory is build.
 %s is replaced by the project name."
  :type 'string
  :group 'emacspeak-wizards)
;;;###autoload
(defun emacspeak-cvs-sf-get-project-snapshot  (project)
  "Grab CVS snapshot  of specified project from Sourceforge."
  (interactive
   (list
    (read-string "Project name: ")))
  (declare (special emacspeak-cvs-local-directory-pattern
                    emacspeak-cvs-sf-anonymous-cvsroot-pattern))
  (let ((cvsroot
         (format emacspeak-cvs-sf-anonymous-cvsroot-pattern project))
        (dir (expand-file-name
              (format emacspeak-cvs-local-directory-pattern
                      project))))
    (unless (file-exists-p dir)
      (make-directory dir 'parents))
    (cd dir)
    (let ((cvs-process nil))
      (setq cvs-process
            (start-process "cvs" "*cvs-download*" "cvs"
                           (format "-d%s"
                                   cvsroot)
                           "login"))
      (accept-process-output cvs-process)
      (process-send-string cvs-process "\n\n\n")
      (cond
       ((file-exists-p
         (expand-file-name
          (format "%s/CVS" project)
          dir))
        (cd (expand-file-name project
                              dir))
        (setq cvs-process
              (start-process "cvs" "*cvs-download*" "cvs"
                             (format "-d%s"
                                     cvsroot)
                             "-z3 -q"
                             "update"
                             "-d")))
       (t
        (setq cvs-process
              (start-process "cvs" "*cvs-download*" "cvs"
                             (format "-d%s"
                                     cvsroot)
                             "-z3 -q"
                             "co"
                             project))))
      (set-process-sentinel cvs-process
                            'emacspeak-cvs-done-alert))))
;;;###autoload
(defun emacspeak-cvs-gnu-get-project-snapshot  (project)
  "Grab CVS snapshot  of specified project from GNU."
  (interactive
   (list
    (read-string "Project name: ")))
  (declare (special emacspeak-cvs-local-directory-pattern
                    emacspeak-cvs-gnu-anonymous-cvsroot-pattern))
  (let ((cvsroot
         (format emacspeak-cvs-gnu-anonymous-cvsroot-pattern
		 project))
        (dir (expand-file-name
              (format emacspeak-cvs-local-directory-pattern
                      project))))
    (unless (file-exists-p dir)
      (make-directory dir 'parents))
    (cd dir)
    (let ((cvs-process nil))
      (setq cvs-process
            (start-process "cvs" "*cvs-download*" "cvs"
                           (format "-d%s"
                                   cvsroot)
                           "login"))
      (accept-process-output cvs-process)
      (process-send-string cvs-process "\n\n\n")
      (cond
       ((file-exists-p
         (expand-file-name
          (format "%s/CVS" project)
          dir))
        (cd (expand-file-name project
                              dir))
        (setq cvs-process
              (start-process "cvs" "*cvs-download*" "cvs"
                             (format "-d%s"
                                     cvsroot)
                             "-z3 -q"
                             "update"
                             "-d")))
       (t
        (setq cvs-process
              (start-process "cvs" "*cvs-download*" "cvs"
                             (format "-d%s"
                                     cvsroot)
                             "-z3 -q"
                             "co"
                             project))))
      (set-process-sentinel cvs-process
                            'emacspeak-cvs-done-alert))))

;;}}}
;;{{{ browse chunks 
(defun emacspeak-wizards-move-and-speak (command count)
  "Speaks a chunk of text bounded by point and a target position.
Target position is specified using a navigation command and a
count that specifies how many times to execute that command
first.
Point is left at the target position.
Interactively, command is specified by pressing the key that
;;invokes the command."
  (interactive
   (list
    (lookup-key global-map
                (read-key-sequence "Key:"))
    (read-minibuffer "Count:")))
  (let ((orig (point)))
    (push-mark orig)
    (funcall command count)
    (emacspeak-speak-region orig (point))))

;;}}}
;;{{{  Learn mode
;;;###autoload
(defun emacspeak-learn-mode ()
  "Helps you learn the keys.  You can press keys and hear what they do.
To leave, press \\[keyboard-quit]."
  (interactive)
  (let ((continue t ))
    (while continue
      (call-interactively 'describe-key-briefly)
      (if (= last-input-event 7)
          (setq continue nil )))
    (message "Leaving learn mode ")))

;;}}}
;;{{{  Generate documentation:
(defsubst ems-variable-symbol-file (o)
  "Locate file that defines a variable."
  (or (symbol-file o)
      (symbol-file (cons 'defvar o))))

(defsubst emacspeak-list-emacspeak-options ()
  "List all Emacspeak customizable options."
  (let ((options nil ))
    (mapatoms
     #'(lambda (symbol)
         (when
             (and (symbolp symbol)
                  (get symbol 'custom-type)
                  (or (string-match "emacspeak" (symbol-name symbol))
                      (string-match "cd-tool" (symbol-name symbol))
                      (string-match "dtk" (symbol-name symbol))
                      (string-match "voice" (symbol-name symbol))
                      (string-match "tts" (symbol-name symbol))))
           (push symbol options))))
    (setq options
          (sort
           options
           #'(lambda (a b )
               (cond
		((string-lessp
		  (ems-variable-symbol-file a)
		  (ems-variable-symbol-file b))
		 t)
		((string-equal (ems-variable-symbol-file a)
			       (ems-variable-symbol-file b))
		 (string-lessp a b))
		(t nil)))))
    options))
           

(defsubst emacspeak-list-emacspeak-commands ()
  "List all Emacspeak commands."
  (let ((commands nil ))
    (mapatoms
     (function
      (lambda (f)
        (when
            (and (fboundp f)
                 (commandp f)
                 (not (string-match "ad-Orig" (symbol-name f)))
                 (not (eq f 'emacspeak))
                 (or (string-match "emacspeak" (symbol-name f))
                     (string-match "cd-tool" (symbol-name f))
                     (string-match "dtk" (symbol-name f))
                     (string-match "voice-setup" (symbol-name f))
                     (string-match "dtk" (symbol-name f))))
          (push f commands)))))
    (setq commands
          (sort commands
                #'(lambda (a b )
                    (cond
                     ((string-lessp
		       (symbol-file a)
		       (symbol-file b))
                      t)
                     ((string-equal (symbol-file a)
                                    (symbol-file b))
                      (string-lessp a b))
                     (t nil)))))
    commands))
;;;###autoload
(defun emacspeak-generate-documentation (filename)
  "Generate docs for all emacspeak commands.
Prompts for FILENAME in which to save the documentation.
Warning! Contents of file filename will be overwritten."
  (interactive "FEnter filename to save DOC in: ")
  (let ((buffer (find-file-noselect filename)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert "DOC --- Automatically generated by command emacspeak-generate-documentation\n\$Id$\n")
      (mapcar
       (function
        (lambda (f)
          (let ((key (where-is-internal f)))
            (insert "------------------------------------------------------------\n\n")
            (insert (format "** %s" f))
            (if key
                (condition-case nil
                    (insert (format "\tKey Sequence:%s\n\n"
                                    (mapconcat
                                     'key-description
                                     key " ")))
                  (error nil)))
            (insert
             (or (documentation f)
                 ""))
            (insert "\n\n"))))
       (emacspeak-list-emacspeak-commands))
      (goto-char (point-max))
      (insert "  \n\nLocal variables:
 mode: outline\nparagraph-separate: \"[ ]*$\"
end:\n\n")
      (save-buffer)))
  (emacspeak-auditory-icon 'task-done))

(defsubst ems-cleanup-commentary (commentary )
  "Cleanup commentary."
  (save-excursion
    (set-buffer
     (get-buffer-create " *doc-temp*"))
    (erase-buffer)
    (insert commentary)
    (goto-char (point-min))
    (flush-lines "{{{")
    (goto-char (point-min))
    (flush-lines "}}}")
    (goto-char (point-min))
    (delete-blank-lines)
    (goto-char (point-min))
    (while (re-search-forward "^;+ ?" nil t)
      (replace-match "" nil nil))
    (buffer-string)))

(defsubst ems-texinfo-escape (string)
  "Escape texinfo special chars"
  (save-excursion
    (set-buffer (get-buffer-create " *doc-temp*"))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[{}@]" nil t)
      (replace-match "@\\&"))
    (buffer-string)))
;;;###autoload
(defun emacspeak-generate-texinfo-command-documentation (filename)
  "Generate texinfo documentation  for all emacspeak
commands  into file commands.texi.
Warning! Contents of file commands.texi will be overwritten."
  (interactive "FEnter filename to save commands documentation  in: ")
  (let ((emacspeak-speak-messages nil)
        (dtk-quiet t)
        (buffer (find-file-noselect filename))
        (module nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert"@c $Id$\n")
      (insert
       "@node Emacspeak Commands\n@chapter Emacspeak Commands\n\n")
      (insert
       (format 
	"This chapter is generated automatically from the source-level documentation.
Any errors or corrections should be made to the source-level
documentation.
This chapter documents a total of %d commands.\n\n"
	(length (emacspeak-list-emacspeak-commands))))
      (mapcar
       (function
        (lambda (f)
          (let ((key (where-is-internal f))
                (key-description nil)
                (commentary nil)
                (this-module (symbol-file f))
                (source-file nil))
            (when this-module
              (setq source-file (locate-library this-module ))
	      (if (char-equal (aref source-file (1- (length source-file))) ?c)
		  (setq source-file (substring  source-file 0 -1)))
              (setq commentary (lm-commentary source-file))
              (setq this-module
                    (file-name-sans-extension this-module))
              (when commentary
                (setq commentary 
                      (ems-cleanup-commentary commentary)))
              (setq this-module
                    (file-name-nondirectory this-module)))
            (unless (string-equal module this-module)
              (if this-module 
                  (setq module this-module)
                (setq module nil))
              (when module 
                (insert
                 (format
                  "@node %s\n@section %s\n\n\n"
                  module module )))
              (insert
               (format "\n\n%s\n\n" 
                       (or commentary "")))
              (insert
               (format
                "Automatically generated documentation
for commands defined in module  %s.\n\n"
                module)))
            (insert (format "\n\n@deffn {Interactive Command} %s %s\n"
                            f
                            (eldoc-function-argstring f)))
            (if key
                (condition-case nil
                    (progn
                      (setq key-description
                            (ems-texinfo-escape
                             (mapconcat
                              'key-description
                              key " ")))
                      (insert
                       (format "@kbd{%s}\n\n"
                               key-description)))
                  (error nil)))
            (insert
             (or
              (ems-texinfo-escape
               (documentation f))
              ""))
            (insert "\n@end deffn\n\n"))))
       (emacspeak-list-emacspeak-commands))
      (emacspeak-url-template-generate-texinfo-documentation (current-buffer))
      (texinfo-all-menus-update)
      (shell-command-on-region (point-min) (point-max)
			       "cat -s"
			       (current-buffer)
			       'replace)
      (save-buffer)))
  (emacspeak-auditory-icon 'task-done))

(defun emacspeak-generate-texinfo-option-documentation (filename)
  "Generate texinfo documentation  for all emacspeak
options  into file filename.
Warning! Contents of file filename will be overwritten."
  (interactive "FEnter filename to save options documentation in: ")
  (let ((emacspeak-speak-messages nil)
        (dtk-quiet t)
        (buffer (find-file-noselect filename))
        (module nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert"@c $Id$\n")
      (insert
       "@node Emacspeak Customizations\n@chapter Emacspeak Customizations \n\n")
      (insert
       (format 
	"This chapter is generated automatically from the source-level documentation.
Any errors or corrections should be made to the source-level
documentation.
This chapter documents a total of %d user customizable
  options.\n\n"
	(length (emacspeak-list-emacspeak-options))))
      (mapcar
       #'(lambda (o)
	   (let ((this-module (ems-variable-symbol-file  o))
		 (commentary nil)
		 (source-file nil))
	     (when this-module
	       (setq source-file (locate-library this-module ))
	       (if (char-equal (aref source-file (1- (length source-file))) ?c)
		   (setq source-file (substring  source-file 0 -1)))
	       (setq commentary (lm-commentary source-file))
	       (setq this-module
		     (file-name-sans-extension this-module))
	       (when commentary
		 (setq commentary 
		       (ems-cleanup-commentary commentary)))
	       (setq this-module
		     (file-name-nondirectory this-module)))
	     (unless (string-equal module this-module)
	       (if this-module 
		   (setq module this-module)
		 (setq module nil))
	       (when module 
		 (insert
		  (format
		   "@node %s Options\n@section %s Options\n\n\n"
		   module module )))
	       (insert
		(format "\n\n%s\n\n" 
			(or commentary "")))
	       (insert
		(format
		 "Automatically generated documentation
for options defined in module  %s.
These options are customizable via Emacs' Custom interface.\n\n"
		 module)))
	     (insert (format "\n\n@defvar {User Option} %s\n"
			     o))
	     (insert
	      (or
	       (ems-texinfo-escape
		(documentation-property  o 'variable-documentation))                        
	       ""))
	     (insert "\n@end defvar\n\n")))
       (emacspeak-list-emacspeak-options))
      (texinfo-all-menus-update)
      (shell-command-on-region (point-min) (point-max)
			       "cat -s"
			       (current-buffer)
			       'replace)
      (save-buffer)))
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ labelled frames
;;;###autoload
(defsubst emacspeak-frame-read-frame-label ()
  "Read a frame label with completion."
  (interactive)
  (let* ((frame-names-alist (make-frame-names-alist))
         (default (car (car frame-names-alist)))
         (input (completing-read
                 (format "Select Frame (default %s): " default)
                 frame-names-alist nil t nil 'frame-name-history)))
    (if (= (length input) 0)
        default)))

(defun emacspeak-frame-label-or-switch-to-labelled-frame (&optional prefix)
  "Switch to labelled frame.
With optional PREFIX argument, label current frame."
  (interactive "P")
  (cond
   (prefix
    (call-interactively 'set-frame-name))
   (t (call-interactively 'select-frame-by-name)))
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))

;;;###autoload
(defun emacspeak-next-frame-or-buffer (&optional frame)
  "Move to next buffer.
With optional interactive prefix arg `frame', move to next frame instead."
  (interactive "P")
  (cond
   (frame
    (other-frame 1)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line))
   (t
    (bury-buffer)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-current-buffer-name))))
;;;###autoload
(defun emacspeak-previous-frame-or-buffer (&optional frame)
  "Move to previous buffer.
With optional interactive prefix arg `frame', move to previous frame instead."
  (interactive "P")
  (cond
   (frame
    (other-frame -1)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line))
   (t
    (let ((l
           (remove-if
            #'(lambda (b)
                (string-equal (substring
			       (buffer-name b)
			       0 1) " "))
            (buffer-list))))
      (switch-to-buffer (nth (1- (length l))
                             l))
      (emacspeak-auditory-icon 'select-object)
      (emacspeak-speak-current-buffer-name)))))

;;}}}
;;{{{  readng different displays of same buffer
;;;###autoload
(defun emacspeak-speak-this-buffer-other-window-display (&optional arg)
  "Speak this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
listen to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to speak.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to speak."
  (interactive "P")
  (let ((window
         (or arg
             (condition-case nil
                 (read (format "%c" last-input-event ))
               (error nil ))))
        (win nil)
        (window-list (get-buffer-window-list
                      (current-buffer)
                      nil 'visible)))
    (or (numberp window)
        (setq window
              (read-minibuffer "Display    to speak")))
    (setq win
          (nth (% window (length window-list ))
               window-list))
    (save-excursion
      (save-window-excursion
        (emacspeak-speak-region
         (window-point win)
         (window-end win))))))
;;;###autoload
(defun emacspeak-speak-this-buffer-previous-display ()
  "Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `previous'."
  (interactive)
  (let ((count (length (get-buffer-window-list
                        (current-buffer)
                        nil 'visible))))
    (emacspeak-speak-this-buffer-other-window-display (1-  count))))
;;;###autoload
(defun emacspeak-speak-this-buffer-next-display ()
  "Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `next'."
  (interactive)
  (emacspeak-speak-this-buffer-other-window-display  1))
;;;###autoload
(defun emacspeak-select-this-buffer-other-window-display (&optional arg)
  "Switch  to this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
move to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to select.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to select."
  (interactive "P")
  (let ((window
         (or arg
             (condition-case nil
                 (read (format "%c" last-input-event ))
               (error nil ))))
        (win nil)
        (window-list (get-buffer-window-list
                      (current-buffer)
                      nil 'visible)))
    (or (numberp window)
        (setq window
              (read-minibuffer "Display to select")))
    (setq win
          (nth (% window (length window-list ))
               window-list))
    (select-frame (window-frame win))
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'select-object)))
;;;###autoload
(defun emacspeak-select-this-buffer-previous-display ()
  "Select this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `previous'."
  (interactive)
  (let ((count (length (get-buffer-window-list
                        (current-buffer)
                        nil 'visible))))
    (emacspeak-select-this-buffer-other-window-display (1-  count))))
;;;###autoload
(defun emacspeak-select-this-buffer-next-display ()
  "Select this buffer as displayed in a `next' frame.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `next'."
  (interactive)
  (emacspeak-select-this-buffer-other-window-display  1))

;;}}}
;;{{{ emacspeak clipboard

(eval-when (load)
  (condition-case nil
      (unless (file-exists-p emacspeak-resource-directory)
        (make-directory emacspeak-resource-directory))
    (error (message "Make sure you have an Emacspeak resource directory %s"
                    emacspeak-resource-directory))))

(defcustom emacspeak-clipboard-file
  (concat emacspeak-resource-directory "/" "clipboard")
  "File used to save Emacspeak clipboard.
The emacspeak clipboard provides a convenient mechanism for exchanging
information between different Emacs sessions."
  :group 'emacspeak-speak
  :type 'string)
;;;###autoload
(defun emacspeak-clipboard-copy (start end &optional prompt)
  "Copy contents of the region to the emacspeak clipboard.
Previous contents of the clipboard will be overwritten.  The Emacspeak
clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg results in Emacspeak
prompting for the clipboard file to use.
Argument START and END specifies  region.
Optional argument PROMPT  specifies whether we prompt for the name of a clipboard file."
  (interactive "r\nP")
  (declare (special emacspeak-resource-directory emacspeak-clipboard-file))
  (let ((clip (buffer-substring-no-properties start end ))
        (clipboard-file
         (if prompt
             (read-file-name "Copy region to clipboard file: "
                             emacspeak-resource-directory
                             emacspeak-clipboard-file)
           emacspeak-clipboard-file))
        (clipboard nil))
    (setq clipboard (find-file-noselect  clipboard-file))
    (let ((emacspeak-speak-messages nil))
      (save-excursion
        (set-buffer clipboard)
        (erase-buffer)
        (insert clip)
        (save-buffer)))
    (message "Copied %s lines to Emacspeak clipboard %s"
             (count-lines start end)
             clipboard-file)))
;;;###autoload
(defun emacspeak-clipboard-paste (&optional paste-table)
  "Yank contents of the Emacspeak clipboard at point.
The Emacspeak clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg pastes from
the emacspeak table clipboard instead."
  (interactive "P")
  (declare (special emacspeak-resource-directory emacspeak-clipboard-file))
  (let ((start (point))
        (clipboard-file emacspeak-clipboard-file))
    (cond
     (paste-table  (emacspeak-table-paste-from-clipboard))
     (t(insert-file-contents clipboard-file)
       (exchange-point-and-mark)))
    (message "Yanked %s lines from  Emacspeak clipboard %s"
             (count-lines start (point))
             (if paste-table "table clipboard"
               clipboard-file))))

;;}}}
;;{{{ utilities
;;;###autoload
(defun emacspeak-wizards-show-list-variable (var)
  "Convenience command to view Emacs variables that are long lists.
Prompts for a variable name and displays its value in a separate buffer.
Lists are displayed one element per line.
Argument VAR specifies variable whose value is to be displayed."
  (interactive "SDisplay variable:")
  (let ((buffer 
         (format "*emacspeak:%s*" var))
        (symbol (symbol-value var)))
    (with-output-to-temp-buffer buffer
      (prin1 symbol))
    (save-excursion
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "(" nil t)
        (replace-match "\n("))
      (goto-char (point-min))
      (fill-paragraph 'justify)
      (indent-sexp)
      (emacs-lisp-mode))
    (pop-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))
;;;###autoload
(defun emacspeak-speak-show-memory-used ()
  "Convenience command to view state of memory used in this session so far."
  (interactive)
  (let ((buffer (get-buffer-create "*emacspeak-memory*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert
       (apply 'format
              "Memory Statistics
 cons cells:\t%d
 floats:\t%d
 vectors:\t%d
 symbols:\t%d
 strings:\t%d
 miscellaneous:\t%d
 integers:\t%d\n"
              (memory-use-counts)))
      (insert  "\nInterpretation of these statistics:\n")
      (insert (documentation 'memory-use-counts))
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ emergency tts restart 

(defcustom emacspeak-emergency-tts-server
  "dtk-exp"
  "TTS server to use in an emergency.
Set this to a TTS server that is known to work at all times.
If you are debugging another speech server and that server
gets wedged for some reason,
you can use command emacspeak-emergency-tts-restart
to get speech back using the reliable TTS server.
It's useful to bind the above command to a convenient key."
  :type 'string
  :group 'emacspeak)
;;;###autoload
(defun emacspeak-emergency-tts-restart ()
  "For use in an emergency.
Will start TTS engine specified by 
emacspeak-emergency-tts-server."
  (interactive)
  (declare (special emacspeak-emergency-tts-server))
  (dtk-select-server emacspeak-emergency-tts-server)
  (dtk-initialize))

(defcustom emacspeak-ssh-tts-server
  "ssh-dtk-exp"
  "SSH TTS server to use by default."
  :type 'string
  :group 'emacspeak)

;;;###autoload
(defun emacspeak-ssh-tts-restart ()
  "Restart specified ssh tts server."
  (interactive)
  (declare (special emacspeak-ssh-tts-server))
  (dtk-select-server emacspeak-ssh-tts-server)
  (dtk-initialize))

;;}}}
;;{{{ customization wizard
;;;###autoload
(defun emacspeak-customize-personal-settings (file)
  "Create a customization buffer for browsing and updating
personal customizations."
  (interactive
   (list
    (read-file-name "Customization file: "
                    nil
                    custom-file)))
  (declare (special custom-file))
  (let* ((buffer (find-file-noselect custom-file))
	 (settings  
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-min))
	    (cdr (read  buffer))))
	 (found nil))
    (setq found
          (mapcar #'(lambda (s)
                      (list (car (second s))
                            'custom-variable))
                  settings))
    (custom-buffer-create (custom-sort-items found t 'first)
                          "*Customize Personal Options*")))

;;}}}
;;{{{  Display properties conveniently

;;; Useful for developping emacspeak:
;;; Display selected properties of interest

(defvar emacspeak-property-table
  '(("personality"  . "personality")
    ("auditory-icon" . "auditory-icon")
    ("action" . "action"))
  "Properties emacspeak is interested in.")
;;;###autoload
(defun emacspeak-show-personality-at-point ()
  "Show value of property personality (and possibly face)
at point."
  (interactive )
  (let ((p (get-text-property (point) 'personality))
        (f (get-text-property (point) 'face))
        (o
         (delq nil
	       (mapcar
		#'(lambda (overlay)
		    (overlay-get overlay 'face))
		(overlays-at (point))))))
    (message "Personality %s Face %s %s" p f
	     (if o
		 o
	       " "))))

;;;###autoload
(defun emacspeak-show-property-at-point (&optional property )
  "Show value of PROPERTY at point.
If optional arg property is not supplied, read it interactively.
Provides completion based on properties that are of interest.
If no property is set, show a message and exit."
  (interactive
   (let
       ((properties (text-properties-at  (point))))
     (cond
      ((and properties
            (= 2 (length properties )))
       (list (car properties )))
      (properties
       (list
        (intern
         (completing-read  "Display property: "
                           emacspeak-property-table ))))
      (t (message "No property set at point ")
         nil))))
  (declare (special emacspeak-property-table))
  (if property
      (message"%s"
              (get-text-property (point) property ))))

;;}}}
;;{{{  moving across blank lines
;;;###autoload
(defun emacspeak-skip-blank-lines-forward ()
  "Move forward across blank lines.
The line under point is then spoken.
Signals end of buffer."
  (interactive)
  (let ((save-syntax (char-syntax 10))
        (start (point))
        (newlines nil)
        (voice-lock-mode t)
        (skipped nil)
        (skip 0))
    (unwind-protect
        (progn
          (modify-syntax-entry   10 " ")
          (end-of-line)
          (setq skip (skip-syntax-forward " "))
          (cond
           ((zerop skip)
            (message "Did not move "))
           ((eobp)
            (message "At end of buffer"))
           (t
            (beginning-of-line)
            (setq newlines (1-   (count-lines start (point))))
            (when (>  newlines 0)
              (setq skipped
                    (format "skip %d " newlines))
              (put-text-property  0 (length skipped)
                                  'personality
                                  voice-annotate skipped))
            (emacspeak-auditory-icon 'select-object)
            (dtk-speak
             (concat skipped
                     (thing-at-point 'line))))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))
;;;###autoload
(defun emacspeak-skip-blank-lines-backward ()
  "Move backward  across blank lines.
The line under point is   then spoken.
Signals beginning  of buffer."
  (interactive)
  (let ((save-syntax (char-syntax 10))
        (voice-lock-mode t)
        (newlines nil)
        (start (point))
        (skipped nil)
        (skip 0))
    (unwind-protect
        (progn
          (modify-syntax-entry   10 " ")
          (beginning-of-line)
          (setq skip (skip-syntax-backward " "))
          (cond
           ((zerop skip)
            (message "Did not move "))
           ((bobp )
            (message "At start  of buffer"))
           (t
            (beginning-of-line)
            (setq newlines (1- (count-lines start (point))))
            (when (> newlines 0)
              (setq skipped  (format "skip %d " newlines))
              (put-text-property  0 (length skipped)
                                  'personality
                                  voice-annotate skipped))
            (emacspeak-auditory-icon 'select-object)
            (dtk-speak
             (concat skipped
                     (thing-at-point 'line))))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))

;;}}}
;;{{{  launch lynx 
(defcustom emacspeak-wizards-links-program "links"
  "Name of links executable."
  :type 'file
  :group 'emacspeak-wizards)

;;;###autoload
(defun emacspeak-links (url)
  "Launch links on  specified URL in a new terminal."
  (interactive
   (list
    (read-from-minibuffer "URL: ")))
  (declare (special emacspeak-wizards-links-program))
  (require 'term)
  (delete-other-windows)
  (switch-to-buffer
   (term-ansi-make-term
    (generate-new-buffer-name
     (format "links-%s"
             (substring url 7)))
    emacspeak-wizards-links-program
    nil
    url))
  (emacspeak-eterm-record-window   1 
                                   (cons 0 1)
                                   (cons 79 20)
                                   'right-stretch 'left-stretch)
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object))

(defcustom emacspeak-wizards-lynx-program
  "lynx"
  "Lynx executable."
  :type 'file
  :group 'emacspeak-wizards)

;;;###autoload
(defun emacspeak-lynx (url)
  "Launch lynx on  specified URL in a new terminal."
  (interactive
   (list
    (read-from-minibuffer "URL: ")))
  (declare (special emacspeak-wizards-lynx-program
                    term-height term-width))
  (require 'term)
  (delete-other-windows)
  (switch-to-buffer
   (term-ansi-make-term
    (generate-new-buffer-name
     (format "lynx-%s"
             (substring url 7)))
    emacspeak-wizards-lynx-program
    nil
    "-show-cursor=yes"
    url))
  (emacspeak-eterm-record-window   1 
                                   (cons 0 1)
                                   (cons 
(- term-width 1)
(- term-height 1))
                                   'right-stretch 'left-stretch)
  (emacspeak-eterm-set-filter-window 1)
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object))

(defcustom emacspeak-wizards-curl-program "curl"
"Name of curl executable."
:type 'string
:group 'emacspeak-wizards)

(defun emacspeak-curl (url)
  "Grab URL using Curl, and preview it with W3."
  (interactive
   (list
    (read-from-minibuffer "URL: ")))
  (declare (special emacspeak-wizards-curl-program))
  (let ((results (get-buffer-create " *curl-download* ")))
    (shell-command
     (format "curl %s 2>/dev/null" url)
     results)
    (switch-to-buffer results)
    (emacspeak-w3-preview-this-buffer)
    (kill-buffer results)))


  (emacspeak-eterm-record-window   1 
                                   (cons 0 1)
                                   (cons 
(- term-width 1)
(- term-height 1))
                                   'right-stretch 'left-stretch)
  (emacspeak-eterm-set-filter-window 1)
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ ansi term 
;;;###AUTOLOAD
(defun emacspeak-wizards-terminal (program)
  "Launch terminal and rename buffer appropriately."
  (interactive (list (read-from-minibuffer "Run program: ")))
  (ansi-term program
             (first (split-string program  )))
  (delete-other-windows)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{ table wizard
(defvar emacspeak-etc-directory
  (expand-file-name  "etc/" emacspeak-directory)
  "Directory containing miscellaneous files  for Emacspeak.")

(declaim (special emacspeak-etc-directory))
(defvar emacspeak-wizards-table-content-extractor
  (expand-file-name "extract-table.pl" emacspeak-etc-directory)
  "Program that extracts table content.")
;;;###autoload
(defun emacspeak-wizards-get-table-content-from-url (task url depth count )
  "Extract table specified by depth and count from HTML
content at URL.
Extracted content is placed as a csv file in task.csv."
  (interactive
   (list
    (read-from-minibuffer "Task:"
                          "table")
    (read-from-minibuffer "URL: ")
    (read-from-minibuffer "Depth: ")
    (read-from-minibuffer "Count: ")))
  (declare (special
            emacspeak-wizards-table-content-extractor))
  (let ((output (format "/tmp/%s.csv" task)))
    (shell-command
     (format  "%s --task=%s --url='%s' --depth=%s --count=%s"
              emacspeak-wizards-table-content-extractor
              task
              url
              depth count ))
    (emacspeak-table-find-csv-file output)
    (delete-file output)))
;;;###autoload
(defun emacspeak-wizards-get-table-content-from-file (task file depth count )
  "Extract table specified by depth and count from HTML
content at file.
Extracted content is placed as a csv file in task.csv."
  (interactive
   (list
    (read-from-minibuffer "Task:"
                          "table")
    (read-file-name "File: ")
    (read-from-minibuffer "Depth: ")
    (read-from-minibuffer "Count: ")))
  (declare (special emacspeak-wizards-table-content-extractor))
  (shell-command
   (format  "%s --task=%s --file=%s --depth=%s --count=%s"
            emacspeak-wizards-table-content-extractor
            task file depth count ))
  (emacspeak-table-find-csv-file (format "/tmp/%s.csv" task)))

;;}}}
;;{{{ annotation wizard

;;; I use this to collect my annotations into a buffer
;;; e.g. an email message to be sent out--
;;; while reading and commenting on large documents.

(defsubst emacspeak-annotate-make-buffer-list  (&optional buffer-list)
  "Returns names from BUFFER-LIST excluding those beginning with a space."
  (let (buf-name)
    (delq nil (mapcar
               (function
                (lambda (b)
                  (setq buf-name (buffer-name b))
                  (and (stringp buf-name)
                       (/= (length buf-name) 0)
                       (/= (aref buf-name 0) ?\ )
                       b)))
               (or buffer-list
                   (buffer-list))))))

(defvar emacspeak-annotate-working-buffer nil
  "Buffer that annotations go to.")

(make-variable-buffer-local 'emacspeak-annotate-working-buffer)

(defvar emacspeak-annotate-edit-buffer
  "*emacspeak-annotation*"
  "Name of temporary buffer used to edit the annotation.")

(defun emacspeak-annotate-get-annotation ()
  "Pop up a temporary buffer and collect the annotation."
  (declare (special emacspeak-annotate-edit-buffer))
  (let ((annotation nil))
    (pop-to-buffer
     (get-buffer-create emacspeak-annotate-edit-buffer))
    (erase-buffer)
    (message "Exit recursive edit when done.")
    (recursive-edit)
    (local-set-key "\C-c\C-c" 'exit-recursive-edit)
    (setq annotation (buffer-string))
    (bury-buffer)
    annotation))
;;;###autoload
(defun emacspeak-annotate-add-annotation (&optional reset)
  "Add annotation to the annotation working buffer.
Prompt for annotation buffer if not already set.
Interactive prefix arg `reset' prompts for the annotation
buffer even if one is already set.
Annotation is entered in a temporary buffer and the
annotation is inserted into the working buffer when complete."
  (interactive "P")
  (declare (special emacspeak-annotate-working-buffer))
  (when  (or reset
             (null emacspeak-annotate-working-buffer))
    (setq emacspeak-annotate-working-buffer
          (get-buffer-create
           (read-buffer "Annotation working buffer: "
                        (cadr
                         (emacspeak-annotate-make-buffer-list))))))
  (let ((annotation nil)
        (work-buffer emacspeak-annotate-working-buffer)
        (parent-buffer (current-buffer)))
    (message "Adding annotation to %s"
             emacspeak-annotate-working-buffer)
    (save-window-excursion
      (save-excursion
        (setq annotation
              (emacspeak-annotate-get-annotation))
        (set-buffer work-buffer)
        (insert annotation)
        (insert "\n"))
      (switch-to-buffer parent-buffer))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ shell-toggle

;;; inspired by eshell-toggle 
;;; switch to the shell buffer, and cd to the directory 
;;; that is the default-directory for the previously current
;;; buffer.
;;;###autoload
(defun emacspeak-wizards-shell-toggle ()
  "Switch to the shell buffer and cd to 
 the directory of the current buffer."
  (interactive )
  (declare (special default-directory))
  (let ((dir default-directory))
    (shell)
    (unless (string-equal (expand-file-name dir)
                          (expand-file-name
                           default-directory))
      (goto-char (point-max))
      (insert (format "pushd %s" dir))
      (comint-send-input)
      (shell-process-cd dir))
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{  run rpm -qi on current dired entry
;;;###autoload
(defun emacspeak-wizards-rpm-query-in-dired ()
  "Run rpm -qi on current dired entry."
  (interactive)
  (declare (special major-mode))
  (unless (eq major-mode 'dired-mode)
    (error "This command should be used in dired mode."))
  (shell-command
   (format "rpm -qi ` rpm -qf %s`"
           (dired-get-filename 'no-location)))
  (other-window 1)
  (search-forward "Summary" nil t)
  (emacspeak-speak-line))

(declaim (special dired-mode-map))
(when (boundp 'dired-mode-map)
  (define-key dired-mode-map "r" 'emacspeak-wizards-rpm-query-in-dired))
;;}}}
;;{{{ auto mode alist utility

(defsubst emacspeak-wizards-augment-auto-mode-alist (ext mode)
  "Add to auto-mode-alist."
  (declare (special auto-mode-alist))
  (setq auto-mode-alist
        (cons
         (cons ext mode)
         auto-mode-alist)))

;;}}}
;;{{{ xl wizard

;;;

(define-derived-mode emacspeak-wizards-xl-mode text-mode
  "Browsing XL Files."
  "Major mode for browsing XL spreadsheets.\n\n
XL Sheets are converted to HTML and previewed using W3."
  (emacspeak-wizards-xl-display))

(defcustom emacspeak-wizards-xlhtml-program "xlhtml"
  "Program for converting XL to HTML.
Set this to nil if you do not want to use the XLHTML wizard."
  :type 'string
  :group 'emacspeak-wizards)

(defvar emacspeak-wizards-xl-preview-buffer nil
  "Records buffer displaying XL preview.")
;;;###autoload
(defun emacspeak-wizards-xl-display ()
  "Called to set up preview of an XL file.
Assumes we are in a buffer visiting a .xls file.
Previews those contents as HTML and nukes the buffer
visiting the xls file."
  (interactive)
  (declare (special emacspeak-wizards-xlhtml-program
                    emacspeak-wizards-xl-preview-buffer))
  (cond
   ((null emacspeak-wizards-xlhtml-program)
    (message "Not using Emacspeak XLHTML wizard."))
   (t 
    (let ((filename (buffer-file-name))
          (xl-buffer (current-buffer))
          (buffer (get-buffer-create " *xl scratch*")))
      (save-excursion
        (set-buffer buffer)
        (shell-command
         (format "%s -a -te %s"
                 emacspeak-wizards-xlhtml-program filename)
         'replace
         (current-buffer))
        (emacspeak-w3-preview-this-buffer))
      (kill-buffer buffer)
      (kill-buffer xl-buffer)))))

(emacspeak-wizards-augment-auto-mode-alist
 "\\.xls$"
 'emacspeak-wizards-xl-mode)

;;}}}
;;{{{ ppt wizard

;;;

(require 'derived)
(define-derived-mode emacspeak-wizards-ppt-mode text-mode
  "Browsing PPT Files."
  "Major mode for browsing PPT slides.\n\n
PPT files  are converted to HTML and previewed using W3."
  (emacspeak-wizards-ppt-display))

(defcustom emacspeak-wizards-ppthtml-program "ppthtml"
  "Program for converting PPT  to HTML.
Set this to nil if you do not want to use the PPTHTML wizard."
  :type 'string
  :group 'emacspeak-wizards)

(defvar emacspeak-wizards-ppt-preview-buffer nil
  "Records buffer displaying PPT preview.")
;;;###autoload
(defun emacspeak-wizards-ppt-display ()
  "Called to set up preview of an PPT file.
Assumes we are in a buffer visiting a .ppt file.
Previews those contents as HTML and nukes the buffer
visiting the ppt file."
  (interactive)
  (declare (special emacspeak-wizards-ppthtml-program
                    emacspeak-wizards-ppt-preview-buffer))
  (emacspeak-w3-without-xsl
   (cond
    ((null emacspeak-wizards-ppthtml-program)
     (message "Not using Emacspeak PPTHTML wizard."))
    (t 
     (let ((filename (buffer-file-name))
	   (ppt-buffer (current-buffer))
	   (buffer (get-buffer-create " *ppt scratch*")))
       (save-excursion
	 (set-buffer buffer)
	 (shell-command
	  (format "%s  %s"
		  emacspeak-wizards-ppthtml-program filename)
	  'replace
	  (current-buffer))
	 (call-interactively 'emacspeak-w3-preview-this-buffer))
       (kill-buffer buffer)
       (kill-buffer ppt-buffer))))))

(emacspeak-wizards-augment-auto-mode-alist
 "\\.ppt$"
 'emacspeak-wizards-ppt-mode)

;;}}}
;;{{{ DVI wizard

(define-derived-mode emacspeak-wizards-dvi-mode fundamental-mode
  "Browsing DVI Files."
  "Major mode for browsing DVI files.\n\n
DVI files  are converted to text and previewed using text mode."
  (emacspeak-wizards-dvi-display))

(defcustom emacspeak-wizards-dvi2txt-program
  (expand-file-name "dvi2txt"
                    emacspeak-etc-directory)
  "Program for converting dvi  to txt.
Set this to nil if you do not want to use the DVI wizard."
  :type 'string
  :group 'emacspeak-wizards)

(defvar emacspeak-wizards-dvi-preview-buffer nil
  "Records buffer displaying dvi preview.")

;;;###autoload
(defun emacspeak-wizards-dvi-display ()
  "Called to set up preview of an DVI file.
Assumes we are in a buffer visiting a .DVI file.
Previews those contents as text and nukes the buffer
visiting the DVI file."
  (interactive)
  (declare (special emacspeak-wizards-dvi2txt-program
                    emacspeak-wizards-dvi-preview-buffer))
  (cond
   ((null emacspeak-wizards-dvi2txt-program)
    (message "Not using Emacspeak DVI wizard."))
   (t 
    (let ((filename (buffer-file-name))
	  (dvi-buffer (current-buffer))
	  (buffer (get-buffer-create " *dvi preview*")))
      (shell-command
       (format "%s  %s &"
	       emacspeak-wizards-dvi2txt-program filename)
       buffer)
      (kill-buffer dvi-buffer)
      (switch-to-buffer buffer)))))

(emacspeak-wizards-augment-auto-mode-alist
 "\\.dvi$"
 'emacspeak-wizards-dvi-mode)

;;}}}
;;{{{ detailed quotes 
(defcustom emacspeak-wizards-quote-command 
  (expand-file-name "quotes.pl"
                    emacspeak-etc-directory)
  "Command for pulling up detailed stock quotes.
this requires Perl module Finance::YahooQuote."
  :type 'file
  :group 'emacspeak-wizards)
(defcustom emacspeak-wizards-quote-row-filter
  '(1 " closed at " 2
      "giving it a P/E ratio of " 16 
      " and a market cap of " 20 ". "
      "The intra-day range was " 13
      ", and the 52 week range is " 14 ". ")
  "Format used to filter rows."
  :type '(repeat
          (choice :tag "Entry"
                  (integer :tag "Column Number:")
		  (string :tag "Text: ")))
  :group 'emacspeak-wizards)

;;;###autoload
(defun emacspeak-wizards-portfolio-quotes ()
  "Bring up detailed stock quotes for portfolio specified by 
emacspeak-websearch-personal-portfolio."
  (interactive)
  (declare (special emacspeak-websearch-personal-portfolio
                    emacspeak-wizards-quote-command
                    emacspeak-wizards-quote-row-filter))
  (let ((temp-file
         (format "/tmp/%s.csv"
                 (gensym "quotes"))))
    (shell-command 
     (format 
      "echo '%s' | perl %s > %s"
      emacspeak-websearch-personal-portfolio
      emacspeak-wizards-quote-command
      temp-file))
    (emacspeak-table-find-csv-file temp-file)
    (setq emacspeak-table-speak-row-filter
          emacspeak-wizards-quote-row-filter)
    (rename-buffer "Portfolio" 'unique)
    (goto-char (point-min))
    (call-interactively 'emacspeak-table-next-row)
    (delete-file temp-file)))

;;}}}
;;{{{ find wizard 

(define-derived-mode emacspeak-wizards-finder-mode  fundamental-mode 
  "Emacspeak Finder"
  "Emacspeak Finder\n\n"
  )

(defcustom emacspeak-wizards-find-switches-widget
  '(cons :tag "Find Expression"
         (menu-choice :tag "Find"
                      (string :tag "Test")
                      (const "-name" )
                      (const "-iname")
                      (const "-path")
                      (const "-ipath")
                      (const "-regexp")
                      (const "-iregexp")
                      (const "-exec")
                      (const "-ok")
                      (const "-newer")
                      (const "-anewer")
                      (const "-cnewer")
                      (const "-used")
                      (const "-user")
                      (const "-uid")
                      (const "-nouser")
                      (const "-nogroup")
                      (const "-perm")
                      (const "-fstype")
                      (const "-lname")
                      (const "-ilname")
                      (const "-empty")
                      (const "-prune")
                      (const "-or")
                      (const "-not")
                      (const "-inum")
                      (const "-atime")
                      (const "-ctime")
                      (const "-mtime")
                      (const "-amin")
                      (const "-mmin")
                      (const "-cmin")
                      (const "-size")
                      (const "-type")
                      (const "-maxdepth")
                      (const "-mindepth")
                      (const "-mount")
                      (const "-noleaf")
                      (const "-xdev"))
         (string :tag "Value"))
  "Widget to get find switch."
  :type 'sexp
  :group 'emacspeak-wizards)

(defvar emacspeak-wizards-finder-args nil
  "List of switches to use as test arguments to find.")

(make-variable-buffer-local 'emacspeak-wizards-finder-args)

(defcustom emacspeak-wizards-find-switches-that-need-quoting
  (list "-name" "-iname"
        "-path" "-ipath"
        "-regexp" "-iregexp")
  "Find switches whose args need quoting."
  :type '(repeat
          (string))
  :group 'emacspeak-wizards)

(defsubst emacspeak-wizards-find-quote-arg-if-necessary (switch arg)
  "Quote find arg if necessary."
  (declare (special emacspeak-wizards-find-switches-that-need-quoting))
  (if (member switch emacspeak-wizards-find-switches-that-need-quoting)
      (format "'%s'" arg)
    arg))
;;;###autoload
(defun emacspeak-wizards-generate-finder   ()
  "Generate a widget-enabled finder wizard."
  (interactive)
  (declare (special default-directory
                    emacspeak-wizards-find-switches-widget))
  (require 'cus-edit)
  (let ((value nil)
        (notify (emacspeak-wizards-generate-finder-callback))
        (buffer-name "*Emacspeak Finder*")
        (buffer nil)
        (inhibit-read-only t))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (setq buffer (get-buffer-create buffer-name))
    (save-excursion
      (set-buffer  buffer)
      (voice-lock-mode t)
      (widget-insert "\n")
      (widget-insert "Emacspeak Finder\n\n")
      (widget-create 'repeat
                     :help-echo "Find Criteria"
                     :tag "Find Criteria"
                     :value value
                     :notify notify
                     emacspeak-wizards-find-switches-widget)
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Find Matching Files"
                     :notify
                     #'(lambda (&rest ignore)
                         (call-interactively
                          'emacspeak-wizards-finder-find)))
      (widget-create 'info-link 
                     :tag "Help"
                     :help-echo "Read the online help."
                     "(find)Finding Files")
      (widget-insert "\n\n")
      (emacspeak-wizards-finder-mode)
      (use-local-map widget-keymap)
      (widget-setup)
      (local-set-key "\M-s" 'emacspeak-wizards-finder-find)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defun emacspeak-wizards-generate-finder-callback ()
  "Generate a callback for use in the Emacspeak Finder."
  (`
   (lambda (widget &rest ignore)
     (declare (special emacspeak-wizards-finder-args))
     (let ((value (widget-value widget)))
       (setq emacspeak-wizards-finder-args value)))))
;;;###autoload
(defun emacspeak-wizards-finder-find (directory)
  "Run find-dired on specified switches after prompting for the
directory to where find is to be launched."
  (interactive
   (list
    (file-name-directory(read-file-name "Directory:"))))
  (declare (special emacspeak-wizards-finder-args))
  (let ((find-args
         (mapconcat
          #'(lambda (pair)
              (format "%s %s"
                      (car pair)
                      (if (cdr pair)
                          (emacspeak-wizards-find-quote-arg-if-necessary
                           (car pair)
                           (cdr pair))
                        "")))
          emacspeak-wizards-finder-args
          " ")))
    (find-dired directory   find-args)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{ alternate between w3 and w3m
;;;###autoload
(defun emacspeak-wizards-use-w3-or-w3m ()
  "Alternates between using W3 and W3M for browse-url."
  (interactive)
  (declare (special browse-url-browser-function))
  (cond
   ((eq browse-url-browser-function 'browse-url-w3)
    (setq browse-url-browser-function 'w3m-browse-url)
    (message "Browse  URL will now use W3M")
    (emacspeak-auditory-icon 'select-object))
   ((eq browse-url-browser-function 'w3m-browse-url)
    (setq browse-url-browser-function 'browse-url-w3)
    (message "Browse  URL will now use W3")
    (emacspeak-auditory-icon 'select-object))
   (t (setq browse-url-browser-function 'w3-fetch)
      (message "Restoring sanity by switching to W3."))))

;;}}}
;;{{{ customize emacspeak
;;;###autoload
(defun emacspeak-customize ()
  "Customize Emacspeak."
  (interactive)
  (customize-group 'emacspeak)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-custom-goto-group))
;;}}}
;;{{{ display environment variable
;;;###autoload
(defun emacspeak-wizards-show-environment-variable (v)
  "Display value of specified environment variable."
  (interactive
   (list
    (read-envvar-name "Display environment variable: " 'exact)))
  (message "%s is %s"
           v
           (getenv v)))

;;}}}
;;{{{ squeeze blank lines in current buffer:
;;;###autoload
(defun emacspeak-wizards-squeeze-blanks (start end)
  "Squeeze multiple blank lines in current buffer."
  (interactive "r")
  (shell-command-on-region start end
                           "cat -s"
                           (current-buffer)
                           'replace))

;;}}}
;;{{{  count slides in region: (LaTeX specific.
;;;###autoload
(defun emacspeak-wizards-count-slides-in-region ()
  "Count slides starting from point."
  (interactive )
  (how-many "begin\\({slide}\\|{part}\\)"))

;;}}}
;;{{{  file specific  headers via occur 

(defvar emacspeak-occur-pattern nil
  "Regexp pattern used to identify header lines by command 
emacspeak-wizards-occur-header-lines.")
(make-variable-buffer-local 'emacspeak-occur-pattern)
;;;###autoload
(defun emacspeak-wizards-how-many-matches (start end &optional prefix)
  "If you define a file local variable 
called `emacspeak-occur-pattern' that holds a regular expression 
that matches  lines of interest, you can use this command to conveniently
run `how-many' to count  matching header lines.
With interactive prefix arg, prompts for and remembers the file local pattern."
  (interactive "rP")
  (declare (special emacspeak-occur-pattern))
  (cond
   ((and (not prefix)
         (boundp 'emacspeak-occur-pattern)
         emacspeak-occur-pattern)
    (how-many  emacspeak-occur-pattern start end 'interactive))
   (t
    (let ((pattern  (read-from-minibuffer "Regular expression: ")))
      (setq emacspeak-occur-pattern pattern)
      (how-many pattern start end 'interactive)))))

;;;###autoload
(defun emacspeak-wizards-occur-header-lines (prefix)
  "If you define a file local variable called
`emacspeak-occur-pattern' that holds a regular expression that
matches header lines, you can use this command to conveniently
run `occur' to find matching header lines. With prefix arg,
prompts for and sets value of the file local pattern."
  (interactive "P")
  (declare (special emacspeak-occur-pattern))
  (cond
   ((and (not prefix)
         (boundp 'emacspeak-occur-pattern)
         emacspeak-occur-pattern)
    (occur emacspeak-occur-pattern)
    (message "Displayed header lines in other window.")
    (emacspeak-auditory-icon 'open-object))
   (t
    (let ((pattern  (read-from-minibuffer "Regular expression: ")))
      (setq emacspeak-occur-pattern pattern)
      (occur pattern)))))

;;}}}
;;{{{   Switching buffers, killing buffers etc
;;;###autoload
(defun emacspeak-switch-to-previous-buffer  ()
  "Switch to most recently used interesting buffer."
  (interactive)
  (switch-to-buffer (other-buffer
                     (current-buffer) 'visible-ok))
  (emacspeak-speak-mode-line )
  (emacspeak-auditory-icon 'select-object ))
;;;###autoload
(defun emacspeak-kill-buffer-quietly   ()
  "Kill current buffer without asking for confirmation."
  (interactive)
  (kill-buffer nil )
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{  spotting words 

(defcustom emacspeak-wizards-spot-words-extension ".tex"
  "Default file extension  used when spotting words."
  :type 'string
  :group 'emacspeak-wizards)
;;;###autoload
(defun emacspeak-wizards-spot-words (ext word)
  "Searches recursively in all files with extension `ext'
for `word' and displays hits in a compilation buffer."
  (interactive
   (list
    (read-from-minibuffer "Extension: "
                          emacspeak-wizards-spot-words-extension)
    (read-from-minibuffer "Word: "
                          (thing-at-point 'word))))
  (declare (special emacspeak-wizards-spot-words-extension))
  (compile 
   (format
    "find . -type f -name '*%s' -print0 | xargs -0 -e grep -n -e \"\\b%s\\b\" "
    ext word))
  (setq emacspeak-wizards-spot-words-extension ext)
  (emacspeak-auditory-icon 'task-done)) 
;;;###autoload
(defun emacspeak-wizards-fix-typo (ext word correction)
  "Search and replace  recursively in all files with extension `ext'
for `word' and replace it with correction.
Use with caution."
  (interactive
   (list
    (read-from-minibuffer "Extension: "
                          emacspeak-wizards-spot-words-extension)
    (read-from-minibuffer "Word: "
                          (thing-at-point 'word))
    (read-from-minibuffer "Correction: "
                          (thing-at-point 'word))))
  (declare (special emacspeak-wizards-spot-words-extension))
  (compile 
   (format
    "find . -type f -name '*%s' -print0 | xargs-0 -e  perl -pi -e    \'s/%s/%s/g' "
    ext word correction))
  (setq emacspeak-wizards-spot-words-extension ext)
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ fix text that has gotten read-only accidentally 
;;;###autoload
(defun emacspeak-wizards-fix-read-only-text (start end)
  "Nuke read-only property on text range."
  (interactive "r")
  (let ((inhibit-read-only t))
    (put-text-property start end 
		       'read-only nil)))

;;}}}
;;{{{ VC viewer 
(defcustom emacspeak-wizards-vc-viewer-command
  "sudo setterm -dump %s -file %s"
  "Command line for dumping out virtual console."
  :type 'string
  :group 'emacspeak-wizards)

(define-derived-mode emacspeak-wizards-vc-viewer-mode  fundamental-mode
  "VC Viewer  Interaction"
  "Major mode for interactively viewing virtual console contents.\n\n
\\{emacspeak-wizards-vc-viewer-mode-map}")

(defvar emacspeak-wizards-vc-console nil
  "Buffer local value specifying console we are viewing.")

(make-variable-buffer-local 'emacspeak-wizards-vc-console)

;;;###autoload
(defun emacspeak-wizards-vc-viewer (console)
  "View contents of specified virtual console."
  (interactive "nConsole:")
  (declare (special emacspeak-wizards-vc-viewer-command
                    emacspeak-wizards-vc-console
                    temporary-file-directory))
  (let ((emacspeak-speak-messages nil)
        (command
         (format emacspeak-wizards-vc-viewer-command
                 console
                 (expand-file-name
                  (format "vc-%s.dump" console)
                  temporary-file-directory)))
        (buffer (get-buffer-create
                 (format "*vc-%s*" console))))
    (shell-command command buffer)
    (switch-to-buffer buffer)
    (kill-all-local-variables)
    (insert-file
     (expand-file-name
      (format "vc-%s.dump" console)
      temporary-file-directory))
    (set-buffer-modified-p nil)
    (emacspeak-wizards-vc-viewer-mode)
    (setq emacspeak-wizards-vc-console console)
    (goto-char (point-min))
    (when (interactive-p) (emacspeak-speak-line))))

;;;###autoload
(defun emacspeak-wizards-vc-viewer-refresh ()
  "Refresh view of VC we're viewing."
  (interactive)
  (declare (special emacspeak-wizards-vc-console))
  (unless (eq major-mode
              'emacspeak-wizards-vc-viewer-mode)
    (error "Not viewing a virtual console."))
  (let ((console emacspeak-wizards-vc-console)
        (command
         (format emacspeak-wizards-vc-viewer-command
                 emacspeak-wizards-vc-console
                 (expand-file-name
                  (format "vc-%s.dump"
                          emacspeak-wizards-vc-console)
                  temporary-file-directory)))
        (inhibit-read-only t)
        (orig (point)))
    (shell-command command)
    (fundamental-mode)
    (erase-buffer)
    (insert-file
     (expand-file-name
      (format "vc-%s.dump"
              console)
      temporary-file-directory))
    (set-buffer-modified-p nil)
    (goto-char orig)
    (emacspeak-wizards-vc-viewer-mode)
    (setq emacspeak-wizards-vc-console console)
    (when (interactive-p)
      (emacspeak-speak-line))))

;;;###autoload
(defun emacspeak-wizards-vc-n ()
  "Accelerator for VC viewer."
  (interactive)
  (declare (special last-input-char))
  (emacspeak-wizards-vc-viewer (format "%c" last-input-char))
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'open-object))

(declaim (special emacspeak-wizards-vc-viewer-mode-map))

(define-key  emacspeak-wizards-vc-viewer-mode-map "\C-l" 'emacspeak-wizards-vc-viewer-refresh)

;;}}}
;;{{{ google hits 

;;;###autoload
(defun emacspeak-wizards-google-hits ()
  "Filter Google results after performing search to show just the
hits."
  (interactive)
  (let ((name   "Google Hits"))
    (emacspeak-url-template-open
     (emacspeak-url-template-get name))))

;;}}}
;;{{{ longest line in region 
;;;###autoload
(defun emacspeak-wizards-find-longest-line-in-region (start end)
  "Find longest line in region.
Moves to the longest line when called interactively."
  (interactive "r")
  (let ((max 0)
        (where nil))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp))
                  (< (point) end))
        (when 
            (< max
               (- (line-end-position)
                  (line-beginning-position)))
          (setq max (- (line-end-position)
                       (line-beginning-position)))
          (setq where (line-beginning-position)))
        (forward-line 1)))
    (when (interactive-p)
      (message "Longest line is %s columns"
               max)
      (goto-char where))
    max))

;;}}}
;;{{{ longest para in region 
;;;###autoload
(defun emacspeak-wizards-find-longest-paragraph-in-region (start end)
  "Find longest paragraph in region.
Moves to the longest paragraph when called interactively."
  (interactive "r")
  (let ((max 0)
        (where nil)
        (para-start start))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp))
                  (< (point) end))
        (forward-paragraph 1)
        (when 
            (< max (- (point) para-start ))
          (setq max(- (point)  para-start))
          (setq where para-start))
        (setq para-start (point))))
    (when (interactive-p)
      (message "Longest paragraph is %s characters"
               max)
      (goto-char where))
    max))

;;}}}
;;{{{ find grep using compile 

(defun emacspeak-wizards-find-grep (glob pattern)
  "Run compile using find and grep. 
Interactive  arguments specify filename pattern and search pattern."
  (interactive
   (list
    (read-from-minibuffer "Look in files: ")
    (read-from-minibuffer "Look for: ")))
  (compile
   (format
    "find . -type f -name '%s' -print0 | xargs -0 -e grep -n -e '%s'"
    glob pattern))
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ face wizard
;;;###autoload
(defun emacspeak-wizards-show-face (face)
  "Show salient properties of specified face."
  (interactive
   (list
    (read-face-name "Face")))
  (let ((output (get-buffer-create "*emacspeak-face-display*")))
    (save-excursion
      (set-buffer output)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Face: %s\n" face))
      (loop for a in
            (mapcar #'car face-attribute-name-alist)
            do
            (unless (eq 'unspecified (face-attribute face a))
              (insert
               (format "%s\t%s\n"
                       a
                       (face-attribute face a)))))
      (insert
       (format "Documentation: %s\n"
               (face-documentation face)))
      (setq buffer-read-only t))
    (when (interactive-p)
      (switch-to-buffer output)
      (goto-char (point-min))
      (emacspeak-speak-mode-line)
      (emacspeak-auditory-icon 'open-object))))
             
   

;;}}}
;;{{{ voice sample
;;;###autoload
(defun emacspeak-wizards-voice-sampler (personality)
  "Read a personality  and apply it to the current line."
  (interactive
   (list
    (voice-setup-read-personality)))
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     'personality
		     personality
		     (emacspeak-speak-line)))

;;;###autoload
(defun emacspeak-wizards-generate-voice-sampler  (step)
  "Generate a buffer that shows a sample line in all the ACSS settings
for the current voice family."
  (interactive "nStep:")
  (let ((buffer (get-buffer-create "*Voice Sampler*"))
        (voice nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (loop for  s from 0 to 9 by step do 
	    (loop for p from 0 to 9 by step do
		  (loop for a from 0 to 9 by step do 
			(loop for r from 0 to 9 by step do 
			      (setq voice (voice-setup-personality-from-style
					   (list nil a p s r )))
			      (insert
			       (format
				" Aural CSS    average-pitch %s pitch-range %s stress %s richness %s"
				a p s r ))
			      (put-text-property (line-beginning-position)
						 (line-end-position)
						 'personality voice)
			      (end-of-line)
			      (insert "\n"))))))
    (switch-to-buffer  buffer)
    (voice-lock-mode 1)
    (goto-char (point-min))))

;;}}}
;;{{{ tramp wizard 
(defcustom emacspeak-wizards-tramp-locations nil
  "Tramp locations used by Emacspeak tramp wizard.
Locations added here via custom can be opened using command
emacspeak-wizards-tramp-open-location
bound to \\[emacspeak-wizards-tramp-open-location]."
  :type '(repeat
          (cons :tag "Tramp"
		(string :tag "Name")
		(string :tag "Location")))
  :group 'emacspeak-wizards)

;;;###autoload
(defun emacspeak-wizards-tramp-open-location (name)
  "Open specified tramp location.
Location is specified by name."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Location:"
                       emacspeak-wizards-tramp-locations
                       nil 'must-match))))
  (declare (special emacspeak-wizards-tramp-locations))
  (let ((location (cdr (assoc name
                              emacspeak-wizards-tramp-locations))))
    (find-file location)))

;;}}}
;;{{{ ISO dates 
;;; implementation based on icalendar.el

(defun emacspeak-wizards-speak-iso-datetime (iso)
  "Make ISO date-time speech friendly."
  (interactive
   (list
    (read-from-minibuffer "ISO DateTime:"
                          (word-at-point))))
  (let ((emacspeak-speak-messages nil)
        (time (emacspeak-speak-decode-iso-datetime iso)))
    (tts-with-punctuations 'some
                           (dtk-speak time))
    (message time)))

;;}}}
;;{{{ date pronouncer wizard
(defvar emacspeak-wizards-mm-dd-yyyy-date-pronounce nil
  "Toggled by wizard to record how we are pronouncing mm-dd-yyyy
dates.")

(defun emacspeak-wizards-toggle-mm-dd-yyyy-date-pronouncer ()
  "Toggle pronunciation of mm-dd-yyyy dates."
  (interactive)
  (declare (special emacspeak-wizards-mm-dd-yyyy-date-pronounce
                    emacspeak-pronounce-date-mm-dd-yyyy-pattern))
  (cond
   (emacspeak-wizards-mm-dd-yyyy-date-pronounce
    (setq emacspeak-wizards-mm-dd-yyyy-date-pronounce nil)
    (emacspeak-pronounce-remove-buffer-local-dictionary-entry
     emacspeak-pronounce-date-mm-dd-yyyy-pattern))
   (t (setq emacspeak-wizards-mm-dd-yyyy-date-pronounce t)
      (emacspeak-pronounce-add-buffer-local-dictionary-entry
       emacspeak-pronounce-date-mm-dd-yyyy-pattern
       (cons 're-search-forward
	     'emacspeak-pronounce-mm-dd-yyyy-date))))
  (message "Will %s pronounce mm-dd-yyyy date strings in
  English."
           (if emacspeak-wizards-mm-dd-yyyy-date-pronounce "" "
  not ")))
    
;;}}}
;;{{{ units wizard

(defun emacspeak-wizards-units ()
  "Run units in a comint sub-process."
  (interactive)
  (make-comint "units" "units"
               nil "--verbose")
  (switch-to-buffer "*units*")
  (emacspeak-auditory-icon 'select-object)
  (goto-char (point-max))
  (unless emacspeak-comint-autospeak
    (emacspeak-toggle-comint-autospeak))
  (emacspeak-speak-mode-line))
      
;;}}}
;;{{{ rivo

(defvar emacspeak-wizards-rivo-program
  (expand-file-name "rivo.pl" emacspeak-etc-directory)
  "Rivo script used by emacspeak.")

(defun emacspeak-wizards-rivo (when channel length output directory)
  "Rivo wizard.
Prompts for relevant information and schedules a rivo job using
  UNIX AT scheduling facility.
RIVO is implemented by rivo.pl ---
 a Perl script  that can be used to launch realaudio and record
   streaming media for  a specified duration."
  (interactive
   (list
    (read-from-minibuffer "At Time: hh:mm Month Day")
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (minibuffer-history emacspeak-realaudio-history))
      (emacspeak-pronounce-define-local-pronunciation
       emacspeak-realaudio-shortcuts-directory " shortcuts/ ")
      (read-file-name "RealAudio resource: "
                      emacspeak-realaudio-shortcuts-directory
                      (if (eq major-mode 'dired-mode)
                          (dired-get-filename)
                        emacspeak-realaudio-last-url)))
    (read-minibuffer "Length:" "00:30:00")
    (read-minibuffer "Output Name:")
    (read-directory-name "Output Directory:")))
  (let ((command
         (format "%s -c %s -l %s -o %s -d %s\n"
                 emacspeak-wizards-rivo-program
                 channel length output directory)))
    (shell-command
     (format "echo '%s' | at %s"
             command when ))))

;;}}}
;;{{{ show commentary:
;;;###autoload

(defun emacspeak-wizards-show-commentary (&optional file)
  "Display commentary. Default is to display commentary from current buffer."
  (interactive "P")
  (let ((filename nil))
    (cond
     ((and (interactive-p)
           file)
      (setq filename (read-file-name "File: ")))
     ((and (interactive-p)
           (null file))
      (setq filename (buffer-file-name (current-buffer))))
     (t (setq filename file)))
    (with-output-to-temp-buffer "*Commentary*"
      (set-buffer standard-output)
      (insert
       (ems-cleanup-commentary
	(lm-commentary filename))))))

;;}}}
;;{{{ unescape URIs

;;;###AUTOLOAD
(defun emacspeak-wizards-unhex-uri (uri)
  "UnEscape URI"
(interactive "sURL:")
(message (url-unhex-string uri)))



;;}}}
(provide 'emacspeak-wizards)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
