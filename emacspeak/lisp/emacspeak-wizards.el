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
;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)

(require 'custom)
(require 'thingatpt)
(require 'voice-lock)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-table-ui)
(require 'shell)
(eval-when-compile (require 'cus-edit))
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



(defun emacspeak-view-emacspeak-tips ()
  "Browse  Emacspeak productivity tips."
  (interactive)
  (declare (special emacspeak-etc-directory))
  (browse-url
   (format "file:///%stips.html"
           emacspeak-etc-directory))
  (emacspeak-auditory-icon 'help)
  (emacspeak-speak-mode-line))

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
      (unless (string= location (car emacspeak-copy-file-location-history))
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
      (unless (string= location (car emacspeak-copy-file-location-history))
        (push location emacspeak-copy-file-location-history))
      (setq location
            (expand-file-name
             (file-name-nondirectory file)
             location)))
    (add-name-to-file
     file location)
    (emacspeak-auditory-icon 'select-object)
    (message "Linked current document to %s" location)))


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
      (unless (string= location (car emacspeak-copy-file-location-history))
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

(defun emacspeak-speak-run-shell-command (command)
  "Invoke shell COMMAND and display its output as a table.
The results are placed in a buffer in Emacspeak's table
  browsing mode.
  Use this for running shell commands that produce tabulated
  output.  This command should be used for shell commands that
  produce tabulated output that works with Emacspeak's table
  recognizer.
  Verify this first by running the command in a shell and
  executing command `emacspeak-table-display-table-in-region'
  normally bound to \\[emacspeak-table-display-table-in-region]."
  (interactive
   (list
    (read-from-minibuffer "Shell command: "
                          nil           ;initial input
                          nil           ; keymap
                          nil           ;read
                          'emacspeak-speak-run-shell-command-history)))
  (let ((buffer-name (format "*%s-output*" command))
        (start nil)
        (end nil))
    (shell-command command buffer-name)
    (pushnew   command
               emacspeak-speak-run-shell-command-history
               :test 'string-equal)
    (save-excursion
      (set-buffer buffer-name)
      (setq start (point-min)
            end (1- (point-max)))
      (condition-case nil
          (emacspeak-table-display-table-in-region  start end )
        (error
         (progn
           (message "Output could not be tabulated correctly")
           (switch-to-buffer buffer-name)))))))

;;}}}
;;{{{ Directory specific settings
(defcustom  emacspeak-speak-load-directory-settings-quietly t
  "*User option that affects loading of directory specific settings.
If set to T,Emacspeak will not prompt before loading
directory specific settings."
  :group 'emacspeak-speak
  :type 'boolean)
  

(defcustom emacspeak-speak-directory-settings
  ".espeak.el"
  "*Name of file that holds directory specific settings."
  :group 'emacspeak-speak
  :type 'string)

(defsubst emacspeak-speak-get-directory-settings ()
  "Return directory specific settings file."
  (declare (special emacspeak-speak-directory-settings))
  (concat default-directory
          emacspeak-speak-directory-settings))

(defun emacspeak-speak-load-directory-settings ()
  "Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory."
  (interactive)
  (let ((settings (emacspeak-speak-get-directory-settings)))
    (when (and (file-exists-p  settings)
               (or emacspeak-speak-load-directory-settings-quietly
                   (y-or-n-p "Load directory settings? ")
                   "Load  directory specific Emacspeak
settings? "))
      (condition-case nil
          (load-file settings)
        (error (message "Error loading settings %s" settings))))))

;;}}}
;;{{{ linux howtos
(defun emacspeak-speak-browse-linux-howto (howto)
  "Browse a Linux Howto file.
We cleanup underlining, and set up outline mode correctly."
  (interactive 
   (list
    (read-file-name "Howto file: "
                    (cond
                     ((file-exists-p "/usr/doc/HOWTO/" )
                      "/usr/doc/HOWTO/")
                     ((file-exists-p
                       "/usr/share/doc/HOWTO/")
                      "/usr/share/doc/HOWTO/")
                     (t (error "You dont have Linux howtos
installed where I expected.")))
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
(defun emacspeak-speak-hostname ()
  "Speak host name."
  (interactive)
  (message (system-name)))

(defcustom emacspeak-speak-show-active-network-interfaces-command
  "echo `/sbin/ifconfig | grep -v '^lo' | grep '^[a-z]' | awk '{print $1}'`"
  "Command that displays names of active network interfaces."
  :type 'string
  :group 'emacspeak-wizards)

(defcustom emacspeak-speak-show-active-network-interfaces-addresses
  "echo `/sbin/ifconfig %s | grep 'inet addr' | awk '{print $2}'| sed 's/addr://'`"
  "Command that displays address of  a specific interface."
  :type 'string
  :group 'emacspeak-wizards
  )

(defvar emacspeak-speak-network-interfaces-list
  (list  "eth0" "ppp0" "eth1" "ppp1" "tr0" "tr1")
  "Used whne prompting for an interface to query.")


(defun emacspeak-speak-show-active-network-interfaces
  (&optional address)
  "Shows all active network interfaces in the echo area.
With interactive prefix argument ADDRESS it prompts for a
specific interface and shows its address. The address is
also copied to the kill ring for convenient yanking."
  (interactive "P")
  (declare (special emacspeak-speak-network-interfaces
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
    (when address 
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

(defun emacspeak-root ()
  "Start a root shell or switch to one that already exists."
  (interactive)
  (declare (special explicit-shell-file-name))
  (cond
   ((comint-check-proc "*root*")
    (pop-to-buffer "*root*")
    (when (featurep 'emacspeak)
      (emacspeak-speak-mode-line)))
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
      (dtk-speak "Enter root password: ")))))

;;}}}
;;{{{ setup CVS access to sourceforge 

(defcustom emacspeak-cvs-local-directory
  (expand-file-name "~/sourceforge/cvs-emacspeak")
  "Directory where we download the snapshot."
  :type 'directory
  :group 'emacspeak-wizards)

(defun emacspeak-cvs-done-alert (process state)
  "Alert user of cvs status."
  (message "Done getting CVS snapshot from sourceforge.")
  (emacspeak-auditory-icon 'task-done))

(defcustom emacspeak-cvs-anonymous-cvsroot
  ":pserver:anonymous@cvs.emacspeak.sourceforge.net:/cvsroot/emacspeak"
  "CVSROOT for emacspeak CVS repository at sourceforge."
  :type 'string
  :group 'emacspeak-wizards)

(defun emacspeak-cvs-get-anonymous  ()
  "Get latest cvs snapshot of emacspeak."
  (interactive)
  (declare (special emacspeak-cvs-local-directory
                    emacspeak-cvs-anonymous-cvsroot))
  (unless (file-exists-p emacspeak-cvs-local-directory)
    (make-directory emacspeak-cvs-local-directory))
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

(defvar emacspeak-cvs-anonymous-cvsroot-pattern
  ":pserver:anonymous@cvs.%s.sourceforge.net:/cvsroot/%s"
  "CVSROOT pattern for project CVS repository at
sourceforge.
Typically %s is replaced by project name.")


(defcustom emacspeak-cvs-local-directory-pattern
  "~/sourceforge/cvs-%s"
  "Pattern from which name of local download directory is build.
 %s is replaced by the project name."
:type 'string
:group 'emacspeak-wizards)

(defun emacspeak-cvs-get-project-snapshot  (project)
  "Grab CVS snapshot  of specified project from Sourceforge."
  (interactive
   (list
    (read-string "Project name: ")))
  (declare (special emacspeak-cvs-local-directory-pattern
                    emacspeak-cvs-anonymous-cvsroot-pattern))
  (let ((cvsroot
         (format emacspeak-cvs-anonymous-cvsroot-pattern
                 project project))
        (dir (expand-file-name
              (format emacspeak-cvs-local-directory-pattern
                      project))))
    (unless (file-exists-p dir)
      (make-directory dir))
    (cd dir)
    (let ((cvs-process nil))
      (setq cvs-process
            (start-process "cvs" "*cvs-download*" "cvs"
                           (format "-d%s"
                                   cvsroot)
                           "login"))
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
;;{{{  Learn mode

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

(defsubst emacspeak-list-emacspeak-commands ()
  "List all commands."
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
                     (string-match "dtk" (symbol-name f))
                     (string-match "tts" (symbol-name f))))
          (push f commands)))))
    (setq commands
          (sort commands
                #'(lambda (a b )
                    (string-lessp
                     (symbol-file a)
                     (symbol-file b)))))
    commands))

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
      (insert " 
Local variables: mode: outline paragraph-separate: \"[ ]*$\"
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
                        (goto-char (point-max))
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

(defun emacspeak-generate-texinfo-command-documentation (filename)
  "Generate texinfo documentation  for all emacspeak
commands into file commands.texi.
Warning! Contents of file commands.texi will be overwritten."
  (interactive "FEnter filename to save DOC in: ")
  (declare (special emacspeak-speak-messages))
  (let ((emacspeak-speak-messages nil)
        (buffer (find-file-noselect filename))
        (module nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert"@c $Id$\n")
      (insert
       "@node Emacspeak Commands\n@chapter Emacspeak Commands\n\n")
      (insert
       "This chapter is generated automatically from the source-level documentation.
Any errors or corrections should be made to the source-level
documentation.\n\n")
      (mapcar
       (function
        (lambda (f)
          (let ((key (where-is-internal f))
                (key-description nil)
                (commentary nil)
                (this-module (symbol-file f)))
            (when this-module
              (setq this-module
                    (file-name-sans-extension this-module))
              (setq commentary
                    (lm-commentary
                     (concat this-module ".el")))
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
            (insert (format "\n\n@deffn %s\n" f))
            
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
              (ems-texinfo-escape(documentation f))
              ""))
            (insert "\n@end deffn\n\n"))))
       (emacspeak-list-emacspeak-commands))
      (texinfo-all-menus-update)
      (save-buffer)))
  (emacspeak-auditory-icon 'task-done))


;;}}}
;;{{{ labelled frames


(defsubst emacspeak-frame-read-frame-label ()
  "Read a frame label with completion."
  (interactive)
  (let* ((frame-names-alist (make-frame-names-alist))
         (default (car (car frame-names-alist)))
         (input (completing-read
                 (format "Select Frame (default %s): " default)
                 frame-names-alist nil t nil 'frame-name-history)))
    (if (= (length input) 0)
        default
      input)))
 
                        
(defun emacspeak-frame-label-or-switch-to-labelled-frame (&optional prefix)
  "Switch to labelled frame.
With optional PREFIX argument, label current frame."
  (interactive "P")
  (cond
   (prefix
    (call-interactively 'set-frame-name))
   (t (select-frame-by-name
       (emacspeak-frame-read-frame-label))))
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))


(defun emacspeak-next-frame ()
  "Move to next frame."
  (interactive)
  (other-frame 1)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

(defun emacspeak-previous-frame ()
  "Move to next frame."
  (interactive)
  (other-frame -1)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{  readng different displays of same buffer

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

(defun emacspeak-speak-this-buffer-next-display ()
  "Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `next'."
  (interactive)
  (emacspeak-speak-this-buffer-other-window-display  1))

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
The emacspeak clipboard provides a convenient mechnaism for exchanging
information between different Emacs sessions."
  :group 'emacspeak-speak
  :type 'string)

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

(defun emacspeak-show-list-variable (var)
  "Convenience command to view Emacs variables that are long lists.
Prompts for a variable name and displays its value in a separate buffer.
Lists are displayed one element per line.
Argument VAR specifies variable whose value is to be displayed."
  (interactive "SDisplay variable:")
  (let ((buffer (get-buffer-create
                 (format "*emacspeak:%s*"
                         var)))
        (symbol (symbol-value var)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (cond
       ((listp symbol)
        (loop for element in symbol
              do
              (insert (format "%s\n"
                              element))))
       (t (insert (format "%s\n" symbol))))
      (goto-char (point-min))
      (emacs-lisp-mode))
    (pop-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))


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
If you are debugging another speech servre and that server
gets wedged for some reason,
you can use command emacspeak-emergency-tts-restart
to get speech back using the reliable TTS server.
It's useful to bind the above command to a convenient key."
  :type 'string
  :group 'emacspeak)


(defun emacspeak-emergency-tts-restart ()
  "For use in an emergency.
Will start TTS engine specified by 
emacspeak-emergency-tts-server."
  (interactive)
  (declare (special emacspeak-emergency-tts-server))
  (dtk-select-server emacspeak-emergency-tts-server)
  (dtk-initialize))

;;}}}
;;{{{ customization wizard
(defun emacspeak-customize-personal-settings (file)
  "Create a customization buffer for browsing and updating
personal customizations."
  (interactive
   (list
    (read-file-name "Customization file: "
                    nil
                    custom-file)))
  (declare (special custom-file))
  (let*((buffer (find-file-noselect custom-file))
        (settings  (cdr (read  buffer)))
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

(defun emacspeak-show-personality-at-point ()
  "Show value of property personality at point."
  (interactive )
  (emacspeak-show-property-at-point 'personality))

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
                                  'annotation-voice skipped))
            (emacspeak-auditory-icon 'select-object)
            (dtk-speak
             (concat skipped
                     (thing-at-point 'line))))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))

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
                                  'annotation-voice skipped))
            (emacspeak-auditory-icon 'select-object)
            (dtk-speak
             (concat skipped
                     (thing-at-point 'line))))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))

;;}}}
;;{{{  launch lynx 

(defun emacspeak-lynx (url)
  "Launch lynx on  specified URL in a new terminal."
  (interactive
   (list
    (read-from-minibuffer "URL: ")))
  (require 'term)
  (delete-other-windows)
  (switch-to-buffer
   (term-ansi-make-term
    (generate-new-buffer-name
     (format "lynx-%s"
             (substring url 7)))
    "/usr/bin/lynx"
    nil
    "-show-cursor=yes"
    url))
  (emacspeak-eterm-record-window   1 
                                   (cons 0 1)
                                   (cons 79 20)
                                   'right-stretch 'left-stretch)
  (term-char-mode)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{ table wizard
(declaim (special emacspeak-etc-directory))

(defvar emacspeak-wizard-table-content-extractor
  (expand-file-name "extract-table.pl" emacspeak-etc-directory)
  "Program that extracts table content.")

(defun emacspeak-wizard-get-table-content-from-url (task url depth count )
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
            emacspeak-wizard-table-content-extractor))
  (let ((output (format "/tmp/%s.csv" task)))
  (shell-command
   (format  "%s --task=%s --url='%s' --depth=%s --count=%s"
            emacspeak-wizard-table-content-extractor
            task
            url
depth count ))
  (emacspeak-table-find-csv-file output)
  (delete-file output)))

(defun emacspeak-wizard-get-table-content-from-file (task file depth count )
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
  (declare (special emacspeak-wizard-table-content-extractor))
  (shell-command
   (format  "%s --task=%s --file=%s --depth=%s --count=%s"
            emacspeak-wizard-table-content-extractor
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
(provide 'emacspeak-wizards)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
