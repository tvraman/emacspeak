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
;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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

(require 'custom)
(require 'thingatpt)
(require 'voice-lock)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-table-ui)
(require 'shell)

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

(defun emacspeak-speak-popup-messages ()
  "Pop up messages buffer. "
  (interactive)
  (pop-to-buffer "*Messages*")
  (goto-char (point-max))
  (emacspeak-auditory-icon 'select-object)
  (forward-line -1)
  (emacspeak-speak-line))

;;}}}
;;{{{ Show active network interfaces
(defun emacspeak-speak-hostname ()
  "Speak host name."
  (interactive)
  (message (system-name)))

(defvar emacspeak-speak-show-active-network-interfaces-command
"echo `ifconfig | grep -v '^lo' | grep '^[a-z]' | awk '{print $1}'`"
"Command that displays names of active network interfaces.")

(defvar emacspeak-speak-show-active-network-interfaces-addresses
"echo `/sbin/ifconfig %s | grep 'inet addr' | awk '{print $2}'| sed 's/addr://'`"
"Command that displays addressesof a specific interface.")

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

(defvar emacspeak-cvs-local-directory
  (expand-file-name "~/cvs-emacspeak")
  "Directory where we get the snapshot.")

(defun emacspeak-cvs-done-alert (process state)
  "Alert user of cvs status."
  (message "Done getting CVS snapshot from sourceforge.")
  (emacspeak-auditory-icon 'task-done))

(defvar emacspeak-cvs-anonymous-cvsroot
  ":pserver:anonymous@cvs.emacspeak.sourceforge.net:/cvsroot/emacspeak"
  "CVSROOT for emacspeak CVS repository at sourceforge.")

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


(defvar emacspeak-cvs-local-directory-pattern
  "~/cvs-%s"
  "Pattern from which name of local download directory is
build.
Typically, %s is replaced by the project name.")

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
                     (string-match "dtk" (symbol-name f))))
          (push f commands)))))
    (setq commands
          (sort commands 'string-lessp))
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
                  (error nil))
              (insert " No global keybinding\n\n"))
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
  -  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ labelled frames


(defsubst emacspeak-frame-read-frame-label ()
  "Read a frame label with completion."
  (interactive)
  (completing-read  "Frame label: "
(loop for f in (frame-list)
                        collect
 (cons (frame-parameter f 'emacspeak-label)
        (frame-parameter f 'emacspeak-label)))))
 
                        
(defun emacspeak-frame-label-or-switch-to-labelled-frame (label
                                                          &optional prefix)
  "Switch to labelled frame.
With optional PREFIX argument, label current frame."
  (interactive
   (list
    (emacspeak-frame-read-frame-label)
    current-prefix-arg))
  (cond
   (prefix
    (modify-frame-parameters (selected-frame)
                             (list (cons 'emacspeak-label label))))
   (t(let ((frame (loop for f in (frame-list)
                        if (string= label (frame-parameter f 'emacspeak-label))
                        return f)))
       (cond
        ( frame
          (select-frame frame)
          (emacspeak-auditory-icon 'select-object)
          (emacspeak-speak-mode-line))
        (t
         (message "No frame labelled %s" label)))))))


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
              (read-minibuffer "Window    to speak")))
    (setq win
          (nth (% window (length window-list ))
               window-list))
    (save-window-excursion
      (emacspeak-speak-region
       (window-start win)
       (window-end win)))))

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
(provide 'emacspeak-wizards)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
