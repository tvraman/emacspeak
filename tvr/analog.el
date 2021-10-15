;;; analog.el --- monitor lists of files or command output

;;; Copyright (C) 2000, 2001 Matthew P. Hodges

;; Author: Matthew P. Hodges <pczmph@unix.ccc.nottingham.ac.uk>
;; Version: $Id: analog.el,v 1.13 2001/03/24 12:07:57 matt Exp $

;; analog.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; analog.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;; 
;; Package to keep track of a list of specified files or the output of
;; specified commands. The principal variable to modify is
;; analog-entry-list, which should be set to a list of entries.
;;
;; Each element of analog-entry-list is a list where the car is a file
;; (or command) and the cdr is an association list of properties.
;;
;; By default, entries are files, but commands can also be specified.
;; Each entry can have a list of attributes describing whether the
;; head or the tail of the output is wanted, how many lines should be
;; kept, a list of regexps to keep or flush etc. Entries can be
;; collected into named groups.

;; Here is an example:
;;
;; (setq analog-entry-list
;;       '(
;;         ;; files in the WWW group
;;         ("/var/log/apache/referer.log"
;;          (group . "WWW")
;;          (lines . 10))                  ; show 10 lines
;;         ("/var/log/apache/combined.log"
;;          (group . "WWW")
;;          (lines . 10))
;;         ;; files in the Mail group
;;         ("/var/log/exim/mainlog"
;;          (group . "Mail")
;;          (hide . ("queue" "completed"))) ; hide lines matching queue/completed
;;         ;; commands in the Commands group
;;         ("df -h"
;;          (group . "Commands")
;;          (type . command)               ; commands must be identified
;;          (lines . all))                 ; keep all lines
;;         ("last"
;;          (group . "Commands")
;;          (type . command)
;;          (position . head)              ; monitor the head of the output
;;          (lines . 6))
;;         ("ps aux"
;;          (group . "Commands")
;;          (type . command)
;;          (keep . "matt")                ; keep lines matching matt
;;          (hide . ("bash" "ssh" "rxvt")) ; hide/keep can be lists
;;          (lines . all))
;;         ;; files in the System group
;;         ("/var/log/syslog"
;;          (group . "System")
;;          (hide . ("CRON" "MARK")))
;;         ))

;;; Code:

;; Settings to customize

(defvar analog-entry-list nil
  "*This is a list of analog entries and their associated properties.
If properties are undeclared, defaults will be used.")

(defcustom analog-default-no-lines 4
  "*The default number of lines to display."
  :group 'analog
  :type 'integer)

(defcustom analog-default-position 'tail
  "*The default position of the file to display.
This can be 'head or 'tail."
  :group 'analog
  :type '(choice (const head)
                 (const tail)))

(defcustom analog-use-timer nil
  "*If t, the *analog* buffer will periodically be updated.
The frequency of updates is controlled by `analog-timer-period'."
  :group 'analog
  :type 'boolean)

(defcustom analog-timer-period 60
  "*The number of seconds between updates of the *analog* buffer.
Only relevant if timers are being used; see `analog-timer'."
  :group 'analog
  :type 'integer)

(defcustom analog-emit-update-messages nil
  "*If t, analog will print a message to the echo area after each update."
  :group 'analog
  :type 'boolean)

(defcustom analog-entry-string "#   Entry: "
  "String indicating entry name in *analog* buffer."
  :group 'analog
  :type 'string)

(defcustom analog-group-string "# Group: "
  "String indicating group name in *analog* buffer."
  :group 'analog
  :type 'string)

;; Faces

(defface analog-group-header-face
  '((((class color) (background light))(:foreground "green4"))
    (((class color) (background dark)) (:foreground "green")))
  "Face used for group headings."
  :group 'analog)

(defface analog-entry-header-face
  '((((class color) (background light)) (:foreground "blue" :family "helv"))
    (((class color) (background dark)) (:foreground "yellow" :family "helv")))
  "Face used for entry headings."
  :group 'analog)

(defface analog-entry-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Face used for entries."
  :group 'analog)

;; Internal variables

(defvar analog-timer nil
  "Timer object controlling updates of the *analog* buffer.
Updates occur if `analog-use-timer' is t. The frequency of updates is
controlled by `analog-timer-period'.")

(defvar analog-default-type 'file
  "*The default type of entry.
Unless otherwise specified, an entry is taken to be a file.")

(defvar analog-entries nil
  "A list of entries inferred from `analog-entry-list'.")

(defvar analog-group-list nil
  "A list of filesets inferred from `analog-entry-list'.
If any entries are not associated with a named group, this list will
contain nil.")

(defvar analog-current-group nil
  "The group of entries currently active.")

(defvar analog-entries-in-current-group nil
  "A list of entries associated with the current group.")

(defvar analog-show-help-prompt nil
  "Prompt issued when `analog-show-help' is called.
This is built from `analog-mode-map' when it is first needed.")

;; Entry point

(defun analog ()
  "Start analog mode.
Also update and select the *analog* buffer."
  (interactive)
  (if (not (buffer-live-p (get-buffer "*analog*")))
      ;; Initialize internal variables
      (analog-init))
  (analog-refresh-display-buffer)
  (show-buffer (selected-window) "*analog*")
  (goto-char (point-min))
  (analog-mode)
  (analog-next-entry))

;; Internal functions
 
(defun analog-get-entry-property (entry property)
  "Return for entry ENTRY the associated property PROPERTY."
  (cdr (assoc property
              (cdr (assoc entry analog-entry-list)))))

(defun analog-get-entry-lines (entry)
  "Get the number of lines associated with ENTRY."
  (or (analog-get-entry-property entry 'lines)
      analog-default-no-lines))

(defun analog-get-entry-type (entry)
  "Get the type of ENTRY.
Currently this can be 'file or command."
  (or (analog-get-entry-property entry 'type)
      analog-default-type))

(defun analog-get-entry-hide (entry)
  "Get the regexps to be hidden for ENTRY.
Currently this can be 'file or command."
  (analog-get-entry-property entry 'hide))

(defun analog-get-entry-keep (entry)
  "Get the regexps to be kept for ENTRY.
Currently this can be 'file or command."
  (analog-get-entry-property entry 'keep))

(defun analog-get-entry-position (entry)
  "Get the position for ENTRY.
This can be head/tail for the beginning/end of the file"
  (or (analog-get-entry-property entry 'position)
      analog-default-position))

(defun analog-init ()
  "Function used to set internal variables."
  ;; build list of entries
  (setq analog-entries (mapcar #'car analog-entry-list))
  ;; build the list of groups
  (analog-update-group-list)
  ;; set the current group
  (setq analog-current-group (car analog-group-list))
  ;; get the files in the current group
  (analog-update-entries-in-current-group)
  ;; kill existing timer; the timer frequency may have changed or the
  ;; timer may have switched on by hand
  (if analog-timer (progn (cancel-timer analog-timer)
                          (setq analog-timer nil)))
  ;; start new timer according to the default behaviour
  (if analog-use-timer
      (analog-toggle-timer)))

(defun analog-update-group-list ()
  "Update the list of groups in `analog-group-list'."
  (setq analog-group-list nil)
  (let ((entries analog-entries))
    (while entries
      (add-to-list 'analog-group-list
                   (analog-get-entry-property (car entries) 'group))
      (setq entries (cdr entries))))
  ;; Elements added first end up at the end of the list; reverse
  (setq analog-group-list (reverse analog-group-list)))

(defun analog-update-entries-in-current-group ()
  "Update the entries in the current group.
The current group is stored in `analog-current-group'."
  (setq analog-entries-in-current-group nil)
  (let ((entries analog-entries))
    (while entries
      (if (equal
           (analog-get-entry-property (car entries) 'group)
           analog-current-group)
          (add-to-list 'analog-entries-in-current-group (car entries)))
      (setq entries (cdr entries))))
  ;; Elements added first end up at the end of the list; reverse
  (setq analog-entries-in-current-group
        (reverse analog-entries-in-current-group)))

;; Commands associated with keybindings (and related functions)

(defun analog-refresh ()
  "Rebuild the internal variables/refresh the display."
  (interactive)
  (analog-init)
  (analog-refresh-display-buffer))

(defun analog-refresh-display-buffer ()
  "Refresh the displayed information."
  (interactive)
  ;; Set up the buffer and window if necessary
  (save-excursion
    (if (not (get-buffer "*analog*"))
        (progn
          (set-buffer (get-buffer-create "*analog*"))
          ;; set buffer local variables come after changing the major mode
          (analog-mode)
          (setq truncate-lines t)
          (setq buffer-read-only t))
      (set-buffer "*analog*"))
    ;; Update the displayed information
    (let ((inhibit-read-only t)
          (standard-output (current-buffer)))
      (erase-buffer)
      ;; Enter group header
      (cond
       ((boundp 'header-line-format)
        (setq header-line-format
            (format "%s%s%s" analog-group-string
                            (or analog-current-group "none")
                            (if analog-timer
                                (format " (updates every %d seconds)"
                                        analog-timer-period)
                              "")))
        (put-text-property 0 (length header-line-format)
                           'face 'analog-group-header-face
                           header-line-format))
       (t
        (insert (format "%s %s" analog-group-string
                        (or analog-current-group "none")))
      
        ;; If a timer is running, indicate that this is the case
        (if analog-timer
            (insert (format " (updates every %d seconds)" analog-timer-period)))
        (put-text-property (line-beginning-position) (line-end-position)
                           'face 'analog-group-header-face)
        (terpri)))
      ;; Go through list of entries
      (let ((entries analog-entries-in-current-group)
            entry)
        (while entries
          (setq entry (car entries))
          ;; Insert entry header
          (let ((type (analog-get-entry-type entry))
                file-name-start file-name-end)
            (insert (concat analog-entry-string "\""))
            (setq file-name-start (point))
            (insert (format "%s" entry))
            (setq file-name-end (point))
            (insert "\"")
            ;; Add text properties
            (put-text-property file-name-start (1+ file-name-start)
                               'analog-entry-start entry)
            (put-text-property (line-beginning-position) (line-end-position)
                               'face 'analog-entry-header-face)
            ;; Make clickable the associated file/command
            (put-text-property file-name-start file-name-end
                               'mouse-face 'highlight)
            (cond
             ((equal type 'file)
              (put-text-property file-name-start file-name-end
                                 'local-map analog-file-entry-map))
             ((equal type 'command)
              (put-text-property file-name-start file-name-end
                                 'local-map analog-command-entry-map))))
          (terpri)
          ;; Insert the file or output of command
          (let ((entry-start (point)))
            (analog-insert-entry entry)
            (put-text-property entry-start (point) 'face 'analog-entry-face))
          (setq entries (cdr entries))))
      (set-buffer-modified-p nil)))
  ;; Send a message to the echo area if analog-emit-update-messages is
  ;; t and the buffer is not in the current window
  (if (and analog-emit-update-messages
           (not (eq (window-buffer) (get-buffer "*analog*"))))
      (message (format "*analog* last updated at %s"
                       (format-time-string "%H:%M:%S")))))

(defun analog-insert-entry (entry)
  "Insert the output from ENTRY into the current buffer."
  (let ((type (analog-get-entry-type entry))
        (entry-string))
    (with-temp-buffer
      ;; Deal with entry depending on type, inserting into the
      ;; temporary buffer
      (cond
       ((eq type 'file)                 ; insert file
        (insert-file-contents entry))
       ((eq type 'command)              ; insert process output
        (let (command args)
          (setq command (car (split-string entry)))
          (setq args (cdr (split-string entry)))
          (apply 'call-process command nil t nil args)))
       (t
        (error "Unknown entry type: %s" (symbol-name type))))
      ;; Flush any regexps not wanted
      (goto-char (point-min))
      (let ((regexps (analog-get-entry-hide entry)))
        (cond
         ((null regexps))               ; do nothing)
         ((stringp regexps)
          (flush-lines regexps))
         ((listp regexps)
          (let ((list regexps))
            (while list
              (if (stringp (car list))
                  (flush-lines (car list)))
              (setq list (cdr list)))))
         (t
          (error "Unrecognized hide property for %s" entry))))
      ;; Keep any specified regexps
      (goto-char (point-min))
      (let ((regexps (analog-get-entry-keep entry)))
        (cond
         ((null regexps))               ; do nothing)
         ((stringp regexps)
          (keep-lines regexps))
         ((listp regexps)
          (let ((list regexps))
            (while list
              (if (stringp (car list))
                  (keep-lines (car list)))
              (setq list (cdr list)))))
         (t
          (error "Unrecognized hide property for %s" entry))))
      ;; Limit the output according to how many lines are required;
      ;; take into account whether we want the head or tail of the
      ;; file
      (let ((line-prop (analog-get-entry-lines entry))
            (position (analog-get-entry-position entry))
            extreme)
        ;; Move to beginning or end of input
        (cond
         ((equal position 'head)
          (goto-char (point-min)))
         ((equal position 'tail)
          (goto-char (point-max)))
         (t
          (error "Unrecognized position property for %s" entry)))
        (setq extreme (point))
        ;; keep the required number of lines
        (cond
         ((integerp line-prop)
          (cond
           ((equal position 'head)
            (forward-line line-prop))
           ((equal position 'tail)
            (forward-line (- line-prop)))))
         ((eq line-prop 'all)
          (cond
           ((equal position 'head)
            (goto-char (point-max)))
           ((equal position 'tail)
            (goto-char (point-min)))))
         (t
          (error "Unknown line type: %s" (symbol-name line-prop))))
        (setq entry-string (buffer-substring (point) extreme))))
    (insert entry-string)))

(defun analog-next-group ()
  "Choose the next group of entries.
A list of groups is kept internally in `analog-group-list'. The
current group is kept internally in `analog-current-group'."
  (interactive)
  (let* ((length (length analog-group-list))
         (position (- length
                      (length (member analog-current-group
                                      analog-group-list)))))
    (setq analog-current-group
          (nth
           (if (eq position (1- length))
               0
             (1+ position))
           analog-group-list)))
  (analog-update-entries-in-current-group)
  (analog-refresh-display-buffer))

(defun analog-previous-group ()
  "Choose the previous group of entries.
A list of groups is kept internally in `analog-group-list'. The
current group is kept internally in `analog-current-group'."
  (interactive)
  (let* ((length (length analog-group-list))
         (position (- length
                      (length (member analog-current-group
                                      analog-group-list)))))
    (setq analog-current-group
          (nth
           (if (eq position 0)
               (1- (length analog-group-list))
             (1- position))
           analog-group-list)))
  (analog-update-entries-in-current-group)
  (analog-refresh-display-buffer))

(defun analog-next-entry ()
  "Move point to the next group entry."
  (interactive)
  ;; Check if we are already on an entry
  (let (on-entry result)
    (when (get-text-property (point) 'analog-entry-start)
        (goto-char (1+ (point)))
        (setq on-entry t))
    ;; Move to next entry
    (setq result (next-single-property-change (point) 'analog-entry-start))
    (if result
        (goto-char result)
      (if on-entry
          (goto-char (1- (point))))
      (message "No more entries"))))

(defun analog-previous-entry ()
  "Move point to the previous group entry."
  (interactive)
  (cond
   ((bobp)
    (message "No more entries"))
   ;; We're one char after the start of the entry
   ((get-text-property (1- (point)) 'analog-entry-start)
    (goto-char (1- (point))))
   (t
    ;; Check if we are already on an entry
    (let (on-entry result)
      (when (get-text-property (point) 'analog-entry-start)
        (goto-char (1- (point)))
        (setq on-entry t))
      ;; Move to next entry
      (setq result (previous-single-property-change (point) 'analog-entry-start))
      (if result
          (goto-char (1- result))
        (if on-entry
            (goto-char (1+ (point))))
        (message "No more entries"))))))

(defun analog-bury-buffer ()
  "Bury the *analog* buffer."
  (interactive)
  (if (eq (window-buffer) (get-buffer "*analog*"))
      (quit-window)))

(defun analog-quit ()
  "Quit analog.
Kill the *analog* buffer and destroy the timer if present."
  (interactive)
  ;; Cancel the timer
  (if analog-timer
      (analog-toggle-timer))
  (if (buffer-live-p (get-buffer "*analog*"))
      (kill-buffer "*analog*")))

(defun analog-toggle-timer ()
  "Toggle analog timer."
  (if analog-timer
      (progn (cancel-timer analog-timer)
             (setq analog-timer nil))
    (setq analog-timer (run-with-timer analog-timer-period
                                       analog-timer-period
                                       'analog-timer-refresh-display-buffer))))

(defun analog-timer-refresh-display-buffer ()
  "Called by the timer to refresh the *analog* buffer.
This function will cancel any existing timer if the *analog* buffer is
dead."
  (if (buffer-live-p (get-buffer "*analog*"))
      (analog-refresh-display-buffer)
    ;; Timer must be non-nil; cancel it
    (analog-toggle-timer)))

(defun analog-toggle-timer-and-redisplay ()
  "Toggle analog timer.
Also redisplay the *analog* buffer."
  (interactive)
  (analog-toggle-timer)
  (analog-refresh-display-buffer))

(defun analog-kill-other-window-buffers ()
  "Kill buffers in other windows.
Also make sure beginning of *analog* buffer is subsequently shown."
  (interactive)
  (let ((analog-buffer (get-buffer "*analog*"))
        buffer-list
        buffer)
    (when (eq (current-buffer) analog-buffer)
      (setq buffer-list
            (delq analog-buffer
                  (mapcar #'window-buffer (cdr (window-list)))))
      (while buffer-list
        (setq buffer (car buffer-list))
        (if (buffer-live-p buffer)
            (kill-buffer buffer))
        (setq buffer-list (cdr buffer-list)))
      (delete-other-windows)
      (set-window-start (selected-window) (point-min)))))

(defun analog-show-help ()
  "Show key bindings."
  (interactive)
  ;; Build the prompt if necessary
  (if (not analog-show-help-prompt)
      (setq analog-show-help-prompt
            (let ((prompt "Choose key to display command:")
                  (map (cdr analog-mode-map)))
                  (while map
                    (setq prompt (format "%s %c" prompt
                                         (caar map)))
                    (setq map (cdr map)))
                  prompt)))
  ;; Select a key
  (let* ((map (cdr analog-mode-map))
         (key (read-char analog-show-help-prompt))
         (function (cdr (assoc key map)))
         documentation)
    (if function
        ;; Get first line of documentation if available
        (progn
          (if (setq documentation (documentation function))
              (setq documentation
                    (if (string-match "\\(.*\\.\\)" documentation)
                        (substring documentation (match-beginning 1)
                                   (match-end 1))
                      documentation))
            "no documentation available.")
          (message "'%c' runs %s -- %s" key
                   (symbol-name function)
                   documentation))
    (message "'%c' is not bound to any function." key))))

(defun analog-entry-on-line ()
  "Find the entry on the current line."
  (let (entry result)
    (setq result (next-single-property-change (line-beginning-position)
                                              'analog-entry-start))
    (if result
        (setq entry (get-text-property result 'analog-entry-start))
      (error "Cannot find analog entry on current line"))))

(defun analog-find-entry-file ()
  "Find the file associated with the entry at point."
  (interactive)
  (save-selected-window
    (let ((file (analog-entry-on-line)))
      (find-file-other-window file)
      (if (equal (analog-get-entry-position file) 'tail)
          (goto-char (point-max))))))

(defun analog-mouse-find-entry-file (event)
  "Find the file associated with the entry at mouse.
Argument EVENT is a mouse event."
  (interactive "e")
  (save-selected-window
    (goto-char (posn-point (event-start event)))
    (let ((file (analog-entry-on-line)))
      (find-file-other-window file)
      (if (equal (analog-get-entry-position file) 'tail)
          (goto-char (point-max))))))

(defun analog-run-entry-command ()
  "Run the command associated with the entry at point."
  (interactive)
  (let ((resize-mini-windows nil)
        (command (analog-entry-on-line)))
    (shell-command command)
    (when (equal (analog-get-entry-position command) 'tail)
      (save-selected-window
        (walk-windows
         (lambda (w)
           (when (equal (buffer-name (window-buffer w))
                      "*Shell Command Output*")
             (select-window w)
             (goto-char (point-max)))))))))

(defun analog-mouse-run-entry-command (event)
  "Run the command associated with the entry at mouse.
Argument EVENT is a mouse event."
  (interactive "e")
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (let ((resize-mini-windows nil)
          (command  (analog-entry-on-line)))
      (shell-command command)
      (when (equal (analog-get-entry-position command) 'tail)
        (save-selected-window
          (walk-windows
           (lambda (w)
             (when (equal (buffer-name (window-buffer w))
                          "*Shell Command Output*")
               (select-window w)
               (goto-char (point-max))))))))))

;; Define the major mode and keymap

(defvar analog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (ems-kbd "1") 'delete-other-windows)
    (define-key map (ems-kbd "<") 'analog-previous-group)
    (define-key map (ems-kbd ">") 'analog-next-group)
    (define-key map (ems-kbd "?") 'analog-show-help)
    (define-key map (ems-kbd "b") 'analog-bury-buffer)
    (define-key map (ems-kbd "i") 'analog-refresh)
    (when (fboundp 'window-list)
      (define-key map (ems-kbd "k") 'analog-kill-other-window-buffers))
    (define-key map (ems-kbd "n") 'analog-next-entry)
    (define-key map (ems-kbd "o") 'other-window)
    (define-key map (ems-kbd "p") 'analog-previous-entry)
    (define-key map (ems-kbd "q") 'analog-quit)
    (define-key map (ems-kbd "r") 'analog-refresh-display-buffer)
    (define-key map (ems-kbd "t") 'analog-toggle-timer-and-redisplay)
    map)
  "Keymap for analog mode.")

(defvar analog-file-entry-map
  (let ((map (copy-keymap analog-mode-map)))
    ;(define-key map (ems-kbd "<mouse-2>") 'analog-mouse-find-entry-file)
    (define-key map (ems-kbd "C-m")       'analog-find-entry-file)
    map)
  "Additional mapping for file entries.")

(defvar analog-command-entry-map
  (let ((map (copy-keymap analog-mode-map)))
    ;(define-key map (ems-kbd "<mouse-2>") 'analog-mouse-run-entry-command)
    (define-key map (ems-kbd "C-m")       'analog-run-entry-command)
    map)
  "Additional mapping for command entries.")

(defun analog-mode ()
  "Major mode for controlling the *analog* buffer."
  (kill-all-local-variables)
  (use-local-map analog-mode-map)
  (setq major-mode 'analog-mode)
  (setq mode-name "analog")
  (run-hooks 'analog-mode-hook))

(provide 'analog)

;;; analog.el ends here
