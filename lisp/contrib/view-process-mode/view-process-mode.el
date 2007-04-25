;;; $Id: view-process-mode.el,v 1.1 1996/10/15 22:52:59 raman Exp raman $
;;; 
;;; Copyright (C) 1994, 1995, 1996 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	This file defines the the view-process-mode, a mode for displaying
;;;	the current processes with ps on UNIX systems. There are also
;;;	commands to sort and filter the output and to send signals to the
;;;	processes.
;;;
;;;     You can display the processes with the command `view-processes'.
;;;	If you are familar with the UNIX ps command and its switches,
;;;	then you can also use the command `View-process-status' or
;;;	it's short cut `ps', which are asking for the command
;;;	switches.  You can also run the commands on a remote system
;;;	via rsh. For that you must give a prefix arg to the
;;;	commands. This leads to a question for the remote host name.
;;;
;;;	You need also the files: adapt.el
;;;				 view-process-system-specific.el
;;;				 view-process-xemacs.el
;;;				 view-process-emacs-19.el
;;; 
;;; Installation: 
;;;   
;;;	Put this file and the file adapt.el 
;;;     in one of your your load-path directories and
;;;	the following line in your ~/.emacs (without leading ;;;):
;;;	(autoload 'ps "view-process-mode"
;;;	"Prints a list with processes in the buffer `View-process-buffer-name'.
;;;     COMMAND-SWITCHES is a string with the command switches (ie: -aux).
;;;     IF the optional argument REMOTE-HOST is given, then the command will
;;;     be executed on the REMOTE-HOST. If an prefix arg is given, then the 
;;;     function asks for the name of the remote host."
;;;     t)
;;;
;;;	In the FSF Emacs 19 you should (but must not) put the following
;;;	line in your ~/.emacs:
;;;	(transient-mark-mode nil)

(provide 'view-process-mode)
(require 'view-process-system-specific)

(defconst View-process-package-version "2.4")

(defconst View-process-package-name "hm--view-process") 

(defconst View-process-package-maintainer "muenkel@tnt.uni-hannover.de")

(defun View-process-xemacs-p ()
  "Returns non nil if the editor is the XEmacs or lemacs."
  (or (string-match "Lucid" emacs-version)
      (string-match "XEmacs" emacs-version)))

(defun View-process-lemacs-p ()
  "Returns non nil if the editor is the lemacs."
  (string-match "Lucid" emacs-version))

(if (not (View-process-xemacs-p))
    (require 'view-process-adapt)
  )

(defvar View-process-status-command "ps"
  "*Command which reports process status (ps).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-status-command)

(defvar View-process-status-command-switches-bsd "-auxw"
  "*Switches for the command `view-processes' on BSD systems.
Switches which suppresses the header line are not allowed here.")

(defvar View-process-status-command-switches-system-v "-edaf"
  "*Switches for the command `view-processes' on System V systems.
Switches which suppresses the header line are not allowed here.")

(defvar View-process-status-last-command-switches nil
  "Switches of the last `View-process-status-command'.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-status-last-command-switches)

(defvar View-process-signal-command "kill"
  "*Command which sends a signal to a process (kill).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-signal-command)

(defvar View-process-renice-command "renice"
  "*Command which alter priority of running processes.")

(make-variable-buffer-local 'View-process-renice-command)

(defvar View-process-default-nice-value "4"
  "*Default nice value for altering the priority of running processes.")

(defvar View-process-rsh-command "rsh"
  "*Remote shell command (rsh).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-rsh-command)

(defvar View-process-uname-command "uname"
  "*The uname command (It returns the system name).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-uname-command)

(defvar View-process-uname-switches "-sr"
  "*Switches for uname, so that it returns the sysname and the release.")

(defvar View-process-test-command "test"
  "*The test command.")

(make-variable-buffer-local 'View-process-test-command)

(defvar View-process-test-switches "-x"
  "*Switches for test, to test if an executable exists.")

(defvar View-process-uptime-command "uptime"
  "*The uptime command. 
No idea at the moment, if this exists on all systems.
It should return some informations over the system.")

(make-variable-buffer-local 'View-process-uptime-command)

(defvar View-process-buffer-name "*ps*"
  "Name of the output buffer for the 'View-process-mode'.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-buffer-name)

(defvar View-process-mode-hook nil
  "*This hook is run after reading in the processes.")

(defvar View-process-motion-help t
  "*If non nil, then help messages are displayed during mouse motion.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-motion-help)

(defvar View-process-display-with-2-windows t
  "*Determines the display type of the `View-process-mode'.
If it is non nil, then 2 windows are used instead of one window.
In the second window are the header lines displayed.")

(defvar View-process-hide-header t
  "*The header lines in the view processes buffer are hide, if this is t.")

(make-variable-buffer-local 'View-process-hide-header)

(defvar View-process-truncate-lines t
  "*Truncates the liens in the view process buffer if t.")

(make-variable-buffer-local 'View-process-truncate-lines)

(defvar View-process-display-short-key-descriptions t
  "*Controls, whether short key descriptions are displayed or not.")

(defvar View-process-display-uptime t
  "*Controls, whether the uptime is displayed or not.")

(defvar View-process-use-font-lock t
  "*Determines, if the `font-lock-mode' should be used or not.")

(defvar View-process-ps-header-window-offset 2
  "Offset for the size of the ps header window.")

(defvar View-process-ps-header-window-size 0
  "Internal variable. The size of the window with the *ps header* buffer.")

(make-variable-buffer-local 'View-process-ps-header-window-size)

(defvar View-process-stop-motion-help nil
  "Internal variable. Stops motion help temporarily.")

(defvar View-process-deleted-lines nil
  "Internal variable. A list with lines, which are deleted by a filter.")

(make-variable-buffer-local 'View-process-deleted-lines)

(defvar View-process-header-buffer-name "*ps header*"
  "Name of the view process header buffer.")

(make-variable-buffer-local 'View-process-header-buffer-name)

(defvar View-process-header-mode-name "psheader"
  "Name of the `view process header mode'.")

(defvar View-process-header-mode-hook nil
  "*This hook is run after building the header buffer.")

(defvar View-process-header-mode-line-off t
  "t means do not display modeline in view-process-header-mode.
This does only work in the XEmacs 19.12 or higher.")

(defvar View-process-header-line-detection-list '("PID" "COMMAND" "COMD" "CMD")
  "*The header line is detected with the help of this list.
At least one of these words must be in a header line. Otherwise
an error is signaled. YOu must only change this list, if your ps
prodices header lines with strings, that are not in this list.") 

(defvar View-process-header-line-background "yellow"
  "*Background color of the header line.")

(defvar View-process-header-line-foreground "blue"
  "*Foreground color of the header line.")
(and (fboundp 'face-font)
(defvar View-process-header-line-font (face-font 'bold)
  "*Font of the header line")
)
(defvar View-process-header-line-underline-p t
  "*T, if the header line should be underlined.")

(defvar View-process-no-mark ?_
  "*A character with specifies, that a line isn't marked.")

(defvar View-process-signaled-line-background nil
  "*Background color of the line with a signaled or reniced process.")

(defvar View-process-signaled-line-foreground "grey80"
  "*Foreground color of the line with a signaled or reniced process.")
(and (fboundp 'face-font)
(defvar View-process-signaled-line-font (face-font 'italic)
  "*Font of the line with a signaled or reniced process.")
)
(defvar View-process-signaled-line-underline-p nil
  "*T, if the \"signaled line\" should be underlined.")

(defvar View-process-signaled-line-mark ?s
  "*A character, which is used as a mark for \"signaled lines\".")

(defvar View-process-signal-line-background nil
  "*Background color of the line with the process which should be signaled.")

(defvar View-process-signal-line-foreground "red"
  "*Foreground color of the line with the process which should be signaled.")
(and (fboundp 'face-font)
(defvar View-process-signal-line-font (face-font 'bold)
  "*Font of the line with the process which should be signaled.")
)
(defvar View-process-signal-line-underline-p nil
  "*T, if the \"signal line\" should be underlined.")

(defvar View-process-signal-line-mark ?K
  "*A character, which is used as a mark for \"signal lines\".")

(defvar View-process-renice-line-background nil
  "*Background color of the line with the process which should be reniced.")

(defvar View-process-renice-line-foreground "red"
  "*Foreground color of the line with the process which should be reniced.")
(and (fboundp 'face-font)
(defvar View-process-renice-line-font (face-font 'bold)
  "*Font of the line with the process which should be reniced.")
)
(defvar View-process-renice-line-underline-p nil
  "*T, if the \"renice line\" should be underlined.")

(defvar View-process-renice-line-mark ?N
  "*A character, which is used as a mark for \"renice lines\".")

(defvar View-process-child-line-background nil
  "*Background color of a line with a child process.")

(defvar View-process-child-line-foreground "darkviolet"
  "*Foreground color of a line with a child process.")
(and (fboundp 'face-font)
(defvar View-process-child-line-font (face-font 'italic)
  "*Font color of a line with a child process.")
)
(defvar View-process-child-line-underline-p nil
  "*T, if the \"line with a child process\" should be underlined.")

(defvar View-process-child-line-mark ?C
  "*A character, which is used as a mark for child processes.")

(defvar View-process-parent-line-background "LightBlue"
  "*Background color of a line with a parent process.")

(defvar View-process-parent-line-foreground "darkviolet"
  "*Foreground color of a line with a parent process.")
(and (fboundp 'face-font)
(defvar View-process-parent-line-font (face-font 'bold)
  "*Font  color of a line with a parent process.")
)
(defvar View-process-parent-line-underline-p t
  "*T, if the \"line with a parent\" should be underlined.")

(defvar View-process-parent-line-mark ?P
  "*A character, which is used as a mark for parent processes.")

(defvar View-process-single-line-background nil
  "*Background color of a line with a single line mark.")

(defvar View-process-single-line-foreground "darkblue"
  "*Foreground color of a line with a single line mark.")
(and (fboundp 'face-font)
(defvar View-process-single-line-font (face-font 'bold)
  "*Font  color of a line with a single line mark.")
)
(defvar View-process-single-line-underline-p t
  "*T, if the \"line with a single line mark\" should be underlined.")

(defvar View-process-single-line-mark ?*
  "*A character, which is used as a single line mark.")

(defvar View-process-font-lock-keywords
  (list
   (cons (concat "^" 
		 (char-to-string View-process-child-line-mark) 
		 " .*")
	 'View-process-child-line-face)
   (cons (concat "^" 
		 (char-to-string View-process-parent-line-mark) 
		 " .*")
	 'View-process-parent-line-face)
   (cons (concat "^\\" 
		 (char-to-string View-process-single-line-mark) 
		 " .*")
	 'View-process-single-line-face)
   (cons (concat "^" 
		 (char-to-string View-process-signaled-line-mark) 
		 " .*")
	 'View-process-signaled-line-face)
   (cons (concat "^" 
		 (char-to-string View-process-signal-line-mark) 
		 " .*")
	 'View-process-signal-line-face)
   (cons (concat "^" 
		 (char-to-string View-process-renice-line-mark) 
		 " .*")
	 'View-process-renice-line-face)
   )
  "The font lock keywords for the `View-process-mode'."
  )  

(defvar View-process-pid-mark-alist nil
  "Internal variable. An alist with marks and pids.")

(make-variable-buffer-local 'View-process-pid-mark-alist)

(defvar View-process-last-pid-mark-alist nil
  "Internal variable. An alist withthe last marks and pids.")

(make-variable-buffer-local 'View-process-last-pid-mark-alist)

(defvar View-process-sorter-and-filter nil
  "*A list, which specifies sorter and filter commands.
These commands will be run over the ps output, every time after
ps has create a new output.
The list consists of sublists, whereby every sublist specifies a 
command. The first element of each list is a keyword, which 
determines a command.
The following keywords are allowed:
 sort		- Sort the output by an output field
 filter		- Filter the output by an output field, delete non matching l.
 exclude-filter	- Filter the output by an output field, delete matching lines
 grep		- Filter the output by the whole line, delete non matching l.
 exclude-grep	- Filter the output by the whole line, delete matching lines
 reverse	- Reverse the order of the output lines.

The cdr of each sublist depends on the keyword. The following shows
the syntax of the different sublist types:
 (sort <fieldname>)
 (filter <fieldname> <regexp>)
 (exclude-filter <fieldname> <regexp>)
 (grep <regexp>)
 (exclude-grep <regexp>)
 (reverse)

Where <fieldname> is a string with determines the name of an output field
and <regexp> is a string with an regular expression. The output field names
are derived from the header line of the ps output.")

(defvar View-process-actual-sorter-and-filter nil
  "Internal variable. It holds the actual sorter and filter commands.
Don't change it!")

(make-variable-buffer-local 'View-process-actual-sorter-and-filter)

(defvar View-process-itimer-value 5
  "*Value of the view process itimer.")

(defvar View-process-system-type nil
  "Internal variable. Type of the system, on which the ps command is called.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-system-type)

(defvar View-process-remote-host nil
  "Internal variable. Name of the remote host or nil.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-remote-host)

(defvar View-process-header-start nil
  "Internal variable. Start of the ps output header line.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-header-start)

(defvar View-process-header-end nil
  "Internal variable. End of the ps output header line.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-header-end)

(defvar View-process-output-start nil
  "Internal variable. Start of the ps output (after the header).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-output-start)

(defvar View-process-output-end nil
  "Internal variable. End of the ps output (after the header).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-output-end)

(defvar View-process-old-window-configuration nil
  "Internal variable. Window configuration before the first ps command.")

(make-variable-buffer-local 'View-process-old-window-configuration)

(defvar View-process-max-fields nil
  "Internal variable. Number of output fields.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-max-fields)

(defvar View-process-field-names nil
  "Internal variable. An alist with the fieldnames and fieldnumbers.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-max-fields)

(defvar View-process-field-blanks-already-replaced nil
  "Internal variable. It is t, if blanks in fields are already replaced.")

(make-variable-buffer-local 'View-process-field-blanks-already-replaced)

(defvar View-process-kill-signals nil
  "An alist with the possible signals for the kill command.
Don't change it by hand!
The variable is initialised each time after running ps.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-kill-signals)

(defvar View-process-kill-signals-general
  '(("SIGHUP" "1") ("SIGKILL" "9") ("SIGTERM" "15")
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("16" "16") ("17" "17") ("18" "18") 
    ("19" "19") ("20" "20") ("21" "21") ("22" "22") ("23" "23") 
    ("24" "24") ("25" "25") ("26" "26") ("27" "27") ("28" "28") 
    ("29" "29") ("30" "30") ("31" "31"))
  "An alist with the possible signals for the kill command.
This list is used, if no system specific list is defined.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")

(defvar View-process-default-kill-signal "SIGTERM"
  "*Default signal for the function `View-process-send-signal-to-process'.
The string must be also in the alist `View-process-kill-signals'!")

(defvar View-process-pid-field-name "PID"
  "*The name of the field with the PID's.
The name must be the same as in the first outputline of the
command `View-process-status-command' (ps).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-pid-field-name)

(defvar View-process-ppid-field-name "PPID"
  "*The name of the field with the PPID's.
The name must be the same as in the first outputline of the
command `View-process-status-command' (ps).
The variable is buffer local.")

(make-variable-buffer-local 'View-process-ppid-field-name)

(defvar View-process-host-names-and-system-types nil
  "A list with the names and the system types of hosts.
Each entry of the list looks like the following:
  (<hostname> (<system-type> <version-number> <bsd-or-system-v>
               <field-name-descriptions> 
               <kill-signals>))
Here are some examples:
  (\"daedalus\" (\"sunos\" \"4\" \"bsd\" 
               View-process-field-name-descriptions-sunos4
               View-process-kill-signals-sunos4))
  (\"bach\" (\"linux\" nil \"bsd\"
           nil
           View-process-kill-signals-linux
           ))
  (\"cesar\" (nil nil \"bsd\"))
The list will be anhanced by the program, each time you run ps on
a new system. But you can also set this variable by hand in your 
.emacs. If the host name is found in this list, then the system 
type will not be checked again." 
  )

(defvar View-process-status-history nil
  "A list with the command switch history of the status command (ps).")

(defvar View-process-remote-host-history nil
  "A list with the remote host history.")

(defvar View-process-field-name-history nil
  "A list with the field name history.")

(defvar View-process-filter-history nil
  "A list with the filter history.")

(defvar View-process-signal-history nil
  "A list with the signal history.")

(defvar View-process-field-name-descriptions nil
  "Help list with the descriptions of ps fields.
Don't change it by hand!
The variable is initialised each time after running ps.
The variable is buffer local.")

(make-variable-buffer-local 'View-process-field-name-descriptions)

(defvar View-process-field-name-descriptions-general 
  '(
    ("m" "Mark column of the View Processes Mode.") ; not a real field name
    ("ADDR" "The memory address of the process. ")
    ("%CPU" "CPU usage in percentage.")
    ("%MEM" "Real Memory usage in percentage.")
    ("COMMAND" "Command Name.")
    ("F" ("Status= "
	  ("0" "0=not in main memory.")
	  ("1" "1=in main memory.")
	  ("2" "2=system process.")
	  ("4" "4=blocked in the main memory.")
	  ("10" "10=swapped out.")
	  ("20" "20=controlled by another one.")))
    ("NI" "UNIX nice value, a positive value means less CPU time.")
    ("PAGEIN" "Number of major page faults.")
    ("PGID" "Process group id. ")
    ("PID" "The process id.")
    ("PPID" "The process id of the parent process.")
    ("PRI" "Priority, a big value is a small priority.")
    ("RSS" "Real (resident set) size, KBytes of program in memory.")
    ("SHARE" "Shared memory")
    ("SID" "ID of the session to which the process belongs. ")
    ("SIZE" "Virtual image size, size of text+data+stack (in KByte ?).")
    ("START" "Start time.")
    ("STAT" ("Status. "
	     ("R" "R=runnable. ")
	     ("S" "S=sleeping. ")
	     ("D" "D=un-interruptible sleep (eg disk or NFS I/O). ")
	     ("T" "T=stopped or traced. ")
	     ("Z" "Z=zombie (terminated). ")
	     ("W" "W=waiting on an event. ")
	     ("I" "I=intermediate status. ")
	     ("N" "N=started with nice. ")
	     ))
    ("SWAP" "Kilobytes (with -p pages) on swap device.")
    ("TIME" "Elapsed process time.")
    ("TPGID" "Process group id of the associated terminal. ")
    ("TRS" "Text resident size.")
    ("TT" ("Dialog station. " ("?" "?=No dialog station")))
    ("TTY" ("Dialog station. " ("?" "?=No dialog station")))
    ("UID" "User Id.")
    ("USER" "Owner of the process.")
    ("WCHAN" "Name of the kernel function where the process is sleeping.")
    )
  "Help list with the descriptions of ps fields.
This is a general list, which should be true for many systems.
This list will only be used, if there is no entry in a special 
list for the system.")

(defvar View-process-insert-blank-alist 
  '(("SZ" behind-predecessor 0)
    ("SIZE" behind-predecessor 0)
    ("RSS" behind-predecessor 0)
    ("START" behind 1))
  "Determines places in the output, where a blank should be inserted.
It is an alist and each sublist has the following structure:
 (field-name position-descriptor offset)
The field-name is a string with the name of the field.
The position-descriptor determines a position. It has one of the
following values:
`in-front' => insert in front of the field.
`in-front-successor' => insert in front of the successor of the field.
`behind' => insert behind of the field.
`behind-predecessor' => insert behind the predecessor of the field.
The offset is an integer , which specifies an offset.")

(defvar View-process-mode-syntax-table nil
  "Syntax table for the `View-process-mode'.")

(if (not View-process-mode-syntax-table)
    (let ((i 0))
      (setq View-process-mode-syntax-table (make-syntax-table))
      (setq i ?!)
      (while (<= i ?#)
	(modify-syntax-entry i "w" View-process-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?, "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?. "w" View-process-mode-syntax-table)
      (setq i ?:)
      (while (<= i ?\;)
	(modify-syntax-entry i "w" View-process-mode-syntax-table)
	(setq i (1+ i)))
      (setq i ??)
      (while (<= i ?@)
	(modify-syntax-entry i "w" View-process-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?\\ "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?^ "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?` "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?' "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?~ "w" View-process-mode-syntax-table)
      (modify-syntax-entry ?¡ "w" View-process-mode-syntax-table)
      ))

(defvar View-process-digit-bindings-send-signal nil
  "The digits 1 to 9 will be bind to send signal commands, if t.")

(defvar View-process-mode-mark-map nil
  "Local subkeymap for View-process-mode buffers.")

(if View-process-mode-mark-map
    nil
  (setq View-process-mode-mark-map (make-keymap))
  (define-key View-process-mode-mark-map "m" 'View-process-mark-current-line)
  (define-key View-process-mode-mark-map "u" 'View-process-unmark-current-line)
  (define-key View-process-mode-mark-map "U" 'View-process-unmark-all)
  (define-key View-process-mode-mark-map "c" 
    'View-process-mark-childs-in-current-line)
  (define-key View-process-mode-mark-map "l" 'View-process-reset-last-marks)
  )

(defvar View-process-mode-i-map nil
  "Local subkeymap for View-process-mode buffers.")

(if View-process-mode-i-map
    nil
  (setq View-process-mode-i-map (make-keymap))
  (define-key View-process-mode-i-map "s" 'View-process-start-itimer)
  (define-key View-process-mode-i-map "d" 'View-process-delete-itimer)
  )

(defvar View-process-mode-comma-map nil
  "Local subkeymap for View-process-mode buffers.")

(if View-process-mode-comma-map
    nil
  (setq View-process-mode-comma-map (make-keymap))
  (define-key View-process-mode-comma-map "k"
    'View-process-send-signal-to-processes-with-mark)
  (define-key View-process-mode-comma-map "a"
    'View-process-renice-processes-with-mark))

(defvar View-process-mode-period-map nil
  "Local subkeymap for View-process-mode buffers.")

(if View-process-mode-period-map
    nil
  (setq View-process-mode-period-map (make-keymap))
  (define-key View-process-mode-period-map "f"
    'View-process-filter-region-by-current-field)
  (define-key View-process-mode-period-map "l"
    'View-process-filter-region)
  (define-key View-process-mode-period-map "s"
    'View-process-sort-region-by-current-field)
  (define-key View-process-mode-period-map "r"
    'View-process-reverse-region)
  (define-key View-process-mode-period-map "k"
    'View-process-send-signal-to-processes-in-region)
  (define-key View-process-mode-period-map "a"
    'View-process-renice-processes-in-region)
  (define-key View-process-mode-period-map "v"
    'View-process-status))
    

(defvar View-process-mode-map nil 
  "Local keymap for View-process-mode buffers.")

(if View-process-mode-map
    nil
  (setq View-process-mode-map (make-keymap))
  (define-key View-process-mode-map "q" 'View-process-quit)
  (define-key View-process-mode-map "V" 'View-process-display-version)
  (define-key View-process-mode-map " " 'scroll-up)
  (define-key View-process-mode-map "b" 'scroll-down)
  (define-key View-process-mode-map "t" 'View-process-toggle-truncate-lines)
  (define-key View-process-mode-map "u" 'View-process-status-update)
  (define-key View-process-mode-map "U" 
    'View-process-remove-all-filter-and-sorter)
  (define-key View-process-mode-map "g" 'revert-buffer)
;  (define-key View-process-mode-map "v" 'View-process-status)
  (define-key View-process-mode-map "v" 'view-processes)
  (define-key View-process-mode-map "f"
    'View-process-filter-by-current-field-g)
  (define-key View-process-mode-map "F"
    'View-process-filter-output-by-current-field)
  (define-key View-process-mode-map "l"
    'View-process-filter-g)
  (define-key View-process-mode-map "L"
    'View-process-filter-output)
  (define-key View-process-mode-map "s"
    'View-process-sort-by-current-field-g)
  (define-key View-process-mode-map "S"
    'View-process-sort-output-by-current-field)
  (define-key View-process-mode-map "r"
    'View-process-reverse-g)
  (define-key View-process-mode-map "R"
    'View-process-reverse-output)
  (define-key View-process-mode-map "k"
    'View-process-send-signal-to-processes-g)
  (define-key View-process-mode-map "K"
    'View-process-send-signal-to-process-in-line)
  (define-key View-process-mode-map "a"
    'View-process-renice-processes-g)
  (define-key View-process-mode-map "A"
    'View-process-renice-process-in-line)
;  (define-key View-process-mode-map "k"
;    'View-process-send-signal-to-process)
  (define-key View-process-mode-map "?"
    'View-process-which-field-name)
  (define-key View-process-mode-map "h"
    'View-process-show-field-names)
  (define-key View-process-mode-map "e"
    'View-process-display-emacs-pid)
  (define-key View-process-mode-map "w" 'View-process-show-pid-and-command)
  (define-key View-process-mode-map "n" 'View-process-next-field)
  (define-key View-process-mode-map "p" 'View-process-previous-field)
  (define-key View-process-mode-map "<" 'View-process-output-start)
  (define-key View-process-mode-map ">" 'View-process-output-end)
  (define-key View-process-mode-map [return]
    'View-process-goto-first-field-next-line)
  (define-key View-process-mode-map "M" 'View-process-submit-bug-report)
  (define-key View-process-mode-map "m" View-process-mode-mark-map)
  (define-key View-process-mode-map "." View-process-mode-period-map)
  (define-key View-process-mode-map "," View-process-mode-comma-map)
  (define-key View-process-mode-map "i" View-process-mode-i-map)
  )

(defvar View-process-pulldown-menu-name "Processes"
  "Name of the pulldown menu in the `View-process-mode'.")

(defvar View-process-pulldown-menu nil
  "Pulldown menu list for the `View-process-mode'.")

(defvar View-process-region-menu nil
  "Menu list for the `View-process-mode', used if a region is active.")

(defvar View-process-marked-menu nil
  "Menu list for the `View-process-mode', used if marked lines exists.
Not used, if a region is active.")

(defvar View-process-non-region-menu nil
  "Menu list for the `View-process-mode', used if a region is not active.")

(defvar View-process-mode-name "Processes"
  "Name of the `view process mode'.")

(defun View-process-make-field-postition-alist-1 ()
"Internal function of View-process-make-field-postition-alist."
  (if (>= (point) View-process-header-end)
      nil
    (let (start end)
      (skip-chars-forward " ")
      (setq start (current-column))
      (skip-chars-forward "^ ")
      (setq end (current-column))
      (cons (list start end) 
	    (View-process-make-field-postition-alist-1))))
  )

(defun View-process-make-field-postition-alist ()
  "Returns an alist with the start and end positions of each field.
The list looks like ((start1 end1) (start2 end2) ...)."
  (save-restriction
    (widen)
    (goto-char View-process-header-start)
    (View-process-make-field-postition-alist-1)))

(defun View-process-overwrite-chars-in-region (begin end char)
  "Overwrite region between BEGIN and END with CHAR."
  (let ((region-begin (if (< begin end) begin end))
	(region-end (if (> end begin) end begin)))
    (save-excursion
      (goto-char region-begin)
      (while (> region-end (point))
	(delete-char 1)
	(View-process-insert-and-inherit char)))))

(defun View-process-replaces-blanks-in-the-fields-of-this-line 
  (field-position-alist)
  "Replaces the blanks in the fields of this line with underscores.
FIELD-POSITION-ALIST is an alist with the name and the 
aproximated start and end positions of each field."
  (if (cdr field-position-alist) ; don't change the last field
      (let ((field-start (+ (View-process-return-beginning-of-line)
			    (car (car field-position-alist))))
	    (field-end (+ (View-process-return-beginning-of-line)
			  (car (cdr (car field-position-alist)))))
	    (next-field-start (+ (View-process-return-beginning-of-line)
				 (car (car 
				       (cdr field-position-alist))))))
	(goto-char field-start)
	(skip-chars-forward " ")
	(if (> (point) field-end)
	    (progn (goto-char field-start) 
		   (delete-char 1) 
		   (View-process-insert-and-inherit "_"))
	  (let ((search-result (search-forward-regexp "[ ]+" field-end t))
		(match-beginning nil))
	    (if search-result
		(if (not (= search-result field-end))
		    (View-process-overwrite-chars-in-region (match-beginning 0)
							    (match-end 0)
							    ?_)
		  (setq match-beginning (match-beginning 0))
		  (if (and (search-forward-regexp "[^ ]+" next-field-start t)
			   (not (eq (point) next-field-start)))
		      (View-process-overwrite-chars-in-region 
		       match-beginning
		       (match-beginning 0)
		       ?_))))
	    ))
	(View-process-replaces-blanks-in-the-fields-of-this-line
	 (cdr field-position-alist)))))

(defun View-process-replaces-blanks-in-fields ()
  "Replaces the blanks in fields with underscrores."
  (save-excursion
    (save-window-excursion
      (let ((field-position-alist (View-process-make-field-postition-alist))
	    (read-only buffer-read-only))
	(setq buffer-read-only nil)
	(goto-char View-process-output-start)
	(while (< (point) View-process-output-end)
	  (beginning-of-line)
	  (View-process-replaces-blanks-in-the-fields-of-this-line
	   field-position-alist)
	  (forward-line))
	(setq buffer-read-only read-only)))))

(defun View-process-replaces-blanks-in-fields-if-necessary ()
  "Replaces blanks in fields, if necessary.
For that it checks `View-process-field-blanks-already-replaced'."
  (if View-process-field-blanks-already-replaced
      nil
    (View-process-replaces-blanks-in-fields)
    (setq View-process-field-blanks-already-replaced t)))

(defun View-process-insert-column-in-region (char 
					     column 
					     begin 
					     end
					     &optional overwrite
					               not-looking-at)
  "Inserts the CHAR at the COLUMN in the region from BEGIN TO END.
The first line must have sufficient columns. No tabs are allowed.
If the optional argument OVERWRITE is non nil, then the CHAR 
overwrites the char in the COLUMN.
The optional argument NOT-LOOKING-AT is nil or a regular expression.
In the second case the insertation will only be done, if NOT-LOOKING-AT
isn't a string starting at the column."
  (save-excursion
    (let ((no-of-lines (count-lines begin end))
	  (line 1))
      (goto-char begin)
      (beginning-of-line)
      (while (<= line no-of-lines)
      (forward-char column)
	(if (not (= (current-column) column))
	    (View-process-insert-and-inherit 
	     (make-string (- column (current-column)) ? )))
	(if overwrite 
	    (progn
	      (delete-char -1)
	      (View-process-insert-and-inherit char))
	  (if (or (not not-looking-at)
		  (not (looking-at not-looking-at)))
	      (progn
		(View-process-insert-and-inherit char)
		(forward-char -1)
		)))
	(forward-line 1)
	(setq line (1+ line))))))

(defun View-process-insert-blank-in-column (column 
					    &optional overwrite
					              not-looking-at)
  "Inserts a blank in all lines of the ps output in column COLUMN.
If OVERWRITE is non nil, then it overwrites the old column char.
The optional argument NOT-LOOKING-AT is nil or a regular expression.
In the second case the insertation will only be done, if NOT-LOOKING-AT
isn't a string starting at the column."
  (let ((read-only buffer-read-only))
    (setq buffer-read-only nil)
    (View-process-insert-column-in-region ? 
					  column 
					  View-process-header-start
					  View-process-output-end
					  overwrite
					  not-looking-at)
    (setq View-process-output-end (point-max))
    (setq buffer-read-only read-only)))

;(defun View-process-insert-blanks-at-line-start ()
;  "Inserts some blanks at the beginning of each output line.
;This space is used for the marks."
;  (save-excursion
;    (goto-char View-process-header-start)
;    (insert "m ")
;    (forward-line)
;    (while (< (point) View-process-output-end)
;      (insert "_ ")
;      (forward-line))))

(defun View-process-insert-blanks-at-line-start ()
  "Inserts some blanks at the beginning of each output line.
This space is used for the marks."
  (save-excursion
    (goto-char View-process-output-end)
    (forward-line -1)
    (while (> (point) View-process-header-start)
      (insert "_ ")
      (forward-line -1))
    (insert "m ")))

(defun View-process-return-position (field-name position-descriptor)
  "Returns a position deppending on the FIELD-NAME and the POSITION-DESCRIPTOR.
The POSITION-DESCRIPTOR must be one of the 4 values: `in-front',
`in-front-successor', `behind' and `behind-predecessor'.
If the FIELD-NAME isn't in the header-line, then it return nil."
  (save-excursion
    (goto-char View-process-header-start)
    (beginning-of-line)
    (if (search-forward field-name (View-process-return-end-of-line) t)
	(cond ((eq position-descriptor 'behind-predecessor)
	       (goto-char (match-beginning 0))
	       (skip-chars-backward " ")
	       (current-column))
	      ((eq position-descriptor 'behind)
	       (current-column))
	      ((eq position-descriptor 'in-front)
	       (goto-char (match-beginning 0))
	       (current-column))
	      ((eq position-descriptor 'in-front-successor)
	       (skip-chars-forward " ")
	       (current-column))))))

(defun View-process-split-merged-fields (insert-blank-alist)
  "Tries to split merged fields.
At the moment this is done by inserting a blank between fields,
which are often merged together. The fields are determined by the
alist INSERT-BLANK-ALIST."
  (cond (insert-blank-alist
	 (let ((position (View-process-return-position 
			  (car (car insert-blank-alist))
			  (car (cdr (car insert-blank-alist))))))
	   (if position
	       (View-process-insert-blank-in-column
		(+ position
		   (car (cdr (cdr (car insert-blank-alist)))))
		nil
		"[^ ][^ ]? ")))
	 (View-process-split-merged-fields (cdr insert-blank-alist)))
	(t)))

(defun View-process-replace-colons-with-blanks ()
  "Replaces colons with blanks, if a colon is also in the header line.
This fixes the output of the IRIX ps on SGI's."
  (save-excursion
    (goto-char View-process-header-start)
    (while (search-forward ":" (View-process-return-end-of-line) t)
      (View-process-insert-blank-in-column (current-column)
					   t))))

(defun View-process-mode ()
  "Mode for displaying and killing processes.
The mode has the following keybindings: 
\\{View-process-mode-map}.

The first column of each outputline will be used to display marked lines.
The following mark signs are possible (one can change them by changing
the variables in the second column of the following table):

Sign	Variable			Description
_	View-process-no-mark		Process isn't marked
*	View-process-single-line-mark	The normal mark.
C	View-process-child-line-mark	Marked as a child of P (see also P)
K	View-process-signal-line-mark	Used during signaling
N	View-process-renice-line-mark	Used during renicing
P	View-process-parent-line-mark	Marked as the parent of P (see also C)
s	View-process-signaled-line-mark	Process was signaled or reniced.

The signal and renice commands are working also on marked processes!"
;  (kill-all-local-variables)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'View-process-revert-buffer)
  (View-process-change-display-type View-process-display-with-2-windows)
  (use-local-map View-process-mode-map)
  (set-syntax-table View-process-mode-syntax-table)
  (setq major-mode 'View-process-mode
	mode-name View-process-mode-name)
;  (View-process-replaces-blanks-in-fields)
  (setq View-process-deleted-lines nil)
  (View-process-call-sorter-and-filter View-process-actual-sorter-and-filter)
  (setq truncate-lines View-process-truncate-lines)
  (View-process-install-pulldown-menu)
;  (View-process-install-mode-motion)
  (View-process-hide-header (and View-process-display-with-2-windows
				 View-process-hide-header))
  (View-process-install-font-lock)
  (View-process-install-mode-motion)
  (run-hooks 'View-process-mode-hook)
  )

(defun View-process-build-field-name-list ()
  "Returns an alist with the field names and the field number.
The list looks like ((\"USER\" 1) (\"PID\" 2) (\"COMMAND\" 3))."
  (goto-char View-process-header-start)
  (forward-word 1)
  (setq View-process-field-names '())
  (let ((i 1))
    (while (<= (point) View-process-header-end)
      (setq View-process-field-names (cons (list (current-word) i)
					   View-process-field-names))
      (setq i (1+ i))
      (forward-word 1))))

(defun View-process-field-name-exists-p (field-name)
  "Returns non nil, if the field FIELD_NAME exists."
  (assoc field-name View-process-field-names))

(defun View-process-translate-field-name-to-position (field-name)
  "Returns the position of the field with the name FIELD-NAME."
  (car (cdr (assoc field-name View-process-field-names)))
  )

(defun View-process-translate-field-position-to-name (position)
  "Returns the field name of the field with the position POSITION."
  (if (> position View-process-max-fields)
      (car (View-process-assoc-2th View-process-max-fields 
				   View-process-field-names))
    (car (View-process-assoc-2th position View-process-field-names))
    ))

(defun View-process-get-system-type-from-host-list (host-name)
  "Returns nil, or the system type of the host with the name HOST-NAME."
  (car (cdr (assoc host-name View-process-host-names-and-system-types))))

(defun View-process-put-system-type-in-host-list (host-name system-type)
  "Puts the HOST-NAME and the SYSTEM-TYPE in a special host list.
The list has the name `View-process-host-names-and-system-types'."
  (if (not (member (list host-name system-type)
		   View-process-host-names-and-system-types))
      (setq View-process-host-names-and-system-types
	    (cons (list host-name system-type)
		  View-process-host-names-and-system-types))))

(defun View-process-bsd-or-system-v (&optional remote-host)
  "This function determines, if the system is a BSD or a System V.
For that it uses the ps command.
If REMOTE-HOST is non nil, then the system of the REMOTE-HOST will 
be tested."
    (if remote-host
	(if (eq 0 (call-process View-process-rsh-command
				nil
				nil
				nil
				remote-host
				(concat View-process-status-command 
					" " 
					"-dfj")))
	    "system-v"
	  "bsd")
      (if (eq 0 (call-process View-process-status-command 
			      nil 
			      nil
			      nil
			      "-dfj"))
	  "system-v"
	"bsd")))

(defun View-process-program-exists-p (program &optional remote-host)
  "Returns t, if the PROGRAM exists.
If REMOTE_HOST is non nil, then the program will be searched remote
on that host."
  (if remote-host
      (or (= 0 (call-process View-process-rsh-command
			     nil
			     nil
			     nil
			     remote-host
			     (concat View-process-test-command
				     " "
				     View-process-test-switches
				     " "
				     program)))
	  (= 0 (call-process View-process-rsh-command
			     nil
			     nil
			     nil
			     remote-host
			     (concat View-process-test-command
				     " "
				     View-process-test-switches
				     " "
				     "/bin/" 
				     program)))
	  (= 0 (call-process View-process-rsh-command
			     nil
			     nil
			     nil
			     remote-host
			     (concat View-process-test-command
				     " "
				     View-process-test-switches
				     " "
				     "/usr/bin/"
				     program))))
    (or (= 0 (call-process View-process-test-command
			   nil
			   nil
			   nil
			   View-process-test-switches
			   program))
	(= 0 (call-process View-process-test-command
			   nil
			   nil
			   nil
			   View-process-test-switches
			   (concat "/bin/" program)))
	(= 0 (call-process View-process-test-command
			   nil
			   nil
			   nil
			   View-process-test-switches
			   (concat "/usr/bin/" program))))))

(defun View-process-search-system-type-in-system-list-1 (system-type
							 system-list)
  "Internal function of `View-process-search-system-type-in-system-list'."
  (cond ((not system-list) nil)
	((equal system-type (car (car system-list)))
	 (cons (car system-list)
	       (View-process-search-system-type-in-system-list-1 
		system-type
		(cdr system-list))))
	(t (View-process-search-system-type-in-system-list-1 system-type
							     (cdr system-list))
	   )))

(defun View-process-search-system-type-in-system-list (system-type system-list)
  "Searches the SYSTEM-TYPE in SYSTEM-LIST.
It returns the entry or nil, if the SYSTEM-TYPE isn't in the list.
If more then one entry with the same SYSTEM-TYPE are found, then the
version number is also checked. If the version number isn't in the 
list, then nil is returned."
  (let ((system-type-entries (View-process-search-system-type-in-system-list-1
			      (car system-type)
			      system-list)))
    (if system-type-entries
	(if (= 1 (length system-type-entries))
	    (car system-type-entries)
	  (View-process-assoc-2th (car (cdr system-type)) system-type-entries))
      nil)))


(defun View-process-generalize-system-type (system-type &optional remote-host)
  "Generalize the SYSTEM-TYPE.
Determines, if the system is in the `View-process-specific-system-list'
and if it is a BSD or a System V system. It returns a list which looks 
like the following: (<system-type> <version-no> <bsd-or-system-v>).
The elements <system-type> and <version-no> are set to nil, if the 
<system-type> isn't in the `View-process-specific-system-list'. In that 
case the third element (<bsd-or-system-v>) is determined with the help
of the ps output. if REMOTE-HOST is non nil, the the ps command to check
the system type is run on the remote host REMOTE-HOST."
  (let ((new-system-type (View-process-search-system-type-in-system-list
			  system-type
			  View-process-specific-system-list)))
    (if new-system-type
	new-system-type
      (list nil nil (View-process-bsd-or-system-v)))))

(defun View-process-get-local-system-type ()
  "Returns the system type of the local host."
  (let ((system-type (View-process-get-system-type-from-host-list
		      (system-name))))
    (if (not system-type)  ; t, if the host isn't in the list
	(progn
	  (if (View-process-program-exists-p View-process-uname-command)
	      (save-excursion
		(let ((buffer (generate-new-buffer "*system-type*")))
		  (call-process View-process-uname-command
				nil
				buffer
				nil
				View-process-uname-switches)
		  (set-buffer buffer)
		  (forward-line -1)
		  (setq system-type (downcase (current-word)))
		  (forward-word 2)
		  (setq system-type 
			(list system-type (downcase (current-word))))
		  (kill-buffer buffer)
		  ;; determine, if the system is in the
		  ;; View-process-specific-system-list and if it is 
		  ;; a BSD or a System V system;
		  ;; The system type will be set to nil, 
		  ;; if it isn't in the list
		  (setq system-type (View-process-generalize-system-type
				     system-type))
		  ))
	    (setq system-type (list nil nil (View-process-bsd-or-system-v))))
	  (View-process-put-system-type-in-host-list (system-name)
						     system-type)
	  system-type)
      system-type)))

(defun View-process-get-remote-system-type ()
  "Returns the system type of the remote host `View-process-remote-host'."
  (let ((system-type (View-process-get-system-type-from-host-list 
		      View-process-remote-host)))
    (if system-type  ; nil, if the host isn't in the list
	system-type
      (if (View-process-program-exists-p View-process-uname-command
					 View-process-remote-host)
	  (let ((buffer (generate-new-buffer "*system-type*")))
	    (save-excursion
	      (call-process View-process-rsh-command
			    nil
			    buffer
			    nil
			    View-process-remote-host
			    (concat View-process-uname-command
				    " "
				    View-process-uname-switches))
	      (set-buffer buffer)
	      (forward-line -1)
	      (setq system-type (downcase (current-word)))
	      (forward-word 2)
	      (setq system-type 
		    (list system-type (downcase (current-word))))
	      (kill-buffer buffer)
	      ;; determine, if the system is in the
	      ;; View-process-specific-system-list and if it is 
	      ;; a BSD or a System V system;
	      ;; The system type will be set to nil, 
	      ;; if it isn't in the list
	      (setq system-type (View-process-generalize-system-type
				 system-type
				 View-process-remote-host))
	      ))
	(setq system-type (list nil nil (View-process-bsd-or-system-v
					 View-process-remote-host))))
      (View-process-put-system-type-in-host-list View-process-remote-host
						 system-type)
      system-type)))

(defun View-process-get-system-type ()
  "Returns the type of the system on which ps was executed."
  (if View-process-remote-host
      (View-process-get-remote-system-type)
    (View-process-get-local-system-type)
    ))

(defun View-process-get-kill-signal-list (system-type)
  "Returns a kill signal list for the SYSTEM-TYPE."
  (if (= 3 (length system-type))
      (if (string= "bsd" (nth 2 system-type))
	  (if View-process-kill-signals-bsd
	      View-process-kill-signals-bsd
	    View-process-kill-signals-general)
	(if View-process-kill-signals-system-v
	    View-process-kill-signals-system-v
	  View-process-kill-signals-general))
    (if (eval (nth 4 system-type))
	(eval (nth 4 system-type))
      (if (string= "bsd" (nth 2 system-type))
	  (if View-process-kill-signals-bsd
	      View-process-kill-signals-bsd
	    View-process-kill-signals-general)
	(if View-process-kill-signals-system-v
	    View-process-kill-signals-system-v
	  View-process-kill-signals-general)))))

(defun View-process-get-field-name-description-list (system-type)
  "Returns a field name description list for the SYSTEM-TYPE.
It returns nil, if no system specific list exists."
  (if (= 3 (length system-type))
      (if (string= "bsd" (nth 2 system-type))
	  (if View-process-field-name-descriptions-bsd
	      View-process-field-name-descriptions-bsd)
	(if View-process-field-name-descriptions-system-v
	    View-process-field-name-descriptions-system-v))
    (if (eval (nth 3 system-type))
	(eval (nth 3 system-type))
      (if (string= "bsd" (nth 2 system-type))
	  (if View-process-field-name-descriptions-bsd
	      View-process-field-name-descriptions-bsd)
	(if View-process-field-name-descriptions-system-v
	    View-process-field-name-descriptions-system-v)))))

(defun View-process-init-internal-variables (use-last-sorter-and-filer)
  "Init internal variables. 
 (without `View-process-header-start').
If USE-LAST-SORTER-AND-FILER is t, then 
'View-process-actual-sorter-and-filter' will not be changed"
  ;; don't replace blanks now
  (setq View-process-field-blanks-already-replaced t) 
  
  (goto-char View-process-header-start)
  (end-of-line)
  (setq View-process-header-end (point))
  ;;  (newline)
  (forward-line)
  (setq View-process-output-start (point))
  (setq View-process-output-end (point-max))
  (goto-char View-process-header-end)
  (forward-word -1)
  (setq View-process-max-fields (View-process-current-field-number))
  (View-process-build-field-name-list)
  (setq View-process-system-type (View-process-get-system-type))
  (setq View-process-kill-signals (View-process-get-kill-signal-list
				   View-process-system-type))
  (setq View-process-field-name-descriptions
	(View-process-get-field-name-description-list View-process-system-type)
	)
  ;; Replace the blanks the next time if it is necessary
  (setq View-process-field-blanks-already-replaced nil)
  (if (not use-last-sorter-and-filer)
      (setq View-process-actual-sorter-and-filter
	    View-process-sorter-and-filter))

  (if View-process-pid-mark-alist
      (progn
	(setq View-process-last-pid-mark-alist View-process-pid-mark-alist)
	(setq View-process-pid-mark-alist nil)))
)

(defun View-process-insert-short-key-descriptions ()
  "Insert short key descriptions at the current point.
If `View-process-display-short-key-descriptions' is nil, then
nothing will be inserted."
  (if View-process-display-short-key-descriptions
      (let ((local-map (current-local-map)))
	(use-local-map View-process-mode-map)
	(insert 
	 (substitute-command-keys
	  (concat 
	   "  \\[view-processes]: new output  "
	   "\\[View-process-status]: new output with new options  "
	   "     \\[revert-buffer]: update output  \n" 
	   "  \\[View-process-filter-by-current-field-g]: field filter  "
	   "\\[View-process-filter-g]: line filter  "
	   "\\[View-process-sort-by-current-field-g]: sort  "
	   "\\[View-process-reverse-g]: reverse  "
	   "\\[View-process-send-signal-to-processes-g]: send signal  "
	   "\\[View-process-quit]: quit\n")))
	(use-local-map local-map))))

(defun View-process-insert-uptime (&optional remote-host)
  "Inserts uptime information at the current point.
if `View-process-display-uptime' is nil, then nothing will be inserted.
If REMOTE-HOST is non nil, then its the name of the remote host."
  (if View-process-display-uptime
      (progn
;	(newline)
	(if remote-host
	    (call-process View-process-rsh-command
			  nil
			  t
			  nil
			  remote-host
			  View-process-uptime-command)
	  (call-process View-process-uptime-command
			nil
			t
			nil)))))

(defun View-process-insert-title-lines (command-switches 
					remote-host
					use-last-sorter-and-filter)
  "Insert the title lines in the output lines.
REMOTE-HOST is nil or the name of the host on which the 
ps command was executed. USE-LAST-SORTER-AND-FILTER determines, if
the last sorter and filter (from `View-process-actual-sorter-and-filter')
are used."
  (insert (or remote-host (system-name) "") 
	  ;;(getenv "HOST") (getenv "HOSTNAME") "")
	  ", "
	  (current-time-string)
	  ", "
	  View-process-status-command 
	  " " 
	  command-switches
	  "\n")
  (View-process-insert-uptime remote-host)
  (View-process-insert-short-key-descriptions)
  (if (or (and use-last-sorter-and-filter
	       View-process-actual-sorter-and-filter)
	  View-process-sorter-and-filter)
      (insert 
       "This output is filtered! Look at `View-process-sorter-and-filter'.\n"))
  (newline 1)
  (setq View-process-ps-header-window-size
	(+ View-process-ps-header-window-offset
	   (count-lines (point-min) (point))
	   (if (and (View-process-xemacs-p)
		    (not (View-process-lemacs-p))
		    View-process-header-mode-line-off)
	       -1
	     0))))

(defun View-process-search-header-line-1 (header-dectection-list
					  no-error-message)
  "Internal funtion of `View-process-search-header-line'."
  (cond (header-dectection-list
	 (goto-char View-process-header-start)
	 (if (search-forward (car header-dectection-list) nil t)
	     (setq View-process-header-start 
		   (View-process-return-beginning-of-line))
	   (View-process-search-header-line-1 (cdr header-dectection-list)
					      no-error-message)))
	(t (setq mode-motion-hook nil) ; otherwise emacs hangs
	   (if no-error-message
	       nil
	     (error (concat "ERROR: No header line detected! "
			    "Look at View-process-header-line-detection-list!")
		  )))))
	     

(defun View-process-search-header-line (&optional no-error-message)
  "Function searches the headerline and sets `View-process-header-start'.
The header line must have at least one of the words of the list
`View-process-header-line-detection-list'.
If NO-ERROR-MESSAGE is t and no header-line is found, then only 
nil (without an error message) will be returned."
  (save-excursion
     (View-process-search-header-line-1 View-process-header-line-detection-list
					no-error-message)
    ))

(defun View-process-save-position ()
  "Saves the current line and column in a cons cell and returns it."
  (save-restriction
    (widen)
    (if (< View-process-header-start (point-max))
      (cons (- (count-lines (or View-process-header-start (point-min))
			    (point))
	       (if (= 0 (current-column))
		   0
		 1))
	    (current-column))
      nil)))

(defun View-process-goto-position (position)
  "Sets the point to the POSITION.
POSITION is a cons cell with a linenumber and a column."
  (if position
      (save-restriction
	(widen)
	(goto-char View-process-header-start)
	(forward-line (car position))
	(move-to-column (cdr position) t)
;	(setq temporary-goal-column (cdr position)) ; doesn't work :-(
	)))

(defun View-process-status (command-switches 
			    &optional remote-host
			    use-last-sorter-and-filter)
  "Prints a list with processes in the buffer `View-process-buffer-name'.
COMMAND-SWITCHES is a string with the command switches (ie: -aux).
IF the optional argument REMOTE-HOST is given, then the command will
be executed on the REMOTE-HOST. If an prefix arg is given, then the 
function asks for the name of the remote host.
If USE-LAST-SORTER-AND-FILTER is t, then the last sorter and filter 
commands are used. Otherwise the sorter and filter from the list
'View-process-sorter-and-filter' are used."
  (interactive 
   (let ((View-process-stop-motion-help t))
     (list 
      (read-string "Command switches: "
		   (or View-process-status-last-command-switches
		       (if (bufferp (get-buffer View-process-buffer-name))
			   (cdr 
			    (assoc 
			     'View-process-status-last-command-switches
			     (buffer-local-variables 
			      (get-buffer View-process-buffer-name)))))
		       (if (string= "bsd" (View-process-bsd-or-system-v))
			   View-process-status-command-switches-bsd
			 View-process-status-command-switches-system-v))
		   'View-process-status-history)
      (if current-prefix-arg 
	  (setq View-process-remote-host 
		(read-string "Remote host name: "
			     View-process-remote-host
			     'View-process-remote-host-history))
	(setq View-process-remote-host nil)))))
  (View-process-save-old-window-configuration)
  (let ((buffer (get-buffer-create View-process-buffer-name))
	(position nil))
;	(point-after-ps nil))
    (if (window-minibuffer-p (selected-window))
	(set-buffer buffer)
      (switch-to-buffer buffer))

    ;; set switches for the next view process command
    (setq View-process-status-last-command-switches command-switches)
    (if (string= "bsd" (View-process-bsd-or-system-v))
	(setq View-process-status-command-switches-bsd command-switches)
      (setq View-process-status-command-switches-system-v command-switches))

    (setq buffer-read-only nil)
    (if (not (= (point-min) (point-max)))
	(progn
	  (setq position (View-process-save-position))
;	(setq point-after-ps (point-min))
;      (setq point-after-ps (point))
	  (erase-buffer)))
    (View-process-insert-title-lines command-switches 
				     remote-host
				     use-last-sorter-and-filter)
    (setq View-process-header-start (point))
    (if remote-host
	(call-process View-process-rsh-command
		      nil
		      t
		      t
		      remote-host
		      (concat View-process-status-command 
			      " " 
			      command-switches))
      (call-process View-process-status-command 
		    nil 
		    t 
		    t 
		    command-switches))
    (View-process-search-header-line)
    (setq View-process-output-end (point-max))
    (View-process-replace-colons-with-blanks)
    (View-process-insert-blanks-at-line-start)
    (View-process-split-merged-fields View-process-insert-blank-alist)
    (View-process-init-internal-variables use-last-sorter-and-filter)
    (View-process-highlight-header-line)
    (goto-char View-process-output-start)
    (View-process-goto-position position) 
;    (goto-char (cond ((> point-after-ps (point-max)) (point-max))
;		     ((= point-after-ps (point-min)) View-process-output-start)
;		     ((< point-after-ps View-process-output-start)
;		      View-process-output-start)
;		     (t point-after-ps)))
    (setq buffer-read-only t)
    (let ((View-process-stop-motion-help t))
;    (setq View-process-stop-motion-help t)
      (View-process-mode)
;    (setq View-process-stop-motion-help nil)
;      (View-process-redraw) ; only the first time (fixes an Emacs 19 bug)
      )
    ))

(defun View-process-status-update ()
  "Runs the `View-process-status' with the last switches
and sorter and filter commands."
  (interactive)
  (if View-process-status-last-command-switches
      (View-process-status View-process-status-last-command-switches
			   View-process-remote-host
			   t)
    (error "ERROR: No view process buffer exists for update!")))

(defun view-processes (&optional remote-host)
  "Prints a list with processes in the buffer `View-process-buffer-name'.
It calls the function `View-process-status' with default switches.
As the default switches on BSD like systems the value of the variable
`View-process-status-command-switches-bsd' is used. 
On System V like systems the value of the variable
`View-process-status-command-switches-system-v' is used.
IF the optional argument REMOTE-HOST is given, then the command will
be executed on the REMOTE-HOST. If an prefix arg is given, then the 
function asks for the name of the remote host."
  (interactive 
   (let ((View-process-stop-motion-help t))
     (list (if current-prefix-arg 
	       (setq View-process-remote-host 
		     (read-string "Remote host name: "
				  View-process-remote-host
				  'View-process-remote-host-history))
	     (setq View-process-remote-host nil)))))
  (if (string= "bsd" (nth 2 (View-process-get-system-type)))
      (View-process-status View-process-status-command-switches-bsd
			   View-process-remote-host)
    (View-process-status View-process-status-command-switches-system-v
			 remote-host)))

;;; itimer functions (to repeat the ps output)

(defun View-process-status-itimer-function ()
  "Itimer function for updating the ps output."
  (save-excursion
    (save-window-excursion
      (View-process-status-update)))
  ;;(View-process-start-itimer)
  )


;;; help functions

(defun View-process-show-pid-and-command-or-field-name ()
  "Displays the pid and the command of the current line or the field name.
If the point is at a blank, then the pid and the command of the current
line are displayed. Otherwise the name of the field and its description
are displayed."
  (interactive)
  (if (looking-at " ")
      (View-process-show-pid-and-command)
    (View-process-which-field-name)))

(defun View-process-show-pid-and-command ()
  "Displays the pid and the command of the current line.
It assumes, that the command is displayed at the end of the line."
  (interactive)
  (if (>= (point) View-process-output-start)
      (message "PID= %s, %s"
	       (View-process-get-pid-from-current-line)
	       (View-process-get-field-value-from-current-line 
		View-process-max-fields
		View-process-max-fields))))

(defun View-process-show-field-names ()
  "Displays the name(s) of the ps output field(s).
If the point is at a blank, then the header line with all field names
is displayed. Otherwise only the name of the field at the point is 
displayed."
  (interactive)
  (if (looking-at " ")
      (View-process-show-header-line)
    (View-process-which-field-name)))

(defun View-process-show-header-line ()
  "Displays the header line in the buffer at the current line."
  (interactive)
  (save-window-excursion
    (let ((header-line (save-restriction
			 (widen)
			 (concat
				 (buffer-substring View-process-header-start
						   View-process-header-end)
				 "\n"))))
      (momentary-string-display header-line
				(View-process-return-beginning-of-line)))))

(defun View-process-which-field-name ()
  "Displays the name of the field under the point in the echo area."
  (interactive)
  (if (>= (point) View-process-header-start)
      (let ((field-name (View-process-translate-field-position-to-name
			 (View-process-current-field-number))))
	(message 
	 (View-process-replace-in-string 
	  "%" 
	  "%%" 
	  (concat field-name
		  ": "
		  (View-process-get-field-name-description field-name)))))))

(defun View-process-get-field-name-description (field-name)
  "Returns a string with a desciption of the ps output field FIELD-NAME."
  (let ((description 
	 (or (car (cdr (assoc field-name
			      View-process-field-name-descriptions)))
	     (car (cdr (assoc field-name 
			      View-process-field-name-descriptions-general))))
	 ))
    (if (stringp description)
	description
      (concat (car description)
	      (View-process-get-value-description 
	       (View-process-get-field-value-from-current-line
		(View-process-translate-field-name-to-position field-name)
		View-process-max-fields)
	       (cdr description))))))

(defun View-process-get-value-description (values value-descriptions)
  "Returns a string with the description of the VALUES.
VALUE-DESCRIPTIONS is an alist with the possible values and its
descriptions."
  (cond ((string= values "") "")
	((or (eq (aref values 0) ?_) (eq (aref values 0) ? ))
	 (View-process-get-value-description (substring values 1)
					     value-descriptions))
	(t (concat
	    (car 
	     (cdr 
	      (assoc 
	       (substring values 0 (string-match "[ _]" values))
	       value-descriptions)))
	    (if (string-match "[ _]" values)
		(View-process-get-value-description
		 (substring values (string-match "[ _]" values))
		 value-descriptions)
	      "")))))


;;; sort functions

(defun View-process-current-field-number ()
  "Returns the field number of the point. 
The functions fails with an error message, if the character under
the point is a blank."
  (View-process-replaces-blanks-in-fields-if-necessary)
  (save-excursion
    (if (looking-at " ")
	(error "Point is on a blank and not in a field!")
      (if (and (eq (point) (point-max))
	       (eq (current-column) 0))
	  (error "Point is not in a field!")
	(let ((field-point (point))
	      (i 0))
	  (beginning-of-line)
	  (skip-chars-forward " ")
	  (while (>= field-point (point))
	    (setq i (1+ i))
	    (skip-chars-forward "^ ")
	    (skip-chars-forward " "))
	  i)))))
	
(defun View-process-sort-fields-in-region (field 
					   beg 
					   end 
					   &optional sort-function)
  "Sort lines in region by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
BEG and END specify region to sort.
If the optional SORT-FUNCTION is nil, then the region is at first
sorted with the function `sort-fields' and then with the function
`sort-float-fields'. Otherwise a sort function like `sort-fields'
must be specified."
  (let ((position (View-process-save-position))
;	(point (point))               ;; that's, because save-excursion
;	(column (current-column))     ;; doesn't work :-(
	(field-no (if (< field View-process-max-fields)
		      field
		    View-process-max-fields)))
    (if sort-function
	(eval (list sort-function field-no beg end))
      (sort-fields field-no beg end)
      (sort-float-fields field-no beg end))
    (View-process-goto-position position)))
;    (goto-char point)
;    (goto-char (+ point (- column (current-column))))))

(defun View-process-remove-sorter (sorter alist)
  "Removes the SORTER entry from the ALIST."
  (cond ((not alist) nil)
	((eq sorter (car (car alist))) (cdr alist))
	(t (cons (car alist) 
		 (View-process-remove-sorter sorter (cdr alist))))))

(defun View-process-sort-output-by-field (field-name
					  &optional dont-remember)
  "Sort the ps output by the field FIELD-NAME.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
  (interactive 
   (let ((View-process-stop-motion-help t))
     (list
      (completing-read "Field Name for sorting: "
		       View-process-field-names
		       nil
		       t
		       (car View-process-field-name-history)
		       View-process-field-name-history))))
  (setq buffer-read-only nil)
  (View-process-sort-fields-in-region
   (View-process-translate-field-name-to-position field-name)
   View-process-output-start
   View-process-output-end)
  (setq buffer-read-only t)
  (if (not dont-remember)
      (setq View-process-actual-sorter-and-filter
	    (append (View-process-remove-sorter
		     'reverse
		     (View-process-remove-sorter 
		      'sort
		      View-process-actual-sorter-and-filter))
		    (list (list 'sort field-name))))))

(defun View-process-sort-by-current-field-g ()
  "Sort the ps output by the field under the point.
It is a generic interface to `View-process-sort-region-by-current-field'
and `View-process-sort-output-by-current-field'.The first will be called
if a region is active and the other one if not.
With a prefix arg, it uses the NTH field instead of the current one."
  (interactive)
  (if (View-process-region-active-p)
      (call-interactively 'View-process-sort-region-by-current-field)
    (call-interactively 'View-process-sort-output-by-current-field)))

(defun View-process-sort-output-by-current-field (&optional nth dont-remember)
  "Sort the whole ps output by the field under the point.
With a prefix arg, it uses the NTH field instead of the current one.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
  (interactive "P")
  (let ((field-number (if nth
			  (if (and (>= nth 1) (<= nth View-process-max-fields))
			      nth
			    (error "ERROR: Wrong field number!"))
			(View-process-current-field-number))))
    (setq buffer-read-only nil)
    (View-process-sort-fields-in-region field-number
					View-process-output-start
					View-process-output-end)
    (setq buffer-read-only t)
    (if (not dont-remember)
	(setq View-process-actual-sorter-and-filter
	      (append (View-process-remove-sorter
		       'reverse
		       (View-process-remove-sorter 
			'sort
			View-process-actual-sorter-and-filter))
		      (list 
		       (list 'sort 
			     (View-process-translate-field-position-to-name
			      field-number))))))))

(defun View-process-sort-region-by-current-field (&optional nth)
  "Sort the region by the field under the point.
With a prefix arg, it uses the NTH field instead of the current one."
  (interactive "P")
  (let ((field-number (if nth
			  (if (and (>= nth 1) (<= nth View-process-max-fields))
			      nth
			    (error "ERROR: Wrong field number!"))
			(View-process-current-field-number))))
    (setq buffer-read-only nil)
    (View-process-sort-fields-in-region 
     field-number
     (save-excursion
       (goto-char (region-beginning))
       (View-process-return-beginning-of-line))
     (save-excursion
       (goto-char (region-end))
       (View-process-return-end-of-line)))
    (setq buffer-read-only t)))

(defun View-process-reverse-output (&optional dont-remember)
  "Reverses the whole output lines.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
  (interactive)
  (setq buffer-read-only nil)
  (let ((position (View-process-save-position)))
;	(line (count-lines (point-min) (point)))
;	(column (current-column)))
    (reverse-region View-process-output-start View-process-output-end)
    (View-process-goto-position position))
;    (goto-line line)
;    (beginning-of-line)
;    (forward-char column))
  (setq buffer-read-only t)
  (if (not dont-remember)
      (setq View-process-actual-sorter-and-filter
	    (if (assq 'reverse View-process-actual-sorter-and-filter)
		(View-process-remove-sorter 
		 'reverse
		 View-process-actual-sorter-and-filter)
	      (append View-process-actual-sorter-and-filter
		      (list (list 'reverse)))))))

(defun View-process-reverse-region ()
  "Reverses the output lines in the region."
  (interactive)
  (setq buffer-read-only nil)
  (let ((region-beginning (if (< (region-beginning) (region-end))
			      (region-beginning)
			    (region-end)))
	(region-end (if (> (region-end) (region-beginning))
			(region-end)
		      (region-beginning)))
	(position (View-process-save-position)))
;	(line (count-lines (point-min) (point)))
;	(column (current-column)))
    (reverse-region (if (< region-beginning View-process-output-start)
			View-process-output-start
		      (goto-char region-beginning)
		      (View-process-return-beginning-of-line))
		    (if (> region-end View-process-output-end)
			View-process-output-end
		      (goto-char region-end)
		      (View-process-return-end-of-line)))
    (View-process-goto-position position))
;    (goto-line line)
;    (beginning-of-line)
;    (forward-char column))
  (setq buffer-read-only t))

(defun View-process-reverse-g ()
  "Reverses the output lines.
It is a generic interface to `View-process-reverse-region'
and `View-process-reverse-output'. The first will be called
if a region is active and the other one if not."
  (interactive)
  (if (View-process-region-active-p)
      (call-interactively 'View-process-reverse-region)
    (call-interactively 'View-process-reverse-output)))

;;; filter functions

(defun View-process-delete-region (start end)
  "Stores deleted lines in `View-process-deleted-lines'."
  (setq View-process-deleted-lines
	(cons (buffer-substring start end)
	      View-process-deleted-lines))
  (delete-region start end))

(defun View-process-remove-all-filter-and-sorter ()
  "Undeletes all filtered lines from `View-process-deleted-lines'.
It removes also all filter and sorter from the list
`View-process-actual-sorter-and-filter'."
  (interactive)
  (let ((buffer-read-only))
    (goto-char View-process-output-end)
    (mapcar '(lambda (line)
	       (insert line))
	    View-process-deleted-lines)
    (setq View-process-output-end (point))
    (setq View-process-actual-sorter-and-filter nil)
    (goto-char View-process-output-start)))

(defun View-process-filter-fields-in-region (regexp 
					     field-no 
					     beg 
					     end
					     &optional exclude)
  "Filters a region with a REGEXP in the field FIELD-NO.
The region start is at BEG and the end at END. If FIELD-NO
is nil, then the whole line is used. All lines which passes
not the filter are deleted in the buffer, if EXCLUDE is nil.
Otherwise only these lines are not deleted."
  (save-restriction
    (widen)
    (let ((region-start (if (< beg end) beg end))
	  (region-end (if (> beg end) beg end)))
      (if (< region-start View-process-output-start)
	  (setq region-start View-process-output-start))
      (goto-char region-end)
      (if field-no
	  (while (>= (point) region-start)
	    (if (string-match regexp 
			      (View-process-get-field-value-from-current-line
			       field-no
			       View-process-max-fields))
		(if exclude
		    (View-process-delete-region 
		     (1- (View-process-return-beginning-of-line))
		     (View-process-return-end-of-line))
		  (forward-line -1))
	      (if exclude
		  (forward-line -1)
		(View-process-delete-region 
		 (1- (View-process-return-beginning-of-line))
		 (View-process-return-end-of-line)))
	      ))
	(beginning-of-line)
	(while (>= (point) region-start)
	  (if (search-forward-regexp regexp 
				     (View-process-return-end-of-line) t)
	      (if exclude
		  (progn
		    (View-process-delete-region 
		     (1- (View-process-return-beginning-of-line))
		     (View-process-return-end-of-line))
		    (beginning-of-line))
		(forward-line -1))
	    (if exclude
		(forward-line -1)
	      (View-process-delete-region 
	       (1- (View-process-return-beginning-of-line))
	       (View-process-return-end-of-line))
	      (beginning-of-line))
	    )))
      (goto-char region-start))
    (setq View-process-output-end (point-max))
    (if (> View-process-output-start View-process-output-end)
	(progn
	  (newline)
	  (setq View-process-output-end View-process-output-start)))))

(defun View-process-filter-output-by-field (field-name 
					    regexp 
					    &optional exclude
					    dont-remember)
  "Filter the whole output by the field FIELD-NAME with REGEXP.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
  (interactive 
   (let ((View-process-stop-motion-help t))
     (list
      (completing-read "Field Name for filtering: "
		       View-process-field-names
		       nil
		       t
		       (car View-process-field-name-history)
		       View-process-field-name-history)
      (read-string "Regexp for filtering the output in the field: "
		   (car View-process-filter-history)
		   View-process-filter-history)
      current-prefix-arg
      )))
  (setq buffer-read-only nil)
  (View-process-filter-fields-in-region 
   regexp
   (View-process-translate-field-name-to-position field-name)
   View-process-output-start
   View-process-output-end
   exclude)
  (setq buffer-read-only t)
  (if (not dont-remember)
      (setq View-process-actual-sorter-and-filter
	    (append View-process-actual-sorter-and-filter
		    (list (list (if exclude 'exclude-filter 'filter)
				field-name
				regexp))))))

(defun View-process-filter-output-by-current-field (regexp 
						    &optional exclude
						    dont-remember)
  "Filter the whole output by the field under the point with REGEXP.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
;  (interactive "sRegexp for filtering the output in the current field: \nP")
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (regexp (read-string 
		   "sRegexp for filtering the output in the current field: "))
	  (exclude current-prefix-arg))
     (list regexp exclude)))
  (let ((current-field-number (View-process-current-field-number)))
    (setq buffer-read-only nil)
    (View-process-filter-fields-in-region regexp
					  current-field-number
					  View-process-output-start
					  View-process-output-end
					  exclude)
  (setq buffer-read-only t)
  (if (not dont-remember)
      (setq View-process-actual-sorter-and-filter
	    (append View-process-actual-sorter-and-filter
		    (list 
		     (list (if exclude 'exclude-filter 'filter)
			   (View-process-translate-field-position-to-name
			    current-field-number)
			   regexp)))))))

(defun View-process-filter-region-by-current-field (regexp &optional exclude)
  "Filter the region by the field under the point with REGEXP.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil."
;  (interactive "sRegexp for filtering the region in the current field: \nP")
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (regexp (read-string 
		   "sRegexp for filtering the region in the current field: "))
	  (exclude current-prefix-arg))
     (list regexp exclude)))
  (setq buffer-read-only nil)
  (View-process-filter-fields-in-region 
   regexp
   (View-process-current-field-number)
   (save-excursion
     (goto-char (region-beginning))
     (View-process-return-beginning-of-line))
   (save-excursion
     (goto-char (region-end))
     (View-process-return-end-of-line))
   exclude)
  (setq buffer-read-only t))

(defun View-process-filter-by-current-field-g (&optional exclude)
  "Filter the whole output by the field under the point with an Regexp.
It is a generic interface to `View-process-filter-region-by-current-field'
and `View-process-filter-output-by-current-field'. The first will be called
if a region is active and the other one if not. 
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil."
  (interactive "P")
  (setq prefix-arg current-prefix-arg)
  (if (View-process-region-active-p)
      (call-interactively 'View-process-filter-region-by-current-field)
    (call-interactively 'View-process-filter-output-by-current-field)))

(defun View-process-filter-output (regexp &optional exclude dont-remember)
  "Filter the whole output with REGEXP.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil.
If DONT-REMEMBER is t, then the filter command isn't inserted 
in the `View-process-actual-sorter-and-filter' list."
;  (interactive "sRegexp for filtering the output: \nP")
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (regexp (read-string 
		   "sRegexp for filtering the output: "))
	  (exclude current-prefix-arg))
     (list regexp exclude)))
  (setq buffer-read-only nil)
  (View-process-filter-fields-in-region regexp
					nil
					View-process-output-start
					View-process-output-end
					exclude)
  (setq buffer-read-only t)
  (if (not dont-remember)
      (setq View-process-actual-sorter-and-filter
	    (append View-process-actual-sorter-and-filter
		    (list (list (if exclude 'exclude-grep 'grep)
				regexp))))))

(defun View-process-filter-region (regexp &optional exclude)
  "Filter the region with REGEXP.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil."
;  (interactive "sRegexp for filtering the region: \nP")
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (regexp (read-string 
		   "sRegexp for filtering the region: "))
	  (exclude current-prefix-arg))
     (list regexp exclude)))
  (setq buffer-read-only nil)
  (View-process-filter-fields-in-region 
   regexp
   nil
   (save-excursion
     (goto-char (region-beginning))
     (View-process-return-beginning-of-line))
   (save-excursion
     (goto-char (region-end))
     (View-process-return-end-of-line))
   exclude)
  (setq buffer-read-only t))

(defun View-process-filter-g (&optional exclude)
  "Filters the output by the field under the point with an Regexp.
It is a generic interface to `View-process-filter-region'
and `View-process-filter-output'. The first will be called
if a region is active and the other one if not.
The matching lines are deleted, if EXCLUDE is t. The non matching
lines are deleted, if EXCLUDE is nil. If you call this function
interactive, then you can give a prefix arg to set EXCLUDE to non nil."
  (interactive "P")
  (setq prefix-arg current-prefix-arg)
  (if (View-process-region-active-p)
      (call-interactively 'View-process-filter-region)
    (call-interactively 'View-process-filter-output)))


;;; call sorter, filter or grep after running ps

(defun View-process-call-sorter-and-filter (sorter-and-filter-list)
  "Call sorter, filter or grep after running ps.
The sorter, filter or grep commands and its parameters are called 
from SORTER-AND-FILTER-LIST."
  (cond ((not sorter-and-filter-list) t)
	((eq 'grep (car (car sorter-and-filter-list)))
	 (View-process-filter-output (car (cdr (car sorter-and-filter-list)))
				     nil
				     t)
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	((eq 'exclude-grep (car (car sorter-and-filter-list)))
	 (View-process-filter-output (car (cdr (car sorter-and-filter-list)))
				     t
				     t)
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	((eq 'sort (car (car sorter-and-filter-list)))
	 (if (assoc (car (cdr (car sorter-and-filter-list)))
		    View-process-field-names)
	     (View-process-sort-output-by-field
	      (car (cdr (car sorter-and-filter-list)))
	      t))
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	((eq 'filter (car (car sorter-and-filter-list)))
	 (if (assoc (car (cdr (car sorter-and-filter-list)))
		    View-process-field-names)
	     (View-process-filter-output-by-field
	      (car (cdr (car sorter-and-filter-list)))
	      (car (cdr (cdr (car sorter-and-filter-list))))
	      nil
	      t))
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	((eq 'exclude-filter (car (car sorter-and-filter-list)))
	 (if (assoc (car (cdr (car sorter-and-filter-list)))
		    View-process-field-names)
	     (View-process-filter-output-by-field
	      (car (cdr (car sorter-and-filter-list)))
	      (car (cdr (cdr (car sorter-and-filter-list))))
	      t
	      t))
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	((eq 'reverse (car (car sorter-and-filter-list)))
	 (View-process-reverse-output t)
	 (View-process-call-sorter-and-filter (cdr sorter-and-filter-list)))
	(t (error "Filter/Sorter command not implemented!"))))


;;; Child processes

(defun View-process-get-child-process-list-1 (pid pid-ppid-alist)
  "Internal function of `View-process-get-child-process-list'."
  (cond ((car pid-ppid-alist)
	 (if (not (string= pid (cdr (car pid-ppid-alist))))
	     (View-process-get-child-process-list-1 pid (cdr pid-ppid-alist))
	   (cons (car (car pid-ppid-alist))
		 (View-process-get-child-process-list-1 pid 
							(cdr pid-ppid-alist))
		 )))))

(defun View-process-get-child-process-list (pid pid-ppid-alist)
  "Returns a list with all direct childs of the processes with the PID.
The list PID-PPID-ALIST is an alist with the pid's as car's 
and ppid's as cdr's.
Example list: (\"0\" \"10\" \"20\")
With \"0\" eq PID as the parent of the direct childs \"10\" and \"20\"."
  (cons pid (View-process-get-child-process-list-1 pid pid-ppid-alist)))

(defun View-process-get-child-process-tree (pid)
  "Returns a list with all childs and subchilds of the processes with the PID.
Example list:  (\"0\" (\"10\") (\"20\" (\"30\" \"40\")))
With \"0\" eq PID as the parent of the direct childs \"10\" and \"20\" 
and with \"20\" as the parent of the direct childs \"30\" and \"40\"."
  (cons pid 
	(mapcar 'View-process-get-child-process-tree
		(cdr (View-process-get-child-process-list 
		      pid
		      (save-excursion 
			(View-process-get-pid-ppid-list-from-region 
			 View-process-output-start
			 View-process-output-end)))))))

;(defun View-process-highlight-process-tree (process-tree)
;  "Highlights all processes in the list process-tree."
;  (cond ((not process-tree))
;	((listp (car process-tree))
;	 (View-process-highlight-process-tree (car process-tree))
;	 (View-process-highlight-process-tree (cdr process-tree)))
;	((stringp (car process-tree))
;	 (View-process-highlight-line-with-pid (car process-tree)
;					       'View-process-child-line-face
;					       View-process-child-line-mark)
;	 (View-process-highlight-process-tree (cdr process-tree)))
;	(t (error "Bug in 'View-process-highlight-process-tree' !"))))

;(defun View-process-highlight-recursive-all-childs (pid)
;  "Highlights all childs of the process with the PID."
;  (interactive "sParent PID: ")
;  (if (not
;       (View-process-field-name-exists-p View-process-ppid-field-name))
;      (error "ERROR: No field `%s' in the output. Try `M-x ps -j' to get it."
;	     View-process-ppid-field-name)
;    (View-process-highlight-line-with-pid pid 
;					  'View-process-parent-line-face
;					  View-process-parent-line-mark)
;    (View-process-highlight-process-tree
;     (cdr (View-process-get-child-process-tree pid)))))

;(defun View-process-highlight-recursive-all-childs-in-line ()
;  "Highlights all the child processes of the process in the current line."
;  (interactive)
;  (View-process-highlight-recursive-all-childs
;   (View-process-get-pid-from-current-line)))

;;; kill processes

(defun View-process-send-signal-to-processes-with-mark (signal)
  "Sends a SIGNAL to all processes, which are marked."
  (interactive
   (let* ((View-process-stop-motion-help t)
	  (signal (completing-read "Signal: "
				   View-process-kill-signals
				   nil
				   t
				   View-process-default-kill-signal
				   View-process-signal-history)))
     (list signal)))
  (if View-process-pid-mark-alist
      (View-process-call-function-on-pid-and-mark-list
       'View-process-send-signal-to-process-in-line
       View-process-pid-mark-alist
       t
       signal)
    (error "ERROR: There is no marked process!.")))

(defun View-process-send-signal-to-processes-in-region (signal)
  "Sends a SIGNAL to all processes in the current region."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (signal (completing-read "Signal: "
				   View-process-kill-signals
				   nil
				   t
				   View-process-default-kill-signal
				   View-process-signal-history)))
     (list signal)))
  (let ((region-start (if (> (region-beginning) View-process-output-start)
			  (region-beginning)
			View-process-output-start))
	(region-end (if (< (region-end) View-process-output-end)
			(region-end)
		      View-process-output-end)))
    (save-excursion
      (goto-char region-start)
      (beginning-of-line)
      (let ((pid-list (View-process-get-pid-list-from-region (point) 
							     region-end)))
	(View-process-send-signal-to-processes-in-pid-list signal 
							   pid-list
							   nil
							   t)
	))))

(defun View-process-send-signal-to-processes-in-pid-list (signal 
							  pid-list
							  &optional 
							  dont-ask
							  dont-update)
  "Sends a SIGNAL to all processes with a pid in PID-LIST.
If DONT-ASK is non nil, then no confirmation question will be asked.
If DONT-UPDATE is non nil, then the command `View-process-status-update'
will not be run after sending a signal."
  (if (not pid-list)
      t
    (View-process-send-signal-to-process signal 
					 (car pid-list)
					 dont-ask
					 dont-update)
    (View-process-send-signal-to-processes-in-pid-list signal
						       (cdr pid-list)
						       dont-ask
						       dont-update)))

(defun View-process-send-signal-to-process-in-line (signal)
  "Sends a SIGNAL to the process in the current line."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (signal (completing-read "Signal: "
				   View-process-kill-signals
				   nil
				   t
				   View-process-default-kill-signal
				   View-process-signal-history)))
     (list signal)))
  (if (and (>= (point) View-process-output-start)
	   (< (point) View-process-output-end))
      (View-process-send-signal-to-process 
       signal
       (View-process-get-pid-from-current-line)
       nil
       t)))

(defun View-process-send-key-as-signal-to-processes ()
  "Converts the key which invokes this command to a signal.
After that it sends this signal to the process in the current line,
or, if an active region exists, to all processes in the region.
For this function only  numbers could be used as keys."
  (interactive)
  (let ((signal (View-process-return-current-command-key-as-string)))
    (if (not (= 0 (string-to-int signal)))
	(if (View-process-region-active-p)
	    (View-process-send-signal-to-processes-in-region signal)
	  (View-process-send-signal-to-process-in-line signal))
      (error "ERROR: This command must be bind to and call by an integer!")
      )))

(defun View-process-send-signal-to-processes-g ()
  "Sends a signal to processes.
It is a generic interface to `View-process-send-signal-to-processes-in-region'
and `View-process-send-signal-to-process-in-line'. The first will be called
if a region is active and the other one if not. If the region isn't
active, but marks are set, then the function is called on every 
marked process."
  (interactive)
  (cond ((View-process-region-active-p)
	 (call-interactively 'View-process-send-signal-to-processes-in-region))
	(View-process-pid-mark-alist
	 (call-interactively 'View-process-send-signal-to-processes-with-mark))
	(t
	 (call-interactively 'View-process-send-signal-to-process-in-line))))

(defun View-process-send-signal-to-process (signal
					    pid
					    &optional 
					    dont-ask
					    dont-update)
  "Sends the SIGNAL to the process with the PID.
If DONT-ASK is non nil, then no confirmation question will be asked.
If DONT-UPDATE is non nil, then the command `View-process-status-update'
will not be run after sending the signal."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (signal (completing-read "Signal: "
				   View-process-kill-signals
				   nil
				   t
				   View-process-default-kill-signal
				   View-process-signal-history))
	  (pid (int-to-string (read-number "Process Id (PID): "))))
     (list signal pid)))
  (if (and (eq (string-to-int pid) (emacs-pid))
	   (or (not View-process-remote-host)
	       (string= View-process-remote-host (getenv "HOSTNAME"))))
      (error "Hey, are you a murderer? You've just tried to kill me!")
    (let (
;	  (signal-line-extent
;	   (View-process-highlight-line-with-pid 
;	    pid
;	    'View-process-signal-line-face
;	    View-process-signal-line-mark))
	  (signal-number (car (cdr (assoc signal View-process-kill-signals)))))
      (View-process-mark-line-with-pid pid View-process-signal-line-mark)
      (if (or dont-ask
	      (if (string= signal-number signal)
		  (y-or-n-p (format 
			     "Do you realy want to send signal %s to PID %s "
			     signal
			     pid))
		(y-or-n-p 
		 (format "Do you realy want to send signal %s (%s) to PID %s "
			 signal
			 signal-number
			 pid))))
	  (progn
	    (if View-process-remote-host
		(call-process View-process-rsh-command
			      nil
			      nil
			      nil
			      View-process-remote-host
			      (concat View-process-signal-command
				      " -"
				      signal-number
				      " "
				      pid))
	      (call-process View-process-signal-command
			    nil
			    nil
			    nil
			    (concat "-" signal-number)
			    pid))
	    (if (not dont-update)
		(View-process-status-update)
	      (View-process-mark-line-with-pid pid 
					       View-process-signaled-line-mark)
	      ))
;	(View-process-delete-extent signal-line-extent)
	(if (View-process-goto-line-with-pid pid)
	    (View-process-unmark-current-line))
	))))


;;; renice processes

(defun View-process-read-nice-value ()
  "Reads and returns a valid nice value."
  (let ((nice-value nil)
	(min-value (if (string= (user-real-login-name) "root") -20 1))
	(prompt "Add nice value [%d ... 20]: "))
    (while (not nice-value)
      (setq nice-value (read-string (format prompt min-value)
				    View-process-default-nice-value))
      (if (and (string= (int-to-string (string-to-int nice-value)) 
			nice-value)
	       (>= (string-to-int nice-value) min-value)
	       (<= (string-to-int nice-value) 20)
	       (not (= (string-to-int nice-value) 0)))
	  (if (> (string-to-int nice-value) 0)
	      (setq nice-value 
		    (concat "+" (int-to-string (string-to-int nice-value)))))
	(setq nice-value nil)
	(setq prompt 
	      "Wrong Format! Try again. Add nice value [%d ... 20]: ")))
    nice-value))

(defun View-process-renice-process (nice-value
				    pid
				    &optional 
				    dont-ask
				    dont-update)
  "Alter priority of the process with the PID.
NICE-VALUE is the value, which will be added to the old nice value.
If DONT-ASK is non nil, then no confirmation question will be asked.
If DONT-UPDATE is non nil, then the command `View-process-status-update'
will not be run after renicing."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (nice-value (View-process-read-nice-value))
	  (pid (int-to-string (read-number "Process Id (PID): "))))
     (list nice-value pid)))
;  (let ((signal-line-extent
;	 (View-process-highlight-line-with-pid 
;	  pid
;	  'View-process-signal-line-face
;	  View-process-renice-line-mark)))
  (View-process-mark-line-with-pid pid View-process-renice-line-mark)
  (if (or dont-ask
	  (y-or-n-p (format 
		     "Do you realy want to renice PID %s with %s "
		     pid
		     nice-value)))
      (progn
	(if View-process-remote-host
	    (call-process View-process-rsh-command
			  nil
			  nil
			  nil
			  View-process-remote-host
			  (concat View-process-renice-command
				  " "
				  nice-value
				  " "
				  pid))
	  (call-process View-process-renice-command
			nil
			nil
			nil
			nice-value
			pid))
	(if (not dont-update)
	    (View-process-status-update)
	  (View-process-mark-line-with-pid pid View-process-signaled-line-mark)
	  ))
;    (View-process-delete-extent signal-line-extent)
    (if (View-process-goto-line-with-pid pid)
	(View-process-unmark-current-line))))

(defun View-process-renice-processes-with-mark (nice-value)
  "Alter priority of  all processes, which are marked.
NICE-VALUE is the value, which will be added to the old nice value."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (nice-value (View-process-read-nice-value)))
     (list nice-value)))
  (if View-process-pid-mark-alist
      (View-process-call-function-on-pid-and-mark-list
       'View-process-renice-process-in-line
       View-process-pid-mark-alist
       t
       nice-value)
    (error "ERROR: There is no marked process!.")))  

(defun View-process-renice-processes-in-region (nice-value)
  "Alter priority of  all processes in the current region.
NICE-VALUE is the value, which will be added to the old nice value."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (nice-value (View-process-read-nice-value)))
     (list nice-value)))
  (let ((region-start (if (> (region-beginning) View-process-output-start)
			  (region-beginning)
			View-process-output-start))
	(region-end (if (< (region-end) View-process-output-end)
			(region-end)
		      View-process-output-end)))
    (save-excursion
      (goto-char region-start)
      (beginning-of-line)
      (let ((pid-list (View-process-get-pid-list-from-region (point) 
							     region-end)))
	(View-process-renice-processes-in-pid-list nice-value pid-list nil t)
	))))

(defun View-process-renice-processes-in-pid-list (nice-value
						  pid-list
						  &optional 
						  dont-ask
						  dont-update)
  "Alter priority all processes with a pid in PID-LIST.
NICE-VALUE is the value, which will be added to the old nice value.
If DONT-ASK is non nil, then no confirmation question will be asked.
If DONT-UPDATE is non nil, then the command `View-process-status-update'
will not be run after renicing"
  (if (not pid-list)
      t
    (View-process-renice-process nice-value 
				 (car pid-list)
				 dont-ask
				 dont-update)
    (View-process-renice-processes-in-pid-list nice-value
					       (cdr pid-list)
					       dont-ask
					       dont-update)))

(defun View-process-renice-process-in-line (nice-value)
  "Alter priority of to the process in the current line.
NICE-VALUE is the value, which will be added to the old nice value."
  (interactive 
   (let* ((View-process-stop-motion-help t)
	  (nice-value (View-process-read-nice-value)))
     (list nice-value)))
  (if (and (>= (point) View-process-output-start)
	   (< (point) View-process-output-end))
      (View-process-renice-process 
       nice-value
       (View-process-get-pid-from-current-line)
       nil
       t)))

(defun View-process-renice-processes-g ()
  "Alter priority of processes.
It is a generic interface to `View-process-renice-processes-in-region'
and `View-process-renice-process-in-line'. The first will be called
if a region is active and the other one if not. If the region isn't
active, but marks are set, then the function is called on every 
marked process."
  (interactive)
  (cond ((View-process-region-active-p)
	 (call-interactively 'View-process-renice-processes-in-region))
	(View-process-pid-mark-alist
	 (call-interactively 'View-process-renice-processes-with-mark))
	(t
	 (call-interactively 'View-process-renice-process-in-line))))


;;; Returning field values

(defun View-process-get-pid-from-current-line ()
  "Returns a string with the pid of the process in the current line."
  (View-process-get-field-value-from-current-line
   (View-process-translate-field-name-to-position View-process-pid-field-name)
   View-process-max-fields)
  )

(defun View-process-get-ppid-from-current-line ()
  "Returns a string with the ppid of the process in the current line."
  (View-process-get-field-value-from-current-line
   (View-process-translate-field-name-to-position View-process-ppid-field-name)
   View-process-max-fields)
  )

(defun View-process-get-pid-list-from-region (begin end)
  "Returns a list with all PID's in the region from BEGIN to END."
  (goto-char begin)
  (if (>= (point) end)
      nil
    (cons (View-process-get-pid-from-current-line)
	  (progn (forward-line)
		 (View-process-get-pid-list-from-region (point) end)))))

(defun View-process-get-pid-ppid-list-from-region (begin end)
  "Returns a list with all PID's ant its PPID's in the region 
from BEGIN to END. END must be greater than BEGIN."
  (goto-char begin)
  (if (>= (point) end)
      nil
    (cons (cons (View-process-get-pid-from-current-line)
		(View-process-get-ppid-from-current-line))
	  (progn (forward-line)
		 (View-process-get-pid-ppid-list-from-region (point) end)))))

(defun View-process-get-field-value-from-current-line (field-no max-fields)
  "Returns the value of the field FIELD-NO from the current line as string.
If the FIELD-NO is >= max-fields, then the rest of the line after the
start of the field FIELD-NO will be returned."
  (save-excursion
    (View-process-jump-to-field field-no max-fields)
    (if (>= field-no max-fields)
	(buffer-substring (point) (View-process-return-end-of-line))
      (current-word)))
  )

(defun View-process-jump-to-field (field-no max-fields)
  "Sets the point at the start of field FIELD-NO in the current line.
MAX_FIELDS is used instead of FIELD-NO, if FIELD-NO > MAX_FIELDS."
  (View-process-replaces-blanks-in-fields-if-necessary)  
  (beginning-of-line)
  (skip-chars-forward " ")
  (if (< field-no 1)
      (error "Parameter FIELD-NO must be >= 1"))
  (if (> field-no max-fields)
      (setq field-no max-fields))
  (if (= field-no 1)
      (point)
    (skip-chars-forward "^ ")
    (skip-chars-forward " ")
    (View-process-jump-to-field-1  (1- field-no))))

(defun View-process-jump-to-field-1 (field-no)
  "Internal function of View-process-jump-to-field"
  (if (= field-no 1)
      (point)
    (skip-chars-forward "^ ")
    (skip-chars-forward " ")
    (View-process-jump-to-field-1  (1- field-no))))  


(defun View-process-display-emacs-pid ()
  "Sets the point to the line with the emacs process."
  (interactive)
  (message (format "This emacs has the PID `%d'!" (emacs-pid))))


;;; mouse functions

(defun View-process-mouse-kill (event)
  "Function for kill a process with the mouse."
  (interactive "e")
  (mouse-set-point event)
  (View-process-send-signal-to-process-in-line "SIGTERM"))


;;; Highlighting functions

(defun View-process-highlight-current-line (face)
  "Highlights the current line with the FACE."
  (let ((read-only buffer-read-only))
    (setq buffer-read-only nil)
    (let ((extent (make-extent (View-process-return-beginning-of-line)
			       (View-process-return-end-of-line))))
      (set-extent-face extent face)
      (setq buffer-read-only read-only)
      extent)
    ))

(defun View-process-goto-line-with-pid (pid)
  "Sets the point in the line with the PID.
It returns nil, if there is no line with the PID in the output."
  (if (string= pid (View-process-get-pid-from-current-line))
      t
    (goto-char View-process-output-start)
    (while (and (< (point) View-process-output-end)
		(not (string= pid (View-process-get-pid-from-current-line))))
      (forward-line))
    (< (point) View-process-output-end)))

;(defun View-process-highlight-line-with-pid (pid face mark)
;  "Highlights the line with the PID with the FACE and sets the MARK.
;It returns the extent of the line."
;  (save-excursion
;    (View-process-goto-line-with-pid pid)
;    (View-process-set-mark-in-current-line mark)
;    (View-process-save-pid-and-mark pid mark)
;    (View-process-highlight-current-line face)
;    ))

;(defun View-process-delete-extent (extent)
;  "Deletes the extent EXTENT."
;  (let ((read-only buffer-read-only))
;    (save-excursion
;      (goto-char (extent-start-position extent))
;      (View-process-set-mark-in-current-line View-process-no-mark)
;      (setq buffer-read-only nil)
;      (delete-extent extent)
;      (setq buffer-read-only read-only))))

;;; mark functions

(defun View-process-save-pid-and-mark (pid mark)
  "Saves the PID and the MARK in a special alist.
The name of the alist is `View-process-pid-mark-alist'."
  (if (assoc pid View-process-pid-mark-alist)
      (setcdr (assoc pid View-process-pid-mark-alist) (list mark ))
    (setq View-process-pid-mark-alist
	  (cons (list pid mark) View-process-pid-mark-alist))))

(defun View-process-remove-pid-and-mark-1 (pid pid-mark-alist)
  "Internal function of `View-process-remove-pid-and-mark'."
  (cond ((not pid-mark-alist) 
	 nil)
	((string= pid (car (car pid-mark-alist)))
	 (View-process-remove-pid-and-mark-1 pid (cdr pid-mark-alist)))
	(t
	 (cons (car pid-mark-alist)
	       (View-process-remove-pid-and-mark-1 pid (cdr pid-mark-alist)))
	 )))

(defun View-process-remove-pid-and-mark (pid)
  "Removes the PID from the alist `View-process-pid-mark-alist'."
  (setq View-process-pid-mark-alist 
	(View-process-remove-pid-and-mark-1 pid View-process-pid-mark-alist))
  )
	
(defun View-process-set-mark-in-current-line (mark)
  "Sets the MARK at the start of the current line."
  (let ((buffer-read-only nil))
    (save-excursion
      (beginning-of-line)
      (delete-char 1)
      (insert mark))))

(defun View-process-mark-line-with-pid (pid &optional mark)
  "Sets the MARK in the line with the PID.
It uses the 'View-process-single-line-mark', if mark is nil."
;  (interactive "sPID: ")
  (interactive (let ((View-process-stop-motion-help t))
		 (list (read-string "PID: "))))
  (save-excursion
    (View-process-goto-line-with-pid pid)
    (View-process-set-mark-in-current-line (or mark
					       View-process-single-line-mark))
    (View-process-save-pid-and-mark pid
				    (or mark
					View-process-single-line-mark))
    ))

(defun View-process-mark-current-line (&optional mark)
  "Sets a mark in the current line.
It uses the 'View-process-single-line-mark' if MARK is nil."
  (interactive)
  (if (or (< (point) View-process-output-start)
	  (> (point) View-process-output-end))
      (error "ERROR: Not in a process line!")
    (View-process-set-mark-in-current-line (or mark
					       View-process-single-line-mark))
    (View-process-save-pid-and-mark (View-process-get-pid-from-current-line)
				    (or mark
					View-process-single-line-mark))))


(defun View-process-unmark-current-line ()
  "Unsets a mark in the current line."
  (interactive)
  (if (and (>= (point) View-process-output-start)
	   (<= (point) View-process-output-end))
      (progn
	(View-process-remove-pid-and-mark
	 (View-process-get-pid-from-current-line))
	(View-process-set-mark-in-current-line View-process-no-mark)
	)
    (error "ERROR: Not in a process line!")))

(defun View-process-mark-process-tree (process-tree)
  "Marks all processes in the list process-tree."
  (cond ((not process-tree))
	((listp (car process-tree))
	 (View-process-mark-process-tree (car process-tree))
	 (View-process-mark-process-tree (cdr process-tree)))
	((stringp (car process-tree))
	 (View-process-mark-line-with-pid (car process-tree)
					  View-process-child-line-mark)
	 (View-process-mark-process-tree (cdr process-tree)))
	(t (error "Bug in 'View-process-mark-process-tree' !"))))

(defun View-process-mark-childs (pid)
  "Marks all childs of the process with the PID."
;  (interactive "sParent PID: ")
  (interactive (let ((View-process-stop-motion-help t))
		 (list (read-string "Parent PID: "))))
  (if (not
       (View-process-field-name-exists-p View-process-ppid-field-name))
      (error "ERROR: No field `%s' in the output. Try `M-x ps -j' to get it."
	     View-process-ppid-field-name)
    (View-process-mark-line-with-pid pid View-process-parent-line-mark)
    (View-process-mark-process-tree
     (cdr (View-process-get-child-process-tree pid)))))

(defun View-process-mark-childs-in-current-line ()
  "Marks all the child processes of the process in the current line."
  (interactive)
  (View-process-mark-childs
   (View-process-get-pid-from-current-line)))

(defun View-process-call-function-on-pid-and-mark-list (function
							pid-mark-alist
							&optional 
							not-interactive
							&rest
							non-interactive-args)
  "Calls the FUNCTION on every process in the PID-MARK-ALIST.
FUNCTION must be an interactive function, which works on the 
process in the current line, if INTERACTIVE is nil.
If INTERACTIVE is t, then the function will be called non interactive
with the NON-INTERACTIVE-ARGS."
  (cond ((not pid-mark-alist))
	((View-process-goto-line-with-pid (car (car pid-mark-alist)))
	 (if not-interactive
	     (eval (cons function non-interactive-args))
	   (call-interactively function))
	 (eval (append (list 'View-process-call-function-on-pid-and-mark-list 
			     'function
			     '(cdr pid-mark-alist)
			     'not-interactive)
		       non-interactive-args)))
	(t
	 (eval (append (list 'View-process-call-function-on-pid-and-mark-list 
			     'function
			     '(cdr pid-mark-alist)
			     'not-interactive)
		       non-interactive-args)))
	 ))

(defun View-process-set-marks-from-pid-mark-alist (pid-mark-alist)
  "Sets the marks of the PID-MARK-ALIST to the pids of the PID-MARK-ALIST."
  (cond ((not pid-mark-alist))
	((View-process-goto-line-with-pid (car (car pid-mark-alist)))
	 (View-process-mark-current-line (car (cdr (car pid-mark-alist))))
	 (View-process-set-marks-from-pid-mark-alist (cdr pid-mark-alist)))
	(t
	 (View-process-set-marks-from-pid-mark-alist (cdr pid-mark-alist)))))

(defun View-process-reset-last-marks ()
  "Resets the last marks."
  (interactive)
  (View-process-set-marks-from-pid-mark-alist View-process-last-pid-mark-alist)
  )

(defun View-process-unmark-all ()
  "Unmarks all processes."
  (interactive)
  (View-process-call-function-on-pid-and-mark-list 
   'View-process-unmark-current-line
   View-process-pid-mark-alist
   t))


;;; commands to moving around in a ps buffer

(defun View-process-output-start ()
  "Set point to the first field after the output start."
  (interactive)
  (goto-char View-process-output-start)
  (skip-chars-forward " "))

(defun View-process-output-end ()
  "Set point to the first field before the output end."
  (interactive)
  (goto-char View-process-output-end)
  (skip-chars-backward " ")
  (skip-chars-backward "^ "))

(defun View-process-next-field ()
  "Moves forward one field."
  (interactive)
  (if (< (point) View-process-output-start)
      (View-process-output-start)
    (skip-chars-forward " ")
    (if (< (point) View-process-output-end)
	(if (= View-process-max-fields (View-process-current-field-number))
	    (progn
	      (forward-line)
	      (skip-chars-forward " ")
	      (if (>= (point) View-process-output-end)
		  (progn
		    (goto-char View-process-output-start)
		    (skip-chars-forward " "))))
	  (skip-chars-forward "^ ")
	  (skip-chars-forward " ")
	  )
      (goto-char View-process-output-start)
      (skip-chars-forward " "))))
      
(defun View-process-previous-field ()
  "Moves backward one field."
  (interactive)
  (skip-chars-backward " ")
  (backward-char)
  (if (> (point) View-process-output-start)
      (if (= View-process-max-fields (View-process-current-field-number))
	  (View-process-jump-to-field View-process-max-fields
				      View-process-max-fields)
	(skip-chars-backward "^ \n")
	(if (< (point) View-process-output-start)
	    (progn
	      (goto-char View-process-output-end)
	      (forward-line -1)
	      (View-process-jump-to-field View-process-max-fields
					  View-process-max-fields))))
    (goto-char View-process-output-end)
    (forward-line -1)
    (View-process-jump-to-field View-process-max-fields
				View-process-max-fields)))

(defun View-process-goto-first-field-next-line ()
  "Set point to the first field in the next line."
  (interactive)
  (if (< (point) View-process-output-start)
      (View-process-output-start)
    (forward-line)
    (if (>= (point) View-process-output-end)
	(View-process-output-start)
      (View-process-jump-to-field 1 View-process-max-fields))))


;;; buffer renaming

(defun View-process-rename-current-output-buffer (new-buffer-name)
  "Renames the ps output buffer to NEW-BUFFER-NAME."
  (interactive
   (let ((View-process-stop-motion-help t))
     (list 
      (read-string "New PS output buffer name: "
		   (generate-new-buffer-name
		    (concat "*ps-" 
			    (or View-process-remote-host
				(getenv "HOSTNAME"))
			    "*"))))))
  (if (not (string= mode-name View-process-mode-name))
      (error "ERROR: Not in a View-process-mode buffer!")
    (if (get-buffer new-buffer-name)
	(error "ERROR: Buffer %s exists!" new-buffer-name)
	(rename-buffer new-buffer-name)
	(setq View-process-buffer-name new-buffer-name)
	(if (or View-process-display-with-2-windows
		(get-buffer View-process-header-buffer-name))
	    (let ((new-header-buffer-name 
		   (generate-new-buffer-name 
		    (concat (substring new-buffer-name 0 -1)
			    " header*")))
		  (buffer (current-buffer)))
	      (set-buffer View-process-header-buffer-name)
	      (rename-buffer new-header-buffer-name)
	      (set-buffer buffer)
	      (setq View-process-header-buffer-name new-header-buffer-name))
	  ))))

;;; For newer versions of field.el
(if (not (fboundp 'sort-float-fields))
    (defalias 'sort-float-fields 'sort-numeric-fields))


;;; Display Functions

(defun View-process-header-mode ()
  "The mode of the buffer with the view process header."
  (set-syntax-table View-process-mode-syntax-table)
  (setq major-mode 'View-process-header-mode
	mode-name View-process-header-mode-name)
  (setq truncate-lines View-process-truncate-lines)
;  (setq buffer-modeline (not View-process-header-mode-line-off))
  (view-process-switch-buffer-modeline (not View-process-header-mode-line-off))
  (run-hooks 'View-process-header-mode-hook)
  )

(defun View-process-top-window-p (&optional window)
  "Returns t, if the WINDOW is the top one.
If WINDOW is nil, then the current window is tested."
  (eq 0 (car (cdr (window-pixel-edges window)))))

(defun View-process-change-display-type (display-with-2-windows)
  "If DISPLAY-WITH-2-WINDOWS is non nil, then a 2 windows display is used."
  (if display-with-2-windows
      (let ((window-size View-process-ps-header-window-size))
	(cond ((eq (count-windows 'NO-MINI) 1)
	       ;; split window
	       (split-window nil window-size)
	       (select-window (next-window nil 'no-minibuf))
	       )
	      ((= (count-windows 'NO-MINI) 2)
	       (if (View-process-top-window-p)
		   (progn
		     ;; delete other windows
		     (delete-other-windows)
		     ;; split window
		     (split-window nil window-size))
		 (select-window (next-window nil 'no-minibuf))
;		 (shrink-window (- (window-height) window-size))
		 )
	       (select-window (next-window nil 'no-minibuf))
	       )
	      ((> (count-windows 'NO-MINI) 2)
	       ;; delete other windows
	       (delete-other-windows)
	       ;; split window
	       (split-window nil window-size)
	       (select-window (next-window nil 'no-minibuf))
	       ))
	;; copy header lines
	(let ((header-lines (buffer-substring (point-min)
					      View-process-header-end))
	      (buffer (get-buffer-create View-process-header-buffer-name)))
	    (select-window (next-window nil 'no-minibuf))
	    ;; load *ps-header* buffer in window
	    (set-window-buffer (get-buffer-window (current-buffer)) buffer)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    ;; insert header lines
	    (insert header-lines)
	    (setq buffer-read-only t)
	    (goto-char (point-min))
	    (View-process-header-mode)
	    (if (not (= (window-height) window-size))
		(shrink-window (- (window-height) window-size)))
	    (select-window (next-window nil 'no-minibuf))
	))
    (let ((header-buffer (get-buffer View-process-header-buffer-name)))
      (if header-buffer
	  (progn
	    (if (get-buffer-window header-buffer)
		(delete-window (get-buffer-window header-buffer)))
	    (kill-buffer header-buffer))))))

(defun View-process-toggle-display-with-2-windows (&optional arg)
  "Change whether the view process output is displayed with two windows.
With ARG, set `View-process-display-with-2-windows' to t, if ARG is 
positive. ARG is a prefix arg."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (setq View-process-display-with-2-windows t)
	(setq View-process-display-with-2-windows nil))
    (if View-process-display-with-2-windows
	(setq View-process-display-with-2-windows nil)
      (setq View-process-display-with-2-windows t)))
  (View-process-change-display-type View-process-display-with-2-windows)
  (if View-process-display-with-2-windows
      (View-process-toggle-hide-header '(1))
    (View-process-toggle-hide-header '(-1))))

(defun View-process-save-old-window-configuration ()
  "Saves the window configuration before the first call of view process."
  (if (not View-process-old-window-configuration)
      (setq View-process-old-window-configuration 
	    (current-window-configuration))
    ))

(defun View-process-hide-header (hide-header)
  "Hides the header lines in the view processes buffer, if HIDE-HEADER is t."
  (if hide-header
      (if (<= View-process-output-start (point-max))
	  (narrow-to-region View-process-output-start (point-max))
	(narrow-to-region (point-max) (point-max)))
    (widen)))

(defun View-process-toggle-hide-header (&optional arg)
  "Change whether the header are hided.
With ARG, set `View-process-hide-header' to t, if ARG is positive.
ARG is a prefix arg."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (setq View-process-hide-header t)
	(setq View-process-hide-header nil))
    (if View-process-hide-header
	(setq View-process-hide-header nil)
      (setq View-process-hide-header t)))
  (View-process-hide-header View-process-hide-header))

;;; Misc. commands

(defun View-process-quit ()
  "Kills the *ps* buffer."
  (interactive)
  (if (y-or-n-p 
       "Do you want really want to quit the view process mode? ") 
      (progn
	(if (get-buffer View-process-buffer-name)
	    (kill-buffer View-process-buffer-name))
	(if (or View-process-display-with-2-windows
		(get-buffer View-process-header-buffer-name))
	    (kill-buffer View-process-header-buffer-name))
	(set-window-configuration View-process-old-window-configuration)
	(setq View-process-old-window-configuration nil)
	)))

(defun View-process-submit-bug-report ()
  "Submit via mail a bug report on View-process-mode."
  (interactive)
  (require 'reporter)
  (let ((bsd-or-system-v (View-process-bsd-or-system-v)))
    (reporter-submit-bug-report
     View-process-package-maintainer
     (concat View-process-package-name " " View-process-package-version)
     (list 'emacs-version
	   'major-mode
	   'View-process-buffer-name
	   'View-process-header-buffer-name
	   'View-process-sorter-and-filter
	   'View-process-actual-sorter-and-filter
	   'View-process-display-with-2-windows
	   'View-process-hide-header
	   'View-process-truncate-lines
	   'View-process-motion-help
	   'View-process-old-window-configuration
	   'View-process-field-names
	   'View-process-max-fields
	   'View-process-output-start
	   'View-process-output-end
	   'View-process-header-start
	   'View-process-header-end
	   'View-process-host-names-and-system-types
	   'View-process-remote-host
	   'View-process-system-type
	   'bsd-or-system-v
	   'View-process-rsh-command
	   'View-process-signal-command
	   'View-process-status-command-switches-bsd
	   'View-process-status-command-switches-system-v
	   'View-process-status-last-command-switches
	   'View-process-status-command
	   'View-process-test-command
	   'View-process-test-switches
	   'View-process-uname-command
	   'View-process-uname-switches
	   )
     nil
     nil
     (concat
      "If it is possible, you should send this bug report from the buffer\n"
      "with the view process mode. Please answer the following questions.\n"
      "Which is the name of your system? \n"
      "Is your system a BSD Unix? \n"
      "Is your system a System V Unix? \n"
      "Describe your bug: "
      ))))

(defun View-process-display-version ()
  "Displays the current version of the mode."
  (interactive)
  (message "View Process Mode, %s, Author: Heiko Münkel."
	   View-process-package-version))

(defun View-process-toggle-truncate-lines (&optional arg)
  "Change whether the lines in this buffer are truncated.
With ARG, set `truncate-lines' to t, if ARG is positive.
ARG is a prefix arg.
It saves also the state of `truncate-lines' for the next
view process command in `View-process-truncate-lines'.
It truncates also the lines in the view process header buffer,
if it is run in a view process mode buffer."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (setq truncate-lines t)
	(setq truncate-lines nil))
    (if truncate-lines
	(setq truncate-lines nil)
      (setq truncate-lines t)))
  (setq View-process-truncate-lines truncate-lines)
  (setq-default View-process-truncate-lines truncate-lines)
  (if (and (eq major-mode 'View-process-mode)
	   (or View-process-display-with-2-windows
	       (get-buffer View-process-header-buffer-name)))
      (let ((buffer (current-buffer))
	    (truncate truncate-lines))
	(set-buffer View-process-header-buffer-name)
	(setq truncate-lines truncate)
	(set-buffer buffer))))

(defun View-process-return-beginning-of-line ()
  "Returns the beginning of the current line.
The point isn't changed."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun View-process-return-end-of-line  ()
  "Returns the end of the current line.
The point isn't changed."
  (save-excursion
    (end-of-line)
    (point)))

(defun View-process-assoc-2th (key list)
  "Return non-nil if KEY is `equal' to the 2th of an element of LIST.
The value is actually the element of LIST whose 2th is KEY."
  (cond ((not list) nil)
	((equal (car (cdr (car list))) key) (car list))
	(t (View-process-assoc-2th key (cdr list)))))
 

(defun View-process-replace-in-string  (from-string 
			       to-string 
			       in-string 
			       &optional start) 
  "Replace FROM-STRING with TO-STRING in IN-STRING.
The optional argument START set the start position > 0.
FROM-STRING is a regular expression."
  (setq start (or start 0))
  (let ((start-of-from-string (string-match from-string in-string start)))
    (if start-of-from-string
	(concat (substring in-string start start-of-from-string)
		to-string
		(View-process-replace-in-string from-string 
						to-string 
						in-string
						(match-end 0)))
      (substring in-string start))))


(defun View-process-toggle-digit-bindings (&optional arg)
  "Change whether the digit keys sends signals to the processes.
 With ARG, set `View-process-digit-bindings-send-signal' to t, 
if ARG is positive. ARG is a prefix arg."  
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (setq View-process-digit-bindings-send-signal t)
	(setq View-process-digit-bindings-send-signal nil))
    (if View-process-digit-bindings-send-signal
	(setq View-process-digit-bindings-send-signal nil)
      (setq View-process-digit-bindings-send-signal t)))
  (if View-process-digit-bindings-send-signal
      (progn
	(define-key View-process-mode-map "0"
	  'undefined)
	(define-key View-process-mode-map "1"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "2"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "3"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "4"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "5"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "6"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "7"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "8"
	  'View-process-send-key-as-signal-to-processes)
	(define-key View-process-mode-map "9"
	  'View-process-send-key-as-signal-to-processes)
	)
    (define-key View-process-mode-map "0"
      'digit-argument)
    (define-key View-process-mode-map "1"
      'digit-argument)
    (define-key View-process-mode-map "2"
      'digit-argument)
    (define-key View-process-mode-map "3"
      'digit-argument)
    (define-key View-process-mode-map "4"
      'digit-argument)
    (define-key View-process-mode-map "5"
      'digit-argument)
    (define-key View-process-mode-map "6"
      'digit-argument)
    (define-key View-process-mode-map "7"
      'digit-argument)
    (define-key View-process-mode-map "8"
      'digit-argument)
    (define-key View-process-mode-map "9"
      'digit-argument)
    ))

(if View-process-digit-bindings-send-signal
    (View-process-toggle-digit-bindings 1)
  (View-process-toggle-digit-bindings -1))

(defun View-process-revert-buffer (&optional ignore-auto noconfirm)
  "Updates the view-process buffer with `View-process-status-update'."
  (View-process-status-update))


;;; Emacs version specific stuff

(if (View-process-xemacs-p)
    (require 'view-process-xemacs)
  (require 'view-process-emacs-19))


;;; face setting
(and window-system
(if (facep 'View-process-child-line-face)
    nil
  (make-face 'View-process-child-line-face)
  (if (View-process-search-color View-process-child-line-foreground)
      (set-face-foreground 'View-process-child-line-face 
			   (View-process-search-color
			    View-process-child-line-foreground)))
  (if (View-process-search-color View-process-child-line-background)
      (set-face-background 'View-process-child-line-face
			   (View-process-search-color
			    View-process-child-line-background)))
  (set-face-font 'View-process-child-line-face 
		 View-process-child-line-font)
  (set-face-underline-p 'View-process-child-line-face 
			View-process-child-line-underline-p)))
(and window-system
(if (facep 'View-process-parent-line-face)
    nil
  (make-face 'View-process-parent-line-face)
  (if (View-process-search-color View-process-parent-line-foreground)
      (set-face-foreground 'View-process-parent-line-face 
			   (View-process-search-color
			    View-process-parent-line-foreground)))
  (if (View-process-search-color View-process-parent-line-background)
      (set-face-background 'View-process-parent-line-face
			   (View-process-search-color
			    View-process-parent-line-background)))
  (set-face-font 'View-process-parent-line-face 
		 View-process-parent-line-font)
  (set-face-underline-p 'View-process-parent-line-face 
			View-process-parent-line-underline-p)))
(and window-system
(if (facep 'View-process-single-line-face)
    nil
  (make-face 'View-process-single-line-face)
  (if (View-process-search-color View-process-single-line-foreground)
      (set-face-foreground 'View-process-single-line-face 
			   (View-process-search-color
			    View-process-single-line-foreground)))
  (if (View-process-search-color View-process-single-line-background)
      (set-face-background 'View-process-single-line-face
			   (View-process-search-color
			    View-process-single-line-background)))
  (set-face-font 'View-process-single-line-face 
		 View-process-single-line-font)
  (set-face-underline-p 'View-process-single-line-face 
			View-process-single-line-underline-p)))
(and window-system
(if (facep 'View-process-signaled-line-face)
    nil
  (make-face 'View-process-signaled-line-face)
  (if (View-process-search-color View-process-signaled-line-foreground)
      (set-face-foreground 'View-process-signaled-line-face 
			   (View-process-search-color
			    View-process-signaled-line-foreground)))
  (if (View-process-search-color View-process-signaled-line-background)
      (set-face-background 'View-process-signaled-line-face
			   (View-process-search-color
			    View-process-signaled-line-background)))
  (set-face-font 'View-process-signaled-line-face 
		 View-process-signaled-line-font)
  (set-face-underline-p 'View-process-signaled-line-face 
			View-process-signaled-line-underline-p)))
(and window-system
(if (facep 'View-process-signal-line-face)
    nil
  (make-face 'View-process-signal-line-face)
  (if (View-process-search-color View-process-signal-line-foreground)
      (set-face-foreground 'View-process-signal-line-face 
			   (View-process-search-color
			    View-process-signal-line-foreground)))
  (if (View-process-search-color View-process-signal-line-background)
      (set-face-background 'View-process-signal-line-face
			   (View-process-search-color
			    View-process-signal-line-background)))
  (set-face-font 'View-process-signal-line-face 
		 View-process-signal-line-font)
  (set-face-underline-p 'View-process-signal-line-face 
			View-process-signal-line-underline-p)))
(and window-system
(if (facep 'View-process-renice-line-face)
    nil
  (make-face 'View-process-renice-line-face)
  (if (View-process-search-color View-process-renice-line-foreground)
      (set-face-foreground 'View-process-renice-line-face 
			   (View-process-search-color
			    View-process-renice-line-foreground)))
  (if (View-process-search-color View-process-renice-line-background)
      (set-face-background 'View-process-renice-line-face
			   (View-process-search-color
			    View-process-renice-line-background)))
  (set-face-font 'View-process-renice-line-face 
		 View-process-renice-line-font)
  (set-face-underline-p 'View-process-renice-line-face 
			View-process-renice-line-underline-p)))
(and window-system 
(if (facep 'View-process-header-line-face)
    nil
  (make-face 'View-process-header-line-face)
  (if (View-process-search-color View-process-header-line-foreground)
      (set-face-foreground 'View-process-header-line-face 
			   (View-process-search-color
			    View-process-header-line-foreground)))
  (if (View-process-search-color View-process-header-line-background)
      (set-face-background 'View-process-header-line-face
			   (View-process-search-color
			    View-process-header-line-background)))
  (set-face-font 'View-process-header-line-face 
		 View-process-header-line-font)
  (set-face-underline-p 'View-process-header-line-face 
			View-process-header-line-underline-p)))

(defun View-process-highlight-header-line ()
  "Highlights the headerline with the face `View-process-header-line-face'."
  (let ((extent 
	 (make-extent View-process-header-start View-process-header-end)
	 ))
    (set-extent-face extent 'View-process-header-line-face)
    (set-extent-property extent 'duplicable t))
  )

;;; A short cut for the View-process-status command

(defalias 'ps 'View-process-status)


