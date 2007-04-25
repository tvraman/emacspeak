;;; $Id: view-process-system-specific.el,v 1.5 1995/03/18 08:20:52 muenkel Exp $
;;;
;;; Copyright (C) 1995 Heiko Muenkel
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
;;;	This file contains system specific stuff for the
;;;	view-process-mode. It isn't necessary, that each systems has
;;;	an entry in this file, because there are default values, which
;;;	are used, if no entry exists.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of you lisp load path directories.
;;;

(provide 'view-process-system-specific)

(defvar View-process-specific-system-list
  '(("linux" nil "bsd"
     nil
     View-process-kill-signals-linux)
    ("sunos" "4" "bsd"
     View-process-field-name-descriptions-sunos4
     View-process-kill-signals-sunos4)
    ("sunos" "5" "system-v"
     View-process-field-name-descriptions-sunos5
     View-process-kill-signals-sunos5)
    ("irix" nil "system-v"
     View-process-field-name-descriptions-irix
     View-process-kill-signals-irix)
    ("hp-ux" nil "system-v"
     View-process-field-name-descriptions-hpux
     View-process-kill-signals-hpux)
    )
  "This is a list with all systems, for which specific information about 
allowed signals and about the ps output exists. Set it to nil, if you don't 
want to use this specific information.
The first string of each sublist is the name of the system, the second 
string is the mayor version number or nil. The third one determines, 
if the ps command is BSD or System V like. The mayor version is only 
necessary, if there are BSD and System V versions with the same system 
names (SUN has done such a stupid system naming.), otherwise it is nil.
The fifth entry is nil or the name of a special list with field name 
descriptions. The sixth entry is nil or the name of a special list with 
kill signals.")

;;; signals

(defvar View-process-kill-signals-bsd nil
  "An alist with the possible signals for the kill command for BSD
systems. It is only used, if the system type can't be determined or if
it is not in the `View-process-specific-system-list'.")

(defvar View-process-kill-signals-system-v nil
  "An alist with the possible signals for the kill command for BSD
systems. It is only used, if the system type can't be determined or if
it is not in the `View-process-specific-system-list'.")

(defvar View-process-kill-signals-sunos4
  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGABRT" "6") ("SIGEMT" "7") 
    ("SIGFPE" "8") ("SIGKILL" "9") ("SIGBUS" "10") ("SIGSEGV" "11") 
    ("SIGSYS" "12") ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") 
    ("SIGURG" "16") ("SIGSTOP" "17") ("SIGTSTP" "18") ("SIGCONT" "19") 
    ("SIGCHLD" "20") ("SIGCLD" "20") ("SIGTTIN" "21") ("SIGTTOU" "22")
    ("SIGIO" "23") ("SIGPOLL" "23") ("SIGXCPU" "24") ("SIGXFSZ" "25") 
    ("SIGVTALRM" "26") ("SIGPROF" "27") ("SIGWINCH" "28") 
    ("SIGLOST" "29") ("SIGUSR1" "30") ("SIGUSR2" "31") 
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("16" "16") ("17" "17") ("18" "18") 
    ("19" "19") ("20" "20") ("21" "21") ("22" "22") ("23" "23") 
    ("24" "24") ("25" "25") ("26" "26") ("27" "27") ("28" "28") 
    ("29" "29") ("30" "30") ("31" "31"))
  "An alist with the possible signals for the kill command for SunOS 4.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")
  
(defvar View-process-kill-signals-sunos5
  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGABRT" "6") ("SIGEMT" "7") 
    ("SIGFPE" "8") ("SIGKILL" "9") ("SIGBUS" "10") ("SIGSEGV" "11") 
    ("SIGSYS" "12") ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") 
    ("SIGUSR1" "16") ("SIGUSR2" "17") ("SIGCHLD" "18") ("SIGCLD" "18")
    ("SIGPWR" "19") ("SIGWINCH" "20") ("SIGURG" "21") ("SIGPOLL" "22")
    ("SIGIO" "22") ("SIGSTOP" "23") ("SIGTSTP" "24") ("SIGCONT" "25") 
    ("SIGTTIN" "26") ("SIGTTOU" "27") ("SIGVTALRM" "28") ("SIGPROF" "29")
    ("SIGXCPU" "30") ("SIGXFSZ" "31") ("SIGWAITING" "32") ("SIGLWP" "33")
    ("SIGFREEZE" "34") ("SIGTHAW" "36") ("SIGRTMIN" "36") ("SIGRTMAX" "43")
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("16" "16") ("17" "17") ("18" "18") 
    ("19" "19") ("20" "20") ("21" "21") ("22" "22") ("23" "23") 
    ("24" "24") ("25" "25") ("26" "26") ("27" "27") ("28" "28") 
    ("29" "29") ("30" "30") ("31" "31") ("32" "32") ("33" "33")
    ("34" "34") ("36" "36") ("43" "43"))
  "An alist with the possible signals for the kill command for SunOS 5.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")
  
(defvar View-process-kill-signals-irix
  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGABRT" "6") ("SIGEMT" "7") 
    ("SIGFPE" "8") ("SIGKILL" "9") ("SIGBUS" "10") ("SIGSEGV" "11") 
    ("SIGSYS" "12") ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") 
    ("SIGUSR1" "16") ("SIGUSR2" "17") ("SIGCLD" "18") ("SIGCHLD" "18") 
    ("SIGPWR" "19") ("SIGWINCH" "20") ("SIGURG" "21") ("SIGPOLL" "22")
    ("SIGIO" "22") ("SIGSTOP" "23") ("SIGTSTP" "24") ("SIGCONT" "25")
    ("SIGTTIN" "26") ("SIGTTOU" "27") ("SIGVTALRM" "28") ("SIGPROF" "29") 
    ("SIGXCPU" "30") ("SIGXFSZ" "31") ("SIG32" "32")
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("16" "16") ("17" "17") ("18" "18") 
    ("19" "19") ("20" "20") ("21" "21") ("22" "22") ("23" "23") 
    ("24" "24") ("25" "25") ("26" "26") ("27" "27") ("28" "28") 
    ("29" "29") ("30" "30") ("31" "31") ("32" "32"))
  "An alist with the possible signals for the kill command for IRIX.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")
  
(defvar View-process-kill-signals-linux
  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGBUS" "7") ("SIGFPE" "8") 
    ("SIGKILL" "9") ("SIGUSR1" "10") ("SIGSEGV" "11") ("SIGUSR2" "12") 
    ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") ("SIGCHLD" "17") 
    ("SIGCONT" "18") ("SIGSTOP" "19") ("SIGTSTP" "20") ("SIGTTIN" "21") 
    ("SIGTTOU" "22") ("SIGIO" "23") ("SIGXCPU" "24") ("SIGXFSZ" "25") 
    ("SIGVTALRM" "26") ("SIGPROF" "27") ("SIGWINCH" "28") ("SIGPWR" "30") 
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("17" "17") ("18" "18") ("19" "19") 
    ("20" "20") ("21" "21") ("22" "22") ("23" "23") ("24" "24") 
    ("25" "25") ("26" "26") ("27" "27") ("28" "28") ("30" "30"))
  "An alist with the possible signals for the kill command for linux.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")

;; all Linux signals
;(defvar View-process-kill-signals
;  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
;    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGBUS" "7") ("SIGFPE" "8") 
;    ("SIGKILL" "9") ("SIGUSR1" "10") ("SIGSEGV" "11") ("SIGUSR2" "12") 
;    ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") ("SIGCHLD" "17") 
;    ("SIGCONT" "18") ("SIGSTOP" "19") ("SIGTSTP" "20") ("SIGTTIN" "21") 
;    ("SIGTTOU" "22") ("SIGIO" "23") ("SIGXCPU" "24") ("SIGXFSZ" "25") 
;    ("SIGVTALRM" "26") ("SIGPROF" "27") ("SIGWINCH" "28") ("SIGPWR" "30") 
;    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
;    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
;    ("14" "14") ("15" "15") ("17" "17") ("18" "18") ("19" "19") 
;    ("20" "20") ("21" "21") ("22" "22") ("23" "23") ("24" "24") 
;    ("25" "25") ("26" "26") ("27" "27") ("28" "28") ("30" "30"))
;  "An alist with the possible signals for the kill command.
;It may be that you've other signals on your system. Try to test
;it with \"kill -l\" in a shell.")

(defvar View-process-kill-signals-hpux
  '(("SIGHUP" "1") ("SIGINT" "2") ("SIGQUIT" "3") ("SIGILL" "4") 
    ("SIGTRAP" "5") ("SIGIOT" "6") ("SIGABRT" "6") ("SIGEMT" "7") 
    ("SIGFPE" "8") ("SIGKILL" "9") ("SIGBUS" "10") ("SIGSEGV" "11") 
    ("SIGSYS" "12") ("SIGPIPE" "13") ("SIGALRM" "14") ("SIGTERM" "15") 
    ("SIGUSR1" "16") ("SIGUSR2" "17") ("SIGCLD" "18") ("SIGCHLD" "18") 
    ("SIGPWR" "19") ("SIGVTALRM" "20") ("SIGPROF" "21") ("SIGIO" "22")
    ("SIGWINCH" "23") ("SIGSTOP" "24") ("SIGTSTP" "25") ("SIGCONT" "26")
    ("SIGTTIN" "27") ("SIGTTOU" "28") ("SIGURG" "29") ("SIGLOST" "30")
    ("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") 
    ("8" "8") ("9" "9") ("10" "10") ("11" "11") ("12" "12") ("13" "13") 
    ("14" "14") ("15" "15") ("16" "16") ("17" "17") ("18" "18") 
    ("19" "19") ("20" "20") ("21" "21") ("22" "22") ("23" "23") 
    ("24" "24") ("25" "25") ("26" "26") ("27" "27") ("28" "28") 
    ("29" "29") ("30" "30") ("31" "31") ("32" "32"))
  "An alist with the possible signals for the kill command for HP-UX.
It may be that you've other signals on your system. Try to test
it with \"kill -l\" in a shell.")

;;; process field descriptions

;; more general descriptions for BSD and System V
 
(defvar View-process-field-name-descriptions-bsd
  '(
    ("CP" "Short-term CPU  utilization  factor  (used  in scheduling). ")
    ("F" "Flags  (in  hex) associated with process as in <sys/proc.h>. ")
    ("LIM"  ("Soft limit on memory used. "
	     ("xx" "xx=no limit. ")))
    ("RE" "Residency  time (seconds in core)")
    ("SIZE" "Virtual image size, size data+stack (in KByte).")
    ("SL" "Sleep time of the process (seconds blocked).")
    ("STAT" ("Status. "
	     ("R" "R=runnable. ")
	     ("S" "S=sleeping < 20s. ")
	     ("D" "D=un-interruptible wait (eg disk or NFS I/O). ")
	     ("T" "T=stopped. ")
	     ("Z" "Z=zombie (terminated). ")
	     ("W" "W=Swapped out. ")
	     ("I" "I=idle, sleeping > 20s. ")
	     ("P" "P=Page Wait." )
	     ("N" "N=started with nice. ")
	     (">" ">=exceeded memory limit. ") 
	     ("SW" "S=sleeping. W=waiting on an event. ")
	     ("IW" "I=intermediate status. W=waiting on an event. ")))
    ("SZ" "Virtual image size, size data+stack (in KByte). ")
    ("WCHAN" "Event on which process is waiting. ")
    )
  "Help list with the descriptions of ps fields for BSD systems.")

(defvar View-process-field-name-descriptions-system-v
  '(
    ("C" "Processor utilization for scheduling. ")
    ("CLS" "Scheduling class. ")
    ("F" "Flags  (in  hex) No meaning should be currently ascribed to them. ")
    ("PRI" "Priority, without -c: no > => prio <, with -c: no > => prio > .")
    ("S" ("State. "
	  ("O" "O=Process is running on a processor. ")
	  ("S" "S=Sleeping, process is waiting for an event. ")
	  ("R" "R=Runnable, process is on run queue. ")
	  ("I" "I=Idle, process is being created. ")
	  ("Z" "Z=Zombie state, process terminated and parent not waiting. ")
	  ("T" "T=Traced, process stopped by a signal, parent is tracing it. ")
	  ("X" "X=SXBRK  state:  process is waiting for more primary memory. ")
	  ))
    ("STIME" "Start time. ")
    ("SZ" "Size (in Pages) of the swappable process's image in main memory. ")
    ("WCHAN" "Event on which process is waiting or in SXBRK state. ")
    )
  "Help list with the descriptions of ps fields for System V.")

;; for specifc systems

(defvar View-process-field-name-descriptions-sunos4
  '(
    ("CP" "Short-term CPU  utilization  factor  (used  in scheduling). ")
    ("F" "Flags  (in  hex) associated with process as in <sys/proc.h>. ")
    ("LIM"  ("Soft limit on memory used. "
	     ("xx" "xx=no limit. ")))
    ("RE" "Residency  time (seconds in core)")
    ("SIZE" "Virtual image size, size data+stack (in KByte).")
    ("SL" "Sleep time of the process (seconds blocked).")
    ("STAT" ("Status. "
	     ("R" "R=runnable. ")
	     ("S" "S=sleeping < 20s. ")
	     ("D" "D=un-interruptible wait (eg disk or NFS I/O). ")
	     ("T" "T=stopped. ")
	     ("Z" "Z=zombie (terminated). ")
	     ("W" "W=Swapped out. ")
	     ("I" "I=idle, sleeping > 20s. ")
	     ("P" "P=Page Wait." )
	     ("N" "N=started with nice. ")
	     (">" ">=exceeded memory limit. ") 
	     ("SW" "S=sleeping. W=waiting on an event. ")
	     ("IW" "I=intermediate status. W=waiting on an event. ")))
    ("SZ" "Virtual image size, size data+stack (in KByte). ")
    ("WCHAN" "Event on which process is waiting. ")
    )
  "Help list with the descriptions of ps fields for SunOS 4.")

(defvar View-process-field-name-descriptions-sunos5
  '(
    ("C" "Processor utilization for scheduling. ")
    ("CLS" "Scheduling class. ")
    ("F" "Flags  (in  hex) No meaning should be currently ascribed to them. ")
    ("PRI" "Priority, without -c: no > => prio <, with -c: no > => prio > .")
    ("S" ("State. "
	  ("O" "O=Process is running on a processor. ")
	  ("S" "S=Sleeping, process is waiting for an event. ")
	  ("R" "R=Runnable, process is on run queue. ")
	  ("I" "I=Idle, process is being created. ")
	  ("Z" "Z=Zombie state, process terminated and parent not waiting. ")
	  ("T" "T=Traced, process stopped by a signal, parent is tracing it. ")
	  ("X" "X=SXBRK  state:  process is waiting for more primary memory. ")
	  ))
    ("STIME" "Start time. ")
    ("SZ" "Size (in Pages) of the swappable process's image in main memory. ")
    ("WCHAN" "Event on which process is waiting or in SXBRK state. ")
    )
  "Help list with the descriptions of ps fields for SunOS 5.")

(defvar View-process-field-name-descriptions-irix
  '(
    ("F" ("Flags. "
	  ("01" "01=Process is a system (resident) process. ")
	  ("02" "02=Process is being traced. ")
	  ("04" "04=Stopped process has been given to parent via wait. ")
	  ("08" "08=Process is sleeping at a non-interruptible priority. ")
	  ("10" "10=Process is in core. ")
	  ("20" "20=Process user area is in core. ")
	  ("40" "40=Process has enabled atomic operator emulation. ")
	  ("80" "80=Process in stream poll or select. ")))
    ("C" "Processor utilization for scheduling. ")
    ("CLS" "Scheduling class. ")
    ("COMD" "The command name. ")
    ("P" "Number of processor on which the process is executing. ")
    ("RSS" "Total resident  size (in pages/4096 Bytes) of process. ")
    ("S" ("State. "
	  ("0" "0=Process is running on a processor. ")
	  ("S" "S=Process is sleeping, waiting for a resource. ")
	  ("R" "R=Process is running. ")
	  ("Z" "Z=Process is terminated and parent not waiting. ")
	  ("T" "T=Process is stopped. ")
	  ("I" "I=Process is in intermediate state of creation. ")
	  ("X" "X=Process is waiting for memory. ")))
    ("STIME" "The starting time of the process. ")
    ("SZ" "Total size (in pages/4096 Bytes) of the process. ")
    ("WCHAN" "Event on which process is waiting. ")
    )
  "Help list with the descriptions of ps fields for IRIX.")

(defvar View-process-field-name-descriptions-hpux
  '(
    ("F" ("Flags. "
	  ("00" "00=Process is swapped. ")
	  ("01" "01=Process is in core. ")
	  ("02" "01=Process is a system process. ")
	  ("04" "04=Process is locked in core (e.g., for physical I/O). ")
	  ("10" "10=Process is being traced by another process. ")
	  ("20" "20=Process is being traced by another process. ") 
	  ;; another tracing flag
	  ))
    ("UID" 
 "Real user ID number of the process owner.  Login name under the -f option. ")
    ("PID" "The process ID of the process. ")
    ("PPID" "The process ID of the parent process. ")
    ("PRI" "The priority of the process; higher numbers mean lower priority. ")
    ("NI" "Nice value; used in priority computation. ")
    ("ADDR" "Memory address of the process, if resident, or disk address. ")
    ("TTY" "The controlling terminal for the process. ")
    ("TIME" "The cumulative execution time for the process [min:sec]. ")
    ("STIME" 
     "Starting time of the process, or starting date if elapsed > 24h. ")
    ("C" "Processor utilization for scheduling. ")
    ("COMD" "The command name. ")
    ("COMMAND" "The command name. ")
    ("S" ("State. "
	  ("0" "0=Process is non-existing. ")
	  ("S" "S=Process is sleeping. ")
	  ("W" "Process is waiting for a resource. ")
	  ("R" "R=Process is running. ")
	  ("Z" "Z=Process is terminated and parent not waiting. ")
	  ("T" "T=Process is stopped. ")
	  ("I" "I=Process is in intermediate state of creation. ")
	  ("X" "X=Process is waiting for memory. ")))
    ("SZ" "Total size (in pages/4096 Bytes) of the process. ")
    ("WCHAN" "Event on which process is waiting. ")
    )
  "Help list with the descriptions of ps fields for HP-UX.")
