;;;$Id$
;;;Description: Emacs interface to beeps program.
(require 'cl)
;;{{{ Variables
(declaim (special emacspeak-directory))
(defvar beeps-program
  (expand-file-name "beeps/beeps" emacspeak-directory)
  "Name of beeps executable")

(defvar beeps-process nil
  "Process object connecting to beeps program")

;;}}}
;;{{{  beeps-initialize

(defsubst beeps-initialize ()
  "Initialize beeps if not already started "
  (declare (special beeps-process))
  (when (or (null beeps-process)
             (not (eq 'run (process-status beeps-process))))
(setq beeps-process
      (start-process "beeps"
                     nil
                     beeps-program))))

 ;;}}}
;;{{{  beeps

(defun beeps (&optional octave length tone decay volume) 
  "Produce a beep"
(declare (special beeps-process))
  (beeps-initialize)
(process-send-string beeps-process
                     (format "%f %d %d %f %s\n"
                             (or volume 2550)
                             (or length 10)
                             (or tone 0)
                             (or decay  0)
                             (or octave "5c"))))

 ;;}}}
;;{{{  beeps-shutdown

(defun beeps-shutdown ()
  "Shut down beeps system"
  (declare (special beeps-process))
  (when (and beeps-process
             (eq 'run (process-status beeps-process)))
    (kill-process beeps-process)
    (setq beeps-process nil)))

 ;;}}}
