;;; emacspeak-realaudio.el --- Play realaudio from Emacs
;;; $Id$
;;; $Author$
;;; Description: Single click access to RealAudio from emacspeak
;;; Keywords: Emacspeak, RealAudio
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
;;{{{ Introduction:

;;; Commentary:

;;; Assuming you have a correctly configured RealAudio
;;; player, this package provides single click access to
;;; starting and stopping a RealAudio stream from anywhere
;;; on the Emacspeak desktop.  Before using this package,
;;; make sure that your realaudio player works outside
;;; Emacs. Then set variable Emacspeak-realaudio-player to
;;; point to the program you use to play RealAudio streams.

;;; Code:

;;}}}
;;{{{ variables
(defgroup emacspeak-realaudio nil
  "Emacspeak Realaudio  customization.")

(defcustom emacspeak-realaudio-revert-to-auditory-icons t
  "Set this to T if you want to switch back from using midi
icons once a realaudio stream is done playing."
  :group 'emacspeak-realaudio
  :type 'boolean)

(defcustom emacspeak-realaudio-player
  (cond
   ((eq window-system 'w32)
    "shelex")
   ((file-exists-p "/usr/bin/trplayer")
    "/usr/bin/trplayer")
   (t "rap"))
  "*Executable that plays realaudio"
  :group 'emacspeak-realaudio
  :type 'string)

(defcustom emacspeak-realaudio-player-options 
  (list "-l" "-i" "-b" "-c" )
  "*Options for realplayer."
  :group 'emacspeak-realaudio
  :type '(repeat :tag "RealAudio Options"
                 (string :tag "Option")))

(defvar emacspeak-realaudio-process nil
  "Process handle to running player")
(defvar emacspeak-realaudio-last-url nil
  "Records the last RealAudio resource we played")
;;;###autoload
(defvar emacspeak-realaudio-history nil
  "History list holding resources we played recently")
;;;###autoload
(defvar emacspeak-realaudio-shortcuts-directory 
  (expand-file-name "realaudio/" emacspeak-directory)
  "*Directory where we keep realaudio shortcuts.
I typically keep .ram --RealAudio metafiles-- in this
directory.
Realaudio metafiles typically contain a single line that
specifies the actual location of the realaudio stream
--typically the .ra file.")

;;}}}
;;{{{ commands

(defvar emacspeak-realaudio-buffer "*realaudio*"
  "Name of realaudio process buffer")

(defvar emacspeak-realaudio-start-time-mark nil
  "Record mark of start time in seconds.")

(make-variable-buffer-local 'emacspeak-realaudio-start-time-mark)

(defvar emacspeak-realaudio-end-time-mark nil
  "Record mark of end time in seconds.")

(make-variable-buffer-local 'emacspeak-realaudio-end-time-mark)

;;;###autoload
(defun emacspeak-realaudio-play (resource &optional prompt-time)
  "Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable emacspeak-realaudio-shortcuts-directory. "
  (interactive
   (list
    (let ((completion-ignore-case t)
          (ido-mode nil)
          (emacspeak-speak-messages nil)
          (minibuffer-history emacspeak-realaudio-history)
          (file nil))
      (emacspeak-pronounce-define-local-pronunciation
       emacspeak-realaudio-shortcuts-directory " shortcuts/ ")
      (kill-new default-directory)
      (setq file
            (read-file-name "RealAudio resource: "
                            emacspeak-realaudio-shortcuts-directory
                            (if (eq major-mode 'dired-mode)
                                (dired-get-filename)
                              emacspeak-realaudio-last-url)))
      (pop kill-ring)
      file)
    current-prefix-arg))
  (declare (special emacspeak-realaudio-player emacspeak-realaudio-this-resource
                    emacspeak-realaudio-buffer 
                    emacspeak-realaudio-player-options ido-mode
                    emacspeak-aumix-multichannel-capable-p
                    emacspeak-realaudio-process
                    emacspeak-realaudio-shortcuts-directory
                    emacspeak-realaudio-history
                    emacspeak-use-auditory-icons))
  (let ((ido-mode nil))
    (unless (or
	     (string-match "^rtsp:" resource)
	     (string-match "^http:"  resource))
      (setq resource
	    (expand-file-name resource)))
    (unless (string-equal resource (car emacspeak-realaudio-history))
      (pushnew resource emacspeak-realaudio-history))
    (when (get-buffer "*realaudio*")
      (kill-buffer emacspeak-realaudio-buffer))
    (let ((process-connection-type nil)
	  (default-directory
	    (if (or (string-match "^rtsp:" resource)
		    (string-match "^http" resource ))
		default-directory
	      (file-name-directory resource)))
	  (options (copy-list emacspeak-realaudio-player-options)))
      (when prompt-time
	(push (read-from-minibuffer "Time spec: ")
	      options)
	(push "-t" options))
      (setq emacspeak-realaudio-process
	    (apply 'start-process"realaudio" emacspeak-realaudio-buffer
		   emacspeak-realaudio-player
		   resource
		   options))
      (when (string-match "trplayer"
			  emacspeak-realaudio-player)
	(save-excursion
	  (set-buffer emacspeak-realaudio-buffer)
	  (emacspeak-realaudio-mode)
          (goto-char (point-min))
          (insert
           (format "Stream: %s\n"
                   resource))
          (set-marker (process-mark emacspeak-realaudio-process) (point))
	  (setq emacspeak-realaudio-this-resource resource)))
      (unless (eq 'run (process-status emacspeak-realaudio-process))
	(error "Failed to start RealAudio"))
      (set-process-sentinel emacspeak-realaudio-process 'emacspeak-realaudio-process-sentinel)
      (message "Launched audio stream")
      (setq emacspeak-realaudio-last-url resource)
      (when
	  (and emacspeak-use-auditory-icons
	       (not emacspeak-aumix-multichannel-capable-p)
	       (not (emacspeak-using-midi-p)))
	(emacspeak-set-auditory-icon-player
	 'emacspeak-play-midi-icon)))))

(defvar emacspeak-realaudio-dont-insist-on-ram-url t
  "*Set to nil if you want emacspeak to insist that realaudio
urls have a .ram or .rm extension.")
;;;###autoload

(defun emacspeak-realaudio-process-sentinel  (process state)
  "Cleanup after realaudio is done. "
  (declare (special emacspeak-realaudio-revert-to-auditory-icons
                    emacspeak-realaudio-reset-auditory-display))
  (when  (and emacspeak-realaudio-revert-to-auditory-icons
              (emacspeak-using-midi-p))
    (emacspeak-set-auditory-icon-player 'emacspeak-serve-auditory-icon))
  (when emacspeak-realaudio-reset-auditory-display
    (emacspeak-aumix-reset)))

(defun emacspeak-realaudio-stop ()
  "Stop playing realaudio"
  (interactive)
  (declare (special emacspeak-realaudio-process))
  (kill-process emacspeak-realaudio-process)
  (message "Stopped RealAudio")
  (emacspeak-toggle-auditory-icons t))
(defun emacspeak-realaudio-dispatch (char )
  "Dispatch `CHAR'  to realaudio process.
Echo output and return it as a string."
  (declare (special emacspeak-realaudio-process))
  (let*  ((buffer (process-buffer emacspeak-realaudio-process))
          (mark (save-excursion
                  (set-buffer buffer)
                  (point-max))))
    (process-send-string
     emacspeak-realaudio-process
     (format "%c" char))
    (accept-process-output  emacspeak-realaudio-process 1)
    (message "%s"
             (save-excursion
               (set-buffer buffer)
               (buffer-substring mark (process-mark
                                       emacspeak-realaudio-process))))))

;;;###autoload
(defun emacspeak-realaudio-get-current-time-in-seconds ()
  "Return current time in seconds."
  (interactive)
  (let* ((emacspeak-speak-messages nil)
         (seconds 0)
         (timespec (emacspeak-realaudio-dispatch ?t))
         (fields (split-string  timespec ":")))
    (pop fields)                        ;discard "time"
    (setq fields 
          (mapcar #'string-to-number fields))
    (setq seconds 
          (+
           (* 3600 (first fields))
           (* 60 (second fields))
           (third fields)))
    (when (interactive-p)
      (kill-new (format "%d" seconds))
      (dtk-speak (format "%d" seconds)))
    seconds))

;;;###autoload
(defun emacspeak-realaudio-set-start-mark (&optional mark-time)
  "Set start mark. Default is to set marker to current play time.
Mark is specified in seconds."
  (interactive "P")
  (declare (special emacspeak-realaudio-start-time-mark))
  (setq emacspeak-realaudio-start-time-mark
        (cond
         ((and (interactive-p) mark-time)
          (read-minibuffer "Mark in seconds:"))
         ((interactive-p)
          (emacspeak-realaudio-get-current-time-in-seconds))
         (t (or mark-time 0))))
  (when (interactive-p)
    (message "Set start mark to %s"
             emacspeak-realaudio-start-time-mark)))

;;;###autoload
(defun emacspeak-realaudio-set-end-mark (&optional mark-time)
  "Set end mark. Default is to set marker to current play time.
Mark is specified in seconds."
  (interactive "P")
  (declare (special emacspeak-realaudio-start-time-mark))
  (setq emacspeak-realaudio-end-time-mark
        (cond
         ((and (interactive-p) mark-time)
          (read-minibuffer "Mark in seconds:"))
         ((interactive-p)
          (emacspeak-realaudio-get-current-time-in-seconds))
         (t (or mark-time 0))))
  (when (interactive-p)
    (message "Set end mark to %s"
             emacspeak-realaudio-end-time-mark)))

;;;###autoload
(defcustom emacspeak-realaudio-mp3-clipper 
  "/usr/local/bin/qmp3cut"
  "Executable used to clip MP3 files."
  :type 'string
  :group 'emacspeak-realaudio)

;;;###autoload
(defun emacspeak-realaudio-write-mp3-clip (start end file)
  "Writes specified clip from current mp3 stream.
Prompts for start and end times as well as file  to save the clippi"
  (interactive
   (list
    (read-from-minibuffer "Start time in seconds:"
                          (format "%s"
                                  emacspeak-realaudio-start-time-mark))
    (read-from-minibuffer "End time in seconds: "
                          (format "%s" emacspeak-realaudio-end-time-mark))
    (read-file-name "File to save clip to")))
  (declare (special emacspeak-realaudio-end-time-mark
                    emacspeak-realaudio-start-time-mark
                    emacspeak-realaudio-this-resource
                    emacspeak-realaudio-mp3-clipper))
  (unless (string-equal"mp3"
		       (file-name-extension emacspeak-realaudio-this-resource))
    (error  "Can only clip MP3  files."))
  (unless (file-executable-p emacspeak-realaudio-mp3-clipper)
    (error
     "I cannot find an MP3 clipper. Install package quelcom to obtain
qmp3cut."))
  (let ((command
         (format "%s -B %ss -E %ss -o %s %s &"
                 emacspeak-realaudio-mp3-clipper
                 start end   file
                 emacspeak-realaudio-this-resource)))
    (shell-command command)
    (message "Executing %s asynchronously."
	     command)))

(defun emacspeak-realaudio-trplayer-command (char)
  "Execute TRPlayer command."
  (interactive "cTRPlayer Command:")
  (declare (special emacspeak-realaudio-process))
  (cond
   ((char-equal char ?\;)
    (emacspeak-realaudio-select-realaudio-buffer))
   (t (emacspeak-realaudio-dispatch char ))))

(emacspeak-fix-interactive-command-if-necessary
 'emacspeak-realaudio-trplayer-command)

(defcustom emacspeak-realaudio-reset-auditory-display t 
  "Set this to T if you want the audio settings reset after
a realaudio sream is done playing."
  :group 'emacspeak-realaudio
  :type 'boolean)
;;;###autoload
(defun emacspeak-realaudio  (&optional ignored)
  "Start or control streaming audio including MP3 and
realaudio.  If using `TRPlayer' as the player, accepts
trplayer control commands if a stream is already playing.
Otherwise, the playing stream is simply stopped.  If no
stream is playing, this command prompts for a realaudio
resource.  Realaudio resources can be specified either as a
Realaudio URL, the location of a local Realaudio file, or as
the name of a local Realaudio metafile. Realaudio resources
you have played in this session are available in the
minibuffer history.  The default is to play the resource you
played most recently. Emacspeak uses the contents of the
directory specified by variable
emacspeak-realaudio-shortcuts-directory to offer a set of
completions. Hit space to use this completion list.

If using TRPlayer, you can either give one-shot commands
using command emacspeak-realaudio available from anywhere on
the audio desktop as `\\[emacspeak-realaudio]'.
Alternatively, switch to buffer *realaudio* using
`\\[emacspeak-realaudio];' if you wish to issue many
navigation commands.  Note that buffer *realaudio* uses a
special major mode that provides the various navigation
commands via single keystrokes."
  (interactive "P")
  (declare (special emacspeak-realaudio-process))
  (let ((ido-mode nil))
    (cond
     ((and emacspeak-realaudio-process
	   (eq 'run (process-status emacspeak-realaudio-process)))
      (if  (string-match "trplayer"
			 emacspeak-realaudio-player)
	  (call-interactively 'emacspeak-realaudio-trplayer-command)
	(emacspeak-realaudio-stop)))
     (t  (call-interactively 'emacspeak-realaudio-play)))))

;;}}}
;;{{{ browsing ramfiles

(defun emacspeak-realaudio-parse-ramfile(ramfile)
  "Returns a list of strings."
  (let ((buff (find-file-noselect ramfile))
        (result nil))
    (save-excursion
      (set-buffer buff)
      (while (not (eobp))
        (push (substring 
               (thing-at-point 'line) 0 -1) 
              result)
        (forward-line 1)))
    (kill-buffer buff)
    result))
;;;###autoload
(defun emacspeak-realaudio-browse (ramfile &optional start-time)
  "Browse RAM file before playing the selected component."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (minibuffer-history emacspeak-realaudio-history))
      (expand-file-name
       (read-file-name "RealAudio resource: "
                       emacspeak-realaudio-shortcuts-directory
                       emacspeak-realaudio-last-url)))    
    current-prefix-arg))
  (let ((components (emacspeak-realaudio-parse-ramfile
                     ramfile))
        (component nil)
        (option nil)
        (s nil))
    (setq component 
          (completing-read
           "Realaudio component:"
           (loop for f in
                 components
                 collect (cons f f ))))
    (when(and  (interactive-p)
               start-time)
      (setq s (read-string "Minutes to skip: "))
      (setq option 
            (if (string-match "?" component)
                (format "&start=00:00:%s:00.0" s)
              (format "?start=00:00:%s:00.0" s)))
      (setq component
            (concat component option )))
    (emacspeak-realaudio-play  component)))
     

;;}}}
;;{{{ define a derived mode for realaudio interaction 

(define-derived-mode emacspeak-realaudio-mode fundamental-mode 
  "Realaudio Interaction"
  "Major mode for streaming audio. \n\n
\\{emacspeak-realaudio-mode-map}"
  (emacspeak-realaudio-setup-keys))

(defun emacspeak-realaudio-setup-keys ()
  "Define key bindings for emacspeak-realaudio."
  (declare (special emacspeak-realaudio-mode-map
                    emacspeak-realaudio-trplayer-keys))
  (define-key emacspeak-realaudio-mode-map " " 'dtk-stop)
  (define-key emacspeak-realaudio-mode-map "C"
    'emacspeak-realaudio-get-current-time-in-seconds)
  (define-key emacspeak-realaudio-mode-map "m"
    'emacspeak-realaudio-set-start-mark)
  (define-key emacspeak-realaudio-mode-map "M"
    'emacspeak-realaudio-set-end-mark)
  (define-key emacspeak-realaudio-mode-map "w"
    'emacspeak-realaudio-write-mp3-clip)
  (define-key emacspeak-realaudio-mode-map [left] 'emacspeak-aumix-wave-decrease)
  (define-key emacspeak-realaudio-mode-map [right] 'emacspeak-aumix-wave-increase)
  (loop for c in emacspeak-realaudio-trplayer-keys
        do
        (define-key emacspeak-realaudio-mode-map
          (format "%c" c)
          'emacspeak-realaudio-trplayer-call-command)))

;;;###autoload
(defvar emacspeak-realaudio-trplayer-keys
  (list ?p ?t ?s ?e ?l ?i
        ?< ?> ?. ?, ?0 ?9
        ?[ ?] ?{ ?})
  "Keys accepted by TRPlayer.")

(defvar emacspeak-realaudio-this-resource nil
  "Records location of resource being played.")

(make-variable-buffer-local 'emacspeak-realaudio-this-resource)

(defun emacspeak-realaudio-trplayer-call-command ()
  "Call appropriate TRPlayer command."
  (interactive)
  (emacspeak-realaudio-trplayer-command last-input-char))

(defun emacspeak-realaudio-select-realaudio-buffer ()
  "Switch to realaudio buffer."
  (interactive)
  (pop-to-buffer "*realaudio*")
  (emacspeak-speak-mode-line))

;;}}}
(provide 'emacspeak-realaudio)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
