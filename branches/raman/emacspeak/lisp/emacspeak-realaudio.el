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

;;; Copyright (c) 1997 by T. V. Raman  
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-sounds)
(require 'thingatpt)
;;}}}
;;{{{ Introduction:

;;; Commentary:

;;; Assuming you have a correctly configured RealAudio
;;; player, this package provides single click access to
;;; starting and stopping a RealAudio stream from anywhere
;;; on the Emacspeak desktop.  Before using this package,
;;; make sure that your realaudio player works outside
;;; Emacs. Then set variable Emacspeak-relaudio-player to
;;; point to the program you use to play RealAudio streams.

;;; Code:


;;}}}
;;{{{ variables

(defvar emacspeak-realaudio-player
  (if (eq window-system 'w32)
      "shelex"
    "rap")
  "*Executable that plays relaudio")

(defvar emacspeak-realaudio-process nil
  "Process handle to running player")
(defvar emacspeak-realaudio-last-url nil
  "Records the last RealAudio resource we played")
(defvar emacspeak-realaudio-history nil
  "History list holding resources we played recently")
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


(defun emacspeak-realaudio-play (resource)
  "Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable
emacspeak-realaudio-shortcuts-directory"
  (interactive
   (list
    (let ((completion-ignore-case t)
          (minibuffer-history emacspeak-realaudio-history))
      (expand-file-name
    (read-file-name "RealAudio resource: "
emacspeak-realaudio-shortcuts-directory
emacspeak-realaudio-last-url)))))
  (declare (special emacspeak-realaudio-player
                    emacspeak-use-midi-icons
                    emacspeak-realaudio-process
                    emacspeak-realaudio-start-time
                    emacspeak-realaudio-mark-time
                    emacspeak-realaudio-shortcuts-directory
                    emacspeak-realaudio-history
                    emacspeak-use-auditory-icons))
  (unless (string= resource (car emacspeak-realaudio-history))
    (pushnew resource emacspeak-realaudio-history))
  (when (and emacspeak-realaudio-process
             (eq 'run (process-status emacspeak-realaudio-process)))
    (kill-process emacspeak-realaudio-process))
  (setq emacspeak-realaudio-process
        (start-process"realaudio" " realaudio"
                      emacspeak-realaudio-player
                      resource ))
  (setq emacspeak-realaudio-start-time (current-time))
  (unless (eq 'run (process-status
                    emacspeak-realaudio-process))
    (message "Failed to start RealAudio"))
  (message "Launched audio stream")
  xx(setq emacspeak-realaudio-last-url resource)
  (when emacspeak-use-auditory-icons
    (unless emacspeak-use-midi-icons
(emacspeak-toggle-midi-icons))))

(defvar emacspeak-realaudio-dont-insist-on-ram-url t
  "*Set to nil if you want emacspeak to insist that realaudio
urls have a .ram or .rm extension.")

(defun emacspeak-realaudio-play-url-at-point ()
  "Play url under point as realaudio"
  (interactive)
  (declare (special emacspeak-realaudio-dont-insist-on-ram-url))
  (let ((url (w3-view-this-url 'no-show)))
    (cond
     ((or emacspeak-realaudio-dont-insist-on-ram-url
       (string-match ".rm?$" url)
       (string-match ".ram?$" url))
      (message "Playing Realaudio URL under point")
        (emacspeak-realaudio-play url))
      (t (message "%s does not look like realaudio"
             url)))))

(defun emacspeak-realaudio-stop ()
  "Stop playing realaudio"
  (interactive)
  (declare (special emacspeak-realaudio-process
                    emacspeak-realaudio-stop-time))
  (kill-process emacspeak-realaudio-process)
(setq emacspeak-realaudio-stop-time (current-time))
  (message "Stopped RealAudio")
  (emacspeak-toggle-auditory-icons t))

(defvar emacspeak-realaudio-start-time nil
  "Records  time when we started playing.
Used to track how long into a stream we've played.")

(defvar emacspeak-realaudio-stop-time nil
"Records time stamp when we stopped playing.")

(defvar emacspeak-realaudio-mark-time nil
"Records where to resume playing if streaming is stopped for
some reason.")


(defun emacspeak-realaudio  (&optional resume)
  "Start or stop playing RealAudio.  Stops playing realaudio
if there is a stream currently playing.  Otherwise, prompts
for a realaudio resource.  Realaudio resources can be
specified either as a Realaudio URL, the location of a local
Realaudio file, or as the name of a local Realaudio
metafile. Realaudio resources you have played in this
session are available in the minibuffer history.  The
default is to play the resource you played most
recently. Emacspeak uses the contents of the directory
specified by variable
emacspeak-realaudio-shortcuts-directory to offer a set of
completions. Hit space to use this completion list.  Optional
interactive prefix arg prompts for time stamp at which to resume."
  (interactive "P")
  (declare (special emacspeak-realaudio-process))
  (cond
   ((and emacspeak-realaudio-process
         (eq 'run (process-status emacspeak-realaudio-process)))
    (emacspeak-realaudio-stop))
   (t  (call-interactively 'emacspeak-realaudio-play))))

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
;;{{{ W3 hook

(add-hook 'w3-mode-hook
          (function
           (lambda nil
             (declare (special w3-mode-map))
             (define-key w3-mode-map "\M-r" 'emacspeak-realaudio-play-url-at-point)
             (define-key w3-mode-map "\M-d" 'emacspeak-realaudio-stop))))

;;}}}
(provide 'emacspeak-realaudio)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
