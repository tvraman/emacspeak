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

;;; Copyright (c) 1995 -- 2002, T. V. Raman
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

(eval-when-compile (require 'cl))
(require 'derived)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'emacspeak-fix-interactive))
(require 'emacspeak-aumix)
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
;;; Emacs. Then set variable Emacspeak-realaudio-player to
;;; point to the program you use to play RealAudio streams.

;;; Code:


;;}}}
;;{{{ variables
(defcustom emacspeak-realaudio-revert-to-auditory-icons t
"Set this to T if you want to switch back from using midi
icons once a realaudio stream is done playing."
:group 'emacspeak
:type 'boolean)

(defcustom emacspeak-realaudio-player
  (cond
   ((eq window-system 'w32)
      "shelex")
    ((file-exists-p "/usr/bin/trplayer")
"/usr/bin/trplayer")
(t "rap"))
  "*Executable that plays realaudio"
:group 'emacspeak
:type 'string)

(defcustom emacspeak-realaudio-player-options 
(when (string= emacspeak-realaudio-player
"/usr/bin/trplayer")
(list "-l" "-i" "-b" "-c" ))
"*Options for realplayer."
:group 'emacspeak
:type 'string)

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

(defvar emacspeak-realaudio-buffer "*realaudio*"
"Name of realaudio process buffer")



(defun emacspeak-realaudio-play (resource &optional prompt-time)
  "Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable
emacspeak-realaudio-shortcuts-directory. "
  (interactive
   (list
    (let ((completion-ignore-case t)
          (emacspeak-speak-messages nil)
          (minibuffer-history emacspeak-realaudio-history))
      (emacspeak-pronounce-define-local-pronunciation
       emacspeak-realaudio-shortcuts-directory " shortcuts/ ")
      (expand-file-name
       (read-file-name "RealAudio resource: "
                       emacspeak-realaudio-shortcuts-directory
                       emacspeak-realaudio-last-url)))
    current-prefix-arg))
  (declare (special emacspeak-realaudio-player
                    emacspeak-realaudio-buffer 
                    emacspeak-realaudio-player-options
                    emacspeak-aumix-multichannel-capable-p
                    emacspeak-realaudio-process
                    emacspeak-realaudio-shortcuts-directory
                    emacspeak-realaudio-history
                    emacspeak-use-auditory-icons))
  (unless (string= resource (car emacspeak-realaudio-history))
    (pushnew resource emacspeak-realaudio-history))
  (when (get-buffer "*realaudio*")
    (kill-buffer emacspeak-realaudio-buffer))
  (let ((process-connection-type nil)
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
        (emacspeak-realaudio-mode)))
    (unless (eq 'run (process-status emacspeak-realaudio-process))
      (error "Failed to start RealAudio"))
    (set-process-sentinel emacspeak-realaudio-process 'emacspeak-realaudio-process-sentinel)
    (message "Launched audio stream")
    (setq emacspeak-realaudio-last-url resource)
    (when
        (and emacspeak-use-auditory-icons
             (not emacspeak-aumix-multichannel-capable-p)
             (not (emacspeak-using-midi-p)))
      (emacspeak-set-auditory-icon-player 'emacspeak-midi-icon))))

(defvar emacspeak-realaudio-dont-insist-on-ram-url t
  "*Set to nil if you want emacspeak to insist that realaudio
urls have a .ram or .rm extension.")

(defun emacspeak-realaudio-play-url-at-point (&optional prompt-time)
  "Play url under point as realaudio"
  (interactive "P")
  (declare (special emacspeak-realaudio-dont-insist-on-ram-url))
  (let ((url (w3-view-this-url 'no-show)))
    (cond
     ((or emacspeak-realaudio-dont-insist-on-ram-url
       (string-match ".rm?$" url)
       (string-match ".ram?$" url))
      (message "Playing Realaudio URL under point")
        (emacspeak-realaudio-play url prompt-time))
      (t (message "%s does not look like realaudio"
             url)))))

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

(defun emacspeak-realaudio-trplayer-command (char)
  "Execute TRPlayer command."
  (interactive "cTRPlayer Command:")
  (declare (special emacspeak-realaudio-process))
  (cond
   ((char-equal char ?\;)
                (emacspeak-realaudio-select-realaudio-buffer))
    (t 
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
                                       emacspeak-realaudio-process))))))))

(emacspeak-fix-interactive-command-if-necessary
 'emacspeak-realaudio-trplayer-command)

(defcustom emacspeak-realaudio-reset-auditory-display t 
  "Set this to T if you want the audio settings reset after
a realaudio sream is done playing."
:group 'emacspeak
:type 'boolean)
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
the audio desktop as \\[emacspeak-realaudio].
Alternatively,  switch to buffer *realaudo* if you
wish to issue many nvigation commands.  Note that buffer
*realaudio* uses a special major mode that provides the
various navigation commands via single keystrokes."

  (interactive "P")
  (declare (special emacspeak-realaudio-process))
  (cond
   ((and emacspeak-realaudio-process
         (eq 'run (process-status emacspeak-realaudio-process)))
    (if  (string-match "trplayer"
                       emacspeak-realaudio-player)
        (call-interactively 'emacspeak-realaudio-trplayer-command)
      (emacspeak-realaudio-stop)))
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
;;{{{ define a derived mode for realaudio interaction 

(define-derived-mode emacspeak-realaudio-mode fundamental-mode 
  "Realaudio Interaction"
  "Major mode for streaming audio. \n\n
\\{emacspeak-realaudio-mode-map}")

(declaim (special emacspeak-realaudio-mode-map))
(defvar emacspeak-realaudio-trplayer-keys
  (list ?p ?t ?s ?e ?l ?i
        ?< ?> ?. ?, ?0 ?9
        ?[ ?] ?{ ?})
  "Keys accepted by TRPlayer.")

(defun emacspeak-realaudio-trplayer-call-command ()
  "Call appropriate TRPlayer command."
  (interactive)
  (emacspeak-realaudio-trplayer-command last-input-char))

(loop for c in emacspeak-realaudio-trplayer-keys
      do
      (define-key emacspeak-realaudio-mode-map
        (format "%c" c)
        'emacspeak-realaudio-trplayer-call-command))

(defun emacspeak-realaudio-select-realaudio-buffer ()
  "Switch to realaudio buffer."
  (interactive)
  (pop-to-buffer "*realaudio*")
  (emacspeak-speak-mode-line))

(define-key emacspeak-realaudio-mode-map [left]
  'emacspeak-aumix-wave-decrease)
(define-key emacspeak-realaudio-mode-map [right] 'emacspeak-aumix-wave-increase)
;;}}}
(provide 'emacspeak-realaudio)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
