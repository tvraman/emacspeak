;;; soundscape.el -- Soundscapes for The Emacspeak Desktop
;;; Description:  Soundscapes Using Boodler
;;; Keywords: Emacspeak,  Audio Desktop Soundscapes
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Soundscapes @url{https://en.wikipedia.org/wiki/Soundscape}
;;; define an acoustic environment.
;;; Boodler  at @url{http://boodler.org} is a
;;; Python-based SoundScape generator.
;;; To use this module, first install boodler.
;;; Then install the soundscape packages (*.boop) files available
;;; at @url{http://boodler.org/lib}.
;;; Make sure boodler works and produces audio in your environment.
;;; finally install the Boodler packages  from
;;; emacspeak/scapes from the Emacspeak GitHub repository by running
;;; @code{cd emacspeak/scapes; make }
;;;
;;; When  boodler is set up and all packages installed, copy
;;; file emacspeak/scapes/soundscapes  to ~/.boodler/Collection.
;;; The above file lists all installed SoundScapes.
;;; Directory emacspeak/scapes also contains
;;; additional Boodler Agents and SoundScapes  that
;;; I have created for use with Emacspeak.
;;;
;;;  Module soundscape.el  defines Emacs conveniences for running
;;; Soundscapes. Main Entry Points:
;;;
;;;@itemize
;;; @item M-x soundscape-toggle -- Enables or
;;; disables automatic SoundScapes.
;;; @item M-x soundscape --
;;; runs a named SoundScape
;;; @item M-x soundscape-stop -- Stops a specified running Soundscape.
;;; @item M-x soundscape-kill -- Kills all running Soundscapes.
;;;@end itemize
;;;
;;; When automatic Soundscapes are enabled, SoundScapes are
;;;  started and stopped based on the current major
;;; mode. Active Soundscape  are displayed as part of the minor-mode-alist.
;;; Command emacspeak-speakc-minor-mode-line can be used to have this spoken.
;;;
;;; Thus, SoundScapes can be thought of as reflecting the
;;; @emph{mood} of the current @emph{mode}.
;;; This package defines a single @var{soundscape-default-theme}
;;; that is loaded using @code{(soundscape-load soundscape-default-theme)}.
;;; Emacs modes that provide similar functionality e.g.,
;;; communication == email, IM, ... map to  the same @emph{mood}.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'subr-x)

;;}}}
;;{{{ Configuration:

(defconst soundscape-player (executable-find "boodler")
  "Soundscape player. Looks for installed boodler.")

(defconst soundscape-mgr (executable-find "boodle-mgr")
  "Soundscape manager. Looks for installed boodler.")

(defgroup soundscape nil
  "Soundscapes For Emacs."
  :group 'applications)

(defcustom soundscape-data
  (expand-file-name "~/.boodler/Collection")
  "Soundscape data directory."
  :type 'directory
  :group 'soundscape)

(defconst soundscape-list (expand-file-name "soundscapes"  soundscape-data)
  "Soundscape player. Looks for installed boodler.")

(defvar soundscape--catalog nil
  "Catalog of installed soundscapes keyed by agent name.")

(defvar soundscape--scapes nil
  "Cache of currently running scapes.")

;;;###autoload
(defcustom soundscape-manager-options
  '("-o" "alsa"
    "-m" "0.5")
  "User customizable options list passed to boodler.
Defaults specify alsa as the output and set master volume to 0.5"
  :group 'soundscape
  :type '(repeat
          (string :tag "Option: ")))

;;}}}
;;{{{ Catalog:
(defvar soundscape--missing-packages nil
  "Records missing packages when building up the catalog.")

(defun soundscape-catalog-add-entry()
  "Add catalog entry from current line."
  (declare (special soundscape--missing-packages))
  (let ((name nil)
        (scape nil)
        (package nil)
        (fields nil))
    (setq scape
          (buffer-substring (line-beginning-position) (line-end-position)))
    (setq fields (split-string scape "/"))
    (setq  package (cl-first fields) name (cl-second fields))
    (cond
     ((and name scape
           (file-exists-p   (expand-file-name package soundscape-data)))
      (push (cons name scape) soundscape--catalog))
     (t (push scape soundscape--missing-packages)))))
(defun soundscape-catalog (&optional refresh)
  "Return catalog of installed Soundscapes, initialize if necessary."
  (declare (special soundscape--catalog soundscape-list))
  (when (null (file-exists-p soundscape-list)) (error "Catalog missing."))
  (cond
   ((and soundscape--catalog (null refresh)) soundscape--catalog)
   (t
    (with-temp-buffer
      (insert-file-contents soundscape-list)
      (goto-char (point-min))
      (while (not (eobp))
        (soundscape-catalog-add-entry)
        (forward-line 1)))
    soundscape--catalog)))

(defsubst soundscape-lookup-name (name)
  "Return package/agent for this name.
Default is to return NullAgent if name not found."
  (or
   (cdr (assoc name (soundscape-catalog)))
   "()"))

(defsubst soundscape-lookup-scape (scape)
  "Return name for this package/agent."
  (car (rassoc scape (soundscape-catalog))))

(defun soundscape-read ()
  "Read name of Soundscape with completion."
  (let ((completion-ignore-case t))
    (soundscape-lookup-name
     (completing-read "Soundscape: " (soundscape-catalog)))))

;;}}}
;;{{{ Running:

(defvar soundscape-processes (make-hash-table :test #'equal)
  "Hash table of running Soundscapes indexed by Soundscape path.")

;;;###autoload
(defun soundscape (scape)
  "Play soundscape."
  (interactive (list (soundscape-read)))
  (declare (special soundscape-processes
                    soundscape-manager-options))
  (let ((process-connection-type  nil)
        (proc (gethash scape soundscape-processes)))
    (unless (process-live-p proc)
      (setq proc
            (apply
             #'start-process
             "Boodler" nil soundscape-player
             `(,@soundscape-manager-options ,scape)))
      (when (process-live-p proc) (puthash scape proc soundscape-processes)))))

(defun soundscape-stop (scape)
  "Stop running Soundscape."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Stop: " (hash-table-keys soundscape-processes)))))
  (declare (special soundscape-processes))
  (let ((proc (gethash scape soundscape-processes)))
    (when (process-live-p proc) (delete-process proc))
    (remhash  scape soundscape-processes)))

(defun soundscape-kill ()
  "Stop all running soundscapes."
  (interactive)
  (declare (special soundscape-processes))
  (mapc  #'soundscape-stop (hash-table-keys soundscape-processes))
  (message "Stopped all soundscapes."))

(defsubst soundscape-running-p (scape)
  "Predicate to check if soundscape is running."
  (declare (special soundscape-processes))
  (process-live-p (gethash  scape soundscape-processes)))

(defun soundscape-current ()
  "Return names of currently running scapes."
  (declare (special soundscape--scapes))
  (mapconcat #'soundscape-lookup-scape soundscape--scapes " "))

;;}}}
;;{{{ Modes->SoundScapes:

(defvar soundscape-mode-table (make-hash-table :test #'eq)
  "Maps mode-names to associated Soundscapes.")

(defsubst  soundscape-for-mode (mode)
  "Return associated soundscape for this mode if any."
  (declare (special soundscape-mode-table))
  (let ((result nil))
    (while mode
      (cl-pushnew (gethash mode soundscape-mode-table) result)
      (setq mode (get mode 'derived-mode-parent)))
    (delq nil result)))

(defsubst  soundscape-map-mode (mode scape)
  "Associate soundscape for this mode."
  (declare (special soundscape-mode-table))
  (when mode
    (puthash mode scape soundscape-mode-table)))

;;}}}
;;{{{ Default mapping:

(defconst soundscape-web-modes
  '(w3-mode eww-mode)
  "List of mode-names that get the Web  mood.")
(defconst soundscape-vc-modes
  '(magit-mode vc-mode)
  "Version control modes.")

(defconst soundscape-communication-modes
  '(
    message-mode gnus-summary-mode gnus-article-mode gnus-group-mode
                 mspools-mode vm-presentation-mode vm-mode mail-mode
                 twittering-mode jabber-roster-mode jabber-chat-mode erc-mode)
  "List of mode names that get the Communication mood.")

(defconst soundscape-help-modes
  '(
    Info-mode  help-mode  Man-mode
               Custom-mode messages-buffer-mode)
  "List of mode names that get the Help mood.")

;;;###autoload
(defun soundscape-load-theme (theme)
  "Sets up automatic Soundscape mappings based on theme.
See  \\{soundscape-default-theme} for details."
  (soundscape-catalog)
  (cl-loop
   for pair in theme do
   (let ((scape (soundscape-lookup-name (cl-first pair)))
         (modes (cl-second pair)))
     (cond
      (scape (mapc #'(lambda (m) (soundscape-map-mode m scape)) modes))
      (t (message "Theme: <%s> not found." (cl-first pair)))))))

(defvar soundscape-default-theme
  `(
    ("()" nil)
    ("BirdChorus" nil)
    ("BirdSongs" (shell-mode term-mode))
    ("BirdSongs" nil)
    ("BuddhaLoop" (special-mode))
    ("Cavern" (prog-mode))
    ("ChangingLoops" (emacspeak-m-player-mode))
    ("Drip" ,soundscape-communication-modes)
    ("LightWind" (comint-mode elfeed-search-mode))
    ("ManyMockingBirds" nil)
    ("MockingBirds" nil)
    ("Nightscape" nil)
    ("NoStormYet"  (fundamental-mode))
    ("RainForever" ,soundscape-help-modes)
    ("RainSounds" ,soundscape-vc-modes)
    ("Bonfire" (calendar-mode diary-mode))
    ("Still" (text-mode))
    ("SurfWaves"  ,soundscape-web-modes)
    ("TonkSpace" (tabulated-list-mode))
    ("WaterFlow"  (dired-mode))
    )
  "*Specifies default map.
Map is a list of lists, where the first element of each sublist
is a Soundscape name, and the second element is a list of
Soundscape names. Soundscape at position 0 (the first entry in
this list) must be the NullAgent written as (). ")

(soundscape-load-theme soundscape-default-theme)

(defun soundscape-update-mood ()
  "Update mood/scape mapping for current major mode.
This updated mapping is not persisted."
  (interactive)
  (let* ((completion-ignore-case t)
         (scape
          (soundscape-lookup-name
           (completing-read "Scape:" (mapcar 'car soundscape-default-theme)))))
    (soundscape-map-mode major-mode scape)
    (soundscape-sync major-mode)
    (message "Now using %s for %s" scape major-mode)))

;;}}}
;;{{{ Soundscape Remote Control

(defvar soundscape--remote
  (make-temp-name "/tmp/soundscape")
  "Name of Unix Domain socket used to control Soundscape.")

(defun soundscape-sentinel (proc state)
  "Delete remote control end point on exit."
  (declare (special soundscape--remote))
  (unless (process-live-p  proc)
    (when (file-exists-p soundscape--remote)
      (delete-file soundscape--remote))))

(defun soundscape-kill-emacs-hook ()
  "Clean up remote control end-points."
  (declare (special soundscape--remote))
  (when (file-exists-p soundscape--remote) (delete-file soundscape--remote)))

(add-hook 'kill-emacs-hook #'soundscape-kill-emacs-hook)

(defvar soundscape-listener-process nil
  "Handle to Soundscape listener.")

(defvar soundscape-remote-control nil
  "Handle to remote control.")

(defun soundscape-lookup-position (name)
  "Return position in soundscape-default-theme."
  (declare (special soundscape-default-theme))
  (format "%s"
          (cl-position-if
           #'(lambda (pair)
               (string= name  (car pair)))
           soundscape-default-theme)))

;;;###autoload
(defun soundscape-init ()
  "Initialize Soundscape module."
  (soundscape-catalog)
  (soundscape-listener)
  (unless (member '(soundscape--auto (:eval (soundscape-current)))
                  minor-mode-alist)
    (push   '(soundscape--auto (:eval (soundscape-current))) minor-mode-alist)))

;;;###autoload
(defun soundscape-listener  (&optional restart)
  "Start  a Soundscape listener.
Listener is loaded with all Soundscapes defined in `soundscape-default-theme' .
Optional interactive prefix arg restarts the listener if already running."
  (interactive "P")
  (declare (special soundscape-listener-process soundscape--remote
                    soundscape-manager-options
                    soundscape-remote-control soundscape-default-theme))
  (let ((process-connection-type nil))
    (cond
     ((or restart (not (process-live-p soundscape-listener-process)))
      (setq
       soundscape-listener-process
       (apply
        #'start-process
        "SoundscapeListener" " *Soundscapes*"  soundscape-player
        `(,@soundscape-manager-options
          "--listen" "--port" ,soundscape--remote
          "org.emacspeak.listen/SoundscapePanel"
          ,@(mapcar #'(lambda (m) (soundscape-lookup-name (car m)))
                    soundscape-default-theme))))
      (set-process-sentinel soundscape-listener-process #'soundscape-sentinel)
      (accept-process-output))
     (t soundscape-listener-process))))

(defun soundscape-listener-shutdown ()
  "Shutdown listener."
  (interactive)
  (declare (special soundscape-listener-process soundscape-remote-control
                    soundscape--scapes))
  (setq soundscape--scapes nil)
  (when (process-live-p soundscape-listener-process)
    (delete-process soundscape-listener-process))
  (when (process-live-p soundscape-remote-control)
    (delete-process soundscape-remote-control))
  (when (file-exists-p soundscape--remote)
    (delete-file soundscape--remote)))

(defun soundscape-remote (names)
  "Activate scapes named names."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (result nil)
          (name " "))
      (while (> (length name) 0)
        (setq name
              (completing-read "Soundscape Name:"
                               (mapcar #'car soundscape-default-theme)))
        (when (> (length name) 0) (push name  result)))
      result)))
  (declare (special soundscape-remote-nc))
  (unless (process-live-p soundscape-listener-process) (soundscape-listener))
  (unless (process-live-p soundscape-remote-control)
    (when (and (process-live-p soundscape-listener-process)
               (file-exists-p soundscape--remote))
      (setq soundscape-remote-control
            (make-network-process  :name "nc-connect"
                                   :family 'local
                                   :remote soundscape--remote))))
  (when (process-live-p soundscape-remote-control)
    (process-send-string
     soundscape-remote-control
     (format "soundscape %s\n"
             (mapconcat #'soundscape-lookup-position names " ")))))

;;}}}
;;{{{ Automatic soundscapes:

;;;###autoload
(defvar soundscape--auto nil
  "Record if automatic soundscapes are on.
Do not set this by hand, use command \\[soundscape-toggle].")

(defun soundscape-sync (mode &optional force)
  "Activate  Soundscapes for  this mode.
Optional interactive prefix arg `force' skips optimization checks."
  (declare (special soundscape--scapes))
  (let ((scapes (soundscape-for-mode mode)))
    (when (or force (not   (equal scapes soundscape--scapes)))
      (setq soundscape--scapes scapes)
      (soundscape-remote (mapcar #'soundscape-lookup-scape scapes)))))

(defvar soundscape--last-mode  nil
  "Caches last seen mode.")

(defun soundscape-update ()
  "Function to update Soundscape automatically."
  (declare (special  soundscape--last-mode))
  (unless
      (or
       (eq major-mode soundscape--last-mode)
       (eq 'minibuffer-inactive-mode major-mode)
       (string-match "temp" (buffer-name)))
    (setq soundscape--last-mode major-mode)
    (soundscape-sync major-mode)))

;;; Advice on select-window, force-mode-line-update etc fire too often.
;;; Ditto with buffer-list-update-hook
;;; Running on an idle timer can be  less responsive
;;;  soundscape-delay (default is 0.5)
;;; but  triggers fewer spurious changes than running on advice.

;;}}}
;;{{{ SoundScape Toggle:
(defsubst soundscape-quiet ()
  "Activate NullAgent."
  (declare (special soundscape-remote-control))
  (when (process-live-p soundscape-remote-control)
    (process-send-string soundscape-remote-control "soundscape 0\n")))

;;;###autoload
(defcustom soundscape-idle-delay 0.5
  "Number of seconds of idle time
before soundscapes are synchronized with current mode."
  :type 'float
  :group 'soundscape)

;;;###autoload
(defun soundscape-toggle ()
  "Toggle automatic SoundScapes.
When turned on, Soundscapes are automatically run based on current major mode.
Run command \\[soundscape-theme] to see the default mode->mood mapping."
  (interactive)
  (declare (special soundscape--auto soundscape--scapes
                    soundscape-idle-delay soundscape--last-mode))
  (cond
   (soundscape--auto
    (cancel-timer soundscape--auto)
    (setq soundscape--auto nil
          soundscape--scapes nil
          soundscape--last-mode nil)
    (soundscape-quiet))
   (t
    (soundscape-init)
    (setq soundscape--auto
          (run-with-idle-timer   soundscape-idle-delay t #'soundscape-update))
    (soundscape-sync major-mode)
    (when (called-interactively-p 'interactive)
      (message "Automatic Soundscapes are now %s"
               (if soundscape--auto "on" "off"))))))

(defun soundscape-restart ()
  "Restart Soundscape  environment."
  (interactive)
  (declare (special soundscape--last-mode  soundscape--scapes soundscape--auto))
  (setq soundscape--scapes nil
        soundscape--last-mode nil)
  (when soundscape--auto (soundscape-toggle)
        (soundscape-listener-shutdown))
  (soundscape-toggle)
  (sit-for 0.1)
  (soundscape-sync major-mode 'force))



;;}}}
;;{{{ Display Theme:

(defun soundscape-theme ()
  "Shows default theme in a special buffer."
  (interactive)
  (declare (special soundscape-default-theme soundscape-base))
  (let ((buffer (get-buffer-create "*Soundscape Theme*"))
        (inhibit-read-only  t))
    (with-current-buffer buffer
      (erase-buffer)
      (cl-loop
       for entry in soundscape-default-theme
       do
       (let ((name  (cl-first entry))
             (modes (cl-second entry)))
         (when (and name modes )
           (insert
            (format "%s:\t%s\n"
                    name
                    (mapconcat
                     #'(lambda (s)
                         (substring (symbol-name s) 0 -5))
                     modes " "))))))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (special-mode)
      (setq default-directory
            (expand-file-name soundscape-data)))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
(provide 'soundscape)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
