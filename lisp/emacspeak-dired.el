;;; emacspeak-dired.el --- Speech enable Dired Mode -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak extension to speech enable dired
;;; Keywords: Emacspeak, Dired, Spoken Output
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-19 16:09:43 -0700 (Sat, 19 Jul 2008) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  Introduction:

;;; Commentary:
;;; This module speech enables dired.
;;; It reduces the amount of speech you hear:
;;; Typically you hear the file names as you move through the dired buffer
;;; Voicification is used to indicate directories, marked files etc.

;;; Code:

;;}}}
;;{{{  required packages

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'dired)

;;}}}
;;{{{ Define personalities

(voice-setup-add-map
 '(
   (dired-broken-symlink 'voice-monotone-extra)
   (dired-set-id  voice-animate)
   (dired-special voice-lighten)
   (dired-header voice-smoothen)
   (dired-mark voice-lighten)
   (dired-perm-write voice-lighten-extra)
   (dired-marked voice-lighten)
   (dired-warning voice-animate-extra)
   (dired-directory voice-bolden-medium)
   (dired-symlink voice-animate-extra)
   (dired-ignored voice-lighten-extra)
   (dired-flagged voice-animate-extra)))

;;}}}
;;{{{  functions:

(defun emacspeak-dired-speak-line ()
  "Speak the dired line intelligently."
  (cl-declare (special emacspeak-speak-last-spoken-word-position))
  (let ((filename (dired-get-filename 'no-dir  t))
        (personality (dtk-get-style)))
    (cond
     (filename
      (dtk-speak (propertize filename 'personality personality))
      (setq emacspeak-speak-last-spoken-word-position (point)))
     (t (emacspeak-speak-line)))))

;;}}}
;;{{{  advice:

(defadvice dired-sort-toggle-or-edit (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced ad-do-it)
    (emacspeak-auditory-icon 'task-done)
    (emacspeak-speak-mode-line))
   (t ad-do-it))
  ad-return-value)

(defadvice dired-query (before emacspeak pre act comp)
  "Produce auditory icon."
  (emacspeak-auditory-icon 'ask-short-question))

(defadvice dired-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defun emacspeak-dired-initialize ()
  "Set up emacspeak dired."
  (emacspeak-dired-label-fields)
  (emacspeak-dired-setup-keys))
(cl-loop
 for  f in
 '(dired ido-dired
         dired-other-window dired-other-frame)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Set up emacspeak."
     (when (ems-interactive-p)
       (emacspeak-dired-initialize)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-mode-line)))))

(defadvice dired-find-file  (around  emacspeak pre act comp)
  "Produce an auditory icon."
  (cond
   ((ems-interactive-p)
    (let ((directory-p (file-directory-p (dired-get-filename t t))))
      ad-do-it
      (when directory-p (emacspeak-dired-label-fields))
      (emacspeak-speak-mode-line)
      (emacspeak-auditory-icon 'open-object)))
   (t ad-do-it))
  ad-return-value)

(cl-loop
 for  f in
 '(
   dired-next-subdir dired-prev-subdir
   dired-tree-up dired-tree-down dired-up-directory
   dired-next-marked-file dired-prev-marked-file
   dired-next-dirline dired-prev-dirline
   dired-jump
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the filename."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-dired-speak-line)))))

(cl-loop
 for f in
 '(dired-next-line dired-previous-line
                   dired-unmark-backward dired-maybe-insert-subdir)
 do
 (eval
  `(defadvice ,f  (after emacspeak pre act comp)
     "Speak the filename name."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-dired-speak-line)))))

;;; Producing auditory icons:
;;; These dired commands do some action that causes a state change:
;;; e.g. marking a file, and then change
;;; the current selection, ie
;;; move to the next line:
;;; We speak the line moved to, and indicate the state change
;;; with an auditory icon.

(defadvice dired-mark (after emacspeak pre act comp)
  "Produce an auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-dired-speak-line)))

(defadvice dired-flag-file-deletion (after emacspeak pre act comp)
  "Produce an auditory icon indicating that a file was marked for deletion."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-dired-speak-line)))

(defadvice dired-unmark (after emacspeak pre act comp)
  "Give speech feedback. Also provide an auditory icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-dired-speak-line)))

;;}}}
;;{{{  labeling fields in the dired buffer:

(defun emacspeak-dired-label-fields-on-current-line ()
  "Labels the fields on a dired line.
Assumes that `dired-listing-switches' contains  -l"
  (let ((start nil)
        (fields (list "permissions"
                      "links"
                      "owner"
                      "group"
                      "size"
                      "modified in"
                      "modified on"
                      "modified at"
                      "name")))
    (save-excursion
      (forward-line 0)
      (skip-syntax-forward " ")
      (while (and fields
                  (not (eolp)))
        (setq start (point))
        (skip-syntax-forward "^ ")
        (put-text-property start (point)
                           'field-name (car fields))
        (setq fields (cdr fields))
        (skip-syntax-forward " ")))))

(defun emacspeak-dired-label-fields ()
  "Labels the fields of the listing in the dired buffer.
Currently is a no-op  unless
unless `dired-listing-switches' contains -l"
  (interactive)
  (cl-declare (special dired-listing-switches))
  (when
      (save-match-data
        (string-match  "l" dired-listing-switches))
    (let ((read-only buffer-read-only))
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (save-excursion
              (goto-char (point-min))
              (dired-goto-next-nontrivial-file)
              (while (not (eobp))
                (emacspeak-dired-label-fields-on-current-line)
                (forward-line 1))))
        (setq buffer-read-only read-only)))))

;;}}}
;;{{{ Additional status speaking commands

(defvar emacspeak-dired-file-cmd-options "-b"
  "Options passed to Unix builtin `file' command.")

(defun emacspeak-dired-show-file-type (&optional file deref-symlinks)
  "Displays type of current file by running command file.
Like Emacs' built-in dired-show-file-type but allows user to customize
options passed to command `file'."
  (interactive (list (dired-get-filename t) current-prefix-arg))
  (cl-declare (special emacspeak-dired-file-cmd-options))
  (with-temp-buffer
    (if deref-symlinks
        (call-process "file" nil t t  "-l"
                      emacspeak-dired-file-cmd-options  file)
      (call-process "file" nil t t
                    emacspeak-dired-file-cmd-options file))
    (when (bolp)
      (backward-delete-char 1))
    (message (buffer-string))))

(defun emacspeak-dired-speak-header-line()
  "Speak the header line of the dired buffer. "
  (interactive)
  (emacspeak-auditory-icon 'section)
  (save-excursion (goto-char (point-min))
                  (forward-line 2)
                  (emacspeak-speak-region (point-min) (point))))

(defun emacspeak-dired-speak-file-size ()
  "Speak the size of the current file.
On a directory line, run du -s on the directory to speak its size."
  (interactive)
  (let ((filename (dired-get-filename nil t))
        (size 0))
    (cond
     ((and filename
           (file-directory-p filename))
      (emacspeak-auditory-icon 'progress)
      (emacspeak-shell-command (format "du -s \"%s\"" filename)))
     (filename
      (setq size (nth 7 (file-attributes filename)))
                                        ; check for ange-ftp
      (when (= size -1)
        (setq size
              (nth  4
                    (split-string (ems--this-line)))))
      (emacspeak-auditory-icon 'select-object)
      (message "File size %s"
               size))
     (t (message "No file on current line")))))

(defun emacspeak-dired-speak-file-modification-time ()
  "Speak modification time  of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Modified on : %s"
               (format-time-string
                emacspeak-speak-time-format-string
                (nth 5 (file-attributes filename)))))
     (t (message "No file on current line")))))

(defun emacspeak-dired-speak-file-access-time ()
  "Speak access time  of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Last accessed   on  %s"
               (format-time-string
                emacspeak-speak-time-format-string
                (nth 4 (file-attributes filename)))))
     (t (message "No file on current line")))))
(defun emacspeak-dired-speak-symlink-target ()
  "Speaks the target of the symlink on the current line."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (cond
       ((file-symlink-p filename)
        (message "Target is %s"
                 (file-chase-links filename)))
       (t (message "%s is not a symbolic link" filename))))
     (t (message "No file on current line")))))
(defun emacspeak-dired-speak-file-permissions ()
  "Speak the permissions of the current file."
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (cond
     (filename
      (emacspeak-auditory-icon 'select-object)
      (message "Permissions %s"
               (nth 8 (file-attributes filename))))
     (t (message "No file on current line")))))

;;}}}
;;{{{  keys
(cl-eval-when (load))

(defun emacspeak-dired-setup-keys ()
  "Add emacspeak keys to dired."
  (cl-declare (special dired-mode-map))
  (define-key dired-mode-map "F" 'emacspeak-wizards-find-file-as-root)
  (define-key dired-mode-map "E" 'emacspeak-dired-epub-eww)
  (define-key dired-mode-map (ems-kbd "C-j") 'emacspeak-dired-open-this-file)
  (define-key dired-mode-map (ems-kbd "C-RET") 'emacspeak-dired-open-this-file)
  (define-key dired-mode-map [C-return] 'emacspeak-dired-open-this-file)
  (define-key dired-mode-map "'" 'emacspeak-dired-show-file-type)
  (define-key  dired-mode-map "/" 'emacspeak-dired-speak-file-permissions)
  (define-key  dired-mode-map ";" 'emacspeak-dired-play-duration)
  (define-key  dired-mode-map (ems-kbd "M-;") 'emacspeak-m-player-add-to-dynamic)
  (define-key  dired-mode-map "a" 'emacspeak-dired-speak-file-access-time)
  (define-key dired-mode-map "c" 'emacspeak-dired-speak-file-modification-time)
  (define-key dired-mode-map "z" 'emacspeak-dired-speak-file-size)
  (define-key dired-mode-map "\M-t" 'emacspeak-dired-speak-symlink-target)
  (define-key dired-mode-map "\C-i" 'emacspeak-speak-next-field)
  (define-key dired-mode-map  "," 'emacspeak-dired-speak-header-line))
;;}}}
;;{{{ Advice locate:
(defun emacspeak-dired-open-this-directory ()
  "Open directory corresponding to file on current line."
  (interactive)
  (cl-assert (dired-get-filename) t "No file here.")
  (funcall-interactively #'dired (file-name-directory    (dired-get-filename))))

(cl-loop
 for f in
 '(locate locate-with-filter)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'open-object)))))
(load "locate" t t)

(cl-declaim (special locate-mode-map))
(define-key locate-mode-map  "j" 'emacspeak-dired-open-this-directory)
(define-key locate-mode-map  [C-return] 'emacspeak-dired-open-this-file)
;;}}}
;;{{{ Context-sensitive openers:

(defun emacspeak-dired-play-this-media ()
  "Plays media on current line."
  (emacspeak-m-player (dired-get-filename)))

(defun emacspeak-dired-play-this-playlist ()
  "Plays playlist on current line."
  (emacspeak-m-player (dired-get-filename) 'playlist))
(declare-function emacspeak-epub-eww "emacspeak-dired" t)

(defun emacspeak-dired-rpm-query-in-dired ()
  "Run rpm -qi on current dired entry."
  (interactive)
  (cl-declare (special major-mode))
  (unless (eq major-mode 'dired-mode)
    (error "This command should be used in dired mode."))
  (shell-command
   (format "rpm -qi ` rpm -qf %s`"
           (dired-get-filename 'no-location)))
  (other-window 1)
  (search-forward "Summary" nil t)
  (emacspeak-speak-line))

(defconst emacspeak-dired-opener-table
  `(("\\.epub$"  emacspeak-dired-epub-eww)
    ("\\.rpm$" emacspeak-dired-rpm-query-in-dired)
    ("\\.mid$"  emacspeak-dired-midi-play)
    ("\\.xhtml" emacspeak-dired-eww-open)
    ("\\.html" emacspeak-dired-eww-open)
    ("\\.htm" emacspeak-dired-eww-open)
    ("\\.pdf" emacspeak-dired-pdf-open)
    ("\\.md" emacspeak-dired-md-open)
    ("\\.csv" emacspeak-dired-csv-open)
    (,emacspeak-media-extensions emacspeak-dired-play-this-media)
    (,emacspeak-m-player-playlist-pattern emacspeak-dired-play-this-playlist))
  "Association of filename extension patterns to Emacspeak handlers.")

(defun emacspeak-dired-open-this-file  ()
  "Smart dired opener. Invokes appropriate Emacspeak handler on
current file in DirEd."
  (interactive)
  (let* ((f (dired-get-filename nil t))
         (ext (file-name-extension f))
         (handler nil))
    (unless f (error "No file here."))
    (unless ext (error "This entry has no extension."))
    (setq handler
          (cl-second
           (cl-find
            (format ".%s" ext)
            emacspeak-dired-opener-table
            :key #'car                  ; extract pattern from entry 
            :test #'(lambda (e pattern) (string-match  pattern e)))))
    (cond
     ((and handler (fboundp handler))
      (funcall-interactively handler))
     (t (call-interactively #'dired-find-file)))))

(defun emacspeak-dired-eww-open ()
  "Open HTML file on current dired line."
  (interactive)
  (eww-open-file (dired-get-filename)))
(declare-function markdown-preview "markdown-mode" (&optional output))
(defun emacspeak-dired-md-open ()
  "Preview markdown  file on current dired line."
  (interactive)
  (let ((buffer (find-file-noselect  (dired-get-filename))))
    (with-current-buffer buffer
      (markdown-preview))))

(declare-function emacspeak-wizards-pdf-open
                  "emacspeak-wizards" (filename &optional ask-pwd))


(defun emacspeak-dired-pdf-open ()
  "Open PDF file on current dired line."
  (interactive)
  (emacspeak-wizards-pdf-open (dired-get-filename current-prefix-arg)))

(defun emacspeak-dired-midi-play ()
  "Play midi  file on current dired line."
  (interactive)
  (emacspeak-wizards-midi-using-m-score
   (dired-get-filename current-prefix-arg)))

(defun emacspeak-dired-epub-eww ()
  "Open epub on current line  in EWW"
  (interactive)
  (emacspeak-epub-eww (shell-quote-argument(dired-get-filename)))
  (emacspeak-auditory-icon 'open-object))

(defun emacspeak-dired-csv-open ()
  "Open CSV file on current dired line."
  (interactive)
  (emacspeak-table-find-csv-file (dired-get-filename current-prefix-arg)))

;;}}}
;;{{{ Locate results as a play-list:


(defun emacspeak-locate-play-results-as-playlist (&optional shuffle)
  "Treat locate results as a play-list.
Optional interactive prefix arg shuffles playlist."
  (interactive "P")
  (cl-declare (special emacspeak-m-player-options))
  (cl-assert (eq major-mode 'locate-mode) t "Not in a locate buffer")
  (save-excursion
    (goto-char (point-min))
    (dired-next-line 3)
    (let* ((m3u (make-temp-file "locate-playlist" nil ".m3u"))
           (buff (find-file-noselect m3u))
           (results nil)
           (file (dired-file-name-at-point)))
      (while file
        (push file results)
        (dired-next-line 1)
        (setq file  (dired-file-name-at-point)))
      (setq results (nreverse results))
      (message "%s tracks matching " (length results))
      (with-current-buffer buff
        (cl-loop
         for f in results do
         (insert (format "%s\n" (expand-file-name f))))
        (save-buffer))
      (let ((emacspeak-m-player-options
             (if shuffle
                 (append emacspeak-m-player-options (list "-shuffle"))
               emacspeak-m-player-options)))
        (emacspeak-m-player  m3u 'play-list)))))

;;}}}
;;{{{ Play Duration Using Soxi:

(defun emacspeak-dired-play-duration ()
  "Speak duration of MP3 files.
If on a file, speak its duration.
If on a directory, speak the total duration of all mp3 files under
  that directory."
  (interactive)
  (cl-assert (executable-find "soxi")
             t "This command needs soxi installed.")
  (cl-assert (eq major-mode 'dired-mode)
             t "This command is only available in dired buffers.")
  (let* ((f   (dired-get-filename)))
    (cond
     ((and (not (file-directory-p f))
           (string-match "\\.mp3$" f))
      (message "%s %s"
               (shell-command-to-string (format "soxi -d '%s'" f))
               (file-name-base f)))
     ((file-directory-p f)
      (message "%s in %s"
               (shell-command-to-string
                (format
                 "find %s -name '*.mp3' -print0 | xargs -0 soxi -Td 2>/dev/null"
                 (shell-quote-argument f)))
               (file-name-base f)))
     (t (message "No mp3  on current line.")))))

;;}}}
;;{{{ Open Downloads:

(defun emacspeak-dired-downloads ()
  "Open Downloads directory."
  (interactive)
  (funcall-interactively 'dired (expand-file-name "~/Downloads") "-alt"))

;;}}}
(provide 'emacspeak-dired)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
