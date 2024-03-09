;;; emacspeak-amark.el --- BookMarks For Audio -*- lexical-binding: t; -*-
;; $Id: emacspeak-amarks.el 5798 2008-08-22 17:35:01Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description: Bookmarks for audio content like mp3
;; Keywords:emacspeak, audio interface to emacs MP3
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;;
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
;; Copyright (c) 1996 by T. V. Raman
;; All Rights Reserved.
;;
;; This file is not part of GNU Emacs, but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



;;; Commentary:

;; Structure emacspeak-amark holds a bookmark into an mp3 file
;; path:  filename containing  marked
;; name: Bookmark tag
;; Position: time offset from start

;;  This library will be used from emacspeak-m-player to set and jump
;; to bookmarks. Amarks are stored in a .amarks.am file in the working
;; directory.  It also provides a simple AMark Browser to use from a
;; directory containing mp3 files where Amarks have been created --
;; see @command{emacspeak-amarks-browse}.
;;  @command{emacspeak-amarks-bookshelf} brings up a @emph{AmarksBookshelf}
;; that can be used to  browse available Amark files.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'dired)

;;;  Structure:

(cl-defstruct
    emacspeak-amark
  "AMark: Holds name,  a file, and a time-position."
  path                                   ; filename
  name                                   ; Bookmark name
  position                               ; Offset in ms from start
  )

;;;  AMark List:

(defvar-local emacspeak-amark-list nil
  "List of buffer-local AMarks. ")

;;;  AMark Functions:

(defsubst emacspeak-amark-names ()
  "Return list of  amark names."
  (cl-declare (special emacspeak-amark-list))
  (cl-loop for a in emacspeak-amark-list collect (emacspeak-amark-name a)))

(defun emacspeak-amark-find (name)
  "Return matching AMark if found in buffer-local AMark list."
  (interactive (list (completing-read "Name: " (emacspeak-amark-names))))
  (cl-declare (special emacspeak-amark-list))
  (cl-find name emacspeak-amark-list
           :test #'string= :key #'emacspeak-amark-name))

(defun emacspeak-amark-add (path name position)
  "Add an AMark to the buffer local list of AMarks. AMarks are
bookmarks in audio content. If there is an existing amark of the
given name, it is updated with path and position."
  (interactive "fPath\nsName\nnPosition")
  (cl-declare (special emacspeak-amark-list))
  (let ((amark (emacspeak-amark-find name)))
    (when (and path (not (zerop (length path))))
      (cond
       (amark                             ; exists, reposition
        (setf (emacspeak-amark-path amark) path
              (emacspeak-amark-position amark) position))
       (t
        (push
         (make-emacspeak-amark :path path :name name :position position)
         emacspeak-amark-list))))))

(defvar emacspeak-amark-file ".amarks.am"
  "Name of file used to save AMarks.")

;;;###autoload
(defun emacspeak-amark-save ()
  "Save buffer-local AMarks in  currently playing directory."
  (interactive)
  (cl-declare (special  emacspeak-amark-file))
  (let ((l  emacspeak-amark-list)
        (print-length nil)
        (buff (find-file-noselect (expand-file-name emacspeak-amark-file))))
    (with-current-buffer buff
      (set (make-local-variable 'backup-inhibited) t)
      (setq buffer-undo-list  t)
      (erase-buffer)
      (prin1 l (current-buffer))
      (save-buffer)
      (message "Saved AMarks in %s" (buffer-file-name))
      (kill-buffer buff)
      (emacspeak-icon 'save-object))))

;;;###autoload
(defun emacspeak-amark-load ()
  "Load AMarks file from  current  media directory."
  (cl-declare (special emacspeak-amark-list emacspeak-amark-file
                       emacspeak-m-player-process))
  (let* ((buff nil)
         (find-file-hook nil)
         (def default-directory)
         (dir
          (when (process-live-p emacspeak-m-player-process)
            (with-current-buffer
                (process-buffer emacspeak-m-player-process) def)))
         (file
          (cond 
           ((file-exists-p (expand-file-name emacspeak-amark-file def))
            (expand-file-name emacspeak-amark-file def))
           (t  (expand-file-name emacspeak-amark-file dir))))
         (l nil ))
    (when (file-exists-p file)
      (setq buff (find-file-noselect file))
      (with-current-buffer buff
        (goto-char (point-min))
        (setq l (read buff))
        (kill-buffer buff)))
    ;;  sort and clean up stale marks 
    (setq
     emacspeak-amark-list
     (sort
      (cl-remove-if-not
       #'(lambda (f)
           (file-exists-p
            (expand-file-name f (file-name-directory file))))
       l
       :key #'emacspeak-amark-path)
      #'(lambda (a b) ;; predicate for sort
          (string-lessp
           (emacspeak-amark-name a) (emacspeak-amark-name b )))))))

(defun emacspeak-amark-file-load ()
  "Open .amark.el on current line in AMark Browser"
  (interactive)
  (cd (file-name-directory (dired-get-filename)))
  (funcall-interactively #'emacspeak-amark-browse))

(defun emacspeak-amark-delete (amark)
  "Delete Amark and save."
  (cl-declare (special emacspeak-amark-list))
  (setq emacspeak-amark-list (remove amark emacspeak-amark-list))
  (emacspeak-icon 'delete-object)
  (emacspeak-amark-save)
  (emacspeak-amark-browse)
  (message "Updated amarks"))

(declare-function emacspeak-m-player-seek-absolute "emacspeak-m-player" (pos))

(defun emacspeak-amark-play (amark)
  "Play amark using m-player."
  (cl-declare (special emacspeak-m-player-options))
  (let ((f (expand-file-name (emacspeak-amark-path  amark) default-directory))
        (emacspeak-m-player-options
         (append
          emacspeak-m-player-options
          `("-ss" ,(emacspeak-amark-position amark)))))
    (cl-assert (file-exists-p f) t "File does not exist:" )
    (emacspeak-m-player f)
    (message "Playing %s from %s"
             (file-name-base f) (emacspeak-amark-position amark))))

;;; Amark Mode:

(define-derived-mode emacspeak-amark-mode special-mode
  "AMark Browser"
  "A light-weight mode for the `*Emacspeak Amark Browser*'.
 1. Provides buttons for opening and removing AMarks.
 2. Enables org integration via command
 `org-store-link' bound to \\[org-store-link].
 3. Stored links can be inserted into org files in the same directory
via command `org-insert-link' bound to \\[org-insert-link]."
  (setq header-line-format "AMark Browser")
  t)

(cl-loop
 for b   in
 '(
   ("C-c i" org-insert-link)
   ("C-c l" org-store-link))
 do
 (emacspeak-keymap-update emacspeak-amark-mode-map b))

;;; Browse Amarks:

(defun emacspeak-amark-list-play ()
  "Play amark list as a playlist.
Maps command \\[emacspeak-m-player] across elements of the amarks
  list.  Pressing `y' as the current item is playing skips to the
  next item; this `y/n' prompt is produced by
  \\[emacspeak-m-player] as is usual when that command is called
  while media is already playing. Here, attempting to play the next
  item while the current item is playing produces the prompt."
  (interactive)
  (cl-declare (special emacspeak-amark-list))
  (when (and emacspeak-amark-list (listp emacspeak-amark-list))
    (mapc #'emacspeak-amark-play emacspeak-amark-list)))

;;;###autoload
(defun emacspeak-amark-browse ()
  "Browse   amarks  in current directory using `emacspeak-amark-mode'."
  (interactive)
  (cl-declare (special emacspeak-amark-list))
  (let ((amarks (or (emacspeak-amark-load) (error "No Amarks here")))
        (buff (get-buffer-create "*Amarks Browser"))
        (inhibit-read-only t))
    (with-current-buffer buff
      (emacspeak-amark-mode)
      (setq emacspeak-amark-list amarks)
      (local-set-key "p" 'backward-button)
      (local-set-key "." 'emacspeak-amark-list-play)
      (local-set-key "n" 'forward-button)
      (erase-buffer)
      (setq buffer-undo-list  t)
      (cl-loop
       for m in amarks do
       (insert-text-button
        (format "%s\t" (emacspeak-amark-name m))
        'mark m
        'action #'(lambda (b) (emacspeak-amark-play (button-get b 'mark))))
       (insert (format "%s\t" (emacspeak-amark-path m)))
       (insert-text-button
        "Remove\n" 'mark m
        'action #'(lambda (b) (emacspeak-amark-delete (button-get b 'mark)))))
      (emacspeak-speak-load-directory-settings)
      (goto-char (point-min)))
    (funcall-interactively #'switch-to-buffer buff)))

;;;###autoload
(defun emacspeak-amark-bookshelf(&optional pattern)
  "Open a locate buffer with all .amarks.am files.
Optional interactive prefix arg prompts for a pattern that is
used to filter the amarks files to show.  Use
\\[emacspeak-dired-open-this-file] to open the AMark Browser on
current file."
  (interactive "P")
  (cl-declare (special emacspeak-amark-file locate-command
                       locate-make-command-line))
  (when pattern (setq pattern (read-from-minibuffer "Filter Pattern:")))
  (let ((case-fold-search t)
        (locate-make-command-line
         #'(lambda (s)
             (list
              locate-command "-i" "-e" "--regexp" s))))
    (cond
     (pattern 
      (locate-with-filter
       (mapconcat
        #'identity
        (split-string pattern)
        "[ '/\"_.,-]")
       emacspeak-amark-file))
     (t (funcall-interactively #'locate emacspeak-amark-file))))
  (rename-buffer "AMark Bookshelf" 'unique)
  (emacspeak-speak-line)
  (emacspeak-icon 'open-object))

(provide  'emacspeak-amark)

