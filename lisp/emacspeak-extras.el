;;; emacspeak-extras.el --- Speech-enable EXTRAS  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable EXTRAS An Emacs Interface to extras
;; Keywords: Emacspeak,  Audio Desktop extras
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;; MERCHANTABILITY or FITNEXTRAS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; Infrequently used wizards archived for posterity.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'term)

;;}}}
;;{{{ Keymaps <-> Org (text) Files :

;; This makes it easy to consolidate personal bindings across machines.
;; It also protects against custom losing settings due to Custom accidents.
;; 

(defun emacspeak-wizards-bindings-from-org (variable filename)
  "Load bindings from a specified file."
  (interactive "vVariable: \nfFilename: ")
  (let ((bindings nil))
    (with-temp-buffer
      "org-to-map"
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((fields
               (split-string
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                " " 'omit-nulls)))
          (push
           (list (cl-first fields) (intern (cl-second fields)))
           bindings))
        (forward-line 1)))
    (setq bindings (nreverse (copy-sequence bindings)))
    (set variable  bindings)
    (customize-save-variable variable bindings)))

(defun emacspeak-wizards-bindings-to-org (variable filename)
  "Persists mapping to org file."
  (interactive "vVariable: \nfFilename: ")
  (let ((buffer (find-file-noselect  filename)))
    (with-current-buffer
        buffer
      (goto-char (point-max))
      (cl-loop
       for binding  in (symbol-value variable) do
       (insert (format "%s %s\n" (cl-first binding) (cl-second binding))))
      (save-buffer buffer))
    (switch-to-buffer buffer)))

;;}}}
;;{{{Midi Playback Using MuseScore ==mscore:

(defvar emacspeak-wizards-media-pipe
  (expand-file-name "pipe.flac" emacspeak-user-directory)
  "Named socket for piped media streams.")

;;;###autoload
(defun emacspeak-wizards-midi-using-m-score (midi-file)
  "Play midi file using mscore from musescore package."
  (interactive "fMidi File:")
  (cl-declare (special emacspeak-wizards-media-pipe))
  (cl-assert (executable-find "mscore") t "Install mscore first")
  (or (file-exists-p emacspeak-wizards-media-pipe)
      (shell-command (format "mknod %s p"
                             emacspeak-wizards-media-pipe)))
  (cl-assert (file-exists-p emacspeak-wizards-media-pipe) t
             "Error creating named socket")
  (emacspeak-m-player emacspeak-wizards-media-pipe)
  (message "converting %s to audio" midi-file)
  (shell-command
   (format "%s -o %s %s &"
           (executable-find "mscore")
           emacspeak-wizards-media-pipe midi-file)))

;;}}}
;;{{{ Braille

;;;###autoload
(defun emacspeak-wizards-braille (s)
  "Insert Braille string at point."
  (interactive "sBraille: ")
  (require 'toy-braille)
  (insert (get-toy-braille-string s))
  (emacspeak-auditory-icon 'yank-object)
  (message "Brailled %s" s))

;;}}}
;;{{{ Add autoload cookies:

(defvar emacspeak-autoload-cookie-pattern
  ";;;;###autoload"
  "autoload cookie pattern.")

(defun emacspeak-wizards-add-autoload-cookies (&optional f)
  "Add autoload cookies to file f.
Default is to add autoload cookies to current file."
  (interactive)
  (cl-declare (special emacspeak-autoload-cookie-pattern))
  (or f (setq f (buffer-file-name)))
  (let ((buffer (find-file-noselect f))
        (count 0))
    (with-current-buffer buffer
      (goto-char (point-min))
      (unless (eq major-mode 'emacs-lisp-mode)
        (error "Not an Emacs Lisp file."))
      (condition-case nil
          (while (not (eobp))
            (re-search-forward "^ *(interactive")
            (beginning-of-defun)
            (forward-line -1)
            (unless (looking-at emacspeak-autoload-cookie-pattern)
              (cl-incf count)
              (forward-line 1)
              (beginning-of-line)
              (insert
               (format "%s\n" emacspeak-autoload-cookie-pattern)))
            (end-of-defun))
        (error "Added %d autoload cookies." count)))))

;;}}}
;;{{{ voice sample

(defsubst voice-setup-read-personality (&optional prompt)
  "Read name of a pre-defined personality using completion."
  (read (completing-read (or prompt "Personality: ")
                         (voice-setup-defined-voices))))
;;;###autoload
(defun emacspeak-wizards-voice-sampler (personality)
  "Read a personality  and apply it to the current line."
  (interactive (list (voice-setup-read-personality)))
  (put-text-property (line-beginning-position) (line-end-position)
                     'personality personality)
  (emacspeak-speak-line))

;;;###autoload
(defun emacspeak-wizards-generate-voice-sampler (step)
  "Generate a buffer that shows a sample line in all the ACSS settings
for the current voice family."
  (interactive "nStep:")
  (let ((buffer (get-buffer-create "*Voice Sampler*"))
        (voice nil))
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (cl-loop
       for a from 0 to 9 by step do
       (cl-loop
        for p from 0 to 9 by step do
        (cl-loop
         for s from 0 to 9 by step do
         (cl-loop
          for r from 0 to 9 by step do
          (setq voice (voice-setup-acss-from-style
                       (list nil a p s r)))
          (insert
           (format
            " Aural CSS average-pitch %s pitch-range %s stress %s richness %s"
            a p s r))
          (put-text-property (line-beginning-position)
                             (line-end-position)
                             'personality voice)
          (end-of-line)
          (insert "\n"))))))
    (switch-to-buffer buffer)
    (goto-char (point-min))))
;;;###autoload
(defun voice-setup-defined-voices ()
  "Return list of voices defined via defvoice"
  (cl-loop
   for v being the symbols of obarray 
   when
   (and
    (symbolp v) (boundp v)
    (string-match "^voice-" (symbol-name v))
    (not (string-match "-settings$" (symbol-name v)))
    (string=
     (find-lisp-object-file-name v 'defvar)
     (expand-file-name "voice-defs.el" emacspeak-lisp-directory)))
   collect v))

;;;###autoload
(defun emacspeak-wizards-show-voices ()
  "Display a buffer with sample text in the defined voices."
  (interactive)
  (let ((buffer (get-buffer-create "*Voice  Sampler*"))
        (inhibit-read-only  t)
        (voices
         (sort
          (voice-setup-defined-voices)
          #'(lambda (a b) (string-lessp (symbol-name a) (symbol-name b))))))
    (save-current-buffer
      (set-buffer buffer)
      (erase-buffer)
      (special-mode)
      (cl-loop
       for v in voices do
       (insert
        (concat
         "This is a sample of "
         (propertize (symbol-name v) 'personality (symbol-value v))
         " --- It uses "
         (propertize
          (symbol-name (symbol-value v)) 'personality (symbol-value v))))
       (insert ".\n")))
    (funcall-interactively #'pop-to-buffer buffer)
    (goto-char (point-min))))

;;}}}
;;{{{ list-voices-display

(defvar ems--wizards-sampler-text
  "Emacspeak --- The Complete Audio Desktop!"
  "Sample text used  when displaying available voices.")

(defun emacspeak-wizards-list-voices (pattern)
  "Show all defined voice-face mappings  in a help buffer.
Sample text to use comes from variable
  `ems--wizards-sampler-text "
  (interactive (list (and current-prefix-arg
                          (read-string "List faces matching regexp: "))))
  (cl-declare (special ems--wizards-sampler-text))
  (let ((list-faces-sample-text ems--wizards-sampler-text))
    (list-faces-display pattern)
    (message "Displayed voice-face mappings in other window.")))

(defun voice-setup-show-rogue-faces ()
  "Return list of voices that map to non-existent faces."
  (cl-declare (special voice-setup-face-voice-table))
  (cl-loop for f being the hash-keys of voice-setup-face-voice-table
           unless (facep f) collect f))
;;}}}
;;{{{ tramp wizard
(defcustom emacspeak-wizards-tramp-locations nil
  "Tramp locations used by Emacspeak tramp wizard.
Locations added here via custom can be opened using command
emacspeak-wizards-tramp-open-location
bound to \\[emacspeak-wizards-tramp-open-location]."
  :type '(repeat
          (cons :tag "Tramp"
                (string :tag "Name")
                (string :tag "Location")))
  :group 'emacspeak-wizards)

(defun emacspeak-wizards-tramp-open-location (name)
  "Open specified tramp location.
Location is specified by name."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Location:"
                       emacspeak-wizards-tramp-locations
                       nil 'must-match))))
  (cl-declare (special emacspeak-wizards-tramp-locations))
  (let ((location (cdr (assoc name
                              emacspeak-wizards-tramp-locations))))
    (find-file
     (read-file-name "Open: " location))))

;;}}}
;;{{{ find grep using compile

(defun emacspeak-wizards-find-grep (glob pattern)
  "Run compile using find and grep.
Interactive  arguments specify filename pattern and search pattern."
  (interactive
   (list
    (read-from-minibuffer "Look in files: ")
    (read-from-minibuffer "Look for: ")))
  (compile
   (format
    "find . -type f -name '%s' -print0 | xargs -0 -e grep -n -e '%s'"
    glob pattern))
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{ fix text that has gotten read-only accidentally

(defun emacspeak-wizards-fix-read-only-text (start end)
  "Nuke read-only property on text range."
  (interactive "r")
  (let ((inhibit-read-only t))
    (put-text-property start end
                       'read-only nil)))

;;}}}
;;{{{ pod -- perl online docs
(declare-function cperl-pod2man-build-command "cperl-mode" nil)

(defun emacspeak-wizards-display-pod-as-manpage (filename)
  "Create a virtual manpage in Emacs from the Perl Online Documentation."
  (interactive
   (list
    (expand-file-name
     (read-file-name "Enter name of POD file: "))))
  (cl-declare (special pod2man-program))
  (require 'man)
  (let* ((pod2man-args (concat filename " | nroff -man "))
         (bufname (concat "Man " filename))
         (buffer (generate-new-buffer bufname)))
    (save-current-buffer
      (set-buffer buffer)
      (with-environment-variables
          (("TERM" "dumb"))
        (set-process-sentinel
         (start-process pod2man-program buffer "sh" "-c"
                        (format (cperl-pod2man-build-command) pod2man-args))
         'Man-bgproc-sentinel)))))

;;}}}
;;{{{ annotation wizard

;; I use this to collect my annotations into a buffer
;; e.g. an email message to be sent out--
;; while reading and commenting on large documents.

(defun emacspeak-annotate-make-buffer-list (&optional buffer-list)
  "Returns names from BUFFER-LIST excluding those beginning with a space."
  (let (buf-name)
    (delq nil (mapcar
               #'(lambda (b)
                   (setq buf-name (buffer-name b))
                   (and (stringp buf-name)
                        (/= (length buf-name) 0)
                        (/= (aref buf-name 0) ?\ )
                        b))
               (or buffer-list
                   (buffer-list))))))

(defvar emacspeak-annotate-working-buffer nil
  "Buffer that annotations go to.")

(make-variable-buffer-local 'emacspeak-annotate-working-buffer)

(defvar emacspeak-annotate-edit-buffer
  "emacspeak-annotation*"
  "Name of temporary buffer used to edit the annotation.")

(defun emacspeak-annotate-get-annotation ()
  "Pop up a temporary buffer and collect the annotation."
  (cl-declare (special emacspeak-annotate-edit-buffer))
  (let ((annotation nil))
    (pop-to-buffer
     (get-buffer-create emacspeak-annotate-edit-buffer))
    (erase-buffer)
    (message "Exit recursive edit when done.")
    (recursive-edit)
    (local-set-key "\C-c\C-c" 'exit-recursive-edit)
    (setq annotation (buffer-string))
    (bury-buffer)
    annotation))

(defun emacspeak-annotate-add-annotation (&optional reset)
  "Add annotation to the annotation working buffer.
Prompt for annotation buffer if not already set.
Interactive prefix arg `reset' prompts for the annotation
buffer even if one is already set.
Annotation is entered in a temporary buffer and the
annotation is inserted into the working buffer when complete."
  (interactive "P")
  (cl-declare (special emacspeak-annotate-working-buffer))
  (when (or reset
            (null emacspeak-annotate-working-buffer))
    (setq emacspeak-annotate-working-buffer
          (get-buffer-create
           (read-buffer "Annotation working buffer: "
                        (cadr (emacspeak-annotate-make-buffer-list))))))
  (let ((annotation nil)
        (work-buffer emacspeak-annotate-working-buffer)
        (parent-buffer (current-buffer)))
    (message "Adding annotation to %s"
             emacspeak-annotate-working-buffer)
    (save-window-excursion
      (save-current-buffer
        (setq annotation
              (emacspeak-annotate-get-annotation))
        (set-buffer work-buffer)
        (insert annotation)
        (insert "\n"))
      (switch-to-buffer parent-buffer))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{  launch Curl

(defcustom emacspeak-curl-cookie-store
  (expand-file-name "~/.curl-cookies")
  "Cookie store used by Curl."
  :type 'file
  :group 'emacspeak-wizards)

(defun emacspeak-curl (url)
  "Grab URL using Curl, and preview it with a browser ."
  (interactive "sURL: ")
  (cl-declare (special emacspeak-curl-program
                       emacspeak-curl-cookie-store))
  (with-temp-buffer
    (shell-command
     (format
      "%s -s --location-trusted --cookie-jar %s --cookie %s '%s'
2>/dev/null"
      emacspeak-curl-program
      emacspeak-curl-cookie-store emacspeak-curl-cookie-store url)
     (current-buffer))
    (browse-url-of-buffer)))

;;}}}
;;{{{ emacspeak clipboard

(cl-eval-when (load)
  (condition-case nil
      (unless (file-exists-p emacspeak-user-directory)
        (make-directory emacspeak-user-directory))
    (error (message "Make sure you have an Emacspeak resource directory %s"
                    emacspeak-user-directory))))

(defcustom emacspeak-clipboard-file
  (concat emacspeak-user-directory "/" "clipboard")
  "File used to save Emacspeak clipboard.
The emacspeak clipboard provides a convenient mechanism for exchanging
information between different Emacs sessions."
  :group 'emacspeak-speak
  :type 'string)
;;;###autoload
(defun emacspeak-clipboard-copy (start end &optional prompt)
  "Use file-based Emacspeak Clipboard ---
dis a convenient way of sharing information between independent
Emacspeak sessions running on  different machines. "
  (interactive "r\nP")
  (cl-declare (special emacspeak-user-directory emacspeak-clipboard-file))
  (let ((clip (buffer-substring-no-properties start end))
        (clipboard-file
         (if prompt
             (read-file-name "Copy region to clipboard file: "
                             emacspeak-user-directory
                             emacspeak-clipboard-file)
           emacspeak-clipboard-file))
        (clipboard nil))
    (setq clipboard (find-file-noselect clipboard-file))
    (ems-with-messages-silenced
     (save-current-buffer
       (set-buffer clipboard)
       (erase-buffer)
       (insert clip)
       (save-buffer)))
    (message "Copied %s lines to Emacspeak clipboard %s"
             (count-lines start end)
             clipboard-file)))

(declare-function emacspeak-table-paste-from-clipboard "emacspeak-extras" t)

;;;###autoload
(defun emacspeak-clipboard-paste (&optional paste-table)
  "Yank contents of the Emacspeak clipboard at point. "
  (interactive "P")
  (cl-declare (special emacspeak-user-directory emacspeak-clipboard-file))
  (let ((start (point))
        (clipboard-file emacspeak-clipboard-file))
    (cond
     (paste-table (emacspeak-table-paste-from-clipboard))
     (t (insert-file-contents clipboard-file)
        (exchange-point-and-mark)))
    (message "Yanked %s lines from  Emacspeak clipboard %s"
             (count-lines start (point))
             (if paste-table "table clipboard"
               clipboard-file))))

;;}}}
(provide 'emacspeak-extras)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
