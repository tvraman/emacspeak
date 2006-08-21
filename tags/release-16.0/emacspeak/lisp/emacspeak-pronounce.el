;;; emacspeak-pronounce.el --- Implements Emacspeak pronunciation dictionaries
;;; $Id$
;;; $Author$
;;; Description: Emacspeak pronunciation dictionaries
;;; Keywords:emacspeak, audio interface to emacs customized pronunciation
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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman 
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


;;; Commentary:
;;{{{  Introduction

;;;This module implements user customizable pronunciation dictionaries
;;;for emacspeak. Custom pronunciations can be defined per file, per
;;;directory and/or per major mode. Emacspeak maintains a persistent
;;;user dictionary upon request and loads these in new emacspeak
;;;sessions. This module implements the user interface to the custom
;;;dictionary as well as providing the internal API used by the rest
;;;of emacspeak in using the dictionary.
;;;Algorithm:

;;;The persistent dictionary is a hash table where the hash keys are
;;;filenames, directory names, or major-mode names. The hash values
;;;are association lists defining the dictionary. Users of this module
;;;can retrieve a dictionary made up of all applicable association
;;;lists for a given file.

;;}}}
;; 
;;; Code:
;;{{{ required packages

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(eval-when-compile (require 'wid-edit)
 (require 'voice-lock))
(require 'thingatpt)
(eval-when (compile)
                                        ;avoid recursive include during compile
  (provide 'emacspeak-pronounce)
  (require 'dtk-speak)
  (require 'emacspeak-sounds))

;;}}}
;;{{{  Dictionary structure:

(defvar emacspeak-pronounce-dictionaries (make-hash-table )
  "Hash table holding emacspeak's persistent pronunciation dictionaries.
Keys are either filenames, directory names, or major mode names.
Values are alists containing string.pronunciation pairs.")

(defsubst emacspeak-pronounce-set-dictionary (key pr-alist)
  (declare (special emacspeak-pronounce-dictionaries))
  (when (stringp key)
    (setq key (intern key )))
  (setf  (gethash key emacspeak-pronounce-dictionaries) pr-alist ))

(defsubst emacspeak-pronounce-get-dictionary (key)
  (declare (special emacspeak-pronounce-dictionaries))
  (when (stringp key)
    (setq key (intern key )))
  (cl-gethash key emacspeak-pronounce-dictionaries))

(defun emacspeak-pronounce-add-dictionary-entry  (key string pronunciation)
  "Add dictionary entry.
This adds pronunciation pair
STRING.PRONUNCIATION to the   dictionary.
Argument KEY specifies a dictionary key e.g. directory, mode etc."
  (declare (special emacspeak-pronounce-dictionaries ))
  (let ((dict  (emacspeak-pronounce-get-dictionary key)))
    (cond
     (dict
      (setf dict
            (cons (cons string pronunciation) dict))
      (emacspeak-pronounce-set-dictionary key dict ))
     (t (emacspeak-pronounce-set-dictionary key
                                            (list (cons string
                                                        pronunciation)))))))

(defun emacspeak-pronounce-add-buffer-local-dictionary-entry  (string pronunciation)
  "Add specified pronunciation for current buffer.
Arguments STRING and PRONUNCIATION specify what is being defined."
  (declare (special emacspeak-pronounce-pronunciation-table)) 
  (cond
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table))
    (emacspeak-auditory-icon 'on))
   ( emacspeak-pronounce-pronunciation-table ;already on --
     (emacspeak-auditory-icon 'on))
   (t                                   ;turn it on
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))))
  (setf (gethash  (intern string) emacspeak-pronounce-pronunciation-table) pronunciation)
  (message "Added  local pronunciation in buffer %s"
           (buffer-name)))

;;}}}
;;{{{ setting up inheritance relations

;;; child inherits parents dictionary
;;; parent stored as a property on child symbol.
;;; when dictionary composed for a buffer, inherited dictionaries are
;;; also looked up.

(defun emacspeak-pronounce-add-super  (parent child)
  "Make CHILD inherit PARENT's pronunciations."
  (let ((orig (get child 'emacspeak-pronounce-supers)))
    (unless (memq parent orig)
      (setq orig
            (nconc orig (list parent)))
      (put child 'emacspeak-pronounce-supers orig ))
    orig))

(defun emacspeak-pronounce-delete-super  (parent child)
  "Stop  child inheriting  PARENT's pronunciations."
  (let ((orig (get child 'emacspeak-pronounce-supers)))
    (when (memq parent orig)
      (setq orig (delq parent  orig))
      (put child 'emacspeak-pronounce-supers orig ))
    orig))

(defun emacspeak-pronounce-compose-pronunciation-table  (&optional buffer)
  "Composes a pronunciation table for BUFFER.
The default  is current
buffer.  Handles inheritance of pronunciation dictionaries between
modes."
  (setq buffer (or buffer (current-buffer )))
  (let* ((table (make-hash-table))
         (filename (buffer-file-name buffer))
         (directory (and filename
                         (file-name-directory  filename)))
         (mode  (save-excursion
                  (set-buffer buffer)
                  major-mode))
         (mode-supers (emacspeak-pronounce-get-supers mode))
         (file-alist (and filename
                          (emacspeak-pronounce-get-dictionary filename)))
         (dir-alist (and directory
                         (emacspeak-pronounce-get-dictionary directory)))
         (mode-alist (emacspeak-pronounce-get-dictionary mode))
         (super-alist nil))
    (loop for super in mode-supers
          do
          (setq super-alist
                (emacspeak-pronounce-get-dictionary super))
          (loop for element in super-alist
                do
                (setf (gethash
                       (intern (car element))
                       table)
                      (cdr element )))      )
    (loop for element in mode-alist
          do
          (setf (gethash
                 (intern (car element))
                 table)
                (cdr element )))
    (loop for element in dir-alist
          do
          (setf (gethash
                 (intern (car element))
                 table)
                (cdr element )))
    (loop for element in file-alist
          do
          (setf (gethash
                 (intern (car element))
                 table)
                (cdr element )))
    table))

;;}}}
;;{{{ defining some inheritance relations:

;;; gnus server mode inherits from gnus group mode

(emacspeak-pronounce-add-super  'gnus-group-mode
                                'gnus-server-mode)

;;; c++ mode inherits from C mode
(emacspeak-pronounce-add-super  'c-mode 'c++-mode)


;;; latex-mode and latex2e-mode inherit from plain-tex-mode

(emacspeak-pronounce-add-super  'plain-tex-mode 'latex-mode)
(emacspeak-pronounce-add-super  'plain-tex-mode 'latex2e-mode)
;;; latex modes should inherit from plain text modes too
(emacspeak-pronounce-add-super 'text-mode 'latex-mode)
(emacspeak-pronounce-add-super 'text-mode 'latex2e-mode)
(emacspeak-pronounce-add-super 'text-mode 'plain-tex-mode)

;;}}}
;;{{{  Composing and applying dictionaries:

;;; Composing a dictionary results in the return of a hash table that
;;; contains the applicable string.pronunciation pairs for a given
;;; buffer.
;;; Applying a pronunciation table results in the strings being
;;; globally replaced by the defined pronunciations.
;;; Case is handled similarly to vanila emacs behavior.

;;{{{  composing the dictionary

(defun emacspeak-pronounce-get-supers (child)
  "Return list of supers.
Argument CHILD  specifies the mode whose supers are being requested."
  (get child 'emacspeak-pronounce-supers))

;;}}}
(defcustom emacspeak-pronounce-pronunciation-personality nil
  "*Pronunciation personality.
This is the personality used when speaking  things that have a pronunciation
applied."
  :group 'emacspeak
  :type 'symbol)

(defsubst emacspeak-pronounce-apply-pronunciations (pronunciation-table )
  "Applies pronunciations specified in pronunciation table to current buffer.
Modifies text and point in buffer."
  (loop for  key  being the hash-keys  of pronunciation-table
        do
        (let ((word (symbol-name key))
              (pronunciation (cl-gethash  key pronunciation-table )))
          (goto-char (point-min))
          (while (search-forward  word nil t)
            (replace-match  pronunciation t t  )
            (and emacspeak-pronounce-pronunciation-personality
                 (put-text-property
                  (match-beginning 0)
                  (+ (match-beginning 0) (length pronunciation))
                  'personality
                  emacspeak-pronounce-pronunciation-personality))))))

;;}}}
;;{{{  loading, clearing  and saving dictionaries

(defcustom emacspeak-pronounce-dictionaries-file  nil
  "File that holds the persistent emacspeak pronunciation dictionaries."
  :type '(file :tag "Dictionary File ")
  :group 'emacspeak)

(declaim (special emacspeak-resource-directory))
(setq emacspeak-pronounce-dictionaries-file
      (expand-file-name  ".dictionary" 
                         emacspeak-resource-directory))


(defun emacspeak-pronounce-save-dictionaries  ()
  "Writes out the persistent emacspeak pronunciation dictionaries."
  (interactive)
  (declare (special emacspeak-pronounce-dictionaries ))
  (let ((filename (read-file-name
                   "Save pronunciation dictionaries to file: "
                   emacspeak-resource-directory
                   emacspeak-pronounce-dictionaries-file ))
        (buffer nil ))
    (setq buffer (find-file-noselect filename))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (loop for key being the hash-keys  of emacspeak-pronounce-dictionaries
            do
            (insert
             (format "(emacspeak-pronounce-set-dictionary '%S\n '%S )\n"
                     key
                     (emacspeak-pronounce-get-dictionary key ))))
      (goto-char (point-min))
      (while (search-forward ")" nil t)
        (replace-match ")\n" nil t))
      (save-buffer))))
(defvar emacspeak-pronounce-dictionaries-loaded nil
  "Indicates if dictionaries already loaded.")

(defun emacspeak-pronounce-load-dictionaries  (&optional filename)
  "Load pronunciation dictionaries.
Optional argument FILENAME specifies the dictionary file."
  (interactive
   (list (read-file-name
          "Load pronunciation dictionaries from file: "
          emacspeak-resource-directory
          emacspeak-pronounce-dictionaries-file )))
  (declare (special emacspeak-pronounce-dictionaries-loaded))
  (load-file filename)
  (setq emacspeak-pronounce-dictionaries-loaded t))

(defun emacspeak-pronounce-clear-dictionaries ()
  "Clear all current pronunciation dictionaries."
  (interactive)
  (declare (special emacspeak-pronounce-dictionaries ))
  (when (yes-or-no-p
         "Do you really want to nuke all currently defined dictionaries?")
    (setq emacspeak-pronounce-dictionaries
          (make-hash-table ))
    (emacspeak-pronounce-refresh-pronunciations)))

;;}}}
;;{{{  Front end to define pronunciations:

(defvar emacspeak-pronounce-pronunciation-keys
  '(("buffer" . "buffer")
    ("file" . "file")
    ("directory" . "directory")
    ("mode" . "mode"))
  "Pronunciations can be defined for these kinds of things.")

(defvar emacspeak-pronounce-current-buffer nil
  "Buffer name where we are currently defining a pronunciation.")

(defvar emacspeak-pronounce-yank-word-point nil
  "Point where we left off reading from the buffer containing the term being defined.")

(make-variable-buffer-local ' emacspeak-pronounce-yank-word-point)
(defsubst emacspeak-pronounce-yank-word ()
  "Yank word at point into minibuffer."
  (interactive)
  (declare (special emacspeak-pronounce-yank-word-point
                    emacspeak-pronounce-current-buffer))
  (let ((string (save-excursion
                  (set-buffer emacspeak-pronounce-current-buffer)
                  (goto-char emacspeak-pronounce-yank-word-point)
                  (buffer-substring-no-properties
                   (point)
                   (save-excursion
                     (forward-word 1)
                     (setq emacspeak-pronounce-yank-word-point (point)))))))
    (insert string)
    (dtk-speak string)))

(defsubst emacspeak-pronounce-read-term (key)
  (declare (special emacspeak-pronounce-yank-word-point
                    emacspeak-pronounce-current-buffer))
  (let ((default  (and (mark)
                       (< (count-lines (region-beginning)
                                       (region-end)) 2)
                       (buffer-substring (region-beginning)
                                         (region-end))))
        (emacspeak-pronounce-yank-word-point (point)))
    (setq emacspeak-pronounce-current-buffer (current-buffer))
    (read-from-minibuffer
     (format "Define pronunciation in %s for: " key)
     default
     (let ((now-map (copy-keymap minibuffer-local-map)))
       (progn
         (define-key now-map  "\C-w"'emacspeak-pronounce-yank-word))
       now-map))))

(defun emacspeak-pronounce-define-local-pronunciation (word  pronunciation )
  "Define buffer local pronounciation.
Argument WORD specifies the word which should be pronounced as specified by PRONUNCIATION."
  (interactive
   (list
    (emacspeak-pronounce-read-term 'buffer)
    (read-from-minibuffer
     (format "Pronounce  as: " ))))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   word pronunciation))

(defun emacspeak-pronounce-define-pronunciation ()
  "Interactively define entries in the pronunciation dictionaries.
Default term to define is delimited by region.
First loads any persistent dictionaries if not already loaded."
  (interactive)
  (declare (special emacspeak-pronounce-dictionaries-loaded))
  (let ((key nil)
        (word nil)
        (pronunciation nil)
        (key-type (completing-read  "Define pronunciation that is specific to: "
                                    emacspeak-pronounce-pronunciation-keys nil t )))
    (cond
     ((string= key-type "buffer")
      (setq key (buffer-name )))        ;handled differently
     ((string= key-type "file")
      (setq key (buffer-file-name))
      (or key
          (error "Current buffer is not associated with a file"))
      (setq key (intern key)))
     ((string= key-type "directory")
      (setq key
            (or
             (condition-case nil
                 (file-name-directory (buffer-file-name ))
               (error nil ))
             default-directory))
      (or key (error "No directory associated with current buffer"))
      (setq key (intern key)))
     ((string= key-type "mode")
      (setq key
            major-mode)
      (or key (error "No major mode found for current buffer")))
     (t (error "Cannot define pronunciations with key type %s" key-type)))
    (setq word (emacspeak-pronounce-read-term key))
    (setq pronunciation
          (read-from-minibuffer
           (format "Pronounce %s as: " word)))
    (when (and (not emacspeak-pronounce-dictionaries-loaded)
               (y-or-n-p "Load pre existing  pronunciation dictionaries first? "))
      (emacspeak-pronounce-load-dictionaries))
    (unless  (string= key-type  "buffer")
      (emacspeak-pronounce-add-dictionary-entry key word pronunciation)
      (emacspeak-pronounce-refresh-pronunciations))
    (when (string= key-type  "buffer")
      (emacspeak-pronounce-add-buffer-local-dictionary-entry  word pronunciation))))


;;}}}
;;{{{ Turning dictionaries on and off on a per buffer basis

(defvar emacspeak-pronounce-pronunciation-table nil
  "Variable holding association list of pronunciations for a buffer.
Becomes automatically buffer local.")
(make-variable-buffer-local 'emacspeak-pronounce-pronunciation-table)
(setq-default emacspeak-pronounce-pronunciation-table nil)

(defun emacspeak-pronounce-toggle-use-of-dictionaries (&optional state)
  "Toggle use of pronunciation dictionaries in current buffer.
Pronunciations can be dfined on a per file, per directory and/or per
mode basis.
Pronunciations are activated on a per buffer basis.
Turning on the use of pronunciation dictionaries results in emacspeak
composing a pronunciation table based on the currently defined
pronunciation dictionaries.
After this, the pronunciations will be applied whenever text in the
buffer is spoken.
Optional argument state can be used from Lisp programs to
explicitly turn pronunciations on or off."
  (interactive)
  (declare (special emacspeak-pronounce-pronunciation-table))
  (cond
   ((or (eq state 'on)
        (not (boundp 'emacspeak-pronounce-pronunciation-table)))
    (make-local-variable 'emacspeak-pronounce-pronunciation-table)
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))
    (emacspeak-auditory-icon 'on)
    (message "Emacspeak pronunciation dictionaries are now active in this buffer"))
   ((or (eq state 'off)
                                        ;already on --turn it off
        emacspeak-pronounce-pronunciation-table )
    (setq emacspeak-pronounce-pronunciation-table nil)
    (emacspeak-auditory-icon 'off)
    (message
     "Emacspeak pronunciation dictionaries no longer active in this buffer"))
   (t                                   ;turn it on
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))
    (message
     "Emacspeak pronunciations have been re-activated in this buffer")
    (emacspeak-auditory-icon 'on))))

(defun emacspeak-pronounce-refresh-pronunciations ()
  "Refresh pronunciation table for current buffer.
Activates pronunciation dictionaries if not already active."
  (interactive)
  (declare (special emacspeak-pronounce-pronunciation-table))
  (cond
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table))
    (emacspeak-auditory-icon 'on)
    (message
     "Refreshed pronunciations for this buffer"))
   ( emacspeak-pronounce-pronunciation-table ;already on --refresh it
     (setq emacspeak-pronounce-pronunciation-table
           (emacspeak-pronounce-compose-pronunciation-table))
     (emacspeak-auditory-icon 'on)
     (message
      "Refreshed pronunciation for this buffer"))
   (t                                   ;turn it on
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))
    (message
     "Refreshed pronunciations for this buffer")
    (emacspeak-auditory-icon 'on))))

;;}}}
;;{{{  dictionary editor 



(defun emacspeak-pronounce-edit-generate-pronunciation-editor  (key)
  "Generate a widget-enabled edit buffer for editting the
pronunciation dictionary for the specified key."
  (declare (special emacspeak-pronounce-dictionaries))
  (unless emacspeak-pronounce-pronunciation-table
    (emacspeak-pronounce-toggle-use-of-dictionaries))
  (let ((value (gethash key emacspeak-pronounce-dictionaries))
        (notify (emacspeak-pronounce-edit-generate-callback key))
        (buffer-name (format "*Dictionary: %s" key))
        (buffer nil)
        (inhibit-read-only t))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (setq buffer (get-buffer-create buffer-name))
    (save-excursion
      (set-buffer  buffer)
      (voice-lock-mode t)
      (widget-insert "\n")
      (widget-insert
       (format "Editting pronunciation dictionary for %s\n\n" key))
      (widget-create 'repeat
                     :help-echo "Edit Pronunciations"
                     :tag "Pronunciations"
                     :value value
                     :notify notify
                     '(cons :tag "Dictionary Entry"
                            (string :tag "Phrase")
                            (string :tag "Pronounce as")))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Save Dictionary"
                     :notify
                     #'(lambda (&rest ignore)
                         (call-interactively 'emacspeak-pronounce-save-dictionaries)))
      (widget-insert "\n\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun emacspeak-pronounce-edit-generate-callback (field-name)
  "Generate a callback for use in the pronunciation editor widget."
  (`
   (lambda (widget &rest ignore)
     (declare (special emacspeak-pronounce-dictionaries))
     (let ((value (widget-value widget)))
       (setf
        (gethash 
         (quote (, field-name))
         emacspeak-pronounce-dictionaries)
        value)))))

(defun emacspeak-pronounce-edit-pronunciations (key)
   "Prompt for and launch a pronunciation editor on the
specified pronunciation dictionary key."
   (interactive
    (list
     (let ((keys
            (loop for k being the hash-keys of
                  emacspeak-pronounce-dictionaries
                  collect
                  (symbol-name k))))
       (completing-read "Edit dictionary: "
                        (mapcar
                         #'(lambda (k)
                             (cons k k ))
                         keys)
                        nil
                        'REQUIRE-MATCH 
                        nil
                        'keys
                        (car keys))))) 
   (declare (special emacspeak-pronounce-dictionaries))
   (emacspeak-pronounce-edit-generate-pronunciation-editor
    (intern key)))

;;}}}
;;{{{ top level dispatch routine

(defvar emacspeak-pronounce-help
  "Dictionary:  Clear Define Edit Load Refresh Save Toggle"
  "Help message listing emacspeak commands.")

(defun emacspeak-pronounce-dispatch ()
  "Provides the user interface front-end to Emacspeak's pronunciation dictionaries."
  (interactive)
  (declare (special emacspeak-pronounce-help))
  (message emacspeak-pronounce-help)
  (let ((event (read-char)))
    (case event
      (?c (call-interactively 'emacspeak-pronounce-clear-dictionaries))
      (?d (call-interactively
           'emacspeak-pronounce-define-pronunciation t))
      (?e (call-interactively
           'emacspeak-pronounce-edit-pronunciations t))
      (?l (call-interactively 'emacspeak-pronounce-load-dictionaries))
      (?r (call-interactively 'emacspeak-pronounce-refresh-pronunciations))
      (?s (call-interactively 'emacspeak-pronounce-save-dictionaries))
      (?t (call-interactively 'emacspeak-pronounce-toggle-use-of-dictionaries))
      (otherwise (message emacspeak-pronounce-help)))
    (emacspeak-auditory-icon 'close-object)))

;;}}}

(provide  'emacspeak-pronounce)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
