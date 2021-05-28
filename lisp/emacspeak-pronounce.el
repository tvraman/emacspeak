;;; emacspeak-pronounce.el --- Emacspeak pronunciation dictionaries -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Emacspeak pronunciation dictionaries
;;; Keywords:emacspeak, audio interface to emacs customized pronunciation
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-02-19 16:55:48 -0800 (Tue, 19 Feb 2008) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{ Introduction

;;; Commentary:
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
;;; Code:

;;}}}
;;{{{ required Modules:

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'voice-setup)

;;}}}
;;{{{ Dictionary structure:

(defvar emacspeak-pronounce-dictionaries (make-hash-table :test #'eq)
  "Hash table  for   pronunciation dictionaries.
Keys are either filenames, directory names, or major mode names.
Values are alists containing string.pronunciation pairs.")

(defun emacspeak-pronounce-set-dictionary (key pr-alist)
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (when (stringp key)
    (setq key (intern key)))
  (setf (gethash key emacspeak-pronounce-dictionaries) pr-alist))

(defun emacspeak-pronounce-get-dictionary (key)
  (cl-declare (special emacspeak-pronounce-dictionaries
                       minibuffer-history))
  (when (stringp key)
    (setq key (intern key)))
  (gethash key emacspeak-pronounce-dictionaries))

(defun emacspeak-pronounce-add-dictionary-entry (key string pronunciation)
  " Adds pronunciation pair STRING.PRONUNCIATION to the dictionary.
Argument KEY specifies a dictionary key e.g. directory, mode etc.
Pronunciation can be a string or a cons-pair.
If it is a string, that string is the new pronunciation.
A cons-pair of the form (matcher . func) results  in 
the match  being passed to the func which returns  the new pronunciation."
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (let* ((dict (emacspeak-pronounce-get-dictionary key))
         (entry (and dict (assoc string dict))))
    (cond
     ((and dict entry)
      (setcdr entry pronunciation))
     (dict
      (setf dict (cons (cons string pronunciation) dict))
      (emacspeak-pronounce-set-dictionary key dict))
     (t
      (emacspeak-pronounce-set-dictionary key
                                          (list (cons string pronunciation)))))))

(defun emacspeak-pronounce-remove-buffer-local-dictionary-entry (string)
  "Remove pronunciation entry."
  (when (and (boundp 'emacspeak-pronounce-pronunciation-table)
             emacspeak-pronounce-pronunciation-table)
    (remhash string
             emacspeak-pronounce-pronunciation-table)))

(defun emacspeak-pronounce-add-buffer-local-dictionary-entry (string pronunciation)
  "Add  pronunciation for current buffer. "
  (cl-declare (special emacspeak-pronounce-pronunciation-table))
  (cond
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table))
    (when (called-interactively-p 'interactive)(emacspeak-auditory-icon 'on)))
   (emacspeak-pronounce-pronunciation-table ;already on --
    (when (called-interactively-p 'interactive)(emacspeak-auditory-icon 'on)))
   (t                                   ;turn it on
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))))
  (puthash string pronunciation
           emacspeak-pronounce-pronunciation-table)
  (when (called-interactively-p 'interactive)
    (message "Added local pronunciation in buffer %s"
             (buffer-name))))

;;}}}
;;{{{ setting up inheritance relations

;;; child inherits parents dictionary
;;; parent stored as a property on child symbol.
;;; when dictionary composed for a buffer, inherited dictionaries are
;;; also looked up.
(defun emacspeak-pronounce-add-super (parent child)
  "Make CHILD inherit PARENT's pronunciations."
  (let ((orig (get child 'emacspeak-pronounce-supers)))
    (unless (memq parent orig)
      (setq orig
            (nconc orig (list parent)))
      (put child 'emacspeak-pronounce-supers orig))
    orig))

(defun emacspeak-pronounce-delete-super (parent child)
  "Stop child inheriting PARENT's pronunciations."
  (let ((orig (get child 'emacspeak-pronounce-supers)))
    (when (memq parent orig)
      (setq orig (delq parent orig))
      (put child 'emacspeak-pronounce-supers orig))
    orig))

(defun emacspeak-pronounce-compose-pronunciation-table (&optional
                                                        buffer)
  "Composes a pronunciation table for BUFFER. "
  (setq buffer (or buffer (current-buffer)))
  (let* ((table (make-hash-table :test #'equal))
         (filename (buffer-file-name buffer))
         (directory (and filename (file-name-directory filename)))
         (mode
          (save-current-buffer
            (set-buffer buffer)
            major-mode))
         (mode-supers (emacspeak-pronounce-get-supers mode))
         (file-alist (and filename (emacspeak-pronounce-get-dictionary filename)))
         (dir-alist (and directory (emacspeak-pronounce-get-dictionary directory)))
         (mode-alist (emacspeak-pronounce-get-dictionary mode))
         (super-alist nil))
    (cl-loop for super in mode-supers
             do
             (setq super-alist (emacspeak-pronounce-get-dictionary super))
             (cl-loop for element in super-alist
                      do
                      (puthash (car element) (cdr element) table)))
    (cl-loop for element in mode-alist
             do
             (puthash (car element) (cdr element) table))
    (cl-loop for element in dir-alist
             do
             (puthash (car element) (cdr element) table))
    (cl-loop for element in file-alist
             do
             (puthash (car element) (cdr element) table))
    table))

;;}}}
;;{{{ defining some inheritance relations:

;;; gnus server mode inherits from gnus group mode

(emacspeak-pronounce-add-super 'gnus-group-mode
                               'gnus-server-mode)

;;; c++ mode inherits from C mode
(emacspeak-pronounce-add-super 'c-mode 'c++-mode)
;;; shell inherits from comint:
(emacspeak-pronounce-add-super 'comint-mode 'shell-mode)
;;; latex-mode and latex2e-mode inherit from plain-tex-mode

(emacspeak-pronounce-add-super 'plain-tex-mode 'latex-mode)
(emacspeak-pronounce-add-super 'plain-tex-mode 'latex2e-mode)
;;; latex modes should inherit from plain text modes too
(emacspeak-pronounce-add-super 'text-mode 'latex-mode)
(emacspeak-pronounce-add-super 'text-mode 'latex2e-mode)
(emacspeak-pronounce-add-super 'text-mode 'plain-tex-mode)
;;;xsl inherits from xml
(emacspeak-pronounce-add-super 'xml-mode 'xsl-mode)

;;; VM,  EWW
(emacspeak-pronounce-add-super 'text-mode 'eww-mode)
(emacspeak-pronounce-add-super 'text-mode 'vm-presentation-mode)

;;}}}
;;{{{ Composing and applying dictionaries:

;;; Composing a dictionary results in the return of a hash table that
;;; contains the applicable string.pronunciation pairs for a given
;;; buffer.
;;; Applying a pronunciation table results in the strings being
;;; globally replaced by the defined pronunciations.
;;; Case is handled similarly to vanila emacs behavior.

;;{{{ composing the dictionary

(defun emacspeak-pronounce-get-supers (child)
  "Return list of supers for mode `child'. "
  (get child 'emacspeak-pronounce-supers))

;;}}}

(defvar-local emacspeak-pronounce-pronunciation-personality voice-lighten
  "Pronunciation personality.
This is the personality used when speaking things that have a pronunciation
applied.")

(defun emacspeak-pronounce-toggle-voice ()
  "Toggle use of pronunciation personality."
  (interactive )
  (cl-declare (special emacspeak-pronounce-pronunciation-personality))
  (setq emacspeak-pronounce-pronunciation-personality (not
                                                       emacspeak-pronounce-pronunciation-personality))
  (emacspeak-auditory-icon
   (if emacspeak-pronounce-pronunciation-personality 'on 'off))
  (message "Turned %s pronunciation personality"
           (if emacspeak-pronounce-pronunciation-personality 'on 'off)))

;;}}}
;;{{{ loading, clearing and saving dictionaries

(cl-declaim (special emacspeak-user-directory))

(defcustom emacspeak-pronounce-dictionaries-file
  (expand-file-name ".dictionary"
                    emacspeak-user-directory)
  "File that holds  emacspeak pronunciations."
  :type '(file :tag "Dictionary File ")
  :group 'emacspeak)



(defun emacspeak-pronounce-save-dictionaries ()
  "Saves  pronunciation dictionaries."
  (interactive)
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (let* ((coding-system-for-write 'utf-8)
         (print-level nil)
         (print-length nil)
         (filename (read-file-name
                    "Save pronunciation dictionaries to file: "
                    emacspeak-user-directory
                    nil nil
                    (file-name-nondirectory emacspeak-pronounce-dictionaries-file)))
         (buffer nil))
    (setq buffer (find-file-noselect filename))
    (save-current-buffer
      (set-buffer buffer)
      (auto-fill-mode nil)
      (erase-buffer)
      (cl-loop for key being the hash-keys of emacspeak-pronounce-dictionaries
               do
               (insert
                (format "(emacspeak-pronounce-set-dictionary '%S\n '%S)\n"
                        key
                        (emacspeak-pronounce-get-dictionary key))))
      (save-buffer))))

(defvar emacspeak-pronounce-dictionaries-loaded nil
  "Indicates if dictionaries already loaded.")
(defun emacspeak-pronounce-load-dictionaries (&optional filename)
  "Load pronunciation dictionaries.
Optional argument FILENAME specifies the dictionary file,
Default is emacspeak-pronounce-dictionaries-file."
  (interactive
   (list
    (read-file-name
     "Load pronunciation dictionaries from file: "
     emacspeak-user-directory emacspeak-pronounce-dictionaries-file)))
  (cl-declare (special emacspeak-pronounce-dictionaries-file emacspeak-pronounce-dictionaries-loaded))
  (setq filename (or  filename  emacspeak-pronounce-dictionaries-file))
  (when (file-exists-p filename)
    (condition-case nil
        (progn
          (ems--fastload filename)
          (setq emacspeak-pronounce-dictionaries-loaded t))
      (error (message "Error loading pronunciation dictionary")))))


(defun emacspeak-pronounce-clear-dictionaries ()
  "Clear all current pronunciation dictionaries."
  (interactive)
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (when (yes-or-no-p
         "Do you really want to nuke all currently defined dictionaries?")
    (setq emacspeak-pronounce-dictionaries (make-hash-table))
    (emacspeak-pronounce-refresh-pronunciations)))

;;}}}
;;{{{ Front end to define pronunciations:

(defvar emacspeak-pronounce-pronunciation-keys
  '(("buffer" . "buffer")
    ("file" . "file")
    ("directory" . "directory")
    ("mode" . "mode"))
  "Pronunciations can be defined for these kinds of things.")

(defvar emacspeak-pronounce-current-buffer nil
  "Buffer name where we are currently defining a pronunciation.")

(defvar emacspeak-pronounce-yank-word-point nil
  "Point where we left off reading from the buffer containing the
  term being defined.")

(make-variable-buffer-local ' emacspeak-pronounce-yank-word-point)

(defun emacspeak-pronounce-read-term (key)
  (cl-declare (special emacspeak-pronounce-yank-word-point
                       emacspeak-pronounce-current-buffer))
  (let ((default (and (mark)
                      (< (count-lines (region-beginning)
                                      (region-end)) 2)
                      (buffer-substring-no-properties (region-beginning)
                                        (region-end))))
        (emacspeak-pronounce-yank-word-point (point)))
    (setq emacspeak-pronounce-current-buffer (current-buffer))
    (read-from-minibuffer
     (format "Define pronunciation in %s for: " key)
     default
     (let ((now-map (copy-keymap minibuffer-local-map)))
       (progn
         (define-key now-map "\C-w"'emacspeak-pronounce-yank-word))
       now-map))))


(defun emacspeak-pronounce-define-local-pronunciation (word pron)
  "Define buffer local pronunciation.
Argument `word' specified the word to be pronounced.
Argument `pron' specifies the new pronunciation. "
  (interactive
   (list
    (emacspeak-pronounce-read-term 'buffer)
    (read-from-minibuffer
     (format "Pronounce as: "))))
  (emacspeak-pronounce-add-buffer-local-dictionary-entry
   word pron))

(defun emacspeak-pronounce-get-key ()
  "Collect key from user.
Returns a pair of the form (key-type . key)."
  (cl-declare (special emacspeak-pronounce-pronunciation-keys))
  (let ((key nil)
        (key-type
         (read
          (completing-read
           "Define pronunciation that is specific to: "
           emacspeak-pronounce-pronunciation-keys nil t))))
    (when (called-interactively-p 'interactive) ;cleanup minibuffer history
      (pop minibuffer-history))
    (cond
     ((eq key-type 'buffer)
      (setq key (buffer-name))) ;handled differently
     ((eq key-type 'file)
      (setq key (buffer-file-name))
      (or key
          (error "Current buffer is not associated with a file"))
      (setq key (intern key)))
     ((eq key-type 'directory)
      (setq key
            (or
             (condition-case nil
                 (file-name-directory (buffer-file-name))
               (error nil))
             default-directory))
      (or key (error "No directory associated with current buffer"))
      (setq key (intern key)))
     ((eq key-type 'mode)
      (setq key
            major-mode)
      (or key (error "No major mode found for current buffer")))
     (t (error "Cannot define pronunciations with key type %s" key-type)))
    (cons key-type key)))


(defun emacspeak-pronounce-define-template-pronunciation ()
  "Interactively define template entries in the pronunciation dictionaries.
Default term to define is delimited by region.
First loads any persistent dictionaries if not already loaded."
  (interactive)
  (cl-declare (special emacspeak-pronounce-dictionaries-loaded))
  (let ((word nil)
        (pronunciation nil)
        (key-pair (emacspeak-pronounce-get-key)))
    (setq word (read-minibuffer "Pattern"))
    (setq pronunciation
          (cons
           (read-minibuffer
            (format "Matcher for %s: " word))
           (read-minibuffer
            (format "Pronouncer for %s: " word))))
    (when (and (not emacspeak-pronounce-dictionaries-loaded)
               (y-or-n-p "Load pre existing pronunciation dictionaries first? "))
      (emacspeak-pronounce-load-dictionaries))
    (unless (eq (car key-pair) 'buffer)
      (emacspeak-pronounce-add-dictionary-entry (cdr key-pair) word pronunciation)
      (emacspeak-pronounce-refresh-pronunciations))
    (when (eq (car key-pair) 'buffer)
      (emacspeak-pronounce-add-buffer-local-dictionary-entry word pronunciation))))


(defun emacspeak-pronounce-define-pronunciation ()
  "Interactively define entries in the pronunciation dictionaries.
Default term to define is delimited by region.
First loads any persistent dictionaries if not already loaded."
  (interactive)
  (cl-declare (special emacspeak-pronounce-dictionaries-loaded))
  (let ((word nil)
        (pronunciation nil)
        (key-pair(emacspeak-pronounce-get-key)))
    (setq word (emacspeak-pronounce-read-term (cdr key-pair)))
    (setq pronunciation
          (read-from-minibuffer
           (format "Pronounce %s as: " word)))
    (when (and (not emacspeak-pronounce-dictionaries-loaded)
               (y-or-n-p "Load pre existing pronunciation dictionaries first? "))
      (emacspeak-pronounce-load-dictionaries))
    (unless (eq (car key-pair) 'buffer)
      (emacspeak-pronounce-add-dictionary-entry (cdr key-pair) word pronunciation)
      (emacspeak-pronounce-refresh-pronunciations))
    (when (eq (car key-pair) 'buffer)
      (emacspeak-pronounce-add-buffer-local-dictionary-entry word pronunciation))))

;;}}}
;;{{{ Turning dictionaries on and off on a per buffer basis
(defvar-local  emacspeak-pronounce-pronunciation-table nil
  "Variable holding association list of pronunciations for a buffer.
Becomes automatically buffer local.")

(setq-default emacspeak-pronounce-pronunciation-table nil)


(defun  emacspeak-pronounce-pronunciation-table ()
  "Closure that returns the pronunciation table."
  emacspeak-pronounce-pronunciation-table)


(defun emacspeak-pronounce-toggle-use-of-dictionaries (&optional state)
  "Toggle use of pronunciation dictionaries in current buffer.
Pronunciations can be defined on a per file, per directory and/or
per mode basis.  Pronunciations are activated on a per buffer
basis.  Turning on the use of pronunciation dictionaries results
in emacspeak composing a pronunciation table based on the
currently defined pronunciation dictionaries.  After this, the
pronunciations will be applied whenever text in the buffer is
spoken.  Optional argument state can be used from Lisp programs
to explicitly turn pronunciations on or off."
  (interactive "P")
  (cl-declare (special emacspeak-pronounce-pronunciation-table))
  (unless state (setq state (not emacspeak-pronounce-pronunciation-table)))
  (cond
   (state
    (unless emacspeak-pronounce-pronunciation-table
      (setq emacspeak-pronounce-pronunciation-table
            (emacspeak-pronounce-compose-pronunciation-table))))
   ((null state)                        ;already on --turn it off
    (setq emacspeak-pronounce-pronunciation-table nil)))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon
     (if emacspeak-pronounce-pronunciation-table 'on 'off))
    (message
     "Turned  pronunciations %s."
     (if emacspeak-pronounce-pronunciation-table " on " " off "))))


(defun emacspeak-pronounce-refresh-pronunciations ()
  "Refresh pronunciation table for current buffer.
Activates pronunciation dictionaries if not already active."
  (interactive)
  (cl-declare (special emacspeak-pronounce-pronunciation-table))
  (cond
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table)))
   (emacspeak-pronounce-pronunciation-table ;already on --refresh it
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table)))
   (t                                   ;turn it on
    (setq emacspeak-pronounce-pronunciation-table
          (emacspeak-pronounce-compose-pronunciation-table))))
  (when (called-interactively-p 'interactive)
    (emacspeak-auditory-icon 'on)
    (message
     "Refreshed pronunciations for this buffer")))

;;}}}
;;{{{ common dictionary containing smileys and friends

(defcustom emacspeak-pronounce-internet-smileys-pronunciations
  '((":-)" . " smile ")
    (";)" . " half-wink ")
    (":)" . " grin ")
    (":-(" . " frown ")
    (":(" . " sigh ")
    (":-I" . " shrug ")
    (":->" . " sarcastic smile ")
    (">:->" . " devillish smile ")
    (">;->" . " lewd smile ")
    (";-)" . " wink "))
  "Pronunciation dictionary used in all instant messenger and IRC chat
modes.
See http://www.charm.net/~kmarsh/smiley.html. "
  :link '(url-link :tag "Smileys Dictionary "
                   "http://oz.uc.edu/~solkode/smileys.html")
  :type '(repeat
          (cons :tag "Dictionary Entry"
                (string :tag "String")
                (string :tag "Pronunciation")))
  :group 'emacspeak)

;;}}}
;;{{{ xml namespace uri's

(defcustom emacspeak-pronounce-common-xml-namespace-uri-pronunciations
  '(
    ("http://schemas.google.com/g/2005" . " gd ")
    ("http://www.w3.org/2005/Atom" . " atom ")
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" . "RDF Syntax")
    ("http://www.w3.org/2002/06/xhtml2" . " xhtml2 ")
    ("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"
     . "XUL")
    ("http://www.mozilla.org/xbl" . " xbl ")
    ("http://www.w3.org/2003/XInclude" . "xinclude")
    ("http://www.w3.org/1999/XSL/Transform" . " XSLT ")
    ("http://www.w3.org/2002/xforms" . " XForms ")
    ("http://www.w3.org/2001/xml-events" . " XEvents ")
    ("http://www.w3.org/2001/vxml" . " vxml ")
    ("http://www.w3.org/2001/XMLSchema-instance". " XSchema Instance ")
    ("http://www.w3.org/2001/XMLSchema". " XSchema ")
    ("http://www.w3.org/1999/xhtml" . " xhtml ")
    ("http://schemas.xmlsoap.org/wsdl/" . " WSDL ")
    ("http://www.w3.org/2001/06/soap-envelope" . " SOAP ENV ")
    ("http://schemas.xmlsoap.org/wsdl/soap/" . " SOAP ")
    ("http://purl.org/dc/elements/1.1/" . "Dublin Core")
    ("http://search.yahoo.com/mrss/" . "media")
    )
  "Pronunciations for well known namespace URIs."
  :type '(repeat
          (cons :tag "Dictionary Entry"
                (string :tag "Namespace URI")
                (string :tag "Pronunciation")))
  :group 'emacspeak)

;;}}}
;;{{{ adding predefined dictionaries to a mode:
(defun emacspeak-pronounce-augment-pronunciations (mode dictionary)
  "Pushes pronunciations in specified dictionary on to the dictionary
for the specified mode."
  (let ((mode-alist (emacspeak-pronounce-get-dictionary mode)))
    (cl-loop for e in dictionary
             do
             (unless (assoc (car e)
                            mode-alist)
               (push e mode-alist)))
    (emacspeak-pronounce-set-dictionary mode mode-alist)))

;;}}}
;;{{{ dictionary editor

(defun emacspeak-pronounce-edit-generate-pronunciation-editor (key)
  "Generate a widget-enabled edit buffer for editing the
pronunciation dictionary for the specified key."
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (unless emacspeak-pronounce-pronunciation-table
    (emacspeak-pronounce-toggle-use-of-dictionaries))
  (let ((value (gethash key emacspeak-pronounce-dictionaries))
        (notify (emacspeak-pronounce-edit-generate-callback key))
        (buffer-name (format "*Dictionary: %s" key))
        (buffer nil)
        (inhibit-read-only t))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (setq buffer (get-buffer-create buffer-name))
    (save-current-buffer
      (set-buffer buffer)
      (widget-insert "\n")
      (widget-insert
       (format "Editing pronunciation dictionary for %s\n\n" key))
      (widget-create 'repeat
                     :help-echo "Edit Pronunciations"
                     :tag "Pronunciations"
                     :value value
                     :notify notify
                     '(cons :tag "Dictionary Entry"
                            (string :tag "Phrase")
                            (choice :tag "Pronunciation"
                                    (string :tag "Pronounce as")
                                    (cons :tag "Pronouncer"
                                          (symbol :tag "Matcher")
                                          (symbol :tag "Pronouncer")))))
      (widget-insert "\n")
      (widget-create 'push-button
                     :tag "Save Dictionary"
                     :notify
                     #'(lambda (&rest _ignore)
                         (call-interactively 'emacspeak-pronounce-save-dictionaries)))
      (widget-insert "\n\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun emacspeak-pronounce-edit-generate-callback (field-name)
  "Generate a callback for use in the pronunciation editor widget."
  `(lambda (widget &rest ignore)
     (cl-declare (special emacspeak-pronounce-dictionaries))
     (let ((value (widget-value widget)))
       (setf
        (gethash
         (quote ,field-name)
         emacspeak-pronounce-dictionaries)
        value))))


(defun emacspeak-pronounce-edit-pronunciations (key)
  "Prompt for and launch a pronunciation editor on the
specified pronunciation dictionary key."
  (interactive
   (list
    (let ((keys
           (cl-loop for k being the hash-keys of
                    emacspeak-pronounce-dictionaries
                    collect
                    (symbol-name k))))
      (completing-read "Edit dictionary: "
                       (mapcar
                        #'(lambda (k)
                            (cons k k))
                        keys)
                       nil
                       'REQUIRE-MATCH
                       nil
                       'keys
                       (car keys)))))
  (cl-declare (special emacspeak-pronounce-dictionaries))
  (emacspeak-pronounce-edit-generate-pronunciation-editor
   (intern key)))

;;}}}
;;{{{ top level dispatch routine

(defvar emacspeak-pronounce-help
  "Dictionary: Clear Define Edit Load Refresh Save Toggle"
  "Help message listing emacspeak commands.")


(defun emacspeak-pronounce-dispatch ()
  "Provides the user interface front-end to Emacspeak's pronunciation dictionaries."
  (interactive)
  (cl-declare (special emacspeak-pronounce-help))
  (message emacspeak-pronounce-help)
  (let ((event (read-char)))
    (cl-case event
      (?c (call-interactively 'emacspeak-pronounce-clear-dictionaries))
      (?d (call-interactively
           'emacspeak-pronounce-define-pronunciation t))
      (?D (call-interactively 'emacspeak-pronounce-define-template-pronunciation t))
      (?e (call-interactively
           'emacspeak-pronounce-edit-pronunciations t))
      (?l (call-interactively 'emacspeak-pronounce-load-dictionaries))
      (?r (call-interactively 'emacspeak-pronounce-refresh-pronunciations))
      (?s (call-interactively 'emacspeak-pronounce-save-dictionaries))
      (?t (call-interactively
           'emacspeak-pronounce-toggle-use-of-dictionaries))
      (?v (call-interactively 'emacspeak-pronounce-toggle-voice))
      (otherwise (message emacspeak-pronounce-help)))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{ Helpers: pronouncers

;;{{{ dates and numbers
(defvar emacspeak-pronounce-number-pattern
  "[0-9]+\\.?[0-9]+%?"
  "Pattern that matches  nnnn.nnnn")

(defvar emacspeak-pronounce-date-mm-dd-yyyy-pattern
  "[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\([0-9]\\{2\\}\\)?"
  "Pattern that matches dates of the form mm-dd-[cc]yy.")
(declare-function calendar-date-string "calendar" (date &optional abbreviate nodayname))

(defun emacspeak-pronounce-mm-dd-yyyy-date (string)
  "Return pronunciation for mm-dd-yyyy dates."
  (save-match-data
    (let ((fields (mapcar #'read (split-string string "-"))))
       (calendar-date-string
        (list (cl-second fields)
              (cl-first fields)
              (cond
               ((< (cl-third fields) 50)
                (+ 2000 (cl-third fields)))
               ((< (cl-third fields) 100)
                (+ 1900 (cl-third fields)))
               (t (cl-third fields)))))
        )))

(defvar emacspeak-pronounce-date-yyyymmdd-pattern
  "[0-9]\\{8\\}"
  "Pattern that matches dates of the form 20130101")

(defun emacspeak-pronounce-yyyymmdd-date (string)
  "Return pronunciation for yyyymmdd dates."
  (save-match-data
     (calendar-date-string
      (list
       (read (substring string 4 6))
       (read (substring string 6))
       (read (substring string 0 4)))
      nil 'nodayname)))

;;}}}
;;{{{ phone numbers

(defvar emacspeak-pronounce-us-phone-number-pattern "1?-?[0-9]\\{3\\}-[0-9]\\{3\\}-[0-9]\\{4\\}"
  "Pattern that matches US phone numbers.")

(defun emacspeak-pronounce-us-phone-number (phone)
  "Return pronunciation for US phone number."
  (when (= 14 (length phone))
    (setq phone (substring phone 2)))
  (let ((area-code (substring phone 0 3))
        (prefix-code (substring phone 4 7))
        (suffix-code (substring phone 8 12)))
    (unless (string-equal "800" area-code)
      (setq area-code
            (replace-regexp-in-string
             "[0-9]" " \\&" area-code)))
    (setq prefix-code
          (replace-regexp-in-string
           "[0-9]" " \\&" prefix-code))
    (setq suffix-code
          (replace-regexp-in-string
           "[0-9]\\{2\\}" " \\&" suffix-code))
    (format "%s %s, %s. "
            area-code prefix-code suffix-code)
    ))
(defvar emacspeak-pronounce-sha-checksum-pattern
  "[0-9a-f]\\{40\\}"
  "Regexp pattern that matches 40-digit SHA check-sum.")

(defun emacspeak-pronounce-sha-checksum (sha)
  "Return pronunciation for 40 digit SHA hash. Useful for working with Git among other things."
  (when
      (and
       (= 40 (length sha))
       (string-match "[0-9a-f]+" sha))
    (format "sha: %s " (substring sha 0 6))))

(defvar emacspeak-pronounce-uuid-pattern
  (concat 
   "[0-9a-f]\\{8\\}" "-"
   "[0-9a-f]\\{4\\}" "-"
   "[0-9a-f]\\{4\\}" "-"
   "[0-9a-f]\\{4\\}" "-"
   "[0-9a-f]\\{12\\}")
  "Regexp pattern that matches hex-encoded, human-readable UUID.")

(defun emacspeak-pronounce-uuid (uuid)
  "Return pronunciation for human-readable UUID."
  (cl-declare (special emacspeak-pronounce-uuid-pattern))
  (when (and (= 36 (length uuid))
             (string-match emacspeak-pronounce-uuid-pattern uuid))
    (format "uid: %s..%s "
            (substring uuid 0 2)
            (substring uuid -2 nil))))

;;}}}

;;}}}
;;{{{Text Mode Pronunciations:

(emacspeak-pronounce-add-dictionary-entry
      'text-mode
      (concat " -" emacspeak-pronounce-number-pattern)
      (cons
       #'re-search-forward
       #'(lambda (number)
           (concat
            " minus "
            (substring number 1)))))

;;}}}
(provide 'emacspeak-pronounce)
;;{{{ emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
