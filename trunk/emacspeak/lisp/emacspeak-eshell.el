;;; emacspeak-eshell.el --- Speech-enable EShell - Emacs Shell
;;; $Id$
;;; $Author$
;;; Description:   Speech-enable EShell
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'backquote)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (load)
  (require 'esh-arg))
;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; EShell is a shell implemented entirely in Emacs Lisp.
;;; It is part of emacs 21 --and can also be used under
;;; Emacs 20.
;;; This module speech-enables EShell

;;}}}
;;{{{  setup various EShell hooks

;;; Play an auditory icon as you display the prompt 
(add-hook 'eshell-after-prompt-hook
          (function
           (lambda nil
             (emacspeak-serve-auditory-icon 'item))))
;;; Speak command output 
(add-hook 'eshell-post-command-hook
          (function 
           (lambda nil
             (declare (special eshell-last-input-end
                               eshell-last-output-end
                               eshell-last-output-start))
             (emacspeak-speak-region eshell-last-input-end
                                     eshell-last-output-end)))
          t)

;;}}}
;;{{{  Advice PComplete --may be factored out later:

(defadvice pcomplete-list (after emacspeak pre act )
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-auditory-icon 'help)))

(defadvice pcomplete (around emacspeak pre act)
  "Say what you completed."
  (cond
   ((interactive-p)
    (emacspeak-kill-buffer-carefully "*Completions*")
    (let ((prior (point ))
          (emacspeak-speak-messages nil)m)
      ad-do-it
      (when (> (point) prior)
        (tts-with-punctuations "all"
                               (dtk-speak
                                (buffer-substring prior
                                                  (point)))))
      (let ((completions-buffer (get-buffer "*Completions*")))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (emacspeak-auditory-icon 'help)
          (switch-to-buffer completions-buffer)))))
   (t ad-do-it))
  ad-return-value)

(defadvice pcomplete-show-completions (around emacspeak pre act comp)
  (let ((emacspeak-speak-messages nil))
    ad-do-it))

;;}}}
;;{{{  Advice top-level EShell 

(defadvice eshell (after emacspeak pre act )
  "Announce switching to shell mode.
Provide an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object )
    (emacspeak-setup-programming-mode)
    (emacspeak-dtk-sync)
    (emacspeak-speak-line)))

;;}}}
;;{{{ advice em-hist 

(loop for f in 
      '(eshell-next-input eshell-previous-input
                          eshell-next-matching-input
                          eshell-previous-matching-input
                          eshell-next-matching-input-from-input
                          eshell-previous-matching-input-from-input)
      do
      (eval
       (`
        (defadvice (, f) (after  emacspeak pre act comp)
          "Speak selected command."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (save-excursion
              (beginning-of-line)
              (eshell-skip-prompt)
              (emacspeak-speak-line 1)))))))

;;}}}
;;{{{  advice em-ls

(defgroup emacspeak-eshell nil
  "EShell on the Emacspeak Audio Desktop."
  :group 'emacspeak
  :group 'eshell
  :prefix "emacspeak-eshell-")

(defcustom emacspeak-eshell-ls-use-personalities t
  "Indicates if ls in eshell uses different voice
personalities."
  :type 'boolean
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-directory-personality 'ursula
  "Personality for directory names."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-symlink-personality 'harry
  "Personality for symlinks."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-executable-personality 'paul-animated
  "Personality for executables."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-readonly-personality 'paul-monotone
  "Personality for read only files."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-unreadable-personality 'kid 
  "Personality for files that are not readable."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-special-personality 'paul-smooth 
  "Personality for special files."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-missing-personality 'paul-italic
  "Personality for missing file."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-archive-personality 'paul-surprized
  "Personality for archive files."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-backup-personality 'paul-monotone 
  "Personality for backup files. "
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom emacspeak-eshell-ls-product-personality 'paul-bold
  "Personality for files that can be recreated."
  :type 'symbol
  :group 'emacspeak-eshell)

(defcustom  emacspeak-eshell-ls-clutter-personality 'paul-monotone
  "Personality for transients."
  :type 'symbol
  :group 'emacspeak-eshell)

(defadvice  eshell-ls-decorated-name (around emacspeak pre act comp)
  "Voiceify the name if requested."
  (cond
   (emacspeak-eshell-ls-use-personalities
    ad-do-it
    (let ((result ad-return-value)
          (file (ad-get-arg 0))
          (personality nil))
      (setq personality
            (cond
             ((not (cdr file)) emacspeak-eshell-ls-missing-personality)
             ((stringp (cadr file)) emacspeak-eshell-ls-symlink-personality)
             ((eq (cadr file) t) emacspeak-eshell-ls-directory-personality)
             ((not (eshell-ls-filetype-p (cdr file) ?-))emacspeak- eshell-ls-special-personality)
             ((and (/= (user-uid) 0)    ; root can execute anything
                   (eshell-ls-applicable (cdr file) 3
                                         'file-executable-p (car file)))
              emacspeak-eshell-ls-executable-personality)
             ((not (eshell-ls-applicable (cdr file) 1
                                         'file-readable-p (car file)))
              emacspeak-eshell-ls-unreadable-personality)

             ((string-match eshell-ls-archive-regexp (car file))
              emacspeak-eshell-ls-archive-personality)

             ((string-match eshell-ls-backup-regexp (car file))
              emacspeak-eshell-ls-backup-personality)

             ((string-match eshell-ls-product-regexp (car file))
              emacspeak-eshell-ls-product-personality)

             ((string-match eshell-ls-clutter-regexp (car file))
              emacspeak-eshell-ls-clutter-personality)

             ((not (eshell-ls-applicable (cdr file) 2
                                         'file-writable-p (car file)))
              emacspeak-eshell-ls-readonly-personality)))
      (if personality
          (add-text-properties 0 (length result)
                               (list 'personality personality)
                               result)))
    result)
   (t ad-return-value)))

;;}}}
;;{{{ Advice em-prompt
(loop for f in 
      '(eshell-next-prompt eshell-previous-prompt
                           eshell-forward-matching-input  eshell-backward-matching-input)
      do
      (eval
       (`
        (defadvice (, f) (after  emacspeak pre act comp)
          "Speak selected command."
          (when (interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-auditory-icon 'select-object)
              (emacspeak-speak-line 1)))))))

;;}}}
;;{{{  advice esh-arg

(loop for f in 
      '(eshell-insert-buffer-name
        eshell-insert-process
        eshell-insert-envvar) 
      do
      (eval 
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Speak output."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line))))))

(defadvice eshell-insert-process (after emacspeak pre
                                        act comp)
  "Speak output."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))
;;}}}
;;{{{ advice esh-mode
(defadvice eshell-delchar-or-maybe-eof (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (cond
     ((= (point) (point-max))
      (message "Sending EOF to comint process"))
     (t (dtk-tone 500 30 'force)
        (and emacspeak-delete-char-speak-deleted-char
             (emacspeak-speak-char t))))
    ad-do-it
    (and emacspeak-delete-char-speak-current-char
         (emacspeak-speak-char t)))
   (t ad-do-it))
  ad-return-value)

(defadvice eshell-delete-backward-char (around emacspeak pre act)
  "Speak character you're deleting."
  (cond
   ((interactive-p )
    (dtk-tone 500 30 'force)
    (and emacspeak-backward-delete-char-speak-deleted-char
         (emacspeak-speak-this-char (preceding-char )))
    ad-do-it
    (and emacspeak-backward-delete-char-speak-current-char
         (emacspeak-speak-this-char  (preceding-char ))))
   (t ad-do-it))
  ad-return-value)

(defadvice eshell-show-output (after emacspeak pre act comp)
  "Speak output."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-region (point) (mark)))))
(defadvice eshell-mark-output (after emacspeak pre act comp)
  "Speak output."
  (when (interactive-p)
    (let ((emacspeak-show-point t)
          (voice-lock-mode t))
      (emacspeak-auditory-icon 'mark-object)
      (emacspeak-speak-line))))
(defadvice eshell-kill-output (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Flushed output")))

(defadvice eshell-kill-input (before emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object )
    (emacspeak-speak-line)))

(defadvice eshell-toggle (after emacspeak pre act comp)
  "Provide spoken context feedback."
  (when (interactive-p)
    (cond
     ((eq major-mode 'eshell-mode)
      (emacspeak-setup-programming-mode)
      (emacspeak-speak-line))
     (t (emacspeak-speak-mode-line)))
    (emacspeak-auditory-icon 'select-object)))
(defadvice eshell-toggle-cd (after emacspeak pre act comp)
  "Provide spoken context feedback."
  (when (interactive-p)
    (cond
     ((eq major-mode 'eshell-mode)
      (emacspeak-speak-line))
     (t (emacspeak-speak-mode-line)))
    (emacspeak-auditory-icon 'select-object)))

;;}}}

(provide 'emacspeak-eshell)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
