;;; emacspeak-view.el --- Speech enable View mode -- Efficient browsing of read-only content
;;; $Id$
;;; $Author$ 
;;; DescriptionEmacspeak extensions for view
;;; Keywords:emacspeak, audio interface to emacs, view-mode
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
;;; Copyright (c) 1996 by T. V. Raman 
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

;;{{{  Introduction:

;;; Provide additional advice to view-mode

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  Setup view mode to work with emacspeak

;;; restore emacspeak keybindings:
(declaim (special emacspeak-prefix))
(add-hook 'view-mode-hook
          (function (lambda ()
                      (local-unset-key emacspeak-prefix )
		      (emacspeak-view-setup-keys))))
;;; Generate automatic advise:

;;}}}
;;{{{ additional interactive commands

(defun emacspeak-view-line-to-top ()
  "Moves current line to top of window"
  (interactive)
  (recenter 0)
  (emacspeak-speak-line)
  (emacspeak-auditory-icon 'select-object))

;;}}}
;;{{{ Advise additional interactive commands:
(defadvice view-file (after emacspeak pre act comp)
  "Load directory specific settings"
  (emacspeak-speak-load-directory-settings)
  (outline-minor-mode 1))

(defadvice view-mode (after emacspeak pre act comp)
  "Announce what happened"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-load-directory-settings)
    (outline-minor-mode 1)
    (if view-mode
	(message "Entered view mode Press %s to exit"
		 (key-description
		  (where-is-internal 'View-exit view-mode-map 'firstonly)))
      (message "Exited view mode"))))

(defadvice View-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice View-exit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice View-leave (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice View-search-regexp-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice View-search-regexp-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice View-search-last-regexp-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice View-search-last-regexp-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice view-exit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice View-scroll-one-more-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defadvice View-scroll-line-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))

(defadvice View-scroll-line-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))
(defadvice View-scroll-page-forward-set-page-size (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (dtk-speak (emacspeak-get-window-contents)))))

(defadvice View-scroll-page-backward-set-page-size (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (dtk-speak (emacspeak-get-window-contents)))))

(defadvice View-scroll-half-page-forward (around emacspeak pre act comp)
  "Read newly scrolled contents"
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (end-of-line)
        (dtk-speak
         (buffer-substring
          start (point))))))
   (t ad-do-it))
  ad-return-value)
(defadvice View-scroll-half-page-backward (around emacspeak pre act comp)
  "Read newly scrolled contents"
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (end-of-line)
        (dtk-speak
         (buffer-substring
          start (point))))))
   (t ad-do-it))
  ad-return-value)

(defadvice View-scroll-lines-forward-set-scroll-size (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (forward-line (window-height))
        (emacspeak-speak-region start (point ))))))

(defadvice View-scroll-lines-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice View-scroll-lines-backward (around  emacspeak pre act comp)
  "provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((buffer (current-buffer)))
      ad-do-it
      (cond
       ((not (eq buffer (current-buffer))) ;we exitted view mode 
        (emacspeak-auditory-icon 'close-object)
        (emacspeak-speak-mode-line))
       (t (emacspeak-auditory-icon 'scroll)
          (dtk-speak (emacspeak-get-window-contents))))))
   (t ad-do-it))
  ad-return-value)

(defadvice View-back-to-mark ( after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))

(defadvice View-goto-line (after emacspeak pre act comp)
  "Provide spoken feedback"
  (when (interactive-p)
    (let ((line-number
           (format "line %s"
                   (ad-get-arg 0 )))
          (voice-lock-mode t))
      (put-text-property 0 (length line-number)
                         'personality voice-annotate line-number)
      (emacspeak-auditory-icon 'large-movement)
      (dtk-speak
       (concat line-number
	       (thing-at-point 'line))))))
(defadvice View-scroll-to-buffer-end (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice View-goto-percent (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice View-revert-buffer-scroll-page-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice View-scroll-page-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice View-scroll-page-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

;;}}}
;;{{{ bind convenience keys
(defvar emacspeak-view-keys-optimized nil
  "Records if we have already optimized Emacspeak
keybindings for view mode")

(defvar emacspeak-view-edit-commands
  (list 'emacspeak-self-insert-command
        'completion-separator-self-insert-autofilling
        'completion-separator-self-insert-command
        'delete-char
        'completion-kill-region
        )
  "Editting commands that emacspeak should rebind in view mode")

(defun emacspeak-view-optimize-view-keys()
  "optimize keybindings for emacspeak in view mode"
  (declare (special emacspeak-view-edit-commands
                    emacspeak-view-keys-optimized
                    view-mode-map emacspeak-keymap))
  (unless emacspeak-view-keys-optimized
    (setq emacspeak-view-keys-optimized t)
    (loop for edit-command in emacspeak-view-edit-commands
          do
          (let ((edit-keys (where-is-internal edit-command view-mode-map)))
            (loop for key in edit-keys 
                  do
                  (let ((command (lookup-key emacspeak-keymap key)))
                    (when command
                      (define-key view-mode-map key command))))))))

(defun emacspeak-view-setup-keys()
  "Setup emacspeak convenience keys"
  (declare (special view-mode-map))
  (loop for i from 0 to 9
        do
        (define-key view-mode-map
          (format "%s" i)
          'emacspeak-speak-predefined-window))
;;;convenience keys
  (define-key view-mode-map "\C-j"
    'emacspeak-hide-speak-block-sans-prefix)
  (define-key view-mode-map "\M- " 'emacspeak-outline-speak-this-heading)
  (define-key view-mode-map "\M-n"
    'outline-next-visible-heading)
  (define-key view-mode-map "\M-p" 'outline-previous-visible-heading)
  (define-key view-mode-map " " 'scroll-up)
  (define-key view-mode-map "\d" 'scroll-down)
  (define-key view-mode-map "P" 'dtk-pause)
  (define-key view-mode-map "R" 'dtk-resume)
  (define-key view-mode-map "S" 'dtk-stop)
  (define-key view-mode-map "t" 'emacspeak-view-line-to-top)
  (define-key view-mode-map ","
    'emacspeak-speak-current-window)
  (define-key view-mode-map "\M-d"
    'emacspeak-pronounce-dispatch)
  (define-key view-mode-map "c" 'emacspeak-speak-char)
  (define-key view-mode-map "w" 'emacspeak-speak-word)
  (emacspeak-view-optimize-view-keys))

;;}}}
(provide  'emacspeak-view)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
