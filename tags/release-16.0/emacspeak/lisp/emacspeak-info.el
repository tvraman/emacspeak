;;; emacspeak-info.el --- Speech enable Info -- Emacs' online documentation viewer
;;; $Id$
;;; $Author$ 
;;; Description:  Module for customizing Emacs info
;;; Keywords:emacspeak, audio interface to emacs
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'info)
  (require 'dtk-speak))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'dtk-voices)
(require 'emacspeak-keymap)
;;{{{ Introduction:

;;; This module extends and customizes the Emacs info reader.

;;}}}
;;{{{  Variables:

(defvar Info-voiceify t
  "*Non-nil enables highlighting and voices in Info nodes.")

(defvar Info-voiceify-maximum-menu-size 30000
  "*Maximum size of menu to voiceify if `Info-voiceify' is non-nil.")



;;}}}
;;{{{  Voices 

(defvar Info-title-personality-alist
  '((?* bold)
    (?= bold-italic)
    (?-  underline))
  "*Alist of personality or list of personalities to use for pseudo-underlined titles.
The alist key is the character the title is underlined with (?*, ?= or ?-).")
  
(defvar emacspeak-info-node   'harry
"Personality to indicate the node name.")

(defvar emacspeak-info-xref
  'ursula
  "Personality to indicate  cross-references.")

(defvar emacspeak-info-menu-5
  'betty
  "Personality for menu items.")


;;}}}
;;{{{  Voiceify a node 

;;; Cloned from info.el Info-fontify-node 
(defun Info-voiceify-node ()
  (declare (special Info-title-personality-alist Info-current-node))
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (setq voice-lock-mode t)
      (if (looking-at "^File: [^,: \t]+,?[ \t]+")
	  (progn
	    (goto-char (match-end 0))
	    (while
		(looking-at "[ \t]*[^:, \t\n]+:[ \t]+\\([^:,\t\n]+\\),?")
	      (goto-char (match-end 0))
	      (put-text-property (match-beginning 1) (match-end 1)
				 'personality emacspeak-info-xref))))
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*+\\|=+\\|-+\\)$"
                                nil t)
	(put-text-property (match-beginning 1) (match-end 1)
			   'personality
			   (cdr (assq (preceding-char) Info-title-personality-alist)))
	;; This is a serious problem for trying to handle multiple
	;; frame types at once.  We want this text to be invisible
	;; on frames that can display the voice above.
	(if (memq (framep (selected-frame)) '(x pc w32))
	    (put-text-property (match-end 1) (match-end 2)
			       'invisible t)))
      (goto-char (point-min))
      (while (re-search-forward "\\*Note[ \n\t]+\\([^:]*\\):" nil t)
	(if (= (char-after (1- (match-beginning 0))) ?\") ; hack
	    nil
	  (put-text-property (match-beginning 1) (match-end 1)
			     'personality emacspeak-info-xref)))
      (goto-char (point-min))
      (if (and (search-forward "\n* Menu:" nil t)
	       (not (string-match "\\<Index\\>" Info-current-node))
	       ;; Don't take time to annotate huge menus
	       (< (- (point-max) (point)) Info-voiceify-maximum-menu-size))
	  (let ((n 0))
	    (while (re-search-forward "^\\* \\([^:\t\n]*\\):" nil t)
	      (setq n (1+ n))
	      (if (memq n '(5 9))       ; visual aids to help with 1-9 keys
		  (put-text-property (match-beginning 0)
				     (1+ (match-beginning 0))
				     'personality emacspeak-info-menu-5))
	      (put-text-property (match-beginning 1) (match-end 1)
				 'personality emacspeak-info-node))))
      (set-buffer-modified-p nil))))

;;}}}
;;{{{ advice

;;; Advice Info mode to voice lock 

(defadvice Info-mode (after emacspeak pre act)
  "Set up voice locking if requested. 
See variable `Info-voiceify`"
  (Info-voiceify-node))

(defcustom  emacspeak-info-select-node-speak-chunk 'screenfull 
"*Specifies how much of the selected node gets spoken.
Possible values are:
screenfull  -- speak the displayed screen
node -- speak the entire node."
:type '(menu-choice
(const screenful)
(const node))
:group 'emacspeak)


(defsubst emacspeak-info-speak-current-window ()
  "Speak current window in info buffer."
  (let ((start  (point ))
        (window (get-buffer-window (current-buffer ))))
    (save-excursion 
      (forward-line (window-height window))
      (emacspeak-speak-region start (point )))))

(defadvice Info-select-node (after emacspeak pre act)
  "Voiceify the Info node if requested. 
Speak the selected node based on setting of
emacspeak-info-select-node-speak-chunk"
  (declare (special emacspeak-info-select-node-speak-chunk))
  (let ((dtk-stop-immediately t ))
    (emacspeak-auditory-icon 'select-object)
    (and Info-voiceify (Info-voiceify-node))
    (cond
     ((eq emacspeak-info-select-node-speak-chunk
          'screenfull)
      (emacspeak-info-speak-current-window))
     ((eq emacspeak-info-select-node-speak-chunk 'node)
      (emacspeak-speak-buffer ))
     (t (emacspeak-speak-line)))))


(defadvice info (after emacspeak pre act)
  "Cue user that info is up."
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-line)))


(defadvice Info-scroll-up (after emacspeak pre act) 
  "Speak the screenful."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (let ((start  (point ))
          (window (get-buffer-window (current-buffer ))))
      (save-excursion 
        (forward-line (window-height window))
        (emacspeak-speak-region start (point ))))))

(defadvice Info-scroll-down (after emacspeak pre act) 
  "Speak the screenful."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (let ((start  (point ))
          (window (get-buffer-window (current-buffer ))))
      (save-excursion 
        (forward-line (window-height window))
        (emacspeak-speak-region start (point ))))))

(defadvice Info-exit (after emacspeak pre act)
  "Play an auditory icon to close info,
and then cue the next selected buffer."
  (when (interactive-p )
    (dtk-stop)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))


(defadvice Info-next-reference (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line)))

(defadvice Info-prev-reference (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line)))

;;}}}
;;{{{ Speak header line if hidden

(defun emacspeak-info-speak-header ()
  "Speak info header line."
  (interactive)
  (declare (special Info-use-header-line
Info-header-line))
  (let ((voice-lock-mode t))
  (cond
   ((and (boundp 'Info-use-header-line)
        (boundp 'Info-header-line)
        Info-header-line)
   (dtk-speak Info-header-line))
   (t (save-excursion
        (beginning-of-buffer)
        (emacspeak-speak-line))))))

;;}}}
;;{{{  Emacs 21 
;;; There is a bug in Emacs 21 that causes info-extract-pointer to be
;;; called erroneously.

(defadvice Info-extract-pointer  (around emacspeak pre act comp)
  "Silence emacspeak during call."
  (let ((emacspeak-speak-messages nil)
        (emacspeak-use-auditory-icons nil))
    ad-do-it))
;;}}}
;;{{{ keymaps
(declaim (special Info-mode-map))
(define-key Info-mode-map "T" 'emacspeak-info-speak-header)
(define-key Info-mode-map "'" 'emacspeak-speak-rest-of-buffer)
;;}}}
(provide  'emacspeak-info)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
