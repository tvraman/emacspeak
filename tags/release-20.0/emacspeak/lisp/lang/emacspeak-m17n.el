;;; emacspeak-m17n.el --- set up for Multilingual support in Emacspeak.

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Koichi INOUE <inoue@argv.org>
;; Keywords: i18n, multimedia, extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Loads and set up everything needed for m17n support.

;;; Code:

;;{{ Add load-path
(eval-when-compile
(let ((here (file-name-directory load-file-name)))
  (or (member here load-path)
      (setq load-path
	    (cons here load-path )))
  (let ((default-directory here))
    (normal-top-level-add-subdirs-to-load-path)))
;;}}
;;{{ Require
(require 'cl)
(require 'dtk-speak)
(require 'emacspeak-m17n-override))
;;}}
;;{{ Override functions
(require 'emacspeak-m17n-override)
;;}}
;;{{ Additional variables
(defvar dtk-speaker-process-coding-system nil
"Coding system to use for process I/O with the speech server.")

(defvar dtk-default-language nil
  "Default language used when no property is set.")

(defvar emacspeak-m17n-auto-put-language-mode nil
  "If t, guess language of characters automatically.")

(defvar emacspeak-m17n-put-language-strategy nil
  "How to determine language automatically.")

(defvar emacspeak-m17n-put-language-internal-strategy nil
  "Put-Language strategy used for string without language property.")

(defvar emacspeak-m17n-rate-offset-alist 
  '((en . 0) (ja . 0))
  "alist of language and rate offset.")

(defvar emacspeak-display-table-alist nil
  "Map from language to auditory-display-table.\n
Car of each cell is naming symbol of language and cdr is corresponding\n
auditory-display-table.")

(defcustom emacspeak-character-echo-non-ascii nil
  "If t, then emacspeak echoes characters  as you type,
even if it is a non-ascii character."
  :group 'emacspeak-speak
  :type 'boolean)

;;}}
;;{{ Generic m17n support functions
(defun emacspeak-m17n-register-display-table (lang table)
  "Register the association of language and auditory-display-table name."
  (setq emacspeak-display-table-alist
	(append (cons (cons lang table) nil)
		emacspeak-display-table-alist)))

(defsubst emacspeak-get-display-table (lang)
  "Get auditory-display-table corresponding to language."
  (let ((tab (assq lang emacspeak-display-table-alist)))
    (cdr tab)))

(defun emacspeak-m17n-get-phonetic-string (char lang)
  "Get phonetic string of char according to its language."
  (let ((table (emacspeak-get-display-table lang)))
    (cond
     ((and (eq lang 'ja) (featurep 'emacspeak-m17n-ja))
      (emacspeak-ja-convert-char-to-phonetic char))
     (t (char-to-string char)))
))

(defun emacspeak-m17n-get-cursor-string (char lang)
  "Get string for speech with cursor movement according to its language."
  (let ((table (emacspeak-get-display-table lang)))
    (cond
     ((and (eq lang 'ja) (featurep 'emacspeak-m17n-ja))
       (bep-get-phonetic-string char))
     (t (char-to-string char)))
))
;;}}
;;{{
(defun emacspeak-m17n-put-language-region (beg end &optional len)
  "Put language properties according to current language configuration."
  (interactive "r")
  (let ((inhibit-read-only t) (buffer-undo-list t)
	(modified (buffer-modified-p))
	before-change-functions after-change-functions)
    (unwind-protect
	(save-match-data
	  (save-excursion
	    (and emacspeak-m17n-put-language-strategy
		 (funcall emacspeak-m17n-put-language-strategy beg end))))
      ;;; Clean up
      (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))

(defun emacspeak-m17n-put-language-internal (beg end &optional len)
  "Put language properties according to current language configuration."
  (interactive "r")
  (declare (special emacspeak-m17n-auto-put-language-mode))
  (when (and emacspeak-m17n-auto-put-language-mode
	     (emacspeak-buffer-visible-p))
  (let ((inhibit-read-only t) (buffer-undo-list t)
	(modified (buffer-modified-p))
	before-change-functions after-change-functions
	(begm (max (window-start) beg))
	(endm (min (window-end) end)))
    (unwind-protect
	(save-match-data
	  (save-excursion
	    (and emacspeak-m17n-put-language-strategy
		 (funcall emacspeak-m17n-put-language-strategy begm endm len))))
      ;;; Clean up
      (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil))))))

(defun emacspeak-m17n-put-language-string-internal (str)
  "Return string with language property, which is put by
emacspeak-m17n-put-language-internal-strategy"
  (with-temp-buffer
    (insert str)
    (let ((emacspeak-m17n-auto-put-language-mode t))
      (save-match-data
	(and emacspeak-m17n-put-language-internal-strategy
	     (funcall emacspeak-m17n-put-language-internal-strategy
		      (point-min) (point-max)))))
    (buffer-string)))

;;}}
;;{{ Put language property

(defun emacspeak-m17n-auto-put-language-mode (&optional arg)
  "Toggle auto-put-language-mode.
Enabled if prefix argument is positive, and disabled
if negative."
  (interactive "P")
  (set (make-local-variable 'emacspeak-m17n-auto-put-language-mode)
       (if arg (> (prefix-numeric-value arg) 0)
	 (not emacspeak-m17n-auto-put-language-mode)))
  (cond
   (emacspeak-m17n-auto-put-language-mode
    (emacspeak-m17n-put-language-install))
   (t
    (emacspeak-m17n-put-language-uninstall)))
  (when (interactive-p)
    (message (concat "Turned " (if emacspeak-m17n-auto-put-language-mode "on" "off")
	       " auto language assignment"))))

;; provide language property automatically.
;; Idea is the same as lazy-voice-lock-mode uses.
(defun emacspeak-m17n-put-language-install ()
  (add-hook 'after-change-functions 'emacspeak-m17n-put-language-internal)
  (add-hook 'window-scroll-functions 'emacspeak-m17n-put-language-after-scroll)
  (add-hook 'window-size-change-functions 'emacspeak-m17n-put-language-after-resize)
  (add-hook 'before-change-functions 'emacspeak-m17n-put-language-arrange-before-change))

(defun emacspeak-m17n-put-language-uninstall ()
  (remove-hook 'after-change-functions 'emacspeak-m17n-put-language-internal)
  (remove-hook 'window-scroll-functions 'emacspeak-m17n-put-language-after-scroll)
  (remove-hook 'window-size-change-functions 'emacspeak-m17n-put-language-after-resize)
  (remove-hook 'before-change-functions 'emacspeak-m17n-put-language-arrange-before-change))

(defun emacspeak-m17n-put-language-after-scroll (window window-start)
  ;; Called from `window-scroll-functions'.
  ;; Borrowed from `lazy-voice-lock.el'.
  (save-excursion
    (goto-char window-start)
    (vertical-motion (window-height window) window)
    (emacspeak-m17n-put-language-region window-start (point)))
  (set-window-redisplay-end-trigger window nil))

(defun emacspeak-m17n-put-language-after-trigger (window trigger-point)
  ;; Called from `redisplay-end-trigger-functions'.
  ;; Borrowed from `lazy-voice-lock.el'.
  (save-excursion
    (goto-char (window-start window))
    (vertical-motion (window-height window) window)
    (emacspeak-m17n-put-language-region trigger-point (point))))

(defun emacspeak-m17n-put-language-after-resize (frame)
  ;; Called from `window-size-change-functions'.
  ;; Borrowed from `lazy-voice-lock.el'.
  (save-excursion
    (save-selected-window
      (select-frame frame)
      (walk-windows (function (lambda (window)
		       (set-buffer (window-buffer window))
		       (emacspeak-m17n-put-language-region
			(window-start window) (window-end window))
		       (set-window-redisplay-end-trigger window nil)))
		    'nomini frame))))

(defun emacspeak-m17n-put-language-arrange-before-change (beg end)
  ;; Called from `before-change-functions'.
  ;; Borrowed from `lazy-voice-lock.el'.
  (unless (eq beg end)
    (let ((windows (get-buffer-window-list (current-buffer) 'nomini t)) window)
      (while windows
	(setq window (car windows))
	(unless (markerp (window-redisplay-end-trigger window))
	  (set-window-redisplay-end-trigger window (make-marker)))
	(set-marker (window-redisplay-end-trigger window) (window-end window))
	(setq windows (cdr windows))))))
;;}}
;;{{ Helper functions
(defun emacspeak-buffer-visible-p (&optional buffer)
  "See whether current buffer is visible.
If visible on some window, return the window.
If BUFFER is not specified, see if currentbuffer is visible."
  (let ((ret nil)
	(buf (if buffer buffer (current-buffer))))
    (walk-windows
     (function (lambda (window)
	(when (eq buf (window-buffer window))
	  (setq ret window))))
     nil t)
    ret))

(defun emacspeak-buffer-portion-visible-p (beg end &optional buffer)
  "See whether specified portion of current buffer is visible.
If visible on some window, return the window.
If BUFFER is not specified, see if currentbuffer is visible."
  (let (ret
	(buf (if buffer buffer (current-buffer))))
    (walk-windows
     (function (lambda (window)
	(when (eq buf (window-buffer window))
	  (setq ret window))))
     nil t)
    (if (and (> beg (window-start ret))
	     (< end (window-end ret)))
	ret)))

(defun emacspeak-m17n-maybe-last-input-language ()
  "Return language of maybe-last-input character or default-language"
  (let (lang)
    (and (> (point) (point-min))
	 (setq lang (get-text-property (1- (point)) 'emacspeak-language)))
    (or lang
	dtk-default-language)
))

(defun emacspeak-m17n-get-rate-offset (lang)
  (let ((off (assoc lang emacspeak-m17n-rate-offset-alist)))
    (if off
	(cdr off) 0)
))

(defun emacspeak-m17n-sync-rate-offset ()
  "Send language-specific rate offset to speech server."
  (interactive)
  (mapcar
   (function (lambda (ent)
	       (process-send-string dtk-speaker-process
				    (format "tts_set_rate_offset %s %s\n"
					    (symbol-name (car ent))
					    (cdr ent)))))
	     emacspeak-m17n-rate-offset-alist))

(defun emacspeak-m17n-set-rate-offset (lang offset)
  (interactive "SWhat language: \nnOffset: ")
  (let ((rate-offset (assq lang emacspeak-m17n-rate-offset-alist)))
    (if rate-offset
	(setcdr rate-offset offset)
      (setq emacspeak-m17n-rate-offset-alist
	    (append emacspeak-m17n-rate-offset-alist (list (cons lang offset)))))
      (emacspeak-m17n-sync-rate-offset)
))

;;}}
;;{{ Redefinition of emacspeak-speak functions
(defun emacspeak-m17n-speak-this-char (char &optional lang)
  "Speak this CHAR."
  (let ((dtk-stop-immediately t )
	(lang (if lang lang 'en)))
    (when char
      (emacspeak-handle-action-at-point)
      (cond
       ((not (eq lang 'en))		; chara with language property
	(dtk-speak (emacspeak-m17n-get-cursor-string char lang) nil lang))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char )))))))

;;}}
;;{{ Global Initialization

(when emacspeak-m17n-auto-put-language-mode
  (add-hook 'after-change-functions 'emacspeak-m17n-put-language-internal)
  (add-hook 'window-scroll-functions 'emacspeak-m17n-put-language-after-scroll)
  (add-hook 'window-size-change-functions 'emacspeak-m17n-put-language-after-resize)
  (add-hook 'before-change-functions 'emacspeak-m17n-put-language-arrange-before-change)
)
;;}}
;;{{ Final setup
;;}}
(provide 'emacspeak-m17n)
;;; emacspeak-m17n.el ends here
