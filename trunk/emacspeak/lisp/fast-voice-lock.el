;;; fast-voice-lock.el --- Support mode for voice locking
;;; $Id$
;;; $Author$ 
;;; Description:  lazy Voice lock mode for Emacspeak
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interpersonality to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2000, T. V. Raman 
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
;;; fast-voice-lock.el --- Automagic text properties caching for fast Voice Lock mode.

;; Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Version: 3.10

;;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose:
;;
;; To make visiting a file in `voice-lock-mode' faster by restoring its personality
;; text properties from automatically saved associated Voice Lock cache files.
;;
;; See caveats and feedback below.
;; See also the lazy-lock package.  (But don't use the two at the same time!)

;; Installation:
;; 
;; Put in your ~/.emacs:
;;
;; (setq voice-lock-support-mode 'fast-voice-lock-mode)
;;
;; Start up a new Emacs and use voice-lock as usual (except that you can use the
;; so-called "gaudier" voiceification regexps on big files without frustration).
;;
;; When you visit a file (which has `voice-lock-mode' enabled) that has a
;; corresponding Voice Lock cache file associated with it, the Voice Lock cache
;; will be loaded from that file instead of being generated by Voice Lock code.

;; Caveats:
;;
;; A cache will be saved when visiting a compressed file using crypt++, but not
;; be read.  This is a "feature"/"consequence"/"bug" of crypt++.
;;
;; Version control packages are likely to stamp all over file modification
;; times.  Therefore the act of checking out may invalidate a cache.

;; Feedback:
;;
;; Feedback is welcome.
;; To submit a bug report (or make comments) please use the mechanism provided:
;;
;; M-x fast-voice-lock-submit-bug-report RET

;; History:
;;
;; 0.02--1.00:
;; - Changed name from turbo-prop to fast-voice-lock.  Automagic for voice-lock only
;; - Made `fast-voice-lock-mode' a minor mode, like G. Dinesh Dutt's fss-mode
;; 1.00--1.01:
;; - Turn on `fast-voice-lock-mode' only if `buffer-file-name' or `interactive-p'
;; - Made `fast-voice-lock-file-name' use `buffer-name' if `buffer-file-name' is nil
;; - Moved save-all conditions to `fast-voice-lock-save-cache'
;; - Added `fast-voice-lock-save-text-properties' to `kill-buffer-hook'
;; 1.01--2.00: complete rewrite---not worth the space to document
;; - Changed structure of text properties cache and threw out file mod checks
;; 2.00--2.01:
;; - Made `condition-case' forms understand `quit'. 
;; - Made `fast-voice-lock' require `voice-lock'
;; - Made `fast-voice-lock-cache-name' chase links (from Ben Liblit)
;; 2.01--3.00:
;; - Changed structure of cache to include `voice-lock-keywords' (from rms)
;; - Changed `fast-voice-lock-cache-mechanisms' to `fast-voice-lock-cache-directories'
;; - Removed `fast-voice-lock-read-others'
;; - Made `fast-voice-lock-read-cache' ignore cache owner
;; - Made `fast-voice-lock-save-cache-external' create cache directory
;; - Made `fast-voice-lock-save-cache-external' save `voice-lock-keywords'
;; - Made `fast-voice-lock-cache-data' check `voice-lock-keywords'
;; 3.00--3.01: incorporated port of 2.00 to Lucid, made by Barry Warsaw
;; - Package now provides itself
;; - Lucid: Use `voice-lock-any-extents-p' for `voice-lock-any-properties-p'
;; - Lucid: Use `list-personalities' for `personality-list'
;; - Lucid: Added `set-text-properties'
;; - Lucid: Made `turn-on-fast-voice-lock' pass 1 not t to `fast-voice-lock-mode'
;; - Removed test for `fast-voice-lock-mode' from `fast-voice-lock-read-cache'
;; - Lucid: Added Lucid-specific `fast-voice-lock-get-personality-properties'
;; 3.01--3.02: now works with Lucid Emacs, thanks to Barry Warsaw
;; - Made `fast-voice-lock-cache-name' map ":" to ";" for OS/2 (from Serganova Vera)
;; - Made `fast-voice-lock-cache-name' use abbreviated file name (from Barry Warsaw)
;; - Lucid: Separated handlers for `error' and `quit' for `condition-case'
;; 3.02--3.03:
;; - Changed `fast-voice-lock-save-cache-external' to `fast-voice-lock-save-cache-data'
;; 3.03--3.04:
;; - Corrected `subrp' test of Lucid code
;; - Replaced `voice-lock-any-properties-p' with `text-property-not-all'
;; - Lucid: Made `fast-voice-lock-set-personality-properties' put `text-prop' on extents
;; - Made `fast-voice-lock-cache-directories' a regexp alist (from Colin Rafferty)
;; - Made `fast-voice-lock-cache-directory' to return a usable cache file directory
;; 3.04--3.05:
;; - Lucid: Fix for XEmacs 19.11 `text-property-not-all'
;; - Replaced `subrp' test of Lucid code with `emacs-version' `string-match'
;; - Made `byte-compile-warnings' omit `unresolved' on compilation
;; - Made `fast-voice-lock-save-cache-data' use a buffer (from Rick Sladkey)
;; - Reverted to old `fast-voice-lock-get-personality-properties' (from Rick Sladkey)
;; 3.05--3.06: incorporated hack of 3.03, made by Jonathan Stigelman (Stig)
;; - Reverted to 3.04 version of `fast-voice-lock-get-personality-properties'
;; - XEmacs: Removed `list-personalities' `defalias'
;; - Made `fast-voice-lock-mode' and `turn-on-fast-voice-lock' succeed `autoload' cookies
;; - Added `fast-voice-lock-submit-bug-report'
;; - Renamed `fast-voice-lock-save-size' to `fast-voice-lock-minimum-size'
;; - Made `fast-voice-lock-save-cache' output a message if no save ever attempted
;; - Made `fast-voice-lock-save-cache-data' output a message if save attempted
;; - Made `fast-voice-lock-cache-data' output a message if load attempted
;; - Made `fast-voice-lock-save-cache-data' do `condition-case' not `unwind-protect'
;; - Made `fast-voice-lock-save-cache' and `fast-voice-lock-read-cache' return nothing
;; - Made `fast-voice-lock-save-cache' check `buffer-modified-p' (Stig)
;; - Added `fast-voice-lock-save-events'
;; - Added `fast-voice-lock-after-save-hook' to `after-save-hook' (Stig)
;; - Added `fast-voice-lock-kill-buffer-hook' to `kill-buffer-hook'
;; - Changed `fast-voice-lock-save-caches' to `fast-voice-lock-kill-emacs-hook'
;; - Added `fast-voice-lock-kill-emacs-hook' to `kill-emacs-hook'
;; - Made `fast-voice-lock-save-cache' check `verify-visited-file-modtime' (Stig)
;; - Made `visited-file-modtime' be the basis of the timestamp (Stig)
;; - Made `fast-voice-lock-save-cache-1' and `fast-voice-lock-cache-data' use/reformat it
;; - Added `fast-voice-lock-cache-filename' to keep track of the cache file name
;; - Added `fast-voice-lock-after-voiceify-buffer'
;; - Added `fast-voice-lock-save-personalities' list of personalities to save (idea from Stig/Tibor)
;; - Made `fast-voice-lock-get-personality-properties' functions use it
;; - XEmacs: Made `fast-voice-lock-set-personality-properties' do extents the Voice Lock way
;; - XEmacs: Removed fix for `text-property-not-all' (19.11 support dropped)
;; - Made `fast-voice-lock-mode' ensure `voice-lock-mode' is on
;; - Made `fast-voice-lock-save-cache' do `cdr-safe' not `cdr' (from Dave Foster)
;; - Made `fast-voice-lock-save-cache' do `set-buffer' first (from Dave Foster)
;; - Made `fast-voice-lock-save-cache' loop until saved or quit (from Georg Nikodym)
;; - Made `fast-voice-lock-cache-data' check `buffer-modified-p'
;; - Made `fast-voice-lock-cache-data' do `voice-lock-compile-keywords' if necessary
;; - XEmacs: Made `voice-lock-compile-keywords' `defalias'
;; 3.06--3.07:
;; - XEmacs: Add `fast-voice-lock-after-voiceify-buffer' to the Voice Lock hook
;; - Made `fast-voice-lock-cache-name' explain the use of `directory-abbrev-alist'
;; - Made `fast-voice-lock-mode' use `buffer-file-truename' not `buffer-file-name'
;; 3.07--3.08:
;; - Made `fast-voice-lock-read-cache' set `fast-voice-lock-cache-filename'
;; 3.08--3.09:
;; - Made `fast-voice-lock-save-cache' cope if `fast-voice-lock-minimum-size' is an a list
;; - Made `fast-voice-lock-mode' respect the value of `voice-lock-inhibit-thing-lock'
;; - Added `fast-voice-lock-after-unvoiceify-buffer'
;; 3.09--3.10:
;; - Rewrite for Common Lisp macros
;; - Made fast-voice-lock.el barf on a crap 8+3 pseudo-OS (Eli Zaretskii help)
;; - XEmacs: Made `add-minor-mode' succeed `autoload' cookie
;; - XEmacs: Made `fast-voice-lock-save-personalities' default to `voice-lock-personality-list'
;; - Made `fast-voice-lock-save-cache' use `voice-lock-value-in-major-mode'
;; - Wrap with `save-buffer-state' (Ray Van Tassle report)
;; - Made `fast-voice-lock-mode' wrap `voice-lock-support-mode'

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (load compile)
  (require 'dtk-voices)
  (provide 'fast-voice-lock) ;prevent byte compiler from recursing
  (require 'voice-lock))

(eval-when-compile
  ;;
  ;; We don't do this at the top-level as we only use non-autoloaded macros.
  (require 'cl)
  ;;
  ;; I prefer lazy code---and lazy mode.
  (setq byte-compile-dynamic t byte-compile-dynamic-docstrings t)
  ;;
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro save-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    (` (let* ((,@ (append varlist
		   '((modified (buffer-modified-p))
		     (inhibit-read-only t) (buffer-undo-list t)
		     before-change-functions after-change-functions
		     deactivate-mark buffer-file-name buffer-file-truename))))
	 (,@ body)
	 (when (and (not modified) (buffer-modified-p))
	   (set-buffer-modified-p nil)))))
  (put 'save-buffer-state 'lisp-indent-function 1))
(defvar fast-voice-lock-save-personalities nil )
(defun fast-voice-lock-submit-bug-report ()
  "Submit via mail a bug report on fast-voice-lock.el."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "simon@gnu.ai.mit.edu" "fast-voice-lock 3.10"
     '(fast-voice-lock-cache-directories fast-voice-lock-minimum-size
       fast-voice-lock-save-others fast-voice-lock-save-events fast-voice-lock-save-personalities)
     nil nil
     (concat "Hi Si.,

I want to report a bug.  I've read the `Bugs' section of `Info' on Emacs, so I
know how to make a clear and unambiguous report.  To reproduce the bug:

Start a fresh Emacs via `" invocation-name " -no-init-file -no-site-file'.
In the `*scratch*' buffer, evaluate:"))))

(defvar fast-voice-lock-mode nil)
(defvar fast-voice-lock-cache-timestamp nil)	; for saving/reading
(defvar fast-voice-lock-cache-filename nil)	; for deleting

;; User Variables:

(defvar fast-voice-lock-cache-directories '("." "~/.emacs-flc")
; - `internal', keep each file's Voice Lock cache file in the same file.
; - `external', keep each file's Voice Lock cache file in the same directory.
  "*Directories in which Voice Lock cache files are saved and read.
Each item should be either DIR or a cons pair of the form (REGEXP . DIR) where
DIR is a directory name (relative or absolute) and REGEXP is a regexp.

An attempt will be made to save or read Voice Lock cache files using these items
until one succeeds (i.e., until a readable or writable one is found).  If an
item contains REGEXP, DIR is used only if the buffer file name matches REGEXP.
For example:

 (let ((home (expand-file-name (abbreviate-file-name (file-truename \"~/\")))))
   (list (cons (concat \"^\" (regexp-quote home)) \".\") \"~/.emacs-flc\"))
    =>
 ((\"^/your/true/home/directory/\" . \".\") \"~/.emacs-flc\")

would cause a file's current directory to be used if the file is under your
home directory hierarchy, or otherwise the absolute directory `~/.emacs-flc'.")

(defvar fast-voice-lock-minimum-size (* 25 1024)
  "*Minimum size of a buffer for cached voiceification.
Only buffers more than this can have associated Voice Lock cache files saved.
If nil, means cache files are never created.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 25600) (c++-mode . 25600) (rmail-mode . 1048576))
means that the minimum size is 25K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.")

(defvar fast-voice-lock-save-events '(kill-buffer kill-emacs)
  "*Events under which caches will be saved.
Valid events are `save-buffer', `kill-buffer' and `kill-emacs'.
If concurrent editing sessions use the same associated cache file for a file's
buffer, then you should add `save-buffer' to this list.")

(defvar fast-voice-lock-save-others t
  "*If non-nil, save Voice Lock cache files irrespective of file owner.
If nil, means only buffer files known to be owned by you can have associated
Voice Lock cache files saved.  Ownership may be unknown for networked files.")



;; User Functions:

;;;###autoload
(defun fast-voice-lock-mode (&optional arg)
  "Toggle Fast Lock mode.
With arg, turn Fast Lock mode on if and only if arg is positive and the buffer
is associated with a file.  Enable it automatically in your `~/.emacs' by:

 (setq voice-lock-support-mode 'fast-voice-lock-mode)

If Fast Lock mode is enabled, and the current buffer does not contain any text
properties, any associated Voice Lock cache is used if its timestamp matches the
buffer's file, and its `voice-lock-keywords' match those that you are using.

Voice Lock caches may be saved:
 - When you save the file's buffer.
 - When you kill an unmodified file's buffer.
 - When you exit Emacs, for all unmodified or saved buffers.
Depending on the value of `fast-voice-lock-save-events'.
See also the commands `fast-voice-lock-read-cache' and `fast-voice-lock-save-cache'.

Use \\[voice-lock-voiceify-buffer] to voiceify the buffer if the cache is bad.

Various methods of control are provided for the Voice Lock cache.  In general,
see variable `fast-voice-lock-cache-directories' and function `fast-voice-lock-cache-name'.
For saving, see variables `fast-voice-lock-minimum-size', `fast-voice-lock-save-events',
`fast-voice-lock-save-others' and `fast-voice-lock-save-personalities'.

Use \\[fast-voice-lock-submit-bug-report] to send bug reports or feedback."
  (interactive "P")
  ;; Only turn on if we are visiting a file.  We could use `buffer-file-name',
  ;; but many packages temporarily wrap that to nil when doing their own thing.
  (set (make-local-variable 'fast-voice-lock-mode)
       (and buffer-file-truename
	    (not (memq 'fast-voice-lock-mode voice-lock-inhibit-thing-lock))
	    (if arg (> (prefix-numeric-value arg) 0) (not fast-voice-lock-mode))))
  (if (and fast-voice-lock-mode (not voice-lock-mode))
      ;; Turned on `fast-voice-lock-mode' rather than `voice-lock-mode'.
      (let ((voice-lock-support-mode 'fast-voice-lock-mode))
	(voice-lock-mode t))
    ;; Let's get down to business.
    (set (make-local-variable 'fast-voice-lock-cache-timestamp) nil)
    (set (make-local-variable 'fast-voice-lock-cache-filename) nil)
    (when (and fast-voice-lock-mode (not voice-lock-voiceified))
      (fast-voice-lock-read-cache))))

(defun fast-voice-lock-read-cache ()
  "Read the Voice Lock cache for the current buffer.

The following criteria must be met for a Voice Lock cache file to be read:
 - Fast Lock mode must be turned on in the buffer.
 - The buffer must not be modified.
 - The buffer's `voice-lock-keywords' must match the cache's.
 - The buffer file's timestamp must match the cache's.
 - Criteria imposed by `fast-voice-lock-cache-directories'.

See `fast-voice-lock-mode'."
  (interactive)
  (let ((directories fast-voice-lock-cache-directories)
	(modified (buffer-modified-p)) (inhibit-read-only t)
	(voiceified voice-lock-voiceified))
    (set (make-local-variable 'voice-lock-voiceified) nil)
    ;; Keep trying directories until voiceification is turned off.
    (while (and directories (not voice-lock-voiceified))
      (let ((directory (fast-voice-lock-cache-directory (car directories) nil)))
	(condition-case nil
	    (when directory
	      (setq fast-voice-lock-cache-filename (fast-voice-lock-cache-name directory))
	      (when (file-readable-p fast-voice-lock-cache-filename)
		(load fast-voice-lock-cache-filename t t t)))
	  (error nil) (quit nil))
	(setq directories (cdr directories))))
    ;; Unset `fast-voice-lock-cache-filename', and restore `voice-lock-voiceified', if
    ;; we don't use a cache.  (Note that `fast-voice-lock-cache-data' sets the value
    ;; of `fast-voice-lock-cache-timestamp'.)
    (set-buffer-modified-p modified)
    (unless voice-lock-voiceified
      (setq fast-voice-lock-cache-filename nil voice-lock-voiceified voiceified))))

(defun fast-voice-lock-save-cache (&optional buffer)
  "Save the Voice Lock cache of BUFFER or the current buffer.

The following criteria must be met for a Voice Lock cache file to be saved:
 - Fast Lock mode must be turned on in the buffer.
 - The event must be one of `fast-voice-lock-save-events'.
 - The buffer must be at least `fast-voice-lock-minimum-size' bytes long.
 - The buffer file must be owned by you, or `fast-voice-lock-save-others' must be t.
 - The buffer must contain at least one `personality' text property.
 - The buffer must not be modified.
 - The buffer file's timestamp must be the same as the file's on disk.
 - The on disk file's timestamp must be different than the buffer's cache.
 - Criteria imposed by `fast-voice-lock-cache-directories'.

See `fast-voice-lock-mode'."
  (interactive)
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (let ((min-size (voice-lock-value-in-major-mode fast-voice-lock-minimum-size))
	  (file-timestamp (visited-file-modtime)) (saved nil))
      (when (and fast-voice-lock-mode
	     ;;
	     ;; "Only save if the buffer matches the file, the file has
	     ;; changed, and it was changed by the current emacs session."
	     ;;
	     ;; Only save if the buffer is not modified,
	     ;; (i.e., so we don't save for something not on disk)
	     (not (buffer-modified-p))
	     ;; and the file's timestamp is the same as the buffer's,
	     ;; (i.e., someone else hasn't written the file in the meantime)
	     (verify-visited-file-modtime (current-buffer))
	     ;; and the file's timestamp is different from the cache's.
	     ;; (i.e., a save has occurred since the cache was read)
	     (not (equal fast-voice-lock-cache-timestamp file-timestamp))
	     ;;
	     ;; Only save if user's restrictions are satisfied.
	     (and min-size (>= (buffer-size) min-size))
	     (or fast-voice-lock-save-others
		 (eq (user-uid) (nth 2 (file-attributes buffer-file-name))))
	     ;;
	     ;; Only save if there are `personality' properties to save.
	     (text-property-not-all (point-min) (point-max) 'personality nil))
	;;
	;; Try each directory until we manage to save or the user quits.
	(let ((directories fast-voice-lock-cache-directories))
	  (while (and directories (memq saved '(nil error)))
	    (let* ((dir (fast-voice-lock-cache-directory (car directories) t))
		   (file (and dir (fast-voice-lock-cache-name dir))))
	      (when (and file (file-writable-p file))
		(setq saved (fast-voice-lock-save-cache-1 file file-timestamp)))
	      (setq directories (cdr directories)))))))))

;;;###autoload
(defun turn-on-fast-voice-lock ()
  "Unconditionally turn on Fast Lock mode."
  (fast-voice-lock-mode t))

;;; API Functions:

(defun fast-voice-lock-after-voiceify-buffer ()
  ;; Delete the Voice Lock cache file used to restore voiceification, if any.
  (when fast-voice-lock-cache-filename
    (if (file-writable-p fast-voice-lock-cache-filename)
	(delete-file fast-voice-lock-cache-filename)
      (message "File %s voice lock cache cannot be deleted" (buffer-name))))
  ;; Flag so that a cache will be saved later even if the file is never saved.
  (setq fast-voice-lock-cache-timestamp nil))

(defalias 'fast-voice-lock-after-unvoiceify-buffer
  'ignore)

;; Miscellaneous Functions:

(defun fast-voice-lock-save-cache-after-save-file ()
  ;; Do `fast-voice-lock-save-cache' if `save-buffer' is on `fast-voice-lock-save-events'.
  (when (memq 'save-buffer fast-voice-lock-save-events)
    (fast-voice-lock-save-cache)))

(defun fast-voice-lock-save-cache-before-kill-buffer ()
  ;; Do `fast-voice-lock-save-cache' if `kill-buffer' is on `fast-voice-lock-save-events'.
  (when (memq 'kill-buffer fast-voice-lock-save-events)
    (fast-voice-lock-save-cache)))

(defun fast-voice-lock-save-caches-before-kill-emacs ()
  ;; Do `fast-voice-lock-save-cache's if `kill-emacs' is on `fast-voice-lock-save-events'.
  (when (memq 'kill-emacs fast-voice-lock-save-events)
    (mapcar 'fast-voice-lock-save-cache (buffer-list))))

(defun fast-voice-lock-cache-directory (directory create)
  "Return usable directory based on DIRECTORY.
Returns nil if the directory does not exist, or, if CREATE non-nil, cannot be
created.  DIRECTORY may be a string or a cons pair of the form (REGEXP . DIR).
See `fast-voice-lock-cache-directories'."
  (let ((dir
	 (cond ((not buffer-file-name)
		;; Should never be nil, but `crypt++' screws it up.
		nil)
	       ((stringp directory)
		;; Just a directory.
		directory)
	       (t
		;; A directory iff the file name matches the regexp.
		(let ((bufile (expand-file-name buffer-file-truename))
		      (case-fold-search nil))
		  (when (save-match-data (string-match (car directory) bufile))
		    (cdr directory)))))))
    (cond ((not dir)
	   nil)
	  ((file-accessible-directory-p dir)
	   dir)
	  (create
	   (condition-case nil
	       (progn (make-directory dir t) dir)
	     (error nil))))))

;; If you are wondering why we only hash if the directory is not ".", rather
;; than if `file-name-absolute-p', it is because if we just appended ".flc" for
;; relative cache directories (that are not ".") then it is possible that more
;; than one file would have the same cache name in that directory, if the luser
;; made a link from one relative cache directory to another.  (Phew!)
(defun fast-voice-lock-cache-name (directory)
  "Return full cache path name using caching DIRECTORY.
If DIRECTORY is `.', the path is the buffer file name appended with `.flc'.
Otherwise, the path name is constructed from DIRECTORY and the buffer's true
abbreviated file name, with all `/' characters in the name replaced with `#'
characters, and appended with `.flc'.

If the same file has different cache path names when edited on different
machines, e.g., on one machine the cache file name has the prefix `#home',
perhaps due to automount, try putting in your `~/.emacs' something like:

 (setq directory-abbrev-alist (cons '(\"^/home/\" . \"/\") directory-abbrev-alist))

Emacs automagically removes the common `/tmp_mnt' automount prefix by default.

See `fast-voice-lock-cache-directory'."
  (if (string-equal directory ".")
      (concat buffer-file-name ".flc")
    (let* ((bufile (expand-file-name buffer-file-truename))
	   (chars-alist
	    (if (eq system-type 'emx)
		'((?/ . (?#)) (?# . (?# ?#)) (?: . (?\;)) (?\; . (?\; ?\;)))
	      '((?/ . (?#)) (?# . (?# ?#)))))
	   (mapchars
	    (function (lambda (c) (or (cdr (assq c chars-alist)) (list c))))))
      (concat
       (file-name-as-directory (expand-file-name directory))
       (mapconcat 'char-to-string (apply 'append (mapcar mapchars bufile)) "")
       ".flc"))))

;; Voice Lock Cache Processing Functions:

(defun fast-voice-lock-save-cache-1 (file timestamp)
  ;; Save the FILE with the TIMESTAMP as:
  ;; (fast-voice-lock-cache-data Version=2 TIMESTAMP voice-lock-keywords PROPERTIES).
  ;; Returns non-nil if a save was attempted to a writable cache file.
  (let ((tpbuf (generate-new-buffer " *fast-voice-lock*"))
	(buname (buffer-name)) (saved t))
    (message "Saving %s voice lock cache..." buname)
    (condition-case nil
	(save-excursion
	  (print (list 'fast-voice-lock-cache-data 2
		       (list 'quote timestamp)
		       (list 'quote voice-lock-keywords)
		       (list 'quote (fast-voice-lock-get-personality-properties)))
		 tpbuf)
	  (set-buffer tpbuf)
	  (write-region (point-min) (point-max) file nil 'quietly)
	  (setq fast-voice-lock-cache-timestamp timestamp
		fast-voice-lock-cache-filename file))
      (error (setq saved 'error)) (quit (setq saved 'quit)))
    (kill-buffer tpbuf)
    (message "Saving %s voice lock cache...%s" buname
	     (cond ((eq saved 'error) "failed")
		   ((eq saved 'quit) "aborted")
		   (t "done")))
    ;; We return non-nil regardless of whether a failure occurred.
    saved))

(defun fast-voice-lock-cache-data (version timestamp keywords properties
			     &rest ignored)
  ;; Change from (HIGH LOW) for back compatibility.  Remove for version 3!
  (when (consp (cdr-safe timestamp))
    (setcdr timestamp (nth 1 timestamp)))
  ;; Compile KEYWORDS and `voice-lock-keywords' in case one is and one isn't.
  (let ((current voice-lock-keywords))
    (setq keywords (voice-lock-compile-keywords keywords)
	  voice-lock-keywords (voice-lock-compile-keywords current)))
  ;; Use the Voice Lock cache PROPERTIES if we're using cache VERSION format 2,
  ;; the current buffer's file timestamp matches the TIMESTAMP, and the current
  ;; buffer's voice-lock-keywords are the same as KEYWORDS.
  (let ((buf-timestamp (visited-file-modtime))
	(buname (buffer-name)) (loaded t))
    (if (or (/= version 2)
	    (buffer-modified-p)
	    (not (equal timestamp buf-timestamp))
	    (not (equal keywords voice-lock-keywords)))
	(setq loaded nil)
      (message "Loading %s voice lock cache..." buname)
      (condition-case nil
	  (fast-voice-lock-set-personality-properties properties)
	(error (setq loaded 'error)) (quit (setq loaded 'quit)))
      (message "Loading %s voice lock cache...%s" buname
	       (cond ((eq loaded 'error) "failed")
		     ((eq loaded 'quit) "aborted")
		     (t "done"))))
    (setq voice-lock-voiceified (eq loaded t)
	  fast-voice-lock-cache-timestamp (and (eq loaded t) timestamp))))

;; Text Properties Processing Functions:

;; This is faster, but fails if adjacent characters have different `personality' text
;; properties.  Maybe that's why I dropped it in the first place?
;(defun fast-voice-lock-get-personality-properties ()
;  "Return a list of all `personality' text properties in the current buffer.
;Each element of the list is of the form (VALUE START1 END1 START2 END2 ...)
;where VALUE is a `personality' property value and STARTx and ENDx are positions."
;  (save-restriction
;    (widen)
;    (let ((start (text-property-not-all (point-min) (point-max) 'personality nil))
;	  (limit (point-max)) end properties value cell)
;      (while start
;	(setq end (next-single-property-change start 'personality nil limit)
;	      value (get-text-property start 'personality))
;	;; Make, or add to existing, list of regions with same `personality'.
;	(if (setq cell (assq value properties))
;	    (setcdr cell (cons start (cons end (cdr cell))))
;	  (setq properties (cons (list value start end) properties)))
;	(setq start (next-single-property-change end 'personality)))
;      properties)))

(defun fast-voice-lock-get-personality-properties ()
  "Return a list of all `personality' text properties in the current buffer.
Each element of the list is of the form (VALUE START1 END1 START2 END2 ...)
where VALUE is a `personality' property value and STARTx and ENDx are positions.
Only those `personality' VALUEs in `fast-voice-lock-save-personalities' are returned."
  (save-restriction
    (widen)
    (let ((personalities (or fast-voice-lock-save-personalities (voice-personality-list))) (limit (point-max))
	  properties regions personality start end)
      (while personalities
	(setq personality (car personalities) personalities (cdr personalities) regions () end (point-min))
	;; Make a list of start/end regions with `personality' property personality.
	(while (setq start (text-property-any end limit 'personality personality))
	  (setq end (or (text-property-not-all start limit 'personality personality) limit)
		regions (cons start (cons end regions))))
	;; Add `personality' personality's regions, if any, to properties.
	(when regions
	  (push (cons personality regions) properties)))
      properties)))

(defun fast-voice-lock-set-personality-properties (properties)
  "Set all `personality' text properties to PROPERTIES in the current buffer.
Any existing `personality' text properties are removed first.
See `fast-voice-lock-get-personality-properties' for the format of PROPERTIES."
  (save-buffer-state (plist regions)
    (save-restriction
      (widen)
      (voice-lock-unvoiceify-region (point-min) (point-max))
      (while properties
	(setq plist (list 'personality (car (car properties)))
	      regions (cdr (car properties))
	      properties (cdr properties))
	;; Set the `personality' property for each start/end region.
	(while regions
	  (set-text-properties (nth 0 regions) (nth 1 regions) plist)
	  (setq regions (nthcdr 2 regions)))))))

;; Functions for XEmacs:

(when (save-match-data (string-match "XEmacs" (emacs-version)))
  ;;
  ;; It would be better to use XEmacs' `map-extents' over extents with a
  ;; `voice-lock' property, but `personality' properties are on different extents.
  (defun fast-voice-lock-get-personality-properties ()
    "Return a list of all `personality' text properties in the current buffer.
Each element of the list is of the form (VALUE START1 END1 START2 END2 ...)
where VALUE is a `personality' property value and STARTx and ENDx are positions.
Only those `personality' VALUEs in `fast-voice-lock-save-personalities' are returned."
    (save-restriction
      (widen)
      (let ((properties ()) cell)
	(map-extents
	 (function (lambda (extent ignore)
	    (let ((value (extent-personality extent)))
	      ;; We're only interested if it's one of `fast-voice-lock-save-personalities'.
	      (when (and value (or (null fast-voice-lock-save-personalities)
				   (memq value fast-voice-lock-save-personalities)))
		(let ((start (extent-start-position extent))
		      (end (extent-end-position extent)))
		  ;; Make or add to existing list of regions with the same
		  ;; `personality' property value.
		  (if (setq cell (assq value properties))
		      (setcdr cell (cons start (cons end (cdr cell))))
		    (push (list value start end) properties))))
	      ;; Return nil to keep `map-extents' going.
	      nil))))
	properties)))
  ;;
  ;; Make extents just like XEmacs' voice-lock.el does.
  (defun fast-voice-lock-set-personality-properties (properties)
    "Set all `personality' text properties to PROPERTIES in the current buffer.
Any existing `personality' text properties are removed first.
See `fast-voice-lock-get-personality-properties' for the format of PROPERTIES."
    (save-restriction
      (widen)
      (voice-lock-unvoiceify-region (point-min) (point-max))
      (while properties
	(let ((personality (car (car properties)))
	      (regions (cdr (car properties))))
	  ;; Set the `personality' property, etc., for each start/end region.
	  (while regions
	    (voice-lock-set-personality (nth 0 regions) (nth 1 regions) personality)
	    (setq regions (nthcdr 2 regions)))
	  (setq properties (cdr properties))))))
  ;;
  ;; XEmacs 19.12 voice-lock.el's `voice-lock-voiceify-buffer' runs a hook.
  (add-hook 'voice-lock-after-voiceify-buffer-hook
	    'fast-voice-lock-after-voiceify-buffer))

(unless (boundp 'voice-lock-inhibit-thing-lock)
  (defvar voice-lock-inhibit-thing-lock nil
    "List of Voice Lock mode related modes that should not be turned on."))

(unless (fboundp 'voice-lock-compile-keywords)
  (defalias 'voice-lock-compile-keywords 'identity))

;; Install ourselves:

(add-hook 'after-save-hook 'fast-voice-lock-save-cache-after-save-file)
(add-hook 'kill-buffer-hook 'fast-voice-lock-save-cache-before-kill-buffer)
(add-hook 'kill-emacs-hook 'fast-voice-lock-save-caches-before-kill-emacs)

;;;###autoload
;;;###dont-autoload
(unless (assq 'fast-voice-lock-mode minor-mode-alist)
  (setq minor-mode-alist (append minor-mode-alist '((fast-voice-lock-mode nil)))))

;; Provide ourselves:

(provide 'fast-voice-lock)

;;; fast-voice-lock.el ends here
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
