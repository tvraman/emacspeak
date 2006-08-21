;;; voice-lock.el --- Provide voice locking for Emacspeak
;;; $Id$
;;; $Author$ 
;;; Description:  Voice lock mode for Emacspeak
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

;;;Copyright (C) 1995 -- 2001, T. V. Raman 
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
;;; Keywords: Emacspeak, Voices, Speech voices
;;; voice-lock.el --- Electric voice lock mode
;; Copyright (C) 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: jwz, then rms, then sm <simon@gnu.ai.mit.edu>
;; Maintainer: FSF
;; Keywords: languages, voices

;; This file is part of GNU Emacs.

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

;; Voice Lock mode is a minor mode that causes your comments to be displayed in
;; one face, strings in another, reserved words in another, and so on.
;;
;; Comments will be displayed in `voice-lock-comment-personality'.
;; Strings will be displayed in `voice-lock-string-personality'.
;; Regexps are used to display selected patterns in other faces.
;;
;; To make the text you type be voiceified, use M-x voice-lock-mode RET.
;; When this minor mode is on, the personalities of the current line are updated with
;; every insertion or deletion.
;;
;; To turn Voice Lock mode on automatically, add this to your ~/.emacs file:
;;
;;  (add-hook 'emacs-lisp-mode-hook 'turn-on-voice-lock)
;;
;; Or if you want to turn Voice Lock mode on in many modes:
;;
;;  (global-voice-lock-mode t)
;;
;; Voiceification for a particular mode may be available in a number of levels
;; of decoration.  The higher the level, the more decoration, but the more time
;; it takes to voiceify.  See the variable `voice-lock-maximum-decoration', and
;; also the variable `voice-lock-maximum-size'.  Support modes for Voice Lock
;; mode can be used to speed up Voice Lock mode.  See `voice-lock-support-mode'.

;; Constructing patterns:
;;
;; See the documentation for the variable `voice-lock-keywords'.
;;
;; Nasty regexps of the form "bar\\(\\|lo\\)\\|f\\(oo\\|u\\(\\|bar\\)\\)\\|lo"
;; are made thusly: (make-regexp '("foo" "fu" "fubar" "bar" "barlo" "lo")) for
;; efficiency.  See /pub/gnu/emacs/elisp-archive/functions/make-regexp.el.Z on
;; archive.cis.ohio-state.edu for this and other functions.

;; Adding patterns for modes that already support Voice Lock:
;;
;; Voice Lock mode uses the buffer local variable `voice-lock-keywords' for the
;; highlighting patterns.  This variable is set by Voice Lock mode from (a) the
;; buffer local variable `voice-lock-defaults', if non-nil, or (b) the global
;; variable `voice-lock-defaults-alist', if the major mode has an entry. 
;; Voice Lock mode is set up via (a) where a mode's patterns are distributed
;; with the mode's package library, (b) where a mode's patterns are distributed
;; with voice-lock.el itself.  An example of (a) is Pascal mode, an example of
;; (b) is C/C++ modes.  (Normally, the mechanism is (a); (b) is used where it
;; is not clear which package library should contain the pattern definitions.)
;;
;; If, for a particular mode, mechanism (a) is used, you need to add your
;; patterns after that package library has loaded, e.g.:
;;
;;  (eval-after-load "pascal" '(add-to-list 'pascal-voice-lock-keywords ...))
;;
;; (Note that only one pattern can be added with `add-to-list'.  For multiple
;; patterns, use one `eval-after-load' form with one `setq' and `append' form,
;; or multiple `eval-after-load' forms each with one `add-to-list' form.)
;; If mechanism (b) is used, you need to add your patterns after voice-lock.el
;; itself has loaded, e.g.:
;;
;;  (eval-after-load "voice-lock" '(add-to-list 'c-voice-lock-keywords ...))
;;
;; Which variable you should add to depends on what level of voiceification you
;; choose and what level is supported.  If you choose the maximum level, by
;; setting the variable `voice-lock-maximum-decoration', you change a different
;; variable.  Maximum level patterns for C are `c-voice-lock-keywords-3', so:
;;
;;  (setq voice-lock-maximum-decoration t)
;;  (eval-after-load "voice-lock"
;;   '(add-to-list 'c-voice-lock-keywords-3
;;		   '("\\<FILE\\>" . voice-lock-type-personality)))
;;
;; To see which variable to set, see the buffer's value of `voice-lock-defaults'
;; or the mode's entry in the global value of `voice-lock-defaults-alist'.

;; Adding patterns for modes that do not support Voice Lock:
;;
;; If you add patterns for a new mode, say foo.el's `foo-mode', say in which
;; you don't want syntactic voiceification to occur, you can make Voice Lock mode
;; use your regexps when turning on Voice Lock by adding to `foo-mode-hook':
;;
;;  (add-hook 'foo-mode-hook
;;   '(lambda () (make-local-variable 'voice-lock-defaults)
;;               (setq voice-lock-defaults '(foo-voice-lock-keywords t))))

;; What is voiceification for?  You might say, "It's to make my code look nice."
;; I think it should be for adding information in the form of cues.  These cues
;; should provide you with enough information to both (a) distinguish between
;; different items, and (b) identify the item meanings, without having to read
;; the items and think about it.  Therefore, voiceification allows you to think
;; less about, say, the structure of code, and more about, say, why the code
;; doesn't work.  Or maybe it allows you to think less and drift off to sleep.
;;
;; So, here are my opinions/advice/guidelines:
;; 
;; - Highlight conceptual objects, such as function and variable names, and
;;   different objects types differently, i.e., (a) and (b) above, highlight
;;   function names differently to variable names.
;; - Keep the personalities distinct from each other as far as possible.
;;   i.e., (a) above.
;; - Use the same face for the same conceptual object, across all modes.
;;   i.e., (b) above, all modes that have items that can be thought of as, say,
;;   keywords, should be highlighted with the same face, etc.
;; - Make the face attributes fit the concept as far as possible.
;;   i.e., function names might be a bold colour such as blue, comments might
;;   be a bright colour such as red, character strings might be brown, because,
;;   err, strings are brown (that was not the reason, please believe me).
;; - Don't use a non-nil OVERRIDE unless you have a good reason.
;;   Only use OVERRIDE for special things that are easy to define, such as the
;;   way `...' quotes are treated in strings and comments in Emacs Lisp mode.
;;   Don't use it to, say, highlight keywords in commented out code or strings.
;; - Err, that's it.


    
;;{{{  first we pull in emacspeak additions
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (require 'emacspeak-sounds)
  (require 'dtk-speak))
(require 'voice-setup)
(require 'lazy-voice-lock)
(require 'fast-voice-lock)
(require 'regexp-opt)
;;}}}
;; User variables.
(defvar voice-lock-verbose (* 0 1024)
  "*If non-nil, means show status messages for buffer voiceification.
If a number, only buffers greater than this size have voiceification messages.")

;;;###autoload
(defvar voice-lock-maximum-decoration nil
  "*Maximum decoration level for voiceification.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise.")

;;;###autoload
(defvar voice-lock-maximum-size (* 250 1024)
  "*Maximum size of a buffer for buffer voiceification.
Only buffers less than this can be voiceified when Voice Lock mode is turned on.
If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 256000) (c++-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.")

;; Voiceification variables --personalities defined in voice-aux.el
(defvar voice-lock-keywords nil
  "*A list of the keywords to highlight.
Each element should be of the form:

 MATCHER
 (MATCHER . MATCH)
 (MATCHER . VOICENAME)
 (MATCHER . HIGHLIGHT)
 (MATCHER HIGHLIGHT ...)
 (eval . FORM)

where HIGHLIGHT should be either MATCH-HIGHLIGHT or MATCH-ANCHORED.

FORM is an expression, whose value should be a keyword element, evaluated when
the keyword is (first) used in a buffer.  This feature can be used to provide a
keyword that can only be generated when Voice Lock mode is actually turned on.

For highlighting single items, typically only MATCH-HIGHLIGHT is required.
However, if an item or (typically) items are to be highlighted following the
instance of another item (the anchor) then MATCH-ANCHORED may be required.

MATCH-HIGHLIGHT should be of the form:

 (MATCH VOICENAME OVERRIDE LAXMATCH)

Where MATCHER can be either the regexp to search for, or the function name to
call to make the search (called with one argument, the limit of the search).
MATCH is the subexpression of MATCHER to be highlighted.  VOICENAME is an
expression whose value is the personality name to use.  VOICENAME's default attributes
may be defined in `voice-lock-personality-attributes'.

OVERRIDE and LAXMATCH are flags.  If OVERRIDE is t, existing voiceification may
be overwritten.  If `keep', only parts not already voiceified are highlighted.
If `prepend' or `append', existing voiceification is merged with the new, in
which the new or existing voiceification, respectively, takes precedence.
If LAXMATCH is non-nil, no error is signaled if there is no MATCH in MATCHER.

For example, an element of the form highlights (if not already highlighted):

 \"\\\\\\=<foo\\\\\\=>\"		Discrete occurrences of \"foo\" in the value of the
			variable `voice-lock-keyword-personality'.
 (\"fu\\\\(bar\\\\)\" . 1)	Substring \"bar\" within all occurrences of \"fubar\" in
			the value of `voice-lock-keyword-personality'.
 (\"fubar\" . fubar-personality)	Occurrences of \"fubar\" in the value of `fubar-personality'.
 (\"foo\\\\|bar\" 0 foo-bar-personality t)
			Occurrences of either \"foo\" or \"bar\" in the value
			of `foo-bar-personality', even if already highlighted.

MATCH-ANCHORED should be of the form:

 (MATCHER PRE-MATCH-FORM POST-MATCH-FORM MATCH-HIGHLIGHT ...)

Where MATCHER is as for MATCH-HIGHLIGHT with one exception.  The limit of the
search is currently guaranteed to be (no greater than) the end of the line.
PRE-MATCH-FORM and POST-MATCH-FORM are evaluated before the first, and after
the last, instance MATCH-ANCHORED's MATCHER is used.  Therefore they can be
used to initialise before, and cleanup after, MATCHER is used.  Typically,
PRE-MATCH-FORM is used to move to some position relative to the original
MATCHER, before starting with MATCH-ANCHORED's MATCHER.  POST-MATCH-FORM might
be used to move, before resuming with MATCH-ANCHORED's parent's MATCHER.

For example, an element of the form highlights (if not already highlighted):

 (\"\\\\\\=<anchor\\\\\\=>\" (0 anchor-personality) (\"\\\\\\=<item\\\\\\=>\" nil nil (0 item-personality)))

 Discrete occurrences of \"anchor\" in the value of `anchor-personality', and subsequent
 discrete occurrences of \"item\" (on the same line) in the value of `item-personality'.
 (Here PRE-MATCH-FORM and POST-MATCH-FORM are nil.  Therefore \"item\" is
 initially searched for starting from the end of the match of \"anchor\", and
 searching for subsequent instance of \"anchor\" resumes from where searching
 for \"item\" concluded.)

Note that the MATCH-ANCHORED feature is experimental; in the future, we may
replace it with other ways of providing this functionality.

These regular expressions should not match text which spans lines.  While
\\[voice-lock-voiceify-buffer] handles multi-line patterns correctly, updating
when you edit the buffer does not, since it considers text one line at a time.

Be very careful composing regexps for this list;
the wrong pattern can dramatically slow things down!")
(make-variable-buffer-local 'voice-lock-keywords)

(defvar voice-lock-defaults nil
  "If set by a major mode, should be the defaults for Voice Lock mode.
The value should be like the `cdr' of an item in `voice-lock-defaults-alist'.")

(defvar voice-lock-defaults-alist
  (let (;; For C and Lisp modes we use `beginning-of-defun', rather than nil,
	;; for SYNTAX-BEGIN.  Thus the calculation of the cache is usually
	;; faster but not infallible, so we risk mis-voiceification.  --sm.
	(c-mode-defaults
	 '((c-voice-lock-keywords c-voice-lock-keywords-1
                                  c-voice-lock-keywords-2 c-voice-lock-keywords-3)
	   nil nil ((?_ . "w")) beginning-of-defun
	   (voice-lock-comment-start-regexp . "/[*/]")
	   (voice-lock-mark-block-function . mark-defun)))
	(c++-mode-defaults
	 '((c++-voice-lock-keywords c++-voice-lock-keywords-1 
                                    c++-voice-lock-keywords-2 c++-voice-lock-keywords-3)
	   nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	   (voice-lock-comment-start-regexp . "/[*/]")
	   (voice-lock-mark-block-function . mark-defun)))
        (idl-mode-defaults
	 '((c++-voice-lock-keywords c++-voice-lock-keywords-1 
                                    c++-voice-lock-keywords-2 c++-voice-lock-keywords-3)
	   nil nil ((?_ . "w") (?~ . "w")) beginning-of-defun
	   (voice-lock-comment-start-regexp . "/[*/]")
	   (voice-lock-mark-block-function . mark-defun)))
        (objc-mode-defaults
	 '((objc-voice-lock-keywords objc-voice-lock-keywords-1
                                     objc-voice-lock-keywords-2 objc-voice-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w")) nil
	   ;; Obsoleted by Emacs 19.35 parse-partial-sexp's COMMENTSTOP.
                                        ;(voice-lock-comment-start-regexp . "/[*/]")
	   (voice-lock-mark-block-function . mark-defun)))
	(java-mode-defaults
	 '((java-voice-lock-keywords java-voice-lock-keywords-1
                                     java-voice-lock-keywords-2 java-voice-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w") (?. . "w")) nil
	   ;; Obsoleted by Emacs 19.35 parse-partial-sexp's COMMENTSTOP.
                                        ;(voice-lock-comment-start-regexp . "/[*/]")
	   (voice-lock-mark-block-function . mark-defun)))
	(lisp-mode-defaults
	 '((lisp-voice-lock-keywords
	    lisp-voice-lock-keywords-1 lisp-voice-lock-keywords-2)
	   nil nil (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	   (voice-lock-comment-start-regexp . ";")
	   (voice-lock-mark-block-function . mark-defun)))
	(scheme-mode-defaults
	 '(scheme-voice-lock-keywords
	   nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
	   (voice-lock-comment-start-regexp . ";")
	   (voice-lock-mark-block-function . mark-defun)))
	;; For TeX modes we could use `backward-paragraph' for the same reason.
	;; But we don't, because paragraph breaks are arguably likely enough to
	;; occur within a genuine syntactic block to make it too risky.
	;; However, we do specify a MARK-BLOCK function as that cannot result
	;; in a mis-voiceification even if it might not voiceify enough.  --sm.
	(tex-mode-defaults
	 '(tex-voice-lock-keywords nil nil ((?$ . "\"")) nil
                                   (voice-lock-comment-start-regexp . "%")
                                   (voice-lock-mark-block-function . mark-paragraph)))
	)
    (list
     (cons 'c++-c-mode			c-mode-defaults)
     (cons 'c++-mode			c++-mode-defaults)
     (cons 'idl-mode			idl-mode-defaults)
     (cons 'c-mode			c-mode-defaults)
(cons 'objc-mode			objc-mode-defaults)
     (cons 'java-mode			java-mode-defaults)
     (cons 'elec-c-mode			c-mode-defaults)
     (cons 'emacs-lisp-mode		lisp-mode-defaults)
     (cons 'inferior-scheme-mode	scheme-mode-defaults)
     (cons 'latex-mode			tex-mode-defaults)
     (cons 'lisp-mode			lisp-mode-defaults)
     (cons 'lisp-interaction-mode	lisp-mode-defaults)
     (cons 'plain-tex-mode		tex-mode-defaults)
     (cons 'scheme-mode			scheme-mode-defaults)
     (cons 'scheme-interaction-mode	scheme-mode-defaults)
     (cons 'slitex-mode			tex-mode-defaults)
     (cons 'tex-mode			tex-mode-defaults)))
  "Alist of default major mode and Voice Lock defaults.
Each item should be a list of the form:

 (MAJOR-MODE . (KEYWORDS KEYWORDS-ONLY CASE-FOLD SYNTAX-ALIST SYNTAX-BEGIN
                ...))

where MAJOR-MODE is a symbol.  KEYWORDS may be a symbol (a variable or function
whose value is the keywords to use for voiceification) or a list of symbols.
If KEYWORDS-ONLY is non-nil, syntactic voiceification (strings and comments) is
not performed.  If CASE-FOLD is non-nil, the case of the keywords is ignored
when voiceifying.  If SYNTAX-ALIST is non-nil, it should be a list of cons pairs
of the form (CHAR-OR-STRING . STRING) used to set the local Voice Lock syntax
table, for keyword and syntactic voiceification (see `modify-syntax-entry').

If SYNTAX-BEGIN is non-nil, it should be a function with no args used to move
backwards outside any enclosing syntactic block, for syntactic voiceification.
Typical values are `beginning-of-line' (i.e., the start of the line is known to
be outside a syntactic block), or `beginning-of-defun' for programming modes or
`backward-paragraph' for textual modes (i.e., the mode-dependent function is
known to move outside a syntactic block).  If nil, the beginning of the buffer
is used as a position outside of a syntactic block, in the worst case.

These item elements are used by Voice Lock mode to set the variables
`voice-lock-keywords', `voice-lock-keywords-only',
`voice-lock-keywords-case-fold-search', `voice-lock-syntax-table' and
`voice-lock-beginning-of-syntax-function', respectively.

Further item elements are alists of the form (VARIABLE . VALUE) and are in no
particular order.  Each VARIABLE is made buffer-local before set to VALUE.

Currently, appropriate variables include `voice-lock-mark-block-function'.
If this is non-nil, it should be a function with no args used to mark any
enclosing block of text, for voiceification via \\[voice-lock-voiceify-block].
Typical values are `mark-defun' for programming modes or `mark-paragraph' for
textual modes (i.e., the mode-dependent function is known to put point and mark
around a text block relevant to that mode).

Other variables include those for buffer-specialised voiceification functions,
`voice-lock-voiceify-buffer-function', `voice-lock-unvoiceify-buffer-function',
`voice-lock-voiceify-region-function', `voice-lock-unvoiceify-region-function',
`voice-lock-comment-start-regexp', `voice-lock-inhibit-thing-lock' and
`voice-lock-maximum-size'.")

(defvar voice-lock-keywords-only nil
  "*Non-nil means Voice Lock should not voiceify comments or strings.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-keywords-case-fold-search nil
  "*Non-nil means the patterns in `voice-lock-keywords' are case-insensitive.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-syntax-table nil
  "Non-nil means use this syntax table for voiceifying.
If this is nil, the major mode's syntax table is used.
This is normally set via `voice-lock-defaults'.")

;; If this is nil, we only use the beginning of the buffer if we can't use
;; `voice-lock-cache-position' and `voice-lock-cache-state'.
(defvar voice-lock-beginning-of-syntax-function nil
  "*Non-nil means use this function to move back outside of a syntactic block.
When called with no args it should leave point at the beginning of any
enclosing syntactic block.
If this is nil, the beginning of the buffer is used (in the worst case).
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-mark-block-function nil
  "*Non-nil means use this function to mark a block of text.
When called with no args it should leave point at the beginning of any
enclosing textual block and mark at the end.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-comment-start-regexp nil
  "*Regexp to match the start of a comment.
This need not discriminate between genuine comments and quoted comment
characters or comment characters within strings.
If nil, `comment-start-skip' is used instead; see that variable for more info.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-voiceify-buffer-function 'voice-lock-default-voiceify-buffer
  "Function to use for voiceifying the buffer.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-unvoiceify-buffer-function 'voice-lock-default-unvoiceify-buffer
  "Function to use for unvoiceifying the buffer.
This is used when turning off Voice Lock mode.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-voiceify-region-function 'voice-lock-default-voiceify-region
  "Function to use for voiceifying a region.
It should take two args, the beginning and end of the region, and an optional
third arg VERBOSE.  If non-nil, the function should print status messages.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-unvoiceify-region-function 'voice-lock-default-unvoiceify-region
  "Function to use for unvoiceifying a region.
It should take two args, the beginning and end of the region.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-inhibit-thing-lock nil
  "List of Voice Lock mode related modes that should not be turned on.
Currently, valid mode names as `fast-voice-lock-mode' and `lazy-voice-lock-mode'.
This is normally set via `voice-lock-defaults'.")

(defvar voice-lock-mode nil)		; For the modeline.
(defvar voice-lock-voiceified nil)	; Whether we have voiceified the buffer.

;;;###autoload
(defvar voice-lock-mode-hook nil
  "Function or functions to run on entry to Voice Lock mode.")

;; Voice Lock mode.

(eval-when-compile
  ;; We don't do this at the top-level as we only use non-autoloaded macros.
  (require 'cl))

;;;###autoload
(defun voice-lock-mode (&optional arg)
  "Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

When Voice Lock mode is enabled, text is voiceified as you type it:

 - Comments are displayed in `voice-lock-comment-personality';
 - Strings are displayed in `voice-lock-string-personality';
 - Certain other expressions are displayed in other personalities according to the
   value of the variable `voice-lock-keywords'.

You can enable Voice Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-voice-lock)

Alternatively, you can use Global Voice Lock mode to automagically turn on Voice
Lock mode in buffers whose major mode supports it and whose major mode is one
of `voice-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-voice-lock-mode t)

There are a number of support modes that may be used to speed up Voice Lock mode
in various ways, specified via the variable `voice-lock-support-mode'.  Where
major modes support different levels of voiceification, you can use the variable
`voice-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Voice Lock mode on/off the buffer is voiceified/devoiceified, though
voiceification occurs only if the buffer is less than `voice-lock-maximum-size'.

For example, to specify that Voice Lock mode use use Lazy Lock mode as a support
mode and use maximum levels of voiceification, put in your ~/.emacs:

 (setq voice-lock-support-mode 'lazy-voice-lock-mode)
 (setq voice-lock-maximum-decoration t)

To voiceify a buffer, without turning on Voice Lock mode and regardless of buffer
size, you can use \\[voice-lock-voiceify-buffer].

To voiceify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[voice-lock-voiceify-block].

The default Voice Lock mode personalities and their attributes are defined in the
variable `voice-lock-personality-attributes', and Voice Lock mode default settings in
the variable `voice-lock-defaults-alist'.  You can set your own default settings
for some mode, by setting a buffer local value for `voice-lock-defaults', via
its mode hook."
  (interactive "P")
  ;; Don't turn on Voice Lock mode if we don't have a display (we're running a
  ;; batch job) or if the buffer is invisible (the name starts with a space).
  (let ((on-p (and (not noninteractive)
		   (not (eq (aref (buffer-name) 0) ?\ ))
		   (if arg
		       (> (prefix-numeric-value arg) 0)
		     (not voice-lock-mode)))))
    (set (make-local-variable 'voice-lock-mode) on-p)
    ;; Turn on Voice Lock mode.
    (when on-p
      (voice-lock-set-defaults)
      (unless (eq voice-lock-voiceify-region-function 'ignore)
	(make-local-hook 'after-change-functions)
	(add-hook 'after-change-functions 'voice-lock-after-change-function nil t))
      (voice-lock-turn-on-thing-lock)
      (run-hooks 'voice-lock-mode-hook)
      ;; Voiceify the buffer if we have to.
      (let ((max-size (voice-lock-value-in-major-mode voice-lock-maximum-size)))
	(cond (voice-lock-voiceified
	       nil)
	      ((or (null max-size) (> max-size (buffer-size)))
	       (voice-lock-voiceify-buffer))
	      (voice-lock-verbose
	       (message "Voiceifying %s...buffer too big" (buffer-name))))))
    ;; Turn off Voice Lock mode.
    (when (not on-p)
      (remove-hook 'after-change-functions 'voice-lock-after-change-function t)
      (voice-lock-unvoiceify-buffer)
      (voice-lock-turn-off-thing-lock)
      (voice-lock-unset-defaults))
    (force-mode-line-update))
  (when (interactive-p)
    (when window-system (font-lock-mode voice-lock-mode))
    (dtk-speak
     (format "Turned %s voice lock mode"
             (if voice-lock-mode "on" "off")))
    (emacspeak-auditory-icon
     (if voice-lock-mode
         'on 'off ))))

;;;###autoload
(defun turn-on-voice-lock ()
  "Turn on Voice Lock mode conditionally.
Turn on only if the device can display it."
    (voice-lock-mode t))

;; Global Voice Lock mode.
;;
;; A few people have hassled in the past for a way to make it easier to turn on
;; Voice Lock mode, without the user needing to know for which modes s/he has to
;; turn it on, perhaps the same way hilit19.el/hl319.el does.  I've always
;; balked at that way, as I see it as just re-moulding the same problem in
;; another form.  That is; some person would still have to keep track of which
;; modes (which may not even be distributed with Emacs) support Voice Lock mode.
;; The list would always be out of date.  And that person might have to be me.

;; Implementation.
;;
;; In a previous discussion the following hack came to mind.  It is a gross
;; hack, but it generally works.  We use the convention that major modes start
;; by calling the function `kill-all-local-variables', which in turn runs
;; functions on the hook variable `change-major-mode-hook'.  We attach our
;; function `voice-lock-change-major-mode' to that hook.  Of course, when this
;; hook is run, the major mode is in the process of being changed and we do not
;; know what the final major mode will be.  So, `voice-lock-change-major-mode'
;; only (a) notes the name of the current buffer, and (b) adds our function
;; `turn-on-voice-lock-if-enabled' to the hook variables `find-file-hooks' and
;; `post-command-hook' (for buffers that are not visiting files).  By the time
;; the functions on the first of these hooks to be run are run, the new major
;; mode is assumed to be in place.  This way we get a Voice Lock function run
;; when a major mode is turned on, without knowing major modes or their hooks.
;;
;; Naturally this requires that (a) major modes run `kill-all-local-variables',
;; as they are supposed to do, and (b) the major mode is in place after the
;; file is visited or the command that ran `kill-all-local-variables' has
;; finished, whichever the sooner.  Arguably, any major mode that does not
;; follow the convension (a) is broken, and I can't think of any reason why (b)
;; would not be met (except `gnudoit' on non-files).  However, it is not clean.
;;
;; Probably the cleanest solution is to have each major mode function run some
;; hook, e.g., `major-mode-hook', but maybe implementing that change is
;; impractical.  I am personally against making `setq' a macro or be advised,
;; or have a special function such as `set-major-mode', but maybe someone can
;; come up with another solution?

;; User interface.
;;
;; Although Global Voice Lock mode is a pseudo-mode, I think that the user
;; interface should conform to the usual Emacs convention for modes, i.e., a
;; command to toggle the feature (`global-voice-lock-mode') with a variable for
;; finer control of the mode's behaviour (`voice-lock-global-modes').
;;
;; I don't think it is better that the feature be enabled via a variable, since
;; it does not conform to the usual convention.  I don't think the feature
;; should be enabled by loading voice-lock.el, since other mechanisms such as
;; M-x voice-lock-mode RET or (add-hook 'c-mode-hook 'turn-on-voice-lock) would
;; cause Voice Lock mode to be turned on everywhere, and it is not intuitive or
;; informative because loading a file tells you nothing about the feature or
;; how to control it.  It would be contrary to the Principle of Least Surprise.

(defvar voice-lock-buffers nil)		; For remembering buffers.
(defvar global-voice-lock-mode nil)

;;;###autoload
(defvar voice-lock-global-modes t
  "*Modes for which Voice Lock mode is automagically turned on.
Global Voice Lock mode is controlled by the `global-voice-lock-mode' command.
If nil, means no modes have Voice Lock mode automatically turned on.
If t, all modes that support Voice Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Voice Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Voice Lock mode is turned on for buffers in C and C++ modes only.")

;;;###autoload
(defun global-voice-lock-mode (&optional arg message)
  "Toggle Global Voice Lock mode.
With prefix ARG, turn Global Voice Lock mode on if and only if ARG is positive.
Displays a message saying whether the mode is on or off if MESSAGE is non-nil.
Returns the new status of Global Voice Lock mode (non-nil means on).

When Global Voice Lock mode is enabled, Voice Lock mode is automagically
turned on in a buffer if its major mode is one of `voice-lock-global-modes'."
  (interactive "P\np")
  (let ((off-p (if arg
		   (<= (prefix-numeric-value arg) 0)
		 global-voice-lock-mode)))
    (if off-p
	(remove-hook 'find-file-hooks 'turn-on-voice-lock-if-enabled)
      (add-hook 'find-file-hooks 'turn-on-voice-lock-if-enabled)
      (add-hook 'post-command-hook 'turn-on-voice-lock-if-enabled)
      (setq voice-lock-buffers (buffer-list)))
    (when message
      (message "Global Voice Lock mode is now %s." (if off-p "OFF" "ON")))
    (setq global-voice-lock-mode (not off-p))))

(defun voice-lock-change-major-mode ()
  ;; Turn off Voice Lock mode if it's on.
  (when voice-lock-mode
    (voice-lock-mode nil))
  ;; Gross hack warning: Delicate readers should avert eyes now.
  ;; Something is running `kill-all-local-variables', which generally means the
  ;; major mode is being changed.  Run `turn-on-voice-lock-if-enabled' after the
  ;; file is visited or the current command has finished.
  (when global-voice-lock-mode
    (add-hook 'post-command-hook 'turn-on-voice-lock-if-enabled)
    (add-to-list 'voice-lock-buffers (current-buffer))))

(defun turn-on-voice-lock-if-enabled ()
  ;; Gross hack warning: Delicate readers should avert eyes now.
  ;; Turn on Voice Lock mode if it's supported by the major mode and enabled by
  ;; the user.
  (remove-hook 'post-command-hook 'turn-on-voice-lock-if-enabled)
  (while voice-lock-buffers
    (if (buffer-live-p (car voice-lock-buffers))
	(save-excursion
	  (set-buffer (car voice-lock-buffers))
	  (if (and (or voice-lock-defaults
		       (assq major-mode voice-lock-defaults-alist))
		   (or (eq voice-lock-global-modes t)
		       (if (eq (car-safe voice-lock-global-modes) 'not)
			   (not (memq major-mode (cdr voice-lock-global-modes)))
			 (memq major-mode voice-lock-global-modes))))
	      (let (inhibit-quit)
		(turn-on-voice-lock)))))
    (setq voice-lock-buffers (cdr voice-lock-buffers))))

(add-hook 'change-major-mode-hook 'voice-lock-change-major-mode)

;; End of Global Voice Lock mode.

;; Voice Lock Support mode.
;;
;; This is the code used to interface voice-lock.el with any of its add-on
;; packages, and provide the user interface.  Packages that have their own
;; local buffer voiceification functions (see below) may have to call
;; `voice-lock-after-voiceify-buffer' and/or `voice-lock-after-unvoiceify-buffer'
;; themselves.

;;;###autoload
(defvar voice-lock-support-mode nil
  "*Support mode for Voice Lock mode.
Support modes speed up Voice Lock mode by being choosy about when voiceification
occurs.  Known support modes are Fast Lock mode (symbol `fast-voice-lock-mode') and
Lazy Lock mode (symbol `lazy-voice-lock-mode').  See those modes for more info.
If nil, means support for Voice Lock mode is never performed.
If a symbol, use that support mode.
If a list, each element should be of the form (MAJOR-MODE . SUPPORT-MODE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . fast-voice-lock-mode) (c++-mode . fast-voice-lock-mode) (t . lazy-voice-lock-mode))
means that Fast Lock mode is used to support Voice Lock mode for buffers in C or
C++ modes, and Lazy Lock mode is used to support Voice Lock mode otherwise.

The value of this variable is used when Voice Lock mode is turned on.")

(defun voice-lock-turn-on-thing-lock ()
  (let ((thing-mode (voice-lock-value-in-major-mode voice-lock-support-mode)))
    (cond ((eq thing-mode 'fast-voice-lock-mode)
	   (fast-voice-lock-mode t))
	  ((eq thing-mode 'lazy-voice-lock-mode)
	   (lazy-voice-lock-mode t)))))

(defvar fast-voice-lock-mode nil)
(defvar lazy-voice-lock-mode nil)

(defun voice-lock-turn-off-thing-lock ()
  (cond (fast-voice-lock-mode
	 (fast-voice-lock-mode nil))
	(lazy-voice-lock-mode
	 (lazy-voice-lock-mode nil))))

(defun voice-lock-after-voiceify-buffer ()
  (cond (fast-voice-lock-mode
	 (fast-voice-lock-after-voiceify-buffer))
	(lazy-voice-lock-mode
	 (lazy-voice-lock-after-voiceify-buffer))))

(defun voice-lock-after-unvoiceify-buffer ()
  (cond (fast-voice-lock-mode
	 (fast-voice-lock-after-unvoiceify-buffer))
	(lazy-voice-lock-mode
	 (lazy-voice-lock-after-unvoiceify-buffer))))

;; End of Voice Lock Support mode.

;; Voiceification functions.

;;;###autoload
(defun voice-lock-voiceify-buffer ()
  "Voiceify the current buffer the way `voice-lock-mode' would."
  (interactive)
  (let ((voice-lock-verbose (or voice-lock-verbose (interactive-p))))
    (funcall voice-lock-voiceify-buffer-function)))

(defun voice-lock-unvoiceify-buffer ()
  (funcall voice-lock-unvoiceify-buffer-function))

(defun voice-lock-voiceify-region (beg end &optional loudly)
  (funcall voice-lock-voiceify-region-function beg end loudly))

(defun voice-lock-unvoiceify-region (beg end)
  (funcall voice-lock-unvoiceify-region-function beg end))

(defun voice-lock-default-voiceify-buffer ()
  (let ((verbose (if (numberp voice-lock-verbose)
		     (> (buffer-size) voice-lock-verbose)
		   voice-lock-verbose)))
    (if verbose (message "Voiceifying %s..." (buffer-name)))
    ;; Make sure we have the right `voice-lock-keywords' etc.
    (if (not voice-lock-mode) (voice-lock-set-defaults))
    ;; Make sure we voiceify etc. in the whole buffer.
    (save-restriction
      (widen)
      (condition-case nil
	  (save-excursion
	    (save-match-data
	      (voice-lock-voiceify-region (point-min) (point-max) verbose)
	      (voice-lock-after-voiceify-buffer)
	      (setq voice-lock-voiceified t)))
	;; We don't restore the old voiceification, so it's best to unvoiceify.
	(quit (voice-lock-unvoiceify-buffer))))
    (if verbose (message "Voiceifying %s...%s" (buffer-name)
			 (if voice-lock-voiceified "done" "aborted")))))

(defun voice-lock-default-unvoiceify-buffer ()
  (save-restriction
    (widen)
    (voice-lock-unvoiceify-region (point-min) (point-max))
    (voice-lock-after-unvoiceify-buffer)
    (setq voice-lock-voiceified nil)))

;; We use this wrapper.  However, `voice-lock-voiceify-region' used to be the
;; name used for `voice-lock-voiceify-syntactically-region', so a change isn't
;; back-compatible.  But you shouldn't be calling these directly, should you?
(defun voice-lock-default-voiceify-region (beg end loudly)
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	(old-syntax-table (syntax-table))
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (unwind-protect
	(save-restriction
	  (widen)
	  ;; Use the voiceification syntax table, if any.
	  (if voice-lock-syntax-table (set-syntax-table voice-lock-syntax-table))
	  ;; Now do the voiceification.
	  (if voice-lock-keywords-only
	      (voice-lock-unvoiceify-region beg end)
	    (voice-lock-voiceify-syntactically-region beg end loudly))
	  (voice-lock-voiceify-keywords-region beg end loudly))
      ;; Clean up.
      (set-syntax-table old-syntax-table)
      (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil)))))

;; The following must be rethought, since keywords can override voiceification.
;      ;; Now scan for keywords, but not if we are inside a comment now.
;      (or (and (not voice-lock-keywords-only)
;	       (let ((state (parse-partial-sexp beg end nil nil 
;						voice-lock-cache-state)))
;		 (or (nth 4 state) (nth 7 state))))
;	  (voice-lock-voiceify-keywords-region beg end))

(defun voice-lock-default-unvoiceify-region (beg end)
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t) (inhibit-read-only t)
	before-change-functions after-change-functions
	buffer-file-name buffer-file-truename)
    (remove-text-properties beg end '(personality nil))
    (and (not modified) (buffer-modified-p) (set-buffer-modified-p nil))))

;; Called when any modification is made to buffer text.
(defun voice-lock-after-change-function (beg end old-len)
  (save-excursion
    (save-match-data
      ;; Rescan between start of line from `beg' and start of line after `end'.
      (voice-lock-voiceify-region
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point))))))

(defun voice-lock-voiceify-block (&optional arg)
  "Voiceify some lines the way `voice-lock-voiceify-buffer' would.
The lines could be a function or paragraph, or a specified number of lines.
If ARG is given, voiceify that many lines before and after point, or 16 lines if
no ARG is given and `voice-lock-mark-block-function' is nil.
If `voice-lock-mark-block-function' non-nil and no ARG is given, it is used to
delimit the region to voiceify."
  (interactive "P")
  (let (voice-lock-beginning-of-syntax-function deactivate-mark)
    ;; Make sure we have the right `voice-lock-keywords' etc.
    (if (not voice-lock-mode) (voice-lock-set-defaults))
    (save-excursion
      (save-match-data
	(condition-case error-data
	    (if (or arg (not voice-lock-mark-block-function))
		(let ((lines (if arg (prefix-numeric-value arg) 16)))
		  (voice-lock-voiceify-region
		   (save-excursion (forward-line (- lines)) (point))
		   (save-excursion (forward-line lines) (point))))
	      (funcall voice-lock-mark-block-function)
	      (voice-lock-voiceify-region (point) (mark)))
	  ((error quit) (message "Voiceifying block...%s" error-data)))))))



;; Syntactic voiceification functions.

;; These record the parse state at a particular position, always the start of a
;; line.  Used to make `voice-lock-voiceify-syntactically-region' faster.
(defvar voice-lock-cache-position nil)
(defvar voice-lock-cache-state nil)
(make-variable-buffer-local 'voice-lock-cache-position)
(make-variable-buffer-local 'voice-lock-cache-state)

(defun voice-lock-voiceify-syntactically-region (start end &optional loudly)
  "Put proper personality on each string and comment between START and END.
START should be at the beginning of a line."
  (let ((synstart (cond (voice-lock-comment-start-regexp
			 (concat "\\s\"\\|" voice-lock-comment-start-regexp))
			(comment-start-skip
			 (concat "\\s\"\\|" comment-start-skip))
			(t
			 "\\s\"")))
	(comstart (cond (voice-lock-comment-start-regexp
			 voice-lock-comment-start-regexp)
			(comment-start-skip
			 (concat "\\s<\\|" comment-start-skip))
			(t
			 "\\s<")))
	state prev prevstate)
    (if loudly (message "Voiceifying %s... (syntactically...)" (buffer-name)))
    (goto-char start)
    ;;
    ;; Find the state at the `beginning-of-line' before `start'.
    (if (eq start voice-lock-cache-position)
	;; Use the cache for the state of `start'.
	(setq state voice-lock-cache-state)
      ;; Find the state of `start'.
      (if (null voice-lock-beginning-of-syntax-function)
	  ;; Use the state at the previous cache position, if any, or
	  ;; otherwise calculate from `point-min'.
	  (if (or (null voice-lock-cache-position)
		  (< start voice-lock-cache-position))
	      (setq state (parse-partial-sexp (point-min) start))
	    (setq state (parse-partial-sexp voice-lock-cache-position start
					    nil nil voice-lock-cache-state)))
	;; Call the function to move outside any syntactic block.
	(funcall voice-lock-beginning-of-syntax-function)
	(setq state (parse-partial-sexp (point) start)))
      ;; Cache the state and position of `start'.
      (setq voice-lock-cache-state state
	    voice-lock-cache-position start))
    ;;
    ;; If the region starts inside a string, show the extent of it.
    (if (nth 3 state)
	(let ((beg (point)))
	  (while (and (re-search-forward "\\s\"" end 'move)
		      (nth 3 (parse-partial-sexp beg (point) nil nil state))))
	  (put-text-property beg (point) 'personality voice-lock-string-personality)
	  (setq state (parse-partial-sexp beg (point) nil nil state))))
    ;;
    ;; Likewise for a comment.
    (if (or (nth 4 state) (nth 7 state))
	(let ((beg (point)))
	  (save-restriction
	    (narrow-to-region (point-min) end)
	    (condition-case nil
		(progn
		  (re-search-backward comstart (point-min) 'move)
		  (forward-comment 1)
		  ;; forward-comment skips all whitespace,
		  ;; so go back to the real end of the comment.
		  (skip-chars-backward " \t"))
	      (error (goto-char end))))
	  (put-text-property beg (point) 'personality voice-lock-comment-personality)
	  (setq state (parse-partial-sexp beg (point) nil nil state))))
    ;;
    ;; Find each interesting place between here and `end'.
    (while (and (< (point) end)
		(setq prev (point) prevstate state)
		(re-search-forward synstart end t)
		(progn
		  ;; Clear out the voices of what we skip over.
		  (remove-text-properties prev (point) '(personality nil))
		  ;; Verify the state at that place
		  ;; so we don't get fooled by \" or \;.
		  (setq state (parse-partial-sexp prev (point)
						  nil nil state))))
      (let ((here (point)))
	(if (or (nth 4 state) (nth 7 state))
	    ;;
	    ;; We found a real comment start.
	    (let ((beg (or (match-end 1) (match-beginning 0))))
	      (goto-char beg)
	      (save-restriction
		(narrow-to-region (point-min) end)
		(condition-case nil
		    (progn
		      (forward-comment 1)
		      ;; forward-comment skips all whitespace,
		      ;; so go back to the real end of the comment.
		      (skip-chars-backward " \t"))
		  (error (goto-char end))))
	      (put-text-property beg (point) 'personality voice-lock-comment-personality)
	      (setq state (parse-partial-sexp here (point) nil nil state)))
	  (if (nth 3 state)
	      ;;
	      ;; We found a real string start.
	      (let ((beg (or (match-end 1) (match-beginning 0))))
		(while (and (re-search-forward "\\s\"" end 'move)
			    (nth 3 (parse-partial-sexp here (point)
						       nil nil state))))
		(put-text-property beg (point) 'personality voice-lock-string-personality)
		(setq state (parse-partial-sexp here (point)
						nil nil state))))))
      ;;
      ;; Make sure `prev' is non-nil after the loop
      ;; only if it was set on the very last iteration.
      (setq prev nil))
    ;;
    ;; Clean up.
    (and prev (remove-text-properties prev end '(personality nil)))))

;;; Additional text property functions.

;; The following three text property functions are not generally available (and
;; it's not certain that they should be) so they are inlined for speed.
;; The case for `fillin-text-property' is simple; it may or not be generally
;; useful.  (Since it is used here, it is useful in at least one place.;-)
;; However, the case for `append-text-property' and `prepend-text-property' is
;; more complicated.  Should they remove duplicate property values or not?  If
;; so, should the first or last duplicate item remain?  Or the one that was
;; added?  In our implementation, the first duplicate remains.

(defsubst voice-lock-fillin-text-property (start end prop value &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end prop nil object)) next)
    (while start
      (setq next (next-single-property-change start prop object end))
      (put-text-property start next prop value object)
      (setq start (text-property-any next end prop nil object)))))

;; This function (from simon's unique.el) is rewritten and inlined for speed.
;(defun unique (list function)
;  "Uniquify LIST, deleting elements using FUNCTION.
;Return the list with subsequent duplicate items removed by side effects.
;FUNCTION is called with an element of LIST and a list of elements from LIST,
;and should return the list of elements with occurrences of the element removed,
;i.e., a function such as `delete' or `delq'.
;This function will work even if LIST is unsorted.  See also `uniq'."
;  (let ((list list))
;    (while list
;      (setq list (setcdr list (funcall function (car list) (cdr list))))))
;  list)

(defsubst voice-lock-unique (list)
  "Uniquify LIST, deleting elements using `delq'.
Return the list with subsequent duplicate items removed by side effects."
  (let ((list list))
    (while list
      (setq list (setcdr list (delq (car list) (cdr list))))))
  list)

;; A generalisation of `facemenu-add-personality' for any property, but without the
;; removal of inactive personalities via `facemenu-discard-redundant-personalities' and special
;; treatment of `default'.  Uses `unique' to remove duplicate property values.
(defsubst voice-lock-prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (voice-lock-unique (append val (if (listp prev) prev (list prev))))
       object)
      (setq start next))))

(defsubst voice-lock-append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (voice-lock-unique (append (if (listp prev) prev (list prev)) val))
       object)
      (setq start next))))

;;; Regexp voiceification functions.

(defsubst voice-lock-apply-highlight (highlight)
  "Apply HIGHLIGHT following a match.
HIGHLIGHT should be of the form MATCH-HIGHLIGHT, see `voice-lock-keywords'."
  (let* ((match (nth 0 highlight))
	 (start (match-beginning match)) (end (match-end match))
	 (override (nth 2 highlight)))
    (cond ((not start)
	   ;; No match but we might not signal an error.
	   (or (nth 3 highlight)
	       (error "No match %d in highlight %S" match highlight)))
	  ((not override)
	   ;; Cannot override existing voiceification.
	   (or (text-property-not-all start end 'personality nil)
	       (put-text-property start end 'personality (eval (nth 1 highlight)))))
	  ((eq override t)
	   ;; Override existing voiceification.
	   (put-text-property start end 'personality (eval (nth 1 highlight))))
	  ((eq override 'keep)
	   ;; Keep existing voiceification.
	   (voice-lock-fillin-text-property start end 'personality
					   (eval (nth 1 highlight))))
	  ((eq override 'prepend)
	   ;; Prepend to existing voiceification.
	   (voice-lock-prepend-text-property start end 'personality
					    (eval (nth 1 highlight))))
	  ((eq override 'append)
	   ;; Append to existing voiceification.
	   (voice-lock-append-text-property start end 'personality
					   (eval (nth 1 highlight)))))))

(defsubst voice-lock-voiceify-anchored-keywords (keywords limit)
  "Voiceify according to KEYWORDS until LIMIT.
KEYWORDS should be of the form MATCH-ANCHORED, see `voice-lock-keywords'."
  (let ((matcher (nth 0 keywords)) (lowdarks (nthcdr 3 keywords)) highlights)
    ;; Until we come up with a cleaner solution, we make LIMIT the end of line.
    (save-excursion (end-of-line) (setq limit (min limit (point))))
    ;; Evaluate PRE-MATCH-FORM.
    (eval (nth 1 keywords))
    (save-match-data
      ;; Find an occurrence of `matcher' before `limit'.
      (while (if (stringp matcher)
		 (re-search-forward matcher limit t)
	       (funcall matcher limit))
	;; Apply each highlight to this instance of `matcher'.
	(setq highlights lowdarks)
	(while highlights
	  (voice-lock-apply-highlight (car highlights))
	  (setq highlights (cdr highlights)))))
    ;; Evaluate POST-MATCH-FORM.
    (eval (nth 2 keywords))))

(defun voice-lock-voiceify-keywords-region (start end &optional loudly)
  "Voiceify according to `voice-lock-keywords' between START and END.
START should be at the beginning of a line."
  (let ((case-fold-search voice-lock-keywords-case-fold-search)
	(keywords (cdr (if (eq (car-safe voice-lock-keywords) t)
			   voice-lock-keywords
			 (voice-lock-compile-keywords))))
	(bufname (buffer-name)) (count 0)
	keyword matcher highlights)
    ;;
    ;; Voiceify each item in `voice-lock-keywords' from `start' to `end'.
    (while keywords
      (if loudly (message "Voiceifying %s... (regexps..%s)" bufname
			  (make-string (setq count (1+ count)) ?.)))
      ;;
      ;; Find an occurrence of `matcher' from `start' to `end'.
      (setq keyword (car keywords) matcher (car keyword))
      (goto-char start)
      (while (if (stringp matcher)
		 (re-search-forward matcher end t)
	       (funcall matcher end))
	;; Apply each highlight to this instance of `matcher', which may be
	;; specific highlights or more keywords anchored to `matcher'.
	(setq highlights (cdr keyword))
	(while highlights
	  (if (numberp (car (car highlights)))
	      (voice-lock-apply-highlight (car highlights))
	    (voice-lock-voiceify-anchored-keywords (car highlights) end))
	  (setq highlights (cdr highlights))))
      (setq keywords (cdr keywords)))))

;; Various functions.

(defun voice-lock-compile-keywords (&optional keywords)
  ;; Compile `voice-lock-keywords' into the form (t KEYWORD ...) where KEYWORD
  ;; is the (MATCHER HIGHLIGHT ...) shown in the variable's doc string.
  (let ((keywords (or keywords voice-lock-keywords)))
    (setq voice-lock-keywords 
	  (if (eq (car-safe keywords) t)
	      keywords
	    (cons t (mapcar 'voice-lock-compile-keyword keywords))))))

(defun voice-lock-compile-keyword (keyword)
  (cond ((nlistp keyword)		; Just MATCHER
	 (list keyword '(0 voice-lock-keyword-personality)))
	((eq (car keyword) 'eval)	; Specified (eval . FORM)
	 (voice-lock-compile-keyword (eval (cdr keyword))))
	((numberp (cdr keyword))	; Specified (MATCHER . MATCH)
	 (list (car keyword) (list (cdr keyword) 'voice-lock-keyword-personality)))
	((symbolp (cdr keyword))	; Specified (MATCHER . VOICENAME)
	 (list (car keyword) (list 0 (cdr keyword))))
	((nlistp (nth 1 keyword))	; Specified (MATCHER . HIGHLIGHT)
	 (list (car keyword) (cdr keyword)))
	(t				; Hopefully (MATCHER HIGHLIGHT ...)
	 keyword)))

(defun voice-lock-value-in-major-mode (alist)
  ;; Return value in ALIST for `major-mode', or ALIST if it is not an alist.
  ;; Alist structure is ((MAJOR-MODE . VALUE)) where MAJOR-MODE may be t.
  (if (consp alist)
      (cdr (or (assq major-mode alist) (assq t alist)))
    alist))

(defun voice-lock-choose-keywords (keywords level)
  ;; Return LEVELth element of KEYWORDS.  A LEVEL of nil is equal to a
  ;; LEVEL of 0, a LEVEL of t is equal to (1- (length KEYWORDS)).
  (cond ((symbolp keywords)
	 keywords)
	((numberp level)
	 (or (nth level keywords) (car (reverse keywords))))
	((eq level t)
	 (car (reverse keywords)))
	(t
	 (car keywords))))
;;{{{ Finally pull in emacspeak voice settings

(require 'voice-settings)

;;}}}

(defun voice-lock-set-defaults ()
  "Set voiceification defaults appropriately for this mode.
Sets various variables using `voice-lock-defaults' (or, if nil, using
`voice-lock-defaults-alist') and `voice-lock-maximum-decoration'."
  ;; Set voiceification defaults.
  (make-local-variable 'voice-lock-voiceified)
  (if (member voice-lock-keywords '(nil (t)))
      (let* ((defaults (or voice-lock-defaults
			   (cdr (assq major-mode voice-lock-defaults-alist))))
	     (keywords
	      (or
               (voice-lock-choose-keywords (nth 0 defaults)
	       (voice-lock-value-in-major-mode
                voice-lock-maximum-decoration))
                (voice-lock-get-major-mode-keywords major-mode ))))
	;; Regexp voiceification?
	(setq voice-lock-keywords (if (fboundp keywords)
				     (funcall keywords)
				   (eval keywords)))
	;; Syntactic voiceification?
        (if(or
            (memq major-mode
                  '(rmail-mode dired-mode compilation-mode shell-mode
                               tex-mode latex-mode latex2e-mode))
            (nth 1 defaults))
	    (set (make-local-variable 'voice-lock-keywords-only) t))
	;; Case fold during regexp voiceification?
	(if (nth 2 defaults)
	    (set (make-local-variable 'voice-lock-keywords-case-fold-search) t))
	;; Syntax table for regexp and syntactic voiceification?
	(if (nth 3 defaults)
	    (let ((slist (nth 3 defaults)))
	      (set (make-local-variable 'voice-lock-syntax-table)
		   (copy-syntax-table (syntax-table)))
	      (while slist
		;; The character to modify may be a single CHAR or a STRING.
		(let ((chars (if (numberp (car (car slist)))
				 (list (car (car slist)))
			       (mapcar 'identity (car (car slist)))))
		      (syntax (cdr (car slist))))
		  (while chars
		    (modify-syntax-entry (car chars) syntax
					 voice-lock-syntax-table)
		    (setq chars (cdr chars)))
		  (setq slist (cdr slist))))))
	;; Syntax function for syntactic voiceification?
	(if (nth 4 defaults)
	    (set (make-local-variable 'voice-lock-beginning-of-syntax-function)
		 (nth 4 defaults)))
	;; Variable alist?
	(let ((alist (nthcdr 5 defaults)))
	  (while alist
	    (set (make-local-variable (car (car alist))) (cdr (car alist)))
	    (setq alist (cdr alist)))))))

(defun voice-lock-unset-defaults ()
  "Unset voiceification defaults.  See `voice-lock-set-defaults'."
  (setq voice-lock-keywords			nil
	voice-lock-keywords-only			nil
	voice-lock-keywords-case-fold-search	nil
	voice-lock-syntax-table			nil
	voice-lock-beginning-of-syntax-function	nil)
  (let* ((defaults (or voice-lock-defaults
		       (cdr (assq major-mode voice-lock-defaults-alist))))
	 (alist (nthcdr 5 defaults)))
    (while alist
      (set (car (car alist)) (default-value (car (car alist))))
      (setq alist (cdr alist)))))

;; Colour etc. support.

;; This section of code is crying out for revision.

;; To begin with, `display-type' and `background-mode' are `frame-parameters'
;; so we don't have to calculate them here anymore.  But all the personality stuff
;; should be frame-local (and thus display-local) anyway.  Because we're not
;; sure what support Emacs is going to have for general frame-local face
;; attributes, we leave this section of code as it is.  For now.  --sm.

(defvar voice-lock-display-type nil
  "A symbol indicating the display Emacs is running under.
The symbol should be one of `color', `grayscale' or `mono'.
If Emacs guesses this display attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.displayType' in your `~/.Xdefaults'.
See also `voice-lock-background-mode' and `voice-lock-personality-attributes'.")

(defvar voice-lock-background-mode nil
  "A symbol indicating the Emacs background brightness.
The symbol should be one of `light' or `dark'.
If Emacs guesses this frame attribute wrongly, either set this variable in
your `~/.emacs' or set the resource `Emacs.backgroundMode' in your
`~/.Xdefaults'.
See also `voice-lock-display-type' and `voice-lock-personality-attributes'.")

(defvar voice-lock-personality-attributes nil
  "A list of default attributes to use for personality attributes.
Each element of the list should be of the form

 (VOICE FOREGROUND BACKGROUND BOLD-P ITALIC-P UNDERLINE-P)

where VOICE should be one of the personality symbols `voice-lock-comment-personality',
`voice-lock-string-personality', `voice-lock-keyword-personality', `voice-lock-type-personality',
`voice-lock-function-name-personality', `voice-lock-variable-name-personality', and
`voice-lock-reference-personality'.  A form for each of these personality symbols should be
provided in the list, but other personality symbols and attributes may be given and
used in highlighting.  See `voice-lock-keywords'.

Subsequent element items should be the attributes for the corresponding
Voice Lock mode personalities.  Attributes FOREGROUND and BACKGROUND should be strings
\(default if nil), while BOLD-P, ITALIC-P, and UNDERLINE-P should specify the
corresponding personality attributes (yes if non-nil).

Emacs uses default attributes based on display type and background brightness.
See variables `voice-lock-display-type' and `voice-lock-background-mode'.

Resources can be used to over-ride these personality attributes.  For example, the
resource `Emacs.voice-lock-comment-personality.attributeUnderline' can be used to
specify the UNDERLINE-P attribute for personality `voice-lock-comment-personality'.")
;;{{{  junked for now

;(defun voice-lock-make-personalities (&optional override)
;  "Make personalities from `voice-lock-personality-attributes'.
;A default list is used if this is nil.
;If optional OVERRIDE is non-nil, personalities that already exist are reset.
;See `voice-lock-make-personality' and `list-personalities-display'."
;  ;; We don't need to `setq' any of these variables, but the user can see what
;  ;; is being used if we do.
;  (if (null voice-lock-display-type)
;      (setq voice-lock-display-type
;	    (let ((display-resource (x-get-resource ".displayType"
;						    "DisplayType")))
;	      (cond (display-resource (intern (downcase display-resource)))
;		    ((x-display-color-p) 'color)
;		    ((x-display-grayscale-p) 'grayscale)
;		    (t 'mono)))))
;  (if (null voice-lock-background-mode)
;      (setq voice-lock-background-mode
;	    (let ((bg-resource (x-get-resource ".backgroundMode"
;					       "BackgroundMode"))
;		  (params (frame-parameters)))
;	      (cond (bg-resource (intern (downcase bg-resource)))
;		    ((eq system-type 'ms-dos)
;		     (if (string-match "light"
;				       (cdr (assq 'background-color params)))
;			 'light
;		       'dark))
;		    ((< (apply '+ (x-color-values
;				   (cdr (assq 'background-color params))))
;			(* (apply '+ (x-color-values "white")) .6))
;		     'dark)
;		    (t 'light)))))
;  (if (null voice-lock-personality-attributes)
;      (setq voice-lock-personality-attributes
;	    (let ((light-bg (eq voice-lock-background-mode 'light)))
;	      (cond ((memq voice-lock-display-type '(mono monochrome))
;		     ;; Emacs 19.25's voice-lock defaults:
;		     ;;'((voice-lock-comment-personality nil nil nil t nil)
;		     ;;  (voice-lock-string-personality nil nil nil nil t)
;		     ;;  (voice-lock-keyword-personality nil nil t nil nil)
;		     ;;  (voice-lock-function-name-personality nil nil t t nil)
;		     ;;  (voice-lock-type-personality nil nil nil t nil))
;		     (list '(voice-lock-comment-personality nil nil t t nil)
;			   '(voice-lock-string-personality nil nil nil t nil)
;			   '(voice-lock-keyword-personality nil nil t nil nil)
;			   (list
;			    'voice-lock-function-name-personality
;			    (cdr (assq 'background-color (frame-parameters)))
;			    (cdr (assq 'foreground-color (frame-parameters)))
;			    t nil nil)
;			   '(voice-lock-variable-name-personality nil nil t t nil)
;			   '(voice-lock-type-personality nil nil t nil t)
;			   '(voice-lock-reference-personality nil nil t nil t)))
;		    ((memq voice-lock-display-type '(grayscale greyscale
;						    grayshade greyshade))
;		     (list
;		      (list 'voice-lock-comment-personality
;			    nil (if light-bg "Gray80" "DimGray") t t nil)
;		      (list 'voice-lock-string-personality
;			    nil (if light-bg "Gray50" "LightGray") nil t nil)
;		      (list 'voice-lock-keyword-personality
;			    nil (if light-bg "Gray90" "DimGray") t nil nil)
;		      (list 'voice-lock-function-name-personality
;			    (cdr (assq 'background-color (frame-parameters)))
;			    (cdr (assq 'foreground-color (frame-parameters)))
;			    t nil nil)
;		      (list 'voice-lock-variable-name-personality
;			    nil (if light-bg "Gray90" "DimGray") t t nil)
;		      (list 'voice-lock-type-personality
;			    nil (if light-bg "Gray80" "DimGray") t nil t)
;		      (list 'voice-lock-reference-personality
;			    nil (if light-bg "LightGray" "Gray50") t nil t)))
;		    (light-bg		; light colour background
;		     '((voice-lock-comment-personality "Firebrick")
;		       (voice-lock-string-personality "RosyBrown")
;		       (voice-lock-keyword-personality "Purple")
;		       (voice-lock-function-name-personality "Blue")
;		       (voice-lock-variable-name-personality "DarkGoldenrod")
;		       (voice-lock-type-personality "DarkOliveGreen")
;		       (voice-lock-reference-personality "CadetBlue")))
;		    (t			; dark colour background
;		     '((voice-lock-comment-personality "OrangeRed")
;		       (voice-lock-string-personality "LightSalmon")
;		       (voice-lock-keyword-personality "LightSteelBlue")
;		       (voice-lock-function-name-personality "LightSkyBlue")
;		       (voice-lock-variable-name-personality "LightGoldenrod")
;		       (voice-lock-type-personality "PaleGreen")
;		       (voice-lock-reference-personality "Aquamarine")))))))
;  ;; Now make the personalities if we have to.
;  (mapcar (function
;	   (lambda (face-attributes)
;	     (let ((face (nth 0 face-attributes)))
;	       (cond (override
;		      ;; We can stomp all over it anyway.  Get outta my face!
;		      (voice-lock-make-personality face-attributes))
;		     ((and (boundp face) (facep (symbol-value face)))
;		      ;; The variable exists and is already bound to a face.
;		      nil)
;		     ((facep face)
;		      ;; We already have a personality so we bind the variable to it.
;		      (set personality face))
;		     (t
;		      ;; No variable or no face.
;		      (voice-lock-make-personality face-attributes))))))
;	  voice-lock-personality-attributes))

;(defun voice-lock-make-personality (face-attributes)
;  "Make a personality from VOICE-ATTRIBUTES.
;VOICE-ATTRIBUTES should be like an element `voice-lock-personality-attributes', so that
;the personality name is the first item in the list.  A variable with the same name as
;the personality is also set; its value is the personality name."
;  (let* ((face (nth 0 face-attributes))
;	 (face-name (symbol-name face))
;	 (set-p (function (lambda (face-name resource)
;		 (x-get-resource (concat face-name ".attribute" resource)
;				 (concat "Personality.Attribute" resource)))))
;	 (on-p (function (lambda (face-name resource)
;		(let ((set (funcall set-p face-name resource)))
;		  (and set (member (downcase set) '("on" "true"))))))))
;    (make-personality face)
;    (add-to-list 'facemenu-unlisted-personalities face)
;    ;; Set attributes not set from X resources (and therefore `make-personality').
;    (or (funcall set-p face-name "Foreground")
;	(condition-case nil
;	    (set-personality-foreground personality (nth 1 face-attributes))
;	  (error nil)))
;    (or (funcall set-p face-name "Background")
;	(condition-case nil
;	    (set-personality-background personality (nth 2 face-attributes))
;	  (error nil)))
;    (if (funcall set-p face-name "Bold")
;	(and (funcall on-p face-name "Bold") (make-personality-bold personality nil t))
;      (and (nth 3 face-attributes) (make-personality-bold personality nil t)))
;    (if (funcall set-p face-name "Italic")
;	(and (funcall on-p face-name "Italic") (make-personality-italic personality nil t))
;      (and (nth 4 face-attributes) (make-personality-italic personality nil t)))
;    (or (funcall set-p face-name "Underline")
;	(set-personality-underline-p personality (nth 5 face-attributes)))
;    (set personality face)))

;;}}}



;;; Various regexp information shared by several modes.
;;; Information specific to a single mode should go in its load library.

(defconst lisp-voice-lock-keywords-1
  (list
   ;; Anything not a variable or type declaration is voiceified as a function.
   ;; It would be cleaner to allow preceding whitespace, but it would also be
   ;; about five times slower.
   (list (concat "^(\\(def\\("
		 ;; Variable declarations.
		 "\\(const\\(\\|ant\\)\\|ine-key\\(\\|-after\\)\\|var\\)\\|"
		 ;; Structure declarations.
		 "\\(class\\|struct\\|type\\)\\|"
		 ;; Everything else is a function declaration.
		 "\\([^ \t\n\(\)]+\\)"
		 "\\)\\)\\>"
		 ;; Any whitespace and declared object.
		 "[ \t'\(]*"
		 "\\(\\sw+\\)?")
	 '(1 voice-lock-keyword-personality)
	 '(8 (cond ((match-beginning 3) voice-lock-variable-name-personality)
		   ((match-beginning 6) voice-lock-type-personality)
		   (t voice-lock-function-name-personality))
	     nil t))
   )
 "Subdued level highlighting for Lisp modes.")

(defconst lisp-voice-lock-keywords-2
  (append lisp-voice-lock-keywords-1
   (list
    ;;
    ;; Control structures.  ELisp and CLisp combined.
;      (make-regexp
;       '("cond" "if" "while" "let\\*?" "prog[nv12*]?" "inline" "catch" "throw"
;	 "save-restriction" "save-excursion" "save-window-excursion"
;	 "save-selected-window" "save-match-data" "unwind-protect"
;	 "condition-case" "track-mouse"
;	 "eval-after-load" "eval-and-compile" "eval-when-compile"
;	 "when" "unless" "do" "flet" "labels" "return" "return-from"
;	 "with-output-to-temp-buffer" "with-timeout"))
    (cons
     (concat
      "(\\("
      "c\\(atch\\|ond\\(\\|ition-case\\)\\)\\|do\\|"
      "eval-\\(a\\(fter-load\\|nd-compile\\)\\|when-compile\\)\\|flet\\|"
      "i\\(f\\|nline\\)\\|l\\(abels\\|et\\*?\\)\\|prog[nv12*]?\\|"
      "return\\(\\|-from\\)\\|save-\\(excursion\\|match-data\\|restriction\\|"
      "selected-window\\|window-excursion\\)\\|t\\(hrow\\|rack-mouse\\)\\|"
      "un\\(less\\|wind-protect\\)\\|"
      "w\\(h\\(en\\|ile\\)\\|ith-\\(output-to-temp-buffer\\|timeout\\)\\)"
      "\\)\\>") 1)
    ;;
    ;; Feature symbols as references.
    '("(\\(featurep\\|provide\\|require\\)\\>[ \t']*\\(\\sw+\\)?"
      (1 voice-lock-keyword-personality) (2 voice-lock-reference-personality nil t))
    ;;
    ;; Words inside \\[] tend to be for `substitute-command-keys'.
    '("\\\\\\\\\\[\\(\\sw+\\)]" 1 voice-lock-reference-personality prepend)
    ;;
    ;; Words inside `' tend to be symbol names.
    '("`\\(\\sw\\sw+\\)'" 1 voice-lock-reference-personality prepend)
    ;;
    ;; CLisp `:' keywords as references.
    '("\\<:\\sw+\\>" 0 voice-lock-reference-personality prepend)
    ;;
    ;; ELisp and CLisp `&' keywords as types.
    '("\\<\\&\\sw+\\>" . voice-lock-type-personality)
    ))
  "Gaudy level highlighting for Lisp modes.")

(defvar lisp-voice-lock-keywords lisp-voice-lock-keywords-1
  "Default expressions to highlight in Lisp modes.")


(defvar scheme-voice-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\("
		   ;; Function names.
		   "\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\)\\|"
		   ;; Class names.
		   "\\(-class\\)"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")
	   '(1 voice-lock-keyword-personality)
	   '(8 (cond ((match-beginning 3) voice-lock-function-name-personality)
		     ((match-beginning 6) voice-lock-variable-name-personality)
		     (t voice-lock-type-personality))
	       nil t))
     ;;
     ;; Control structures.
;(make-regexp '("begin" "call-with-current-continuation" "call/cc"
;	       "call-with-input-file" "call-with-output-file" "case" "cond"
;	       "do" "else" "for-each" "if" "lambda"
;	       "let\\*?" "let-syntax" "letrec" "letrec-syntax"
;	       ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
;	       "and" "or" "delay"
;	       ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
;	       ;;"quasiquote" "quote" "unquote" "unquote-splicing"
;	       "map" "syntax" "syntax-rules"))
     (cons
      (concat "(\\("
	      "and\\|begin\\|c\\(a\\(ll\\(-with-\\(current-continuation\\|"
	      "input-file\\|output-file\\)\\|/cc\\)\\|se\\)\\|ond\\)\\|"
	      "d\\(elay\\|o\\)\\|else\\|for-each\\|if\\|"
	      "l\\(ambda\\|et\\(-syntax\\|\\*?\\|rec\\(\\|-syntax\\)\\)\\)\\|"
	      "map\\|or\\|syntax\\(\\|-rules\\)"
	      "\\)\\>") 1)
     ;;
     ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
     '("\\<<\\sw+>\\>" . voice-lock-type-personality)
     ;;
     ;; Scheme `:' keywords as references.
     '("\\<:\\sw+\\>" . voice-lock-reference-personality)
     ))
"Default expressions to highlight in Scheme modes.")


(defconst c-voice-lock-keywords-1 nil
  "Subdued level highlighting for C modes.")

(defconst c-voice-lock-keywords-2 nil
  "Medium level highlighting for C modes.")

(defconst c-voice-lock-keywords-3 nil
  "Gaudy level highlighting for C modes.")

(defconst c++-voice-lock-keywords-1 nil
  "Subdued level highlighting for C++ modes.")

(defconst c++-voice-lock-keywords-2 nil
  "Medium level highlighting for C++ modes.")

(defconst c++-voice-lock-keywords-3 nil
  "Gaudy level highlighting for C++ modes.")

;;; Objective-C.

(defconst objc-voice-lock-keywords-1 nil
  "Subdued level highlighting for Objective-C mode.")

(defconst objc-voice-lock-keywords-2 nil
  "Medium level highlighting for Objective-C mode.
See also `objc-voice-lock-extra-types'.")


(defvar objc-voice-lock-extra-types '("Class" "BOOL" "IMP" "SEL")
  "*List of extra types to voiceify in Objective-C mode.
Each list item should be a regexp not containing word-delimiters.
For example, a value of (\"Class\" \"BOOL\" \"IMP\" \"SEL\") means the words
Class, BOOL, IMP and SEL are treated as type names.

The value of this variable is used when Voice Lock mode is turned on."
)
(defvar java-voice-lock-extra-types '("[A-Z\300-\326\330-\337]\\sw+")
  "*List of extra types to voiceify in Java mode.
Each list item should be a regexp not containing word-delimiters.
For example, a value of (\"[A-Z\300-\326\330-\337]\\\\sw+\") means capitalised
words (and words conforming to the Java id spec) are treated as type names.

The value of this variable is used when Voice Lock mode is turned on."
)

(defun voice-lock-match-c-style-declaration-item-and-skip-to-next (limit)
  "Match, and move over, any declaration/definition item after point.
Matches after point, but ignores leading whitespace and `*' characters.
Does not move further than LIMIT.

The expected syntax of a declaration/definition item is `word' (preceded by
optional whitespace and `*' characters and proceeded by optional whitespace)
optionally followed by a `('.  Everything following the item (but belonging to
it) is expected to by skip-able by `scan-sexps', and items are expected to be
separated with a `,' and to be terminated with a `;'.

Thus the regexp matches after point:	word (
					^^^^ ^
Where the match subexpressions are:	  1  2

The item is delimited by (match-beginning 1) and (match-end 1).
If (match-beginning 2) is non-nil, the item is followed by a `('.

This function could be MATCHER in a MATCH-ANCHORED `voice-lock-keywords' item."
  (when (looking-at "[ \t*]*\\(\\sw+\\)[ \t]*\\((\\)?")
    (save-match-data
      (condition-case nil
	  (save-restriction
	    ;; Restrict to the end of line, currently guaranteed to be LIMIT.
	    (narrow-to-region (point-min) limit)
	    (goto-char (match-end 1))
	    ;; Move over any item value, etc., to the next item.
	    (while (not (looking-at "[ \t]*\\(\\(,\\)\\|;\\|$\\)"))
	      (goto-char (or (scan-sexps (point) 1) (point-max))))
	    (goto-char (match-end 2)))
	(error t)))))
(defconst objc-voice-lock-keywords-3 nil
  "Gaudy level highlighting for Objective-C mode.
See also `objc-voice-lock-extra-types'.")

;; Regexps written with help from Stephen Peters <speters@us.oracle.com> and
;; Jacques Duthen Prestataire <duthen@cegelec-red.fr>.
(let* ((objc-keywords
	(eval-when-compile
	  (regexp-opt '("break" "continue" "do" "else" "for" "if" "return"
			"switch" "while" "sizeof" "self" "super") t)))
       (objc-type-types
	`(mapconcat 'identity
	  (cons
	   (,@ (eval-when-compile
		 (regexp-opt
		  '("auto" "extern" "register" "static" "typedef" "struct"
		    "union" "enum" "signed" "unsigned" "short" "long"
		    "int" "char" "float" "double" "void" "volatile" "const"
		    "id" "oneway" "in" "out" "inout" "bycopy" "byref"))))
	   objc-voice-lock-extra-types)
	  "\\|"))
       (objc-type-depth `(regexp-opt-depth (,@ objc-type-types)))
       )
 (setq objc-voice-lock-keywords-1
  (append
   ;;
   ;; The list `c-voice-lock-keywords-1' less that for function names.
   (cdr c-voice-lock-keywords-1)
   (list
    ;;
    ;; Voiceify compiler directives.
    '("@\\(\\sw+\\)\\>"
      (1 voice-lock-keyword-personality)
      ("\\=[ \t:<(,]*\\(\\sw+\\)" nil nil
       (1 voice-lock-function-name-personality)))
    ;;
    ;; Voiceify method names and arguments.  Oh Lordy!
    ;; First, on the same line as the function declaration.
    '("^[+-][ \t]*\\(PRIVATE\\)?[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
      (1 voice-lock-type-personality nil t)
      (3 voice-lock-type-personality nil t)
      (4 voice-lock-function-name-personality)
      ("\\=[ \t]*\\(\\sw+\\)?:[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
       nil nil
       (1 voice-lock-function-name-personality nil t)
       (3 voice-lock-type-personality nil t)
       (4 voice-lock-variable-name-personality)))
    ;; Second, on lines following the function declaration.
    '(":" ("^[ \t]*\\(\\sw+\\)?:[ \t]*\\((\\([^)\n]+\\))\\)?[ \t]*\\(\\sw+\\)"
	   (beginning-of-line) (end-of-line)
	   (1 voice-lock-function-name-personality nil t)
	   (3 voice-lock-type-personality nil t)
	   (4 voice-lock-variable-name-personality)))
    )))

 (setq objc-voice-lock-keywords-2
  (append objc-voice-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Voiceify all type specifiers.
    `(eval .
      (cons (concat "\\<\\(" (,@ objc-type-types) "\\)\\>")
	    'voice-lock-type-personality))
    ;;
    ;; Voiceify all builtin keywords (except case, default and goto; see below).
    (concat "\\<" objc-keywords "\\>")
    ;;
    ;; Voiceify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
      (1 voice-lock-keyword-personality) (2 voice-lock-reference-personality nil t))
    ;; Voiceify tags iff sole statement on line, otherwise we detect selectors.
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	   (beginning-of-line) (end-of-line)
	   (1 voice-lock-reference-personality)))
    ;;
    ;; Voiceify null object pointers.
    '("\\<[Nn]il\\>" . voice-lock-reference-personality)
    )))

 (setq objc-voice-lock-keywords-3
  (append objc-voice-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to voiceify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Voiceify all storage classes and type specifiers, plus their items.
    `(eval .
      (list (concat "\\<\\(" (,@ objc-type-types) "\\)\\>"
		    "\\([ \t*&]+\\sw+\\>\\)*")
	    ;; Voiceify each declaration item.
	    (list 'voice-lock-match-c-style-declaration-item-and-skip-to-next
		  ;; Start with point after all type specifiers.
		  (list 'goto-char (list 'or (list 'match-beginning
						   (+ (,@ objc-type-depth) 2))
					 '(match-end 1)))
		  ;; Finish with point after first type specifier.
		  '(goto-char (match-end 1))
		  ;; Voiceify as a variable or function name.
		  '(1 (if (match-beginning 2)
			  voice-lock-function-name-personality
			voice-lock-variable-name-personality)))))
    ;;
    ;; Voiceify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (voice-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 2)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality))))
    ;;
    ;; Voiceify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 voice-lock-type-personality)
      (voice-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality))))
    )))
 )

(defvar objc-voice-lock-keywords objc-voice-lock-keywords-1
  "Default expressions to highlight in Objective-C mode.
See also `objc-voice-lock-extra-types'.")


;;; Java.

(defconst java-voice-lock-keywords-1 nil
  "Subdued level highlighting for Java mode.")

(defconst java-voice-lock-keywords-2 nil
  "Medium level highlighting for Java mode.
See also `java-voice-lock-extra-types'.")

(defconst java-voice-lock-keywords-3 nil
  "Gaudy level highlighting for Java mode.
See also `java-voice-lock-extra-types'.")

;; Regexps written with help from Fred White <fwhite@bbn.com> and
;; Anders Lindgren <andersl@csd.uu.se>.
(let* ((java-keywords
	(eval-when-compile
	  (regexp-opt
	   '("catch" "do" "else" "super" "this" "finally" "for" "if"
	     ;; Anders Lindgren <andersl@csd.uu.se> says these have gone.
	     ;; "cast" "byvalue" "future" "generic" "operator" "var"
	     ;; "inner" "outer" "rest"
	     "interface" "return" "switch" "throw" "try" "while") t)))
       ;;
       ;; These are immediately followed by an object name.
       (java-minor-types
	(eval-when-compile
	  (regexp-opt '("boolean" "char" "byte" "short" "int" "long"
			"float" "double" "void"))))
       ;;
       ;; These are eventually followed by an object name.
       (java-major-types
	(eval-when-compile
	  (regexp-opt
	   '("abstract" "const" "final" "synchronized" "transient" "static"
	     ;; Anders Lindgren <andersl@csd.uu.se> says this has gone.
	     ;; "threadsafe"
	     "volatile" "public" "private" "protected" "native"))))
       ;;
       ;; Random types immediately followed by an object name.
       (java-other-types
	'(mapconcat 'identity (cons "\\sw+\\.\\sw+" java-voice-lock-extra-types)
		    "\\|"))
       (java-other-depth `(regexp-opt-depth (,@ java-other-types)))
       )
 (setq java-voice-lock-keywords-1
  (list
   ;;
   ;; Voiceify class names.
   '("\\<\\(class\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 voice-lock-type-personality) (2 voice-lock-function-name-personality nil t))
   ;;
   ;; Voiceify package names in import directives.
   '("\\<\\(import\\|package\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 voice-lock-keyword-personality) (2 voice-lock-reference-personality nil t))
   ))

 (setq java-voice-lock-keywords-2
  (append java-voice-lock-keywords-1
   (list
    ;;
    ;; Voiceify all builtin type specifiers.
    (cons (concat "\\<\\(" java-minor-types "\\|" java-major-types "\\)\\>")
	  'voice-lock-type-personality)
    ;;
    ;; Voiceify all builtin keywords (except below).
    (concat "\\<" java-keywords "\\>")
    ;;
    ;; Voiceify keywords and targets, and case default/goto tags.
    (list "\\<\\(break\\|case\\|continue\\|goto\\)\\>[ \t]*\\(-?\\sw+\\)?"
	  '(1 voice-lock-keyword-personality) '(2 voice-lock-reference-personality nil t))
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:"
	   (beginning-of-line) (end-of-line)
	   (1 voice-lock-reference-personality)))
    ;;
    ;; Voiceify keywords and types; the first can be followed by a type list.
    (list (concat "\\<\\("
		  "implements\\|throws\\|"
		  "\\(extends\\|instanceof\\|new\\)"
		  "\\)\\>[ \t]*\\(\\sw+\\)?")
	  '(1 voice-lock-keyword-personality) '(3 voice-lock-type-personality nil t)
	  '("\\=[ \t]*,[ \t]*\\(\\sw+\\)"
	    (if (match-beginning 2) (goto-char (match-end 2))) nil
	    (1 voice-lock-type-personality)))
    ;;
    ;; Voiceify all constants.
    '("\\<\\(false\\|null\\|true\\)\\>" . voice-lock-reference-personality)
    ;;
    ;; Javadoc tags within comments.
    '("@\\(author\\|exception\\|return\\|see\\|version\\)\\>"
      (1 voice-lock-reference-personality prepend))
    '("@\\(param\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 voice-lock-reference-personality prepend)
      (2 voice-lock-variable-name-personality prepend t))
    )))

 (setq java-voice-lock-keywords-3
  (append java-voice-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to voiceify type specifiers individually, as Java is hairy.
   (list
    ;;
    ;; Voiceify random types in casts.
    `(eval .
      (list (concat "(\\(" (,@ java-other-types) "\\))"
		    "[ \t]*\\(\\sw\\|[\"\(]\\)")
	    ;; Voiceify the type name.
	    '(1 voice-lock-type-personality)))
    ;;
    ;; Voiceify random types immediately followed by an item or items.
    `(eval .
      (list (concat "\\<\\(" (,@ java-other-types) "\\)\\>"
		    "\\([ \t]*\\[[ \t]*\\]\\)*"
		    "[ \t]*\\sw")
	    ;; Voiceify the type name.
	    '(1 voice-lock-type-personality)))
    `(eval .
      (list (concat "\\<\\(" (,@ java-other-types) "\\)\\>"
		    "\\([ \t]*\\[[ \t]*\\]\\)*"
		    "\\([ \t]*\\sw\\)")
	    ;; Voiceify each declaration item.
	    (list 'voice-lock-match-c-style-declaration-item-and-skip-to-next
		  ;; Start and finish with point after the type specifier.
		  (list 'goto-char (list 'match-beginning
					 (+ (,@ java-other-depth) 3)))
		  (list 'goto-char (list 'match-beginning
					 (+ (,@ java-other-depth) 3)))
		  ;; Voiceify as a variable or function name.
		  '(1 (if (match-beginning 2)
			  voice-lock-function-name-personality
			voice-lock-variable-name-personality)))))
    ;;
    ;; Voiceify those that are immediately followed by an item or items.
    (list (concat "\\<\\(" java-minor-types "\\)\\>"
		  "\\([ \t]*\\[[ \t]*\\]\\)*")
	  ;; Voiceify each declaration item.
	  '(voice-lock-match-c-style-declaration-item-and-skip-to-next
	    ;; Start and finish with point after the type specifier.
	    nil (goto-char (match-end 0))
	    ;; Voiceify as a variable or function name.
	    (1 (if (match-beginning 2)
		   voice-lock-function-name-personality
		 voice-lock-variable-name-personality))))
    ;;
    ;; Voiceify those that are eventually followed by an item or items.
    (list (concat "\\<\\(" java-major-types "\\)\\>"
		  "\\([ \t]+\\sw+\\>"
		  "\\([ \t]*\\[[ \t]*\\]\\)*"
		  "\\)*")
	  ;; Voiceify each declaration item.
	  '(voice-lock-match-c-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 5) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Voiceify as a variable or function name.
	    (1 (if (match-beginning 2)
		   voice-lock-function-name-personality
		 voice-lock-variable-name-personality))))
    )))
 )

(defvar java-voice-lock-keywords java-voice-lock-keywords-3
  "Default expressions to highlight in Java mode.
See also `java-voice-lock-extra-types'.")


(defun voice-lock-match-c++-style-declaration-item-and-skip-to-next (limit)
  ;; Match, and move over, any declaration/definition item after point.
  ;; The expect syntax of an item is "word" or "word::word", possibly ending
  ;; with optional whitespace and a "(".  Everything following the item (but
  ;; belonging to it) is expected to by skip-able by `forward-sexp', and items
  ;; are expected to be separated with a ",".
  ;;
  ;; The regexp matches:	word::word (
  ;;				^^^^  ^^^^ ^
  ;; Match subexps are:		  1     3  4
  ;;
  ;; So, the item is delimited by (match-beginning 1) and (match-end 1).
  ;; If (match-beginning 3) is non-nil, that part of the item follows a ":".
  ;; If (match-beginning 4) is non-nil, the item is followed by a "(".
  (if (looking-at "[ \t*&]*\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ \t]*\\((\\)?")
      (save-match-data
	(condition-case nil
	    (save-restriction
	      ;; Restrict to the end of line, currently guaranteed to be LIMIT.
	      (narrow-to-region (point-min) limit)
	      (goto-char (match-end 1))
	      ;; Move over any item value, etc., to the next item.
	      (while (not (looking-at "[ \t]*\\(\\(,\\)\\|;\\|$\\)"))
		(goto-char (or (scan-sexps (point) 1) (point-max))))
	      (goto-char (match-end 2)))
	  (error t)))))

(let ((c-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while")
       "break\\|continue\\|do\\|else\\|for\\|if\\|return\\|switch\\|while")
      (c-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union" "enum"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const"
       ;"[A-Z]\\sw+")
       (concat
        "auto\\|c\\(har\\|onst\\)\\|double\\|e\\(num\\|xtern\\)\\|"
        "[A-Z]\\sw+\\|"
	       "float\\|int\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|typedef\\|"
	       "un\\(ion\\|signed\\)\\|vo\\(id\\|latile\\)"))	; 6 ()s deep.
      (c++-keywords
;      ("break" "continue" "do" "else" "for" "if" "return" "switch" "while"
;	"asm" "catch" "delete" "new" "operator" "sizeof" "this" "throw" "try"
;       "protected" "private" "public")
       (concat "asm\\|break\\|c\\(atch\\|ontinue\\)\\|d\\(elete\\|o\\)\\|"
	       "else\\|for\\|if\\|new\\|"
	       "p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|return\\|"
	       "s\\(izeof\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while"))
      (c++-type-types
;      ("auto" "extern" "register" "static" "typedef" "struct" "union"
;      "enum"
       ;"[A-Z]\\sw+\\|"
;	"signed" "unsigned" "short" "long" "int" "char" "float" "double"
;	"void" "volatile" "const" "class" "inline" "friend" "bool"
;	"virtual" "complex" "template")
       (concat
        "auto\\|bool\\|c\\(har\\|lass\\|o\\(mplex\\|nst\\)\\)\\|"
        "[A-Z]\\sw+\\|"
	       "double\\|e\\(num\\|xtern\\)\\|f\\(loat\\|riend\\)\\|"
	       "in\\(line\\|t\\)\\|long\\|register\\|"
	       "s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|"
	       "t\\(emplate\\|ypedef\\)\\|un\\(ion\\|signed\\)\\|"
	       "v\\(irtual\\|o\\(id\\|latile\\)\\)"))		; 11 ()s deep.
      )
 (setq c-voice-lock-keywords-1
  (list
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;;
   ;; Voiceify function name definitions (GNU style; without type on line).
   (list (concat "^\\(\\sw+\\)[ \t]*(") 1 'voice-lock-function-name-personality)
   ;;
   ;; Voiceify filenames in #include <...> preprocessor directives as strings.
   '("^#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 voice-lock-string-personality)
   ;;
   ;; Voiceify function macro names.
   '("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 voice-lock-function-name-personality)
   ;;
   ;; Voiceify symbol names in #elif or #if ... defined preprocessor directives.
   '("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 voice-lock-reference-personality) (2 voice-lock-variable-name-personality nil t)))
   ;;
   ;; Voiceify otherwise as symbol names, and the preprocessor directive names.
   '("^#[ \t]*\\(\\sw+\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 voice-lock-reference-personality) (2 voice-lock-variable-name-personality nil t))
   ))

 (setq c-voice-lock-keywords-2
  (append c-voice-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Voiceify all type specifiers.
    (cons (concat "\\<\\(" c-type-types "\\)\\>") 'voice-lock-type-personality)
    ;;
    ;; Voiceify all builtin keywords (except case, default and goto; see below).
    (cons (concat "\\<\\(" c-keywords "\\)\\>") 'voice-lock-keyword-personality)
    ;;
    ;; Voiceify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 voice-lock-keyword-personality) (2 voice-lock-reference-personality nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:" 1 voice-lock-reference-personality)
    )))

 (setq c-voice-lock-keywords-3
  (append c-voice-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to voiceify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Voiceify all storage classes and type specifiers, plus their items.
    (list (concat "\\<\\(" c-type-types "\\)\\>"
		  "\\([ \t*&]+\\sw+\\>\\)*")
	  ;; Voiceify each declaration item.
	  '(voice-lock-match-c++-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 8) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Voiceify as a variable or function name.
	    (1 (if (match-beginning 4)
		   voice-lock-function-name-personality
		 voice-lock-variable-name-personality))))
    ;;
    ;; Voiceify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (voice-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 4)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality))))
    ;;
    ;; Voiceify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 voice-lock-type-personality)
      (voice-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 4)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality))))
    )))

 (setq c++-voice-lock-keywords-1
  (append
   ;;
   ;; The list `c-voice-lock-keywords-1' less that for function names.
   (cdr c-voice-lock-keywords-1)
   ;;
   ;; Voiceify function name definitions, possibly incorporating class name.
   (list
    '("^\\(\\sw+\\)\\(::\\(\\sw+\\)\\)?[ \t]*("
      (1 (if (match-beginning 2)
	     voice-lock-type-personality
	   voice-lock-function-name-personality))
      (3 voice-lock-function-name-personality nil t))
    )))

 (setq c++-voice-lock-keywords-2
  (append c++-voice-lock-keywords-1
   (list
    ;;
    ;; The list `c-voice-lock-keywords-2' for C++ plus operator overloading.
    (cons (concat "\\<\\(" c++-type-types "\\)\\>") 'voice-lock-type-personality)
    ;;
    ;; Voiceify operator function name overloading.
    '("\\<\\(operator\\)\\>[ \t]*\\([[(><!=+-][])><=+-]?\\)?"
      (1 voice-lock-keyword-personality) (2 voice-lock-function-name-personality nil t))
    ;;
    ;; Voiceify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>[ \t]*\\(\\sw+\\)?"
      (1 voice-lock-keyword-personality) (2 voice-lock-reference-personality nil t))
    '("^[ \t]*\\(\\sw+\\)[ \t]*:[^:]" 1 voice-lock-reference-personality)
    ;;
    ;; Voiceify other builtin keywords.
    (cons (concat "\\<\\(" c++-keywords "\\)\\>") 'voice-lock-keyword-personality)
    )))

 (setq c++-voice-lock-keywords-3
  (append c++-voice-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   (list
    ;;
    ;; Voiceify all storage classes and type specifiers, plus their items.
    (list (concat "\\<\\(" c++-type-types "\\)\\>"
		  "\\([ \t*&]+\\sw+\\>\\)*")
	  ;; Voiceify each declaration item.
	  '(voice-lock-match-c++-style-declaration-item-and-skip-to-next
	    ;; Start with point after all type specifiers.
	    (goto-char (or (match-beginning 13) (match-end 1)))
	    ;; Finish with point after first type specifier.
	    (goto-char (match-end 1))
	    ;; Voiceify as a variable or function name.
	    (1 (cond ((match-beginning 2) voice-lock-type-personality)
		     ((match-beginning 4) voice-lock-function-name-personality)
		     (t voice-lock-variable-name-personality)))
	    (3 (if (match-beginning 4)
		   voice-lock-function-name-personality
		 voice-lock-variable-name-personality) nil t)))
    ;;
    ;; Voiceify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (voice-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 (if (match-beginning 4)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality))))
    ;;
    ;; Voiceify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 voice-lock-type-personality)
      (voice-lock-match-c++-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (cond ((match-beginning 2) voice-lock-type-personality)
		((match-beginning 4) voice-lock-function-name-personality)
		(t voice-lock-variable-name-personality)))
       (3 (if (match-beginning 4)
	      voice-lock-function-name-personality
	    voice-lock-variable-name-personality) nil t)))
    )))
 )

(defvar c-voice-lock-keywords c-voice-lock-keywords-1
  "Default expressions to highlight in C mode.")

(defvar c++-voice-lock-keywords c++-voice-lock-keywords-3
  "Default expressions to highlight in C++ mode.")


(defvar tex-voice-lock-keywords
;  ;; Regexps updated with help from Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 voice-lock-function-name-personality)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 voice-lock-reference-personality)
;    ;; It seems a bit dubious to use `bold' and `italic' personalities since we might
;    ;; not be able to display those voices.
;    ("{\\\\bf\\([^}]+\\)}" 1 'bold keep)
;    ("{\\\\\\(em\\|it\\|sl\\)\\([^}]+\\)}" 2 'italic keep)
;    ("\\\\\\([a-zA-Z@]+\\|.\\)" . voice-lock-keyword-personality)
;    ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 voice-lock-function-name-personality keep))
  ;; Rewritten and extended for LaTeX2e by Ulrik Dickow <dickow@nbi.dk>.
  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
     2 voice-lock-function-name-personality)
    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
     2 voice-lock-reference-personality)
    ("^[ \t]*\\\\def\\\\\\(\\(\\w\\|@\\)+\\)" 1 voice-lock-function-name-personality)
    "\\\\\\([a-zA-Z@]+\\|.\\)"
    ;; It seems a bit dubious to use `bold' and `italic' personalities since we might
    ;; not be able to display those voices.
    ;; LaTeX2e: \emph{This is emphasized}.
    ("\\\\emph{\\([^}]+\\)}" 1 'italic keep)
    ;; LaTeX2e: \textbf{This is bold}, \textit{...}, \textsl{...}
    ("\\\\text\\(\\(bf\\)\\|it\\|sl\\){\\([^}]+\\)}"
     3 (if (match-beginning 2) 'bold 'italic) keep)
    ;; Old-style bf/em/it/sl. Stop at `\\' and un-escaped `&', for good tables.
    ("\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)"
     3 (if (match-beginning 2) 'bold 'italic) keep))
  "Default expressions to highlight in TeX modes.")

;; Install ourselves:

(unless (assq 'voice-lock-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(voice-lock-mode " Voice") minor-mode-alist)))

 
;; Provide ourselves:

(provide 'voice-lock)

;;; voice-lock.el ends here
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
