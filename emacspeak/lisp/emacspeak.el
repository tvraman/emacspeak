;;; emacspeak.el --- Emacspeak -- The Complete Audio Desktop
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak: A speech interface to Emacs
;;; Keywords: Emacspeak, Speech, Dectalk,
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
;;{{{ Introduction 

;;; Commentary:
;;;The complete audio desktop.

;;;Emacspeak extends Emacs to be a fully functional audio desktop.
;;; This is the main emacspeak module.
;;; It actually does very little:
;;; It loads the various parts of the system.
;;; Code:

;;}}}
;;{{{ Required modules
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)
(require 'emacspeak-load-path)
(require 'emacspeak-preamble)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-sounds)
;;}}}
;;{{{ autoloads

(load-library "emacspeak-loaddefs")
(load-library "emacspeak-cus-load")

;;}}}
;;{{{  Customize groups 
(defconst emacspeak-version
  (let ((x "$Revision$"))
    (string-match "[0-9.]+" x)
    (substring x (match-beginning 0)
               (match-end 0)))
  "Version number for Emacspeak.")

(defgroup emacspeak nil
  "Emacspeak: The Complete Audio Desktop  "
  :link '(url-link :tag "SourceForge" "http://emacspeak.sf.net")
  :link '(url-link :tag "Papers" "http://emacspeak.sf.net/publications")
  :link '(url-link :tag "Mail"
                   "http://www.cs.vassar.edu/~priestdo/emacspeak/")
  :link '(url-link :tag "Search" "http://www.cs.vassar.edu/cgi-bin/emacspeak-search")
  :link '(url-link :tag "Applications"
                   "http://emacspeak.sf.net/applications.html")
  :link '(url-link :tag "Guide"
                   "http://emacspeak.sf.net/user-guide")
  :link '(url-link :tag "Tips"
                   "http://emacspeak.sf.net/tips.html")
  :link   (list 'file-link :tag "NEWS" (expand-file-name "etc/NEWS" emacspeak-directory))
  :link   (list 'file-link :tag "FAQ" (expand-file-name "etc/FAQ" emacspeak-directory))
  :link '(custom-manual "(emacspeak)Top")
;;; end links 
  :prefix "emacspeak-"
  :group 'applications
  :group 'accessibility
  :version emacspeak-version)

(defcustom emacspeak-startup-hook nil
  "Hook to run after starting emacspeak."
  :type 'hook
  :group 'emacspeak)
;;;###autoload
(defcustom emacspeak-media-player 'emacspeak-m-player
  "Default media player to use.
This is a Lisp function that takes a resource locator."
  :type 'function
  :group 'emacspeak)

;;}}}
;;{{{ Package Setup Helper

(defun emacspeak-do-package-setup (package module)
  "Setup Emacspeak extension for a specific PACKAGE.
This function  adds the appropriate form to
`after-load-alist' to set up Emacspeak support for a given
package.
Argument MODULE specifies the emacspeak module that implements the speech-enabling extensions."
  (declare (special load-history))
  (cond
   ((assoc package load-history)
    (require module)
    (emacspeak-fix-commands-loaded-from package))
   (t
    (add-hook 'after-load-alist
              (`
               ((, package)
                (progn
                  (require (quote (, module )))
                  (emacspeak-fix-commands-loaded-from (, package)))))))))

;;}}}
;;{{{ Setup package extensions
(emacspeak-do-package-setup "add-log" 'emacspeak-add-log)
(emacspeak-do-package-setup "analog" 'emacspeak-analog)
(emacspeak-do-package-setup "ansi-color" 'emacspeak-ansi-color)                            
(emacspeak-do-package-setup "arc-mode" 'emacspeak-arc)
(emacspeak-do-package-setup "babel" 'emacspeak-babel )
(emacspeak-do-package-setup "bbdb" 'emacspeak-bbdb )
(emacspeak-do-package-setup "bibtex" 'emacspeak-bibtex)
(emacspeak-do-package-setup "bookmark" 'emacspeak-bookmark)
(emacspeak-do-package-setup "browse-kill-ring" 'emacspeak-browse-kill-ring )
(emacspeak-do-package-setup "buff-sel" 'emacspeak-buff-sel)
(emacspeak-do-package-setup "bs" 'emacspeak-bs)
(emacspeak-do-package-setup "buff-menu" 'emacspeak-buff-menu)
(emacspeak-do-package-setup "cc-mode" 'emacspeak-c)
(emacspeak-do-package-setup "calc" 'emacspeak-calc)
(emacspeak-do-package-setup "calculator" 'emacspeak-calculator)
(emacspeak-do-package-setup "calendar" 'emacspeak-calendar)
(emacspeak-do-package-setup "cc-mode" 'emacspeak-c)
(emacspeak-do-package-setup "semantic" 'emacspeak-cedet)
(emacspeak-do-package-setup "checkdoc" 'emacspeak-checkdoc)
(emacspeak-do-package-setup "cmuscheme" 'emacspeak-cmuscheme)
(emacspeak-do-package-setup "compile" 'emacspeak-compile)
(emacspeak-do-package-setup "cperl-mode" 'emacspeak-cperl)
(emacspeak-do-package-setup "ecb" 'emacspeak-ecb)
(emacspeak-do-package-setup "cus-edit" 'emacspeak-custom)
(emacspeak-do-package-setup "damlite" 'emacspeak-damlite)
(emacspeak-do-package-setup "desktop" 'emacspeak-desktop )
(emacspeak-do-package-setup "dired" 'emacspeak-dired )
(emacspeak-do-package-setup "dismal" 'emacspeak-dismal)
(emacspeak-do-package-setup "dictation" 'emacspeak-dictation)
(emacspeak-do-package-setup "dictionary" 'emacspeak-dictionary)
(emacspeak-do-package-setup "dmacro" 'emacspeak-dmacro)
(emacspeak-do-package-setup "doctor" 'emacspeak-entertain)
(emacspeak-do-package-setup "dunnet" 'emacspeak-entertain)
(emacspeak-do-package-setup "ediary" 'emacspeak-ediary)
(emacspeak-do-package-setup "ediff" 'emacspeak-ediff)
(emacspeak-do-package-setup "emms" 'emacspeak-emms)
(emacspeak-do-package-setup "eperiodic" 'emacspeak-eperiodic)
(emacspeak-do-package-setup "erc" 'emacspeak-erc)
(emacspeak-do-package-setup "eshell" 'emacspeak-eshell)
(emacspeak-do-package-setup "enriched" 'emacspeak-enriched)
(emacspeak-do-package-setup "facemenu" 'emacspeak-facemenu)
(emacspeak-do-package-setup "find-dired" 'emacspeak-find-dired)
(emacspeak-do-package-setup "find-func" 'emacspeak-find-func)
(emacspeak-do-package-setup "flyspell" 'emacspeak-flyspell)
(emacspeak-do-package-setup "folding" 'emacspeak-folding)
(emacspeak-do-package-setup "forms" 'emacspeak-forms)
(emacspeak-do-package-setup "generic" 'emacspeak-generic)
(emacspeak-do-package-setup "gnus" 'emacspeak-gnus)
(emacspeak-do-package-setup "gnuplot" 'emacspeak-gnuplot)
(emacspeak-do-package-setup "gomoku" 'emacspeak-gomoku)
(emacspeak-do-package-setup "gud" 'emacspeak-gud)
(emacspeak-do-package-setup "gdb-ui" 'emacspeak-gud)
(emacspeak-do-package-setup "hangman" 'emacspeak-entertain)
(emacspeak-do-package-setup "hideshow" 'emacspeak-hideshow)
					;(emacspeak-do-package-setup "html-helper-mode" 'html-voice )
(emacspeak-do-package-setup "hyperbole" 'emacspeak-hyperbole)
(emacspeak-do-package-setup "imenu" 'emacspeak-imenu)
(emacspeak-do-package-setup "ibuffer" 'emacspeak-ibuffer)
(emacspeak-do-package-setup "ido" 'emacspeak-ido)
(emacspeak-do-package-setup "info" 'emacspeak-info)
(emacspeak-do-package-setup "ispell" 'emacspeak-ispell)
(emacspeak-do-package-setup "iswitchb" 'emacspeak-iswitchb)
(emacspeak-do-package-setup "jabber" 'emacspeak-jabber)
(emacspeak-do-package-setup "jde" 'emacspeak-jde)
(emacspeak-do-package-setup "kmacro" 'emacspeak-kmacro)
(emacspeak-do-package-setup "kotl" 'emacspeak-kotl)
(emacspeak-do-package-setup "make-mode" 'emacspeak-make-mode)
(emacspeak-do-package-setup "man" 'emacspeak-man)
(emacspeak-do-package-setup "message" 'emacspeak-message)
(emacspeak-do-package-setup "meta-mode" 'emacspeak-metapost)
(emacspeak-do-package-setup "mpg123" 'emacspeak-mpg123)
(emacspeak-do-package-setup "midge-mode" 'emacspeak-midge)
(emacspeak-do-package-setup "mpuz" 'emacspeak-entertain)
(emacspeak-do-package-setup "mspools" 'emacspeak-mspools)
(emacspeak-do-package-setup "nero" 'emacspeak-nero)
(emacspeak-do-package-setup "net-utils" 'emacspeak-net-utils)
(emacspeak-do-package-setup "newsticker" 'emacspeak-newsticker)
(emacspeak-do-package-setup "nxml-mode" 'emacspeak-nxml)
(emacspeak-do-package-setup "oobr" 'emacspeak-oo-browser)
(emacspeak-do-package-setup "org" 'emacspeak-org)
(emacspeak-do-package-setup "outline" 'emacspeak-outline)
(emacspeak-do-package-setup "perl-mode" 'emacspeak-perl)
(emacspeak-do-package-setup "php-mode" 'emacspeak-php-mode)
(emacspeak-do-package-setup "pcl-cvs" 'emacspeak-pcl-cvs)
(emacspeak-do-package-setup "pcvs" 'emacspeak-pcl-cvs)
(emacspeak-do-package-setup "psgml" 'emacspeak-psgml)
(emacspeak-do-package-setup "python-mode" 'emacspeak-python)
(emacspeak-do-package-setup "re-builder" 'emacspeak-re-builder)
(emacspeak-do-package-setup "reftex" 'emacspeak-reftex)
(emacspeak-do-package-setup "rmail" 'emacspeak-rmail)
(emacspeak-do-package-setup "rpm-spec-mode" 'emacspeak-rpm-spec)
(emacspeak-do-package-setup "sgml-mode" 'emacspeak-sgml-mode)
(emacspeak-do-package-setup "sh-script" 'emacspeak-sh-script)
(emacspeak-do-package-setup "sigbegone" 'emacspeak-sigbegone)
(emacspeak-do-package-setup "solitaire" 'emacspeak-solitaire)
(emacspeak-do-package-setup "speedbar" 'emacspeak-speedbar)
(emacspeak-do-package-setup "sawfish" 'emacspeak-sawfish)
(emacspeak-do-package-setup "ses" 'emacspeak-ses)
(emacspeak-do-package-setup "sql-mode" 'emacspeak-sql)
(emacspeak-do-package-setup "sql" 'emacspeak-sql)
(emacspeak-do-package-setup "supercite" 'emacspeak-supercite)
(emacspeak-do-package-setup "sudoku" 'emacspeak-sudoku)
(emacspeak-do-package-setup "swbuff" 'emacspeak-swbuff)
(emacspeak-do-package-setup "tar-mode" 'emacspeak-tar)
(emacspeak-do-package-setup "tcl" 'emacspeak-tcl)
(emacspeak-do-package-setup "tdtd" 'emacspeak-tdtd)
(emacspeak-do-package-setup "xslide" 'emacspeak-xslide)
(emacspeak-do-package-setup "xslt-process" 'emacspeak-xslt-process)
(emacspeak-do-package-setup "tempo" 'emacspeak-tempo)
(emacspeak-do-package-setup "tnt" 'emacspeak-tnt)
(emacspeak-do-package-setup "term" 'emacspeak-eterm )
(emacspeak-do-package-setup "eudc" 'emacspeak-eudc )
(emacspeak-do-package-setup "tetris" 'emacspeak-tetris)
(emacspeak-do-package-setup "tex-site" 'emacspeak-auctex)
(emacspeak-do-package-setup "texinfo" 'emacspeak-texinfo)
(emacspeak-do-package-setup "tmm" 'emacspeak-facemenu)
(emacspeak-do-package-setup "todo-mode" 'emacspeak-todo-mode)
(emacspeak-do-package-setup "view" 'emacspeak-view)
(emacspeak-do-package-setup "view-process-mode" 'emacspeak-view-process)
(emacspeak-do-package-setup "vm" 'emacspeak-vm)
(emacspeak-do-package-setup "w3" 'emacspeak-w3)
(emacspeak-do-package-setup "w3m" 'emacspeak-w3m)
(emacspeak-do-package-setup "wdired" 'emacspeak-wdired)
(emacspeak-do-package-setup "wid-edit" 'emacspeak-widget)
(emacspeak-do-package-setup "windmove" 'emacspeak-windmove)
(emacspeak-do-package-setup "winring" 'emacspeak-winring)
(emacspeak-do-package-setup "wrolo" 'emacspeak-wrolo)

;;}}}
;;{{{ finder

;;; Finder is special -- it needs to conditionally
;;; regenerate the database
(add-hook 'after-load-alist
          '("finder"
            (progn
              (load-library "emacspeak-finder")
              (unless (file-newer-than-file-p
                       (expand-file-name
                        "emacspeak-finder-inf.el" 
			emacspeak-lisp-directory)
                       (expand-file-name
                        emacspeak-lisp-directory
			"emacspeak.el"))
                (emacspeak-finder-compile-keywords))
              (load-library "emacspeak-finder-inf")
              (push
               (cons 'emacspeak "Audio Desktop")
               finder-known-keywords))))

;;}}}
;;{{{  Submit bugs

(defconst emacspeak-bug-address
  "raman@cs.cornell.edu"
  "Address of the maintainer of this package.")

(defun emacspeak-submit-bug ()
  "Function to submit a bug to the programs maintainer."
  (interactive)
  (require 'reporter)
  (when
      (yes-or-no-p "Are you sure you want to submit a bug report? ")
    (let (
          (vars '(window-system
                  window-system-version
                  emacs-version
                  system-type
                  emacspeak-version  dtk-program
                  dtk-speech-rate dtk-character-scale
                  dtk-split-caps dtk-capitalize
                  dtk-punctuation-mode
                  emacspeak-line-echo  emacspeak-word-echo
                  emacspeak-character-echo
                  emacspeak-use-auditory-icons
                  emacspeak-audio-indentation )))
      (mapcar
       (function
        (lambda (x)
          (if (not (and (boundp x) (symbol-value x)))
              (setq vars (delq x vars))))) vars)
      (reporter-submit-bug-report  emacspeak-bug-address
                                   (concat "Emacspeak Version: " emacspeak-version )
                                   vars
                                   nil nil
                                   "Description of Problem:"))))

;;}}}
;;{{{ exporting emacspeak environment to subprocesses

(defun emacspeak-export-environment ()
  "Export shell environment.
This exports emacspeak's system variables to the environment
so it can be passed to subprocesses.
Additionally, we set EMACS_UNIBYTE to avoid problems under
Emacs 20.3"
  (declare (special emacspeak-directory
                    emacspeak-play-program
                    emacspeak-sounds-directory
                    emacspeak-unibyte))
  (when emacspeak-unibyte
    (setenv "EMACS_UNIBYTE" "1"))
  (setenv "EMACSPEAK_DIR" emacspeak-directory)
  (setenv "EMACSPEAK_SOUNDS_DIR" emacspeak-sounds-directory)
  (setenv "EMACSPEAK_PLAY_PROGRAM" emacspeak-play-program)
  )

;;}}}
;;{{{ setup programming modes 

;;; turn on automatic voice locking , split caps and punctuations for programming modes
;;;###autoload
(defun emacspeak-setup-programming-mode ()
  "Setup programming mode. Turns on audio indentation and
sets punctuation mode to all, activates the dictionary and turns on split caps."
  (declare (special dtk-split-caps
                    emacspeak-audio-indentation))
  (dtk-set-punctuations 'all)
  (or dtk-split-caps
      (dtk-toggle-split-caps))
  (emacspeak-pronounce-refresh-pronunciations)
  (or emacspeak-audio-indentation
      (emacspeak-toggle-audio-indentation))
  (emacspeak-dtk-sync))

(defun emacspeak-setup-programming-modes ()
  "Setup programming modes."
  (mapcar
   (function (lambda (hook)
               (add-hook hook
                         'emacspeak-setup-programming-mode)))
   (list 'c-mode-common-hook
         'prolog-mode-hook
         'lisp-mode-hook
         'emacs-lisp-mode-hook
         'lisp-interaction-mode-hook
         'midge-mode-hook
         'meta-common-mode-hook
         'perl-mode-hook
         'cperl-mode-hook
         'sh-mode-hook
         'sql-mode-hook
         'sgml-mode-hook
         'xml-mode-hook
	 'nxml-mode-hook
         'xsl-mode-hook
         'makefile-mode-hook
         'TeX-mode-hook
         'LaTeX-mode-hook
         'bibtex-mode-hook
         'tcl-mode-hook
         'html-helper-mode-hook
         'scheme-mode-hook
         'dired-mode-hook)))

;;}}}
;;{{{ set up after-init-hook to fix interactive functions 

(add-hook 'after-init-hook
          'emacspeak-fix-commands-that-use-interactive)

;;}}}
;;{{{ Emacspeak:

(defcustom emacspeak-play-emacspeak-startup-icon nil
  "If set to T, emacspeak plays its icon as it launches."
  :type 'boolean
  :group 'emacspeak)
(defvar emacspeak-unibyte t
  "Set this to nil before starting  emacspeak 
if you are running in a multibyte enabled environment.")

(defun emacspeak()
  "Starts the Emacspeak speech subsystem.  Use emacs as you
normally would, emacspeak will provide you spoken feedback
as you work.  Emacspeak also provides commands for having
parts of the current buffer, the mode-line etc to be spoken.

If you are hearing this description as a result of pressing
\\[emacspeak-describe-emacspeak] you may want to press
\\[dtk-stop] to stop speech, and then use the arrow keys to
move around in the Help buffer to read the rest of this
description, which includes a summary of all emacspeak
keybindings.

All emacspeak commands use \\[emacspeak-prefix-command] as a
prefix key.  You can also set the state of the TTS engine  by
using \\[emacspeak-dtk-submap-command] as a prefix.  Here is
a summary of all emacspeak commands along with their
bindings.  You need to precede the keystrokes listed below
with \\[emacspeak-prefix-command].

Emacspeak also provides a fluent speech extension to the
emacs terminal emulator (eterm).  Note: You need to use the
term package that comes with emacs-19.29 and later.

\\{emacspeak-keymap}

See the online documentation for individual commands and
functions for details.   "
  (interactive)
  (declare (special mark-even-if-inactive
                    emacspeak-pronounce-load-pronunciations-on-startup
                    emacspeak-pronounce-dictionaries-file
                    default-enable-multibyte-characters
                    emacspeak-unibyte
                    emacspeak-play-program
                    emacspeak-sounds-directory))
;;; fixes transient mark mode in emacspeak 
  (setq mark-even-if-inactive t)
;;; force unibyte
  (when emacspeak-unibyte
    (setq default-enable-multibyte-characters nil))
  (emacspeak-export-environment)
  (require 'emacspeak-aumix)
  (when (featurep 'ido)
    (require 'emacspeak-ido))
  (require 'emacspeak-sounds)
  (dtk-initialize)
  (require 'emacspeak-personality)
  (require 'emacspeak-redefine)
  (require 'emacspeak-fix-interactive)
  (require 'emacspeak-keymap)
  (require 'emacspeak-advice)
  (require 'emacspeak-replace)
  (require 'emacspeak-buff-menu)
  (when (and  emacspeak-play-emacspeak-startup-icon 
              (file-exists-p "/usr/bin/mpg123"))
    (start-process "mp3" nil "mpg123"
                   "-q"
                   (expand-file-name "emacspeak.mp3" emacspeak-sounds-directory)))
  (emacspeak-sounds-define-theme-if-necessary emacspeak-sounds-default-theme)
  (when emacspeak-pronounce-load-pronunciations-on-startup
    (emacspeak-pronounce-load-dictionaries emacspeak-pronounce-dictionaries-file))
  (run-hooks 'emacspeak-startup-hook)
  (emacspeak-setup-programming-modes)
					;(require 'emacspeak-wizards)
  (tts-with-punctuations 'some
			 (dtk-speak
			  (format "  Press %s to get an   overview of emacspeak  %s \
 I am  completely operational,  and all my circuits are functioning perfectly! "
				  (substitute-command-keys
				   "\\[emacspeak-describe-emacspeak]" )
				  emacspeak-version))))

(defun emacspeak-describe-emacspeak ()
  "Give a brief overview of emacspeak."
  (interactive)
  (describe-function 'emacspeak)
  (switch-to-buffer "*Help*")
  (dtk-set-punctuations 'all)
  (emacspeak-speak-buffer))

;;}}}
(provide 'emacspeak)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
