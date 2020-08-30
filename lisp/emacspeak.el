;;; emacspeak.el --- The Complete Audio Desktop  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Emacspeak: A speech interface to Emacs
;;; Keywords: Emacspeak, Speech, Dectalk,
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 16:33:47 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4642 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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

;;;Emacspeak extends Emacs to be a fully functional audio desktop.
;;; This is the main emacspeak module.
;;; It actually does very little:
;;; It sets up Emacs to load package-specific
;;; Emacspeak modules as each package is loaded.
;;; It implements function emacspeak which loads the rest of the system.

;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-sounds)
(require 'emacspeak-fix-interactive)

;;}}}
;;{{{ autoloads

(unless noninteractive
  (mapc
   #'(lambda (f)
       (let ((file-name-handler-alist nil)
             (load-source-file-function  nil))
         (load (expand-file-name f emacspeak-lisp-directory))))
   '("emacspeak-loaddefs.el" "emacspeak-cus-load.el"
     "g-client/g-loaddefs" "g-client/g-cus-load")))
;;}}}
;;{{{  Customize groups

(defgroup emacspeak nil
  "Emacspeak: The Complete Audio Desktop  "
  :link '(url-link :tag "Web" "http://emacspeak.sf.net"
                   :help-echo "Visit Emacspeak Web Site")
  :link '(url-link :tag "Blog" "http://emacspeak.blogspot.com"
                   :help-echo "Read Emacspeak Blog")
  :link '(url-link :tag "Papers"
                   "http://emacspeak.sf.net/publications"
                   :help-echo "Papers describing Emacspeak
design and implementation.")
  :link '(url-link :tag "Gist" "https://gist.github.com/tvraman"
                   :help-echo "Useful Code Fragments")
  :link '(url-link :tag "Emacs Tour" "http://www.gnu.org/s/emacs/tour/"
                   :help-echo "A guided Tour Of Emacs")
  :link '(url-link :tag "Search"
                   "http://www.cs.vassar.edu/cgi-bin/emacspeak-search"
                   :help-echo "Search Emacspeak mail archive at Vassar.")
  :link '(url-link :tag "Apps"
                   "https://tvraman.github.io/emacspeak/applications.html"
                   :help-echo "Browse available  applications on
the Emacspeak desktop.")
  :link '(url-link :tag "Guide"
                   "https://tvraman.github.io/emacspeak/manual"
                   :help-echo "Read online user guide.")
  :link '(url-link :tag "Tips"
                   "https://tvraman.github.io/emacspeak/tips.html"
                   :help-echo "Read Emacspeak Tips and Tricks.")
  :link   (list 'file-link :tag "NEWS" (expand-file-name
                                        "etc/NEWS"
                                        emacspeak-directory)
                :help-echo "What's New In This Release")
  :link '(custom-manual "(emacspeak)Top")
;;; end links
  :prefix "emacspeak-"
  :group 'applications
  :group 'accessibility)

;;;###autoload

;;}}}
;;{{{ Package Setup Helper

(defun emacspeak-do-package-setup (package module)
  "Setup Emacspeak extension for a specific PACKAGE.  This function adds
the appropriate form to `after-load-alist' to set up Emacspeak support
for a given package.  Argument MODULE (a symbol)specifies the emacspeak module
that implements the speech-enabling extensions for `package' (a string)."
  (eval-after-load package
    `(progn
       (require ',module)
       (emacspeak-fix-commands-loaded-from ,(format "%s" module))
       (emacspeak-fix-commands-loaded-from ,package))))

;;; DocView
(declare-function doc-view-open-text "doc-view")
(eval-after-load "doc-view"
  `(add-hook 'doc-view-mode-hook #'doc-view-open-text))

;;; find-func:
(eval-after-load  "find-func"
  `(progn
     (emacspeak-fix-commands-loaded-from "find-func")))

;;; subr.el
(eval-after-load  "subr"
  `(progn
     (emacspeak-fix-commands-loaded-from "find-func")))

;;}}}
;;{{{ Setup package extensions

(emacspeak-do-package-setup "abc-mode" 'emacspeak-abc-mode)
(emacspeak-do-package-setup "add-log" 'emacspeak-add-log)
(emacspeak-do-package-setup "analog" 'emacspeak-analog)
(emacspeak-do-package-setup "ansi-color" 'emacspeak-ansi-color)
(emacspeak-do-package-setup "apt-sources" 'emacspeak-apt-sources)
(emacspeak-do-package-setup "arc-mode" 'emacspeak-arc)
(emacspeak-do-package-setup "bbdb" 'emacspeak-bbdb)
(emacspeak-do-package-setup "bibtex" 'emacspeak-bibtex)
(emacspeak-do-package-setup "bookmark" 'emacspeak-bookmark)
(emacspeak-do-package-setup "bmk-mgr" 'emacspeak-bmk-mgr)
(emacspeak-do-package-setup "browse-kill-ring" 'emacspeak-browse-kill-ring)
(emacspeak-do-package-setup "bs" 'emacspeak-bs)
(emacspeak-do-package-setup "buff-menu" 'emacspeak-buff-menu)
(emacspeak-do-package-setup "cc-mode" 'emacspeak-c)
(emacspeak-do-package-setup "calc" 'emacspeak-calc)
(emacspeak-do-package-setup "calculator" 'emacspeak-calculator)
(emacspeak-do-package-setup "calendar" 'emacspeak-calendar)
(emacspeak-do-package-setup "cc-mode" 'emacspeak-c)
(emacspeak-do-package-setup "semantic" 'emacspeak-cedet)
(emacspeak-do-package-setup "checkdoc" 'emacspeak-checkdoc)
(emacspeak-do-package-setup "chess" 'emacspeak-chess)
(emacspeak-do-package-setup "cider" 'emacspeak-cider)
(emacspeak-do-package-setup "clojure" 'emacspeak-clojure)
(emacspeak-do-package-setup "cmuscheme" 'emacspeak-cmuscheme)
(emacspeak-do-package-setup "ciel" 'emacspeak-ciel)
(emacspeak-do-package-setup "company" 'emacspeak-company)
(emacspeak-do-package-setup "compile" 'emacspeak-compile)
(emacspeak-do-package-setup "cperl-mode" 'emacspeak-cperl)
(emacspeak-do-package-setup "dumb-jump" 'emacspeak-dumb-jump)
(emacspeak-do-package-setup "ecb" 'emacspeak-ecb)
(emacspeak-do-package-setup "ein" 'emacspeak-ein)
(emacspeak-do-package-setup "ein-notebook" 'emacspeak-ein)
(emacspeak-do-package-setup "cus-edit" 'emacspeak-custom)
(emacspeak-do-package-setup "deadgrep" 'emacspeak-deadgrep)
(emacspeak-do-package-setup "debugger" 'emacspeak-debugger)
(emacspeak-do-package-setup "desktop" 'emacspeak-desktop)
(emacspeak-do-package-setup "diff-mode" 'emacspeak-diff-mode)
(emacspeak-do-package-setup "dired" 'emacspeak-dired)
(emacspeak-do-package-setup "dismal" 'emacspeak-dismal)
(emacspeak-do-package-setup "dictionary" 'emacspeak-dictionary)
(emacspeak-do-package-setup "doctor" 'emacspeak-entertain)
(emacspeak-do-package-setup "dunnet" 'emacspeak-entertain)
(emacspeak-do-package-setup "ediary" 'emacspeak-ediary)
(emacspeak-do-package-setup "ediff" 'emacspeak-ediff)
(emacspeak-do-package-setup "ediff-mult" 'emacspeak-ediff)
(emacspeak-do-package-setup "eglot" 'emacspeak-eglot)
(emacspeak-do-package-setup "elisp-refs" 'emacspeak-elisp-refs)
(emacspeak-do-package-setup "elscreen" 'emacspeak-elscreen)
(emacspeak-do-package-setup "xkcd" 'emacspeak-xkcd)
(emacspeak-do-package-setup "emms" 'emacspeak-emms)
(emacspeak-do-package-setup "epa" 'emacspeak-epa)
(emacspeak-do-package-setup "eperiodic" 'emacspeak-eperiodic)
(emacspeak-do-package-setup "erc" 'emacspeak-erc)
(emacspeak-do-package-setup "eshell" 'emacspeak-eshell)
(emacspeak-do-package-setup "ess" 'emacspeak-ess)
(emacspeak-do-package-setup "eclim" 'emacspeak-eclim)
(emacspeak-do-package-setup "evil" 'emacspeak-evil)
(emacspeak-do-package-setup "eww" 'emacspeak-eww)
(emacspeak-do-package-setup "elfeed" 'emacspeak-elfeed)
(emacspeak-do-package-setup "enriched" 'emacspeak-enriched)
(emacspeak-do-package-setup "facemenu" 'emacspeak-facemenu)
(emacspeak-do-package-setup "find-dired" 'emacspeak-find-dired)
(emacspeak-do-package-setup "find-things-fast" 'emacspeak-ftf)
(emacspeak-do-package-setup "find-func" 'emacspeak-find-func)
(emacspeak-do-package-setup "flycheck" 'emacspeak-flycheck)
(emacspeak-do-package-setup "flymake" 'emacspeak-flymake)
(emacspeak-do-package-setup "flyspell" 'emacspeak-flyspell)
(emacspeak-do-package-setup "folding" 'emacspeak-folding)
(emacspeak-do-package-setup "forge" 'emacspeak-forge)
(emacspeak-do-package-setup "forms" 'emacspeak-forms)
(emacspeak-do-package-setup "generic" 'emacspeak-generic)
(emacspeak-do-package-setup "geiser" 'emacspeak-geiser)
(emacspeak-do-package-setup "github-explorer" 'emacspeak-gh-explorer)
(emacspeak-do-package-setup "gtags" 'emacspeak-gtags)
(emacspeak-do-package-setup "gnus" 'emacspeak-gnus)
(emacspeak-do-package-setup "gnuplot" 'emacspeak-gnuplot)
(emacspeak-do-package-setup "gomoku" 'emacspeak-gomoku)
(emacspeak-do-package-setup "gud" 'emacspeak-gud)
(emacspeak-do-package-setup "gdb-ui" 'emacspeak-gud)
(emacspeak-do-package-setup "helm" 'emacspeak-helm)
(emacspeak-do-package-setup "go-mode" 'emacspeak-go-mode)
(emacspeak-do-package-setup "hangman" 'emacspeak-entertain)
(emacspeak-do-package-setup "hideshow" 'emacspeak-hideshow)
(emacspeak-do-package-setup "hydra" 'emacspeak-hydra)
(emacspeak-do-package-setup "hide-lines" 'emacspeak-hide-lines)
(emacspeak-do-package-setup "imenu" 'emacspeak-imenu)
(emacspeak-do-package-setup "ibuffer" 'emacspeak-ibuffer)
(emacspeak-do-package-setup "ido" 'emacspeak-ido)
(emacspeak-do-package-setup "iedit" 'emacspeak-iedit)
(emacspeak-do-package-setup "ivy" 'emacspeak-ivy)
(emacspeak-do-package-setup "info" 'emacspeak-info)
(emacspeak-do-package-setup "ispell" 'emacspeak-ispell)
(emacspeak-do-package-setup "jabber" 'emacspeak-jabber)
(emacspeak-do-package-setup "jdee" 'emacspeak-jdee)
(emacspeak-do-package-setup "js2" 'emacspeak-js2)
(emacspeak-do-package-setup "indium" 'emacspeak-indium)
(emacspeak-do-package-setup "js2-mode" 'emacspeak-js2)
(emacspeak-do-package-setup "jss" 'emacspeak-jss)
(emacspeak-do-package-setup "kite" 'emacspeak-kite)
(emacspeak-do-package-setup "kmacro" 'emacspeak-kmacro)
(emacspeak-do-package-setup "lispy" 'emacspeak-lispy)
(emacspeak-do-package-setup"lua-mdoe" 'emacspeak-lua)
(emacspeak-do-package-setup "magit" 'emacspeak-magit)
(emacspeak-do-package-setup "make-mode" 'emacspeak-make-mode)
(emacspeak-do-package-setup "markdown-mode" 'emacspeak-markdown)
(emacspeak-do-package-setup "man" 'emacspeak-man)
(emacspeak-do-package-setup "message" 'emacspeak-message)
(emacspeak-do-package-setup "meta-mode" 'emacspeak-metapost)
(emacspeak-do-package-setup "muse-mode" 'emacspeak-muse)
(emacspeak-do-package-setup "midge-mode" 'emacspeak-midge)
(emacspeak-do-package-setup "mines" 'emacspeak-mines)
(emacspeak-do-package-setup "mpuz" 'emacspeak-entertain)
(emacspeak-do-package-setup "mspools" 'emacspeak-mspools)
(emacspeak-do-package-setup "nero" 'emacspeak-nero)
(emacspeak-do-package-setup "navi-mode" 'emacspeak-navi-mode)
(emacspeak-do-package-setup "net-utils" 'emacspeak-net-utils)
(emacspeak-do-package-setup "newsticker" 'emacspeak-newsticker)
(emacspeak-do-package-setup "nov" 'emacspeak-nov)
(emacspeak-do-package-setup "nxml-mode" 'emacspeak-nxml)
(emacspeak-do-package-setup "org" 'emacspeak-org)
(emacspeak-do-package-setup "orgalistorg" 'emacspeak-orgalist)
(emacspeak-do-package-setup "origami" 'emacspeak-origami)
(emacspeak-do-package-setup "outline" 'emacspeak-outline)
(emacspeak-do-package-setup "perl-mode" 'emacspeak-perl)
(emacspeak-do-package-setup "pianobar" 'emacspeak-pianobar)
(emacspeak-do-package-setup "proced" 'emacspeak-proced)
(emacspeak-do-package-setup "popup" 'emacspeak-popup)
(emacspeak-do-package-setup "project" 'emacspeak-project)
(emacspeak-do-package-setup "projectile" 'emacspeak-projectile)
(emacspeak-do-package-setup "php-mode" 'emacspeak-php-mode)
(emacspeak-do-package-setup "package"'emacspeak-package)
(emacspeak-do-package-setup "paradox"'emacspeak-paradox)
(emacspeak-do-package-setup "pcvs" 'emacspeak-pcl-cvs)
(emacspeak-do-package-setup "elpy" 'emacspeak-elpy)
(emacspeak-do-package-setup "elpy" 'emacspeak-elpy)
(emacspeak-do-package-setup "pydoc" 'emacspeak-pydoc)
(emacspeak-do-package-setup "python" 'emacspeak-python)
(emacspeak-do-package-setup "python-mode" 'emacspeak-py)
(emacspeak-do-package-setup "racket-mode" 'emacspeak-racket)
(emacspeak-do-package-setup "re-builder" 'emacspeak-re-builder)
(emacspeak-do-package-setup "rg" 'emacspeak-rg)
(emacspeak-do-package-setup "racer" 'emacspeak-racer)
(emacspeak-do-package-setup "rust-mode" 'emacspeak-rust-mode)
(emacspeak-do-package-setup "reftex" 'emacspeak-reftex)
(emacspeak-do-package-setup "rst" 'emacspeak-rst)
(emacspeak-do-package-setup "related" 'emacspeak-related)
(emacspeak-do-package-setup "rmail" 'emacspeak-rmail)
(emacspeak-do-package-setup "rpm-spec-mode" 'emacspeak-rpm-spec)
(emacspeak-do-package-setup "ruby-mode" 'emacspeak-ruby)
(emacspeak-do-package-setup "sgml-mode" 'emacspeak-sgml-mode)
(emacspeak-do-package-setup "sh-script" 'emacspeak-sh-script)
(emacspeak-do-package-setup "shx" 'emacspeak-shx)
(emacspeak-do-package-setup "sage-shell-mode" 'emacspeak-sage)
(emacspeak-do-package-setup "slime" 'emacspeak-slime)
(emacspeak-do-package-setup "sigbegone" 'emacspeak-sigbegone)
(emacspeak-do-package-setup "smartparens" 'emacspeak-smartparens)
(emacspeak-do-package-setup "smart-window" 'emacspeak-smart-window)
(emacspeak-do-package-setup "solitaire" 'emacspeak-solitaire)
(emacspeak-do-package-setup "speedbar" 'emacspeak-speedbar)

(emacspeak-do-package-setup "syslog" 'emacspeak-syslog)
(emacspeak-do-package-setup "ses" 'emacspeak-ses)
(emacspeak-do-package-setup "sql-mode" 'emacspeak-sql)
(emacspeak-do-package-setup "sql" 'emacspeak-sql)
(emacspeak-do-package-setup "sdcv" 'emacspeak-sdcv)
(emacspeak-do-package-setup "supercite" 'emacspeak-supercite)
(emacspeak-do-package-setup "sudoku" 'emacspeak-sudoku)
(emacspeak-do-package-setup "table" 'emacspeak-etable)
(emacspeak-do-package-setup "tab-bar" 'emacspeak-tab-bar)
(emacspeak-do-package-setup "tar-mode" 'emacspeak-tar)
(emacspeak-do-package-setup "tcl" 'emacspeak-tcl)
(emacspeak-do-package-setup "tdtd" 'emacspeak-tdtd)
(emacspeak-do-package-setup "xref" 'emacspeak-xref)
(emacspeak-do-package-setup "tempo" 'emacspeak-tempo)
(emacspeak-do-package-setup "term" 'emacspeak-eterm)
(emacspeak-do-package-setup "eudc" 'emacspeak-eudc)
(emacspeak-do-package-setup "tetris" 'emacspeak-tetris)
(emacspeak-do-package-setup "threes" 'emacspeak-threes)
(emacspeak-do-package-setup "tide" 'emacspeak-tide)
(emacspeak-do-package-setup "2048-game" 'emacspeak-2048)
(emacspeak-do-package-setup "tex-site" 'emacspeak-auctex)
(emacspeak-do-package-setup "texinfo" 'emacspeak-texinfo)
(emacspeak-do-package-setup "transient" 'emacspeak-transient)
(emacspeak-do-package-setup "tmm" 'emacspeak-facemenu)
(emacspeak-do-package-setup "todo-mode" 'emacspeak-todo-mode)
(emacspeak-do-package-setup "twittering-mode" 'emacspeak-twittering)
(emacspeak-do-package-setup "typo" 'emacspeak-typo)
(emacspeak-do-package-setup "vdiff" 'emacspeak-vdiff)
(emacspeak-do-package-setup "view" 'emacspeak-view)
(emacspeak-do-package-setup "vuiet" 'emacspeak-vuiet)
(emacspeak-do-package-setup "vm" 'emacspeak-vm)
(emacspeak-do-package-setup "wdired" 'emacspeak-wdired)
(emacspeak-do-package-setup "cus-edit" 'emacspeak-custom)
(emacspeak-do-package-setup "wid-edit" 'emacspeak-widget)
(emacspeak-do-package-setup "widget" 'emacspeak-widget)
(emacspeak-do-package-setup "windmove" 'emacspeak-windmove)
(emacspeak-do-package-setup "winring" 'emacspeak-winring)
(emacspeak-do-package-setup "woman" 'emacspeak-woman)
(emacspeak-do-package-setup "yasnippet" 'emacspeak-yasnippet)
(emacspeak-do-package-setup "yaml-mode" 'emacspeak-yaml)
(emacspeak-do-package-setup "ytel" 'emacspeak-ytel)
;;}}}
;;{{{  Submit bugs

(defconst emacspeak-bug-address
  "emacspeak@cs.vassar.edu"
  "Address for bug reports and questions.")

(defun emacspeak-submit-bug ()
  "Function to submit a bug to the programs maintainer."
  (interactive)
  (require 'reporter)
  (when
      (yes-or-no-p "Are you sure you want to submit a bug report? ")
    (let (
          (vars '(
                  emacs-version
                  system-type
                  emacspeak-version  dtk-program
                  dtk-speech-rate dtk-character-scale
                  dtk-split-caps dtk-capitalize
                  dtk-punctuation-mode
                  emacspeak-line-echo  emacspeak-word-echo
                  emacspeak-character-echo
                  emacspeak-use-auditory-icons
                  emacspeak-audio-indentation)))
      (mapcar
       #'(lambda (x)
           (if (not (and (boundp x) (symbol-value x)))
               (setq vars (delq x vars))))vars)
      (reporter-submit-bug-report
       emacspeak-bug-address
       (concat "Emacspeak Version: " emacspeak-version)
       vars
       nil nil
       "Description of Problem:"))))

;;}}}
;;{{{ exporting emacspeak environment to subprocesses

(defun emacspeak-export-environment ()
  "Export shell environment.
This exports emacspeak's system variables to the environment
so it can be passed to subprocesses."
  (cl-declare (special emacspeak-directory emacspeak-play-program
                       emacspeak-sounds-directory))
  (setenv "EMACSPEAK_DIR" emacspeak-directory)
  (setenv "EMACSPEAK_SOUNDS_DIR" emacspeak-sounds-directory)
  (setenv "EMACSPEAK_PLAY_PROGRAM" emacspeak-play-program))

;;}}}
;;{{{ setup programming modes

;;; turn on automatic voice locking , split caps and punctuations in
;;; programming  modes

;;;###autoload
(defun emacspeak-setup-programming-mode ()
  "Setup programming mode.
Turns on audio indentation and sets
punctuation mode to all, activates the dictionary and turns on split
caps."
  (cl-declare (special dtk-split-caps emacspeak-audio-indentation))
  (ems-with-messages-silenced
   (dtk-set-punctuations 'all)
   (or dtk-split-caps (dtk-toggle-split-caps))
   (emacspeak-pronounce-refresh-pronunciations)
   (or emacspeak-audio-indentation (emacspeak-toggle-audio-indentation))))

(defun emacspeak-setup-programming-modes ()
  "Setup programming modes."
  (add-hook 'prog-mode-hook #'emacspeak-setup-programming-mode)
  (mapc
   #'(lambda (hook)
       (add-hook hook #'emacspeak-setup-programming-mode))
   '(
     conf-unix-mode-hook html-helper-mode-hook
     markdown-mode-hook muse-mode-hook
     sgml-mode-hook xml-mode-hook nxml-mode-hook xsl-mode-hook
     TeX-mode-hook LaTeX-mode-hook bibtex-mode-hook)))

;;}}}
;;{{{ set up after-init-hook to fix interactive functions

(add-hook 'after-init-hook 'emacspeak-fix-commands-that-use-interactive)
(run-at-time 10 nil #'emacspeak-fix-commands-that-use-interactive)
;;}}}
;;{{{ set up after-init-hook to fix interactive functions

(add-hook 'after-init-hook 'emacspeak-fix-commands-that-use-interactive)
                                        ;(add-hook 'after-init-hook 'emacspeak-keymap-refresh)

;;}}}
;;{{{ Emacspeak:

(defcustom emacspeak-play-emacspeak-startup-icon
  t
  "If set to T, emacspeak plays its icon as it launches."
  :type 'boolean
  :group 'emacspeak)

(defsubst emacspeak-play-startup-icon ()
  "Play startup icon if requested."
  (cl-declare (special emacspeak-play-emacspeak-startup-icon))
  (let ((player   (executable-find "mplayer")))
    (when (and  emacspeak-play-emacspeak-startup-icon player)
      (start-process
       "mp3" nil
       player
       (expand-file-name "emacspeak.mp3"
                         emacspeak-sounds-directory)))))
(defvar emacspeak-startup-message
  (format
   "  Press %s to get an   overview of emacspeak  %s \
 I am  completely operational,  and all my circuits are functioning perfectly!"
   (substitute-command-keys
    "\\[emacspeak-describe-emacspeak]")
   emacspeak-version)
  "Emacspeak startup message.")


;;;###autoload
(defun emacspeak()
  "Start the Emacspeak Audio Desktop.
Use Emacs as you normally would,
emacspeak will provide you spoken feedback as you work.  Emacspeak also
provides commands for having parts of the current buffer, the
mode-line etc to be spoken.

If you are hearing this description as a result of pressing
\\[emacspeak-describe-emacspeak] you may want to press \\[dtk-stop] to
stop speech, and then use the arrow keys to move around in the Help
buffer to read the rest of this description, which includes a summary
of all emacspeak keybindings.

All emacspeak commands use \\[emacspeak-prefix-command] as a prefix
key.  You can also set the state of the TTS engine by using
\\[emacspeak-dtk-submap-command] as a prefix.  Here is a summary of all
emacspeak commands along with their bindings.  You need to precede the
keystrokes listed below with \\[emacspeak-prefix-command].

Emacspeak also provides a fluent speech extension to the Emacs
terminal emulator (eterm).  Note: You need to use the term package that
comes with emacs-19.29 and later.

\\{emacspeak-keymap}

Emacspeak provides a set of additional keymaps to give easy access to
its extensive facilities.

Press C-; to access keybindings in emacspeak-hyper-keymap:
\\{emacspeak-hyper-keymap}.

Press C-' or C-.  to access keybindings in emacspeak-super-keymap:
\\{emacspeak-super-keymap}.

Press C-, to access keybindings in emacspeak-alt-keymap:
\\{emacspeak-alt-keymap}.

See the online documentation \\[emacspeak-open-info] for individual
commands and options for details."
  (interactive)
  (cl-declare (special
               ad-redefinition-action
               emacspeak-pronounce-load-pronunciations-on-startup line-move-visual
               emacspeak-info-directory
               use-dialog-box emacspeak-pronounce-dictionaries-file
               emacspeak-play-program emacspeak-sounds-directory))
  (let ((ad-redefinition-action 'accept)
        (file-name-handler-alist nil)
        (load-source-file-function  nil))
    (emacspeak-export-environment)
    (setq-default line-move-visual nil)
    (setq use-dialog-box nil)
    (when (boundp 'Info-directory-list)
      (push emacspeak-info-directory Info-directory-list))
    (require 'emacspeak-personality)
    (dtk-initialize)
    (tts-configure-synthesis-setup)
    (require 'emacspeak-keymap)
    (require 'emacspeak-redefine)
    (require 'emacspeak-advice)
    (emacspeak-sounds-define-theme-if-necessary emacspeak-sounds-default-theme)
    (when emacspeak-pronounce-load-pronunciations-on-startup
      (emacspeak-pronounce-load-dictionaries
       emacspeak-pronounce-dictionaries-file)
      (add-hook  'messages-buffer-mode-hook #'emacspeak-pronounce-refresh-pronunciations))
    (emacspeak-setup-programming-modes)
    (emacspeak-use-customized-blink-paren)
    (emacspeak-fix-commands-that-use-interactive)
    ;(require 'emacspeak-m-player)
    (run-hooks 'emacspeak-startup-hook)
    (tts-with-punctuations
        'some
      (dtk-speak-and-echo emacspeak-startup-message))
    (emacspeak-play-startup-icon)))

(defun emacspeak-info ()
  "Open Emacspeak Info Manual."
  (interactive)
  (funcall-interactively
   #'info
   (expand-file-name "info/emacspeak.info" emacspeak-directory)
   "*Emacspeak Info*"))

(defun emacspeak-describe-emacspeak ()
  "Give a brief overview of emacspeak."
  (interactive)
  (describe-function 'emacspeak)
  (switch-to-buffer "*Help*")
  (dtk-set-punctuations 'all)
  (emacspeak-speak-buffer))

;;}}}
;;{{{Advice find-func:
(eval-after-load
    "find-func"
  `(progn
     (emacspeak-fix-commands-loaded-from "find-func")))
;;}}}
(provide 'emacspeak)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}

;;; emacspeak.el ends here
