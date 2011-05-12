;;;$Id$
;;; Description: Generate commands.texi
;;; Load all emacspeak modules and write commands.texi

;;; Code:
(require 'cl)
(defvar emacs-site-lisp-directory
  (expand-file-name "../../site-lisp"
                                     (file-name-directory load-file-name))
  "Directory where we keep personal libraries")

(defsubst augment-load-path (path &optional library whence at-end)
  "add directory to load path"
  (interactive "Denter directory name: ")
  (declare (special emacs-site-lisp-directory))
  (cond
   ((and library
	 (locate-library library))
					;do nothing
    )
   (t (setq path (expand-file-name path
				   (or whence
				       (and (boundp
					     'emacs-site-lisp-directory)
					    emacs-site-lisp-directory))))
      (if at-end
	  (setq load-path (append (delete path load-path) (list path))
		(debug-on-error t))
	(setq load-path (cons path (delete path load-path)))))))

(defsubst augment-auto-mode-alist (ext mode)
  "Add to auto-mode-alist."
  (declare (special auto-mode-alist))
  (setq auto-mode-alist
	(cons
	 (cons ext mode)
	 auto-mode-alist)))

(defsubst load-library-if-available (lib)
  "Load a library only if it is around"
  (condition-case nil
      (cond
       ((locate-library lib)
	(load-library lib)
	(message "Loaded %s" lib)
	t)
       (t (message "Could not locate library %s" lib)
	  nil))
    (error (message
	    "Error loading %s"
	    lib))))
(require 'parse-time)
(defvar emacspeak-modules-dependency-alist
  '(
    ("emacspeak-load-path.el"  . nil )
    ("emacspeak-setup.el"  . nil )
    ("emacspeak-sounds"  . nil )
    ("emacspeak-speak"  . nil )
    ("emacspeak"  . nil )
    ("cd-tool"   . nil)
    ("amixer" . nil)
    ("dtk-interp" . nil)
    ("dtk-speak" . nil)
    ("voice-setup" . nil)
    ("dectalk-voices" . nil)
    ("emacspeak-actions". nil )
    ("emacspeak-advice" . nil)
    ("emacspeak-ansi-color" . nil)
    ("emacspeak-arc" . ("arc-mode"))
    ("emacspeak-auctex" . ("auctex-prepare"))
    ("emacspeak-aumix" . nil)
    ("emacspeak-babel" . ("babel"))
    ("emacspeak-bbdb". ("bbdb-prepare"))
    ("emacspeak-bibtex" . ("bibtex"))
    ("emacspeak-bookmark" . ("bookmark"))
    ("emacspeak-browse-kill-ring" . ("browse-kill-ring"))
    ("emacspeak-bs" . nil)
    ("emacspeak-buff-menu" . nil)
    ("emacspeak-c". nil )
    ("emacspeak-calc" . nil )
    ("emacspeak-calculator" . nil )
    ("emacspeak-calendar" . ("calendar"))
    ("emacspeak-checkdoc" . nil )
    ("emacspeak-cmuscheme" . nil )
    ("emacspeak-compile" . nil )
    ("emacspeak-cperl"  . nil )
    ("emacspeak-daisy" . nil)
    ("emacspeak-bookshare" . nil)
    ("emacspeak-npr" . nil)
    ("emacspeak-custom"  . ("custom" "cus-edit"))
    ("emacspeak-dictation"  . nil )
    ("emacspeak-dictionary"  . nil )
    ("emacspeak-dired"  . ("dired"))
    ("emacspeak-dismal"  . ("dismal-prepare" "dismal"))
    ("emacspeak-dmacro"  . nil )
    ("emacspeak-ecb"  . nil )
    ("emacspeak-ediff"  . nil )
    ("emacspeak-enriched"  . nil )
    ("emacspeak-entertain"  . nil )
    ("emacspeak-erc"  . ("erc-prepare"))
    ("emacspeak-emms" . ("emms-prepare"))
    ("emacspeak-zinf" . nil)
    ("emacspeak-eshell"  . ("eshell-prepare") )
    ("emacspeak-eterm"  . nil )
    ("emacspeak-eudc"  . ("eudc-prepare"))
    ("emacspeak-facemenu"  . nil )
    ("emacspeak-filtertext"  . nil )
    ("emacspeak-find-dired"  . nil )
    ("emacspeak-find-func"  . nil )
    ("emacspeak-finder-inf"  . ("finder"))
    ("emacspeak-finder"  . nil )
    ("emacspeak-fix-interactive"  . nil )
    ("emacspeak-flyspell"  . ("flyspell"))
    ("emacspeak-folding"  . ("folding-prepare"))
    ("emacspeak-forms"  . nil )
    ("emacspeak-generic"  . nil )
    ("emacspeak-gnuplot"  . nil )
    ("emacspeak-gnus"  . nil )
    ("emacspeak-gomoku"  . nil )
    ("emacspeak-gridtext"  . nil )
    ("emacspeak-gud"  . nil )
    ("emacspeak-hide"  . nil )
    ("emacspeak-hideshow"  . nil )
                                        ;("emacspeak-hyperbole"  . ("hyperbole-prepare"))
    ("emacspeak-ibuffer"  . nil )
    ("emacspeak-iswitchb" . nil)
    ("emacspeak-ido" . nil)
    ("emacspeak-imenu"  . nil )
    ("emacspeak-info"  . ("info"))
    ("emacspeak-ispell"  . ("ispell"))
    ("emacspeak-jde"  . ("jde-prepare"))
    ("emacspeak-keymap"  . nil )
    ("emacspeak-kotl"  . nil )
    ("emacspeak-make-mode"  . nil )
    ("emacspeak-man"  . ("man") )
    ("emacspeak-message"  . nil )
    ("emacspeak-metapost"  . nil )
    ("emacspeak-midge"  . nil )
    ("emacspeak-muse" . nil)
    ("emacspeak-mpg123"  . ("mpg123"))
    ("emacspeak-madplay" . nil)
    ("emacspeak-alsaplayer" . nil)
    ("emacspeak-mspools"  . ("mspools") )
    ("emacspeak-net-utils"  . nil )
    ("emacspeak-nxml-mode" . ("nxml-prepare"))
    ("emacspeak-ocr"  . nil )
    ("emacspeak-oo-browser"  . nil )
    ("emacspeak-outline"  . nil )
    ("emacspeak-pcl-cvs"  . nil )
    ("emacspeak-perl"  . nil )
    ("emacspeak-pronounce"  . nil )
    ("emacspeak-psgml"  . ("psgml-prepare"))
    ("emacspeak-python"  . ("python-mode"))
    ("emacspeak-realaudio"  . nil )
    ("emacspeak-re-builder"  . nil )
    ("emacspeak-redefine"  . nil )
    ("emacspeak-reftex"  . nil )
    ("emacspeak-remote"  . nil )
    ("emacspeak-replace"  . nil )
    ("emacspeak-rmail"  . nil )
    ("emacspeak-rpm"  . nil )
    ("emacspeak-sgml-mode"  . nil )
    ("emacspeak-sh-script"  . nil )
    ("emacspeak-sigbegone"  . nil )
    ("emacspeak-solitaire"  . nil )
    ("emacspeak-sql"  . nil )
    ("emacspeak-speedbar"  . ("speedbar-prepare"))
    ("emacspeak-ruby" . nil)
    ("emacspeak-supercite"  . ("supercite"))
    ("emacspeak-table-ui"  . nil )
    ("emacspeak-table"  . nil )
    ("emacspeak-tabulate"  . nil )
    ("emacspeak-tapestry"  . nil )
    ("emacspeak-tar"  .  ("tar-mode"))
    ("emacspeak-tcl"  . nil )
    ("emacspeak-tdtd"  . nil )
    ("emacspeak-tempo"  . nil )
    ("emacspeak-tetris"  . ("tetris"))
    ("emacspeak-sudoku"  . ("sudoku"))
    ("emacspeak-texinfo"  . ("texinfo"))
    ("emacspeak-tnt"  . ("tnt-prepare"))
    ("emacspeak-jabber"  . nil)
    ("emacspeak-url-template"  . nil )
    ("emacspeak-view"  . nil )
    ("emacspeak-vm"  . ("vm-prepare"))
    ("emacspeak-w3"  . ("w3-prepare"))
    ("emacspeak-websearch"  . nil )
    ("emacspeak-ebook"  . nil )
    ("emacspeak-google"  . nil )
    ("emacspeak-webspace"  . nil )
    ("emacspeak-widget"  . nil )
    ("emacspeak-windmove"  . nil )
    ("emacspeak-winring"  . nil )
    ("emacspeak-wizards"  . nil )
					;("emacspeak-wrolo"  . ("wrolo"))
    ("emacspeak-xml-shell"  . nil )
    ("emacspeak-xslt-process"  . nil )
    ("outloud-voices"  . nil )
    ("regexp-opt"  . nil )
    ("tapestry"  . nil )
    ("emacspeak-etable" . "table")
    ("voice-setup"  . nil )
    ("emacspeak-add-log" . nil)
("emacspeak-analog" . (list "analog-prepare" "analog"))
("emacspeak-desktop" . nil)
("emacspeak-ediary" . nil)
("emacspeak-eperiodic" . ("eperiodic"))
("emacspeak-m-player" . nil)
("emacspeak-personality" . nil)
("emacspeak-rpm-spec" . nil)
("emacspeak-swbuff" . nil)
("emacspeak-todo-mode" . nil)
("emacspeak-w3m" . ("w3m-prepare"))
("emacspeak-wdired"  . nil)
("emacspeak-xslt" .  nil)
    )
  "Association list that specifies dependency relations.
This helps pull in all emacspeak modules cleanly.")

(declaim (special load-path))
(augment-load-path (expand-file-name "../lisp"
                                     (file-name-directory load-file-name)))
(augment-load-path emacs-site-lisp-directory)
(defvar emacspeak-speak-messages nil)
(defun emacspeak-utils-generate-commands-documentation ()
  "Generate commands.texi and DOC ."
  (declare (special emacspeak-modules-dependency-alist
                    byte-compile-disable-print-circle))
  (setq byte-compile-disable-print-circle t)
  (let ((emacspeak-speak-messages nil)
        (debug-on-error t)
        (dtk-quiet t))
    (setq default-enable-multibyte-characters nil)
    (mapcar
     #'(lambda (pair)
         (condition-case nil
             (progn
               (mapcar #'load-library (cdr pair))
               (load-library (car pair)))
           (error nil))
	 (message "%s\n" (car pair)))
     emacspeak-modules-dependency-alist)
    (emacspeak-generate-texinfo-command-documentation
     "commands.texi")
    (emacspeak-generate-documentation
     "../etc/DOC")
    (load-library  "emacspeak-cus-load")
    (emacspeak-generate-texinfo-option-documentation
     "options.texi")))

(emacspeak-utils-generate-commands-documentation)
(message "Documented %d commands and %d options"
         (length (emacspeak-list-emacspeak-commands))
         (length (emacspeak-list-emacspeak-options)))
