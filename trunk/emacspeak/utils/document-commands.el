;;;$Id$
;;; Description: Generate commands.texi
;;; Load all emacspeak modules and write commands.texi 

;;; Code:
(require 'cl)
(defvar emacs-personal-library-directory
  (expand-file-name "~/emacs/lisp/site-lisp")
  "Directory where we keep personal libraries")

(defsubst augment-load-path (path &optional library whence at-end)
  "add directory to load path"
  (interactive "Denter directory name: ")
  (declare (special emacs-personal-library-directory))
  (cond
   ((and library
         (locate-library library))
                                        ;do nothing
    )
   (t (setq path (expand-file-name path
                                   (or whence
                                       (and (boundp
                                             'emacs-personal-library-directory)
                                            emacs-personal-library-directory))))
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


(defvar emacspeak-modules-dependency-alist
  '(
    ("emacspeak-load-path.el"  . nil )
    ("emacspeak-setup.el"  . nil )
    ("emacspeak-sounds"  . nil )
    ("emacspeak-speak"  . nil )
    ("emacspeak"  . nil )
    ("cd-tool"   . nil)
    ("dtk-css-speech" . nil)
    ("dtk-interp" . nil)
    ("dtk-speak" . nil)
    ("dtk-tcl" . nil)
    ("dtk-voices" . nil)
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
    ("emacspeak-buff-sel". ("buff-sel"))
    ("emacspeak-c". nil )
    ("emacspeak-calc" . nil )
    ("emacspeak-calculator" . nil )
    ("emacspeak-calendar" . ("calendar"))
    ("emacspeak-checkdoc" . nil )
    ("emacspeak-cmuscheme" . nil )
    ("emacspeak-compile" . nil )
    ("emacspeak-cperl"  . nil )
    ("emacspeak-daisy" . nil)
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
    ("emacspeak-freeamp"  . nil )
    ("emacspeak-generic"  . nil )
    ("emacspeak-gnuplot"  . nil )
    ("emacspeak-gnus"  . nil )
    ("emacspeak-gomoku"  . nil )
    ("emacspeak-gridtext"  . nil )
    ("emacspeak-gud"  . nil )
    ("emacspeak-hide"  . nil )
    ("emacspeak-hideshow"  . nil )
    ("emacspeak-hyperbole"  . ("hyperbole-prepare"))
    ("emacspeak-ibuffer"  . nil )
    ("emacspeak-imcom"  . nil )
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
    ("emacspeak-mpg123"  . ("mpg123"))
    ("emacspeak-mspools"  . ("mspools") )
    ("emacspeak-net-utils"  . nil )
    ("emacspeak-ocr"  . nil )
    ("emacspeak-oo-browser"  . nil )
    ("emacspeak-outline"  . nil )
    ("emacspeak-pcl-cvs"  . nil )
    ("emacspeak-perl"  . nil )
    ("emacspeak-pronounce"  . nil )
    ("emacspeak-psgml"  . ("psgml-prepare"))
    ("emacspeak-python"  . ("python-mode"))
    ("emacspeak-realaudio"  . nil )
    ("emacspeak-redefine"  . nil )
    ("emacspeak-reftex"  . nil )
    ("emacspeak-remote"  . nil )
    ("emacspeak-replace"  . nil )
    ("emacspeak-rmail"  . nil )
    ("emacspeak-rpm"  . nil )
    ("emacspeak-sawfish"  . nil )
    ("emacspeak-sgml-mode"  . nil )
    ("emacspeak-sh-script"  . nil )
    ("emacspeak-solitaire"  . nil )
    ("emacspeak-speedbar"  . ("speedbar-prepare"))
    ("emacspeak-sql"  . nil )
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
    ("emacspeak-texinfo"  . ("texinfo"))
    ("emacspeak-tnt"  . ("tnt-prepare"))
    ("emacspeak-url-template"  . nil )
    ("emacspeak-view-process"  . ("view-ps-prepare" "view-process-mode"))
    ("emacspeak-view"  . nil )
    ("emacspeak-vm"  . ("vm-prepare"))
    ("emacspeak-w3"  . ("w3-prepare"))
    ("emacspeak-websearch"  . nil )
    ("emacspeak-widget"  . nil )
    ("emacspeak-windmove"  . nil )
    ("emacspeak-winring"  . nil )
    ("emacspeak-wizards"  . nil )
    ;("emacspeak-wrolo"  . ("wrolo"))
    ("emacspeak-xml-shell"  . nil )
    ("emacspeak-xslt-process"  . nil )
    ("fast-voice-lock"  . nil )
    ("jit-voice-lock"  . nil )
    ("lazy-voice-lock"  . nil )
    ("outloud-css-speech"  . nil )
    ("outloud-voices"  . nil )
    ("regexp-opt"  . nil )
    ("tapestry"  . nil )
    ("voice-lock"  . nil )
    ("voice-settings"  . nil )
    ("voice-setup"  . nil )
    )
  "Association list that specifies dependency relations.
This helps pull in all emacspeak modules cleanly.")

(declaim (special load-path))
(augment-load-path (expand-file-name "~/emacs/lisp/emacspeak/lisp"))
(augment-load-path emacs-personal-library-directory)

(defun emacspeak-utils-generate-commands-documentation ()
  "Generate commands.texi and DOC ."
  (declare (special emacspeak-modules-dependency-alist))
  (let ((emacspeak-speak-messages nil))
  (mapcar
   #'(lambda (pair)
       (mapcar #'load-library (cdr pair))
(load-library (car pair))
(message "%s\n" (car pair)))
emacspeak-modules-dependency-alist)
(emacspeak-generate-texinfo-command-documentation
 "commands.texi")
(emacspeak-generate-documentation
 "../etc/DOC")))

(emacspeak-utils-generate-commands-documentation)
