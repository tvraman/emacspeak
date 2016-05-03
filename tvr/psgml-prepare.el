;;;$Id$
;;{{{ autload and load path

(augment-load-path "psgml" "psgml")
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t )
(setq sgml-system-path
      (list "."
            (expand-file-name "~/text/xml/DTD")))
(setq-default sgml-namecase-general nil
              sgml-indent-data t)

              
(setq-default sgml-validate-command
              "xmllint --valid --noout %s ")

(setq sgml-catalog-files
      (append
       sgml-catalog-files
       '("CATALOG"
         "/home/raman/text/xml/CATALOG")))

;;}}}
;;{{{ functions 
(defvar psgml-xmllint-program "xmllint"
  "XML Lint program.")

(defun psgml-xmllint-reindent-buffer ()
  "Use XML Lint to  reformat the buffer."
  (interactive)
  (declare (special psgml-xmllint-program))
  (save-restriction
    (widen)
    (let ((orig (point)))
      (shell-command-on-region (point-min) (point-max)
                               (format "%s  --format   - "
                                       psgml-xmllint-program)
                               (current-buffer)
                               'replace
                               "*xml errors*")
      (goto-char orig)
      (emacspeak-auditory-icon 'task-done))))

(defcustom psgml-xmlindent-program "xmlindent"
  "XML indent tool."
  :type 'string
  :group 'psgml)

(defcustom psgml-xmlindent-spaces 2
  "Number of spaces to use for each indent level."
  :type 'number
  :group 'psgml)

(defun psgml-xmlindent-visited-file ()
  "Reindents visited file and updates buffer."
  (interactive)
  (let ((this-file (buffer-file-name)))
    (unless this-file
      (error "Not visiting a file."))
    (save-buffer)
    (shell-command
     (format "%s -w -i %s %s"
             psgml-xmlindent-program
             psgml-xmlindent-spaces
             this-file))
    (revert-buffer nil 'quietly)
    (xml-mode)))

(defun psgml-reindent-region (start end)
  "Reindent current region."
  (interactive "r")
  (save-excursion
    (indent-region  start end nil)
    (emacspeak-auditory-icon 'fill-object)))

(require 'psgml)

(defun augment-sgml-custom-dtd (doctype public system
                                        &optional no-xml-declaration-p)
  "Augment sgml-custom-dtd setting."
  (declare (special sgml-custom-dtd))
  (setq sgml-custom-dtd
        (cons 
         (list doctype
               (format 
                "%s\n<!DOCTYPE %s PUBLIC \"%s\" \"%s\">"
                (if no-xml-declaration-p 
                    ""
                  "<?xml version=\"1.0\"?>")
                doctype public system))
         sgml-custom-dtd)))

;;}}}
;;{{{ hooks 

(add-hook 'sgml-mode-hook
          (function
           (lambda nil
             (setq sgml-insert-missing-element-comment nil)
             (define-key sgml-mode-map "\C-x\C-i"
               'psgml-xmlindent-visited-file)
             (define-key sgml-mode-map "\C-xl"
               'psgml-xmllint-reindent-buffer)
             (define-key sgml-mode-map "\M-\C-q"
               'psgml-reindent-region))))

;;}}}
;;{{{ auto mode alist 

(defvar xml-mode-extensions nil
  "Extensions for which we use xml mode.")
(setq xml-mode-extensions
      (list "\\.glade$" 
            "\\.wml$"  
            "\\.html$"
            "\\.xml$"  
            "\\.jsp$"
            "\\.tld$"
            "\\.xsd$"  
            "\\.vxml$"  
            "\\.xul$"
            "\\.xsl$"  
            "\\.jsp$"
            "\\.rdf$"  ))
(mapcar
 (function
  (lambda (ext)
    (augment-auto-mode-alist ext 'xml-mode)))
 xml-mode-extensions)

;;}}}
;;{{{ set up folding 
(fold-add-to-marks-list 'xae-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
(fold-add-to-marks-list 'xml-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
(fold-add-to-marks-list 'sgml-mode
                        "<!-- {" 
                        "<!-- } -->" " --> ")
;;}}}
;;{{{  set up custom DTDs
(augment-sgml-custom-dtd 
 "project"
 "-//apache//DTD ant //EN"
 "DTD/ant.dtd")

(augment-sgml-custom-dtd 
 "mathml"
 "-//W3C//DTD mathml //EN"  
 "DTD/DTD/mathml2.dtd")
(augment-sgml-custom-dtd 
 "xsl:stylesheet"
 "-//W3C//DTD xslt //EN"  
 "DTD/xslt-1.0.dtd")

(augment-sgml-custom-dtd 
 "html"
 "-//W3C//DTD XHTML 1.0 Strict//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
 )

(augment-sgml-custom-dtd
 "spec"
 "-//W3C//DTD Specification V2.1//EN"
 "http://www.w3.org/XML/1998/06/xmlspec-v21.dtd")
 
 

(augment-sgml-custom-dtd"schema"
                        "-//W3C//DTD XML Schema 1.0 //EN"
                        "DTD/XMLSchema.dtd")

(augment-sgml-custom-dtd
 "wnl"
 "-//WAPFORUM//DTD WML 1.1//EN"
 "wml-1.1.dtd")
(augment-sgml-custom-dtd
 "vxml"
 "-//VoiceXMLForum//DTD voicexml 1.0//EN"
 "voicexml-1.0.dtd")
(augment-sgml-custom-dtd
 "voicexml"
 "-//W3C//DTD XHTML+VoiceXML 1.0 Strict//EN"
 "xhtml+voicexml10/xhtml+voicexml10.dtd"
 )

(setq sgml-set-face nil)
(setq sgml-use-text-properties nil)
;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
