;;;$Id$
;;; Setup Emacs JDE
;;{{{ locations

(augment-load-path "elib" "stack-f")

(setq my-use-cedet nil)
(cond
 (my-use-cedet
  (augment-load-path "eieio" "eieio")
  (load-library "semantic-prepare")
  (load-library "speedbar-prepare")
  (load-library "overlay-fix")
  )
 (t (load-library "cedet-prepare")))

(augment-load-path "jde/lisp" "jde")
(load-library "jde")
(load-library "jde-ant")

;;}}}
;;{{{  jde and senator 
(add-hook
 'jde-mode-hook
 (function
  (lambda nil
    (define-key jde-mode-map "\M-\t" 'jde-complete-in-line)
    (senator-minor-mode 1)
    (define-key senator-prefix-map "j" 'senator-jump))))

;;}}}
;;{{{ setup folding 

(fold-add-to-marks-list 'java-mode
                        "//<" "//>" "")
(fold-add-to-marks-list 'jde-mode
                        "//<" "//>" "")

(define-key help-map "j" 'jde-help-symbol)
(define-key help-map "\C-j" 'jde-show-class-source)
;(augment-load-path "jde-misc")

;;}}}
;;{{{ put jdebug on hyper-j 

(defvar tvr-jdebug-key-prefix "\C-x@hj"
"My personal key prefix for jdebug ")

(defun tvr-jde-bug-define-key  (k f )
  "Setup k to run f in jdebug using prefix
tvr-jdebug-key-prefix"
  (declare (special jde-mode-map tvr-jdebug-key-prefix))
  (define-key jde-mode-map
    (concat tvr-jdebug-key-prefix k) f))
(mapcar
 (function
  (lambda (spec)
    (tvr-jde-bug-define-key 
(first spec)
(second spec))))
 '(("b" jde-bug-set-breakpoint)
   ("s" jde-bug-step-over)
   ("S" jde-bug-step-into)
   ("o" jde-bug-step-out)
   ("c" jde-bug-continue)
   ("e"  jde-bug-exit)
   ("C" jde-bug-set-conditional-breakpoint)
   ("B" jde-bug-clear-breakpoint)
   ("0" jde-bug-save-breakpoints)
   ("w" jde-bug-watch-field-access)
   ("W" jde-bug-watch-field-modification)
   ("t" jde-bug-trace-method-entry)
   ("T" jde-bug-thread-show-thread-info)))

;;}}}
(add-to-list 'compilation-error-regexp-alist 
	       '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))

(load-library "jde-xref")
;(load-library "ajdee")
