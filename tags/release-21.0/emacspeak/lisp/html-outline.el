;;; html-outline.el --- Extends html-helper-mode to provide outline and imenu support
;;; Author: T. V. Raman
;;; $Id$
;;; Description: Outlining for HTML helper mode
;;; Implements the necessary extensions to make outline minor mode recognize
;;; html markup.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
;;{{{  outline help

;;; Inspired by out-xtra.el (part of auctex)

;;; html-helper-mode-hook:
(defun html-outline-level ()
  ;; only recognizes sectioning constructs. 
  (save-excursion
    (looking-at outline-regexp)
    (skip-chars-forward "<hH ")
    (string-to-int (buffer-substring (point) (+ 1 (point))))))

(declaim (special outline-level))
(add-hook 'html-helper-mode-hook
	  (function (lambda ()
                      (require 'outline)
                      (make-local-variable 'outline-regexp)
                      (setq outline-regexp  "[\t ]*<[hH][1-6][]*>.*<\/[hH]")
                      (make-local-variable 'outline-level)
                      (setq outline-level 'html-outline-level)
                      (outline-minor-mode 1))))

;;}}}
;;{{{  imenu extensions

(add-hook
 'html-helper-mode-hook
 (function
  (lambda ()
    (declare (special imenu-generic-expression
		      imenu-create-index-function))
    (require 'imenu)
    (setq imenu-create-index-function 'imenu-default-create-index-function)
    (setq imenu-generic-expression
	  '(
	    (nil
	     "^ *<[hH][1-6] *><[aA][^>]+>\\(.*\\)</[aA]></[hH][1-6]>" 1)
	    (nil "^ *<[hH][1-6] *>\\([^<]*\\)</[hH][1-6]>" 1))))))

;;}}}
(provide 'html-outline)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
