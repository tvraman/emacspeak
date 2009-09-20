;;; Saved through ges-version 0.3.3dev at 2004-11-20 18:20
;;; From: Marco Parrone <marc0@autistici.org>
;;; Subject: toy-braille.el  ---  Code to get "toy" UCS braille strings
;;; Keywords: Emacs, Unicode, UCS, toy, braille
;;; Newsgroups: gmane.emacs.sources
;;; Date: Mon, 12 Jul 2004 21:40:05 GMT
;;; Organization: none

;;; toy-braille.el  ---  Code to get "toy" UCS braille strings.     -*- coding: utf-8 -*-

;; NO (C)  2004  Marco Parrone.
;; This file is placed into the Public Domain.

;; The author puts no legal restrictions on what you can do with this
;; file.  So copying and distribution of this file, with or without
;; modification, are permitted.

;; Filename: toy-braille.el
;; Version: 0.1.0 (alpha)
;; Updated: 12th of July 2004
;; Keywords: Unicode, UCS, toy, braille
;; Description: Code to get "toy" UCS braille strings.
;; Language: Emacs Lisp
;; Compatibility: GNU Emacs 21.3

;;; Commentary:

;; This is a bit of toy code to write in braille.
;;
;; To try this, load this file (`M-x load-file path/to/toy-braille.el'), then do:
;;
;;   M-: (get-toy-braille-string "just a test")
;;
;; That's just a toy, meant as an excuse and maybe a tool to learn a
;; bit of braille, nothing more.
;;
;; Unicode fonts are needed.
;;
;; You can try:
;;
;;   M-: (set-default-font "-*-unifont-*-*-*-*-*-*-*-*-*-*-*-*")
;;
;; or
;;
;;   M-: (set-default-font "-*-clearlyu-*-*-*-*-*-*-*-*-*-*-iso10646-*")
;;
;; (it will only work if the relative font is installed and properly configured).

;; References:
;;
;;   http://www.nbp.org/ic/nbp/braille/index.html
;;
;;   http://www.unicode.org/Public/4.0-Update1/UnicodeData-4.0.1.txt

;;; Code:

(setq toy-braille-map
      '((?  . "⠀") (?. . "⠲") (?, . "⠐")

        (?1 . "⠼⠁") (?2 . "⠼⠃") (?3 . "⠼⠉") (?4 . "⠼⠙")
        (?5 . "⠼⠑") (?6 . "⠼⠋") (?7 . "⠼⠛") (?8 . "⠼⠓")
        (?9 . "⠼⠊") (?0 . "⠼⠚")

        (?a . "⠁") (?b . "⠃") (?c . "⠉") (?d . "⠙")
        (?e . "⠑") (?f . "⠋") (?g . "⠛") (?h . "⠓")
        (?i . "⠊") (?j . "⠚") (?k . "⠅") (?l . "⠇")
        (?m . "⠍") (?n . "⠝") (?o . "⠕") (?p . "⠏")
        (?q . "⠟") (?r . "⠗") (?s . "⠎") (?t . "⠞")
        (?u . "⠥") (?v . "⠧") (?w . "⠺") (?x . "⠭")
        (?y . "⠽") (?z . "⠵")

        (?A . "⠠⠁") (?B . "⠠⠃") (?C . "⠠⠉") (?D . "⠠⠙")
        (?E . "⠠⠑") (?F . "⠠⠋") (?G . "⠠⠛") (?H . "⠠⠓")
        (?I . "⠠⠊") (?J . "⠠⠚") (?K . "⠠⠅") (?L . "⠠⠇")
        (?M . "⠠⠍") (?N . "⠠⠝") (?O . "⠠⠕") (?P . "⠠⠏")
        (?Q . "⠠⠟") (?R . "⠠⠗") (?S . "⠠⠎") (?T . "⠠⠞")
        (?U . "⠠⠥") (?V . "⠠⠧") (?W . "⠠⠺") (?X . "⠠⠭")
        (?Y . "⠠⠽") (?Z . "⠠⠵")))

(defun get-toy-braille-string (instr)
  (let ((inlst (string-to-list instr)))
    (apply 'concat
      (mapcar (lambda (c)
                (let ((tc (assoc c toy-braille-map)))
                  (if tc
                    (cdr tc)
                    (error
                     (concat "Character `"
                             (string c)
                             "' not found in `toy-braille-map'")))))
      inlst))))

;;;; toy-braille.el ends here.

(provide 'toy-braille)
