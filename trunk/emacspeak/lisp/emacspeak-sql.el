;;; emacspeak-sql.el --- Speech enable sql-mode
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak extension to speech enable sql-mode
;;; Keywords: Emacspeak, database interaction
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech enables sql-mode--
;;; available from
;;; http://paddington.ic.uva.nl/public/sql-modes.zip
;;; sql-mode.el implemented by the above package
;;;sets up an Emacs to SQL interface where you can
;;;interactively evaluate SQL expressions.
;;; Code:

;;}}}
;;{{{ advice

(defadvice sqlplus-execute-command (after emacspeak pre act comp)
  "Provide auditory feedback and place point at the start of the output."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (sqlplus-back-command 2)
    (forward-line 1)
    (emacspeak-speak-line)))

(defadvice sqlplus-back-command (after emacspeak pre act
                                       comp)"Move prompt appropriately,  and speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (forward-line 1)
    (emacspeak-speak-line)))

(defadvice sqlplus-forward-command (after emacspeak pre act
                                       comp)
  "Move prompt appropriately,  and speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (forward-line 1)
    (emacspeak-speak-line)))


(defadvice sqlplus-next-command (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice sqlplus-previous-command (after emacspeak pre act comp)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))


(defadvice sql-send-region (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    ad-do-it
    (emacspeak-auditory-icon 'mark-object))
   (t ad-do-it))
  ad-return-value)

(defadvice sql-send-buffer (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    (emacspeak-auditory-icon 'select-object)
    ad-do-it
    (emacspeak-auditory-icon 'mark-object))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{ setup voice lock

(eval-when (compile)
  (require 'regexp-opt))

(defvar sql-voice-lock-keywords
  nil
  "Additional expressions to highlight in sql mode.")

(let ((types-1				;SQL Types
       (eval-when-compile
	 (regexp-opt '("char" "character"
			"date" "dec" "decimal" "double[ \\n\\t]+precision"
			"float"
			"int" "integer"
			"long"
			"mlslabel"
			"number" "raw" "real" "rowid"
			"smallint"
			"varchar" "varchar2"))))
      (types-2				;PL/SQL Additional Types
       (eval-when-compile
	 (regexp-opt '("binary_integer" "boolean"
			"cursor"))))
      (types-3				;PL/SQL Column and Record Type Indicators
       (eval-when-compile
	 (regexp-opt '("%type"
		       "%rowtype"))))
      (functions-1			;Single Row Number Functions
       (eval-when-compile
	 (regexp-opt '("abs"
			"ceil" "cos" "cosh"
			"exp"
			"floor"
			"ln" "log"
			"mod"
			"power"
			"round"
			"sign" "sin" "sinh" "sqrt"
			"tan" "tanh" "trunc"))))
      (functions-2			;Single Row Character Functions Returning Char
       (eval-when-compile
	 (regexp-opt '("chr" "concat"
			"initcap"
			"lower" "lpad" "ltrim"
			"nls_initcap" "nls_lower" "nls_upper"
			"replace" "rpad" "rtrim"
			"soundex" "substr" "substrb"
			"translate"
			"upper"))))
      (functions-3			;Single Row Character Functions Returning Number
       (eval-when-compile
	 (regexp-opt '("ascii"
			"instr" "instrb"
			"length" "lengthb"
			"nlssort"))))
      (functions-4			;Single Row Date Functions
       (eval-when-compile
	 (regexp-opt '("add_months"
			"last_day"
			"months_between"
			"new_time" "next_day"))))
      (functions-5			;Other Single Row Functions (no parens needed!)
       (eval-when-compile
	 (regexp-opt '("currval"
			"nextval"
			"sqlcode" "sqlerrm" "sysdate"
			"uid" "user"))))
      (functions-6			;Single Row Conversion Functions
       (eval-when-compile
	 (regexp-opt '("chartorowid" "convert"
			"hextoraw"
			"rawtohex" "rowidtochar"
			"to_char" "to_date" "to_label" "to_multi_byte" "to_number"
			"to_single_byte"))))
      (functions-7			;Other Single Row Functions (parens needed)
       (eval-when-compile
	 (regexp-opt '("decode" "dump"
			"greatest" "greatest_lb"
			"least" "least_ub"
			"nvl"
			"userenv"
			"vsize"))))
      (functions-8			;Group Functions
       (eval-when-compile
	 (regexp-opt '("avg"
			"count"
			"glb"
			"lub"
			"max" "min"
			"stddev" "sum"
			"variance"))))
      (functions-9			;PL/SQL Pragma Clause and Function
       (eval-when-compile
	 (regexp-opt '("exception_init"
			"raise_application_error"))))
      (cmndwords-1			;These words start SQL commands
       (eval-when-compile
	 (regexp-opt '("alter" "analyze" "audit"
			"comment" "commit" "create"
			"delete" "drop"
			"explain\\([ \\t]+plan\\)?"
			"grant"
			"insert"
			"lock\\([ \\t]+table\\)?"
			"noaudit"
			"rename" "revoke" "rollback\\([ \\t]+segment\\)?"
			"savepoint" "select" "set"
			"truncate"
			"update"))))
      (cmndwords-2			;These words appear in SQL command names
       (eval-when-compile
	 (regexp-opt '("cluster" "column" "controlfile"
			"database\\([ \\t]+link\\)?"
			"function"
			"index"
			"package\\([ \\t]+body\\)?" "procedure" "profile"
			"resource cost" "role"
			"schema" "sequence" "session" "snapshot\\([ \\t]+log\\)?"
			"synonym" "system"
			"table" "tablespace" "transaction" "trigger"
			"user"
			"view"))))
      (exceptions			;Predefined Exceptions + OTHERS
       (eval-when-compile
	 (regexp-opt '("cursor_already_open"
			"dup_val_on_index"
			"invalid_cursor"
			"invalid_number"
			"login_denied"
			"no_data_found"
			"not_logged_on"
			"program_error"
			"storage_error"
			"timeout_on_resource"
			"too_many_rows"
			"transaction_backed_out"
			"value_error"
			"zero_divide"
			"others"))))
      (operators			;SQL Set Operators
       (eval-when-compile
	 (regexp-opt '("and"
			"in" "intersect"
			"like"
			"minus"
			"not"
			"or"
			"union\\([ \\n\\t]+all\\)?"))))
      (keywords-1			;SQL Keywords (most, not all :-)
       (eval-when-compile
	 (regexp-opt '("add" "admin" "after" "all" "allocate\\([ \\n\\t]+extent\\)?"
			"any" "\\(no\\)?archivelog" "as" "asc"
			"backup" "before" "between" "by"
			"\\(no\\)?cache" "cancel" "cascade" "change" "check"
			"checkpoint" "connect[ \\n\\t]+by" "constraints?" "convert"
			"\\(no\\)?cycle"
			"datafiles?" "default" "desc" "distinct" "drop"
			"exclusive" "execute" "externally"
			"false" "for[ \\n\\t]+update\\(\\n\\t]+of\\)?" "foreign"
			"from"
			"group[ \\n\\t]+by"
			"having"
			"identified" "increment" "instance" "is"
			"key"
			"limit" "logfile\\([ \\n\\t]+member\\)?"
			"maxtrans" "\\(no\\)?maxvalue" "\\(no\\)?minvalue"
			"modify" "mount"
			"nowait" "null"
			"on" "option" "\\(no\\)?order" "order[ \\n\\t]+by"
			"primary" "private" "public"
			"recover" "references" "replace" "reuse"
			"start[ \\n\\t]+with" "size" "storage"
			"to" "true"
			"unique" "unlimited" "using"
			"values" "where" "with"))))
      (keywords-2			;PL/SQL Keywords (hopefully all)
       (eval-when-compile
	 (regexp-opt '("begin" "close" "current" "declare" "deleting" "each" "else"
			"elsif" "end" "exception" "exit\\([ \\n\\t]+when\\)?" "fetch"
			"for" "goto" "if" "in" "inserting" "into" "is" "loop" "of"
			"open" "out" "pragma" "raise" "record" "return" "reverse"
			"row" "then" "type" "updating" "when" "while"))))
      )
  (setq sql-voice-lock-keywords
	(list
	 (list (concat "\\b\\(" types-1     "\\)\\b")
	       1 'voice-lock-type-personality)
	 (list (concat "\\b\\(" types-2     "\\)\\b")
	       1 'voice-lock-type-personality)
	 (list (concat "\\w\\(" types-3     "\\)\\b")
	       1 'voice-lock-type-personality)
	 (list (concat "\\b\\(" functions-1 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-2 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-3 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-4 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-5 "\\)[ \n\t;]") ;No parentheses!
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-6 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-7 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-8 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" functions-9 "\\)[ \n\t]*(")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" cmndwords-1 "\\)\\b")
	       1 'voice-lock-keyword-personality)
	 (list (concat "\\b\\(" cmndwords-2 "\\)\\b")
	       1 'voice-lock-keyword-personality)
	 (list (concat "\\b\\(" exceptions  "\\)\\b")
	       1 'voice-lock-function-name-personality)
	 (list (concat "\\b\\(" operators   "\\)\\b")
	       1 'voice-lock-keyword-personality)
	 (list (concat "\\b\\(" keywords-1  "\\)\\b")
	       1 'voice-lock-keyword-personality)
	 (list (concat "\\b\\(" keywords-2  "\\)\\b")
	       1 'voice-lock-keyword-personality)
	 (list "\\b\\(function\\|procedure\\|package\\)[ \t]+\\([^ \t(]+\\)"
	       2 'voice-lock-function-name-personality)
	 (list "'\\([^']*\\)'" 1 'voice-lock-string-personality t)
	 (list "\\(--.*\\)" 1 'voice-lock-comment-personality t)
	 (list "\\(/\\*.*\\*/\\)" 1 'voice-lock-comment-personality t)
	 (list "^\\([rR][eE][mM]\\([ \t].*\\)?\\)$" 1
               'voice-lock-comment-personality t)
	 (list "^\\(prompt\\([ \t].*\\)?\\)$" 1 'voice-lock-comment-personality t))))

(voice-lock-set-major-mode-keywords 'sql-mode
                                    'sql-voice-lock-keywords)

;;}}}
(provide 'emacspeak-sql)

;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
