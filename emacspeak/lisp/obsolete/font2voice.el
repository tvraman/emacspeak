;;{{{  font to voice 

;;; let's define the standard symbols used as fonts as
;;; personalities here.

(outloud-define-voice-alias 'font-lock-variable-name-face 'voice-lock-variable-name-personality)
(outloud-define-voice-alias 'font-lock-reference-face 'voice-lock-reference-personality)
(outloud-define-voice-alias'font-lock-comment-face  'voice-lock-comment-personality)
(outloud-define-voice-alias'font-lock-string-face  'voice-lock-string-personality)
(outloud-define-voice-alias 'font-lock-keyword-face  'voice-lock-keyword-personality)
(outloud-define-voice-alias 'font-lock-builtin-face  'voice-lock-builtin-personality)
(outloud-define-voice-alias 'font-lock-function-name-face 'voice-lock-function-name-personality)
(outloud-define-voice-alias 'font-lock-type-face  'voice-lock-type-personality)
(outloud-define-voice-alias 'font-lock-constant-face  'voice-lock-constant-personality)
(outloud-define-voice-alias 'font-lock-warning-face  'voice-lock-warning-personality)
;;}}}
;;{{{  font to voice 

;;; let's define the standard symbols used as fonts as
;;; personalities here.

(dtk-define-voice-alias 'font-lock-variable-name-face 'voice-lock-variable-name-personality)
(dtk-define-voice-alias 'font-lock-reference-face 'voice-lock-reference-personality)
(dtk-define-voice-alias'font-lock-comment-face  'voice-lock-comment-personality)
(dtk-define-voice-alias'font-lock-string-face  'voice-lock-string-personality)
(dtk-define-voice-alias 'font-lock-keyword-face  'voice-lock-keyword-personality)
(dtk-define-voice-alias 'font-lock-builtin-face  'voice-lock-builtin-personality)
(dtk-define-voice-alias 'font-lock-function-name-face 'voice-lock-function-name-personality)
(dtk-define-voice-alias 'font-lock-type-face  'voice-lock-type-personality)
(dtk-define-voice-alias 'font-lock-constant-face  'voice-lock-constant-personality)
(dtk-define-voice-alias 'font-lock-warning-face  'voice-lock-warning-personality)
;;}}}
;;{{{ standard symbols as voices:

;; (dtk-define-voice-alias 'voice-lock-comment-personality 'paul-monotone)              ;;
;; (dtk-define-voice-alias 'voice-lock-underline-personality 'paul-animated)            ;;
;; (dtk-define-voice-alias 'voice-lock-bold-personality 'harry)                         ;;
;; (dtk-define-voice-alias 'voice-lock-italic-personality 'paul-italic)                 ;;
;; (dtk-define-voice-alias 'voice-lock-doc-string-personality 'dennis)                  ;;
;; (dtk-define-voice-alias 'voice-lock-string-personality 'betty)                       ;;
;; (dtk-define-voice-alias 'voice-lock-function-name-personality 'harry)                ;;
;; (dtk-define-voice-alias 'voice-lock-warning-personality 'paul-angry)                 ;;
;; (dtk-define-voice-alias 'voice-lock-keyword-personality 'ursula)                     ;;
;; (dtk-define-voice-alias 'voice-lock-builtin-personality 'harry)                      ;;
;; (dtk-define-voice-alias 'voice-lock-variable-name-personality 'paul-animated)        ;;
;; (dtk-define-voice-alias 'voice-lock-type-personality 'paul-smooth)                   ;;
;; (dtk-define-voice-alias 'voice-lock-reference-personality 'paul-animated)            ;;

;;}}}
;;{{{ standard symbols as voices:

(outloud-define-voice-alias 'voice-lock-comment-personality 'paul-monotone)
(outloud-define-voice-alias 'voice-lock-underline-personality 'paul-animated)
(outloud-define-voice-alias 'voice-lock-bold-personality 'paul-bold)
(outloud-define-voice-alias 'voice-lock-italic-personality 'paul-italic)
(outloud-define-voice-alias 'voice-lock-doc-string-personality 'dennis)
(outloud-define-voice-alias 'voice-lock-string-personality 'betty)
(outloud-define-voice-alias 'voice-lock-function-name-personality 'harry)
(outloud-define-voice-alias 'voice-lock-warning-personality 'paul-angry)
(outloud-define-voice-alias 'voice-lock-keyword-personality
                            'ursula)
(outloud-define-voice-alias 'voice-lock-builtin-personality
			    'harry)
(outloud-define-voice-alias 'voice-lock-variable-name-personality 'paul-italic)
(outloud-define-voice-alias 'voice-lock-type-personality 'paul-smooth)
(outloud-define-voice-alias 'voice-lock-reference-personality 'paul-italic)

;;}}}
;;{{{  Associate faces to standard voices:

(dtk-define-voice-alias 'bold 'harry)
(dtk-define-voice-alias 'bold-italic 'betty)
(dtk-define-voice-alias 'underline 'ursula)
(dtk-define-voice-alias 'fixed 'paul-monotone)
(dtk-define-voice-alias 'italic 'paul-animated)
(dtk-define-voice-alias 'excerpt 'annotation-voice )

;;}}}
;;{{{  Associate faces to standard voices:

(outloud-define-voice-alias 'bold 'paul-smooth)
(outloud-define-voice-alias 'bold-italic 'betty)
(outloud-define-voice-alias 'underline 'ursula)
(outloud-define-voice-alias 'fixed 'paul-monotone)
(outloud-define-voice-alias 'italic 'paul-italic)
(outloud-define-voice-alias 'excerpt 'annotation-voice )

;;}}}
;;{{{ setup standard mappings

;; (voice-setup-set-voice-for-face 'bold 'bold)
;; (voice-setup-set-voice-for-face 'italic 'italic)
;; (voice-setup-set-voice-for-face 'underline 'underline)
;; (voice-setup-set-voice-for-face 'underlined 'underline)
;; (voice-setup-set-voice-for-face 'bold-italic 'bold-italic)

;; (voice-setup-set-voice-for-face  'font-lock-comment-face 'voice-lock-comment-personality)
;; (voice-setup-set-voice-for-face 'font-lock-underline-face   'voice-lock-underline-personality )
;; (voice-setup-set-voice-for-face 'font-lock-bold-face   'voice-lock-bold-personality )
;; (voice-setup-set-voice-for-face 'font-lock-italic-face   'voice-lock-italic-personality )
;; (voice-setup-set-voice-for-face 'font-lock-doc-string-personality   'voice-lock-doc-string-personality)
;; (voice-setup-set-voice-for-face'font-lock-constant-face   'voice-lock-constant-personality)
;; (voice-setup-set-voice-for-face'font-lock-string-face   'voice-lock-string-personality)
;; (voice-setup-set-voice-for-face'font-lock-function-name-face 'voice-lock-function-name-personality)
;; (voice-setup-set-voice-for-face'font-lock-warning-face   'voice-lock-warning-personality)
;; (voice-setup-set-voice-for-face'font-lock-keyword-face   'voice-lock-keyword-personality)
;; (voice-setup-set-voice-for-face'font-lock-builtin-face   'voice-lock-builtin-personality)
;; (voice-setup-set-voice-for-face'font-lock-variable-name-face 'voice-lock-variable-name-personality)
;; (voice-setup-set-voice-for-face'font-lock-type-face   'voice-lock-type-personality)
;; (voice-setup-set-voice-for-face'font-lock-reference-face   'voice-lock-reference-personality)

;;}}}
