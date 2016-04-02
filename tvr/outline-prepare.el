; Outline-minor-mode key map
(define-prefix-command 'tvr-outline-map nil)
; HIDE
(define-key tvr-outline-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key tvr-outline-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key tvr-outline-map "o" 'hide-other)        ; Hide other branches
(define-key tvr-outline-map "c" 'hide-entry)        ; Hide this entry's body
(define-key tvr-outline-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key tvr-outline-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key tvr-outline-map "a" 'show-all)          ; Show (expand) everything
(define-key tvr-outline-map "e" 'show-entry)        ; Show this heading's body
(define-key tvr-outline-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key tvr-outline-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key tvr-outline-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key tvr-outline-map "u" 'outline-up-heading)                ; Up
(define-key tvr-outline-map "n" 'outline-next-visible-heading)      ; Next
(define-key tvr-outline-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key tvr-outline-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key tvr-outline-map "b" 'outline-backward-same-level)       ; Backward - same level
;restore what we stole
(define-key tvr-outline-map "\C-o" 'open-line)
(global-set-key "\C-o" tvr-outline-map)

