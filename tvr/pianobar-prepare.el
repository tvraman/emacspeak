(load-library "pianobar")
(setq pianobar-key "\C-x@ap")
(pianobar-key-setup)
(define-key pianobar-key-map "," 'pianobar)
