(augment-load-path "emms" "emms")
(require 'emms-default)
(emms-setup 'cvs
'cvs "~/mp3"
"/disk/local5/"
"/disk/local6/")
;(require 'emms)
;(require 'emms-player-simple)
;(require 'emms-source-file)

      ;emms-source-list '((emms-directory-tree "~/mp3/")))


;;; add trplayer
(define-emms-simple-player raplayer
  '(file url)
  (regexp-opt '(".ra" ".ram" ".rpm"))
  "trplayer")
(setq emms-stream-default-action 'emms-play-url)
(setq emms-player-list
      '(emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer
                         emms-player-raplayer)
)
