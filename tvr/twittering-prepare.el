;; -*- lexical-binding: nil; -*-
(defalias 'epa--decode-coding-string 'decode-coding-string)
(setq twittering-use-master-password t)

(eval-after-load "twittering-mode"
`(progn
   (require 'epa)
(setq twittering-timer-interval 300)
(setq twittering-timer-interval-for-redisplaying 300)
(setq twittering-initial-timeline-spec-string
              '("(:home+@)"
                ;"(:search/tvraamn/+:search/chromevox/+:search/emacspeak/+:search/googleaccess/)"
                ))
(setq twittering-number-of-tweets-on-retrieval 50)
(setq twittering-edit-skeleton 'inherit-any)
))
