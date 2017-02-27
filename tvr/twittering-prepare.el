(require 'epa)
(augment-load-path "twittering-mode" "twittering-mode")
(load-library "twittering-mode")
(setq twittering-use-master-password t)
(setq epa-protocol 'OpenPGP)
(setq twittering-timer-interval 300)
(setq twittering-timer-interval-for-redisplaying 300)
(setq twittering-initial-timeline-spec-string
              '("(:home+@)"
                "(:search/tvraamn/+:search/chromevox/+:search/emacspeak/+:search/googleaccess/)"))
(setq twittering-number-of-tweets-on-retrieval 50)
(setq twittering-edit-skeleton 'inherit-any)
