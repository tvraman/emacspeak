(load-library "midge-mode")
(setq auto-mode-alist (cons '("\\.mgh?$" . midge-mode) auto-mode-alist))

(setq midge-midi-player "playmidi -a")
