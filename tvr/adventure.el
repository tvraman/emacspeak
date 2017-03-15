(defun adventure ()
(interactive)
(comint-run "/usr/games/adventure")
(emacspeak-toggle-comint-autospeak))
