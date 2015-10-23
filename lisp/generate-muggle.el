;;; Generate A Muggle:
;;; Take a name of a keymap (symbol)
;;; And generate an interactive command that can be bound to a key.
;;; Invoking that command temporarily activates the previously supplied keymap.
;;; That activated keymap remains active until the user presses a key that is not bound in that keymap.
;;; Inspired by the Hydra package.

(defun emacspeak-muggles-generate (keymap-name)
  "Generate a Muggle from specified keymap-name.
Argument `keymap-name' is a symbol  that names a keymap."
  (unless (and (symbolp keymap-name)
           (boundp keymap-name)
               (keymapp (symbol-value keymap-name)))
    (error "%s is not a keymap." keymap-name))
  (lexical-let
      ((cmd-name (intern (format "emacspeak-muggles-%s-cmd" keymap-name)))
       (doc-string
        (format "Invoke commands from %s until an unbound key is pressed."
                keymap-name)))
    (eval

     `(defun ,cmd-name ()
    ,doc-string
    (interactive)
    (let((cmd nil))
      (while
          (setq cmd (lookup-key ,keymap-name (read-key-sequence "Key: ")))
        (call-interactively cmd))
      (emacspeak-auditory-icon 'select-object)
(emacspeak-muggles-generate 'view-mode-map)
