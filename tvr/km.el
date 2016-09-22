;;; Move emacspeak-hyper-keys etc from a list of conses to a list of lists  -*- lexical-binding: t; -*-
;;; Loading this file once should suffice to complete the migration.
;;; Make sure to save the customized settings after loading this.


(defun emacspeak-wizards-migrate-keymap (km)
  "Migrate emacspeak keymaps like hyper to new format."
  (let ((l nil)
        (k (copy-sequence km)))
    (loop for p in k do
          (push (list (car p) (cdr p)) l))
(setq km (copy-sequence l))))
(setq emacspeak-alt-keys (emacspeak-wizards-migrate-keymap emacspeak-alt-keys))
(setq emacspeak-hyper-keys (emacspeak-wizards-migrate-keymap emacspeak-hyper-keys))
(setq emacspeak-alt-keys (emacspeak-wizards-migrate-keymap emacspeak-alt-keys))
(setq emacspeak-super-keys (emacspeak-wizards-migrate-keymap emacspeak-super-keys))
(setq emacspeak-personal-keys (emacspeak-wizards-migrate-keymap emacspeak-personal-keys))
(setq emacspeak-personal-ctlx-keys (emacspeak-wizards-migrate-keymap emacspeak-personal-ctlx-keys))
(setq emacspeak-media-location-bindings (emacspeak-wizards-migrate-keymap emacspeak-media-location-bindings))
