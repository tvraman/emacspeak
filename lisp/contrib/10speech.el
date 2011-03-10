;;; This is from Tim Cross sent to the emacspeak list.

;; I've been using the following elisp to start emacspeak for years. I put this in
;; the /etc/emacs/site-start.d directory - it is called 10speech.el and has a
;; number of 10 so that it gets loaded before other packages etc. This is
;; important - for reliable setup, emacspeak's defadvice etc should be loaded
;; early in the startup process. 

;; You may want to remove some of my default settings i.e. setting speech rate,
;; turning off Raman's VM MIME configuration etc. At the very least, this should
;; give you an idea and a starting point.

;; Note that the script uses two environment variables, RUN_EMACSPEAK, which needs
;; to be set to have emacspeak loaded and SPEECH_SERVER, which I use to be able to
;; have multiple builds of emacspeak. With these two, I can control whether
;; emacspeak is loaded or which version is loaded simply by setting/changing
;; environment variables. The other nice thing is that you can have this script
;; installed centrally on a system used by non-emacspeak users and it won't impact
;; - they would have to set the RUN_EMACSPEAK env variable. 

;; Tim


;;      Filename: 10speech.el
;; Creation Date: Sunday, 16 January 2005 01:38 PM EST
;; Last Modified: Thursday, 03 June 2010 10:04 AM EST
;;        Author: Tim Cross <tcross@une.edu.au>
;;   Description: Script to load emacspeak when you run emacs
;;

(when (and (not noninteractive)
           (not (featurep 'emacspeak))
           (getenv "RUN_EMACSPEAK"))
  (let ((src-path (concat "/home/" user-login-name "/"
                          (or (getenv "SPEECH_SERVER")
                              "emacspeak"))))
    (add-to-list 'load-path
                 (expand-file-name "lisp"src-path))
    (setenv "EMACSPEAK_DIR" src-path)
    (setq emacspeak-aumix-multichannel-capable-p nil)
    (setq tts-default-speech-rate 90)
    (setq outloud-default-speech-rate 90)
    (setq emacspeak-vm-use-raman-settings nil)
    (load-library "emacspeak-setup")
    (add-hook 'emacspeak-startup-hook
              #'(lambda ()
                (dtk-set-rate tts-default-speech-rate 1)
                (dtk-interp-sync)
                (emacspeak-toggle-auditory-icons t)))))


