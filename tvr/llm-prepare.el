;;; llm-prepare.el --- LLM   -*- lexical-binding: t -*-
;;; Prepare various  LLM front-ends 
;;; Keys are stored in passwd-store


(require 'ellama)
(require 'llm-gemini)
(require 'gptel "gptel" 'no-error)

;;; Ellama:
(setopt
 ellama-provider
 (make-llm-gemini :key (auth-source-pass-get 'secret "ai.google" )))

(setq ellama-assistant-nick "Gemini")
(global-set-key (kbd "C-; ," ) 'ellama-chat)

;;; gptel:

(when (featurep 'gptel)
  (setopt
   gptel-backend
   (gptel-make-gemini
    "Gemini"
    :key(auth-source-pass-get 'secret "ai.google" )
    :stream t)))
