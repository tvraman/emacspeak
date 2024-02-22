;;; llm-prepare.el --- LLM   -*- lexical-binding: t -*-
;;; Prepare various  LLM front-ends
;;; Keys are stored in passwd-store

(require 'ellama)
(require 'llm-gemini)
(require 'gptel)

;;; Ellama:
(setopt
 ellama-provider
 (make-llm-gemini :key (auth-source-pass-get 'secret "ai.google" )))

(setopt
 ellama-providers
 '(("gemma" .
    (make-llm-ollama
     :chat-model "gemma:7b" :embedding-model "gemma:7b"))))

(setq ellama-assistant-nick "Gemini")
(global-set-key (kbd "C-; ," ) 'ellama-chat)

;;; gptel:
(if (zerop (length (string-trim (shell-command-to-string "pidof ollama"))))
                                        ; remote  model
  (setopt
   gptel-backend
   (gptel-make-gemini "Gemini"
     :key(auth-source-pass-get 'secret "ai.google" )
     :stream t))
  (setopt gptel-backend                   ; gemma
        (gptel-make-ollama "gemma"
          :models '("gemma:2b") :stream t)))
