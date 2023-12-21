(setq-default gptel-backend
      (gptel-make-ollama
       "Ollama"
       :host "localhost:11434"
       :models '("zephyr" "llama2")
       :stream t))

(setq gptel-model "zephyr")
