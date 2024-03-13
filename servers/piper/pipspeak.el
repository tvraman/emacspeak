;;; pipspeak --- Interface To Piper TTS -*- lexical-binding: t; -*-
(require 'cl-lib)
(defvar pipspeak-sh
  (expand-file-name "pipspeak" (file-name-directory load-file-name))
  "Spawn Piper TTS")

(defvar pipspeak-pip nil
  "process handle")

(defun pipspeak-start ()
  "Start the Piper process"
  (interactive)
  (cl-declare (special  pipspeak-pip))
  (unless (process-live-p pipspeak-pip)
    (let ((process-connection-type nil))
      (setq  pipspeak-pip (start-process  "pip" nil  pipspeak-sh))))
  (pipspeak-speak "Piper is running!"))
(defun pipspeak-shutdown ()
  "Shutdown Piper TTS"
  (interactive)
  (cl-declare (special pipspeak-pip))
  (delete-process pipspeak-pip))
(defun pipspeak-speak (text)
  "Speak text"
  (interactive "sText:")
  (cl-declare (special pipspeak-pip))
  (cl-assert (process-live-p pipspeak-pip) t "Start Piper first")
  (process-send-string pipspeak-pip  (format "%s\n" text)))
