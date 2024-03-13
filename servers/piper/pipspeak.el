(require 'cl-lib)
(defvar pipspeak-pip nil
  "process handle")

(defun pipspeak-start ()
  "Start the Piper process"
  (interactive)
  (cl-declare (special  pipspeak-pip))
  (unless (process-live-p pipspeak-pip)
    (let ((process-connection-type nil))
      (setq  pipspeak-pip
             (start-process  "pip" "*pip*" (executable-find
                                            "pipspeak")))))
  (pipspeak-speak "Piper is running!"))

(defun pipspeak-speak (text)
  "Speak text"
  (interactive "sText:")
  (cl-declare (special pipspeak-pip))
  (cl-assert (process-live-p pipspeak-pip) t "Start Piper first")
  (process-send-string pipspeak-pip  (format "%s\n" text)))
