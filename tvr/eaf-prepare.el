(push (expand-file-name "eaf/" tvr-site-lib) load-path)
(load-library "eaf")
(require 'eaf-browser)
(eaf-setq eaf-browser-enable-adblocker "true")
(setq eaf-pdf-extension-list nil)
(defun emacspeak-eaf-web (url)
  "Launch async web browser."
  (interactive (list (emacspeak-eww-read-url)))
  (make-thread #'(lambda () (eaf-open-browser url))))

(global-set-key (ems-kbd "C-; C-w") 'emacspeak-eaf-web)

                                        ;(require


                                        ;'eaf-js-video-player)

                                        ;(require 'eaf-org-previewer)
                                        ;(require 'eaf-image-viewer)
                                        ;(require 'eaf-music-player)
                                        ;(require 'eaf-file-sender)
                                        ;(require 'eaf-file-browser)
                                        ;(require 'eaf-airshare)
                                        ;(require 'eaf-system-monitor)
                                        ;(require 'eaf-jupyter)
                                        ;(require 'eaf-pdf-viewer)
                                        ;(require 'eaf-video-player)
                                        ;(require 'eaf-demo)
                                        ;(require 'eaf-netease-cloud-music)
                                        ;(require 'eaf-mindmap)
                                        ;(require 'eaf-terminal)
                                        ;(require 'eaf-markdown-previewer)
                                        ;(require 'eaf-vue-demo)
                                        ;(require 'eaf-file-manager)
                                        ;(require 'eaf-camera)

