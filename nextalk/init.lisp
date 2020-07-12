(in-package :nyxt)

(ql:quickload "tts")
(tts:init)
(tts:speak "Welcome To The Next In Browsers!")
(swank:create-server :port 4006 :dont-close t)
(tts:speak "Swank server started at port 4006")

(tts:speak "Done loading init file!")
