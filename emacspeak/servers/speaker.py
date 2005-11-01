#!/usr/bin/env python
#$Id$
#Licence: GPL
__author__ = "$Author$"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright (c) 2005 T. V. Raman"
__license__ = "GPL"

"""Speaker --- Wrapper class around Emacspeak TTS Server.

The emacspeak TTS server provides a simple but powerful and
well-tested speech-server abstraction. That server is implemented
as an external program (typically in TCL).  This wrapper class
provides Python access to available Emacspeak speech servers.

Initially, this class will provide Python access to the TTS
server commands.  Over time, this module may also implement
functionality present in Emacspeak's Lisp layer ---specifically,
higher level TTS functionality provided by the following
emacspeak modules:

0)dtk-speak.el

1)emacspeak-pronounce.el

"""

import os

class Speaker:
    "Provides speech servre abstraction."

    location="/usr/share/emacs/site-lisp/emacspeak/servers"

    def __init__ (self, engine='outloud', host='localhost'):
        "Launches speech engine."
        self.__engine =engine
        if host is 'localhost':
            self.__server = os.path.join(Speaker.location, self.__engine)
        else:
            self.__server = os.path.join(Speaker.location,
                                         "ssh-%s" % self.__engine)
        self.__handle = os.popen(self.__server,"w")
        self.__handle.flush()

    def say(self, text=""):
        "Speaks specified text. All queued text is spoken immediately."
        self.__handle.write("q {%s}\nd\n" %text)
        self.__handle.flush()

    def speak(self):
        "Forces queued text to be spoken."
        self.__handle.write("d\n")
        self.__handle.flush()
    
    def addText(self, text=""):
        "Queue text to be spoken. Output is produced by next call to speak()."
        self.__handle.write("q {%s}\n" %text)

    def stop(self):
        "Silence ongoing speech."
        self.__handle.write("s\n")
        self.__handle.flush()

    def shutdown(self):
        "Shutdown speech engine."
        self.__handle.close()


    def sayUtterances(self, list):
        "Speak list of utterances."
        for t in list: self.__handle.write("q { %s }\n" %str(t))
        self.__handle.write("d\n")
        self.__handle.flush()

    def version(self):
        "Speak TTS version info."
        self.__handle.write("version\n")
        self.__handle.flush()

    def punctuations(self, mode):
        "Set punctuation mode."
        self.__handle.write("tts_set_punctuations %s\n" % mode)
        self.__handle.flush()


    def rate(self, r):
        "Set speech rate."
        self.__handle.write("tts_set_speech_rate %s\n" % r)
        self.__handle.flush()

    def letter (self, l):
        "Speak single character."
        self.__handle.write("l {%s}\n" %l)
        self.__handle.flush()

    def tone(self, pitch=440, duration=50):
        "Produce specified tone."
        self.__handle.write("t %s %s\n " % (pitch, duration))
        self.__handle.flush()

    def silence( self, duration=50):
        "Produce specified silence."
        self.__handle.write("sh  %s" %  duration)
        self.__handle.flush()

    def splitcaps(self, flag):
        "Set splitcaps mode. 1  turns on, 0 turns off"
        self.__handle.write("tts_split_caps %s\n" % flag)
        self.__handle.flush()

    def capitalize(self, flag):
        "Set capitalization  mode. 1  turns on, 0 turns off"
        self.__handle.write("tts_capitalize %s\n" % flag)
        self.__handle.flush()

    def allcaps(self, flag):
        "Set allcaps  mode. 1  turns on, 0 turns off"
        self.__handle.write("tts_allcaps %s\n" % flag)
        self.__handle.flush()

if __name__=="__main__":
    s=Speaker()
    s.addText(range(10))
    s.speak()
    

