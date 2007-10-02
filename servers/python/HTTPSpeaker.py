#!/usr/bin/python2.4

"""HTTP wrapper around Emacspeak speech server.

Speech server is launched on HTTP server startup.

Speech commands are invoked via URLs:

http://host:port/cmd?arg

calls speaker.cmd(arg)

"""

__id__ = "$Id$"
__author__ = "$Author$"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright (c) 2005 T. V. Raman"
__license__ = "LGPL"


from speaker import Speaker
from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
import sys
import urllib

class HTTPSpeaker (HTTPServer):

    """Speech server via HTTP."""

    def __init__(self, address, handler,
                 engine='outloud'):
        """Initialize HTTP listener."""
        HTTPServer.__init__(self, address, handler)
        self.speaker = Speaker(engine)

class SpeakHTTPRequestHandler(BaseHTTPRequestHandler):

    """Handle HTTP Speak requests."""
    handlers = ['say',
                'speak',
                'letter',
                'addText',
                'silence',
                'tone',
                'stop',
                'punctuation',
                'rate',
                'allcaps',
                'capitalize',
                'splitcaps',
                'reset',
                'shutdown',
                'version'             ]

    def do_GET(self):
        """Produce speech."""
        cmd = None
        arg = None

        # we either get a cmd or a cmd?arg
        q=self.path.find('?')
        if q == -1:
            cmd =self.path[1:]
        else:
            cmd = self.path[1:q]
            arg = self.path[q+1:].replace('+', ' ')

        if cmd not in self.handlers:
            sys.stderr.write("cmd %s handlers %s" % (cmd, self.handlers))
            self.send_error(501, "unknown method %s" % cmd)
            return

        if hasattr(self.server.speaker, cmd):
            method = getattr(self.server.speaker, cmd)
            if arg is None:
                method()
            else:
                method(urllib.unquote(arg))
            self.send_response(200, self.path)
        else: self.send_error(501, "Speaker error")

    def do_POST(self):
        """Handle speech request in a POST message. """
        contentLength = self.headers.getheader('content-length')
        if contentLength:
            contentLength = int(contentLength)
            inputBody = self.rfile.read(contentLength)
            if inputBody.startswith("speak:"):
                self.server.speaker.speak( inputBody[6:])
                self.send_response(200, 'OK')
            elif inputBody == "stop":
                self.server.speaker.stop()
                self.send_response(200, 'OK')
            elif inputBody == "isSpeaking":
                self.send_response(200, 'OK')
                self.send_header("Content-type", "text/html")
                self.end_headers()
                self.wfile.write("0")
            else:
                self.send_error(501, 'Unknown POST message')
    
def start():
    if sys.argv[1:]:
        engine = sys.argv[1]
    else:
        engine='outloud'
    if sys.argv[2:]:
        port = int(sys.argv[2])
    else:
        port = 8000
    server_address = ('', port)
    httpd = HTTPSpeaker  (server_address,
    SpeakHTTPRequestHandler, engine)
    httpd.serve_forever()


if __name__ == '__main__':
    start()
