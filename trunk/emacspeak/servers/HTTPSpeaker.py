"""HTTP wrapper around Emacspeak speech server. """

from speaker import Speaker
import sys

from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler

class HTTPSpeaker (HTTPServer):
    
    """Speech server via HTTP."""

    def __init__(self, address, handler,
                 engine='outloud'):
        """Initialize HTTP listener."""
        HTTPServer.__init__(self, address, handler)
        self.__speaker = Speaker(engine)
    
class SpeakHTTPRequestHandler(BaseHTTPRequestHandler):

    """Handle HTTP Speak requests."""

    def do_GET(self):
        """Produce speech."""
        self.send_response(200, "It worked I hope")
        if self.server is not None:
            self.server.__speaker.say("this is a test. ")
    

def start():
    if sys.argv[1:]:
        port = int(sys.argv[1])
    else:
        port = 8000
    server_address = ('', port)
    httpd = HTTPSpeaker  (server_address,  SpeakHTTPRequestHandler)
    httpd.serve_forever()
    
