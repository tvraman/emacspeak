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
        self.speaker = Speaker(engine)
    
class SpeakHTTPRequestHandler(BaseHTTPRequestHandler):

    """Handle HTTP Speak requests."""
    handlers = ['say', 'speak',
            'addText', 'silence',  'tone', 'stop',
            'punctuation', 'rate', 'allcaps', 'capitalize',
            'splitcaps',
            'reset', 'version'
            ]
    
    def do_GET(self):
        """Produce speech."""
        if self.path[1:] in self.handlers:
            sys.stderr.write("will call %s" % self.path[1:])
        self.send_response(200, self.path)
        if self.server is not None:
            self.server.speaker.say("Testing %s " % self.path[1:])

def start():
    if sys.argv[1:]:
        port = int(sys.argv[1])
    else:
        port = 8000
    server_address = ('', port)
    httpd = HTTPSpeaker  (server_address,  SpeakHTTPRequestHandler)
    httpd.serve_forever()
    

if __name__ == '__main__':
    start()

