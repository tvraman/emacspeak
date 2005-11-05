"""HTTP wrapper around Emacspeak speech server.

Speech server is launched on HTTP server startup.

Speech commands are invoked via URLs:

http://host:port/cmd?arg

calls speaker.cmd(arg)

"""

from speaker import Speaker
from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
import sys

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
                'shutdown'
                'version'             ]
    
    def do_GET(self):
        """Produce speech."""
        cmd = None
        arg = None
        
        # we either get a cmd or a cmd?arg
        q=self.path.find('?')
        if q is -1:
            cmd =self.path[1:]
        else:
            cmd = self.path[1:q]
            arg = self.path[q+1:]
        
        if cmd not in self.handlers:
            self.send_error(501, "unknown method %s" % cmd)
            return
        
        if (self.server is not None
            and hasattr(self.server.speaker, cmd)):
            method = getattr(self.server.speaker, cmd)
            if arg is None:
                method()
            else:
                method(arg)
            self.send_response(200, self.path)
        else:
            self.send_error(501, "Speaker error")
    
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

