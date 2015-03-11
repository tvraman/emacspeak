#!/usr/bin/python
# A Python client for HTTPSpeaker.
__id__ = "$Id$"
__author__ = "Jason White"
__version__ = "$Revision$"
__date__ = "$Date$"
__copyright__ = "Copyright 2011 Jason White"
__license__ = "LGPL"

from httplib import HTTPConnection, HTTPResponse

class HTTPSpeakerError(Exception):
    "Holds the HTTP status code and reason phrase returned by the server in the event of an error."
    def __init__(self, status, reason):
        self.status = status
        self.reason = reason
    def __str__(self):
        return "Status code " + str(self.status) + ": " + self.reason

class HTTPSpeakerClient:
    """Emacspeak HTTP speech client, for HTTPSpeaker instances."""
    def __init__(self, host="127.0.0.1", port=8000):
        "Initialize client to connect to server at given host and port."
        self._connection=HTTPConnection(host, port)

    def postCommand(self, command, arg=""):
        """Post command, with argument arg (default, empty), to the speech server.
        Returns the body of the server's HTTP response, if any. On error, HTTPSpeakerError is raised."""
        body = command
        if arg:
            body += ": " + arg
        self._connection.request("POST", "/", body, {"Content-type": "text/plain"})
        response=self._connection.getresponse()
        if response.status != 200:
            raise HTTPSpeakerError(response.status, response.reason)
        return response.read()

    def speak(self, text):
        "Speak the supplied string."
        self.postCommand("speak", text)

    def stop(self):
        "Stop speaking."
        self.postCommand("stop")

    def isSpeaking(self):
        "Return '0' when not speaking."
        return self.postCommand("isSpeaking")

    def close(self):
        "Close the connection to the speech server."
        self._connection.close()

# Test the server (assumes localhost:8000).
if __name__ == '__main__':
    client = HTTPSpeakerClient()
    client.speak("Hello, world!")
    print client.isSpeaking()
    client.close()
