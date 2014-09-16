Simple local TTS server using Chrome TTS.

Author: Charles L. Chen (clchen@google.com)

This is based off of sample code from Chrome Commando TCP Server.
https://github.com/GoogleChrome/chrome-app-samples/tree/master/samples/tcpserver

Main changes from the sample code:
-Added Chrome TTS support.
-Added auto start on port 2222 of localhost to save a button click.
-Removed ability to remotely open web pages.
-Added TTS commands.

If you want to add commands, you can add them by modifying BrowserCommands.js

Note that this is a Chrome App and NOT an extension since the Chrome sockets API
only works on Chrome Apps, not extensions. This means you have to launch Chrome
TTS Server first and keep its app window open in the background for it to work.
