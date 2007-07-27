//$Id:$

/*
 * Defines a utility class for launching the speech server.
 */
/**
 * Speaker encapsulates speech server connection.
 * @constructor
 * @param start: boolean Start new server if true.
 * Optional: @param {path} Specifies Emacspeak install root
 * e.g., /usr/share/emacs/site-lisp/emacspeak/
 */

function Speaker(start, path) {
    this.path_ = (path === undefined) ? '/usr/share/emacs/site-lisp/emacspeak/'  : path;
    this.path_ += 'servers/python/HTTPSpeaker.py';
    if (start)
        try {
            var exec_file = Components.classes['@mozilla.org/file/local;1'].
                createInstance(Components.interfaces.nsILocalFile);
            this.exec_ = Components.classes['@mozilla.org/process/util;1'].
                createInstance(Components.interfaces.nsIProcess);
            exec_file.initWithPath(this.path_);
            this.exec_.init(exec_file);
        } catch (err) {
            repl.print(
                       'Cannot initialize executable with path=' + this.path_ + ' error: ' + err);
        }
}

/**
 * Launch the server
 */

Speaker.prototype.init = function() {
  try {
    this.exec_.run(false, null, 0);
    /**
     * @private url_
     * URL of our local server
     */
    this.url_ ='http://localhost:8000/';
    this.say('Emacspeak');
  } catch (err) {
    repl.print('Error running ' + this.path_ );
  }
};


/**
 * Speak argument.
 @param {string} text to speak.
 * @return void
 */
Speaker.prototype.say = function(text) {
  var url = this.url_ + 'say?' +encodeURIComponent(text);
  var xhr  = new XMLHttpRequest();
  xhr.open('GET', url,true);
  xhr.send(null);
};

repl.print('Loaded speaker.js');
