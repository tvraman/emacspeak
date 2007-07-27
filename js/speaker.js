//$Id:$

/*
 * Defines a utility class for launching the speech server.
 */


/**
 * Speaker encapsulates speech server connection.
 * @constructor
 * Optional: @param {path} Specifies Emacspeak install root
 * e.g., /usr/share/emacs/site-lisp/emacspeak/
 */

function Speaker( basepath) {
    basepath = (basepath === undefined) ? '/usr/share/emacs/site-lisp/emacspeak/'  : basepath;

    /**
* @private path_ location of server  executable
*/
    this.path_ = basepath + 'servers/python/HTTPSpeaker.py';

    /**
     * @private url_
     * URL of our local server
     */
    this.url_ ='http://localhost:8000/';
}

/**
 * Launch the server
 */

Speaker.prototype.init = function() {
    try {
        // See http://developer.mozilla.org/en/docs/Code_snippets:Running_applications
        var exec_file = Components.classes['@mozilla.org/file/local;1'].
        createInstance(Components.interfaces.nsILocalFile);
        this.exec_ = Components.classes['@mozilla.org/process/util;1'].
        createInstance(Components.interfaces.nsIProcess);
        exec_file.initWithPath(this.path_);
        this.exec_.init(exec_file);
        this.exec_.run(false, null, 0);
    } catch (err) {
        repl.print(
            'Cannot initialize executable with path=' + this.path_ + ' error: ' + err);
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
    xhr.onreadystatechange = function (data) {repl.print(data);};
    xhr.send(null);
};

repl.print('Loaded speaker.js');
