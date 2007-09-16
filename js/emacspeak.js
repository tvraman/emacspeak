//$Id:$

/**
 * @file emacspeak.js
 * Holds Emacspeak connection
 */


/**
 * Emacspeak: encapsulate Emacspeak connection
 * @constructor
 * Optional: @param {path} Specifies Emacspeak install root
 * e.g., /usr/share/emacs/site-lisp/emacspeak/
 */

function Emacspeak( basepath) {
  /**
   * @private basepath_ Install location
   */
  this.path_ = (basepath === undefined) ? '/usr/share/emacs/site-lisp/emacspeak/'  : basepath;

  /**
   * @private cmd_ location of server  executable
   */
  this.cmd_ = basepath + 'servers/python/HTTPSpeaker.py';

  /**
   * @private url_
   * URL of our local server
   */
  this.url_ ='http://localhost:8000/';
}


/**
 * Load needed modules and initialize things.
 */

Emacspeak.prototype.init = function() {
  try {
    var js = 'file://localhost' + this.path_ + 'js/';
    repl.load(js + 'di.js');
    repl.load(js + 'adom.js');
    window.addEventListener(
                            'load',
                            function () {repl.updateADom(); },
                            true);
    this.say('Emacspeak Enabled Firefox');
  } catch (err) {
    repl.print('Error during init ' + err);
  }
};

/**
 * Launch the server
 */

Emacspeak.prototype.initserver = function() {
  try {
    // See http://developer.mozilla.org/en/docs/Code_snippets:Running_applications
    var exec_file = Components.classes['@mozilla.org/file/local;1'].
    createInstance(Components.interfaces.nsILocalFile);
    this.exec_ = Components.classes['@mozilla.org/process/util;1'].
    createInstance(Components.interfaces.nsIProcess);
    exec_file.initWithPath(this.cmd_);
    this.exec_.init(exec_file);
    this.exec_.run(false, null, 0);
  } catch (err) {
    repl.print(
        'Cannot initialize executable with path=' + this.cmd_ + ' error: ' + err);
  }
};


/**
 * Speak argument.
 @param {string} text to speak.
 * @return void
 */
Emacspeak.prototype.say = function(text) {
  var url = this.url_ + 'say?' +encodeURIComponent(text);
  var xhr  = new XMLHttpRequest();
  xhr.open('GET', url,true);
  // xhr.onreadystatechange = function (data) {repl.print(data);};
  xhr.send(null);
};

repl.print('Loaded emacspeak.js');
