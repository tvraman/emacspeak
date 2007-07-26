// $Id:$

/*
 *@file init.js
 * Include bootstrapping code.
 */

/**
 * Emacspeak: Encapsulate Emacspeak specific bits.
 * @constructor
 * @param path {String} Location where Emacspeak is installed
 */

function Emacspeak (path) {
  this.path_ = (path === undefined) ? '/usr/share/emacs/site-lisp/emacspeak/' : path;
  /*
   * @private speaker_ Handle to speaker instance.
   */
  this.speaker_ = null;
}

/**
 * Load needed modules and initialize things.
 */

Emacspeak.prototype.init = function() {
  try {
    var js = 'file://localhost' + this.path_ + 'js/';
    repl.load(js + 'dis.js');
    repl.print('dis.js');
    repl.load(js + 'adom.js');
    repl.print('adom');
    repl.load(js + 'speaker.js');
    this.speaker_ = new Speaker();
    this.speaker_.init();
    this.speaker_.say('Welcome to Emacspeak');
  } catch (err) {
    repl.print("Error during init " + err);
  }
};
  
repl.print("Loaded init.js");
