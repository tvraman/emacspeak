// $Id:$

/*
 *@file init.js
 * Include bootstrapping code.
 */

/**
 * Emacspeak: Encapsulate Emacspeak specific bits.
 * @constructor
 * @param basepath {String} Location where Emacspeak is installed
 */

function Emacspeak (basepath) {
  this.path_ = (basepath === undefined) ? '/usr/share/emacs/site-lisp/emacspeak/' : basepath;

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
    repl.load(js + 'di.js');
    repl.load(js + 'adom.js');
    repl.load(js + 'speaker.js');
    this.speaker_ = new Speaker( this.path_);
      this.speaker_.say('Emacspeak Enabled Firefox');
  } catch (err) {
    repl.print('Error during init ' + err);
  }
};

repl.print('Loaded init.js');

