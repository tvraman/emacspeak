//$Id$


/*
 * ADOM: Holds a proxy to a DOM
 * Provides convenience methods for obtaining custom views
 * Constructor takes  the   document to viewed as argument 
 */

function ADom (document) {
    this.document_ = document;
    document.aDOM = this;
    this.root_ = document.documentElement;
    this.current_ = this.root_;
    this.snapshot_ = null;
    this.visiting_ = -1;
}

/*
 * Return HTML for current node.
 *@Return {string}; innerHTML
 */
ADom.prototype.html = function () {
    return this.current_.innerHTML;
};


/*
 * Reset view.
 * Resets current to point at the root.
 */
ADom.prototype.reset = function () {
    this.root_ = this.document_.documentElement;
    this.current_ = this.root_;
};


/*
 * next: Move to next sibling.
 */
ADom.prototype.next = function () {
    this.current_ = this.current_.nextSibling;
};

/*
 * previous: Move to previous sibling.
 */
ADom.prototype.previous = function() {
    this.current_ = this.current_.previousSibling;
};


/*
 * up: Move to parent.
 */
ADom.prototype.up = function () {
    this.current_ = this.parentNode;
};


/*
 * down: Move to first child
 */
ADom.prototype.down = function () {
    this.current_ = this.current_.firstChild;
};


/*
 * first: Move to first sibling
 */
ADom.prototype.first = function () {
    this.current_ = this.current_.parentNode.childNodes[0];
};

/*
 * last: Move to last sibling.
 */
ADom.prototype.last = function () {
    this.current_  = this.current_.parentNode.lastChild;
};



/*
 * Move to  document body
 */
ADom.prototype.body = function () {
    this.current_ =  this.document_.body;
};


/*
 * summarize: Summarize current node.
 * Returns: @{string};
 */
ADom.prototype.summarize = function () {
    var summary = this.current_.tagName +' ';
    summary += 'has ' + this.current_.childNodes.length + 'children ';
    summary += ' with ' + this.current_.innerHTML.length + ' bytes of content.';
    return summary;
};
    
        


/*
 * title: return document title
 * Returns: @{string}
 */
ADom.prototype.title = function () {
    return this.document_.title;
};

/*
 * Return document being viewed.
 */
ADom.prototype.document = function () {
    return this.document_;
};


/*
 * Return the current node being viewed.
 */
ADom.prototype.current = function () {
    return this.current_;
};



/*
 * Set snapshot to forms array
 * Return forms array.
 */
ADom.prototype.forms = function () {
    this.snapshot_ = this.document_.forms;
    return this.snapshot_;
};


/*
 * Return current snapshot.
 */
ADom.prototype.snapshot = function () {
    return this.snapshot_;
};


/*
 * find: set snapshot_ to list of elements found by name
 */
ADom.prototype.find = function (tagName) {
  this.snapshot_ = this.current_.getElementsByTagName(tagName);
};


/*
 * visit: visit each node in snapshot_ in turn.
 * Optional argument dir if specified visits in the reverse direction.
 */
ADom.prototype.visit = function (dir) {
  if (dir) {
    this.visiting_--;
  } else {
    this.visiting_++;
  }
  // wrap around 
  if (this.visiting_ == this.snapshot_.length) {
    this.visiting_ = 0;
  }
  if (this.visiting_ == -1) {
    this.visiting_ = this.snapshot_.length -1;
  }
  return this.snapshot_[this.visiting_];
};
"loaded adom.js";
