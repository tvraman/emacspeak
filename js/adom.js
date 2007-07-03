//$Id$
// <Class ADom

/*
 * ADOM: Holds a proxy to a DOM
 * Provides convenience methods for obtaining custom views
 * Constructor takes  the   document to view as argument
 */

function ADom (document) {
  this.document_ = document;
  document.adom = this;
  this.root_ = document.documentElement;
  this.current_ = document.documentElement;
  this.view_ = null;
}

// >
// < Navigators:

/*
 * Reset view.
 * Resets current to point at the root.
 * @return {node} current node.
 */
ADom.prototype.reset = function () {
  this.root_ = this.document_.documentElement;
  return this.current_ = this.root_;
};

/*
 * next: Move to next sibling.
 * @return {node} current node.
 */
ADom.prototype.next = function () {
  return this.current_ = this.current_.nextSibling;
};

/*
 * previous: Move to previous sibling.
 * @return {node} current node.
 */
ADom.prototype.previous = function() {
  return this.current_ = this.current_.previousSibling;
};

/*
 * up: Move to parent.
 * @return {node} current node.
 */
ADom.prototype.up = function () {
  return this.current_ = this.parentNode;
};

/*
 * down: Move to first child
 * @return {node} current node.
 */
ADom.prototype.down = function () {
  return this.current_ = this.current_.firstChild;
};

/*
 * first: Move to first sibling
 * @return {node} current node.
 */
ADom.prototype.first = function () {
  return this.current_ = this.current_.parentNode.firstChild;
};

/*
 * last: Move to last sibling.
 * @return {node} current node.
 */
ADom.prototype.last = function () {
  return this.current_  = this.current_.parentNode.lastChild;
};

/*
 * Move to  document body
 * @return {node} current node.
 */
ADom.prototype.body = function () {
  return this.current_ =  this.document_.body;
};

// >
// <Summarizers:

/*
 * Return HTML for current node.
 *@Return {string}; HTML
 */
ADom.prototype.html = function () {
  var html ='<base href=\"' + this.document_.baseURI +  '\"/>';
  html +='<' + this.current_.tagName;
  var map = this.current_.attributes;
  if (map  instanceof NamedNodeMap) {
    for (var i = 0; i < map.length; i++) {
      html += ' ' + map[i].name + '=';
      html += '\"' +map[i].value + '\"';
    }
  }
  if (this.current_.childNodes.length === 0) {
    return html += '/>';
  } else {
    html += '>' + this.current_.innerHTML;
    html += '</' + this.current_.tagName +'>';
    return html;
  }
};

/*
 * summarize: Summarize current node.
 * @Return {string};
 */
ADom.prototype.summarize = function () {
  var summary = this.current_.tagName +' ';
  summary += 'has ' + this.current_.childNodes.length + 'children ';
  summary += ' with ' + this.current_.innerHTML.length + ' bytes of content.';
  return summary;
};

/*
 * title: return document title
 * @Return  {string}
 */
ADom.prototype.title = function () {
  return this.document_.title;
};

/*
 * url: Return base URL of document.
 * @return {String} url
 */
ADom.prototype.url = function () {
  return this.document_.baseURI;
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

// >
// <RingBuffer:

/*
 *  Implements iteration.
 */
RingBuffer = function (list) {
  this.list_ = list;
  this.index_ = -1;
  this.len_ = list.length;
};
                                                                                                                                                                                                                                                          
RingBuffer.prototype.next = function () {
  if (this.index_ == this.len_ -1) {
    this.index_ = -1;
  }
  this.index_++;
  return this.list_.item(this.index_);
};
                                                                                                                                                                                                                                                                                          
RingBuffer.prototype.previous = function () {
  if (this.index_ === -1 || this.index_ === 0) {
    this.index_ = this.len_;
  }
  this.index_--;
  return this.list_.item(this.index_);
};

// >
// <XPathRingBuffer:

/*
 *  Implements RingBuffer.
 */
XPathRingBuffer = function (nodes) {
  this.list_ = nodes;
  this.index_ = -1;
  this.len_ = nodes.snapshotLength;
};
                                                                                                                                                                                                                                                                                                                  
XPathRingBuffer.prototype.next = function () {
  if (this.index_ == this.len_ -1) {
    this.index_ = -1;
  }
  this.index_++;
  return this.list_.snapshotItem(this.index_);
};
                                                                                                                                                                                                                                                                                                                                                  
XPathRingBuffer.prototype.previous = function () {
  if (this.index_ === -1 || this.index_ === 0) {
    this.index_ = this.len_;
  }
  this.index_--;
  return this.list_.snapshotItem(this.index_);
};

// >
// <XPath 

/*
 * filter: Apply XPath selector to create a filtered view.
 * @return {RingBuffer} of selected nodes suitable for use by visit()
 */

ADom.prototype.filter = function (xpath) {
  var start = this.current_ || this.root_;
  var snap   =
  this.document_.evaluate(xpath,
                          start, null,
                          XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null);
  return this.view_ = new XPathRingBuffer(snap);
};

// >
// <Viewers And Visitors:

/*
 * Set view to forms array
 * Return forms array.
 */
ADom.prototype.forms = function () {
  this.view_ = new RingBuffer(this.document_.forms);
  return this.view_;
};
/*
 * find: set view_ to RingBuffer of elements found by name
 */
ADom.prototype.find = function (tagName) {
  var start = this.current_ || this.root_;
  return this.view_ = new RingBuffer(start.getElementsByTagName(tagName));
};

/*
 * visit: visit each node in view_ in turn.
 * Optional argument dir if specified visits in the reverse direction.
 */
ADom.prototype.visit = function (dir) {
  if (dir) {
      this.current_ = this.view_.previous();
  } else  {
      this.current_ = this.view_.next();
  }
  // skip empties
  if (this.current_.childNodes.length === 0 && this.current_.attributes.length  === 0) {
      return this.visit(dir);
  } else {
      return this.current_;
  }
};

// >
// < Eventing:

// >
// < A11y Reflection:

// >
// <repl hookup

/*
 * Update adom pointer in repl to point to current document.
 * @return {ADom}
 */
repl.updateADom = function ()  {
  if (repl.adom == undefined || content.document.adom == undefined) {
    repl.adom = new ADom(content.document);
    return repl.adom;
  }
  if (repl.adom.document_ != content.document) {
    repl.adom = content.document.adom;
    return repl.adom;
  }
  return repl.adom;
};

// >
// <end of file

"loaded adom.js";

// local variables:
// folded-file: t
// end:

// >
