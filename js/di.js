//$Id:$
/*
 * Description: DomInspector helpers
 */


/*
 * Show attributes of a DOM node along with NameSpace information
 */
repl.showAttributes = function(node) {
  var map = node.attributes;
  if (map  instanceof NamedNodeMap) {
    for (i = 0; i < map.length; i++) {
      repl.print (map[i].name +' = ' + map[i].value, false);
      if (map[i].namespaceURI) {
        repl.print("\tNamespace: " + map[i].namespaceURI);
      } else {
        repl.print("");
      }
    }
  } else {
    repl.print (node + " has no attributes.");
  }
};
 

/*
 * Function to attach to window.onerror
 */
repl.showErrors = function (m, u, l) {
    repl.print("Message: " +m + "URL: " +u +"Line: " + l);
};


/*
 * Show properties of an object.
 */
repl.showProps = function (object) {
    for (var name in object) {
      repl.print(name);
    }
};
