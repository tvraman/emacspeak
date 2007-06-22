//$Id:$
// Description: DomInspector helpers

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
}
  
repl.showErrors = function (m, u, l) {
    repl.print("Message: " +m
               + "URL: " +u
               +"Line: " + l);
}

repl.showProps = function (object) {
    for (var name in object) {
      repl.print(name);
    }
  }
      
      
