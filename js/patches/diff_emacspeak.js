Index: js/emacspeak.js
===================================================================
--- js/emacspeak.js	(revision 6157)
+++ js/emacspeak.js	(working copy)
@@ -37,17 +37,23 @@
  */
 
 Emacspeak.prototype.init = function() {
+  function setupUpdateADom(){
+    if (repl.setupUpdateADomRan) return;
+    if(window.gBrowser){
+      gBrowser.addEventListener('load', repl.updateADom, true);
+      repl.setupUpdateADomRan = true;
+    }
+  }
   try {
     var js = 'file://localhost' + this.path_ + 'js/';
     repl.load(js + 'di.js');
     repl.load(js + 'adom.js');
-    window.addEventListener(
-                        "load",
-                        function () {
-                                     gBrowser.addEventListener('load', repl.updateADom, true);
-                                     },
-                        false);
 
+    // for if the window hasn't loaded yet:
+    window.addEventListener("load", setupUpdateADom, false);
+    // for if the window has already loaded:
+    setupUpdateADom();
+
     this.say('Emacspeak Enabled Firefox');
   } catch (err) {
     repl.print('Error during init ' + err);
@@ -80,12 +86,22 @@
  @param {string} text to speak.
  * @return void
  */
+//Emacspeak.prototype.say = function(text) {
+//  var url = this.url_ + 'say?' +encodeURIComponent(text);
+//  var xhr  = new XMLHttpRequest();
+//  xhr.open('GET', url,true);
+//  // xhr.onreadystatechange = function (data) {repl.print(data);};
+//  xhr.send(null);
+//};
+
+// Get method in HTTPSpeaker.py is currently broken, use post instead
 Emacspeak.prototype.say = function(text) {
-  var url = this.url_ + 'say?' +encodeURIComponent(text);
-  var xhr  = new XMLHttpRequest();
-  xhr.open('GET', url,true);
+  var url = this.url_;
+  var xhr = new XMLHttpRequest();
+  xhr.open('POST', url, true);
+  xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
   // xhr.onreadystatechange = function (data) {repl.print(data);};
-  xhr.send(null);
+  xhr.send('speak:' + text);
 };
 
 repl.print('Loaded emacspeak.js');
