Index: js/adom.js
===================================================================
--- js/adom.js	(revision 6157)
+++ js/adom.js	(working copy)
@@ -484,7 +484,7 @@
  * Update adom pointer in repl to point to current document.
  * @return {ADom}
  */
-repl.prototype.updateADom = function ()  {
+repl.updateADom = function ()  {
     if (content.document.adom == undefined) {
         // constructor caches adom in content.document
         repl.adom = new ADom(content.document);
