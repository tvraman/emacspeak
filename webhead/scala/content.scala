import  com.gargoylesoftware.htmlunit._

val w = new com.gargoylesoftware.htmlunit.WebClient()
w.setThrowExceptionOnScriptError(false);
val p:com.gargoylesoftware.htmlunit.html.HtmlPage  =
  w.getPage("http://www.google.com")

println(p.asText())
