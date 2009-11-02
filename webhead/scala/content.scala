import  com.gargoylesoftware.htmlunit._
object Client {
  def main(args: Array[String]) = {
    val w = new com.gargoylesoftware.htmlunit.WebClient()
    w.setThrowExceptionOnScriptError(false);
    val p:com.gargoylesoftware.htmlunit.html.HtmlPage  =
      w.getPage(args(0))
    println(p.asText())
  }
}
