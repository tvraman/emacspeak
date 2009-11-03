import  com.gargoylesoftware.htmlunit._

object Content {
  val w = new com.gargoylesoftware.htmlunit.WebClient()
  w.setThrowExceptionOnScriptError(false);
  def content (url : String) {
    val p:com.gargoylesoftware.htmlunit.html.HtmlPage  =
      this.w.getPage(url)
    println(url)
    println(p.asText())
  }

  def main(args: Array[String]) = {
    args.map(content(_))
  }
}
