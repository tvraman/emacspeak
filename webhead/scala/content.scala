import  com.gargoylesoftware.htmlunit._
import  com.gargoylesoftware.htmlunit.html._

object Content {
  val w = new WebClient()
  w.setThrowExceptionOnScriptError(false);

  def content (url : String) {
    val p:HtmlPage  = this.w.getPage(url)
    println(url)
    println(p.asText())
  }

  def main(args: Array[String]) = args.map(content(_))
}
