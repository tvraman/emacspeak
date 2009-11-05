/*
 * $Id$
 * A simple script for dumping page contents given a URL
 * */
package net.emacspeak.web

import  com.gargoylesoftware.htmlunit._
import  com.gargoylesoftware.htmlunit.html._

object Content {
  val w = new WebClient(BrowserVersion.FIREFOX_3)
  w.setThrowExceptionOnScriptError(false)

/*
 * * content: Return page content as text
 *  @param: url --- URL of page to open
 * */

  def content (url : String) {
    val p:HtmlPage  = this.w.getPage(url)
    println(p.asText())
  }

/*
 * * scala Content <url_string>
 * */

  def main(args: Array[String]) = content(args(0))
}
