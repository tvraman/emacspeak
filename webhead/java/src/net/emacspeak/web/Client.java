package net.emacspeak.web;
//< Imports:



import java.io.IOException;
import java.io.StringWriter;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

//>

public class Client {

    //< main:

    public static void main(String args[]) 
        throws Exception {
        final WebClient webClient = new WebClient();
        final HtmlPage page = webClient.getPage("file:./src/test/resources/00-test.html");
        writeContent(page);
        writeXml(page);
    }

    //>
    //< writeContent

    public static void writeContent (HtmlPage page) {
        System.out.println( page.asText());
    }

    //>
    //< writeXml

    public static void writeXml (HtmlPage page) {
        HtmlElement body = page.getFirstByXPath("/html");
        System.out.println(body.asXml());
    }
    //>
        
} // class Client

//<End Of File:

// local variables:
// folded-file: t
// end:
//>
