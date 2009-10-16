package net.emacspeak.web;
//< Imports:



import java.io.IOException;
import java.io.StringWriter;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

//>
/**
 * @file   Client.java
 * @author T.V Raman <rtv.raman.tv@gmail.com>
 * @date   Fri Oct 9 14:30:21 2009
 * 
 * @brief  Implements a headless web client.
 * class Client implements an interactive command-loop that:
 *  Accepts commands on standard-input,
 * Returns results on standard-output.
 */

public class Client {


    /** 
     * Constructor: Initialize WebClient
     * 
     * 
     * @return  Newly constructed Client
     */

    public Client () {
        _client = new WebClient();
    }

    //< main:
    /** 
     *  
     * 
     * @param args String Args[] (not used)
     */
    public static void main(String args[]) 
        throws Exception {
        Client c = new Client();
        final HtmlPage page = c._client.getPage("file:./src/test/resources/00-test.html"); 
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

//<Class members

    private final WebClient _client;
//>
} // class Client

//<End Of File:

// local variables:
// folded-file: t
// end:
//>
