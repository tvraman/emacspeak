package net.emacspeak.web;
/**
 * @file   Client.java
 * @author <a href="tv.raman.tv@gmail.com">T.V Raman </a>
 * @date   Fri Oct 9 14:30:21 2009
 * 
 * @Description:  Implements a headless web client.
 */

//< Imports:


import java.util.HashMap;
import java.io.IOException;
import java.io.StringWriter;

import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

//>
/**
 * @class Client implements an interactive command-loop that:
 * Encapsulates a headless browser,
 *  Accepts commands on standard-input,
 * Returns results on standard-output.
 */

public class Client {
//<Class members

    private final WebClient _client;
    private  HtmlPage _page;
//>
//<declare arg counts for commands.

    /**
     * Hashmap <code>cliArgs</code> holds mapping from CLI commands
     * to implementation methods.
     *
     */
    private static HashMap <String,Integer>cliArgs;
    static {
        cliArgs = new HashMap<String, Integer>();
        cliArgs.put("/open", 1);
        cliArgs.put("/content", 0);
cliArgs.put("/xml", 0);
    }

    //>
//<private helper argCheck

    /**
     * Check if command called with right number of arguments.
     *
     * @param command a <code>String</code> command name
     * @param argCount an <code>int</code> arg count
     * @return a <code>boolean</code> true if right number of arguments.
     */
    private  boolean argCheck(final String command, final int argCount) {
        Integer count = (Integer) cliArgs.get(command);
        if (count != null
            && argCount != count.intValue()) {
            System.err.println(command
                     + " expects "
                     + count
                     + " arguments, but got "
                     + argCount);
            return false;
        } else {
            return true;
        }
    }

    //>
//<ArgCount For commands 
//>
//<Constructor:

    /** 
     * Constructor: Initialize WebClient
     * 
     */

    public Client () {
        _client = new WebClient();
    }

//>
    //< main:
    /** 
     *  
     * 
     * @param args String Args[] (not used)
     */
    public static void main(String args[]) 
        throws Exception {
        Client c = new Client();
        final HtmlPage page = c.open("file:./src/test/resources/00-test.html"); 
        c.content();
        c.xml();
    }
//>
//<open


public HtmlPage open (String location)
    throws IOException {
    return  (_page = this._client.getPage(location));
}
//>
    //< content

    public  void content () {
        System.out.println( this._page.asText());
    }

    //>
    //< xml

    public  void xml () {
        HtmlElement html = this._page.getFirstByXPath("/html");
        System.out.println(html.asXml());
    }

    //>
} // class Client

//<End Of File:

// local variables:
// folded-file: t
// end:
//>
