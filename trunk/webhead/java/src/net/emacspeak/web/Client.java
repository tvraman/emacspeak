package net.emacspeak.web;

// ^javax?\.
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;

import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.BrowserVersion;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.StringTokenizer;

/**
 * @file   Client.java
 * @author <a href="tv.raman.tv@gmail.com">T.V Raman </a>
 * @date   Fri Oct 9 14:30:21 2009
 * 
 * @Description:  Implements a headless web client.
 */

//< Imports:


// ^javax?\.


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
        _client = new WebClient(com.gargoylesoftware.htmlunit.BrowserVersion.FIREFOX_3);
        _client.setThrowExceptionOnScriptError(false);
    }

    //>
    //<getWebClient 

    /**
     * Returns handle to stored WebClient
     *
     * @return a <code>WebClient</code> value
     */
    public WebClient getWebClient () {
        return this._client;
    }

    //>
    //<getPageWebClient 

    public HtmlPage getPage () {
        return this._page;
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
        final HtmlPage page = c.open("http://www.google.com");
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
        try {
            System.out.println( this._page.asText());
        } catch (Exception e) {
            System.err.println(e);
        }
    }

    //>
    //< xml

    public  void xml () {
        HtmlElement html = this._page.getFirstByXPath("/html");
        try {
            System.out.println(html.asXml());
        } catch( Exception e) {
            System.err.println(e);
        }
    }

    //>
    //<method: dispatch

    /**
     * Tokenize string and dispatch to appropriate command.
     *
     * @param command a <code>String</code> value
     */
    public void dispatch (final String command)
    throws IOException{
        StringTokenizer tokenizer = new StringTokenizer(command, " ");
        String[] words = new String[tokenizer.countTokens()];
        if (tokenizer.countTokens() == 0) {
            System.err.println("Empty command.");
            return;
        }
        int i = 0;
        while (tokenizer.hasMoreTokens()) {
            words[i++ ] = tokenizer.nextToken();
        }
        String c = words[0];
        if (!argCheck(c, words.length - 1)) {
            //wrong number of args --return.
            System.err.println( "Command was> " + command);
            return;
        }
        if (c.equals("/content")) {
            content();
        } else if (c.equals("/open")) {
            open(words[1]);
        }  else {
            System.err.println("Unknown command " + c);
        }
    }

    //>
    //< method: commandLoop

    /**
     * Read and execute commands from specified stream.
     *
     * @param in an <code>InputStream</code> stream to get commands from
     */
    public void commandLoop (final InputStream in) {
        String command;
        BufferedReader input = new BufferedReader(new InputStreamReader(in));
        try {
            while ((command = input.readLine()) != null) {
                dispatch(command);
            }
        } catch (IOException ioE) {
            System.err.println("Error reading commands from input stream.");
        }
    }

    //>
} // class Client
//<End Of File:

// local variables:
// folded-file: t
// end:
//>
