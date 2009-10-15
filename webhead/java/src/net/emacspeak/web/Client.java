package net.emacspeak.web;
//< Imports:

import java.io.IOException;
import java.io.StringWriter;



import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

//>

public class Client {

// < main:

    public static void main(String args[]) 
    throws Exception {
        final WebClient webClient = new WebClient();
        final HtmlPage page = webClient.getPage("file:///tmp/test.html");
        //System.out.println( page.getTitleText());
        System.out.println( page.asText());
        // printMarkup(page.getDocumentElement());
    }

// >

    
        
               
    
}
