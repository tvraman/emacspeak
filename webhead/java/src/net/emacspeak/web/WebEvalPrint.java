package net.emacspeak.web;
import java.io.IOException;
import java.io.StringWriter;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
public class WebEvalPrint {
    public static void main(String args[]) 
    throws Exception {
        final WebClient webClient = new WebClient();
        final HtmlPage page = webClient.getPage("file:///tmp/test.html");
        System.out.println( page.getTitleText());
        System.out.println( page.asText());
        // printMarkup(page.getDocumentElement());
    }

    protected static void printMarkup (HtmlElement doc)
        throws IOException,
               javax.xml.transform.TransformerConfigurationException,
    javax.xml.transform.TransformerException{
        
            // set up a transformer
            TransformerFactory transfac = TransformerFactory.newInstance();
            Transformer trans = transfac.newTransformer();
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");

            // create string from xml tree
            StringWriter sw = new StringWriter();
            StreamResult result = new StreamResult(sw);
            DOMSource source = new DOMSource(doc);
            trans.transform(source, result);
            String xmlString = sw.toString();

            // print xml
            System.out.println("Here's the xml:\n\n" + xmlString);
    }
}
