<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:output method="html" indent="yes"/>
  
  <xsl:include href="identity.xsl"/>
  <!-- { html body  --> 

  <xsl:template match="//script|meta|link"/>
  <xsl:template match="html/body">
    <xsl:element name="body">
<p><strong>
<a href="#__about_this_style">Contents Revealed</a>
        </strong></p>
      <xsl:apply-templates 
                           select =
        "//div|//h1|//h2|//h3|//p|//ul|//ol|//dl"/>
<h2><a name="__about_this_style">About This Style</a></h2>
<p>
This style extracts content from a layout-rich WWW page.
      </p>
    </xsl:element>
  </xsl:template>

  <!-- } -->
</xsl:stylesheet>

<!--
Local Variables:
mode: xae
sgml-indent-step: 2
sgml-indent-data: t
sgml-set-face: nil
sgml-insert-missing-element-comment: nil
folded-file: t
End:
--> 
