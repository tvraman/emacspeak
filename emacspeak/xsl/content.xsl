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
      <xsl:apply-templates 
                           select = "//h1|//h2|//h3|//p|//ul|//ol|//dl|//li|//blockquote"/>
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
