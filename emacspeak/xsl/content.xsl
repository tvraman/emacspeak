<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:output method="html" indent="yes"/>
  
  <!-- {identity default  -->   
  <xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>



  <!-- } -->
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
<!-- {end of file  --> 
<!--
local variables:
folded-file: t
end:
 <!-- } -->
