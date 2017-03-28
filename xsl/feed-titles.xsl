<?xml version="1.0" encoding="UTF-8"?>
<!--  Return list of title/link pairs  from RSS or Atom feeds -->
<xsl:stylesheet
    version="1.0"
    xmlns:atom="http://www.w3.org/2005/Atom"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

  <xsl:output method="text" indent="no"/>
<!-- Output a list of lists 
  of the form ((title url)...) -->
  
<xsl:template match="/">
    (
    <xsl:for-each select="//item | //atom:entry">
      (
"<xsl:value-of select= "title | atom:title"/>"
"<xsl:value-of select="link | atom:link/@href"/>"
)
</xsl:for-each>
)
  </xsl:template>
</xsl:stylesheet>
