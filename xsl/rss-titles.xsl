<?xml version="1.0" encoding="UTF-8"?>
<!--  Return list of title/link pairs  from RSS -->
<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

  <xsl:output method="text" indent="no"/>
  
  <xsl:template match="/">
    (
    <xsl:for-each select="//item">
      (
"<xsl:value-of select="title"/>"
"<xsl:value-of select="link"/>"
)
</xsl:for-each>
)
  </xsl:template>

  
  
</xsl:stylesheet>
