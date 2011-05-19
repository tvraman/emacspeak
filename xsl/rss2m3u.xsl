<?xml version="1.0" encoding="UTF-8"?>
<!-- Turn RSS  media feeds into M3U playlists. -->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

  <xsl:output method="text" indent="no"/>
  
  
  
  
  <xsl:template match="/">
    <xsl:for-each select="//enclosure">
<xsl:value-of select="@url"/>
<xsl:text>
</xsl:text>
</xsl:for-each>
  </xsl:template>

  
  
</xsl:stylesheet>
