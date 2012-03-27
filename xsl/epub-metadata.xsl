<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ns="http://www.daisy.org/z3986/2005/ncx/" 
                version="1.0">

  <xsl:output method="text" indent="yes" encoding="UTF-8"/>
  
  <xsl:template match="/"> Title:<xsl:value-of select="/ns:ncx/ns:docTitle/ns:text"/> Author:<xsl:value-of select="/ns:ncx/ns:docAuthor/ns:text"/> </xsl:template>
  
</xsl:stylesheet>
