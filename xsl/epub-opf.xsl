<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:opf="http://www.idpf.org/2007/opf"
                version="1.0">

  <xsl:output method="text" indent="yes" encoding="UTF-8"/>
  
  <xsl:template match="/">
  <xsl:if test="/opf:package/opf:metadata/dc:title"><xsl:value-of select="/opf:package/opf:metadata/dc:title"/></xsl:if>
<xsl:text>
</xsl:text>
 <xsl:if test="/opf:package/opf:metadata/dc:creator"> <xsl:value-of
 select="/opf:package/opf:metadata/dc:creator"/> </xsl:if> </xsl:template>
</xsl:stylesheet>
