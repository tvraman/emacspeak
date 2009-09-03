<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ns="http://www.daisy.org/z3986/2005/ncx/" 
                version="1.0">

  <xsl:output method="html" indent="yes" encoding="UTF-8"/>
  <xsl:template match="/">
    <html>
      <head>
        
        
        <title>
          <xsl:value-of select="/ns:ncx/ns:docTitle/ns:text"/>
        </title>
      </head>
      <body>
        <h1><xsl:value-of select="/ns:ncx/ns:docTitle/ns:text"/></h1>
        <xsl:apply-templates select="/ns:ncx/ns:navMap"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="ns:navMap">
    <h2>Table Of Contents</h2>
    <ol>
      <xsl:apply-templates select="ns:navPoint"/>
    </ol>
  </xsl:template>

  <xsl:template match="ns:navPoint">
    <li>
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="./ns:content/@src"/>
        </xsl:attribute>
        <xsl:value-of select="./ns:navLabel/ns:text"/>
      </a>
    </li>
  </xsl:template>
</xsl:stylesheet>
