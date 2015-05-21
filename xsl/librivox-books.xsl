<?xml version="1.0" encoding="UTF-8"?>
<!-- Librivox Books -->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

  <xsl:output method="html" indent="no"/>
  
  <xsl:template match="/xml/books">
    <html>
      <head>
        <title>Librivox
        </title>
      </head>
      <body>
        <ol>
          <xsl:applied-templates select="book"/>
        </ol>
      </body>
    </html>
  </xsl:template>
<xsl:template match="book">
  <li>
<strong><xsl:value-of select="title"/></strong>
by <xsl:apply-templates select="authors"/><br/>
<a>
  <xsl:attribute name="href">
<xsl:value-of select="url_rss"/>
  </xsl:attribute> Stream Book</a>
<a>
  <xsl:attribute name="href">
<xsl:value-of select="url_zip"/>
  </xsl:attribute> Download Book</a>
  </li>
</xsl:template>

  <xsl:template  match="item">
  </xsl:template>
  
</xsl:stylesheet>
