<?xml version="1.0" encoding="UTF-8"?>
<!-- NPR Listings -->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >

  <xsl:output method="html" indent="no"/>
  
  
  
  
  <xsl:template match="/">
    <html>
      <head>
        <title>NPR 
        <xsl:value-of select="/list/title"/></title>
      </head>
      <body>
        <ol>
          <xsl:apply-templates select="//item"/>
        </ol>
      </body>
    </html>
  </xsl:template>

  <xsl:template  match="item">
    <li>
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="@id"/>
        </xsl:attribute>
        <xsl:value-of select="title"/>
      </a>
      <xsl:value-of select="additionalInfo"/>
      <strong>Story Counts:</strong>
      Today: <xsl:value-of select="@storycounttoday"/>
      Month: <xsl:value-of select="@storycountmonth"/>
      Total: <xsl:value-of select="@storycountall"/>
    </li>
  </xsl:template>
  
</xsl:stylesheet>
