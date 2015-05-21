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
        <h1>Librivox Results</h1>
        <xsl:apply-templates select="book"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="authors">
    <ul>
      <xsl:for-each select="author">
        <li><xsl:value-of select="last_name"/>
        <xsl:value-of select="first_name"/>,
        <xsl:value-of select="dob"/>--<xsl:value-of select="dod"/>)
        </li>
      </xsl:for-each>
    </ul>
  </xsl:template>
  <xsl:template match="book">
    <h2><xsl:value-of select="title"/></h2>
    <xsl:apply-templates select="authors"/>
    <table><tr>
      <td><a>
        <xsl:attribute name="href">
          <xsl:value-of select="url_rss"/>
      </xsl:attribute> RSS: Stream Book</a></td>
      <td><a>
        <xsl:attribute name="href">
          <xsl:value-of select="url_zip"/>
      </xsl:attribute> Download Book</a></td>
    </tr></table>
    <p><xsl:value-of select="description"/></p>
  </xsl:template>




</xsl:stylesheet>
