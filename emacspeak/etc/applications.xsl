<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  
  <xsl:output method="html" media-type="text/html"/>
  <xsl:strip-space elements="*"/>
  

  

  <xsl:template match="applications">
    <html>
      <xsl:apply-templates select="preamble"/>
      <body>
        <xsl:apply-templates select="introduction"/>
        <p>
          As of the last update, there are a total of
          <em><xsl:value-of select="count(//application)"/></em>
          speech-enabled applications on the Emacspeak audio desktop.
        </p>
        <table>
          <caption>
            <xsl:value-of select="@caption"/>
          </caption>
          <xsl:apply-templates select="category" />
        </table>
        <xsl:apply-templates select="postamble"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="preamble">
    <head>
      <link rel="stylesheet"
            href= "http://www.w3.org/StyleSheets/Core/Chocolate" type="text/css">
      </link>
      <title>
        <xsl:value-of select="@title"/>
      </title>
    </head>
  </xsl:template>

  <xsl:template match="introduction">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="postamble">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="category">
    <tr>
      <td colspan="3">
        <xsl:value-of select="@name"/>
        (<xsl:value-of select="count(./application)"/>)
    </td></tr>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="application">
    <tr>
      <td><xsl:value-of select="position()"/></td>
      <td><xsl:value-of select="@name"/></td>
      <td> <xsl:apply-templates/></td>
    </tr>
  </xsl:template>
  <xsl:template match="*|@*">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
