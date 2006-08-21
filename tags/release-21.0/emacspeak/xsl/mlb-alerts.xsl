<!--$Id$-->
<!--Description: MLB Alerts.-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output encoding="iso8859-15"
  method="html"  indent="yes"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>Major League Baseball  Alerts </title>
      </head>
      <body>
        <h1>Major League Baseball Alerts</h1>
        
        <xsl:if test="/alerts/game">
          <ul>
            <xsl:apply-templates select="/alerts/game"/>
          </ul>
        </xsl:if>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="game">
    <li>
      <xsl:element name="a">
        <xsl:attribute name="href">
          <!--<xsl:value-of select="@game_id"/>-->
        </xsl:attribute>
        <xsl:value-of select="@text"/>
    </xsl:element></li>
  </xsl:template>
</xsl:stylesheet>

