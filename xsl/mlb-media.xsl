<!--$Id$-->
<!--Description: MLB Media .-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output encoding="UTF-8"
  method="html"  indent="yes"/>
  <xsl:template match="/">
    <html>
      <head>
        <title>Major League Baseball  Game Highlights </title>
      </head>
      <body>
        <h1>Major League Baseball Highlights</h1>
        <ol>
        <xsl:for-each select="/highlights/media">
<li>
          <a>
<xsl:attribute name="href">
  <xsl:value-of select="url[1]"/>
</xsl:attribute>
<xsl:value-of select="bigblurb"/>
          </a>
</li>
        </xsl:for-each>
        </ol>
        
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>

