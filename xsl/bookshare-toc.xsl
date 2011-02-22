<?xml version="1.0" encoding="UTF-8"?>
<!-- Extract light-weight table of contents for Bookshare Books:
AKA   avoid the bloatware that is Daisy.
-->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:dtb="http://www.daisy.org/z3986/2005/dtbook/">

  <xsl:output method="html" indent="no"/>
  <xsl:param name="base" />
  
  
  <xsl:template match="dtb:dtbook">
    <html>
      <head>
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
        <title>
          <xsl:value-of select="./dtb:book/dtb:frontmatter/dtb:doctitle"/>
        </title>
      </head>
      <body>
        <h1>
        <xsl:value-of select="dtb:book/dtb:frontmatter/dtb:doctitle"/>
        </h1>
        <ol>
          <xsl:for-each select="//dtb:level2">
            <li>
              <a>
                <xsl:attribute name="href">
                <xsl:value-of select="concat($base,'#',@id)"/></xsl:attribute>
                <xsl:value-of select="dtb:h2"/>
              </a>
            </li>
          </xsl:for-each>
        </ol>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
