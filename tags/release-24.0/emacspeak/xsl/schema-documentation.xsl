<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--Description:

Extract documentation  from an XML Schema   and display as HTML
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xsd="http://www.w3.org/2001/XMLSchema"
  version="1.0">
  <xsl:template match="/">
    <html>
      <title>Documentation From XML Schema</title>
      <body>
        <xsl:apply-templates select="//xsd:documentation"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="xsd:documentation">
    <div>
      <h2>
        Documentation For
        <xsl:choose>
          <xsl:when test="../../@name">
            <em>
              <xsl:value-of select="../../@name"/>
            </em>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="name(../..)"/>
          </xsl:otherwise>
        </xsl:choose>
      </h2>
      <p>
        <xsl:apply-templates/>
      </p>
    </div>
  </xsl:template>
</xsl:stylesheet>
