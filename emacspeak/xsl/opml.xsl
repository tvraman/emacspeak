<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
View OPML feeds as XHTML
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output encoding="iso8859-15" method="xml" indent="yes"/>
  <xsl:template match="/opml">
    <html>
      <xsl:apply-templates/>
    </html>
  </xsl:template>
  <xsl:template match="head">
    <xsl:copy-of select="."/>
  </xsl:template>
  <xsl:template match="body">
<body>
  <h1><xsl:value-of select="../head/title"/></h1>
  <ol>
<xsl:apply-templates select=".//outline"/>
  </ol>
</body>
  </xsl:template>
<xsl:template match="outline">
  <xsl:if test="@xmlUrl|@xmlurl">
<li><xsl:element name="a">
<xsl:attribute name="href">
<xsl:value-of select="@xmlUrl|@xmlurl"/>
</xsl:attribute>
<xsl:value-of select="@title|@text"/>
</xsl:element>
</li>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
<!--

Local Variables:
folded-file: t
End:
-->
