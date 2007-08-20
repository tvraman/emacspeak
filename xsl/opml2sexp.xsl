<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Turn OPML feed into an S-expression suitable for Emacs newsticker
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output encoding="iso8859-15" method="text" indent="yes"/>
<xsl:template match="/opml">
  <xsl:text>(</xsl:text> <xsl:apply-templates select=".//outline"/> <xsl:text>)</xsl:text>
</xsl:template>  
  <xsl:template match="outline">
    <xsl:if test="@xmlUrl">
    
("<xsl:value-of select="@title"/>"
"<xsl:value-of select="@xmlUrl"/>")
    </xsl:if>
  </xsl:template>
  
</xsl:stylesheet>
<!--

Local Variables:
folded-file: t
End:
-->
