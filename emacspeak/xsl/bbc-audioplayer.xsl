<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Convert BBC AudioPlay lists into direct access links
-->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="iso8859-15"
  method="html"  indent="yes"/>
<xsl:include href="identity.xsl"/>
<xsl:template match="a">
    <xsl:choose>
    <xsl:when test="substring(@href, $base)">
      rewrite
</xsl:when>
<xsl:otherwise>
<xsl:copy/>
        </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:
    
</xsl:tempalte>
</xsl:stylesheet>
<!--
Local Variables:
folded-file: t
End:
--> 
