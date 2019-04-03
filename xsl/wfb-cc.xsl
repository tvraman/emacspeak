<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Extract country-code mappings from CIA World Fact Book
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0" >

  <xsl:output method="text" indent="yes" encoding="UTF-8"/>

  <!-- { output   -->

  <xsl:template match="/">

    (<xsl:apply-templates select="//option"/>)
  </xsl:template>

  <xsl:template select="option">
    <xsl:if test="@data-place-code">
      ("<xsl:copy-of  select='.'/>"
      "<xsl:value-of select='./@data-place-code'/>")
    </xsl:if>
  </xsl:template>
  <!-- } -->
</xsl:stylesheet>
<!--
    Local Variables:
    mode: xae
    sgml-indent-step: 2
    sgml-indent-data: t
    sgml-set-face: nil
    sgml-insert-missing-element-comment: nil
    folded-file: t
    End:
    -->
