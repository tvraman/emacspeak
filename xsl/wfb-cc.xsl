<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2022,   All Rights Reserved.
License: GPL
Extract country-code mappings from CIA World Fact Book as an alist
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" >
  <xsl:output method="text"  encoding="UTF-8"/>
  <xsl:template match="/">
    (<xsl:apply-templates select="//option"/>)
  </xsl:template>

  <xsl:template match="option">
    <xsl:if test="string-length(@data-place-code) &gt; 0">
      ("<xsl:value-of  select='normalize-space(.)'/>"
      .
      "<xsl:value-of select='@data-place-code'/>")
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
<!--
    Local Variables:
    mode: nxml
    sgml-indent-step: 2
    sgml-indent-data: t
    sgml-set-face: nil
    sgml-insert-missing-element-comment: nil
    folded-file: t
    End:
    -->
