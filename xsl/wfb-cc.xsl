<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Extract country-code mappings from CIA World Fact Book
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:set="http://exslt.org/sets"
                version="1.0"
                exclude-result-prefixes="set">
  
  
  
  <xsl:output method="text" indent="yes" encoding="UTF-8"/>
  
  <!-- { output   -->
  
  <xsl:template match="body">
    
      
	<xsl:for-each select="//select//option" >
<xsl:if test="@value">
	  <xsl:copy-of  select="."/>
    <xsl:value-of select="@data-place-code"/>
</xsl:if>
	</xsl:for-each>
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
