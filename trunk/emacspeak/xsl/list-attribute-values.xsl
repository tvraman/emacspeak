<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Display all values of a specified attribute.
Hard-wired to display attribute class by default.
Param attr  specifies the attribute to list.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" indent="yes" encoding="iso8859-1"/>
  <xsl:param name="attr"
    select="'class'"/>
  <xsl:param name="selector">//@<xsl:value-of select="$attr"/></xsl:param>
    
<!-- { html body  -->
  <xsl:template match="//script|//head"/>
  <xsl:template match="/html/body">

      <xsl:for-each select="//@class">
<xsl:value-of select="."/><xsl:text>
</xsl:text></xsl:for-each>
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
