<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Count matches for  by param locator.
Param locator is an XPath expression.
Param path is the same expression, but quoted so it can be
shown in the output.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
<!--
  <xsl:output method="text" indent="yes" encoding="iso8859-15"/>
-->
  <xsl:param name="locator"/>
  <xsl:param name="path"/>
  
<!-- { html   -->
<!--add base uri if available. -->
  
  <xsl:template match="/">
      <xsl:value-of select="$locator"/>
<xsl:text>
    </xsl:text>
  </xsl:template>
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
