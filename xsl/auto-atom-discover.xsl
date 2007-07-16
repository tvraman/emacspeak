<?xml version="1.0"?>
<!--$Id: auto-rss-discover.xsl 4678 2007-06-25 15:14:54Z tv.raman.tv $-->

<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Display all atom links
-->
<xsl:stylesheet  xmlns:h="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="xml" indent="yes" encoding="iso8859-1"/>
  <xsl:template name="generate-atom">
    <xsl:if test="count(//link[@type='application/atom+xml'])">
      <xsl:apply-templates select="//link[@type='application/atom+xml']" mode="atom"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="h:link|link" mode="atom">
    <xsl:if test="@type='application/atom+xml'">
      <p>
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="@href"/>
          </xsl:attribute>
          <xsl:value-of select="@title"/>
        </a>
      </p>
    </xsl:if>
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
