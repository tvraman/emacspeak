<?xml version="1.0"?>
<!--$Id$-->

<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Display all RSS links
-->
<xsl:stylesheet  xmlns:h="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="xml" indent="yes" encoding="iso8859-1"/>
  <xsl:template name="generate-rss">
    <xsl:if test="count(//link[@type='application/rss+xml'])">
      <xsl:apply-templates select="//link[@type='application/rss+xml']" mode="rss"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="h:link|link" mode="rss">
    <xsl:variable name="amphetadesk">http://127.0.0.1:8888/my_channels.html</xsl:variable>
    <xsl:if test="@type='application/rss+xml'">
      <p>
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="@href"/>
          </xsl:attribute>
          <xsl:value-of select="@title"/>
        </a>
        <form action="{$amphetadesk}" method="POST">
          <input type="hidden" name="add_url" value="{@href}"/>
          <input type="submit" name="submit" value="Add to AmphetaDesk" />
        </form>
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
