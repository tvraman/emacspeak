<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Extract content as specified by param locator.
Param locator is an XPath expression.
Param path is the same expression, but quoted so it can be
shown in the output.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"
              encoding="iso8859-15"/>
  <xsl:param name="locator"/>
<xsl:param name="path"/>
  <xsl:param name="base"/>
  <xsl:include href="identity.xsl"/>
<!-- { html body  -->
<!-- nuke these -->
  <xsl:template match="//script|//meta"/>
<!--add base uri if available. -->
  <xsl:template match="/html/head">
    <head>
      <xsl:element name="base">
        <xsl:attribute name="href">
          <xsl:value-of select="$base"/>
        </xsl:attribute>
      </xsl:element>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
  <xsl:template match="/html/body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
<h2> Nodes Matching   <xsl:value-of select="$path"/></h2>
      <p>Found <xsl:value-of select="count($locator)"/> matching elements.</p>
      <xsl:for-each select="$locator">
        <xsl:element name="{name()}">
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates/>
        </xsl:element><br/>
      </xsl:for-each>
    </xsl:element>
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
