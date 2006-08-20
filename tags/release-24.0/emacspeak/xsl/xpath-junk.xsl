<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Junk  content that matches  by param locator.
Param locator is an XPath expression.
Param path is the same expression, but quoted so it can be
shown in the output.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:set="http://exslt.org/sets"
  version="1.0">
  <xsl:param name="locator"/>
  <xsl:param name="path"/>
  <xsl:param name="base"/>
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/>
  <xsl:variable name="j" select="$locator"/>
  <xsl:template match="*|@*" mode="copy" >
    <xsl:if test="not(set:intersection(., $j))">
      <xsl:copy>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates select="node()" mode="copy"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>
  <!-- { html   -->
  <!--add base uri if available. -->
  <xsl:template match="head">
    <head>
      <xsl:apply-templates select="title"/>
      <xsl:if test="string-length($base) &gt; 0">
        <base>
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </base>
      </xsl:if>
    </head>
  </xsl:template>
  <xsl:template match="body">
    <body>
      <xsl:apply-templates  mode="copy"/>
      <h2>
        Skipped Nodes  Matching   <xsl:value-of select="$path"/>
      </h2>
      <p>
        Found <xsl:value-of select="count($locator)"/>
        matching
        elements to skip in 
        in 
        <xsl:element name="a">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
          document 
        </xsl:element>
        .
      </p>
    </body>
  </xsl:template>
  <xsl:include href="object.xsl"/>
  <xsl:include href="identity.xsl"/>
  <!-- nuke these -->
  <xsl:template match="//script|//meta//iframe"/>
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
