<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Produce a list of anchors.
Focus specifically on extracting links to multimedia content
that is stashed away inside object or embed tags.
Eventually try produce only one instance of each link.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/>
<!-- {nuke these elements. -->
  <xsl:include href="identity.xsl"/>
  <xsl:template match="script|meta|link"/>
<!-- } -->
<!-- {html body  -->
  <xsl:template match="/html/body">
    <body>
      <p>Multimedia links. </p>
      <ol>
        <xsl:apply-templates select="//a"/>
      </ol>
    </body>
  </xsl:template>
  <xsl:template match="a">
    <xsl:variable name="url" select="@href"/>
<xsl:variable name="suffix"
      select="substring($url,
      string-length($url) - 4)"/>
     <xsl:if test="contains($suffix,'.ra')       or contains($suffix, 'mp3')       or contains($suffix,'.rm')       or contains($suffix, 'm3u')">
      <li>
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>
      </li>
    </xsl:if>
  </xsl:template>
  <xsl:template match="object|embed">
    <xsl:variable name="url" select="@src"/>
    <xsl:if test="contains($url,'.ra')       or contains($url, 'rm')       or contains($url, 'mp3')       or contains($url, 'm3u')">
      <li>
        <xsl:element name="a">
          <xsl:attribute name="href">
            <xsl:value-of select="@src"/>
          </xsl:attribute>
          <xsl:value-of select="name()"/>
        </xsl:element>
      </li>
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
