<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Show list of anchors.
-->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  
  <xsl:output method="html" indent="yes"
  encoding="iso8859-15"/>
  <xsl:include href="object.xsl"/>
  <xsl:include href="identity.xsl"/>
  <!-- {nuke these elements. --> 
  
  <xsl:template match="script|meta|link"/>
  
  <!-- } -->
  <!-- {html body  --> 
  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="head">
    <head>
      <xsl:apply-templates select="title"/>
      <xsl:if test="string-length($base) &gt; 0">
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
      </xsl:if>
    </head>
  </xsl:template>
  <xsl:template match="body">
    <table>
      <caption>Anchors View</caption>
      <tr>
        <td><a href="#__about_this_style">About This Style</a></td>
    </tr></table>
    <ol>
      <xsl:apply-templates select="//a"/>
    </ol>
    <h2><a name="__about_this_style">About This Style</a></h2>
    <p>This style produces a list of anchors found in the document.</p>
  </xsl:template>
  
  <xsl:template match="a">
    <li>
      <xsl:copy>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates select="node()"/>
      </xsl:copy>
    </li>
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
