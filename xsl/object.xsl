<?xml version="1.0" encoding="UTF-8"?>
<!--$Id$-->

<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Transform HTML Object element into an anchor usable in W3.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output encoding="UTF-8"
  method="html"  indent="yes"/>
<xsl:template match="td/@title">
<xsl:text> </xsl:text>
 <xsl:value-of select="."/> 
<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="button">
  <input>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
  </input>
</xsl:template>
<xsl:template match="a/div">
  <span>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
</span>
</xsl:template>
  <xsl:template match="object">
    <xsl:for-each select="param[@name='src' or @name='SRC']">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="@value"/>
        </xsl:attribute>
        Object Link
      </a>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="frame">
    <a>
      <xsl:attribute name="href">
        <xsl:value-of select="@src"/>
      </xsl:attribute>
      Frame: <xsl:value-of select="@name"/>
      <br/>
    </a>
  </xsl:template>
  
  <xsl:template match="embed">
    <xsl:for-each select="@src|@SRC">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="."/>
        </xsl:attribute>
        Embed Link
      </a>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
