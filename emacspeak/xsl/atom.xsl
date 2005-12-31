<?xml version="1.0"?>
<!--
    Author: T. V. Raman <raman@cs.cornell.edu>
    Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
    License: GPL
    View an Atom feed as clean HTML
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:atom="http://purl.org/atom/ns#"
                xmlns:xhtml="http://www.w3.org/1999/xhtml"
                version="1.0">
  <xsl:output encoding="iso8859-15" method="xml" indent="yes"/>
  
  <xsl:template match="atom:feed">
    <html>
      <head>
        <title>
          <xsl:apply-templates select="atom:title"/> 
        </title>
      </head>
      <body>
        <h1><xsl:value-of select="atom:title"/> <xsl:apply-templates select="atom:link[@rel='service.post']"/></h1>
        <xsl:apply-templates select="atom:entry"/>
        <h2><xsl:apply-templates select="atom:link[@rel='alternate']"/></h2>
        <p>
          <xsl:apply-templates select="atom:tagline"/>
          <xsl:apply-templates select="atom:author"/>
        </p>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="atom:entry">
    <h2>
      <xsl:apply-templates select="atom:title"/>
      <xsl:apply-templates
          select="atom:link[@rel='service.edit']"/>
    </h2>
    <xsl:apply-templates select="atom:summary|atom:content"/>
    <xsl:apply-templates select="atom:link[@rel='alternate']"/>
  </xsl:template>
  <xsl:template match="atom:content|atom:summary">
    <!-- hard-wiring disable-output-escaping for now 
         should be made conditional on @mode=escaped -->
    <xsl:copy-of select="node()"/>
  </xsl:template>
  <xsl:template match="xhtml:div">
    <xsl:copy/>
  </xsl:template>
  <xsl:template match="atom:link">
    <p><a>
      <xsl:attribute name="href"><xsl:value-of
      select="@href"/></xsl:attribute>
      <xsl:choose>
        <xsl:when test="@rel='service.edit'">[Edit]</xsl:when>
        <xsl:when test="@rel='service.post'">[Post]</xsl:when>
        <xsl:otherwise>PermaLink: <xsl:value-of select="@title"/></xsl:otherwise>
      </xsl:choose>
    </a>
    </p>
  </xsl:template>  
  

</xsl:stylesheet>
