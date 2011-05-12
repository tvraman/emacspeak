<?xml version="1.0" encoding="utf-8"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
License: GPL
Copyright: This file is part of the g-client package.
Accept a GEvent invite
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:a="http://www.w3.org/2005/Atom"
                xmlns:gd="http://schemas.google.com/g/2005" >
  <xsl:output method="xml"  indent="yes"/>
  <xsl:param name="email"/>
  <xsl:template match="gd:who">
    <xsl:choose>
      <xsl:when test="@email=$email">
        <xsl:copy>
          <xsl:apply-templates  select="@*"/>
          <gd:attendeeStatus
              value='http://schemas.google.com/g/2005#event.accepted'>
          </gd:attendeeStatus>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy>
          <xsl:apply-templates select="@*"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>
      </xsl:otherwise>
  </xsl:choose></xsl:template>
  <xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  
  
</xsl:stylesheet>
