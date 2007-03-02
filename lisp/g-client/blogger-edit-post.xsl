<?xml version="1.0"?>
<!--$Id:$-->
<!--
Author: T. V. Raman
License: GPL
Description: Used by gblogger to edit out server-generated elements 
when posting or updating an entry.
-->

<xsl:stylesheet xmlns:atom="http://www.w3.org/2005/Atom" xmlns="http://www.w3.org/2005/Atom" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" indent="yes"/>
<xsl:template match="atom:id|atom:issued|atom:modified|atom:created|atom:published|atom:updated"/>  
<xsl:template match="atom:content">
  <content type="xhtml">
    <xsl:attribute name="type">xhtml</xsl:attribute>
<xsl:apply-templates/>
  </content>
</xsl:template>
  <xsl:template match="*|@*">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
