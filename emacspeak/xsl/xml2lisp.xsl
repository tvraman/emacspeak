<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--
Author: T. V. Raman
Copyright:GPL
Description:Convert XML to a Lisp S-expression.
Goal: Replace Emacs' xml-parse.el with equivalent functionality
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="text"/>
  <xsl:template match="text()">
    "<xsl:copy-of select="."/>"
  </xsl:template>
  <xsl:template match="node()">
    (<xsl:choose>
      <xsl:when test="@*">
        ("<xsl:value-of select="name()"/>"
        <xsl:apply-templates select="@*"/>)
      </xsl:when>
      <xsl:otherwise>
        "<xsl:value-of select="name()"/>"
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="node()"/>)
  </xsl:template>

  <xsl:template match="@*">
    ("<xsl:value-of  select="name()"/>" . "<xsl:value-of select="."/>")
  </xsl:template>
</xsl:stylesheet>
