<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--
Author: T. V. Raman
Copyright:GPL
Description:Convert XML to a Lisp S-expression.
Goal: Replace Emacs' xml-parse.el with equivalent functionality
Shortcomings: Quotes in  PCDATA will be lost
Still very slow, possibly write a native  libxml2 app?
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output method="text"/>
  <xsl:template match="text()">
    <xsl:variable name="text" select="normalize-space()"/>
    <xsl:if test="$text">
      "<xsl:value-of select="$text"/>
      "
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="*">
    (<xsl:choose>
      <xsl:when test="@*">
        ("<xsl:value-of select="name()"/>
        "<xsl:apply-templates select="@*"/>
        )
      </xsl:when>
      <xsl:otherwise>
        "<xsl:value-of select="name()"/>
        "
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates/>
    )
  </xsl:template>
  
  <xsl:template match="@*">
    ("<xsl:value-of  select="name()"/>
    ". "<xsl:value-of select="."/>
    ")
  </xsl:template>
</xsl:stylesheet>
