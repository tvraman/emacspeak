<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2007,   All Rights Reserved.
License: GPL
Return alist of title/url pairs from  an Atom stream.
-->
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:docs='http://schemas.google.com/docs/2007' 
    xmlns:batch='http://schemas.google.com/gdata/batch' 
    xmlns:gd='http://schemas.google.com/g/2005'
    xmlns:atom="http://www.w3.org/2005/Atom"
    xmlns:xhtml="http://www.w3.org/1999/xhtml"
    xmlns:openSearch="http://a9.com/-/spec/opensearchrss/1.0/"
    xmlns:gr="http://www.google.com/schemas/reader/atom/"
    version="1.0">
  <xsl:output encoding="UTF-8" method="text" indent="yes"/>
  <xsl:template name="tq">
    <xsl:param name="string" select="''" />
    <xsl:choose>
      <xsl:when test="contains($string, '&quot;')"> <xsl:value-of select="substring-before($string, '&quot;')" />\"<xsl:call-template name="tq"><xsl:with-param name="string" select="substring-after($string, '&quot;')" /></xsl:call-template> </xsl:when>
      <xsl:otherwise> <xsl:value-of select="$string" /> </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="atom:feed">
    <xsl:if test="count(atom:entry) > 1 ">
      (
      <xsl:apply-templates select="atom:entry"/>    
      )
    </xsl:if>
  </xsl:template>
  
  
  <xsl:template match="atom:entry">
    (
    " <xsl:value-of select="atom:link[1]/@href"/>"
    .
"<xsl:call-template name="tq"> <xsl:with-param name="string" select="atom:title"/></xsl:call-template>"
    )
  </xsl:template>

  <xsl:template match="atom:content|atom:summary">
    <xsl:choose>
      <xsl:when test="@src">
	[<a>
	<xsl:attribute name="href">
	  <xsl:value-of select="@src"/>
	</xsl:attribute>
	<img alt=" Download">
	  <xsl:attribute name="src"><xsl:value-of
	  select="@src"/></xsl:attribute>
	</img>
	</a>]
      </xsl:when>
      <xsl:when test="@type='html' or @type='text/html'">
	<xsl:value-of disable-output-escaping="yes"
		      select="node()"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:copy-of select="node()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="atom:link|gd:feedLink">
    <a>
      <xsl:attribute name="href">
	<xsl:value-of
	    select="@href"/>
      </xsl:attribute>
      <xsl:choose>
	<xsl:when test="@rel='service.edit'">[Edit]</xsl:when>
	<xsl:when test="@rel='edit'">[Edit]</xsl:when>
	<xsl:when
	    test="@rel='edit-media'">[Edit-Media]</xsl:when>
	<xsl:when test="@rel='media-edit'">[Media-Edit]</xsl:when>
	<xsl:when test="@rel='service.post'">[Post]</xsl:when>
	<xsl:when test="@rel='next'">[Next]</xsl:when>
	<xsl:when test="@rel='self'">[Self]</xsl:when>
	<xsl:when test="@rel='alternate'">[HTML]</xsl:when>
	<xsl:when test="@rel='enclosure'">[<xsl:value-of select="@type"/>]</xsl:when>
        <xsl:when test="contains(@rel, 'books')">
        [<xsl:value-of select="substring-after(@rel,'books/2008/')"/>]</xsl:when>
	<xsl:otherwise>[<xsl:value-of select="substring-after(@rel,'#')"/>]</xsl:otherwise>
      </xsl:choose>
    </a>
  </xsl:template>
  
  
  <xsl:template match="atom:author">
    <a>
      <xsl:attribute name="href">
	<xsl:value-of select="atom:uri"/>
      </xsl:attribute>
      <xsl:value-of select="atom:name"/>
    </a>
  </xsl:template>
  <xsl:template match="atom:title"> <xsl:value-of select="."/> </xsl:template>
</xsl:stylesheet>
