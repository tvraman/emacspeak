<?xml version="1.0" encoding="utf-8"?>
<!--$Id$
Description: Extract nodes in a specified page range from a Daisy3 XML file.
Page range is specified via params start and end.
All nodes appearing between 
<pagenum>start</pagenum>
and <pagenum>end+1</pagenum>
are extracted. All other nodes are ignored.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  <xsl:param name="start" select="1"/>
  <xsl:param name="end" select="1"/>
  <xsl:param name="base" />
  <xsl:param name="css">revstd.css</xsl:param>
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/> 
  <xsl:variable name="include" select="0"/>
<xsl:template match="*|@*" mode="copy">
    <xsl:value-of select="name()"/> has <xsl:value-of
    select="$include"/>
<xsl:text>
    </xsl:text>
<xsl:choose>
    <xsl:when test="$include = 1">
      <xsl:copy>
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates select="node()" mode="copy"/>
      </xsl:copy>
    </xsl:when>
<xsl:otherwise>
<xsl:apply-templates select="*" mode="copy"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="/">
    <html>
      <head>
        <link>
          <xsl:attribute name="type"> text/css</xsl:attribute>
          <xsl:attribute name="href" >
            <xsl:value-of select="$css"/>
        </xsl:attribute></link>
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
        <title>
          Pages
          <xsl:value-of select="$start"/>--<xsl:value-of select="$end"/>
          from  <xsl:value-of select="/dtbook3/head/title"/>
        </title>
      </head>
      <body>
        <xsl:variable name="pages" select="//pagenum"/>
        <xsl:choose>
          <xsl:when test="count($pages)  &gt; 0">
            <xsl:apply-templates select="*" mode="copy"/>
          </xsl:when>
          <xsl:otherwise>
            <strong>This book has no page boundary markers.</strong>
          </xsl:otherwise>
        </xsl:choose>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="//pagenum" mode="copy">
    <xsl:variable name="page-number" select="number(text())"/>
    <xsl:choose>
      <xsl:when test="$start &lt;= $page-number and $page-number &lt;= $end">
        page number is <xsl:value-of select="$page-number"/>
        <xsl:variable name="include" select="1"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="include" select="0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
</xsl:stylesheet>
