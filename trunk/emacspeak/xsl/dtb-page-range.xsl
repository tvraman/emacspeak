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
  xmlns:set="http://exslt.org/sets"
  version="1.0">
  <xsl:param name="start" select="1"/>
  <xsl:param name="end" select="1"/>
  <xsl:param name="base" />
  <xsl:param name="css">revstd.css</xsl:param>
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/> 
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
            <xsl:variable name="all" select="//*"/>
            <xsl:variable name="first"
            select="//pagenum[number(text())=$start]"/>
            <xsl:variable name="last"
            select="//pagenum[number(text()) &gt; $end]"/>
            <xsl:variable name="after" select="set:trailing($all, $first)"/>
            <xsl:variable name="before" select="set:leading($all, $last)"/>
            <xsl:for-each select="set:intersection($before, $after)">
              <xsl:copy-of select="."/>
            </xsl:for-each>
          </xsl:when>
          <xsl:otherwise>
            <strong>This book has no page boundary markers.</strong>
          </xsl:otherwise>
        </xsl:choose>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
