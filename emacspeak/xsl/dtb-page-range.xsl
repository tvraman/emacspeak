<?xml version="1.0" encoding="utf-8"?>
<!--$Id$


Description: Extract nodes in a specified page range from a
Daisy3 XML file.  Page range is specified via params start and
end.  All nodes appearing between <pagenum>start</pagenum> and
<pagenum>end+1</pagenum> are extracted. All other nodes are
ignored. 

The nodes we want to output are located by computing the 
intersection of two sets A and B:

A: The set of nodes that follow the start page marker ($after)
B:The set of nodes that precede the end page marker ($before)

Leading and trailing nodes were originally  computed using set:leading and
set:trailing; this has been replaced with a call to following and 
preceding for speed 
and the final intersection is computed using set:intersection.
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:set="http://exslt.org/sets"
  version="1.0">
  <xsl:param name="start" select="1"/>
  <xsl:param name="end" select="1"/>
  <xsl:param name="base" />
  <xsl:param name="css">revstd.css</xsl:param>
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/>
  <xsl:key name="pageKey" match="pagenum" use="number(text())"/>
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
            <xsl:variable name="first"
            select="key('pageKey', $start)"/>
            <xsl:variable name="last" select="key('pageKey',
            number($end+1))"/>
            <xsl:variable  name="after" select="$first[1]/following::p"/>
            <xsl:variable name="before" select="$last[1]/preceding::p"/>
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
  <xsl:template match="pagenum">
    <p><strong><xsl:apply-templates/></strong></p>
  </xsl:template>
</xsl:stylesheet>
