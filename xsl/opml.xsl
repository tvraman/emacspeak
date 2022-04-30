<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2022,   All Rights Reserved.
License: GPL
View OPML feeds as XHTML
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">
  <xsl:output encoding="UTF-8" method="html" indent="yes"/>
  <xsl:template match="/opml">
    <html>
      <xsl:apply-templates/>
    </html>
  </xsl:template>
  <xsl:template match="head">
    <head>
      <xsl:copy-of select="title"/>
    </head>
  </xsl:template>
  <xsl:template match="body">
    <body>
      <h1>
      <xsl:value-of select="/opml/head/title"/> </h1>
      <xsl:if test="/opml/head/dateModified">
        <h2>Date Modified: <xsl:copy-of
        select="/opml/head/dateModified"/></h2>
      </xsl:if>
      <ol>
        <xsl:apply-templates select=".//outline"/>
      </ol>
    </body>
  </xsl:template>
  <xsl:template match="outline">

    <xsl:if test="@xmlUrl|@xmlurl|@URL">
      <li><xsl:value-of select="@description|@subtext"/>
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="@xmlUrl|@xmlurl|@URL"/>
        </xsl:attribute>
        <xsl:value-of select="@title|@text"/>
        <xsl:choose>
          <xsl:when test="@type='link'"> (Link: C-o to open) </xsl:when>
          <xsl:when test="@type='atom'"> (Atom: C-a to open) </xsl:when>
          <xsl:when test="@type='rss'"> (RSS: C-r to open) </xsl:when>
          <xsl:when test="@type='audio'"> (Play: C-u ; or U)
            </xsl:when>
          <xsl:otherwise>(<xsl:value-of select="@type"/>)</xsl:otherwise>
        </xsl:choose>
      </a>
      </li>
    </xsl:if>

  </xsl:template>

</xsl:stylesheet>
<!--

Local Variables:
folded-file: t
End:
-->
