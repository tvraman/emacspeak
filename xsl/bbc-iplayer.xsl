<?xml version='1.0'?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Transform BBC iPlayer schedules to simple HTML.
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="html" indent="yes" encoding="UTF-8"/>
  <xsl:variable name="base"> http://www.bbc.co.uk/iplayer/console/</xsl:variable>
  <xsl:template match="/">    <html>
      <head>
<base>
<xsl:attribute name="href"><xsl:value-of select="$base"/></xsl:attribute>
</base>
        <title>
          <xsl:value-of select="/schedule/service/title"/>
        </title>
      </head>
      <body>
        <h1><xsl:value-of select="/schedule/service/title"/></h1>
        <xsl:apply-templates select="//broadcasts"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="broadcasts">
    
    <ol>
      <xsl:apply-templates select="broadcast"/>
    </ol>
  </xsl:template>

  <xsl:template match="broadcast">
    <li>
<a>
<xsl:attribute name="href"><xsl:value-of select="./programme/pid"/></xsl:attribute>
        <xsl:value-of select="./programme/display_titles/title"/>
</a>
        <xsl:value-of select="./programme/display_titles/subtitle"/>
          <xsl:value-of select="./programme/short_synopsis"/>
</li>
  </xsl:template>
</xsl:stylesheet>
