<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
View BBC  Program Guides 
http://downloads.bbc.co.uk/podcasts/ppg.xml
http://downloads.bbc.co.uk/podcasts/ppg.xsd

-->
<xsl:stylesheet xmlns:ppg="http://bbc.co.uk/2007/7/ppg" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output encoding="UTF-8" method="xml" indent="yes"/>
  <xsl:template match="/ppg:ppg">
    <html>
      <head>
        <title>BBC Program Guide</title>
      </head>
      <body>
        <h1>BBC Program Guide: Modified <xsl:value-of select="@modifiedDate"/></h1>
        <xsl:apply-templates select="program"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="program">
    <xsl:choose>
      <xsl:when test="@public='true'                        and @active='true'                        and @liveItems &gt; 0                        and @region='all'">
        <h2>
          <xsl:value-of select="title"/>
          <em>(<xsl:value-of select="bbcGenre/@name"/>)</em>
        </h2>
        <p>
        <xsl:copy-of select="description"/></p>
        <p>
          Frequency: 
          <xsl:value-of select="@frequency"/>.
          Items are live for <xsl:value-of select="@daysLive"/> days,
        <xsl:value-of select="@liveItems"/> available.</p>
        <xsl:apply-templates select="link"/>
        <p>
          Duration: <xsl:value-of
          select="@typicalDuration"/></p>
      </xsl:when>
    </xsl:choose>
  </xsl:template>
  <xsl:template match="link">
    <a>
      <xsl:attribute name="href">
        <xsl:value-of select="@url"/>
      </xsl:attribute>
      <xsl:value-of select="@target"/>
    </a>
  </xsl:template>
</xsl:stylesheet>
<!--

Local Variables:
folded-file: t
End:
-->
