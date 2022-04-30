<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2022,   All Rights Reserved.
License: GPL
View an RSS feed as clean HTML
Only supports RSS 1.0
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" 
                xmlns:dc="http://purl.org/dc/elements/1.1/"
                xmlns:media="http://search.yahoo.com/mrss/" 
                xmlns:rss="http://purl.org/rss/1.0/"
                xmlns:content="http://purl.org/rss/1.0/modules/content/" 
                xmlns:str="http://exslt.org/strings"
                version="1.0">
  <xsl:param name="base"/>
  <xsl:output encoding="UTF-8" method="html" indent="yes"/>
  <!-- {rss 1.0 -->
  <!-- Nuke all itunes elements -->
  <xsl:template match="itunes:*"/>
  
  <xsl:template match="rss">
    <html>
      <head>
        <title>
          <xsl:apply-templates select="rss:channel/rss:title|channel/title"/>
        </title>
      </head>
      <body>
        <ol>
          <xsl:apply-templates select="//item|//rss:item"/>
        </ol>
        <p>
          <xsl:apply-templates select="channel/description|rss:channel/rss:description"/>
        </p>
        <p>
          <xsl:apply-templates select="description|rss:description"/>
          <a>
            <xsl:attribute name="href"> <xsl:value-of select="$base"/> </xsl:attribute>
            RSS 
          </a>
        </p>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="rss:item|item">
    <li>
      <h2>
        <xsl:element name="a">
          <xsl:attribute name="href">
            <xsl:value-of select="rss:link|link"/>
          </xsl:attribute>
          <xsl:apply-templates select="title|rss:title"/>
        </xsl:element>
      </h2>
      <xsl:apply-templates
          select="description|rss:description"/>
      <!--
<br/><em><xsl:value-of select="pubDate|rss:pubDate"/></em><br/>
-->
      <xsl:apply-templates
          select="enclosure|rss:enclosure|media:content"/>
    </li>
  </xsl:template>
  <xsl:template match="rss:title|rss:description|title|description">
    <xsl:value-of select="." disable-output-escaping="yes"/>
  </xsl:template>
  
  <xsl:template match="enclosure|rss:enclosure|media:content">
    <xsl:element name="a">
      <xsl:choose>
        <xsl:when test="string-length(@url) != 0">
          <xsl:attribute name="href">
            <xsl:value-of select="str:decode-uri(@url)"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:when test="string-length(@href) != 0">
          <xsl:attribute name="href">
            <xsl:value-of
                select="str:decode-uri(@href)"/>
          </xsl:attribute>
        </xsl:when>
        <xsl:otherwise>Boom</xsl:otherwise>
      </xsl:choose>
      Enclosure:
      <xsl:value-of select="@length"/>
    </xsl:element>
  </xsl:template>
  
  <!-- } -->
  <!-- {identity default  -->
  <xsl:template match="*|@*">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()" />
    </xsl:copy>
  </xsl:template>
  <!-- } -->
</xsl:stylesheet>
<!--
    Local Variables:
    folded-file: t
    End:
-->
