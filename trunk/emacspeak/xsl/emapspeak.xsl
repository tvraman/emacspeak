<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Transformation rules for speaking map metadata from google maps.
-->
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                 version="1.0">
  <xsl:param name="base"/>
  <xsl:template match="/page">
    <html>
      <head>
        <title><xsl:apply-templates select="title"/></title>
      </head>
      <body>
        <h1><xsl:apply-templates select="title"/></h1>
        <table summary="Coordinates">
          <tr>
            <td>
              <a>
                <xsl:attribute name="href">
                  <xsl:value-of select="$base"/>
                </xsl:attribute>
Reference Point</a></td>
<td>Lat: <xsl:value-of select="center/@lat"/></td>
            <td>Lng: <xsl:value-of select="center/@lng"/></td>
        </tr></table>
        <xsl:apply-templates select="overlay/location"/>
        <xsl:apply-templates select="directions"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="location">
    <h2>Location
    <xsl:value-of select="position()"/>:
    <xsl:apply-templates select="info/title"/></h2>
    <table summary="info">
      <xsl:apply-templates select="info"/>
      <tr>
        <td>Location</td>
        <td>Lat: <xsl:value-of select="point/@lat"/></td>
        <td>Lng: <xsl:value-of select="point/@lng"/></td>
      </tr>
    </table>
  </xsl:template>

  <xsl:template match="info">
    <tr>
      <td>
        <xsl:apply-templates select="address"/>
      </td>
      <td>Tel: <xsl:value-of select="phone/text()"/></td>
      <td>Distance: <xsl:value-of select="distance/text()"/></td>
    </tr>
    <tr>
      <td>Description</td>
      <td>
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="normalize-space(description/references/reference/url)"/>
          </xsl:attribute>
          <xsl:value-of select="description/references/reference/title"/>
        </a>
      </td>
      <td>
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="normalize-space(url)"/>
          </xsl:attribute>
        Web Site</a>
      </td>
    </tr>
  </xsl:template>
  <xsl:template match="address">

    <xsl:apply-templates select="line"/>
  </xsl:template>
  <xsl:template match="line">
    <xsl:copy-of select="."/><br/>
  </xsl:template>
  <xsl:template match="directions">
    <h2>Directions</h2>
    <xsl:apply-templates select="segments"/>
    <table summary="location">
      <tr>
        <td>From</td><td>To</td>
      </tr>
      <tr>
        <td>
        <xsl:apply-templates select="source/address"/></td>
        <td><xsl:apply-templates select="destination/address"/>
        </td>
      </tr>
    </table>
  </xsl:template>
<xsl:template match="segments">
<p> Route consists of 
<xsl:value-of select="count(segment)"/> segments making up a
total of 
<xsl:value-of select="@meters"/> meters (approximately
<xsl:value-of select="@distance"/>)
and is expected to take <xsl:value-of select="@seconds"/>
(approximately <xsl:value-of select="@time"/>).
</p>
<table summary="directions">
<xsl:apply-templates select="segment"/>
</table>
</xsl:template>

<xsl:template match="segment">
<tr>
<td>
  <xsl:value-of select="@seconds"/> s
</td>
<td>
  
  <strong>
    <xsl:copy-of select="./node()"/>.
  </strong>
  and Go 
  <em>
    <xsl:value-of select="@meters"/> meters (approximately
  <xsl:value-of select="@distance"/>)</em>
</td>
</tr>
</xsl:template>
</xsl:stylesheet>
