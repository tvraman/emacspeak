<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Transformation rules for speaking map metadata from google maps.
-->
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output  method="html"/>
  <xsl:template match="/page">
    <html>
      <head>
        <title><xsl:apply-templates select="title"/></title>
      </head>
      <body>
        <h1><xsl:apply-templates select="title"/></h1>
        <table summary="Coordinates">
<tr>
  <td>Lat: <xsl:value-of select="center/@lat"/></td>
  <td>Lng: <xsl:value-of select="center/@lng"/></td>
</tr></table>
<xsl:apply-templates select="overlay/location"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="location">
    <h2>Location
<xsl:value-of select="position()"/>:
     <xsl:apply-templates select="info/title"/></h2>
    <table summary="Coordinates">
<tr>
  <td>Lat: <xsl:value-of select="point/@lat"/></td>
  <td>Lng: <xsl:value-of select="point/@lng"/></td>
</tr></table>
<xsl:apply-templates select="info"/>
  </xsl:template>
  

<xsl:template match="info">
  <table summary="info">
    <tr>
<td>Address</td><td>Phone</td><td>Distance</td></tr>
<tr>
  <td>
<xsl:apply-templates select="address"/>
  </td>
<td><xsl:value-of select="phone/text()"/></td>
<td><xsl:value-of select="distance/text()"/></td>
</tr>
  </table>
</xsl:template>
<xsl:template match="address">

  <xsl:apply-templates select="line"/>
</xsl:template>
<xsl:template match="line">
<xsl:copy-of select="."/><br/>
</xsl:template>
</xsl:stylesheet>
