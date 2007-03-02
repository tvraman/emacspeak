<?xml version="1.0" encoding="utf-8"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
License: GPL
Copyright: This file is part of the g-client package.
Reusable GEvent XSL rules
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:a="http://www.w3.org/2005/Atom"
                xmlns:gd="http://schemas.google.com/g/2005" >
  <xsl:output method="xml"  indent="yes"/>
  <xsl:variable
      name="gd">http://schemas.google.com/g/2005</xsl:variable>
  <xsl:variable name="eventprefix" select="concat($gd, '#event.')"/>
  <xsl:template match="a:entry" >
    <h1>
      <a>
        <xsl:attribute name="href"><xsl:value-of select="a:id" disable-output-escaping="yes"/> </xsl:attribute>
        <xsl:value-of select="a:title"
                      disable-output-escaping="yes"/></a>
        <xsl:if test="a:link[@rel='edit']/@href">
          <a>
            <xsl:attribute name="href"><xsl:value-of select="a:link[@rel='edit']/@href"/>
            </xsl:attribute>
            [Edit]
          </a>

    </xsl:if></h1>




    <p>
      <xsl:value-of select="a:summary" disable-output-escaping="yes"/>
    </p>
    <table>
      <tr>
        <td>
        <xsl:value-of select="gd:when/@startTime"/>
        </td>
<td>
        <xsl:value-of select="gd:when/@endTime"/></td>
        <!--
        <td><xsl:value-of
        select="substring-after(gd:transparency/@value,
        $eventprefix)"/></td>
        -->
      </tr>
      <tr>
        <td>
          <a>
            <xsl:attribute name="href">mailto:<xsl:value-of select="a:author/a:email" disable-output-escaping="yes"/>
            </xsl:attribute>
            <xsl:value-of select="a:author/a:name" disable-output-escaping="yes"/>
        </a></td>
        <!--<td><xsl:value-of select="substring-after(gd:eventStatus/@value, $eventprefix)"/> </td>
        <td><xsl:value-of
        select="substring-after(gd:visibility/@value,
        $eventprefix)"/></td>
        
        -->
<td>
          <xsl:value-of select="gd:where/@valueString"/>
        </td>
      </tr>
    </table>
    <p>
      <xsl:value-of select="a:content" disable-output-escaping="yes"/>
    </p>
<!-- too many show up 
    <h3>Participants</h3>
<ul>
<xsl:for-each  select="gd:who">
<li><a>
  <xsl:attribute name="href">mailto:<xsl:value-of select="@email"/></xsl:attribute>
<xsl:value-of select="@valueString"/>
</a>
</li>
</xsl:for-each>
</ul>
-->
  </xsl:template>

  <xsl:template match="gd:eventStatus">
    
  </xsl:template>
  <xsl:template match="gd:visibility">
    Visibility
  </xsl:template>
  <xsl:template match="gd:transparency">
    Transparency
  </xsl:template>



</xsl:stylesheet>
