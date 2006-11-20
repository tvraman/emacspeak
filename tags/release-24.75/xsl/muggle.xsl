<?xml version="1.0" encoding="utf-8"?>
<!--$Id$-->
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Run a muggle input file to produce XHTML
params: what - what to look for 
-->
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                 version="1.0">
  <xsl:param name="what"/>
  <xsl:template match="/muggle">
    <html>
      <head>
        <title>
          <xsl:apply-templates select="title"/>
        </title>
      </head>
      <body>
        <xsl:apply-templates select="mug"/>
      </body>
    </html>
  </xsl:template>
  
  
  <xsl:template match="mug">
    <h2>
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="@url"/>
        </xsl:attribute>
        <xsl:value-of select="title"/>
      </a>
    </h2>
    
    <xsl:copy-of select="."/>
  </xsl:template>
  
  
</xsl:stylesheet>
