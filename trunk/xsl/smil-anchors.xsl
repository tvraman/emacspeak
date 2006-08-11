<?xml version="1.0" encoding="utf-8"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Convert audio links in SMIL files to XHTML
navigation list
-->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="iso8859-15"
  method="html"  indent="yes"/>
  <xsl:template match="/smil">
    <html><head>
        <title>
          <xsl:value-of select="meta[@name='title']/@content"/>
        </title>
      </head>
      <body>
        <ol>
          <xsl:apply-templates select="body/audio"/>
        </ol>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="audio">
    <li><a>
        <xsl:attribute name="href">
          <xsl:value-of select="@src"/>
        </xsl:attribute>
        <xsl:value-of select="@title"/>
      </a>
      <xsl:value-of select="@author"/>
    </li>
  </xsl:template>
  
  
</xsl:stylesheet>
