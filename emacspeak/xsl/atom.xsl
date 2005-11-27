<?xml version="1.0"?>
<!--
    Author: T. V. Raman <raman@cs.cornell.edu>
    Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
    License: GPL
    View an Atom feed as clean HTML
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:atom="http://purl.org/atom/ns#"
                xmlns:str="http://exslt.org/strings"
                version="1.0">
  <xsl:param name="base"/>
  <xsl:variable name="amphetadesk">http://127.0.0.1:8888/my_channels.html</xsl:variable>
  <xsl:output encoding="iso8859-15" method="xml" indent="yes"/>
  <!-- {{{ Atom -->
  <xsl:template match="atom:feed">
    <html>
      <head>
        <title>
          <xsl:apply-templates select="atom:title"/> 
        </title>
      </head>
      <body>
        <h1>
          <xsl:apply-templates select="atom:tagline"/>
        </h1>
        <xsl:apply-templates select="atom:entry"/>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="atom:entry">
    <h2>
      <xsl:apply-templates select="atom:title"/>
    </h2>
    <xsl:apply-templates select="atom:content"/>
  </xsl:template>
  <xsl:template match="atom:content">
    <!-- hard-wiring disable-output-escaping for now 
         should be made conditional on @mode=escaped -->
    <xsl:value-of disable-output-escaping="yes" select="node()"/>
  </xsl:template>
  
  <!--}}}-->
  
  
  
  
  
  
  
  
  
</xsl:stylesheet>
<!--

Local Variables:
folded-file: t
End:
-->
