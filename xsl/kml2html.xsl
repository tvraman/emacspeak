<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Transform KML to speakable XHTML
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:kml="http://earth.google.com/kml/2.0"
                xmlns:kml21="http://earth.google.com/kml/2.1"
                >
  <xsl:output encoding="iso8859-15"
              method="xml"  indent="yes"/>
  
  <xsl:template match="kml:kml">
    <html>
      <head>
        <title><xsl:value-of select="kml:Document/kml:name"/></title>
      </head>
      <xsl:apply-templates select="kml:Document|kml:Folder|kml:Placemark"/>
    </html>
  </xsl:template>
  <xsl:template match="kml:Document|kml:Folder">
<body>
        <h1><xsl:value-of select="kml:name"/></h1>
        <p>
          <xsl:value-of select="kml:Snippet"
                        disable-output-escaping="yes"/>
        </p>
        <ol>
        <xsl:apply-templates select="kml:Placemark"/>
        </ol>
      </body>
  </xsl:template>
  <xsl:template match="kml:Placemark">
    <li>
      <em><xsl:value-of select="kml:name"/></em>
      <xsl:value-of select="kml:description"
                    disable-output-escaping="yes"/><br/>
Coordinates: <em><xsl:value-of select="kml:Point/kml:coordinates"/></em><br/>
<blockquote>
      <xsl:value-of select="kml:address"
                    disable-output-escaping="yes"/>
    <xsl:value-of select=".//kml:text"
                    disable-output-escaping="yes"/>
</blockquote></li>
    
  </xsl:template>
  
  
</xsl:stylesheet>
<!--
    Local Variables:
    mode: xae
    sgml-indent-step: 2
    sgml-indent-data: t
    sgml-set-face: nil
    sgml-insert-missing-element-comment: nil
    folded-file: t
    End:
--> 
