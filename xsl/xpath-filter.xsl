<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Extract content as specified by param locator.
Param locator is an XPath expression.
Param path is the same expression, but quoted so it can be
shown in the output.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:set="http://exslt.org/sets"
                version="1.0"
                exclude-result-prefixes="set">
  <xsl:param name="locator"/>
  <xsl:param name="path"/>
  <xsl:param name="base"/>
  <xsl:output method="html" indent="yes" encoding="UTF-8"/>
  <xsl:include href="object.xsl"/>
  <!-- { html   -->
<!-- nuke these -->  
  <xsl:template
      match="//script|//meta|//nolayer|//ilayer|//layer|//spacer|//*[@style='display:none']"/>
  <!--add base uri if available. -->
  <xsl:template match="head">
    <head>
      <xsl:apply-templates select="title"/>
      <xsl:if test="string-length($base) &gt; 0">
        <base>
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </base>
      </xsl:if>
    </head>
  </xsl:template>
  <xsl:template match="*|@*" mode="copy" >
    <xsl:variable name="i" select="$locator"/>
    <xsl:if test="not(set:intersection(ancestor::*, $i))">
      <xsl:copy-of select="."/>
      <br/>
    </xsl:if>
  </xsl:template>
  <xsl:template match="body">
    <body>
      <div>
	<xsl:for-each select="$locator" >
	  <xsl:copy-of  select="."/>
	</xsl:for-each>
	

      </div>
      <div>
	<h2>
	  Summary:
	  <xsl:value-of select="count($locator)"/>  Nodes Matching   <xsl:value-of select="$path"/>
	  in <xsl:element name="a">
	  <xsl:attribute name="href">
	    <xsl:value-of select="$base"/>
	  </xsl:attribute>
	  document.
	</xsl:element>
	</h2> 
      </div>
    </body>
  </xsl:template>
  
  
  
  <xsl:include href="identity.xsl"/>
  <!-- nuke these -->
  
  <!-- } -->
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
