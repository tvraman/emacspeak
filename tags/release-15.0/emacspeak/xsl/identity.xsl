<?xml version="1.0"?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) 2001  All Rights Reserved.
Identity transform used in all style sheets.
-->
 
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="utf-8"
              method="html"  indent="yes"/>
  <!-- {identity default  -->   
  <xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- fix to avoid a bizarre bug in xsltproc 
  where entity &nbsp; gets changed to &#0302; &#0240;
  when using the -html option.  -->
  <xsl:template match="//body//text()">
    <xsl:value-of select="translate(., '&#160;', ' ')"/>
  </xsl:template>




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
