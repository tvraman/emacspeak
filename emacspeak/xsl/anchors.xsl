<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Description: Show list of anchors.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:output method="html" indent="yes"/>
  
  <!-- {identity default  -->   
  <xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>



  <!-- } -->
<!-- {nuke these elements. --> 

<xsl:template match="script|meta|link"/>

<!-- } -->
<!-- {html body  --> 

<xsl:template match="/html/body">
<ul>
<xsl:apply-templates select="//a"/>
</ul>
</xsl:template>
<xsl:template match="//a">
<li>
<xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
</li>
</xsl:template>

<!-- } -->
   
</xsl:stylesheet>
