<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Description: Generate Table of contents
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
<!-- {contents  --> 
<xsl:template match="/html/body">
    <ol>
      <xsl:apply-templates select="//h1|//h2|//h3"/>
    </ol>

</xsl:template>
<!-- } -->
</xsl:stylesheet>
