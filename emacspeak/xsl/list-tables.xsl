<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Description: List tables  by turning each row into a list.
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
<!-- {listify tables --> 
<xsl:template match="table">
<div class="table">
<xsl:apply-templates/>
</div>
</xsl:template>

<xsl:template match="tr">
<ol>
<xsl:apply-templates/>
  </ol>
</xsl:template>
<xsl:template match="td">
<li>
<xsl:apply-templates/>
  </li>
</xsl:template>
<!-- } -->
</xsl:stylesheet>
