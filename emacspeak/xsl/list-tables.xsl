<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Description: List tables  by turning each row into a list.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:output method="html" indent="yes"/>
  
  <xsl:include href="identity.xsl"/>
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
