<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: List tables  by turning each row into a list.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:output method="html" indent="yes"
              encoding="iso8859-15"/>
  
  <xsl:include href="identity.xsl"/>
<!-- {nuke these elements. --> 

<xsl:template match="script|meta|link"/>

<!-- } -->
<!-- {listify tables --> 
<xsl:template match="/html/body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <table>
        <caption>Tables As Lists</caption>
<tr>
<td><a href="#__about_listify_tables">About This
              Style</a></td>
        </tr></table>
      <xsl:apply-templates />
    </xsl:element>

<h2><a name="__about_listify_tables">About This Style</a> </h2>

      <p>
Tables are turned into lists.
Each table row is a separate list,
with each table cell an item in that list.
Nested tables produce nested lists.
  </p>
  </xsl:template>

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
