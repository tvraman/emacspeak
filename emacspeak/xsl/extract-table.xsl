<?xml version="1.0"?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) 2001  All Rights Reserved.

Description: Extract specified table.  Table to extract is
specified by param table-index, default value is 1.  Result
is to extract the table appearing in position table-index of
node-set //table//table

-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>
  <xsl:param name="table-index">1</xsl:param>
  <xsl:include href="identity.xsl"/>
  <!-- { html body  --> 

  <!-- nuke these -->
  <xsl:template match="//script|//meta"/>
  <xsl:template match="/html/body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="//table//table[$table-index]"/>
    </xsl:element>
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
