<?xml version="1.0"?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) 2001  All Rights Reserved.

Description: Extract specified table.  Table to extract is
specified by param table-index, default value is 1.  Result
is to extract the table appearing in position table-index of
node-set //table//table
Parameter base specifies base URL of source document.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:param name="table-index">1</xsl:param>
  <xsl:param name="base"/>
  <xsl:include href="identity.xsl"/>
<!-- { html body  -->
<!--add base uri if available. -->
  <xsl:template match="/html/head">
    <head>
      <xsl:element name="base">
        <xsl:attribute name="href">
          <xsl:value-of select="$base"/>
        </xsl:attribute>
      </xsl:element>
      <xsl:apply-templates/>
    </head>
  </xsl:template>
<!-- nuke these -->
  <xsl:template match="//script|//meta"/>
  <xsl:template match="/html/body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <h2>Table <xsl:value-of select="$table-index"/></h2>
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
