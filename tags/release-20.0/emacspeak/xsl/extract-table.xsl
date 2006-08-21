<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Extract specified table.  Table to extract is
specified by parametersparameter table-index whose default
value is 1.  Result is to extract the table appearing in
position (table-index) in node-set //table//table.
Parameter base specifies base URL of source document. 

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"
  encoding="iso8859-15"/>
  <xsl:param name="table-index">1</xsl:param>
  <xsl:param name="base"/>
  <xsl:include href="object.xsl"/>
  <xsl:include href="identity.xsl"/>
  <!-- { html body  -->
  <!--add base uri if available. -->
  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="head">
    <head>
      <xsl:apply-templates select="title"/>
      <xsl:if test="string-length($base) &gt; 0">
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
      </xsl:if>
    </head>
  </xsl:template>
  <!-- nuke these -->
  <xsl:template match="//script|//meta"/>
  <xsl:template match="body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <h2>Table (<xsl:value-of select="$table-index"/>) </h2>
      <xsl:for-each select="//table//table">
        <xsl:if test="position() = $table-index">
          <xsl:element name="table">
            <xsl:apply-templates select="@*"/>
            <xsl:apply-templates/>
          </xsl:element>
        </xsl:if>
      </xsl:for-each>
      
      <p>
        Table <xsl:value-of select="$table-index"/>
        from  
        <xsl:element name="a">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
          Source Document
        </xsl:element>
      </p>
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
