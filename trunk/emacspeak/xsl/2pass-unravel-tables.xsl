<?xml version="1.0"?>
<!-- { introduction -->
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) 2001  All Rights Reserved.

Description: Factor out nested tables to the end of document.
Each nested table is replaced with an anchor that links to
the  actual table which appears in a final section of the
document.

The default mode, which is the first pass, passes everything
except tables unmodified.

Tables are matched by rule match = //tables//table 
which creates the required anchor element, deferring the
copying out of the table to the second-pass.


Finally we invoke mode second-pass which provides a rule 
for copying out the table.

During the second pass, nested tables get recursively
processed by calling apply-templates which by default
applies rules from the first pass.

-->
<!-- } -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:include href="object.xsl"/>
  <xsl:include href="identity.xsl"/>
  <!-- { html body  -->
  <!-- nuke these -->
  <xsl:template match="//script|//meta|//iframe"/>
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
  <xsl:template match="body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <table>
        <caption>
          <a href="#__about_unravel_tables">Tables Unraveled</a>
        </caption>
        <tr>
          <td>
            <a href="#__nested_tables">
              <xsl:value-of select="count(//table)"/>
               
              tables of which 
              <xsl:value-of select="count(//table//table)"/>
              are nested
            </a>
          </td>
        </tr>
      </table>
      <xsl:apply-templates/>
      <h2>
        <a name="__nested_tables" id="__nested_tables"> Nested Tables </a>
      </h2>
      <p>
        There are 
        <xsl:value-of select="count(//table//table)"/>
        nested tables in this page.
      </p>
      <xsl:apply-templates select="//table//table" mode="second-pass"/>
      <h2>
        <a name="__about_unravel_tables">About This Style</a>
      </h2>
      <p>
        Note that nested tables have been moved to  section <a href="#__nested_tables">nested tables</a>.
        The table cell that contained the nested table has been
        replaced with a hyperlink that navigates to the actual
        table. If the author has provided a summary and or
        caption for the nested table, those will be displayed
        as the hyperlink text.
      </p>
    </xsl:element>
  </xsl:template>
  <!-- rule that defers rendering of nested tables -->
  <xsl:template match="//table//table">
    <xsl:element name="a">
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text><xsl:value-of select="generate-id(.)"/>
      </xsl:attribute>
      <xsl:value-of select="caption"/>
      Table <xsl:value-of select="position()"/>
      <xsl:value-of select="@summary"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="//table//table" mode="second-pass">
    <xsl:element name="a">
      <xsl:attribute name="name">
        <xsl:value-of select="generate-id(.)"/>
      </xsl:attribute>
      <xsl:element name="table">
        <xsl:apply-templates select="@*"/>
        <xsl:apply-templates/>
      </xsl:element>
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
