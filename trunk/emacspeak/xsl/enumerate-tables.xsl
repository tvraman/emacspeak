<?xml version="1.0"?>
<!-- { introduction -->
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Enumerate all tables in a page, 
Each nested table is replaced with an anchor that links to
the  actual table.
The default mode, which is the first pass, passes everything
except nested tables unmodified.

Nested tables are matched by rule match = //tables//table 
which creates the required anchor element, deferring the
copying out of the nested table to the second-pass.

During the second pass, nested tables get recursively
processed by calling apply-templates which by default
applies rules from the first pass.


The  second-pass uses  a for-each iterator over
nodeset //table//table.

Using for-each allows this style to correctly number the
nested tables during the second-pass; this numbering can be
used as the table-index for extract-tables.xsl.

-->
<!-- } -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:set="http://exslt.org/sets"
  version="1.0">
  <xsl:param name="base"/>
  <!-- cache these for readability and efficiency -->
  <xsl:variable name="all" select="/descendant::table"/>
  <xsl:variable name="nested" select="//table//table"/>
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/>
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
    <body>
      <xsl:apply-templates select="@*"/>
      <xsl:if test="count($all)  &gt; 0">
        <table>
          <caption>
            <a href="#__about_enumerated_tables">Tables Enumerated</a>
          </caption>
          <tr>
            <td>
              <xsl:value-of select="count($all)"/> tables 
            </td>
          </tr>
        </table>
      </xsl:if>
      <xsl:for-each select="$all">
        <h2>Table <xsl:value-of select="position()"/></h2>
        <xsl:apply-templates select="."/>
      </xsl:for-each>
      <xsl:if test="count(//table//table)  &gt; 0">
        <h2>
          <a name="__enumerated_tables"
            id="__nested_tables"><xsl:value-of
          select="count(//table//table)"/> Nested Tables </a>
        </h2>
        
        
        <xsl:for-each select="$all">
          <xsl:if test="set:intersection($nested, .)">
            <h2>
              <a>
                <xsl:attribute name="href">
                  #src-<xsl:value-of select="generate-id(.)"/>
                </xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:value-of
                select="generate-id(.)"/></xsl:attribute>
                <em>Table <xsl:value-of select="position()"/> </em>
              </a>
              <xsl:value-of select="count(./tr)"/> Rows 
              And <xsl:value-of select="count(./tr/td)"/> Cells
            </h2>
            <table>
              <xsl:apply-templates select="@*"/>
              <xsl:apply-templates/>
            </table>
          </xsl:if>
        </xsl:for-each>
        <h2>
          <a name="__about_enumerated_tables">About This
          Style</a>
          <p>Tables have been enumerated ---with the numbering
            matching the position of each table in the overall list
            of tables in the document.
            This numbering is useful when thinking of each table in a Web
            page as a conceptual <em>card</em>
            and can be used to advantage in extracting a few select
            <em>cards</em> from the conceptual deck of cards making
          up the Web page.</p>
        </h2>
        
      </xsl:if>
    </body>
  </xsl:template>
  
  <xsl:template match="//table//table">
    <xsl:variable name="rows" select="count(./tr)"/>
    <xsl:variable name="cols" select="count(./tr/td)"/>
    <a>
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text>
        <xsl:value-of select="generate-id(.)"/>
      </xsl:attribute>
      <xsl:attribute name="name">
        <xsl:text>src-</xsl:text>
        <xsl:value-of select="generate-id(.)"/>
      </xsl:attribute>
      <xsl:choose>
        <xsl:when test="@summary">
          <xsl:value-of select="@summary"/>
        </xsl:when>
        <xsl:when test="$rows = 1 and $cols = 1">
          <xsl:apply-templates select="./tr/td/*"/>
        </xsl:when>
        <xsl:otherwise>
          [<xsl:value-of select="$rows"/>, <xsl:value-of select="$cols"/>]
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text> </xsl:text>
    </a>
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
