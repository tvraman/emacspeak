<?xml version="1.0" ?>
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
  <!-- { html body  --> 

  <xsl:template match="/html/body">
    <xsl:element name="body">
      <xsl:apply-templates select="@*"/>
      <h2>Readers Note </h2>

      <p>
        Note that nested tables have been moved to the end of this
        document under section <a
                                  href="#__nested_tables">nested tables</a>.
        The table cell that contained the nested table has been
        replaced with a hyperlink that navigates to the actual
        table. If the author has provided a summary and or
        caption for the nested table, those will be displayed
        as the hyperlink text.
      </p>
      <xsl:apply-templates />
      <h2><a name="__nested_tables"
             id="__nested_tables">
          Embedded Tables </a> </h2>
      <xsl:apply-templates select="//table//table" mode="second-pass"/>
    </xsl:element>
  </xsl:template>
  
  <xsl:template match="//table//table"  >
    <xsl:element name="a">
      <xsl:attribute name="href">
        <xsl:text>#</xsl:text><xsl:value-of select="generate-id(.)"/>
      </xsl:attribute>
      <xsl:value-of select="caption"/>
      Summary: <xsl:value-of select="@summary"/>
    </xsl:element>
  </xsl:template>



  <xsl:template   match="//table//table"
                mode="second-pass">
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
