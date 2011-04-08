<?xml version="1.0" encoding="UTF-8"?>
<!-- Extract light-weight table of contents for Bookshare Books:
AKA   avoid the bloatware that is Daisy.
This stylesheet also handles legacy Bookshare materials.
-->

<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:dtb="http://www.daisy.org/z3986/2005/dtbook/">

  <xsl:output method="html" indent="no"/>
  <xsl:param name="base" />
  <!-- Define separate templates for dtb:book and legacy dtbook3-->
  
  <xsl:template match="dtb:dtbook">
    <html>
      <head>
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
        <title>
          <xsl:value-of select="./dtb:book/dtb:frontmatter/dtb:doctitle"/>
        </title>
      </head>
      <body>
        <h1>
          <xsl:value-of select="dtb:book/dtb:frontmatter/dtb:doctitle"/>
        </h1>
        <p>By: <author><xsl:value-of
        select="./dtb:book/dtb:frontmatter/dtb:docauthor"/>
        </author></p>

        <p>
<xsl:if test="count(//dtb:pagenum) &gt; 1">
          <a>
            <xsl:attribute name="href">
              <xsl:value-of select="concat($base,'?')"/>
            </xsl:attribute>
          Extract Page Range
            [<xsl:value-of
            select="(//dtb:pagenum)[1]"/> --
            <xsl:value-of
                select="(//dtb:pagenum)[last()]"/>]
</a>
</xsl:if>
        </p>
        <ol>
          <xsl:for-each select="//dtb:bodymatter//dtb:level1|//dtb:bodymatter//dtb:level2">
            <xsl:if test="dtb:h2|dtb:h1">
              <li>
                <a>
                  <xsl:attribute name="href">
                  <xsl:value-of select="concat($base,'#',@id)"/></xsl:attribute>
                  <xsl:value-of select="dtb:h1|dtb:h2"/>
                </a>
              </li>
              
          </xsl:if></xsl:for-each>
        </ol>
        <h1>Copyright Notice And Legalese</h1>
        <ol>
          <xsl:for-each select="//dtb:frontmatter//dtb:level2">
            <xsl:if test="dtb:h2">
              <li>
                <a>
                  <xsl:attribute name="href">
                  <xsl:value-of select="concat($base,'#',@id)"/></xsl:attribute>
                  <xsl:value-of select="dtb:h2"/>
                </a>
              </li>
              
          </xsl:if></xsl:for-each>
        </ol>
      </body>
    </html>
  </xsl:template>

  <!-- legacy: -->
  <xsl:template match="dtbook3">
    <html>
      <head>
        <xsl:element name="base">
          <xsl:attribute name="href">
            <xsl:value-of select="$base"/>
          </xsl:attribute>
        </xsl:element>
        <title>
          <xsl:value-of select="./head/title"/>
        </title>
      </head>
      <body>
        <h1>
          <xsl:value-of select="./head/title"/>
        </h1>
        <p>
          By: <author><xsl:value-of select="./head/author"/>
        </author></p>
        <p>
          <a>
            <xsl:attribute name="href">
              <xsl:value-of select="concat($base,'?')"/>
            </xsl:attribute>
            Extract Pages: [<xsl:value-of select="(//pagenum)[1]"/> --
            <xsl:value-of select="(//pagenum)[count(//pagenum)]"/>
          </a>

        </p>
        <ol>
          <xsl:for-each select="//level2">
            <li>
              <a>
                <xsl:attribute name="href">
                <xsl:value-of select="concat($base,'#',@id)"/></xsl:attribute>
                <xsl:value-of select=".//h2"/>
              </a>
            </li>
          </xsl:for-each>
        </ol>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
