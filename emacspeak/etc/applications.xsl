<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		version="1.0"
                xmlns:redirect="org.apache.xalan.xslt.extensions.Redirect"        
                extension-element-prefixes="redirect" >

  <xsl:output method="html" media-type="text/html"/>
  <xsl:strip-space elements="*"/>
  

  

  <xsl:template match="applications">
    <table>
      <caption>
        <xsl:value-of select="@caption"/>
      </caption>
      <xsl:apply-templates />
    </table>
  </xsl:template>

  <xsl:template match="category">
    <tr>
      <td colspan="2">
        <xsl:value-of select="@name"/>
      </td></tr>
    <xsl:apply-templates />
  </xsl:template>
  
  <xsl:template match="application">
    <tr>
      <td><xsl:value-of select="@name"/></td>
      <td> <xsl:apply-templates/></td>
    </tr>
  </xsl:template>
  <xsl:template match="*|@*">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
