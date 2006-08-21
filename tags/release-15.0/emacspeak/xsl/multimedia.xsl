<?xml version="1.0" ?>
<!--Author: T. V. Raman <raman@cs.cornell.edu>
Description: Produce a list of anchors.
Focus specifically on extracting links to multimedia content
that is stashed away inside object or embed tags.
Eventually try produce only one instance of each link.
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:output method="html" indent="yes"/>
  
  <!-- {nuke these elements. --> 
  <xsl:include href="identity.xsl"/>
  <xsl:template match="script|meta|link"/>

  <!-- } -->
  <!-- {html body  --> 

  <xsl:template match="/html/body">
<p>Multimedia links</p>
    <ul>
      <xsl:apply-templates select="//a|//object|//embed"/>
    </ul>
  </xsl:template>

  <xsl:template match="//a">
    <li>
      <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
    </li>
  </xsl:template>

  <xsl:template match="object|embed">
    <li>
      <xsl:element name="a">
        <xsl:attribute name="href">
          <xsl:value-of select="@src"/>
        </xsl:attribute>
        <xsl:value-of select="name()"/>
      </xsl:element>
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
