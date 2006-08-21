<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description:  Many WWW pages use a script handler as a
submit button on forms.
This transformation adds a vanila <input type="submit">
button so that one can submit such forms without Javascript.
-->


<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  
  <xsl:output method="html" indent="yes"
  encoding="iso8859-15"/>
  <xsl:include href="object.xsl"/>
  <xsl:include href="identity.xsl"/>
  <!-- {nuke these elements. --> 
  
  <xsl:template match="script|meta|link"/>
  <xsl:template match="form">
    <xsl:element name="form">
      <xsl:apply-templates select="@*"/>
      <p>
      <input type="submit"/>Submit Button</p>
      <xsl:apply-templates/>
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
