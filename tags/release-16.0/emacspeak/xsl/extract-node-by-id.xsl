<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Display node identified by  id
id is specified as  parameter node-id
Parameter base specifies base URL of source document. 
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  
  <xsl:output method="html" indent="yes"
              encoding="iso8859-15"/>
<xsl:param name="node-id">1</xsl:param>  
<xsl:param name="base"/>
  <xsl:include href="identity.xsl"/>
<!-- { html body  -->
<!--add base uri if available. -->
<xsl:template match="/">
    <xsl:apply-templates select="//*[@id=$node-id]"/>
  </xsl:template>
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
