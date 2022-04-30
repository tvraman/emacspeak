<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2022,   All Rights Reserved.
License: GPL
Turn audio element into an anchor 
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="UTF-8"
              method="html"  indent="yes"/>
  <!-- {audio 
  <xsl:template match="//audio">
<a>
<xsl:attribute name="href">
<xsl:value-of select="source/@src"/>
(Audio)</a>
</xsl:template>
</xsl:template>
  -->
</xsl:stylesheet>
<!--
    Local Variables:
    mode: nxml
    sgml-indent-step: 2
    sgml-indent-data: t
    sgml-set-face: nil
    sgml-insert-missing-element-comment: nil
    folded-file: t
    End:
--> 
