<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Just show me the google hit list
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes" encoding="iso8859-15"/>
<xsl:param name="query"></xsl:param>
  <xsl:include href="identity.xsl"/>
<!-- {nuke these elements. -->
  <xsl:template match="script|meta|link"/>
  <xsl:template match="/html">
    <html>
      <head>
<base href="http://www.google.com/"/>
        <title>Google Hits <xsl:value-of select="$query"/></title>
      </head>
      <body>
        <xsl:apply-templates select="//div"/>
        <p>
        <xsl:apply-templates select="//form[not(@name)]"/>
        </p>
      </body>
    </html>
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
