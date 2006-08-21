<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  <xsl:output method="html" indent="yes"
encoding="iso8859-15"/>
  
  <xsl:include href="identity.xsl"/>
  <!-- { html body  --> 

  <xsl:template match="//script|meta|link"/>
<xsl:template match="td/font|td/strong|td/span|td/font|td/nobr">
<span>
<xsl:apply-templates/>
    </span><br/>
  </xsl:template>
  <xsl:template match="html/body">
    <xsl:element name="body">
<p><strong>
<a href="#__about_this_style">Contents Revealed</a>
        </strong></p>
      <xsl:apply-templates 
                           select =
        "//div|//h1|//h2|//h3|//h4|//h5|//h6|//td/strong|//td/em|//td/li|//td/font|//td/nobr|//td/span|//p|//pre|//ul|//ol|//dl"/>

<h2><a name="__about_this_style">About This Style</a></h2>
<p>
This style extracts content from a layout-rich WWW page.
      </p>
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
