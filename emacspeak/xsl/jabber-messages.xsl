<?xml version="1.0" ?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Show  jabber messages.
-->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  
  
  <xsl:output method="html" indent="yes"
  encoding="iso8859-15"/>
  <xsl:param name="session"/>
  
  <xsl:template match="jabber">
    <html>
      <head>
        <style type="text/css">
          @media speech {
          span.<xsl:value-of
          select="substring(message[1]/@from, 1, 3)"/> {
          font-style: italic}
          span.<xsl:value-of
          select="substring(message[1]/@from, 1, 3)"/> {
          voice-family: paul;
          stress: 2; richness: 9; 
          pitch: 1; pitch-range: 9; }
          }
        </style>
        <title>Messages From <xsl:value-of select="$session"/></title>
      </head>
      <body>
        <h1>Messages From <xsl:value-of select="$session"/></h1>
        <table>
          <tr>
            <td>From</td>
            <td>Date</td>
            <td>Time</td>
            <td>Message</td>
          </tr>
          <xsl:apply-templates/>
        </table>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="message">
    <tr>
      <td><xsl:value-of select="substring(@from, 1, 3)"/></td>
      <td><xsl:value-of select="@date"/></td>
      <td><xsl:value-of select="@time"/></td>
      <td>
        <xsl:element name="span">
          <xsl:attribute name="class">
            <xsl:value-of select="substring(@from, 1, 3)"/>
          </xsl:attribute>
          <xsl:apply-templates/>
        </xsl:element>
      </td>
    </tr>
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
