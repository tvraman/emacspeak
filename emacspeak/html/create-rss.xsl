<?xml version="1.0"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Turn html links to RSS items.
-->
 
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output encoding="iso8859-15"
              method="xml"  indent="yes"/>
  <!-- {identity default  -->   

  <xsl:template match="*|@*" >
    <xsl:copy>
       <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- } -->
<!-- {root rss document  --> 
<xsl:template match="/">
<rss version="0.91">
  <channel>
    <title>Emacspeak --The Complete Audio Desktop</title>
    <description>
Emacspeak is a speech interface that allows visually
    impaired users to interact independently and efficiently with
    the computer. Audio formatting --a technique pioneered by  <a
    href=
    "http://www.cs.cornell.edu/home/raman/aster/aster-toplevel.html">
    AsTeR</a>-- and full support for <a href=
    "http://www.w3.org/Press/1998/CSS2-REC">W3C's Aural CSS
    (ACSS)</a> allows Emacspeak to produce rich aural presentations
    of electronic information. By seamlessly blending all aspects
    of the Internet such as Web-surfing and messaging, Emacspeak
    speech-enables local and remote information via a consistent
    and well-integrated user interface. Available free of cost on
    the Internet, Emacspeak has dramatically changed how the author
    and hundreds of blind and visually impaired users around the
    world interact with the personal computer and the Internet. A
    rich suite of <a href="applications.html">task-oriented
    tools</a> provides efficient speech-enabled access to the audio
    desktop and evolving semantic WWW. When combined with Linux
    running on low-cost PC hardware, Emacspeak/Linux provides a
    reliable, stable speech-friendly solution that opens up the
    Internet to visually impaired users around the world.
        </description>
    <language>en-us</language>
    <link>http://emacspeak.sf.net/</link> 
    <copyright>Copyright 2003, Emacspeak Inc</copyright>
    <author>(raman@cs.cornell.edu) T. V. Raman</author>
	<image>
		<link>http://emacspeak.sf.net/</link>
		<url>http://emacspeak.sf.net/emacspeak.jpg</url>
		<title>Emacspeak --The Complete Audio Desktop</title>
	</image>
<xsl:apply-templates select="links/a"/>
      </channel>
    </rss>
  </xsl:template>

<!-- } -->
<!-- {turn anchors into items --> 

<xsl:template match="a">
<item>
<link>
<xsl:value-of select="@href"/>
      </link>
<title><xsl:apply-templates />
      </title>
<description>
<xsl:apply-templates/>
      </description>
    </item>
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
