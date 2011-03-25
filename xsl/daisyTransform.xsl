<?xml version="1.0" encoding="UTF-8"?>
<!-- Originally from Bookshare.
Fri Feb 11 08:02:56 PST 2011
Cloning here to possibly improve for use with Emacspeak.
-->

<!--******************************
       DAISY XSL TRANSFORM

    Make an XSL capable browser
    understand DAISY markup.
****************************** -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:dtb="http://www.daisy.org/z3986/2005/dtbook/">

  <xsl:output method="html" indent="no"/>
  
  
  <!--******************************
   DOCBOOK, HEAD, META, LINK, BOOK
  *******************************-->

  <!-- docbook translates to html -->
  <xsl:template match="/dtb:dtbook">
    <html><xsl:apply-templates/></html>
  </xsl:template>

  <!-- head maps directly -->
  <xsl:template match="dtb:head">
    <xsl:element name="head">
      <xsl:if test="@profile">
        <xsl:attribute name="profile"><xsl:value-of select="@profile"/></xsl:attribute>
      </xsl:if>

      <title><xsl:value-of select="/dtb:dtbook/dtb:book/dtb:frontmatter/dtb:doctitle"/></title>

      <link rel="stylesheet" type="text/css" href="html.css" />

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- meta maps directly
       Include: content
       If applicable, include: http-equiv, name
       NOTE: meta contains no content so no apply-templates necessary -->
  <xsl:template match="dtb:meta">
    <xsl:element name="meta">
      <xsl:if test="@http-equiv">
        <xsl:attribute name="http-equiv"><xsl:value-of select="@http-equiv"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@name">
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
      </xsl:if>
      <xsl:attribute name="content"><xsl:value-of select="@content"/></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- link maps directly
       If aqpplicable, includes: charset, href, hreflang, media, rel, rev, type
       NOTE: link contains no content so no apply-templates necessary -->
  <xsl:template match="dtb:link">
    <xsl:element name="link">
      <xsl:call-template name="link-attributes"/>
      <xsl:if test="@media">
        <xsl:attribute name="media"><xsl:value-of select="@media"/></xsl:attribute>
      </xsl:if>
    </xsl:element>
  </xsl:template>

  <!-- book should be translated to body -->
  <xsl:template match="dtb:book">
    <body><xsl:apply-templates/></body>
  </xsl:template>


  <!--*******************************
  FRONTMATTER, BODYMATTER, REARMATTER
  ******************************* -->

  <!--frontmatter, bodymatter and rearmatter become divisions with appropriate class attributes-->
  <xsl:template match="dtb:frontmatter | dtb:bodymatter | dtb:rearmatter">
    <xsl:element name="div">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>


  <!--**************************
  DOCTITLE, DOCAUTHOR, COVERTITLE
  ***************************-->

  <!-- doctitle is h1 with class for styling -->
  <xsl:template match="dtb:doctitle">
    <h1 class="doctitle"><xsl:apply-templates/></h1>
  </xsl:template>

  <!-- docauthor is p with class for styling -->
  <xsl:template match="dtb:docauthor">
    <p class="docauthor"><xsl:apply-templates/></p>
  </xsl:template>

  <!-- covertitle is p with class for styling -->
  <xsl:template match="dtb:covertitle">
    <p class="covertitle"><xsl:apply-templates/></p>
  </xsl:template>


  <!--***********************
             LEVELS
  ************************-->

  <!-- Levels map to div with class -->
  <xsl:template match="dtb:level | dtb:level1 | dtb:level2 | dtb:level3 | dtb:level4 | dtb:level5 | dtb:level6">
    <xsl:element name="div">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>

  <!--***********************
            HEADINGS
  ************************-->

  <!--h1...h6 map directly -->
  <xsl:template match="dtb:h1 | dtb:h2 | dtb:h3 | dtb:h4 | dtb:h5 | dtb:h6">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>

  <!-- hd as child of level converts to h1...h6 based on number of level ancestors
       If more than 6 ancestors then defaults to h6, flattening hierarchy beyond level 6 -->
  <xsl:template match="dtb:level/dtb:hd">
    <xsl:variable name="levelDepth" select="count(ancestor-or-self::dtb:level)" />
    <xsl:choose>
      <xsl:when test="$levelDepth &lt;= 6">
        <xsl:element name="{concat('h',$levelDepth)}">
          <xsl:call-template name="base-attributes"/>
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="h6">
          <xsl:call-template name="base-attributes"/>
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!--for hd within items like list use paragraph with class -->
  <!-- for bridgehead use paragraph with class -->
  <xsl:template match="dtb:hd | dtb:bridgehead">
    <xsl:element name="p">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>


    <!--*************************
         PAGENUM, LINENUM
  ************************-->

  <!--Put the pagenum into a paragraph element if the parent is level or level1...level6 otherwise put it into a span
      Use the pagenum class for formatting -->
  <xsl:template match="dtb:pagenum">
    <xsl:choose>
      <xsl:when test="parent::dtb:level or parent::dtb:level1 or parent::dtb:level2 or parent::dtb:level3 or parent::dtb:level4 or parent::dtb:level5 or parent::dtb:level6">
        <xsl:element name="p">
          <xsl:call-template name="base-attributes"/>
          <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
          <xsl:apply-templates />
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="span">
          <xsl:call-template name="base-attributes"/>
          <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
          <xsl:apply-templates />
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- linenum is translated to span with class -->
  <xsl:template match="dtb:linenum">
    <xsl:element name="span">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>


  <!--*************************
        GENERAL BLOCKS
  ************************-->

  <!-- address, div, p map directly -->
  <xsl:template match="dtb:address | dtb:div | dtb:p">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>

  <!-- annotation, epigraph, linegroup, note, poem, prodnote, sidebar map to div with class
       For prodnote, sidebar: Exclude: render attribute, no way to express -->
  <xsl:template match="dtb:annotation | dtb:epigraph | dtb:linegroup | dtb:note | dtb:poem | dtb:prodnote | dtb:sidebar">
    <xsl:element name="div">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>

  <!-- blockquote maps directly
       If applicable, include: cite -->
  <xsl:template match="dtb:blockquote">
    <xsl:element name="blockquote">
      <xsl:call-template name="base-attributes"/>
      <xsl:if test="@cite">
        <xsl:attribute name="cite"><xsl:value-of select="@cite"/></xsl:attribute>
      </xsl:if>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- byline, dateline, line maps to a p with class -->
  <xsl:template match="dtb:byline | dtb:dateline | dtb:line">
    <xsl:element name="p">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
      <xsl:apply-templates />
    </xsl:element>
  </xsl:template>


  <!--*************************
       GENERAL INLINES
  ************************-->

  <!-- a maps directly
       If applicable, include: charset, href, hreflang, rel, rev, type 
       If external is true then target a new window -->
  <xsl:template match="dtb:a">
    <xsl:element name="a">
      <xsl:call-template name="base-attributes"/>
      <xsl:call-template name="link-attributes"/>
      <xsl:if test="@external='true'">
        <xsl:attribute name="target">_blank</xsl:attribute>
      </xsl:if>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- bdo maps directly,
       Include: dir -->
  <xsl:template match="dtb:bdo">
    <bdo dir="{@dir}"><xsl:apply-templates/></bdo>
  </xsl:template>

  <!-- abbr, acronym, cite, dfn, em, kbd, samp, strong, sub, sup map directly -->
  <xsl:template match="dtb:abbr | dtb:acronym | dtb:cite | dtb:dfn | dtb:em | dtb:kbd | dtb:samp | dtb:strong | dtb:sub | dtb:sup">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  
  <!-- code maps directly
       If has class preserve-whitespace then surround with a pre
       tag -->
<!-- TVR: bookshare books dont appear to use preserve-space where
they should -->
   <xsl:template match="dtb:code|code">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
          <xsl:element name="pre"><xsl:apply-templates/></xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- span maps to span for classes underline, strikethrough, double-strikethrough, small-caps, but is omitted otherwise -->
  <xsl:template match="dtb:span">
    <xsl:choose>
      <xsl:when test="@class='underline'">
        <span class="underline"><xsl:apply-templates/></span>
      </xsl:when>
      <xsl:when test="@class='strikethrough'">
        <span class="strikethrough"><xsl:apply-templates/></span>
      </xsl:when>
      <xsl:when test="@class='double-strikethrough'">
        <span class="double-strikethrough"><xsl:apply-templates/></span>
      </xsl:when>
      <xsl:when test="@class='small-caps'">
        <span class="small-caps"><xsl:apply-templates/></span>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!--title-->

  <!-- author maps to p with class -->
  <xsl:template match="dtb:author">
    <xsl:element name="p">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)"/></xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- br maps directly
       NOTE: no apply-templates needed since this tag is always self closing-->
  <xsl:template match="dtb:br">
    <br />
  </xsl:template>

  <!-- q maps directly
       If applicable, includes: cite -->
  <xsl:template match="dtb:q">
    <xsl:element name="q">
      <xsl:call-template name="base-attributes"/>
      <xsl:if test="@cite">
        <xsl:attribute name="cite"><xsl:value-of select="@cite"/></xsl:attribute>
      </xsl:if>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- annoref, noteref maps to span with class -->
  <xsl:template match="dtb:annoref | dtb:noteref">
    <xsl:element name="span">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)" /></xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- sent, w have no equivalent tag -->
  <xsl:template match="dtb:sent | dtb:w">
    <xsl:apply-templates/>
  </xsl:template>


  <!--*************************
           LISTS
  ************************-->

  <!--Get fancy with the various list types-->

  <!-- An unordered list will be wrapped in ul tags -->
  <xsl:template match="dtb:list[@type='ul']">
    <xsl:element name="ul">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- A preformatted list will be wrapped in ul tags with an appropriate class.
       CSS can be used to turn off default display symbols, the list will still be
       rendered as such in the browser's DOM, which will let screen readers
       announce the item as a list -->
  <xsl:template match="dtb:list[@type='pl']">
    <xsl:element name="ul">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class">pl</xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- An ordered list will be wrapped in ol tags
       Ensure the desired formatting is preserved by pushing the enum attribute into the class attribute
           Note: replaces enum="1" with class="one" to ensure CSS 2.1 validation
       If applicable, include: start -->
  <xsl:template match="dtb:list[@type='ol']">
    <xsl:element name="ol">
      <xsl:call-template name="base-attributes"/>
      <xsl:choose>
        <xsl:when test="@enum='1'">
          <xsl:attribute name="class">one</xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="class"><xsl:value-of select="@enum"/></xsl:attribute>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@start">
        <xsl:attribute name="start"><xsl:value-of select="@start"/></xsl:attribute>
      </xsl:if>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- li maps directly -->
  <xsl:template match="dtb:li">
    <xsl:element name="li">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- lic maps to span -->
  <xsl:template match="dtb:lic">
    <xsl:element name="span">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="class"><xsl:value-of select="local-name(.)"/></xsl:attribute>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <!-- *************************
         DEFINITION LIST
  ************************ -->

  <!-- dd, dl, dt map directly -->
  <xsl:template match="dtb:dd | dtb:dl | dtb:dt">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <!--*************************
            TABLES
  ************************ *** -->

  <!-- table maps directly
       If applicable, include: border, cellpadding, cellspacing, frame, rules, summary, width -->
  <xsl:template match="dtb:table">
    <xsl:element name="table">
      <xsl:call-template name="base-attributes"/>
      <xsl:if test="@border">
        <xsl:attribute name="border"><xsl:value-of select="@border"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@cellpadding">
        <xsl:attribute name="cellpadding"><xsl:value-of select="@cellpadding"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@cellspacing">
        <xsl:attribute name="cellspacing"><xsl:value-of select="@cellspacing"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@frame">
        <xsl:attribute name="frame"><xsl:value-of select="@frame"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@rules">
        <xsl:attribute name="rules"><xsl:value-of select="@rules"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@summary">
        <xsl:attribute name="summary"><xsl:value-of select="@summary"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@width">
        <xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute>
      </xsl:if>

      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- table/caption maps directly -->
  <xsl:template match="dtb:table/dtb:caption">
    <xsl:element name="caption">
      <xsl:call-template name="base-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- tr maps directly
       If applicable, include: align, char, charoff, valign -->
  <xsl:template match="dtb:tr">
    <xsl:element name="tr">
      <xsl:call-template name="base-attributes"/>
      <xsl:call-template name="alignment-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- col, colgroup map directly
       If applicable, include: align, char, charoff, span, valign, width -->
  <xsl:template match="dtb:col | dtb:colgroup">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:call-template name="alignment-attributes"/>
      <xsl:if test="@span">
        <xsl:attribute name="span"><xsl:value-of select="@span"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@width">
        <xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute>
      </xsl:if>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- tbody, thead, tfoot map directly
       If applicable, include: align, char, charoff, valign -->
  <xsl:template match="dtb:tbody | dtb:thead | dtb:tfoot">
    <xsl:element name="{local-name(.)}">
      <xsl:call-template name="base-attributes"/>
      <xsl:call-template name="alignment-attributes"/>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <!-- td, th map directly
       If applicable, include: abbr, align, axis, char, charoff, colspan, headers, rowspan, scope, valign -->
  <xsl:template match="dtb:td | dtb:th">
    <xsl:element name="td">
      <xsl:call-template name="base-attributes"/>
      <xsl:call-template name="alignment-attributes"/>
      <xsl:if test="@abbr">
        <xsl:attribute name="abbr"><xsl:value-of select="@abbr"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@axis">
        <xsl:attribute name="axis"><xsl:value-of select="@axis"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@colspan">
        <xsl:attribute name="colspan"><xsl:value-of select="@colspan"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@headers">
        <xsl:attribute name="headers"><xsl:value-of select="@headers"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@rowspan">
        <xsl:attribute name="rowspan"><xsl:value-of select="@rowspan"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@scope">
        <xsl:attribute name="scope"><xsl:value-of select="@scope"/></xsl:attribute>
      </xsl:if>
      
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>


  <!--*************************
            IMAGES
  ************************ *** -->

  <!-- img maps directly
       Include: alt, src
       If applicable, include: longdesc, height, width
       NOTE: img is self closing so no apply-templates necessary -->
  <xsl:template match="dtb:img">
    <xsl:element name="img">
      <xsl:call-template name="base-attributes"/>
      <xsl:attribute name="alt"><xsl:value-of select="@alt"/></xsl:attribute>
      <xsl:attribute name="src"><xsl:value-of select="@src"/></xsl:attribute>
      <xsl:if test="@longdesc">
        <xsl:attribute name="longdesc"><xsl:value-of select="@longdesc"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@height">
        <xsl:attribute name="height"><xsl:value-of select="@height"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="@width">
        <xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute>
      </xsl:if>
    </xsl:element>
  </xsl:template>

  <!-- imggroup maps to div with class -->
  <xsl:template match="dtb:imggroup">
    <div class="imggroup"><xsl:apply-templates/></div>
  </xsl:template>

  <!-- imggroup/caption maps to div with class 
       Excludes imgref - no way to express -->
  <xsl:template match="dtb:imggroup/dtb:caption">
    <div class="caption"><xsl:apply-templates/></div>
  </xsl:template>


  <!--*************************
            Helpers
  ************************ *** -->
  
  <!-- If applicable include id -->
  <xsl:template name="base-attributes">
    <xsl:if test="@id">
      <xsl:attribute name="id"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <!-- If applicable include align, char, charoff, valign -->
  <xsl:template name="alignment-attributes">
    <xsl:if test="@align">
      <xsl:attribute name="align"><xsl:value-of select="@align"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@char">
      <xsl:attribute name="char"><xsl:value-of select="@char"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@charoff">
      <xsl:attribute name="charoff"><xsl:value-of select="@charoff"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@valign">
      <xsl:attribute name="valign"><xsl:value-of select="@valign"/></xsl:attribute>
    </xsl:if>
  </xsl:template>
  
  <!-- If applicable include: charset, href, hreflang, rel, rev, type -->
  <xsl:template name="link-attributes">
    <xsl:if test="@charset">
      <xsl:attribute name="charset"><xsl:value-of select="@charset"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@href">
      <xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@hreflang">
      <xsl:attribute name="hreflang"><xsl:value-of select="@hreflang"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@rel">
      <xsl:attribute name="rel"><xsl:value-of select="@rel"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@rev">
      <xsl:attribute name="rev"><xsl:value-of select="@rev"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="@type">
      <xsl:attribute name="type"><xsl:value-of select="@type"/></xsl:attribute>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
