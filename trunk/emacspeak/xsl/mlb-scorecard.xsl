<!--$Id$-->
<!--Description: Summarize baseball games.-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">
  
  <xsl:output encoding="iso8859-15"
  method="html"  indent="yes"/>
  <xsl:template match="/">
    <html>
      <head>
<title>Major League Baseball  Scores
--- <xsl:value-of select="/scoreboard/@last_modified"/></title>
      </head>
<body>
<h1>Games In Progress</h1>
<xsl:apply-templates select="/scoreboard/ig_game"/>
<h1>Completed Games</h1>
<xsl:apply-templates select="/scoreboard/go_game"/>
      </body>
    </html>
  </xsl:template>
<xsl:template match="go_game">
<p>
       <xsl:apply-templates select="team[1]"/> beat
      <xsl:apply-templates select="team[2]"/>
      <xsl:apply-templates select="l_pitcher"/>
<xsl:apply-templates select="sv_pitcher"/>
    </p>    
  </xsl:template>
<xsl:template match="team">
<xsl:value-of select="@name"/>
    <xsl:if test="@code=../game/@home_code"> at home </xsl:if>
with 
<xsl:value-of select="gameteam/@R"/> runs on 
    <xsl:value-of select="gameteam/@H"/> hits and  
<xsl:value-of select="gameteam/@E"/> errors;
  </xsl:template>
<xsl:template match="w_pitcher">
The winning pitcher  was 
<xsl:value-of select="pitcher/@name"/>
and he is now 
<xsl:value-of select="@wins"/> and <xsl:value-of select="@losses"/>.
  </xsl:template>
<xsl:template match="game">
In the <xsl:value-of select="@league"/>,</xsl:template>
<xsl:template match="l_pitcher">
The losing  pitcher  was 
<xsl:value-of select="pitcher/@name"/>
and he goes  
<xsl:value-of select="@wins"/> and <xsl:value-of select="@losses"/>.
  </xsl:template>
<xsl:template match="sv_pitcher">

<xsl:value-of select="pitcher/@name"/> got the save 
and he now has <xsl:value-of select="@saves"/> saves.
  </xsl:template>
<xsl:template match="*|@*" >
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
