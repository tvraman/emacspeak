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
  <h1>Major League Baseball Scores</h1>
<h2> <xsl:value-of select="count(/scoreboard/ig_game)"/> Games In Progress</h2>
<xsl:apply-templates select="/scoreboard/ig_game"/>
<h2>  <xsl:value-of select="count(/scoreboard/go_game)"/>Completed Games</h2>
<xsl:apply-templates select="/scoreboard/go_game"/>
      </body>
    </html>
  </xsl:template>
<xsl:template match="ig_game">
<p>
       <xsl:apply-templates select="team[1]"/> are leading 
      <xsl:apply-templates select="team[2]"/>
They are in the 
<xsl:choose>
<xsl:when test="inningnum/@half='B'"> bottom </xsl:when>
<xsl:when test="inningnum/@half='T'"> top </xsl:when>
      </xsl:choose> half of the <xsl:value-of
        select="inningnum/@inning"/> innings.
<xsl:value-of select="pitcher/@name"/> is pitching  to 
<xsl:value-of select="batter/@name"/>
with  
<xsl:choose>
<xsl:when test="@outs=0"> no </xsl:when>
<xsl:otherwise><xsl:value-of select="@outs"/></xsl:otherwise>
      </xsl:choose> outs. 
<xsl:apply-templates select="on_base"/>
    </p>    
  </xsl:template>
<xsl:template match="on_base">
<xsl:value-of select="player/@name"/> is on  
<xsl:choose>
<xsl:when test="@base='1'"> first </xsl:when>
      <xsl:when test="@base='2'"> second </xsl:when>
<xsl:when test="@base='3'"> third </xsl:when>
    </xsl:choose>
base.
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
The <xsl:value-of select="@name"/>
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
