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
        --- <xsl:value-of select="/scoreboard/@date"/></title>
      </head>
      <body>
        <h1>Major League Baseball Scores For <xsl:value-of
        select="/scoreboard/@date"/></h1>
        <xsl:if test="/scoreborad/ig_game">
          <h2> <xsl:value-of select="count(/scoreboard/ig_game)"/> Games In Progress</h2>
          <xsl:apply-templates select="/scoreboard/ig_game"/>
        </xsl:if>
        <xsl:if test="/scoreboard/go_game">
          <h2>  <xsl:value-of select="count(/scoreboard/go_game)"/>Completed Games</h2>
          <xsl:apply-templates select="/scoreboard/go_game"/>
        </xsl:if>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="ig_game">
    <xsl:variable name="status">
      <xsl:choose>
        <xsl:when test="team[1]/gameteam/@R &gt;
          ../team[2]/gameteam/@R">
        lead </xsl:when>
        <xsl:when test="team[1]/gameteam/@R &lt;
          ../team[2]/gameteam/@R">
        trail  </xsl:when>
        <xsl:when test="team[1]/gameteam/@R =
          ../team[2]/gameteam/@R">
        tie   </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <h3>The <xsl:value-of select="team[1]/@name"/> 
      <xsl:variable name="$status"/>
    <xsl:value-of select="team[2]/@name"/></h3>
    <p>
      <xsl:apply-templates select="team[1]"/> 
      <xsl:value-of select="$status"/>
      <xsl:apply-templates select="team[2]"/>
      They are in the 
      <em><xsl:choose>
          <xsl:when test="inningnum/@half='B'"> bottom </xsl:when>
          <xsl:when test="inningnum/@half='T'"> top </xsl:when>
      </xsl:choose> half</em> of the
      <em> <xsl:value-of
      select="inningnum/@inning"/> innings</em>.
      <em><xsl:value-of select="pitcher/@name"/></em>
      is pitching  to 
      <em><xsl:value-of select="batter/@name"/></em>
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
    <xsl:variable name="status">
      <xsl:choose>
        <xsl:when test="team[1]/gameteam/@R &gt; team[2]/gameteam/@R">
        beat </xsl:when>
        <xsl:when test="team[1]/gameteam/@R &lt; team[2]/gameteam/@R">
        lost  to  </xsl:when>
        <xsl:when test="team[1]/gameteam/@R =
          ../team[2]/gameteam/@R">
        tie   </xsl:when>
      </xsl:choose>
    </xsl:variable>
    <h3>The <xsl:value-of select="team[1]/@name"/> 
      <xsl:value-of select="$status"/>
    <xsl:value-of select="team[2]/@name"/></h3>
    <p>
      <xsl:apply-templates select="team[1]"/> 
      <xsl:value-of select="$status"/>
      <xsl:apply-templates select="team[2]"/>
      <xsl:apply-templates select="l_pitcher"/>
      <xsl:apply-templates select="sv_pitcher"/>
    </p>    
  </xsl:template>
  <xsl:template match="team">
    The
    <strong><xsl:value-of select="@name"/></strong>
    <xsl:if test="@code=../game/@home_code"> at home </xsl:if>
    with 
    <xsl:value-of select="gameteam/@R"/> runs on 
    <xsl:value-of select="gameteam/@H"/> hits and  
    <xsl:value-of select="gameteam/@E"/> errors
  </xsl:template>
  <xsl:template match="w_pitcher">
    The winning pitcher  was 
    <strong><xsl:value-of select="pitcher/@name"/></strong>
    and he is now 
    <em><xsl:value-of select="@wins"/> and <xsl:value-of select="@losses"/></em>.
  </xsl:template>
  <xsl:template match="game">
  In the <code><xsl:value-of select="@league"/></code>,</xsl:template>
  <xsl:template match="l_pitcher">
    The losing  pitcher  was 
    <xsl:value-of select="pitcher/@name"/>
    who is  now   
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
