<?xml version="1.0" encoding="utf-8"?>
<!--
Author: T. V. Raman <raman@cs.cornell.edu>
License: GPL
Copyright: This file is part of the g-client package.
XSL rules for handling YouTube responses
-->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:date="http://exslt.org/dates-and-times">
  <xsl:output method="xml"  indent="yes"/>
  <xsl:template match="ut_response">
    <xsl:choose>
      <xsl:when test="@status = 'ok'">
        <html>
          <head>
            <title>YouTube Response</title>
          </head>
          <body>
            <xsl:apply-templates/>
          </body>
        </html>
      </xsl:when>
      <xsl:otherwise>
        <p>Error</p>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="video_list">
    <p>List contains <xsl:value-of select="total"/> items.</p>
    <xsl:apply-templates select="video"/>
  </xsl:template>

  <xsl:template match="video|video_details">
    <h1><xsl:value-of select="title"/> </h1>
    
    <dl>
      <dt>Description</dt>
      <dd><p>
        <a>
          <xsl:attribute name="href">
            <xsl:value-of  select="url"/>
          </xsl:attribute>
          Watch</a>:
          <xsl:value-of select="description"/></p>
      </dd>
      <dt>Tags</dt>
      <dd>
        <p><xsl:value-of select="tags"/></p>
      </dd>
    </dl>
    <table>
      <tr>
        <td>Length</td>
        <td><xsl:value-of select="length_seconds"/>S</td>
        <td>Author</td>
        <td><xsl:value-of
        select="author"/></td>
      </tr>
      <tr>
        <td>Average Rating</td>
        <td><xsl:value-of select="rating_avg"/> </td>
        <td>Rating Count</td> <td><xsl:value-of
        select="rating_count"/></td>
      </tr>
      <tr>
        <td>Views</td> <td><xsl:value-of
        select="view_count"/></td>
        <td>Comments</td>
        <td><xsl:value-of select="comment_count"/></td>
        
      </tr>
      <tr>
        <td>Uploaded</td>
        <td>
          <xsl:value-of select=
                        "date:add( '1970-01-01', date:duration( ./upload_time))"/>
        </td>
        <td>ID</td>
        <td> <xsl:value-of select="id"/></td>
      </tr>
    </table>
  </xsl:template>
  <xsl:template match="user_profile">
    <xsl:for-each select="node()">
      <xsl:value-of select="name(.)"/>: <xsl:value-of
      select="."/><br/>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
