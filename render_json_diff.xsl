<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:template match="/">
        <html>
            <head>
                <title>JSON Compatibility Test</title>
                <style>
                .body {
                    font-family: monospace;
                }
                .object, .list {
                    padding-left: 20px;
                }
                .added, .added_key, .changed .to {
                    color: green;
                    font-weight: bold;
                }
                .allowed_added, .allowed_added_key, .similar .to {
                    color: green;
                }
                .removed, .removed_key, .changed .from {
                    color: red;
                    font-weight: bold;
                    text-decoration: line-through;
                }
                .allowed_removed, .allowed_removed_key, .similar .from {
                    color: red;
                    text-decoration: line-through;
                }
                </style>
            </head>
            <body>
                <xsl:apply-templates/>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="obj_diff">
        <div class="obj">
            <span>{</span>
            <div class="object">
                <xsl:for-each select="*">
                    <xsl:apply-templates select="."/>
                    <xsl:if test="position() != last()">
                        <span>,</span>
                        <br/>
                    </xsl:if>
                </xsl:for-each>
            </div>
            <span>}</span>
        </div>
    </xsl:template>

    <xsl:template match="array_diff">
        <span class="list">
            <span>[</span>
            <br/>
            <xsl:for-each select="item">
                <xsl:apply-templates/>
                <xsl:if test="position() != last()">
                    <span>,</span>
                    <br/>
                </xsl:if>
            </xsl:for-each>
            <br/>
            <span>]</span>
        </span>
    </xsl:template>

    <xsl:template match="pair">
        <span>
            <xsl:attribute name="class"><xsl:value-of select="@class"/></xsl:attribute>
            <span>"</span>
            <span class="key"><xsl:value-of select="key"/></span>
            <span>": </span>
            <xsl:apply-templates select="value/*"/>
        </span>
    </xsl:template>

    <xsl:template match="null">
        <span class="null">null</span>
    </xsl:template>

    <xsl:template match="bool|num|same|similar|changed|from|to">
        <span><xsl:attribute name="class"><xsl:value-of select="name()"/></xsl:attribute><xsl:apply-templates/></span>
    </xsl:template>

    <xsl:template match="str">
        <span><xsl:attribute name="class">
            <xsl:value-of select="name()"/></xsl:attribute>
            <span>"</span>
            <xsl:apply-templates/>
            <span>"</span>
        </span>
    </xsl:template>

    <xsl:template match="list">
        <span>[</span>
        <div class="list">
            <xsl:for-each select="item">
                <xsl:apply-templates select="."/>
                <xsl:if test="position() != last()">
                    <span>,</span>
                    <br/>
                </xsl:if>
            </xsl:for-each>
        </div>
        <span>]</span>
    </xsl:template>

    <xsl:template match="item">
        <span>
            <xsl:attribute name="class"><xsl:value-of select="@class"/></xsl:attribute>
            <xsl:apply-templates/>
        </span>
    </xsl:template>

    <xsl:template match="obj">
        <span>{</span>
        <div class="object">
            <xsl:for-each select="pair">
                <xsl:apply-templates select="."/>
                <xsl:if test="position() != last()">
                    <span>,</span>
                    <br/>
                </xsl:if>
            </xsl:for-each>
        </div>
        <span>}</span>
    </xsl:template>

    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>