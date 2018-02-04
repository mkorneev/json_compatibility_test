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
                .obj_content, .item {
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

    <xsl:template match="@*|node()">
        <xsl:copy>
            <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>