articles.html: raw.xml
	xsltproc ../xsl/atom-view.xsl full.xml >articles.html

raw.xml:
	wget -O raw.xml 'http://emacspeak.blogspot.com/feeds/posts/default?max-results=1000'

articles.xml: raw.xml
	xmllint raw.xml > articles.xml

raw:
	wget -O raw.xml 'http://emacspeak.blogspot.com/feeds/posts/default?max-results=1000'
