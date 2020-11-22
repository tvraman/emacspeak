articles.html: full.xml
	xsltproc ../xsl/atom-view.xsl full.xml >articles.html

raw.xml:
	wget -O raw.xml 'http://emacspeak.blogspot.com/feeds/posts/default?max-results=10'
	xmlindent -i 2  -l 75 -w raw.xml 
