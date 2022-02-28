#$Id$
# Note: Building the info pages requires package hydra to be installed.
# Built info pages are checked into GitHub.
# You should need to rebuild info files
#*only* if you change the Emacspeak Lisp sources.
#http://emacspeak.sf.net/info
SF_HOME= raman,emacspeak@web.sourceforge.net:/home/groups/e/em/emacspeak/htdocs/info
#https://tvraman.github.io/emacspeak/manual
GH_PAGES=../../gh-pages-emacspeak/manual
BATCH = -batch -q -no-site-file     \
--eval '(setq file-name-handler-alist nil gc-cons-threshold 128000000)'
FILES=*.texi
INCLUDES= style.texi preamble.texi copyright.texi announce.texi emacspeak-significance.texi \
introduction.texi install.texi using.texi audio-desktop.texi voices.texi online-help.texi \
packages.texi eterm.texi docs.texi keyboard.texi tts-server.texi inc-turning-twenty.texi \
acknowledge.texi intro-docs.texi documents.texi structure.texi web-browsing.texi tts.texi

MAKEINFO = makeinfo 
CSS=--css-ref="http://www.w3.org/StyleSheets/Core/Modernist"
EMACS=emacs

all: info  introducing-emacspeak.info turning-twenty.info #emacspeak.pdf

info: emacspeak.info

emacspeak.info: emacspeak.texi $(INCLUDES)
	$(MAKEINFO) emacspeak.texi

introducing-emacspeak.info: introducing-emacspeak.texi
	$(MAKEINFO) introducing-emacspeak.texi	

turning-twenty.info: turning-twenty.texi
	$(MAKEINFO) turning-twenty.texi
force:

docs.texi:force
	$(EMACS) $(BATCH) -l ../utils/self-document.el -f self-document-all-modules

emacspeak.pdf: emacspeak.texi $(INCLUDES)
	makeinfo --pdf emacspeak.texi
	\rm emacspeak.aux emacspeak.cp emacspeak.cps emacspeak.fn \
emacspeak.fns emacspeak.ky emacspeak.kys emacspeak.log \
emacspeak.toc  emacspeak.vr emacspeak.vrs

man:
	makeinfo -o ${GH_PAGES}  --html  ${CSS} emacspeak.texi
	tidy -q -utf8 -mic -asxml  ${GH_PAGES}/*.html 2>&1 > /dev/null  || echo tidy
	@echo "Commit and push updated HTML files from gh-pages!"

publish:
	cd ../../gh-pages-emacspeak && make

introducing-emacspeak.html: introducing-emacspeak.texi
	makeinfo --no-split --html  introducing-emacspeak.texi
	perl add-css.pl introducing-emacspeak.html
#For publishing to SF
pub:  html/index.html introducing-emacspeak.html
	rsync introducing-emacspeak.html ${SF_HOME}
	rsync -avz  html ${SF_HOME}/info/


clean: 
	\rm *.info *.pdf || echo "all clean"

spell:
	codespell -I .codespell *.texi


tags:
	etags *.texi
