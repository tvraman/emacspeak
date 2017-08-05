# Build all setup files we use 

EMACS=emacs
BATCH = -batch -q -no-site-file    
COMPILE =  -f batch-byte-compile

SETUP = emacs-startup.elc console.elc screen.elc x.elc my-functions.elc \
vm-prepare.elc gnus-prepare.elc bbdb-prepare.elc mspools-prepare.elc \
elfeed-prepare.elc \
nxml-prepare.elc folding-prepare.elc \
auctex-prepare.elc reftex-prepare.elc \
calc-prepare.elc js-prepare.elc tcl-prepare.elc \
slime-prepare.elc company-prepare.elc python-mode-prepare.elc \
 org-prepare.elc erc-prepare.elc \
jabber-prepare.elc twittering-prepare.elc \
fap-prepare.elc iplayer-prepare.elc \
auto-correct-prepare.elc color-theme-prepare.elc 

all: $(SETUP)

%.elc: %.el
	$(EMACS) $(BATCH) $(COMPILE) $<
