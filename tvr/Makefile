# Build all setup files we use 

EMACS=emacs
BATCH = -batch -q -no-site-file    
COMPILE =  -f batch-byte-compile

SETUP = emacs-startup.elc kbd-setup.elc  \
 vm-prepare.elc gnus-prepare.elc bbdb-prepare.elc \
laptop-local.elc touchpad-disable.elc elfeed-prepare.elc \
 folding.elc folding-prepare.elc \
auctex-prepare.elc reftex-prepare.elc \
calc-prepare.elc js-prepare.elc tcl-prepare.elc \
slime-prepare.elc  python-mode-prepare.elc \
 sp-prepare.elc org-prepare.elc erc-prepare.elc \
jabber-prepare.elc twittering-prepare.elc \
iplayer-prepare.elc \
helm-prepare.elc vdiff-prepare.elc \
projectile-prepare.elc  yasnippet-prepare.elc 

all: $(SETUP)

%.elc: %.el
	$(EMACS) $(BATCH) $(COMPILE) $<
clean:
	rm -f *.elc
