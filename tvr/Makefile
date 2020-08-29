# Build all setup files we use 

EMACS=emacs
BATCH = -batch -q -no-site-file     
COMPILE = -f batch-byte-compile
SETUP = emacs-startup.elc \
sp-prepare.elc lispy-prepare.elc  \
vm-prepare.elc gnus-gmail-prepare.elc  \
laptop-local.elc touchpad-disable.elc   slime-prepare.elc   \
org-prepare.elc  \
jabber-prepare.elc 

all: $(SETUP)

%.elc: %.el
	$(EMACS) $(BATCH) -f package-initialize  $(COMPILE)  $<
clean:
	rm -f *.elc
