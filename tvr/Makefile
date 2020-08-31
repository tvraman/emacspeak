# Build all setup files we use 

EMACS=emacs
BATCH = -batch -q -no-site-file     
COMPILE = -f batch-byte-compile
SETUP = emacs-startup.elc \
sp-prepare.elc lispy-prepare.elc  \
vm-prepare.elc gnus-gmail-prepare.elc  \
laptop-local.elc touchpad-disable.elc   slime-prepare.elc   \
org-prepare.elc jabber-prepare.elc  all-prepare.elc 

ALL= vm-prepare.el gnus-gmail-prepare.el jabber-prepare.el \
sp-prepare.el lispy-prepare.el slime-prepare.el org-prepare.el
all: all-prepare.elc emacs-startup.elc

all-prepare.el: $(ALL)
	cat $(ALL) > all-prepare.el

laptop-local.elc: laptop-local.el
%.elc: %.el
	$(EMACS) $(BATCH) -f package-initialize  $(COMPILE)  $<
clean:
	rm -f *.elc
