# Build all setup files we use 

EMACS=emacs
BATCH = -batch -q -no-site-file     
COMPILE = -f batch-byte-compile

ALL= \
00-exwm-prepare.el \
gnus-gmail-prepare.el \
kbd-macros.el \
lispy-prepare.el \
org-prepare.el \
slime-prepare.el \
sp-prepare.el \
vm-prepare.el \
weekday-colors.el

all: all-prepare.elc emacs-startup.elc 

all-prepare.el: $(ALL)
	cat $(ALL) > all-prepare.el

%.elc: %.el
	$(EMACS) $(BATCH) -f package-initialize  $(COMPILE)  $<
clean:
	rm -f *.elc
