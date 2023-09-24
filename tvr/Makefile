# Build all-prepare.el

EMACS=emacs
BATCH = -batch -q -no-site-file      
COMPILE = -f batch-byte-compile

ALL= \
gnus-gmail-prepare.el \
kbd-macros.el \
laptop-local.el \
lispy-prepare.el \
org-prepare.el \
slime-prepare.el \
sp-prepare.el \
sdcv-prepare.el \
vm-prepare.el   \
yasnippet-prepare.el 


all:  emacs-startup.elc
	make test

all-prepare.el: 
	cat $(ALL) > all-prepare.el

%.elc: %.el
	$(EMACS) $(BATCH) -f package-initialize  $(COMPILE)  $<
clean:
	rm -f *.elc all-prepare.el

test: all-prepare.el emacs-startup.elc
	emacs -batch  --eval \
'(setq force-load-messages t  debug-on-error t)' \
	-f package-initialize  -l ./emacs-startup.elc 
