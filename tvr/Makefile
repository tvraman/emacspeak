# Build and test my startup

EMACS=emacs
BATCH = -batch -q -no-site-file      
COMPILE = -f batch-byte-compile
COMMON_OPTS=	-f package-initialize  -l ./emacs-startup.elc \
 -l ../lisp/emacspeak-loaddefs.el  -f tvr-after-init
TEST=-batch  --eval '(setq force-load-messages t  debug-on-error t)'
Q_TEST=-batch  --eval '(setq debug-on-error t)'
#files we will load on startup:

ALL= \
gnus-gmail-prepare.el \
kbd-macros.el \
laptop-local.el \
lispy-prepare.el \
org-prepare.el \
slime-prepare.el \
sp-prepare.el \
sdcv-prepare.el \
vm-prepare.el   

# We create all-prepare.el and compile our startup file.

all:  all-prepare.el emacs-startup.elc 

# Concatenate all files we will load to create all-prepare.el
all-prepare.el:  ${ALL}
	cat $(ALL) > all-prepare.el

# Generic rule to build .elc file from a .el file
%.elc: %.el
	$(EMACS) $(BATCH) -f package-initialize  $(COMPILE)  $<

clean:
	rm -f *.elc all-prepare.el

# Verbose Test: Show all files as they load
test: all-prepare.el emacs-startup.elc
	emacs ${TEST} ${COMMON_OPTS}

# Slightly less verbose:
q: all-prepare.el emacs-startup.elc
	emacs ${Q_TEST} ${COMMON_OPTS}	
