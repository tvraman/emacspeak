# Build Boodler packages:
# Boodler packages are built and installed for each dir in SRC
DISTFILES= \
birds/ listen/ nature/ seascape/ \
ladspa-asoundrc Makefile readme.org  \
soundscape.el soundscapes


SRC :=  seascape birds nature listen
BOOPS :=  $(foreach d,${SRC},org.emacspeak.$d.1.0.boop)
all: ${BOOPS} soundscape.elc 

soundscape.elc: soundscape.el
	emacs -batch -q --no-site-file -f batch-byte-compile $<

dist: 
	tar cfj soundscapes.tar.bz2 ${DISTFILES} 

# Create and install packages if modified 

org.emacspeak.%.1.0.boop:  %/*
	boodle-mgr -f  --import create  $* && boodle-mgr -f install $@

.phony: clean

clean:
	rm -f *.boop *.elc *.bz2
