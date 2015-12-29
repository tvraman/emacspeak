# Build Boodler packages:
SRC :=  seascape birds nature listen
BOOPS :=  $(foreach d,${SRC},org.emacspeak.$d.1.0.boop)

all: ${BOOPS}


# Create and install any modified packages:

org.emacspeak.%.1.0.boop:  %/*
	boodle-mgr -f  --import create  $*
	boodle-mgr -f install $@

.phony: clean
clean:
	rm -f *.boop
