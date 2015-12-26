# Build Boodler packages:
BOOPS=$(shell ls *.boop)
all: ${BOOPS}

org.emacspeak.seascape.1.0.boop: seascape/main.py
	boodle-mgr --import create  seascape 

org.emacspeak.listen.1.0.boop: listen/main.py
	boodle-mgr --import create  listen 

org.emacspeak.nature.1.0.boop: nature/main.py
	boodle-mgr --import create  nature 

org.emacspeak.birds.1.0.boop: birds/*.aiff
	boodle-mgr --import create  birds 

install:  ${BOOPS}
	boodle-mgr install  org.emacspeak.seascape.1.0.boop
	boodle-mgr install org.emacspeak.listen.1.0.boop
	boodle-mgr install org.emacspeak.nature.1.0.boop
	boodle-mgr install org.emacspeak.birds.1.0.boop
