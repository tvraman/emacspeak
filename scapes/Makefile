# Build Boodler packages:
BOOPS=$(shell ls *.boop)
all: ${BOOPS}

org.emacspeak.seascape.1.0.boop: seascape/*
	boodle-mgr -f  --import create  seascape 

org.emacspeak.listen.1.0.boop: listen/*
	boodle-mgr -f  --import create  listen 

org.emacspeak.nature.1.0.boop: nature/*
	boodle-mgr -f  --import create  nature 

org.emacspeak.birds.1.0.boop: birds/*
	boodle-mgr -f  --import create  birds 

install:  ${BOOPS}
	boodle-mgr -f  install  org.emacspeak.seascape.1.0.boop
	boodle-mgr -f  install org.emacspeak.listen.1.0.boop
	boodle-mgr -f  install org.emacspeak.nature.1.0.boop
	boodle-mgr -f  install org.emacspeak.birds.1.0.boop
