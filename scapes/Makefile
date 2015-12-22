# Build Boodler packages:

org.emacspeak.seascape.1.0.boop: seascape/main.py
	boodle-mgr --import create  seascape 

org.emacspeak.listen.1.0.boop: listen/main.py
	boodle-mgr --import create  listen 

install: org.emacspeak.listen.1.0.boop  org.emacspeak.seascape.1.0.boop
	boodle-mgr install  org.emacspeak.seascape.1.0.boop
	boodle-mgr install org.emacspeak.listen.1.0.boop
