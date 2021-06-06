#
GHP=$(shell pwd)
man:
	cd ../emacspeak/info && make man
	cd ${GHP}
	git commit -a -m "Freshened docs"
	git push 
