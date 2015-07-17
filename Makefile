#
GHP=$shell(pwd)
man:
	git pull -a
	cd ../emacspeak/info && make man
	cd ${GHP}
	git commit -a -m "regenreted docs"
	git push 
