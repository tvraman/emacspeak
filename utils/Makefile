tests: module-test 

module-test: self-document.elc
	emacs -batch -q -l self-document.elc -f self-document-module-test

load-test: self-document.elc
	emacs -batch -q -l self-document.elc -f self-document-load-test

self-document.elc: self-document.el
	emacs -q -batch -f batch-byte-compile self-document.el
