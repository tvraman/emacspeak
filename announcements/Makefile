all: now-thirty.org
	codespell $<
	../etc/dw.py $<
	tex-check $<
	lacheck now-thirty.tex
	pdflatex now-thirty.tex 1> /dev/null
	grep -i overful now-thirty.log || echo done

