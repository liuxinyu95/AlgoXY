.PHONY: all

all: img pdf

img:
	make -C images

pdf:
	latex main.tex

html:

clean:
	rm -f *.aux
	rm -f *.dvi
	rm -f *.pdf
	rm -f *.ps
	rm -f *.log
	rm -f *.idv
	rm -f *.out
	rm -f *.tmp
	rm -f *.lg
	rm -f *.4??
	rm -f *.xref
	rm -f *.png
	rm -f *.css
	rm -f *.html
	make -C images clean