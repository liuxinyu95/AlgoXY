HEADER = 
SOURCE = bstree-en.tex
OBJECT = bstree-en
TEX = latex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

haskell:
	make haskell -C src

image:
	convert img/tree.png img/tree.eps

pdf: $(OBJECT).dvi
	dvipdfmx $(OBJECT)

tex: image
	latex $(OBJECT)
	latex $(OBJECT)

html:
	htlatex.bat $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm *.aux *.dvi *.log *.out *.pdf *~