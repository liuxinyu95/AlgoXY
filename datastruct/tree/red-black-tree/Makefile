HEADER = 
SOURCE = rbtree-en.tex
OBJECT = rbtree-en
TEX = latex

.PHONY: all

all: pdf

cpp:
	make cpp -C src

haskell:
	make haskell -C src

image:
	make -C img

pdf: $(OBJECT).dvi
	dvipdfmx $(OBJECT)

tex: image
	latex $(OBJECT)
	latex $(OBJECT)

html:
	htlatex $(OBJECT) "html, imgdir:img/"
	mv *.png img/

clean:
	rm -f *.4ct *.4tc *.aux *.css *.dvi *.html 
	rm -f *.idv *.lg *.log *.out *.pdf *.tmp *.xref *.cache *.zip *~